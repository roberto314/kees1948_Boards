/*	EXORcister simulator
 *	Copyright
 *		(C) 2011 Joseph H. Allen
 *
 * This is free software; you can redistribute it and/or modify it under the 
 * terms of the GNU General Public License as published by the Free Software 
 * Foundation; either version 1, or (at your option) any later version.  
 *
 * It is distributed in the hope that it will be useful, but WITHOUT ANY 
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS 
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more 
 * details.  
 * 
 * You should have received a copy of the GNU General Public License along with 
 * this software; see the file COPYING.  If not, write to the Free Software Foundation, 
 * 675 Mass Ave, Cambridge, MA 02139, USA.
 */

/* Exorciser / swtpc emulator */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <sys/poll.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <signal.h>
#include <unistd.h>

#include "sim6800.h"
#include "exor.h"
#include "exorterm.h"
#include "utils.h"

char *lpt_name; /* Name of line printer file */
FILE *lpt_file; /* Line printer file */

/* Options */

int swtpc = 0;
const char *exbug_name; /*  = "exbug.bin"; */
int trace_disk = 1; /* Enable disk trace */
int lower = 0; /* Allow lower case */

int protect_roms = 1; /* Protect "ROM"s from writing if set */

/* Diskettes */

struct drive_info {
        const char *name;
        FILE *f;
        int bytes; /* Bytes per sector */
        int tracks; /* Tracks per disk */
        int sects; /* Sectors per track */
} drive[4] =
{
        { 0, 0, 0, 0, 0 },
        { 0, 0, 0, 0, 0 },
        { 0, 0, 0, 0, 0 },
        { 0, 0, 0, 0, 0 }
};

/* Memory */
unsigned char mem[65536];

int pending_read_ahead = 1;
unsigned char read_ahead_c;

int count = 10;

int polling = 1; /* Allow ACIA polling */

const char *local_prefix;

/* Copy a file by name */

int copyfile(const char *src, const char *dest)
{
        FILE *f, *g;
        printf("Copying %s to %s...\n", src, dest);
        f = fopen(src, "r");
        if (f)
        {
                g = fopen(dest, "w");
                if (g)
                {
                        char buf[1024];
                        size_t len;
                        while ((len = fread(buf, 1, sizeof(buf), f)))
                                fwrite(buf, 1, len, g);
                        fclose(g);
                        fclose(f);
                        return 0;
                }
                else
                {
                        fclose(f);
                        return -1;
                }
        }
        else
        {
                return -1;
        }
}

/* Find configuration or state file */
/* If 'copy' set, make a writable copy of the file in the user's home directory */

const char *choose_config_file(const char *name, int copy)
{
        FILE *f;
        /* Create path $HOME/.exorsim */
        if (!local_prefix)
        {
                char *home = getenv("HOME");
                char *tmp = malloc(strlen(home) + strlen("/.exorsim") + 1);
                sprintf(tmp, "%s/.exorsim", home);
                local_prefix = tmp;
                mkdir(local_prefix, 0700); /* Create directory in case it doesn't exist */
        }
        /* First try current directory */
        f = fopen(name, "r");
        if (f)
        {
                fclose(f);
                return name;
        }
        else
        {
                /* Next, try ~/.exotsim */
                char *local = malloc(strlen(local_prefix) + 1 + strlen(name) + 1);
                sprintf(local, "%s/%s", local_prefix, name);
                f = fopen(local, "r");
                if (f)
                {
                        fclose(f);
                        return local;
                }
                else
                {
                        /* Try /usr/local/share/exorsim */
                        char *sys = malloc(strlen(DATADIR) + strlen(name) + 1);
                        sprintf(sys, "%s%s", DATADIR, name);
                        if (copy)
                        {
                                /* Make local copy */
                                copyfile(sys, local);
                                return local;
                        }
                        else
                        {
                                /* Otherwise it had better be there */
                                return sys;
                        }
                }
        }
}

void getsect(int n, int addr, int sect, int len)
{
        if (trace_disk == 2) printf("Read sector %d into %x, size=%d\n", sect, addr, len);
        if (drive[n].f) {
                fseek(drive[n].f, sect * drive[n].bytes, SEEK_SET);
                fread(mem + addr, len, 1, drive[n].f);
        } else {
                printf("Tried to read from non-existent disk %d\n", n);
                stop = 1;
        }
}

void getsect1(int n, unsigned char *addr, int ofst, int len)
{
        if (trace_disk == 2) printf("Read sector %d\n", ofst / 256);
        fseek(drive[n].f, ofst, SEEK_SET);
        fread(addr, len, 1, drive[n].f);
}

void putsect(int n, int addr, int sect, int len)
{
        if (trace_disk == 2) printf("Write sector %d into %x, size=%d\n", sect, addr, len);
        if (drive[n].f) {
                fseek(drive[n].f, sect * drive[n].bytes, SEEK_SET);
                fwrite(mem + addr, len, 1, drive[n].f);
                fflush(drive[n].f);
        } else {
                printf("Tried to write to non-existent disk %d\n", n);
                stop = 1;
        }
}

void putsect1(int n, unsigned char *addr, int ofst, int len)
{
        if (trace_disk == 2) printf("Write sector %d\n", ofst / 256);
        fseek(drive[n].f, ofst, SEEK_SET);
        fwrite(addr, len, 1, drive[n].f);
        fflush(drive[n].f);
}

/* FD1771 emulation.. */

int cur_drive = 0;
int cur_sect = 1;
int cur_track = 0;
unsigned char cur_status = 0;
unsigned char cur_buf[256];
int cur_count;
unsigned char cur_data;
int cur_state; /* 0 = IDLE, 1 = read single, 2 = read multiple, 3 = write single */
int cur_dir = 1;

int count1;
static int old_ssda_val = 0x1000;
static int val_cntr;
static char statusreg;

/* All memory reads go through this function */

unsigned char mread(unsigned short addr){
        switch (addr) {
                case 0xFCFb: {
                        printf("FCFB Read \n");
                        return 0xF;
                } 
                case 0xFCFa: {
                        printf("FCFA Read \n");
                        return 0xF;
                } 
                case 0xFCF9: {
                        printf("FCF9 Read \n");
                        return 0xF;
                } 
                case 0xFCF8: {
                        printf("FCF8 Read \n");
                        return 0x80;
                } 
                case 0xEC00: { // PIAREGA
                        return 0x00; //PA5 is for DS
                } 
                case 0xEC01: { // PIAREGB
                        return 0x10; //Return WPROT
                } 
                case 0xEC03: { // PIACTRLB
                        return 0x80; //Return IDX from Floppy
                } 
                case 0xEC04: { // SSDA Status
                        //printf("%04x\n", pc);
                        if ((pc == 0x220E) ||
                            (pc == 0x221C) ||
                            (pc == 0x222A) ||
                            (pc == 0x2232) ||
                            (pc == 0x2242) ||
                            (pc == 0x224E)){
                                return 0x40; //Return TUF
                        }
                        else{
                                return 0x18; //Return TX FIFO empty
                        }                } 
                case 0xFCF4: { /* Check serial port status */
                        if (quick_term_poll())
                                return 0x03;
                        else
                                return 0x02;
                } 
                case 0xFCF5: { /* Read from serial port */
                        if (quick_term_poll())
                                return term_in();
                        else
                                return 0;
                } 
                default: {
                        return mem[addr];
                }
        }
}

/* All memory writes go through this function */

void mwrite(unsigned short addr, unsigned char data){
                /* Do not write to ROM */
        if (protect_roms) {
                if ((addr >= 0xE800 && addr < 0xEC00) ||
                    (addr >= 0xF000 && addr < 0xFC00) ||
                    (addr >= 0xFCFC && addr < 0xFD00))
                        return;
        }
        mem[addr] = data;
        switch (addr) {
                case 0xFCF5: { /* Write to serial port */
                        term_out(data);
                        /* putchar(data); fflush(stdout); */
                        break;
                } 
                case 0xEC04: {
                        printf("Ctrl1:  %02X from %04X\n", data, pc-3);
                        statusreg = data;
                        break;
                } 
                case 0xEC05: {
                        switch (statusreg & 3){
                                case 0:
                                        printf("Ctrl2:  %02X from %04X\n", data, pc-3);
                                break;
                                case 1:
                                        printf("Sync:   %02X from %04X\n", data, pc-3);
                                break;
                                case 2:
                                        if (data == old_ssda_val){
                                                val_cntr ++;
                                        } else {
                                                printf("Ctrl3:  %02X from %04X\n", data, pc-3);
                                        }
                                        old_ssda_val = data;
                                break;
                                case 3:
                                        printf("TXData: %02X from %04X\n", data, pc-3);
                                break;
                                default:
                                break;
                        }
                }
        }
}

unsigned short pull2();

/* Addresses of floppy parameters */

#define CURDRV 0	/* Current drive: 0 -3 */
#define STRSCT 1	/* Starting sector (2 bytes) */
#define NUMSCT 3	/* Number of sectors (2 bytes) */
#define LSCTLN 5	/* Length of last sector (1 byte) */
#define CURADR 6	/* Transfer address (2 bytes) */
#define FDSTAT 8	/* Error status: 0x30 means no error */

#define SCTCNT 0x0B	/* Sector count (2 byts): (STRSCT + NUMSCT - SCTCNT - 1) is bad sector number */
#define SIDES 0x0D	/* bit 7 = 1 means single-sided, bit 7 = 0 means double-sided */

/* MDOS disk error codes for FDSTAT */

#define ER_NON '0'	/* No error */
#define ER_CRC '1'	/* Data CRC error */
#define ER_WRT '2'	/* Write protected disk */
#define ER_RDY '3'	/* Disk not ready */
#define ER_MRK '4'	/* Deleted data mark encountered */
#define ER_TIM '5'	/* Timeout */
#define ER_DAD '6'	/* Invalid disk address */
#define ER_SEK '7'	/* Seek error */
#define ER_DMA '8'	/* Data address mark error */
#define ER_ACR '9'	/* Address mark CRC error */

/* Check drive number */

int check_drive(int n){
        if (n >= 4) {
                // printf("\r\nFloppy error: attempt to access drive number %d >= 4\n", n);
                mem[FDSTAT] = ER_RDY;
                c_flag = 1;
                return -1;
        }
        if (!drive[n].f) {
                // printf("\r\nFloppy error: attempt to access non-existent disk %d\n", n);
                mem[FDSTAT] = ER_RDY;
                c_flag = 1;
                return -1;
        }
        return 0;
}

/* Check sector number */

int check_sect(int n, int sect)
{
        if (sect >= drive[n].sects * drive[n].tracks) {
                printf("\r\nFloppy error: attempt to access past end of disk %d, sector %d\n", n, sect);
                mem[FDSTAT] = ER_DAD;
                c_flag = 1;
                return -1;
        }
        return 0;
}

/* Send character to printer */

void lpt_out(unsigned char c)
{
        if (c)
        {
                if (lpt_file)
                {
                        fputc(c, lpt_file);
                        if (c == '\n')
                        {
                                fflush(lpt_file);
                        }
                }
                else
                {
                        term_out(c);
                }
        }
}

/* All jumps go through this function */

int intercept_inch = -1;
int echo_flag_addr;
int exbug_detected = 0;

void jump(unsigned short addr){

        if (addr == intercept_inch) {
                /* Intercept INCH function */
                /* Note that we don't intercept output functions, we just emulate the ACIA hardware, see mwrite() */
                acca = term_in();
                if (!mem[echo_flag_addr]) { /* Echo flag */
                        term_out(acca);
                        /* putchar(c);
                        fflush(stdout); */
                } else {
                        mem[echo_flag_addr] = 0;
                }
                c_flag = 0; /* No error */
        }
        else
        {
                /* Intercept EXORdisk-II calls */
                /* EXORdisk-II PROM is 0xE800 - 0xEBFF */
                /* Note that Line Printer driver is included in the EXORdisk-II PROM */
                switch (addr) {
                        case 0xE800: /* OSLOAD (no modified parms) */ {
                                printf("\nOSLOAD...\n");
                                getsect(0, 0x0020, 23, 128);
                                getsect(0, 0x0020 + 0x0080, 24, 128);
                                pc = 0x0020;
                                sp = 0x00FF;
                                return;
                        } case 0xE822: /* FDINIT (no modified parms) */ {
                                mem[0x24] = 0;
                                c_flag = 0;
                                break;
                        } 
#if 0
                          case 0xF853: /* CHKERR */ {
                                break;
                        } case 0xE85A: /* PRNTER */ {
                                break;
                        } 
#endif
                          case 0xE869: /* READSC (read full sectors) */ {
                                mem[LSCTLN] = 128;
                        } case 0xE86D: /* READPS (read partial sectors) (FDSTAT, carry, sides) */ {
                                int x;
                                int n = mem[CURDRV];
                                int first = (mem[STRSCT] << 8) + mem[STRSCT + 1];
                                int num = (mem[NUMSCT] << 8) + mem[NUMSCT + 1];
                                int addr = (mem[CURADR] << 8) + mem[CURADR + 1];
                                int last = mem[LSCTLN];
                                if (trace_disk == 2) printf("Read sectors: drive=%d, first=%d, number=%d, addr=%x, size of last=%d\n", n, first, num,
                                       addr, last);
                                if (check_drive(n))
                                        break;
                                for (x = 0; x != num; ++x) {
                                        if (check_sect(n, first + x))
                                                goto oops;
                                        getsect(n, addr + 128 * x, first + x, ((x + 1 == num) ? mem[LSCTLN] : 128));
                                }
                                mem[FDSTAT] = ER_NON;
                                if (drive[n].tracks == 77)
                                        mem[SIDES] = 0x80;
                                else
                                        mem[SIDES] = 0;
                                c_flag = 0;
                                oops: break;
                        } case 0xE86F: /* RDCRC */ {
                                if (trace_disk) printf("RDCRC\n");
                                int x;
                                int n = mem[CURDRV];
                                int first = (mem[STRSCT] << 8) + mem[STRSCT + 1];
                                int num = (mem[NUMSCT] << 8) + mem[NUMSCT + 1];
                                int addr = (mem[CURADR] << 8) + mem[CURADR + 1];
                                int last = mem[LSCTLN];
                                if (trace_disk) printf("RDCRC: drive=%d, first=%d, number=%d, addr=%x, size of last=%d\n", n, first, num,
                                       addr, last);
                                if (check_drive(n))
                                        break;
                                for (x = 0; x != num; ++x) {
                                        if (check_sect(n, first + x))
                                                goto oops;
                                }
                                mem[FDSTAT] = ER_NON;
                                if (drive[n].tracks == 77)
                                        mem[SIDES] = 0x80;
                                else
                                        mem[SIDES] = 0;
                                c_flag = 0;
                                break;
                        } case 0xE875: /* RESTOR */ {
                                int n = mem[CURDRV];
                                if (trace_disk) printf("RESTOR\n");
                                if (check_drive(n))
                                        break;
                                mem[FDSTAT] = ER_NON;
                                if (drive[n].tracks == 77)
                                        mem[SIDES] = 0x80;
                                else
                                        mem[SIDES] = 0;
                                c_flag = 0;
                                break;
                        } case 0xE878: /* SEEK */ {
                                int n = mem[CURDRV];
                                int first = (mem[STRSCT] << 8) + mem[STRSCT + 1];
                                if (trace_disk) printf("SEEK\n");
                                if (check_drive(n))
                                        break;
                                if (check_sect(n, first))
                                        break;
                                if (drive[n].tracks == 77)
                                        mem[SIDES] = 0x80;
                                else
                                        mem[SIDES] = 0;
                                c_flag = 0;
                                break;
                        } case 0xE872: /* RWTEST */ {
                                if (trace_disk) printf("RWTEST\n");
                        } case 0xE87B: /* WRTEST */ {
                                unsigned char buf[128];
                                int x;
                                int n = mem[CURDRV];
                                int first = (mem[STRSCT] << 8) + mem[STRSCT + 1];
                                int num = (mem[NUMSCT] << 8) + mem[NUMSCT + 1];
                                int addr = (mem[CURADR] << 8) + mem[CURADR + 1];
                                if (trace_disk) printf("WRTEST\n");
                                if (check_drive(n))
                                        break;
                                for (x = 0; x != 128; x += 2) {
                                        buf[x] = mem[addr];
                                        buf[x + 1] = mem[addr + 1];
                                }
                                for(x=0; x != num; ++x) {
                                        if (check_sect(n, first + x))
                                                goto oops;
                                        if (trace_disk) printf("Wrtest sector %d drive %d\n", first + x, n);
                                        fseek(drive[n].f, (first + x) * 128, SEEK_SET);
                                        fwrite(buf, 128, 1, drive[n].f);
                                        fflush(drive[n].f);
                                }
                                c_flag = 0;
                                if (drive[n].tracks == 77)
                                        mem[SIDES] = 0x80;
                                else
                                        mem[SIDES] = 0;
                                mem[FDSTAT] = ER_NON;
                                break;
                        } case 0xE87E: /* WRDDAM */ {
                                int n = mem[CURDRV];
                                printf("\r\nFloppy error: we do not support WRDDAM\n");
                                c_flag = 1;
                                if (drive[n].tracks == 77)
                                        mem[SIDES] = 0x80;
                                else
                                        mem[SIDES] = 0;
                                mem[FDSTAT] = ER_WRT;
                                break;
                        } case 0xE884: /* WRITSC */ {
                                if (trace_disk) printf("WRITSC\n");
                        } case 0xE881: /* WRVERF */ {
                                int x;
                                int n = mem[CURDRV];
                                int first = (mem[STRSCT] << 8) + mem[STRSCT + 1];
                                int num = (mem[NUMSCT] << 8) + mem[NUMSCT + 1];
                                int addr = (mem[CURADR] << 8) + mem[CURADR + 1];
                                int last = mem[LSCTLN];
                                if (trace_disk) printf("WRVERF: drive=%d, first=%d, number=%d, addr=%x, size of last=%d\n", n, first, num,
                                       addr, last);
                                if (check_drive(n))
                                        break;
                                for(x=0; x != num; ++x) {
                                        if (check_sect(n, first + x))
                                                goto oops;
                                        putsect(n, addr + 128 * x, first + x, 128);
                                }
                                if (drive[n].tracks == 77)
                                        mem[SIDES] = 0x80;
                                else
                                        mem[SIDES] = 0;
                                mem[FDSTAT] = ER_NON;
                                c_flag = 0;
                                break;
                        } case 0xE887: /* CLOCK */ {
                                printf("Floppy: Someone called CLOCK?\n");
                                c_flag = 0;
                                break;
                        } case 0xEBC0: /* LPINIT */ {
                                if (trace_disk) printf("LPINIT\n");
                                c_flag = 0;
                                break;
                        } case 0xEBCC: /* LIST */ {
                                if (trace_disk) printf("LIST\n");
                                lpt_out(acca);
                                c_flag = 0;
                                break;
                        } case 0xEBE4: /* LDATA */ {
                                if (trace_disk)printf("LDATA\n");
                                while (mem[ix] != 4) {
                                        lpt_out(mem[ix]);
                                        ++ix;
                                }
                                lpt_out('\r');
                                lpt_out('\n');
                                c_flag = 0;
                                break;
                        } case 0xEBF2: /* LDATA1 */ {
                                if (trace_disk) printf("LDATA1\n");
                                while (mem[ix] != 4) {
                                        lpt_out(mem[ix]);
                                        ++ix;
                                }
                                c_flag = 0;
                                break;
                        } default: {
                                pc = addr;
                                return;
                        }
                }
        }
        /* Notice that call was intercepted */
        simulated(addr);
        /* Return from subroutine now */
        addr = pull2();
        jump(addr);
}

int load_exbug()
{
        FILE *f = fopen(exbug_name, "r");
        if (!f) {
                fprintf(stderr, "Couldn't load '%s'\n", exbug_name);
                return -1;
        }
        if (1 != fread(mem, 64 * 1024, 1, f)) {
                fprintf(stderr, "Couldn't read '%s'\n", exbug_name);
                return -1;
        }
        printf("'%s' loaded.\n", exbug_name);
        fclose(f);
        if (!memcmp(&mem[0xFA8B], "\xb6\xfc\xf4\x47", 4))
        {
                printf("  EXBUG-1.1 detected\n");
                intercept_inch = 0xFA8B;
                echo_flag_addr = 0xFF53;
                exbug_detected = 1;
        }
        else if (!memcmp(&mem[0xFA6B], "\xb6\xfc\xf4\x47", 4))
        {
                printf("  EXBUG-1.2 detected\n");
                intercept_inch = 0xFA6B;
                echo_flag_addr = 0xFF53;
                exbug_detected = 1;
        }
        return 0;
}

void close_drive(int n)
{
        if (drive[n].f) {
                fclose(drive[n].f);
        }
        drive[n].f = 0;
}

void show_drive(int n)
{
        if (drive[n].f) {
                printf("%s mounted as drive %d\n", drive[n].name, n);
        } else {
                printf("No disk in drive %d\n", n);
        }
}

void set_drive(int n, const char *name)
{
        drive[n].name = name;
}

int load_drive(int n){
        FILE *f;
        long t;
        f = fopen(drive[n].name, "r+");
        if (!f) {
                fprintf(stderr, "Couldn't open '%s'\n", drive[n].name);
                return -1;
        }
        fseek(f, 0, SEEK_END);
        t = ftell(f);
        if (t == 128 * 26 * 77) {
                printf("'%s' opened for drive %d (single sided)\n", drive[n].name, n);
                drive[n].f = f;
                drive[n].bytes = 128;
                drive[n].tracks = 77;
                drive[n].sects = 26;
        } else if (t == 128 * 26 * 77 * 2) {
                printf("'%s' opened for drive %d (double sided)\n", drive[n].name, n);
                drive[n].f = f;
                drive[n].bytes = 128;
                drive[n].tracks = 77 * 2;
                drive[n].sects = 26;
        } else if (t == 128 * 40 * 16) {
                printf("'%s' opened for drive %d (single sided minifloppy)\n", drive[n].name, n);
                drive[n].f = f;
                drive[n].bytes = 128;
                drive[n].tracks = 40;
                drive[n].sects = 16;
        } else {
                fclose(f);
                printf("'%s' is not a valid disk: it's size must be %d or %d\n", drive[n].name, 128*26*77*2, 128*26*77);
                return -1;
        }
return 0;
}

void ctrl_c()
{
        printf("Interrupt!\n");
        stop = 1;
}

int main(int argc, char *argv[])
{
        int x;
        int diskn = 0;
        int gotox = 0;
        mon_out = stdout;
        mon_in = stdin;
        const char *facts_name = 0;
        int lpt_append = 1;

        for (x = 1; x != argc; ++x) {
                if (argv[x][0] == '-') {
                        if (!strcmp(argv[x], "--facts") && x + 1 != argc) {
                                ++x;
                                facts_name = argv[x];
                        } else if (!strcmp(argv[x], "--trace")) {
                                trace = 1;
                        } else if (!strcmp(argv[x], "--dtrace")) {
                                trace_disk = 1;
                        } else if (!strcmp(argv[x], "--mon")) {
                                stop = 1;
                        } else if (!strcmp(argv[x], "--no_exorterm")) {
                                exorterm = 0;
                        } else if (!strcmp(argv[x], "--skip") && x + 1 != argc) {
                                ++x;
                                skip = atoi(argv[x]);
                        } else if (!strcmp(argv[x], "--exbug") && x + 1 != argc) {
                                exbug_name = argv[++x];
                        } else if (!strcmp(argv[x], "-x")) {
                                gotox = 1;
                        } else if (!strcmp(argv[x], "-0") && x + 1 != argc) {
                                drive[0].name = argv[++x];
                        } else if (!strcmp(argv[x], "-1") && x + 1 != argc) {
                                drive[1].name = argv[++x];
                        } else if (!strcmp(argv[x], "-2") && x + 1 != argc) {
                                drive[2].name = argv[++x];
                        } else if (!strcmp(argv[x], "-3") && x + 1 != argc) {
                                drive[3].name = argv[++x];
                        } else if (!strcmp(argv[x], "--lpt") && x + 1 != argc) {
                                lpt_append = 0;
                                lpt_name = argv[++x];
                        } else if (!strcmp(argv[x], "--append") && x + 1 != argc) {
                                lpt_append = 1;
                                lpt_name = argv[++x];
                        } else if (!strcmp(argv[x], "--lower")) {
                                lower = 1;
                        } else if (!strcmp(argv[x], "--no_protect")) {
                                protect_roms = 0;
                        } else {
                                printf("EXORciser emulator\n");
                                printf("\n");
                                printf("exor [options] [-0 disk0] [-1 disk1] [-2 disk2] [-3 disk3]\n");
                                printf("\n");
                                printf("  --trace	Produce instruction trace on stderr\n");
                                printf("  --dtrace	Produce disk access trace on stderr\n");
                                printf("  --skip nnn    Skip first nnn insns in trace\n");
                                printf("  --swtpc       Simulate SWTPC instead of EXORciser\n");
                                printf("  --exbug name	Give name for ROM if not 'exbug.bin'\n");
                                printf("  -x            Go into EXBUG/SWTBUG instead of MDOS/FLEX\n");
                                printf("  --facts file  Process facts files for commented disassembly\n");
                                printf("  --lower       Allow lowercase\n");
                                printf("  --mon         Start at monitor prompt\n");
                                printf("  --lpt file    Save line printer output to a file\n");
                                printf("  --append file Append line printer output to a file\n");
                                printf("  --no_protect  Allow writing to ROMs\n");
                                printf("  --no_exorterm Disable exorterm emulation\n");
                                printf("\n");
                                printf("Default disk0 is mdos.dsk/flex.dsk\n");
                                printf("\n");
                                printf("Hints:\n");
                                printf("  To load MDOS from EXBUG, type MAID followed by E800;G\n");
                                printf("\n");
                                exit(-1);
                        }
                } else {
                        if (diskn == 4) {
                                printf("Only up to four disks allowed\n");
                                return -1;
                        } else {
                                drive[diskn++].name = argv[x];
                        }
                }
        }

        /* Line printer */

        if (!lpt_name)
                lpt_name = getenv("EXOR_LPT_NAME");

        if (!lpt_name)
                lpt_name = "listing.lp";

        /* Open line printer */
        if (lpt_name)
        {
                if (lpt_append)
                        lpt_file = fopen(lpt_name, "a");
                else
                        lpt_file = fopen(lpt_name, "w");
                if (!lpt_file)
                {
                        fprintf(stderr, "Couldn't open line printer file %s\n", lpt_name);
                        return -1;
                }
        }

        /* Default disk image names */
        if (!drive[0].name) drive[0].name = getenv("EXOR_DRIVE0");
        if (!drive[1].name) drive[1].name = getenv("EXOR_DRIVE1");
        if (!drive[2].name) drive[2].name = getenv("EXOR_DRIVE2");
        if (!drive[3].name) drive[3].name = getenv("EXOR_DRIVE3");

        if (!drive[0].name) {
                drive[0].name = choose_config_file("mdos.dsk", 1);
        }

        if (!exbug_name) {
                exbug_name = getenv("EXOR_EXBUG");
        }
        /* Default memory image name */
        if (!exbug_name) {
                exbug_name = choose_config_file("exbug.bin", 0);
        }

        if (!facts_name) {
                facts_name = getenv("EXOR_FACTS");
        }

        if (!facts_name) {
                facts_name = choose_config_file("facts", 0);
        }

        /* Load facts file */
        if (facts_name) {
                FILE *f;
                printf("Load facts file '%s'\n", facts_name);
                f = fopen(facts_name, "r");
                if (f) {
                        parse_facts(f);
                        fclose(f);
                } else {
                        printf("Couldn't load '%s'\n", facts_name);
                }
        }

        /* Load initial memory image */
        if (load_exbug()) {
                /* Start halted if there is no ROM */
                stop = 1;
        }

        /* Mount drives */
        for (x = 0; x != 4; ++x) {
                if (drive[x].name)
                        load_drive(x);
        }

        if (!drive[0].f) {
                /* Do not boot DOS if there is no disk */
                gotox = 1;
        }

        /* Read starting address from reset vector */
        pc = ((mem[0xFFFE] << 8) + mem[0xFFFF]);

        /* ...but jump right to OS load unless gotox is set */
        if (!gotox) {
                if (swtpc) {
                        /* Jump right into flex */
                        sp = 0xA042;
                        pc = 0xE28F;
                } else {
                        /* Jump right into MDOS */
                        if (exbug_detected)
                        {
                                /* But only if EXBUG was recogized */
                                sp = 0xFF8A;
                                // pc = 0xE800;
                                jump(0xE800);
                        }
                }
        }

        /* system("stty cbreak -echo -icrnl"); */
        save_termios();
        sim_termios();

        signal(SIGINT, ctrl_c);

        printf("\nHit Ctrl-C for simulator command line.  Starting simulation...\n");
        izexorterm();

        sim();
        // echo test of terminal emulator
        // while (!stop) term_out(term_in());

        /* system("stty cooked echo icrnl"); */
        restore_termios();
        if (lpt_file)
                fclose(lpt_file);
        return 0;
}
