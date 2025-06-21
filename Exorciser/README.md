# Motorola Exorciser
This is an attempt to run the Exorciser Firmware on the kees CPUXXCMI Hardware.

### Modifications: ###

.) Obviously ROM and GALs (see kees_Exorciser Directory)

.) Hardware Mods:

	- The ROM needs A11 and A12, no need to desolder R12 and R13
	- GAL G1 needs A9 on Pin 18
    - smaller Resistor R16 (i used 470E)
    - Jumper on JP8
	- Remove U16 and U17
	- make an Adapter for the HC74 in Position U16

	 Socket | IC    Function
	 Pin    | Pin  
	  1     |  1 + 13  | /RESET
	  2     |  2 + 4   | +5V
	  3     |  3 + 11  | IACK (from GAL)
	  4     |  n.c.    | 
	  5     |  n.c.    | 
	  6     |  9       | Q2 (to SYS Input of GAL)
	  7     |  7       | GND
	  8     |  n.c.    | 
	  9     |  n.c.    | 
	  10    |  n.c.    | 
	  11    |  n.c.    | 
	  12    |  n.c.    | 
	  13    |  n.c.    | 
	  14    |  14 + 10 | +5V
	  --    |  5 + 12  | Q1
	  --    |  8,6 n.c.|

	  This is for switching on the ROM in Address FFFE and FFFF (Reset Vector).
	  After two Read Cycles it switches to RAM from FE00-FFFF (thats why A9 is neded).

### Firmware ###

The Rom starts @ E000 but is accessed @ E800 where the Exordisk Firmware sits. At F000 is the Exbug Monitor. At FCF8-FCFF is the original PROM and the 6820. Here the Address FCFD is important, it holds the Initialisation Value for the MC6850.
I have a Github page for Reverse Engineering the MDOS Operating System and the Exbug Monitor.

