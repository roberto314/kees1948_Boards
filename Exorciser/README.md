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

Also we need A8 on the GAL G1 because of extended firmware. We need ROM in ED00-EFFF, and we need Output from the HC245 only EC00-ECFF

The easiest way is to use U3B - a 74HC132 NAND. Pins 4+5 are unfortunately connected to +5V on the upper side (difficult to see). Cut this and connect it to GAL G1 Pin9, connect U3-6 to U4-6 (HC245).
Now cut the trace from G1-19 (OVMA) to U4-6 on the upper side above G1.
Finally connect A8 from the CPU Socket (i used the 6809 socket Pin16) to G1-19.

### Firmware: ###

The Rom starts @ E000 but is accessed @ E800 where the Exordisk Firmware sits. At F000 is the Exbug Monitor. At FCF8-FCFF is the original PROM and the 6820. Here the Address FCFD is important, it holds the Initialisation Value for the MC6850.
I have a Github page for Reverse Engineering the MDOS Operating System and the Exbug Monitor.
For the extended firmware the ROM has now also ED00-EFFF.

### Memory Map: ###

see the Exorciser User Guide pg. 31 (3-4). With Exordisk there is a little less RAM useable since the ROM and IO sit @ E800-EFFF.

          ---------------
    FE00 |   RAM         | (originally from FF00)
    FCFC |   PROM        |
    FCF8 |   PIA         |
    FCF4 |   ACIA        |
    FC00 | IO Area       |
    F000 | Exbug ROM     |
    EC00 | Exordisk IO   |
    E800 | Exordisk ROM  |
    E7FF |---------------|
         |               |
         |   Exorciser   |
         |      RAM      |
         |               |
    0000  ---------------

