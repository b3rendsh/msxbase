# MSX for RomWBW

MSX system software for retrocomputers with RomWBW HBIOS.

The MSX BIOS and DOS are modified to work with HBIOS functions for disk, console, date/time and memory bank switching. 

BASIC graphics commands and the full screen editor use direct TMS videocard i/o via the MSX BIOS.  
BASIC sound commands use direct AY-3-8910 PSG i/o via the MSX BIOS.

The i/o ports and other settings can be configured in a build configuration file.

## Hardware requirements

HBDOS can be used on a Z80 or Z180 RomWBW compatible computer with at least 192KB RAM and support for a system timer.

MSX BASIC and the MSX ROM loader require a TMS9918A compatible video card, system timer and 128KB RAM.

## Usage

Review build options in the base.inc file.  
Build the CP/M loader program with z88dk.  
Prebuilt binaries with default options are available in the bin folder.

Copy COMMAND.COM to the root directory of the first FAT12/FAT16 partition on the disk.

Start hbmsx.com or rcmsx.com from RomWBW CP/M to load HBDOS / Disk BASIC.

Start msxrom.com from RomWBW CP/M to load a MSX (game) ROM image e.g. "msxrom arkanoid.rom".

## HBDOS

HBDOS is compatible with all functions of MSX-DOS 1 and includes enhancements to support large disks with standard FAT12 or FAT16 partitions.

It is a CP/M 2.2 work-alike DOS that uses the FAT filesystem. Many text applications that work on MSX-DOS 1 or (RomWBW) CP/M 2.2 will run without modification. Direct disk access, FAT32, i/o byte, user areas and subdirectories are not supported.

With the default disk setting the first large HBIOS disk unit is used and all FAT partitions on the disk are mapped to a drive letter, with a maximum of 8 drives.

The standard MSX-DOS 1 command interpreter can be used (COMMAND.COM) with HBDOS. MSXDOS.SYS is not used.

At the DOS command prompt enter "basic" to start MSX BASIC.

## BASIC

All functions of MSX 1 BASIC are available, if HBDOS is loaded then the Disk BASIC 1 extension is also available.

If the MSXBOOT build option is set to 0 then only MSX BASIC will be loaded, DOS and Disk BASIC commands won't be available but BASIC will have more free memory.

Use the IPL command to return to RomWBW i.e. do a cold reboot.

In Disk BASIC use "call system" to return to the DOS command prompt.

## ROM CART

The MSX ROM loader supports MSX ROM cartridge images of maximum 32KB.

Not all MSX ROM games will work and some games may require an additional ROM patch or additional hardware.

When a MSX ROM is running you can reboot RomWBW by pressing the CTRL+STOP key (default mapped to CTRL+V).

## Console

The RomWBW (VT100) console can be used for keyboard input and screen output, with some limitations:

Cursor and function keys may not work, use control key combinations or a MSX compatible keyboard. See console.inc for cursor key mapping.

To paste text into BASIC set the terminal send character delay to at least 40ms.

The MSX BIOS uses VT52 escape sequences, on a VT100 console sometimes an extra character is displayed.

The MSX 1 BIOS text mode is set to 40 columns.


## Disclaimer

The binaries and sources are provided freely and "as it is" in the hope that it will be useful, but without any warranty of any kind, either expressed or implied. Use at own risk!
