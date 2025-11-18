# Make z80 binary from asm source with z88dk tools

# system agnostic commands
ifdef ComSpec
	RMF	:= del /f /q
	RMD	:=
	SEARCH	:= find
	CP	:= copy /b
	GETDATE	:= powershell get-date -format "{yyyy-MM-dd}" 
	QUOTE	:= "
	/	:= $(strip \)
else
	RMF	:= rm -f 
	RMD	:= /*
	SEARCH	:= grep
	CP	:= cp
	GETDATE	:= date +'%Y-%m-%d'
	QUOTE	:= '"'
	/	:= /
endif 

BASE := ../base


all:	msx100 hbmsx rcmsx

rdate:
	@echo db $(QUOTE)$(shell $(GETDATE))$(QUOTE) > rdate.inc

msx100:
	@echo Assembling Baseline MSX
	z80asm -I$(BASE) -DMSX100 -b -l -m -Oobj -o=bios.bin bios.asm
	z80asm -I$(BASE) -DMSX100 -b -l -m -Oobj -o=basic.bin basic.asm
	z88dk-appmake +rom -b obj/bios.bin -o obj$/base.tmp -s 32768 --org 0
	z88dk-appmake +inject -b obj$/base.tmp -i obj$/basic.bin -s 9856 -o bin$/msx1_base.img
	@echo done

hbmsx:	rdate
	@echo Assembling MSX standard settings
	$(RMF) obj$/hbdos$(RMD)
	z80asm -DHBMSX -b -d -l -m -Oobj -o=hbdos1 hbdos/main.asm hbdos/disk.asm hbdos/sys.asm
	z88dk-appmake +glue -b obj/hbdos1 --filler 0xFF --clean
	z80asm -I$(BASE) -DHBMSX -b -l -m -Oobj -o=bios.bin bios.asm
	z80asm -I$(BASE) -DHBMSX -b -l -m -Oobj -o=basic.bin basic.asm
	z80asm -I$(BASE) -DHBMSX -b -l -m -Oobj hbmsx.asm -o=hbmsx.com
	$(CP) obj$/hbmsx.com bin$/hbmsx.com
	@echo done

rcmsx:	rdate
	@echo Assembling MSX customized settings
	$(RMF) obj$/hbdos$(RMD)
	z80asm -DRCMSX -b -d -l -m -Oobj -o=hbdos1 hbdos/main.asm hbdos/disk.asm hbdos/sys.asm
	z88dk-appmake +glue -b obj/hbdos1 --filler 0xFF --clean
	z80asm -I$(BASE) -DRCMSX -b -l -m -Oobj -o=bios.bin bios.asm
	z80asm -I$(BASE) -DRCMSX -b -l -m -Oobj -o=basic.bin basic.asm
	z80asm -I$(BASE) -DRCMSX -b -l -m -Oobj hbmsx.asm -o=rcmsx.com
	$(CP) obj$/rcmsx.com bin$/rcmsx.com
	@echo done

clean:
	$(RMF) obj$(RMD)

