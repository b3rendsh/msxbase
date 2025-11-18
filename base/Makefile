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

all:	bios1 basic1 img1 bios2 basic2 subrom img2

bios1:
	@echo Assembling MSX1 BIOS
	z80asm -DMSX1 -b -l -m -Oobj -o=bios1.bin bios.asm
	$(CP) obj$/bios1.bin bin$/msx1_bios.bin
	@echo done

basic1:
	@echo Assembling MSX1 BASIC
	z80asm -DMSX1 -b -l -m -Oobj -o=basic1.bin basic.asm
	$(CP) obj$/basic1.bin bin$/msx1_basic.bin
	@echo done

img1:	
	@echo Creating 32K MSX1 BASE image
	z88dk-appmake +rom -b obj/bios1.bin -o obj$/base.tmp -s 32768 --org 0
	z88dk-appmake +inject -b obj$/base.tmp -i obj$/basic1.bin -s 9856 -o bin$/msx1_base.img
	@echo done

bios2:
	@echo Assembling MSX2 BIOS
	z80asm -DMSX2 -b -l -m -Oobj -o=bios2.bin bios.asm
	$(CP) obj$/bios2.bin bin$/msx2_bios.bin
	@echo done

basic2:
	@echo Assembling MSX2 BASIC
	z80asm -DMSX2 -b -l -m -Oobj -o=basic2.bin basic.asm
	$(CP) obj$/basic2.bin bin$/msx2_basic.bin
	@echo done
	
subrom:
	@echo Assembling MSX2 SUBROM
	z80asm -DMSX2 -b -l -m -Oobj -o=subrom.bin subrom.asm
	$(CP) obj$/subrom.bin bin$/msx2_subrom.bin
	@echo done

img2:	
	@echo Creating 32K MSX2 BASE image
	z88dk-appmake +rom -b obj/bios2.bin -o obj$/base.tmp -s 32768 --org 0
	z88dk-appmake +inject -b obj$/base.tmp -i obj$/basic2.bin -s 9856 -o bin$/msx2_base.img
	@echo done

clean:
	$(RMF) obj$(RMD)

