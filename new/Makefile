ASM			:= nasm
ASMFLAGS	:= -I boot/include/		# must has the tail /

DASM		:= ndisasm
DASMFLAGS	:=

CC 			:= gcc
CFLAGS 		:=

LS 			:= ld
LDFLAGS 	:=

IMAGE		:= floppy.img
IMAGEDIR	:= floppy

all: prepare writeFloppy floppy/loader.bin

prepare:
	bash -c "if [[ ! -d ./bin ]]; then mkdir ./bin; fi"

bin/boot.bin: boot/boot.asm boot/include/fat12.inc boot/include/load.inc
	$(ASM) $(ASMFLAGS) -o $@ $<

bin/loader.bin: boot/loader.asm
	$(ASM) $(ASMFLAGS) -o $@ $<

$(IMAGE):
	# create raw img
	dd if=/dev/zero of=$(IMAGE) bs=512 count=2880 # 1440 KB
	# make fs
	mkfs -t vfat -F 12 $(IMAGE)

floppy/loader.bin: mountFloppy bin/loader.bin
	cp bin/loader.bin ./floppy

writeFloppy: $(IMAGE) bin/boot.bin
	dd if=bin/boot.bin of=$(IMAGE) bs=512 count=1 conv=notrunc

mountFloppy: $(IMAGE)
	bash -c "if [[ ! -d ./floppy ]]; then mkdir ./floppy; fi"
	mount $(IMAGE) $(IMAGEDIR) -o loop -t vfat

umountFloppy: $(IMAGE)
	umount $(IMAGE)
	rm -rf ./floppy

clean:
	rm -rf ./bin
	rm -f $(IMAGE)

tar:
	tar -zcf myos.tar.gz ../../myos/

.PHONY: clean writeFloppy mountFloppy
