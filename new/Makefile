# CONFIG
KERNELENTRY		:= 0x30400

# FLAG
ASM				:= nasm
ASMBOOTFLAGS	:= -I boot/include/ # must has the tail /
ASMKERNELFLAGS	:= -I include/ -f elf  # must has the tail /
#ASMDEPSFLAGS	:= -M

DASM			:= ndisasm
DASMFLAGS		:=

CC 				:= gcc
CFLAGS 			:= -I include/ -c -fno-builtin -m32

LD 				:= ld
LDFLAGS 		:= -Ttext $(KERNELENTRY) -m elf_i386 -s

BOOTDIR			:= boot
KERNELDIR		:= kernel
LIBDIR			:= lib

# OUTPUT
BOOT			:= $(BOOTDIR)/boot.bin
LOADER			:= $(BOOTDIR)/loader.bin
BOOTLOADER		:= $(BOOT) $(LOADER)

KERNEL			:= $(KERNELDIR)/kernel.bin

ASMOBJS			:= $(KERNELDIR)/kernel.o $(LIBDIR)/klib.o
COBJS			:= $(KERNELDIR)/start.o 
OBJS			:= $(ASMOBJS) $(COBJS)

IMAGE			:= floppy.img
MOUNTPOINT		:= floppy
INSTALL			:= $(MOUNTPOINT)/loader.bin

all: prepare $(INSTALL) umount

prepare:

$(BOOTLOADER): %.bin: %.asm
	$(ASM) $(ASMBOOTFLAGS) -o $@ $<

$(ASMOBJS): %.o: %.asm
	$(ASM) $(ASMKERNELFLAGS) -o $@ $<

$(COBJS): %.o: %.c
	$(CC) $(CFLAGS) -o $@ $<

$(KERNEL): $(OBJS)
	$(LD) $(LDFLAGS) -o $(KERNEL) $(OBJS)

$(IMAGE): $(BOOTLOADER) $(KERNEL)
	# create raw img
	dd if=/dev/zero of=$(IMAGE) bs=512 count=2880 # 1440 KB
	# make fs
	mkfs -t vfat -F 12 $(IMAGE)
	# overwrite boot sector. Notice: Don't do this when mounted
	dd if=$(BOOT) of=$(IMAGE) bs=512 count=1 conv=notrunc

mount: $(IMAGE)
	bash -c "if [[ ! -d $(MOUNTPOINT) ]]; then mkdir $(MOUNTPOINT); fi"
	mount $(IMAGE) $(MOUNTPOINT) -o loop -t vfat

$(INSTALL): mount
	/bin/cp -f $(LOADER) $(MOUNTPOINT)
	/bin/cp -f $(KERNEL) $(MOUNTPOINT)

umount:
	umount $(IMAGE)
	-rm -rf $(MOUNTPOINT)

clean:
	-rm -rf $(BOOTLOADER)
	-rm -rf $(KERNEL)
	-rm -rf $(OBJS)

disasm:

tar:
	tar -zcf myos.tar.gz ../../myos/

.PHONY: prepare mount umount clean debug disasm
