; --------------------------------------------
; BOOT
; --------------------------------------------


; --------------------------------------------
; LOADER
; space for loader file, temporary for one sector of FAT root dir
;   this space at least allocate one sector (>= 512)
;   max space rely on loader file size
;   also consider loader stack space, now I set it same to OffsetOfLoader, so make sure there is also enough space below this address
;   (or enough space below the top of this segment if offset=0x0000 or so)
; --------------------------------------------
SegmentOfLoader equ 0x9000
OffsetOfLoader  equ 0x0100

; --------------------------------------------
; FAT
; space for FAT, load FAT sectors in pairs, so 1KB is enough
; --------------------------------------------
SegmentOfFAT    equ 0x8000
OffsetOfFAT     equ 0x0000

; --------------------------------------------
; KERNEL
; --------------------------------------------
; space for kernel file, temporary for one sector of FAT root dir
;   this space at least allocate one sector (>= 512)
;   max space rely on kernel file size
;   also consider kernel stack space, now I set it same to OffsetOfKernel, so make sure there is also enough space below this address
;   (or enough space below the top of this segment if offset=0x0000 or so)
SegmentOfKernel equ 0x7000
OffsetOfKernel  equ 0x0100
KernelEntryPoint    equ 0x30400                 ; Notice: must same with gcc -Ttext
                                                ; it is e_entry in elf header, means vaddr of entry

KernelPageDirBase   equ 0x200000                ; physical addr
                                                ; size=4KB, and must be 12 bit align (low 12 bit all zero)
KernelPageTblBase   equ 0x201000


