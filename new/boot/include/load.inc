; space for loader file, temporary for FAT root dir
; this space at least allocate one sector (>= 512)
; max space rely on loader file size
SegmentOfLoader equ 0x9000
OffsetOfLoader  equ 0x0100

; space for FAT, load FAT sectors in pairs, so 1KB is enough
SegmentOfFAT    equ 0x8000
OffsetOfFAT     equ 0x0100

SegmentOfKernel equ 0x8000
OffsetOfKernel  equ 0x0000

