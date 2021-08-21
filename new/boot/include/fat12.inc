; A FAT file system volume is composed of four basic regions, which are laid out in this order on the volume:
; 0 - Reserved Region
; 1 - FAT Region
; 2 - Root Directory Region (doesn't exist on FAT32 volumes)
; 3 - File and Directory Data Region

; BS values
DrvNum        equ 0                             ; int 0x13 drive number. 0x00 for floppy disks, 0x80 for hard disks

; BPB values
; define const, to read fields in other file without load BS_BPB
BytsPerSec    equ 512                           ; count of bytes per sector
SecPerClus    equ 1                             ; number of sectors per allocation unit
RsvdSecCnt    equ 1                             ; number of reserved sectors in the Reserved region of the volume starting at the first sector of the volume
NumFATs       equ 2                             ; count of FAT data structures on the volume
RootEntCnt    equ 224                           ; count of 32-byte directory entries in the root directory
                                                ; set 224 means root entry occupies 14 sectors
TotSec16      equ 2880                          ; total count of sectors in all four regions of the volume
                                                ; 2880 * 512 = 1440 KB, standard size of floppy
Media         equ 0xf0                          ; removable media
FATSz16       equ 9                             ; count of sectors occupied by ONE FAT
                                                ; set 9 to make sure FAT is enough for all clusters, every entry in FAT12 is 12 bit
SecPerTrk     equ 18                            ; sectors per track for int 0x13
NumHeads      equ 2                             ; number of heads for int 0x13
HiddSec       equ 0                             ; This field is generally only relevant for media visible on interrupt 0x13
                                                ; This field should always be zero on media that are not partitioned
                                                ; Exactly what value is appropriate is operating system specific
TotSec32      equ 0                             ; FAT12 use BPB_ToSec16 instead of this field

; inference
FirstFAT1Sector    equ RsvdSecCnt                                             ; first sector of FAT1 = 1

FirstRootDirSector equ (RsvdSecCnt + (NumFATs * FATSz16))                     ; first sector of root dir = 19
RootDirSectors     equ ((RootEntCnt * 32) + BytsPerSec - 1) / BytsPerSec      ; sectors occupied by root dir = 14

FirstDataSector    equ FirstRootDirSector + RootDirSectors                    ; first sector of data = 33
DataSec            equ TotSec16 - FirstDataSector                             ; sectors occupied by data = 2847

CountofClusters    equ DataSec / SecPerClus                                   ; clusters occupied by data = 2847
                                                                              ; CountofCluster < 4085 means determine the type of this volume is FAT12
MaxCluster         equ CountofClusters + 1                                    ; maximum valid cluster number = 2848

%macro FAT_UTILS 0
; --------------------------------------------
; function: void readSector(uint16 sector, uint8 count, uint32 bufPtr);
; param:
;       sector: start sector to read (LBA)
;       count = count of sectors to read
;       bufPtr = destination to store sectors, push seq: seg, offset
; --------------------------------------------
readSector:
    push bp
    mov bp, sp
    sub sp, 0x40

    ; LBA2CHS procedure, not abstract to a function to save space
.LBA2CHS:
    mov bx, SecPerTrk
    xor dx, dx
    mov ax, word [bp+4]                         ; get param LBA
    div bx                                      ; LBA/SecPerTrk => ax=C*NumHeads+H, dx=S-1
    inc dx                                      ; dx=S
    mov [bp-2], dx                              ; local var S

    mov bx, NumHeads
    xor dx, dx
    div bx                                      ; (C*NumHeads+H)/NumHeads => ax=C, dx=H
    mov [bp-4], dx                              ; local var H
    mov [bp-6], ax                              ; local var C

    ; int 1302
    mov ch, byte [bp-6]
    mov cl, byte [bp-5]
    ror cl, 2
    and cl, byte [bp-2]
    mov dh, byte [bp-4]
    mov dl, DrvNum
    mov ax, word [bp+9]
    mov es, ax
    mov bx, word [bp+7]
.retry:
    mov ah, 2
    mov al, byte [bp+6]
    int 0x13
    jc .retry                                   ; CF set on error, clear if no error

    mov sp, bp
    pop bp
    ret


; ------------------------------------------------
; function: uint16 getFATByCluster(uint16 cluster);
; return FAT entry (12bit)
; ------------------------------------------------
getFATByCluster:
    push bp
    mov bp, sp
    sub sp, 0xf                                 ; 15 bytes of local stack space

    mov ax, word [bp+4]                         ; get param cluster
    mov bx, ax
    shr bx, 1
    add ax, bx                                  ; FATOffset=cluster+(cluster/2)

    xor dx, dx
    mov bx, BytsPerSec
    div bx                                      ; FATOffset/BPB_BytsPerSec
                                                ; quotient=ax remainder=dx
    add ax, RsvdSecCnt                          ; ax=ThisFATSecNum
                                                ; dx=ThisFATEntOffset
    mov [bp-2], dx                              ; local var ThisFATEntOffset

    ; load FAT sectors in pairs, because FAT12 entry may across two FAT sectors
    push word SegmentOfFAT
    push word OffsetOfFAT
    push byte 2
    push ax
    call readSector
    add sp, 7

    ; get FAT entry by cluster
    mov ax, SegmentOfFAT
    mov es, ax
    mov bx, OffsetOfFAT
    add bx, [bp-2]                              ; entry offset of SegmentOfFAT = OffsetOfFAT+ThisFATEntOffset
    mov ax, [es:bx]                             ; ax=2 bytes contain the 12 bit entry

    ; judge cluster is odd/even
    mov bx, word [bp+4]                         ; get param cluster
    and bx, 0x0001
    jz .even_branch
.odd_branch:
    ; if cluster is odd
    shr ax, 4                                   ; high 12 bits of the 16 bits is entry
    jmp .out_branch
.even_branch:
    ; if cluster is even
    and ax, 0x0fff                              ; low 12 bits of the 16 bits is entry
.out_branch:
    mov sp, bp
    pop bp
    ret

; ------------------------------------------------
; function: void readFile(uint16 firstCluster,  uint32 bufPtr);
; param:
;       firstCluster: the first data cluster of this file
;       bufPtr = destination to store file, push seq: seg, offset
; ------------------------------------------------
readFile:
    push bp
    mov bp, sp
    sub sp, 0xf                                 ; 15 bytes of local stack

.read_next_cluster:
    ; procedure: getSectorByCluster, not abstract as function to save space
    ; sector = (cluster - 2) * BPB_SecPerClus + FirstDataSector
    mov ax, [bp+4]                              ; get param cluster
    sub ax, 2
    mov bx, SecPerClus
    mul bx
    add ax, FirstDataSector                     ; get first sector of this cluster

    push word [bp+8]                            ; get param bufPtr seg, then push it
    push word [bp+6]                            ; get param next offset to write, then push it
    push byte 1
    push ax
    call readSector
    add sp, 7

    ; get next cluster
    push word [bp+4]
    call getFATByCluster
    add sp, 2

    mov word [bp+4], ax                         ; update next cluster, directly update param space to save space
    add word [bp+6], BytsPerSec                 ; offset add one sector size

    cmp ax, 0x0ff8                              ; FAT content >= 0x0ff8 means EOF
    jl .read_next_cluster

    mov sp, bp
    pop bp
    ret

%endmacro

; ------------------------------------------------
; Boot Sector & BIOS Parameter Block (BS BPB)
; 8 bytes in total
; usage:
;    BS_BPB boot_code_label
; Below init value is same as header made by `mkfs -t vfat -F 12`
; ------------------------------------------------

%macro BS_BPB 1
BS_jmpBoot:
    jmp short %1                                ; 0xEB cb
    nop                                         ; 0x90
BS_OEMName:
    db 'MSWIN4.1'                               ; recommended string, for compatibility

BPB_BytsPerSec:
    dw BytsPerSec
BPB_SecPerClus:
    db SecPerClus
BPB_RsvdSecCnt:
    dw RsvdSecCnt
BPB_NumFATs:
    db NumFATs
BPB_RootEntCnt:
    dw RootEntCnt
BPB_TotSec16:
    dw TotSec16
BPB_Media:
    db Media
BPB_FATSz16:
    dw FATSz16
BPB_SecPerTrk:
    dw SecPerTrk
BPB_NumHeads:
    dw NumHeads
BPB_HiddSec:
    dd HiddSec
BPB_TotSec32:
    dd TotSec32

BS_DrvNum:
    db DrvNum
BS_Reserved1:
    db 0                                        ; reserved
BS_BootSig:
    db 0x29                                     ; indicate that the following three fields in the boot sector are present
BS_VolID:
    dd 0                                        ; This ID is usually generated by simply combining the current date and time into a 32-bit value
BS_VolLab:
    db 'myos - zjm '                            ; volume label
BS_FilSysType:
    db 'FAT12   '
%endmacro
