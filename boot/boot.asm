%include "fat12.inc"
%include "load.inc"

SECTION boot vstart=0x00007c00

; FAT12 header
BS_BPB LABEL_START

LABEL_START:
    ; --------------------------------------------
    ; BIOS to MBR interface
    ; https://en.wikipedia.org/wiki/Master_boot_record#BIOS_to_MBR_interface
    ; --------------------------------------------
    ; cs:ip = 0x0000:0x7c00
    ; dl = boot drive unit
    ;  (fixed disks / removable drives: 0x80=first, 0x81=second, ..., 0xfe;
    ;  floppies / superfloppies: 0x00=first, 0x01=second, ..., 0x7e;
    ;  0xff, 0x7f are reserved for ROM / remote drives and must not be used on disk.
    ;  Many MBRs were coded to ignore the DL value and work with a hard-wired value (normally 0x80), anyway.)
    ; dh bit 5 = 0

    ; init segment register
    mov ax, cs
    mov ds, ax
    mov es, ax
    mov ss, ax
    mov sp, 0x7c00

    cli                                         ; close response to maskable hardware interrupt

    ; clear entire window (not necessary, comment out to save space)
    mov ax, 0x0600                              ; clear entire window
    mov bh, 0x07                                ; attr for blank to write
    mov cx, 0                                   ; windows upper left corner (0,0)
    mov dx, 0x184f                              ; windows lower right corner (24, 79)
    int 0x10

    ; show booting msg
    mov cx, 12
    mov bp, BootingMsg
    call dispStr

    ; reset 1st floppy disk
    ; The read/write arm is moved to cylinder 0 and prepares for the disk I/O
    xor ah, ah
    xor dl, dl
    int 0x13

    ; search loader.bin in root dir
    mov cx, RootDirSectors                      ; total sectors/remained sectors to read
    mov ax, FirstRootDirSector                  ; start sector/sector to read
.LABEL_SEARCH_ROOT_DIR_NEXT_SECTOR:
    pusha                                       ; actually only need push cx, ax, here choose to exchange time for space

    ; read a sector to SegmentOfLoader:OffsetOfLoader
    push word SegmentOfLoader                   ; push params
    push word OffsetOfLoader
    push word 1
    push ax
    call readSector

    mov cx, 16                                  ; total dir entries per sector/remained entries to read
.LABEL_SEARCH_NEXT_DIR_ENTRY:
    push cx

    mov ax, cx
    neg al                                      ; neg: complement negation, reverse bit and plus 1
    and ax, 0xf                                 ; current entry=16-cx

    mov bl, 32
    mul bl
    add ax, OffsetOfLoader                      ; entry offset relative to SegmentOfLoader
    mov di, ax
    mov ax, SegmentOfLoader
    mov es, ax                                  ; es:di point to entry

    mov cx, 11                                  ; total bytes per entry name/remained bytes to read
    mov si, LoaderFileName                      ; ds:si point to const LoaderFileName
.LABEL_SEARCH_NEXT_BYTE:
    push cx

    cld
    lodsb                                       ; load a byte from ds:si to al, si++
    cmp al, byte [es:di]
    jnz .LABEL_SEARCH_NEXT_BYTE_BREAK           ; find different byte, this entry is wrong

    inc di                                      ; di++
    pop cx
    loop .LABEL_SEARCH_NEXT_BYTE                ; --remained bytes

.LABEL_SEARCH_NEXT_BYTE_BREAK:
    jcxz .LABEL_NAME_FOUND                      ; if cx=0 means this name found
    add sp, 2                                   ; there is a cx pushed but not pop yet because of break in LABEL_SEARCH_NEXT_BYTE loop
    pop cx
    loop .LABEL_SEARCH_NEXT_DIR_ENTRY           ; --remained entries

    popa
    inc ax                                      ; point to next sector
    loop .LABEL_SEARCH_ROOT_DIR_NEXT_SECTOR     ; --remained sectors

    ; read out all sectors but not find loader
.LABEL_NO_LOADER:
    ; show no loader msg
    ; Notice: here es has been change in upper functions, need to re-assign
    mov cx, 11
    xor ax, ax
    mov es, ax
    mov bp, NoLoaderMsg
    call dispStr
    jmp SUSPEND

.LABEL_NAME_FOUND:
    add sp, 2+16                                ; cx of LABEL_SEARCH_NEXT_ENTRY and
                                                ; all 16-bit registers of LABEL_SEARCH_ROOT_DIR_NEXT_SECTOR
                                                ; pushed but not pop
    ; now es:di point after this entry name's end
    add di, 0xf                                 ; es:(di-11) point to this entry start
                                                ; es:(di-11+26) point to entry's DIR_FstClusLO

    ; read entire file into SegmentOfLoader:OffsetOfLoader
    push word SegmentOfLoader
    push word OffsetOfLoader
    push word [es:di]                           ; push param first cluster
    call readFile

    ; show boot ready msg
    ; Notice: here es has been change in upper functions, need to re-assign
    mov cx, 12
    xor ax, ax
    mov es, ax
    mov bp, BootReadyMsg
    call dispStr

    ; jmp to loader code
    jmp SegmentOfLoader:OffsetOfLoader

SUSPEND:
    hlt
    jmp SUSPEND

; --------------------------------------------
; FAT related functions, use macro to import here
; --------------------------------------------
FAT_UTILS

; --------------------------------------------
; procedure: dispStr
; param:
;       cx = number of chars in string
;       es:bp = string to write
;       [CurrentRow] = row to write
;       [CurrentCol] = col to write
; --------------------------------------------
dispStr:
    mov ax, 0x1301                              ; update cursor after writing
    mov bx, 0x0007                              ; page number=0 attr=0x07
    mov dh, [CurrentRow]
    xor dl, dl
    int 0x10

    inc byte [CurrentRow]
    ret

; --------------------------------------------
; data
; --------------------------------------------
LoaderFileName db 'LOADER  BIN'                 ; FAT short directory entry's DIR_Name field
                                                ; must be 11 bytes and all in upper case
CurrentRow:    db 0                             ; row number to display str
BootingMsg:    db "Booting...",0x0d,0x0a
BootReadyMsg:  db "Boot Ready",0x0d,0x0a
NoLoaderMsg:   db "No loader",0x0d,0x0a

times 510-($-$$) db 0
                 db 0x55, 0xaa
