%include "fat12.inc"
%include "load.inc"

SECTION mbr vstart=0x00007c00
; FAT12 header
BS_BPB LABEL_START


LABEL_START:
    ; --------------------------------------------
    ; BIOS to MBR interface
    ; --------------------------------------------
    ; cs:ip = 0x0000:0x7c00
    ; dl = boot drive unit
    ;  (fixed disks / removable drives: 0x80=first, 0x81=second, ..., 0xfe;
    ;  floppies / superfloppies: 0x00=first, 0x01=second, ..., 0x7e;
    ;  0xff, 0x7f are reserved for ROM / remote drives and must not be used on disk)
    ; dh bit 5 = 0

    ; init segment register
    mov ax, cs
    mov ds, ax
    mov es, ax
    mov ss, ax
    mov sp, 0x7c00

    ; clear entire window (not necessary, comment out to save space)
    ;mov ax, 0x0600                              ; clear entire window
    ;mov bh, 0x07                                ; attr for blank to write
    ;mov cx, 0                                   ; windows upper left corner (0,0)
    ;mov dx, 0x184f                              ; windows lower right corner (24, 79)
    ;int 0x10

    ; show booting msg
    mov cx, 12
    mov bp, BootingMsg
    call dispStr

    ; reset 1st floppy disk
    ; TODO why need this
    xor ah, ah
    xor dl, dl
    int 0x13

    ; search loader.bin in root dir
    ; TODO pack all below to a function: read file into buf by name
    mov cx, RootDirSectors                      ; total sectors/remained sectors to read
    mov ax, FirstRootDirSector                  ; start sector/sector to read
LABEL_SEARCH_ROOT_DIR_NEXT_SECTOR:
    pusha                                       ; actually only need push cx, ax, here choose to exchange time for space

    ; read a sector to SegmentOfLoader:OffsetOfLoader
    push word SegmentOfLoader                        ; push params
    push word OffsetOfLoader
    push byte 1
    push ax
    call readSector
    add sp, 7

    mov cx, 16                                  ; total dir entries per sector/remained entries to read
LABEL_SEARCH_NEXT_DIR_ENTRY:
    push cx

    mov ax, cx
    neg al                                      ; current entry=16-cx (cx<=16)=16-cl
                                                ; complement negation, equals to al=16-cl

    mov bl, 32
    mul bl
    add ax, OffsetOfLoader                      ; entry offset relative to SegmentOfLoader
    mov di, ax
    mov ax, SegmentOfLoader
    mov es, ax                                  ; es:di point to entry

    mov cx, 11                                  ; total bytes per entry name/remained bytes to read
LABEL_SEARCH_NEXT_BYTE:
    push cx

    mov si, LoaderFileName                      ; ds:si point to const LoaderFileName
    cld
    lodsb                                       ; load a byte from ds:si to al, si++
    cmp al, byte [es:di]
    jnz LABEL_SEARCH_NEXT_BYTE_BREAK            ; find different byte, this entry is wrong

    inc di                                      ; di++
    pop cx
    loop LABEL_SEARCH_NEXT_BYTE                 ; --remained bytes

LABEL_SEARCH_NEXT_BYTE_BREAK:
    jcxz LABEL_NAME_FOUND                       ; if cx=0 means this name found
    add sp, 2                                   ; there is a cx pushed but not pop yet because of break in LABEL_SEARCH_NEXT_BYTE loop
    pop cx
    loop LABEL_SEARCH_NEXT_DIR_ENTRY            ; --remained entries

    popa
    inc ax                                      ; point to next sector
    loop LABEL_SEARCH_ROOT_DIR_NEXT_SECTOR      ; --remained sectors

    ; read out all sectors but not find loader
LABEL_NO_LOADER:
    ; show no loader msg
    mov cx, 11
    mov bp, NoLoaderMsg
    call dispStr
    jmp $                                       ; TODO or hlt

LABEL_NAME_FOUND:
    add sp, 2+4                                 ; cx of LABEL_SEARCH_NEXT_ENTRY and
                                                ; cx, ax of LABEL_SEARCH_ROOT_DIR_NEXT_SECTOR
                                                ; pushed but not pop
    ; now es:di point after this entry name's end
    add di, 0xf                                 ; es:(di-11) point to this entry start
                                                ; es:(di-11+26) point to entry's DIR_FstClusLO

    ; read entire file into SegmentOfLoader:OffsetOfLoader
    push word SegmentOfLoader
    push word OffsetOfLoader
    push word [es:di]                           ; push param first cluster
    call readFile
    add sp, 6

    ; show boot ready msg
    mov cx, 12
    mov bp, BootReadyMsg
    call dispStr

    ; jmp to loader code
    jmp SegmentOfLoader:OffsetOfLoader


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
; procedure: hex2Ascii
; param:
;       bl = 8 bit num
; return:
;       ax = 2 ascii char
; example: bl=0x0001 => ax=0x3130
; --------------------------------------------
hex2Ascii:
    mov cx, 2                                   ; loop count, parse 4 bit once
.loop:
    rol bl, 4
    mov al, 0x0f
    and al, bl
    add al, 0x90
    daa
    adc al, 0x40
    daa
    rol ax, 8                                   ; now al stores ascii, shift it to ah
    loop .loop
    ret


; --------------------------------------------
; data
; --------------------------------------------
LoaderFileName db 'LOADER  BIN'                 ; FAT short directory entry's DIR_Name field
                                                ; must be 11 bytes and all in upper case
CurrentRow:    db 0                             ; row number to display str
BootingMsg:    db "Booting...",0x0d,0x0a
BootReadyMsg:  db "Boot ready",0x0d,0x0a
NoLoaderMsg:   db "No loader",0x0d,0x0a
;StrBuf:        times 16 db 0


times 510-($-$$) db 0
                 db 0x55, 0xaa
