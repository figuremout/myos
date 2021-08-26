; Follow caller clean-up convention (cdecl) from now on
%include "load.inc"
%include "fat12.inc"
%include "pm.inc"

OffsetOfLoaderStack equ OffsetOfLoader          ; loader stack grow down

SECTION loader vstart=OffsetOfLoader
LOADER_START:
    ; when entering this loader:
    ; cs:ip has been changed to SegmentOfLoader:OffsetOfLoader
    ; ss:sp point to 0x0000:0x7c00
    mov ax, cs
    mov ds, ax
    mov es, ax
    mov ss, ax
    mov sp, OffsetOfLoaderStack

    cli                                         ; interrupt system still not working

    ; show loading msg
    push cs
    push word LoadingMsg
    call putString_16
    add sp, 4

    ; search kernel.bin in root dir
    mov cx, RootDirSectors                      ; total sectors/remained sectors to read
    mov ax, FirstRootDirSector                  ; start sector/sector to read
.LABEL_SEARCH_ROOT_DIR_NEXT_SECTOR:
    pusha                                       ; actually only need push cx, ax, here choose to exchange time for space

    ; read a sector to SegmentOfKernel:OffsetOfKernel
    push word SegmentOfKernel                   ; push params
    push word OffsetOfKernel
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
    add ax, OffsetOfKernel                      ; entry offset relative to SegmentOfKernel
    mov di, ax
    mov ax, SegmentOfKernel
    mov es, ax                                  ; es:di point to entry

    mov cx, 11                                  ; total bytes per entry name/remained bytes to read
    mov si, KernelFileName                      ; ds:si point to const KernelFileName
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
.LABEL_NO_KERNEL:
    ; show no kernel msg
    push cs
    push word NoKernelMsg
    call putString_16
    add sp, 4

    jmp .SUSPEND

.LABEL_NAME_FOUND:
    add sp, 2+16                                ; cx of LABEL_SEARCH_NEXT_ENTRY and
                                                ; all 16-bit registers of LABEL_SEARCH_ROOT_DIR_NEXT_SECTOR
                                                ; pushed but not pop
    ; now es:di point after this entry name's end
    add di, 0xf                                 ; es:(di-11) point to this entry start
                                                ; es:(di-11+26) point to entry's DIR_FstClusLO

    ; read entire file into SegmentOfKernel:OffsetOfKernel
    push word SegmentOfKernel
    push word OffsetOfKernel
    push word [es:di]                           ; push param first cluster
    call readFile

    ; kill motor
    call killMotor                              ; otherwise driver light will stay on TODO

    ; show load ready msg
    ; Notice: here es has been change in upper functions, need to re-assign
    push cs
    push word LoadReadyMsg
    call putString_16
    add sp, 4

    ; get memory info
    mov ebx, 0                                  ; next ARDS number
    mov di, ARDSBuf                             ; es:di point to ARDSBuf
.memCheckLoop:
    mov eax, 0xe820                             ; func number
    mov ecx, 20                                 ; ARDS size, normally 20
    mov edx, 0x534d4150                         ; signature 'SMAP'
    int 0x15
    jc .memCheckErr
    add di, 20                                  ; es:di point to next space to store ARDS
    inc dword [ARDSCount]
    cmp ebx, 0                                  ; int 0x15 will update ebx to get next ARDS, if ebx=0 means this is the last ARDS
    jne .memCheckLoop
    jmp .memCheckDone
.memCheckErr:
    mov dword [ARDSCount], 0
.memCheckDone:

    ; show mem info title
    push cs
    push word ARDSTitle
    call putString_16
    add sp, 4


    ; show mem info
    call showMemInfo

    ; enter protect mode
    ; caculate segment & offset of GDT
    mov eax, [cs:gdt_ptr+2]                     ; get base addr of GDT
    xor edx, edx
    mov ebx, 16
    div ebx                                     ; eax=segment, edx=0ffset
    mov ds, eax
    mov ebx, edx                                ; ds:ebx point to GDT

    ; skip 0# desc
    ; create 1# desc, code segment
    mov dword [ebx+0x08], 0x0000ffff
    mov dword [ebx+0x0c], 0x00cf9800

    ; create 2# desc, data and stack segment
    mov dword [ebx+0x10], 0x0000ffff
    mov dword [ebx+0x14], 0x00cf9200

    ; init GDTR
    lgdt [cs:gdt_ptr]                           ; load limit and base addr of GDT to GDTR

    ; open A20 line
    in al, 0x92                                 ; read origin data
    or al, 0b000000_10                          ; bit 7-2 reserved. bit 1 is ALT_A20_GATE, set to open A20
                                                ; bit 0 is INIT_NOW, set to reset CPU (restart) ;
    out 0x92, al

    cli                                         ; TODO Notice: although never sti before, the instruction cli here still necessary
                                                ; otherwise error will occurs in virtualbox, not in bochs
    mov eax, cr0
    or eax, 1                                   ; bit 0 of cr0 is PE
    mov cr0, eax                                ; set PE, enter protect mode
    jmp dword 0x0008:(SegmentOfLoader*16+FLUSH) ; fresh pipeline and serialize CPU
                                                ; 0x0008 is desc selector: index=0b0000_0000_0000_1 (1# desc, code segment)
                                                ; TI=0, RPL=00

.SUSPEND:
    hlt
    jmp .SUSPEND

; --------------------------------------------
; FAT related functions, use macro to import here
; --------------------------------------------
FAT_UTILS

; --------------------------------------------
; Protect Mode related functions, use macro to import here
; --------------------------------------------
PM_UTILS

; --------------------------------------------
; killMotor
; TODO dont know why, seems to have little doc
; --------------------------------------------
killMotor:
    mov dx, 0x03f2
    xor al, al
    out dx, al
    ret

; --------------------------------------------
; function: void showMemInfo()
; --------------------------------------------
showMemInfo:
    push bp
    mov bp, sp

    mov si, ARDSBuf
    mov cx, [ARDSCount]
.nextRow:
    push cx
    mov cx, 5                                   ; every ARDS 5 members (columns)
.nextCol:
    push cx

    ; store this member into ARDSStruct
    mov ax, cs
    mov es, ax
    mov di, 5
    sub di, cx
    shl di, 2                                   
    add di, ARDSStruct                          ; di=(5-cx)*4+ARDSStruct point to next member field of ARDSStruct
                                                ; es:di point to one field of ARDSStruct
    mov eax, [si]                               ; get 32-bit member value
    stosd                                       ; store eax at es:di

    ; generate ascii, every member 32 bit
    mov byte [StrBuf], 0x20                     ; fill in 1 space at member head
    mov word [StrBuf+1], 0x7830                 ; fill in '0x'

    mov bl, [si+3]
    call hex2Ascii
    mov [StrBuf+3], ax

    mov bl, [si+2]
    call hex2Ascii
    mov [StrBuf+5], ax

    mov bl, [si+1]
    call hex2Ascii
    mov [StrBuf+7], ax

    mov bl, [si]
    call hex2Ascii
    mov [StrBuf+9], ax

    mov byte [StrBuf+11], 0x0020                ; fill in 1 space and \0 at member tail

    push si                                     ; keep si value
    ; show ascii
    push cs
    push word StrBuf
    call putString_16
    add sp, 4

    pop si
    add si, 4
    pop cx
    loop .nextCol
    ; show \n
    mov dword [StrBuf], 0x00000a0d
    push cs
    push word StrBuf
    call putString_16
    add sp, 4

    ; if Type==1 & (BaseAddrLow + LengthLow) >= MemLimit then update it
    ; Type: AddressRangeMemory=1, AddressRangeReserved=2
    cmp dword [ARDSStruct.Type], 1
    jne .notUpdate
    mov eax, [ARDSStruct.BaseAddrLow]
    add eax, [ARDSStruct.LengthLow]
    cmp eax, [MemLimit]
    jl .notUpdate
    mov [MemLimit], eax
.notUpdate:

    pop cx
    dec cx
    cmp cx, 0
    jnz .nextRow                                ; not use loop because loop can only short jmp, here need near jmp

    ; show MemLimit
    push cs
    push word MemLimitTitle
    call putString_16
    add sp, 4

    mov bl, [MemLimit+3]
    call hex2Ascii
    mov [StrBuf], ax

    mov bl, [MemLimit+2]
    call hex2Ascii
    mov [StrBuf+2], ax

    mov bl, [MemLimit+1]
    call hex2Ascii
    mov [StrBuf+4], ax

    mov bl, [MemLimit]
    call hex2Ascii
    mov [StrBuf+6], ax

    mov dword [StrBuf+8], 0x00000a0d
    push cs
    push word StrBuf
    call putString_16
    add sp, 4

    mov sp, bp
    pop bp
    ret

; --------------------------------------------
; procedure: hex2Ascii
; param:
;       bl = 8 bit num
; return:
;       ax = 2 ascii char
; example: bl=0x01 => ax=0x3130
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
; function: void putString_16(uint32 strPtr);
; 16-bit code: display string end with 0x00 and update cursor
; Notice: because this function will sti in the end, so don't use it before interrupt system inited
; Notice: now I comment out sti, to use it in real mode
; --------------------------------------------
putString_16:
    push bp
    mov bp, sp
    sub sp, 0x40

    mov ax, [bp+6]                              ; get param strPtr segment
    mov [bp-2], ax                              ; local var strPtr segment
    mov ax, [bp+4]                              ; get param strPtr offset
    mov [bp-4], ax                              ; local var strPtr offset
.getc:
    mov ax, [bp-2]                              ; get strPtr segment
    mov es, ax
    mov bx, [bp-4]                              ; get strPtr next char offset

    mov cl, [es:bx]                             ; read in a byte
    or cl, cl                                   ; check if it's 0
    jz .exit                                    ; end

    xor ch, ch
    push cx
    call putChar_16
    add sp, 2

    inc word [bp-4]                                  ; inc strPtr offset, point to next char
    jmp .getc

.exit:
    mov sp, bp
    pop bp
    ret

; --------------------------------------------
; function: void putChar_16(uint16 ascii);
; display a char at cursor and update cursor (VGA)
; param char only low 8 bit valid
; --------------------------------------------
putChar_16:
    push bp
    mov bp, sp

    push ds                                     ; better not change ds, may affect caller's context
    ; prepare segment register for video memory
    mov ax, 0xb800
    mov es, ax
    mov ds, ax

    ; get current cursor position
    mov dx, 0x03d4                              ; 0x03d4 CRT index port
    mov al, 0x0e                                ; index for cursor location hight 8 bit
    out dx, al
    inc dx                                      ; 0x03d5 CRT data port
    in al, dx                                   ; read cursor location high 8 bit
    shl ax, 8

    dec dx                                      ; 0x03d4
    mov al, 0x0f                                ; index for cursor location low 8 bit
    out dx, al
    inc dx                                      ; 0x03d5
    in al, dx                                   ; read cursor location low 8 bit
    mov bx, ax                                  ; bx=cursor position

    mov cl, [bp+4]                              ; get param char ascii
    cmp cl, 0x0d                                ; if CR
    jnz .if_0a
    ; char is CR
    mov bl, 80
    div bl                                      ; quotient=al=row remainder=ah=col
    mul bl                                      ; ax=al*80=row start position
    mov bx, ax
    jmp .set_cursor

.if_0a:
    ; char is not CR
    cmp cl,0x0a                                 ; if LF
    jnz .put_other
    ; char is LF
    add bx, 80                                  ; cursor position add a line
    jmp .roll_screen

.put_other:                                     ; display normal char
    shl bx, 1                                   ; trans cursor position into offset
    mov [bx], cl                                ; fill in ascii
    mov byte [bx+1], 0x07                       ; fill in attr

    shr bx, 1
    inc bx                                      ; cursor position++

.roll_screen:
    cmp bx, 2000                                ; if cursor out of screen
    jl .set_cursor

    ; copy chars from row 1-24 to row 0-23
    cld
    mov di,0x0000                               ; es:di point to 1st row start
    mov si,0x00a0                               ; ds:si point to 2nd row start

    mov cx, 1920                                ; copy a word(ascii, attr) once, 24 rows need to copy 24*80=1920 times
    rep movsw

    ; clear last row
    mov bx, 3840                                ; 1920*2=3840 last row(row 24) start offset
    mov cx, 80
.cls:
    mov word [bx],0x0720                        ; 0x0720 bg=black fg=white char=space
    add bx,2
    loop .cls
    mov bx, 1920                                ; cursor point to last row start

.set_cursor:
    mov dx, 0x03d4                              ; CRT index port
    mov al, 0x0e                                ; index for cursor location hight 8 bit
    out dx, al
    inc dx                                      ; 0x03d5 CRT data port
    mov al, bh
    out dx, al

    dec dx                                      ; 0x03d4
    mov al, 0x0f                                ; index for cursor location low 8 bit
    out dx, al
    inc dx                                      ; 0x03d5
    mov al, bl
    out dx, al

    pop ds
    mov sp, bp
    pop bp
    ret

; --------------------------------------------
; Below compile in 32-bit, MAKE SURE 16-bit code locate before here
; --------------------------------------------
    [bits 32]
FLUSH:
    ; PROTECT MODE
    mov ax, 0x0010                              ; 0x0010 is desc selector: index=0b0000_0000_0001_0 (2# desc, data segment)
                                                ; TI=0, RPL=00
    ; init ds, es, fs, gs, ss to data segment
    mov ds, ax
    mov es, ax
    mov fs, ax
    mov gs, ax
    mov ss, ax
    mov esp, 0x7000                             ; TODO why the stack here

    ; enter paging mode
.paging:
    ; caculate how many PTE needed accroding to MemLimit
    xor edx, edx
    mov eax, [SegmentOfLoader*16+MemLimit]
    mov ebx, 0x400000                           ; one PT can map 1024*4K=4MB memory
    div ebx                                     ; eax=PT num(or PDE num), edx=extra bytes
    test edx, edx                               ; test if remainder is 0
    jz .no_remainder
    inc eax                                     ; if has remainder, add one PT to store extra bytes
.no_remainder:
    mov ecx, eax                                ; PT num
    push eax                                    ; store PT num

    ; TODO for simplify, linear directly maps to physical, another method is map to high addr
    ; 目前采用对等映射，也就是这个导致生成的elf的线性地址必须被设定成一个小值，因为没有这么大内存
    ; init PD
    mov edi, KernelPageDirBase                  ; es:edi point to PD
    mov eax, KernelPageTblBase                  ; PDE: base(31-12), linear addr start from 0
    or eax, 0x007                               ; PDE: G=0, D=0, A=0, PCD=0, PWT=0, US=1, RW=1, P=1
.nextPDE:
    stosd                                       ; mov eax to es:edi
    add eax, 4096                               ; PDE point to next PT
    loop .nextPDE

    ; init all PT
    pop eax                                     ; restore PT num
    mov ebx, 1024                               ; 1024 PTE per PT
    mul ebx                                     ; eax=PTE num
    mov ecx, eax
    mov edi, KernelPageTblBase                  ; es:edi point to PT
    xor eax, eax                                ; PTE: base(31-12), phsical page start from 0
    or eax, 0x007                               ; PTE: G=0, D=0, A=0, PCD=0, PWT=0, US=1, RW=1, P=1
.nextPTE:
    stosd
    add eax, 4096                               ; PTE point to next physical page
    loop .nextPTE

    ; send PD physical addr to CR3(PDBR, Page Directory Base Register)
    mov eax, KernelPageDirBase                  ; base(31-12), PCD=0, PWT=0
    mov cr3, eax

    ; cr0 bit 31 is PG, paging mode is on when it set
    mov eax, cr0
    or eax, 0x80000000                          ; set PG
    mov cr0, eax

RELOCATE_KERNEL:
    xor esi, esi
    mov esi, [SegmentOfKernel*16+OffsetOfKernel+0x1c]
                                                ; get e_phoff from elf header
    add esi, SegmentOfKernel*16+OffsetOfKernel  ; ds:esi point to program header table's physical addr

    xor ebx, ebx
    mov bx, word [SegmentOfKernel*16+OffsetOfKernel+0x2a]
                                                ; get e_phentsize from elf header
    xor ecx, ecx
    mov cx, word [SegmentOfKernel*16+OffsetOfKernel+0x2c]
                                                ; get e_phnum from elf header

    ; polling every program header, relocate every segment
.nextPH:
    mov eax, [esi+0]                            ; get p_type from program header
    cmp eax, 0                                  ; if PT_NULL
    jz .noAction

    mov eax, [esi+0x04]                         ; get p_offset from program header
    add eax, SegmentOfKernel*16+OffsetOfKernel  ; eax is segment's physical addr

    push dword [esi+0x10]                       ; get p_filesz from program header and push as param size
    push eax                                    ; push as param pSrc
    push dword [esi+0x08]                       ; get p_vaddr from program header and push as pDest
                                                ; TODO that means use vaddr as physical addr
    call memcpy
    add esp, 12
.noAction:
    add esi, ebx                                ; add e_phentsize, esi point to next program header
    loop .nextPH

    jmp dword 0x0008:(KernelEntryPoint)         ; enter kernel code segment


; --------------------------------------------
; function: void* memcpy(void* es:pDest, void* ds:pSrc, int iSize);
; --------------------------------------------
memcpy:
    push ebp
    mov ebp, esp

    push esi
    push edi
    push ecx

    mov edi, [ebp+8]                            ; get param destination
    mov esi, [ebp+12]                           ; get param source
    mov ecx, [ebp+16]                           ; get param count
.nextByte:
    cmp ecx, 0
    jz .break
    mov al, [ds:esi]                            ; read in one byte from source
    inc esi
    mov byte [es:edi], al                       ; output one byte to destination
    inc edi
    loop .nextByte
.break:
    mov eax, [ebp+8]                            ; return destination
    
    pop ecx
    pop edi
    pop esi
    mov esp, ebp
    pop ebp
    ret

; --------------------------------------------
; data area
; --------------------------------------------
; ARDS(Address Range Descriptor Structure)
ARDSTitle:     db 0x0d,0x0a,'INT 0x15 AX=0xE802 Memory Map:',0x0d,0x0a
               db 'BaseAddrLow BaseAddrHigh LengthLow   LengthHigh     Type    ',0x0d,0x0a,0x00
ARDSCount:     dd 0
ARDSBuf:       times 256 db 0
ARDSStruct:
    .BaseAddrLow:   dd 0
    .BaseAddrHigh:  dd 0
    .LengthLow:     dd 0
    .LengthHigh:    dd 0
    .Type:          dd 0
MemLimitTitle:      db 'MemLimit: 0x',0x00
MemLimit:           dd 0

KernelFileName:db 'KERNEL  BIN'                 ; FAT short directory entry's DIR_Name field
                                                ; must be 11 bytes and all in upper case
LoadingMsg:    db 'Loading...',0x0d,0x0a,0x00
LoadReadyMsg:  db "Load Ready",0x0d,0x0a,0x00
NoKernelMsg:   db "No Kernel",0x0d,0x0a,0x00
EnterPmMsg:    db "Protect Mode",0x0d,0x0a,0x00

; TODO use macro value
gdt_ptr:          dw 23                         ; limit of GDT, use only 3 desc for now, every desc 8 bytes
               dd 0x00008000                    ; base addr (a linear addr) of GDT

StrBuf:        times 256 db 0

