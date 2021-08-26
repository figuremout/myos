; --------------------------------------------
; function: void putString(uint32 linear_addr);
; 32-bit protect mode code: display string end with 0x00 and update cursor
; Notice: because this function will sti in the end, so don't use it before interrupt system inited
; --------------------------------------------
putString:
    push bp
    mov bp, sp
    sub sp, 0x40

    mov eax, [bp+4]                             ; get param linear_addr
    mov [bp-4], eax
    cli                                         ; close maskable hardware interrupt during hardware operation
.getc:
    mov eax, [bp-4] 
    mov cl, [eax]                               ; read in a byte
    or cl, cl                                   ; check if it's 0
    jz .exit                                    ; end

    xor ch, ch
    push cx
    call putChar
    add sp, 2
    inc dword [bp-4]

    jmp .getc

.exit:
    sti
    mov sp, bp
    pop bp
    ret

; --------------------------------------------
; function: void putChar(uint16 ascii);
; display a char at cursor and update cursor (VGA)
; param char only low 8 bit valid
; --------------------------------------------
putChar:
    push bp
    mov bp, sp

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
    and ebx, 0x0000ffff

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
    mov [0x800b8000+ebx], cl                    ; fill in ascii
    mov byte [0x800b8000+ebx+1], 0x07                       ; fill in attr

    shr bx, 1
    inc bx                                      ; cursor position++

.roll_screen:
    cmp bx, 2000                                ; if cursor out of screen
    jl .set_cursor

    ; copy chars from row 1-24 to row 0-23
    cld
    mov edi,0x800b8000                          ; es:di point to 1st row start
    mov esi,0x800b80a0                          ; ds:si point to 2nd row start

    mov ecx, 1920                               ; copy a word(ascii, attr) once, 24 rows need to copy 24*80=1920 times
    rep movsw

    ; clear last row
    mov bx, 3840                                ; 1920*2=3840 last row(row 24) start offset
    mov ecx, 80
.cls:
    mov word [0x800b8000+ebx], 0x0720           ; 0x0720 bg=black fg=white char=space
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

    mov sp, bp
    pop bp
    ret


