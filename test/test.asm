%include "fat12.inc"


SECTION test vstart=0x00007c00
    ; --------------------------------------------------
    ; BIOS to MBR interface
    ; --------------------------------------------------
    ; cs:ip = 0x0000:0x7c00
    ; dl = boot drive unit
    ;      (fixed disks / removable drives: 0x80=first, 0x81=second, ..., 0xfe;
    ;       floppies / superfloppies: 0x00=first, 0x01=second, ..., 0x7e;
    ;       0xff, 0x7f are reserved for ROM / remote drives and must not be used on disk)
    ; dh bit 5 = 0
BS_BPB start
start:
    mov ax, 0
    mov ds, ax
    mov ss, ax
    mov es, ax
    mov sp, 0x7c00


    mov ah, 0x08
    mov cx, 0
    mov es, cx
    mov di, cx
    int 0x13

    push word 0
    push 0xFFFF
    call print_hex_word

    push word 0
    push ax
    call print_hex_word

    push word 0
    push 0xFFFF
    call print_hex_word

    push word 0
    push dx
    call print_hex_word

    push word 0
    push 0xFFFF
    call print_hex_word

    push word 0
    push bx
    call print_hex_word

    push word 0
    push 0xFFFF
    call print_hex_word

    push word 0
    push cx
    call print_hex_word

    push word 0
    push 0xEEEE
    call print_hex_word

    push word 0
    push es
    call print_hex_word

    push word 0
    push 0xFFFF
    call print_hex_word

    push word 0
    push di
    call print_hex_word


    ; ax=0000 ah=return code=00
    ; dx=0301 heads=4 number of drive=1
    ; bx=0080 bl=80
    ; cx=7711 0111_0111_0001_0001 cylinders=0111_0111=119  sectorsPerTrack=01_0001=17

    jmp $


; usage: print_hex_word(uint16 hex_num, uint4 forre_color, uint4 page_num);
; must call in 16 bit real mode
print_hex_word:
    pusha           ; Save all registers, 16 bytes total
    mov bp, sp
    mov cx, 0x0404  ; CH = number of iteration
                    ; CL = Number of bits to rotate each iteration
    mov dx, [bp+18] ; DX = num to show
    mov bx, [bp+20] ; BX = page number and foreground color
                                       
.loop:
    rol dx, cl      ; rotate highest 4 bit to lowest
    mov ax, 0x0e0f  ; AH=0E TELETYPE OUTPUT
                    ; AL=mask to get lowest 4 bit
    and al, dl      ; AL=copy of lowest 4 bit
    add al, 0x90    ; Work as if we are packed BCD
    daa             ; Decimal adjust after add.
                    ;    If nibble in AL was between 0 and 9, then CF=0 and
                    ;    AL=0x90 to 0x99
                    ;    If nibble in AL was between A and F, then CF=1 and
                    ;    AL=0x00 to 0x05
    adc al, 0x40    ; AL=0xD0 to 0xD9
                    ; or AL=0x41 to 0x46
    daa             ; AL=0x30 to 0x39 (ASCII '0' to '9')
                    ; or AL=0x41 to 0x46 (ASCII 'A' to 'F')
   int 0x10         ; Print ASCII character in AL
   dec ch
   jnz .loop       ; Go back if more nibbles to process
   popa            ; Restore all the registers
   ret 4

    str_buf         times 16 db 0
    times 510-($-$$) db 0
                     db 0x55,0xaa
