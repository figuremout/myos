[bits 32]

extern cstart

[section .data]
clock_init_msg: db "^",0x00

[section .bss]                                  ; bss (Block Started by Symbol)
StackSpace:     resb 2*1024
StackTop:

[section .text]
global _start

_start:
    mov esp, StackTop
    call cstart
    jmp $
