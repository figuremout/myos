%macro PM_UTILS 0
; --------------------------------------------
; function: uint64 makeSegDesc(uint32 base, uint32 limit, uint32 attr)
; return edx:eax=desc
; --------------------------------------------
makeSegDesc:
    push bp
    mov bp, sp

    mov eax, [bp+4]                             ; get param base
    mov ebx, [bp+8]                             ; get param limit
    mov ecx, [bp+12]                            ; get param attr

    mov edx, eax
    shl eax, 16
    or ax, bx                                   ; desc low 32 bit done

    and edx, 0xffff0000                         ; clear unrelated bits
    rol edx, 8
    bswap edx                                   ; prepare base high 16 bits

    xor bx, bx
    or edx, ebx                                 ; fill in high 4 bits of limit

    or edx, ecx                                 ; fill in attr
    
    mov sp, bp
    pop bp
    ret

%endmacro
