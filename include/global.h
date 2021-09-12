// EXTERN is defined as extern except in global.c
#ifdef GLOBAL_VAR_HERE
#undef EXTERN
#define EXTERN
#endif

EXTERN t_8        gdt_ptr[6];
EXTERN DESCRIPTOR gdt[GDT_SIZE];
EXTERN t_8        idt_ptr[6];
EXTERN GATE       idt[IDT_SIZE];
