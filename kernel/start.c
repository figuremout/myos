#include "type.h"
#include "const.h"
#include "protect.h"

PUBLIC void putString(char* str);

PUBLIC t_8 gdt_ptr[6]; // 8 byte, 47-16 base, 15-0 limit
PUBLIC DESCRIPTOR gdt[GDT_SIZE];

PUBLIC void cstart(void) {
    int i = 1;
    putString("\r\nKernel is running!\r\n"); // maybe \0 not need
}
