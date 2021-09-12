#ifndef _MYOS_CONST_H
#define _MYOS_CONST_H

#define EXTERN extern   // define EXTERN except in global.c
#define PUBLIC
#define PRIVATE static

#define TRUE 1
#define FALSE 0

#define GDT_SIZE 128
#define IDT_SIZE 256

#define PRIVILEGE_KRNL 0
#define PRIVILEGE_TASK 1
#define PRIVILEGE_USER 3

#endif
