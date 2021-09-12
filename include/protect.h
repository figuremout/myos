#ifndef _MYOS_PROTECT_H

#define _MYOS_PROTECT_H
typedef struct {
    t_16 limit_low;
    t_16 base_low;
    t_8  base_mid;
    t_8  attr1;             // P(1), DPL(2), S(1), TYPE(4)
    t_8  limit_high_attr2;  // G(1), D/B(1), L(1), AVL(1), limit_high(4)
    t_8  base_high;
} DESCRIPTOR;

typedef struct {
    t_16 offset_low;
    t_16 selector;
    t_8  dcount;
    t_8  attr;
    t_16 offset_high;
} GATE;

#endif
