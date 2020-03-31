//
// Created by WTF on 2020/3/26.
//
#include "gemcc.h"

Initializer *newLabelInit(char *label, long addend){
    Initializer *init = calloc(1, sizeof(Initializer));
    init->label = label;
    init->addend = addend;
    return init;
}

Initializer *newValInit(long val){
    Initializer *init = calloc(1, sizeof(Initializer));
    init->val = val;
    return init;
}

Initializer *newNestInit(Initializer *nested){
    Initializer *init = calloc(1, sizeof(Initializer));
    init->nestedInit = nested;
    return init;
}
