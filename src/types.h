/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#ifndef TYPES_H
#define TYPES_H

#include "array.h" // struct array
#include "syntax.h" // of ()

struct state;

typedef int (* procedure)(struct state*, struct array* of (char const*));

typedef void* variable;

#endif
