/**
Interprets command line arguments.

@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#ifndef INTERPRETER_H
#define INTERPRETER_H

#include "array.h" // struct array*, of ()
#include "extensions.h" // __*__, __attribute__ (())

int interpret
(struct array_const* of (char const*) arguments)
__attribute__ ((__nonnull__));

#endif
