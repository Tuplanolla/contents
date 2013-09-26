/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#ifndef INDEFINIX_H
#define INDEFINIX_H

#include "array.h" // struct array
#include "gnu.h" // __attribute__ (())
#include "syntax.h" // of ()

int indefinix_run
(struct array* of (char*) arguments)
__attribute__ ((nonnull));

#endif
