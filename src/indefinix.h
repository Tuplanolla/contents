/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#ifndef INDEFINIX_H
#define INDEFINIX_H

#include "gnu.h" // __attribute__ (())
#include "syntax.h" // of ()
#include "array.h" // struct array

int indefinix_run
(struct array* of (char*) arguments)
__attribute__ ((nonnull));

#endif
