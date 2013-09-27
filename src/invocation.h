/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#ifndef INVOCATION_H
#define INVOCATION_H

#include "array.h" // struct array
#include "gnu.h" // __attribute__ (())
#include "syntax.h" // of ()
#include "types.h" // procedure

struct invocation {
	procedure instance;
	struct array* of (char const*) arguments;
};

procedure invocation_instance
(struct invocation const* invocation)
__attribute__ ((nonnull));

struct array* of (void*) invocation_arguments
(struct invocation const* invocation)
__attribute__ ((nonnull));

#endif
