/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#ifndef RESOLVER_H
#define RESOLVER_H

#include <stddef.h> // size_t

#include "gnu.h" // __attribute__ (())
#include "array.h" // struct array

void* resolve
(struct array* array, char const* (* accessor)(void const*), char const* argument, size_t limit)
__attribute__ ((nonnull));

#endif
