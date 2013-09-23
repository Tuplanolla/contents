/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#ifndef RESOLVER_H
#define RESOLVER_H

#include <stddef.h> // size_t

#include "gnu.h" // __attribute__ (())
#include "array.h" // struct array

const void* resolve
(struct array* array, const char* (* accessor)(const void*), const char* argument, size_t limit)
__attribute__ ((nonnull));

#endif
