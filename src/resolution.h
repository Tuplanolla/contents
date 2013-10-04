/**
Searches arrays for matches.

@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#ifndef RESOLUTION_H
#define RESOLUTION_H

#include <stddef.h> // size_t

#include "array.h" // struct array*, of ()
#include "extensions.h" // __*__, __attribute__ (())

int resolution_create
(void** result, struct array const* array, char const* (* accessor)(void const*), char const* argument, size_t limit)
__attribute__ ((nonnull));

void resolution_destroy
(void* resolution)
__attribute__ ((nonnull));

#endif
