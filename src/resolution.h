/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#ifndef RESOLUTION_H
#define RESOLUTION_H

#include <stddef.h> // size_t

#include "array.h" // struct array
#include "gnu.h" // __attribute__ (())
#include "syntax.h" // of ()

int resolution_create
(void** result, struct array const* array, char const* (* accessor)(void const*), char const* argument, size_t limit)
__attribute__ ((nonnull));

int resolution_destroy
(void* resolution)
__attribute__ ((nonnull));

#endif
