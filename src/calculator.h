/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#ifndef CALCULATOR_H
#define CALCULATOR_H

#include <stddef.h> // size_t

#include "gnu.h" // __attribute__ (())

#define SIZE_MAX ((size_t )-1)

size_t minimum
(size_t x, size_t y)
__attribute__ ((const));

size_t maximum
(size_t x, size_t y)
__attribute__ ((const));

size_t edit_distance
(const char* x, const char* y)
__attribute__ ((nonnull, pure));

#endif
