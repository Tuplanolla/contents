/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#ifndef CALCULATOR_H
#define CALCULATOR_H

#include <stddef.h> // size_t

#include "gnu.h" // __attribute__ (())

size_t minimum
(size_t x, size_t y)
__attribute__ ((const));

size_t maximum
(size_t x, size_t y)
__attribute__ ((const));

int edit_distance
(size_t* result, char const* x, char const* y)
__attribute__ ((nonnull));

#endif
