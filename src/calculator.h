/**
Calculates complicated things for the user.

@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#ifndef CALCULATOR_H
#define CALCULATOR_H

#include <stddef.h> // size_t

#include "gnu.h" // __attribute__

/**
Returns the smallest given size.

@param x The first size.
@param y The second size.
@return The smallest size.
**/
size_t minimum(size_t x, size_t y)
		__attribute__ ((const));

/**
Returns the biggest given size.

@param x The first size.
@param y The second size.
@return The biggest size.
**/
size_t maximum(size_t x, size_t y)
		__attribute__ ((const));

/**
Calculates the edit distance between two strings.

@param x The first string.
@param y The second string.
@return The edit distance.
**/
size_t distance(const char* x, const char* y)
		__attribute__ ((pure));

#endif
