/**
Carries out common calculations.

@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#ifndef CALCULATOR_H
#define CALCULATOR_H

#include <stddef.h> // size_t

#include "gnu.h" // __*__, __attribute__ (())

/**
Returns the smaller of the given sizes.

@param x The first size.
@param y The second size.
@return The smaller size.
**/
size_t minimum
(size_t x, size_t y)
__attribute__ ((__const__));

/**
Returns the bigger of the given sizes.

@param x The first size.
@param y The second size.
@return The bigger size.
**/
size_t maximum
(size_t x, size_t y)
__attribute__ ((__const__));

/**
Calculates the edit distance between two strings.

@param result A pointer to the destination of the edit distance.
@param x The first string.
@param y The second string.
@return The number <code>0</code> if successful or <code>-1</code> otherwise.
**/
int edit_distance
(size_t* result, char const* x, char const* y)
__attribute__ ((__nonnull__));

#endif
