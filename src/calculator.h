/**
Calculates complicated things for the user.

@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#ifndef CALCULATOR_H
#define CALCULATOR_H

/**
Returns the edit distance between two strings.

@param x The first string.
@param y The second string.
@return The edit distance.
**/
size_t distance(const char* x, const char* y);

#endif
