/**
Prints information for the user.

@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#ifndef HELPER_H
#define HELPER_H

#include <stdio.h> // FILE

/**
Prints a short usage reference to the given stream.

@param stream The stream.
@return The number <code>0</code> if successful and <code>-1</code> otherwise.
**/
int print_help(FILE* stream);

/**
Prints a short project summary to the given stream.

@param stream The stream.
@return The number <code>0</code> if successful and <code>-1</code> otherwise.
**/
int print_summary(FILE* stream);

#endif
