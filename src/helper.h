/**
Prints information for the user.

@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#ifndef HELPER_H
#define HELPER_H

#include <stdio.h> // FILE

#include "data.h" // struct proposal

/**
Prints a short usage reference to the given stream.

Fails if the stream is <code>NULL</code> or
 <code>fprintf</code> fails.

@param stream The stream.
@return The number <code>0</code> if successful or
 <code>-1</code> otherwise.
**/
int print_help(FILE* stream);

/**
Prints a short project summary to the given stream.

Fails if the stream is <code>NULL</code> or
 <code>fprintf</code> fails.

@param stream The stream.
@return The number <code>0</code> if successful or
 <code>-1</code> otherwise.
**/
int print_summary(FILE* stream);

/**
Prints a list of argument correction guesses to the given stream.

Fails if the stream is <code>NULL</code>,
 the proposal contains no suggestions or
 <code>fprintf</code> fails.

@param stream The stream.
@param proposal The guesses.
@return The number <code>0</code> if successful or
 <code>-1</code> otherwise.
**/
int print_suggestions(FILE* stream, const struct proposal* proposal);

#endif
