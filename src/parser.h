/**
Incomplete!
Understands the user's wishes.

@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#ifndef PARSER_H
#define PARSER_H

#include "state.h" // struct state

/**
Parses the arguments inside the given state.

Fails if .

@param state The mutable state.
@return The number <code>0</code> if successful or
 <code>-1</code> otherwise.
**/
int parse(struct state* state)
		__attribute__ ((nonnull));

#endif
