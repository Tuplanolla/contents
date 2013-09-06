/**
Understands and carries out the user's wishes.

@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#ifndef PARSER_H
#define PARSER_H

/**
Parses and manages the delegation of the given arguments.

@param arguments The arguments.
@return The number <code>0</code> if successful and <code>-1</code> otherwise.
**/
int parse(const char* const* arguments);

#endif
