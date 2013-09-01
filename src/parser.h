/**
Carries out the user's wishes.

@author Sampsa "Tuplanolla" Kiiskinen
@file
**/

#ifndef PARSER_H
#define PARSER_H

/**
Parses and manages the delegation of the given arguments.

@param arguments The arguments.
@return The error code.
**/
int parse(const char* const* arguments);

#endif
