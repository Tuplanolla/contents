/**
Incomplete!
Separates and delegates the execution of the user's wishes.

@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#ifndef PARSER_H
#define PARSER_H

/**
Parses the given arguments and delegates their execution.

@param arguments The arguments.
@return The number <code>0</code> if successful or <code>-1</code> otherwise.
**/
int parse(const char* const* arguments);

#endif
