/**
@author Sampsa "Tuplanolla" Kiiskinen
**/

#ifndef MAIN_C
#define MAIN_C

#include "helper.h" // help
#include "parser.h" // parse

/**
@param length The length of <code>arguments</code>.
@param arguments A <code>NULL</code> terminated array of the name of the program and the command line arguments.
@return The number <code>0</code> if successful and something else otherwise.
**/
int main(const int length, const char* const* const arguments) {
	if (length > 1)
		return parse(1 + arguments);
	else
		return help();
}

#endif
