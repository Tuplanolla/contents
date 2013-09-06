/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#include "parser.h" // parse

/**
@param length The length of <code>arguments</code>.
@param arguments A <code>NULL</code> terminated array of the program name and the command line arguments.
@return The number <code>0</code> if successful and something else otherwise.
**/
int main(const int length, const char* const* const arguments) {
	return -parse(arguments + 1);
}
