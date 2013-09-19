/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#include "gnu.h" // __attribute__
#include "state.h" // initialize()
#include "parser.h" // parse()

/**
@param count The amount of elements in <code>arguments</code>.
@param arguments A <code>NULL</code> terminated array of the executable's name and its command line arguments.
@return The number <code>0</code> if successful or a positive number less than <code>128</code> otherwise.
**/
int main(const int count __attribute__ ((unused)), const char* const* const arguments) {
	initialize();
	return -parse(arguments + 1);
}
