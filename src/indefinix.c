/**
Contains the main entry point.

@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#include <stdlib.h> // EXIT_FAILURE, EXIT_SUCCESS

#include "array.h" // struct array*, array_*(), of ()
#include "interpreter.h" // interpret()

/**
Manages the state.

@param count The amount of command line arguments plus one.
@param arguments The location of the executable and its command line arguments.
@return The value <code>EXIT_SUCCESS</code> if successful or
 <code>EXIT_FAILURE</code> otherwise.
**/
int main
(int const count, char** const arguments) {
	if (count < 1)
		return EXIT_FAILURE;
	size_t const argument_count = (size_t )count - 1;
	struct array_const* of (char const*) array;
	if (array_const_create(&array, argument_count, sizeof (char*)) == -1)
		return EXIT_FAILURE;
	for (size_t argument = 0;
			argument < argument_count;
			++argument) {
		if (array_const_add_last(array, &arguments[argument + 1]) == -1) {
			array_const_destroy(array);
			return EXIT_FAILURE;
		}
	}
	if (interpret(array) == -1) {
		array_const_destroy(array);
		return EXIT_FAILURE;
	}
	array_const_destroy(array);
	return EXIT_SUCCESS;
}
