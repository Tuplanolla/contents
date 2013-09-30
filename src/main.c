/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#include <stddef.h> // size_t
#include <stdlib.h> // EXIT_FAILURE, EXIT_SUCCESS

#include "indefinix.h" // indefinix_invoke()

int main
(int const count, char const* const* const arguments) {
	if (indefinix_invoke(arguments + 1, (size_t )count - 1) == -1)
		return EXIT_FAILURE;
	return EXIT_SUCCESS;
}
