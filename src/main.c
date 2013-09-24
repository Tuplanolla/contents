/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#include "syntax.h" // of ()
#include "array.h" // struct array, array_create(), array_destroy(), array_add_last()
#include "indefinix.h" // indefinix_run()

int main
(int const count, char const** const arguments) {
	size_t const argument_count = (size_t )count;
	int status = 0;
	struct array* of (char*) array;
	if (array_create(&array, argument_count, sizeof (char*)) == -1) {
		status = -1;
		goto array;
	}
	for (size_t argument = 1; argument < argument_count; ++argument) {
		if (array_add_last(array, &arguments[argument]) == -1) {
			status = -1;
			goto all;
		}
	}
	if (indefinix_run(array) == -1) {
		status = -1;
		goto all;
	}
all:
	if (array_destroy(array) == -1)
		status = -1;
array:
	return -status;
}
