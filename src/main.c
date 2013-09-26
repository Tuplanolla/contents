/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#include "array.h" // struct array, array_add_last(), array_create(), array_destroy()
#include "indefinix.h" // indefinix_run()
#include "syntax.h" // of ()

int main
(int const count, char const* const* const arguments) {
	size_t const argument_count = (size_t )count;
	int status = 0;
	struct array* of (char*) array;
	if (array_create(&array, argument_count, sizeof (char*)) == -1) {
		status = -1;
		goto nothing;
	}
	for (size_t argument = 1; argument < argument_count; ++argument) {
		if (array_add_last(array, &arguments[argument]) == -1) {
			status = -1;
			goto array;
		}
	}
	if (indefinix_run(array) == -1) {
		status = -1;
		goto array;
	}
array:
	array_destroy(array);
nothing:
	return -status;
}
