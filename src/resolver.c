/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#include "resolver.h"

#include <stddef.h> // size_t, NULL
#include <stdlib.h> // malloc(), free()
#include <string.h> // strlen(), memmove()

#include "array.h" // struct array

void* resolve
(struct array* const array, char const* (* accessor)(void const*), char const* const argument, size_t const limit) {
	struct array* candidates;
	if (array_create_copy(&candidates, array) == -1)
		return NULL;
	void* resolution = NULL;
	size_t const argument_length = strlen(argument);
	for (size_t character = 0;
			character < argument_length;
			++character) {
		for (size_t position = 0;
				position < array_count(candidates);
				) {
			void* candidate;
			array_read(&candidate, candidates, position);
			if (accessor(candidate)[character] == argument[character])
				++position;
			else
				array_remove(NULL, candidates, position);
		}
		if (array_count(candidates) == 0) {
			resolution = NULL;
			break;
		} else if (array_count(candidates) == 1
				&& ((limit == 0 && character == argument_length - 1)
				|| (limit != 0 && character >= limit - 1)))
			array_read(&resolution, candidates, 0);
	}
	array_destroy(candidates);
	return resolution;
}
