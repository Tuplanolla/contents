/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#include "resolver.h" // struct suggestion

#include <stddef.h> // size_t, NULL
#include <string.h> // strlen()

#include "syntax.h" // of ()
#include "array.h" // struct array

void* resolver_match
(struct array const* const array, char const* (* const accessor)(void const*), char const* const argument, size_t const limit) {
	void* match = NULL;
	struct array* candidates;
	if (array_create_copy(&candidates, array) == -1)
		goto nothing;
	size_t const argument_length = strlen(argument);
	for (size_t character = 0;
			character < argument_length;
			++character) {
		for (size_t position = 0;
				position < array_count(candidates);
				) {
			const void* candidate;
			if (array_read(&candidate, candidates, position) == -1) {
				match = NULL;
				goto array;
			}
			if (accessor(candidate)[character] == argument[character])
				++position;
			else
				if (array_remove(NULL, candidates, position) == 1) {
					match = NULL;
					goto array;
				}
		}
		if (array_count(candidates) == 0) {
			match = NULL;
			break;
		} else if (array_count(candidates) == 1)
			if ((limit == 0 && character == argument_length - 1)
					|| (limit != 0 && character >= limit - 1))
				if (array_read(&match, candidates, 0) == -1) {
					match = NULL;
					goto array;
				}
	}
array:
	if (array_destroy(candidates) == -1)
		match = NULL;
nothing:
	return match;
}
