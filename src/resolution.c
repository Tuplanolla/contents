/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#include "resolution.h"

#include <stddef.h> // NULL, size_t
#include <stdlib.h> // free(), malloc()
#include <string.h> // strlen()

#include "array.h" // struct array, array_count(), array_create_copy(), array_destroy(), array_read(), array_remove()

int resolution_create
(void** const result, struct array const* const array, char const* (* const accessor)(void const*), char const* const argument, size_t const limit) {
	int status = 0;
	void* candidate = malloc(array_unit(array));
	if (candidate == NULL)
		goto nothing;
	void* match = malloc(array_unit(array));
	if (match == NULL)
		goto candidate;
	struct array* candidates;
	if (array_create_copy(&candidates, array) == -1)
		goto match;
	size_t const argument_length = strlen(argument);
	for (size_t character = argument_length;
			character > 0;
			) {
		--character;
		for (size_t position = 0;
				position < array_count(candidates);
				) {
			if (array_read(candidate, candidates, position) == -1) {
				status = -1;
				goto array;
			}
			if (accessor(candidate)[character] == argument[character])
				++position;
			else
				if (array_remove(NULL, candidates, position) == 1) {
					status = -1;
					goto array;
				}
		}
		if (array_count(candidates) == 0) {
			status = -1;
			goto array;
		} else if (array_count(candidates) == 1)
			if ((limit == 0 && character == argument_length - 1)
					|| (limit != 0 && character >= limit - 1))
				if (array_read(match, candidates, 0) == -1) {
					status = -1;
					goto array;
				}
	}
	if (array_destroy(candidates) == -1) {
		status = -1;
		goto match;
	}
	free(candidate);
	*result = match;
	goto nothing;
array:
	if (array_destroy(candidates) == -1)
		status = -1;
match:
	free(match);
candidate:
	free(candidate);
nothing:
	return status;
}

int resolution_destroy
(void* const resolution) {
	free(resolution);
	return 0;
}
