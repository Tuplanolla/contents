/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#include "suggestions.h"

#include <stddef.h> // size_t, NULL
#include <string.h> // strlen()

#include "array.h" // struct array, array_create(), array_destroy()
#include "calculator.h" // maximum(), edit_distance()
#include "truncation.h" // truncation_create(), truncation_destroy()
#include "suggestion.h" // suggestion_edit_distance()

#define SIZE_MAX ((size_t )-1)

static int distance_comparator(void const* const x, void const* const y) {
	size_t const x_distance = ((struct suggestion const* )x)->edit_distance;
	size_t const y_distance = ((struct suggestion const* )y)->edit_distance;
	if (x_distance < y_distance)
		return -1;
	if (x_distance > y_distance)
		return 1;
	return 0;
}

int suggestions_create
(struct array** of (struct suggestion*) const result, struct array* const array, char const* (* const accessor)(void const*), char const* const argument, size_t const completion_limit, size_t const count_limit, size_t const distance_limit) {
	int status = 0;
	size_t const count = array->count;
	struct array* of (struct suggestion*) suggestions;
	if (array_create(&suggestions, count, sizeof (struct suggestion*)) == -1) {
		status = -1;
		goto nothing;
	}
	char* truncation;
	size_t const truncation_length = completion_limit == 0 ?
			SIZE_MAX : // truncation_length
			maximum(completion_limit, strlen(argument));
	for (size_t position = 0;
			position < count;
			++position) {
		void* instance;
		if (array_read(&instance, array, position) == -1) {
			status = -1;
			goto array;
		}
		size_t distance;
		if (completion_limit == 0) {
			if (truncation_create(&truncation, accessor(instance), truncation_length) == -1) {
				status = -1;
				goto array;
			}
			if (edit_distance(&distance, argument, truncation) == -1) {
				status = -1;
				goto truncation;
			}
			if (truncation_destroy(truncation) == -1) {
				status = -1;
				goto array;
			}
		} else
			if (edit_distance(&distance, argument, accessor(instance)) == -1) {
				status = -1;
				goto array;
			}
		struct suggestion* suggestion; // TODO do away with pointers
		if (suggestion_create(&suggestion, distance, instance) == -1) {
			status = -1;
			goto array;
		}
		if (array_add_last(suggestions, suggestion) == -1) {
			status = -1;
			goto array;
		}
	}
	if (array_sort(suggestions, &distance_comparator) == -1) {
		status = -1;
		goto array;
	}
	for (size_t position = 0;
			position < array_count(suggestions);
			++position) {
		struct suggestion const* suggestion;
		if (array_read(&suggestion, suggestions, position) == -1) {
			status = -1;
			goto array;
		}
		if (position >= count_limit
				|| suggestion_edit_distance(suggestion) > distance_limit) {
			if (array_truncate(suggestions, position) == -1) {
				status = -1;
				goto array;
			}
			break;
		}
	}
	*result = suggestions;
	goto nothing;
truncation:
	if (truncation_destroy(truncation) == -1)
		status = -1;
array:
	if (array_destroy(suggestions) == -1)
		status = -1;
nothing:
	return status;
}

int suggestions_destroy
(struct array* of (struct suggestion*) const array) {
	for (size_t position = 0; position < array->count; ++position) {
		struct suggestion* suggestion; // TODO do away with pointers
		if (array_read(&suggestion, array, position) == -1)
			return -1;
		if (suggestion_destroy(suggestion) == -1)
			return -1;
	}
	return array_destroy(array);
}
