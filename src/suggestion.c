/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#include "suggestion.h" // struct suggestion

#include <stdlib.h> // malloc(), free()
#include <stddef.h> // NULL

size_t suggestion_edit_distance
(struct suggestion const* const suggestion) {
	return suggestion->edit_distance;
}

void* suggestion_instance
(struct suggestion const* const suggestion) {
	return suggestion->instance;
}

int suggestion_create
(struct suggestion** const result, size_t const edit_distance, void* const instance) {
	struct suggestion* const suggestion = malloc(sizeof *suggestion);
	if (suggestion == NULL)
		return -1;
	suggestion->edit_distance = edit_distance;
	suggestion->instance = instance;
	*result = suggestion;
	return 0;
}

int suggestion_destroy
(struct suggestion* const suggestion) {
	free(suggestion);
	return 0;
}
