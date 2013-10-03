/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#include "state.h" // struct state

#include <stddef.h> // NULL, size_t
#include <stdlib.h> // free(), malloc()

#include "array.h" // struct array*, of ()

int state_create
(struct state** const result) {
	struct state* const state = malloc(sizeof *state);
	if (state == NULL)
		return -1;
	*result = state;
	return 0;
}

void state_destroy
(struct state* const state) {
	free(state);
}

int state_parse
(struct state* const state, struct array_const* of (char const*) const arguments) {
	return -1;
}

int state_execute
(struct state* const state) {
	return -1;
}
