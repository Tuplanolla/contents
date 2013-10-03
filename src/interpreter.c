/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#include "interpreter.h"

#include "array.h" // struct array*, of ()
#include "state.h" // struct state, state_*()

int interpret
(struct array_const* of (char const*) const arguments) {
	struct state* state;
	if (state_create(&state) == -1)
		return -1;
	if (state_parse(state, arguments) == -1) {
		state_destroy(state);
		return -1;
	}
	if (state_execute(state) == -1) {
		state_destroy(state);
		return -1;
	}
	state_destroy(state);
	return 0;
}
