/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#include "syntax.h" // of ()
#include "array.h" // struct array, array_create(), array_destroy(), array_add_last()
#include "data.h" // struct action, actions_create(), struct property, properties_create(), properties_destroy(), actions_destroy()
#include "state.h" // struct state, state_create(), state_parse(), state_execute(), state_destroy()

static int run
(struct array* of (const char*) arguments) {
	int status = 0;
	struct array* of (const struct action*) actions;
	if (actions_create(&actions) == -1) {
		status = -1;
		goto actions;
	}
	struct array* of (const struct property*) properties;
	if (properties_create(&properties) == -1) {
		status = -1;
		goto properties;
	}
	struct state* state;
	if (state_create(&state, actions, properties) == -1) {
		status = -1;
		goto state;
	}
	if (state_parse(state, arguments) == -1) {
		status = -1;
		goto all;
	}
	if (state_execute(state) == -1) {
		status = -1;
		goto all;
	}
all:
	if (state_destroy(state) == -1)
		status = -1;
state:
	if (properties_destroy(properties) == -1)
		status = -1;
properties:
	if (actions_destroy(actions) == -1)
		status = -1;
actions:
	return status;
}

int main
(const int count, char* const* const arguments) {
	const size_t argument_count = (size_t )count;
	int status = 0;
	struct array* of (const char*) array;
	if (array_create(&array, argument_count, sizeof (char*)) == -1) {
		status = -1;
		goto array;
	}
	for (size_t argument = 1; argument <= argument_count; ++argument) {
		if (array_add_last(array, arguments[argument]) == -1) {
			status = -1;
			goto all;
		}
	}
	if (run(array) == -1) {
		status = -1;
		goto all;
	}
all:
	if (array_destroy(array) == -1)
		status = -1;
array:
	return -status;
}
