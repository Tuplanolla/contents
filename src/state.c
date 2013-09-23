/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#include "state.h"

#include <stddef.h> // NULL
#include <stdlib.h> // malloc(), free()

#include "syntax.h" // of ()
#include "array.h" // struct array
#include "data.h" // struct action, struct property
#include "resolver.h" // resolve()

int state_create
(struct state** const result, struct array* const of (const struct action*) actions, struct array* const of (const struct property*) properties) {
	struct array* of (const struct invocation*) invocations;
	if (array_create(&invocations, 1, sizeof (struct invocation*)) == -1) {
		return -1;
	}
	struct state* const state = malloc(sizeof *state);
	if (state == NULL) {
		if (array_destroy(invocations) == -1)
			return -1;
		return -1;
	}
	state->actions = actions;
	state->properties = properties;
	state->invocations = invocations;
	state->automatic_completion_length = 0; // TODO configuration
	state->suggestion_count = 3;
	state->maximum_suggestion_distance = 2;
	*result = state;
	return 0;
}

int state_destroy
(struct state* const state) {
	int status = 0;
	if (array_destroy(state->invocations) == -1)
		status = -1;
	free(state);
	return status;
}

static const char* action_accessor
(const void* const action) {
	return ((const struct action* )action)->name;
}

int state_parse
(struct state* const state, struct array* const of (const char*) arguments) {
	for (size_t position = 0; position < arguments->count; ++position) {
		const char* argument;
		if (array_read(&argument, arguments, position) == -1)
			return -1;
		const struct action* const action = resolve(state->actions, &action_accessor, argument, state->automatic_completion_length);
		if (action == NULL)
			return -1;
	}
	return -1;
}

int state_execute
(struct state* const state) {
	return -1;
}
