/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#include "state.h" // struct state, struct invocation

#include <stdlib.h> // malloc(), free()
#include <stddef.h> // NULL

#include "syntax.h" // of ()
#include "array.h" // struct array
#include "action.h" // struct action, action_name()
#include "resolver.h" // resolve()
#include "arity.h" // arity_to_integral()

int state_create
(struct state** const result, struct array* const of (struct action*) actions, struct array* const of (struct property*) properties) {
	struct array* of (struct invocation*) invocations;
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

int state_parse
(struct state* const state, struct array* const of (char*) arguments) {
	size_t position;
	for (position = 0; position < arguments->count; ++position) {
		const char* argument;
		if (array_read(&argument, arguments, position) == -1)
			return -1;
		const struct action* const action = resolve(state->actions, (const char* (*)(const void*) )&action_name, argument, state->automatic_completion_length);
		if (action == NULL) {
			// TODO schedule &infer
			return -1;
		}
		// TODO schedule action->instance
		if (action->arity == ARITY_VARIADIC) {
			// TODO consume everything
			break;
		} else {
			size_t integral;
			if (arity_to_integral(&integral, action->arity) == -1)
				return -1;
			position += integral;
			continue;
		}
	}
	if (position == 0)
		(void )0; // TODO schedule &help
	return 0;
}

int state_execute
(struct state* const state) {
	(void )state;
	return -1;
}
