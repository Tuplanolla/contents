/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#include "state.h" // struct state, struct invocation

#include <stdlib.h> // malloc(), free()
#include <stddef.h> // NULL

#include "syntax.h" // of ()
#include "array.h" // struct array, array_create(), array_destroy()
#include "action.h" // struct action, action_name()
#include "resolver.h" // resolver_match()
#include "arity.h" // arity_to_integral()

int state_create
(struct state** const result, struct array* of (struct action*) const actions, struct array* of (struct property*) const properties) {
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
	state->automatic_completion_limit = 1; // TODO configuration
	state->suggestion_count = 3;
	state->suggestion_edit_distance = 2;
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

#include <stdio.h>
#define REPORT printf

int state_parse
(struct state* const state, struct array* of (char*) const arguments) {
	size_t position;
	for (position = 0; position < array_count(arguments); ++position) {
		char const* argument;
		if (array_read(&argument, arguments, position) == -1)
			return -1;
		REPORT("\"%s\" @ %zu\n", argument, position);
		char const* (* accessor)(void const*) = (char const* (*)(void const*) )&action_name;
		struct action const* const action = resolver_match(state->actions, accessor, argument, state->automatic_completion_limit);
		if (action == NULL) {
			// TODO schedule &infer
			return -1;
		}
		REPORT("\"%s\" = %s\n", argument, action_name(action));
		// TODO schedule action_instance(action)
		if (action_arity(action) == ARITY_VARIADIC) {
			REPORT("everything -> %s\n", action_name(action));
			// TODO consume everything
			break;
		} else {
			size_t integral;
			if (arity_to_integral(&integral, action_arity(action)) == -1)
				return -1;
			position += integral;
			REPORT("%zu arguments -> %s\n", integral, action_name(action));
		}
	}
	if (position == 0)
		(void )0; // TODO schedule &help
	return 0;
}

int state_schedule
(struct state* const state, struct invocation* const invocation) {
	(void )state;
	(void )invocation;
	return -1;
}

int state_execute
(struct state* const state) {
	(void )state;
	return -1;
}
