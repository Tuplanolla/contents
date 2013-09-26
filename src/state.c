/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#include "state.h" // procedure, struct state

#include <stdlib.h> // free(), malloc()
#include <stddef.h> // NULL

#include "action.h" // procedure, struct action, action_arity(), action_instance(), action_name()
#include "arity.h" // arity_to_integral()
#include "array.h" // struct array, array_count(), array_create(), array_destroy(), array_read()
#include "invocation.h" // struct invocation
#include "resolution.h" // resolution_create()
#include "syntax.h" // of ()

int state_create
(struct state** const result, struct array* of (struct action*) const actions, struct array* of (struct property*) const properties) {
	struct array* of (struct invocation) invocations;
	if (array_create(&invocations, 1, sizeof (struct invocation)) == -1) {
		return -1;
	}
	struct state* const state = malloc(sizeof *state);
	if (state == NULL) {
		array_destroy(invocations);
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

void state_destroy
(struct state* const state) {
	array_destroy(state->invocations);
	free(state);
}

static int converter_convert // TODO move and implement
(void** const result, procedure const instance, char const* const argument, size_t const position) {
	if (result != NULL)
		*result = argument;
	return 0;
}

int state_parse
(struct state* const state, struct array* of (char*) const arguments) {
	size_t position;
	for (position = 0; position < array_count(arguments); ++position) {
		char const* argument;
		if (array_read(&argument, arguments, position) == -1)
			return -1;
		char const* (* const accessor)(void const*) = (char const* (*)(void const*) )&action_name;
		struct action* action;
		if (resolution_create((void** )&action, state->actions, accessor, argument, state->automatic_completion_limit) == -1)
			return -1;
		if (action == NULL) {
			(void )0; // TODO schedule &infer
			return -1;
		}
		if (action_arity(action) == ARITY_VARIADIC) {
			(void )0; // TODO consume everything
			break;
		} else {
			size_t integral;
			if (arity_to_integral(&integral, action_arity(action)) == -1)
				return -1;
			procedure instance = action_instance(action);
			struct array* of (void*) actual_arguments;
			if (array_create(&actual_arguments, integral + 1, sizeof (void*)) == -1)
				return -1;
			for (size_t actual_position = 0;
					actual_position < integral;
					++actual_position) {
				char const* some_argument;
				if (array_read(&some_argument, arguments, position + actual_position) == -1)
					return -1;
				void* actual_argument;
				converter_convert(&actual_argument, instance, some_argument, actual_position);
				array_add_last(actual_arguments, actual_argument);
			}
			struct invocation invocation = {
				.instance = instance,
				.arguments = actual_arguments
			};
			array_add_last(state->invocations, &invocation);
			position += integral;
		}
		resolution_destroy(action);
	}
	if (position == 0)
		(void )0; // TODO schedule &help
	return 0;
}

int state_execute
(struct state* const state) {
	int status = 0;
	struct array* of (struct invocation) invocations = state->invocations;
	for (size_t position = 0; position < array_count(invocations); ++position) {
		struct invocation invocation;
		if (array_read(&invocation, invocations, position) == -1) {
			status = -1;
			continue;
		}
		array_destroy(invocation.arguments);
	}
	return status;
}
