/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#include "state.h" // enum *, struct *

#include <stddef.h> // NULL, size_t
#include <stdio.h> // stdout
#include <stdlib.h> // free(), malloc()
#include <string.h> // strlen()

#include "array.h" // struct array*, of ()
#include "call.h" // call_execute()

enum status state_execution_status
(struct state const* const state) {
	return state->execution.status;
}

int state_create
(struct state** const result) {
	struct state* const state = malloc(sizeof *state);
	if (state == NULL)
		return -1;
	state->execution.status = STATUS_NORMAL;
	state->execution.calls = NULL;
	state->configuration.invisible.place = NULL;
	state->configuration.invisible.suggestion.amount = 3;
	state->configuration.invisible.suggestion.distance = 2;
	state->configuration.invisible.behavior = BEHAVIOR_ABORT;
	state->configuration.invisible.verbosity = VERBOSITY_NONE;
	state->configuration.invisible.output = stdout;
	state->configuration.visible.location = NULL;
	state->configuration.visible.editor = NULL;
	state->configuration.visible.completion = 1;
	state->configuration.visible.order.sorting = SORTING_NORMAL;
	state->configuration.visible.order.grouping = GROUPING_DIRECTORIES;
	state->configuration.visible.order.hiding = HIDING_HIDDEN;
	state->configuration.visible.wrapping = CONTINUATION_WRAP;
	state->configuration.visible.justification.first = ALIGNMENT_LEFT;
	state->configuration.visible.justification.second = ALIGNMENT_LEFT;
	state->configuration.visible.filling.first = PADDING_FILL;
	state->configuration.visible.filling.second = PADDING_FILL;
	state->configuration.visible.interaction = ANSWER_NONE;
	state->configuration.visible.affix.infix = NULL;
	state->configuration.visible.affix.prefix = NULL;
	state->configuration.visible.affix.suffix = NULL;
	state->configuration.visible.headaffix.infix = NULL;
	state->configuration.visible.headaffix.prefix = NULL;
	state->configuration.visible.headaffix.suffix = NULL;
	state->configuration.visible.tailaffix.infix = NULL;
	state->configuration.visible.tailaffix.prefix = NULL;
	state->configuration.visible.tailaffix.suffix = NULL;
	state->configuration.visible.unusual.indexed = NULL;
	state->configuration.visible.unusual.present = NULL;
#define CREATE(result) array_create(result, 64, sizeof (char))
	if (array_const_create(&state->execution.calls, 8, sizeof (struct call)) == -1
			|| CREATE(&state->configuration.invisible.place) == -1
			|| CREATE(&state->configuration.visible.location) == -1
			|| CREATE(&state->configuration.visible.editor) == -1
			|| CREATE(&state->configuration.visible.affix.infix) == -1
			|| CREATE(&state->configuration.visible.affix.prefix) == -1
			|| CREATE(&state->configuration.visible.affix.suffix) == -1
			|| CREATE(&state->configuration.visible.headaffix.infix) == -1
			|| CREATE(&state->configuration.visible.headaffix.prefix) == -1
			|| CREATE(&state->configuration.visible.headaffix.suffix) == -1
			|| CREATE(&state->configuration.visible.tailaffix.infix) == -1
			|| CREATE(&state->configuration.visible.tailaffix.prefix) == -1
			|| CREATE(&state->configuration.visible.tailaffix.suffix) == -1
			|| CREATE(&state->configuration.visible.unusual.indexed) == -1
			|| CREATE(&state->configuration.visible.unusual.present) == -1) {
		state_destroy(state);
		return -1;
	}
#undef CREATE
	*result = state;
	return 0;
}

void state_destroy
(struct state* const state) {
	if (state == NULL)
		return;
	array_const_destroy(state->execution.calls);
	array_destroy(state->configuration.invisible.place);
	array_destroy(state->configuration.visible.location);
	array_destroy(state->configuration.visible.editor);
	array_destroy(state->configuration.visible.affix.infix);
	array_destroy(state->configuration.visible.affix.prefix);
	array_destroy(state->configuration.visible.affix.suffix);
	array_destroy(state->configuration.visible.headaffix.infix);
	array_destroy(state->configuration.visible.headaffix.prefix);
	array_destroy(state->configuration.visible.headaffix.suffix);
	array_destroy(state->configuration.visible.tailaffix.infix);
	array_destroy(state->configuration.visible.tailaffix.prefix);
	array_destroy(state->configuration.visible.tailaffix.suffix);
	array_destroy(state->configuration.visible.unusual.indexed);
	array_destroy(state->configuration.visible.unusual.present);
	free(state);
}

int state_parse
(struct state* const state, struct array_const* of (char const*) const arguments) {
	size_t const count = array_const_count(arguments);
	if (count < 1) {
		struct call call = {
			.command = COMMAND_HELP,
			.arguments = NULL
		};
		if (array_const_add_last(state->execution.calls, &call) == -1)
			return -1;
		return 0;
	}
	for (size_t position = 0;
			position < count;
			++position) {
		char const* argument;
		if (array_const_read(&argument, arguments, position) == -1)
			return -1;
		struct action* action;
		action = NULL;/* if (resolution_create(
				(void** )&action, actions,
				(char const* (*)(void const*) )&action_name,
				argument,
				state->configuration.visible.completion) == -1)
			return -1; */
		if (action == NULL) {
			state->execution.status = STATUS_ERROR;
			if (array_const_truncate_whole(state->execution.calls) == -1) {
				// resolution_destroy(action);
				return -1;
			}
			size_t const size = strlen(argument) + 1;
			char* const first_argument = malloc(size);
			memcpy(first_argument, argument, size);
			struct call call = {
				.command = COMMAND_INFER,
				.arguments = first_argument
			};
			if (array_const_add_last(state->execution.calls, &call) == -1) {
				// resolution_destroy(action);
				return -1;
			}
			return 0;
		}
		// TODO parse arguments in a switch of action type
		// resolution_destroy(action);
	}
	return 0;
}

int state_execute
(struct state* const state) {
	struct array_const* of (struct call const) calls = state->execution.calls;
	size_t const count = array_const_count(calls);
	for (size_t position = 0;
			position < count;
			++position) {
		struct call call;
		if (array_const_read(&call, calls, position) == -1)
			state->execution.status = STATUS_ERROR;
		if (state->configuration.invisible.behavior == BEHAVIOR_CONTINUE
				|| state->execution.status == STATUS_NORMAL)
			call_execute(state, &call);
		free(call.arguments); // TODO consider call_destroy()
	}
	return 0;
}
