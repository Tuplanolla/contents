/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#include "executor.h"

#include "action.h" // action_name()
#include "array.h" // struct array, array_read()
#include "helper.h" // helper_usage(), helper_suggestions(), helper_summary()
#include "state.h" // struct state
#include "suggestion.h" // struct suggestion
#include "suggestions.h" // suggestions_create(), suggestions_destroy()
#include "syntax.h" // of ()

#include <stdio.h> // stderr, fprintf()
int execute_nothing
(struct state* const state, struct array* of (char const*) const arguments) {
	(void )state;
	(void )arguments;
	fprintf(stderr, "Something isn't implemented.\n");
	return -1;
}

int execute_help
(struct state* const state, struct array* of (char const*) const arguments) {
	(void )arguments;
	return helper_usage(state->output_stream);
}

int execute_version
(struct state* const state, struct array* of (char const*) const arguments) {
	(void )arguments;
	return helper_summary(state->output_stream);
}

int execute_infer
(struct state* const state, struct array* of (char const*) const arguments) {
	char const* argument;
	if (array_read(&argument, arguments, 0) == -1)
		return -1;
	struct array* of (struct suggestion) suggestions;
	char const* (* const accessor)(void const*) = (char const* (*)(void const*) )&action_name;
	if (suggestions_create(&suggestions, state->actions, accessor, argument, state->automatic_completion_limit, state->suggestion_count, state->suggestion_edit_distance) == -1)
		return -1;
	if (helper_suggestions(state->output_stream, suggestions, accessor) == -1) {
		suggestions_destroy(suggestions);
		return -1;
	}
	suggestions_destroy(suggestions);
	return 0;
}
