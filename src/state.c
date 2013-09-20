/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#include "state.h"

#include <stddef.h> // NULL
#include <stdlib.h> // free, malloc
#include <stdio.h> // stdout
#include <stdbool.h> // true

#include "data.h" // actions, properties

void destroy_state(struct state* state) {
	free(state);
}

struct state* create_state(void) {
	struct state* const state = malloc(sizeof *state);
	state->actions = &actions[0];
	state->properties = &properties[0];
	state->automatic_completion_length = 0;
	state->suggestion_count = 3;
	state->maximum_suggestion_distance = 2;
	state->first_executable = NULL;
	state->last_executable = NULL;

	state->log_stream = stdout;
	state->target_stream = stdout;
	state->verbose_printing = true;
	return state;
}

int hold(struct state* const state, struct maybe* const maybe) {
	if (state == NULL || maybe == NULL)
		return -1;
	struct executable* const last = malloc(sizeof *last);
	if (last == NULL)
		return -1;
	last->next = NULL;
	last->maybe = maybe;
	if (state->first_executable == NULL)
		state->first_executable = last;
	if (state->last_executable != NULL)
		state->last_executable->next = last;
	state->last_executable = last;
	return 0;
}

const struct maybe* release(struct state* const state) {
	if (state == NULL)
		return NULL;
	struct executable* const first = state->first_executable;
	if (first == NULL)
		return NULL;
	const struct maybe* const maybe = first->maybe;
	free(first);
	struct executable* const second = state->first_executable->next;
	if (second == NULL)
		state->last_executable = NULL;
	state->first_executable = second;
	return maybe;
}
