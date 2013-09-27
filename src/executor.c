/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#include "executor.h"

#include "helper.h" // helper_usage(), helper_summary()
#include "state.h" // struct state

#include <stdio.h> // stderr, fprintf()
int execute_nothing
(struct state* const state, struct array* of (void*) const arguments) {
	(void )state;
	(void )arguments;
	fprintf(stderr, "Something isn't implemented.\n");
	return -1;
}

int execute_help
(struct state* const state, struct array* of (void*) const arguments) {
	(void )arguments;
	return helper_usage(state->output_stream);
}

int execute_version
(struct state* const state, struct array* of (void*) const arguments) {
	(void )arguments;
	return helper_summary(state->output_stream);
}
