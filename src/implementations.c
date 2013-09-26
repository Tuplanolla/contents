/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#include "implementations.h"

#include "helper.h" // helper_usage(), helper_summary()
#include "state.h" // struct state

int execute_help
(struct state* const state) {
	return helper_usage(state->output_stream);
}

int execute_version
(struct state* const state) {
	return helper_summary(state->output_stream);
}
