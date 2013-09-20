/**
Incomplete!

@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#include "executor.h"

#include "data.h" // struct action
#include "state.h" // target_stream
#include "helper.h" // print_help(), print_summary()

int execute(struct state* const state) {
	switch (state->actions[15].command) {
	case COMMAND_HELP:
		return print_help(state->target_stream);
	case COMMAND_VERSION:
		return print_summary(state->target_stream);
	}
	return -1;
}
