/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#include "call.h" // struct call

#include <stdlib.h> // abort()

#include "action.h" // enum command
#include "printer.h" // print_*()

int call_execute
(struct state* const state, struct call const* const call) {
	switch (call->command) {
	case COMMAND_CONFIGURE:
		return -1;
	case COMMAND_SET:
		return -1;
	case COMMAND_POP:
		return -1;
	case COMMAND_GET:
		return -1;
	case COMMAND_OBLITERATE:
		return -1;
	case COMMAND_MAKE:
		return -1;
	case COMMAND_EDIT:
		return -1;
	case COMMAND_ADD:
		return -1;
	case COMMAND_REMOVE:
		return -1;
	case COMMAND_UPDATE:
		return -1;
	case COMMAND_LOOKUP:
		return -1;
	case COMMAND_FIND:
		return -1;
	case COMMAND_TOUCH:
		return -1;
	case COMMAND_DESTROY:
		return -1;
	case COMMAND_HELP:
		return print_usage(state->configuration.invisible.output);
	case COMMAND_VERSION:
		return print_summary(state->configuration.invisible.output);
	case COMMAND_INFER:
		return -1;
	case COMMAND_BIND:
		return -1;
	case COMMAND_COUNT:
		abort();
	}
	return -1;
}
