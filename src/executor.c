/**
Incomplete!

@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#include "executor.h"

#include "data.h" // struct action
#include "state.h" // target_stream
#include "helper.h" // print_help(), print_summary()

int execute(const struct action action, const char* const* const arguments) {
	switch (action.command) {
	case COMMAND_HELP:
		return print_help(target_stream);
	case COMMAND_VERSION:
		return print_summary(target_stream);
	}
	return -1;
}
