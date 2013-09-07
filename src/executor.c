/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#include "executor.h"

#include <stdio.h> // FILE, stdout

#include "data.h" // struct action
#include "helper.h" // print_help(), print_summary()

int execute(const struct action resolution, const char* const* const arguments) {
	FILE* const stream = stdout;

	switch (resolution.thing.c) {
	case COMMAND_HELP:
		return print_help(stream);
	case COMMAND_VERSION:
		return print_summary(stream);
	case COMMAND_TEST:
		return 0;
	}
	return -1;
}
