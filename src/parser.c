/**
Incomplete!

@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#include "parser.h"

#include <stdbool.h> // true
#include <stdio.h> // FILE, stdout, fprintf()

#include "data.h" // struct action, struct maybe, struct proposal
#include "resolver.h" // resolve(), correct(), organize()
#include "executor.h" // execute()
#include "calculator.h" // minimum()
#include "state.h" // target_stream
#include "helper.h" // print_suggestions(), print_help()
#include "logger.h" // track()

int parse(const char* const* const arguments) {
	const size_t autocompletion = 3, suggestions = 3, score = 5; // TODO remove

	for (size_t position = 0, number = 0;
			true;
			++position, ++number) {
		const char* const argument = arguments[position];
		track("Resolving the argument at the position %zu as the command number %zu.\n", position + 1, number + 1);
		const struct maybe container = resolve(argument, autocompletion);
		switch (container.type) {
		case RESOLUTION_ERROR:
			track("Resolved the argument \"%s\" to nothing.\n", argument);
			const struct proposal unsorted_guesses = correct(argument, autocompletion);
			const struct proposal guesses = filter(unsorted_guesses, suggestions, score);
			print_suggestions(target_stream, guesses);
			return -1;
		case RESOLUTION_END:
			track("Resolved the end of arguments.\n");
			if (number == 0)
				print_help(target_stream);
			return 0;
		case RESOLUTION_COMMAND:
			track("Resolved the argument \"%s\" to the command \"%s\".\n", argument, container.instance.name);
			if (execute(container.instance, &arguments[position + 1]) == -1) { // TODO delay execution
				track("Couldn't execute the command \"%s\".\n", container.instance.name);
				return -1;
			}
		case RESOLUTION_FLAG:
			break;
		}
		position += container.instance.arity; // TODO handle variadic commands and premature sentinels etc
	}
	track("Reached a supposedly impossible condition.\n");
	return -1;
}
