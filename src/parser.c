/**
Incomplete!

@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#include "parser.h"

#include <stdio.h> // FILE, stdout, fprintf()

#include "data.h" // struct action, struct container, struct holder
#include "resolver.h" // resolve(), correct(), organize()
#include "executor.h" // execute()
#include "calculator.h" // minimum()

int parse(const char* const* arguments) {
	FILE* const debug_stream = stderr;
	const size_t limit = 4,
			score = 5,
			suggerations = 3;

	for (size_t position = 0;
			;
			++position) {
		fprintf(debug_stream, "Resolving %s.\n", *arguments);
		const struct container container = resolve(*arguments, limit);
		switch (container.type) {
		case TYPE_ERROR:
			fprintf(debug_stream, "Resolution failed!\n");
			const struct holder unsorted_guesses = correct(*arguments, limit);
			const struct holder guesses = filter(unsorted_guesses, suggerations, score);
			fprintf(debug_stream, "Did you mean");
			for (size_t iterator = 0;
					iterator < guesses.count;
					++iterator) {
				if (iterator != 0) {
					if (iterator == guesses.count - 1)
						fprintf(debug_stream, " or ");
					else
						fprintf(debug_stream, ", ");
				} else
					fprintf(debug_stream, " ");
				fprintf(debug_stream, "%s (distance %zu)", guesses.guesses[iterator].instance->name, guesses.guesses[iterator].distance);
			}
			fprintf(debug_stream, "?\n");
			return -1; // resolution problem
		case TYPE_END:
			return 0; // done
		case TYPE_COMMAND:
			if (execute(container.instance, arguments + 1) == -1) {
				fprintf(debug_stream, "Failed!\n");
				return -1; // execution problem
			}
		case TYPE_FLAG:
			break;
		}
		arguments += container.instance.arity + 1; // TODO handle variadic etc
	}
}
