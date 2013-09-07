/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#include "parser.h"

#include <stdio.h> // FILE, stdout, fprintf()

#include "data.h" // struct action, struct container, struct holder
#include "resolver.h" // resolve(), approximate()
#include "executor.h" // execute()

#define ever (;;)

int parse(const char* const* arguments) {
	FILE* const debug_stream = stderr;
	const size_t limit = 3;

	const char* const argument;
	for ever {
		fprintf(debug_stream, "Resolving: %s\n", *arguments);
		const struct container container = resolve(*arguments, limit);
		fprintf(debug_stream, "Type: %u\n", container.type);
		switch (container.type) {
		case TYPE_ERROR:
			fprintf(debug_stream, "Resolution failed!\n");
			const struct holder guesses = approximate(*arguments, limit);
			return -1; // resolution problem
		case TYPE_END:
			fprintf(debug_stream, "That's all.\n");
			return 0; // done
		case TYPE_COMMAND:
			fprintf(debug_stream, "Instance: %s (%s)\n", container.instance.name, container.instance.abbreviation);
			if (execute(container.instance, arguments + 1) == -1) {
				fprintf(debug_stream, "Execution failed!\n");
				return -1; // execution problem
			}
		case TYPE_FLAG:
			break;
		}
		fprintf(debug_stream, "\n");
		arguments += 1 + container.instance.arity;
	}
}
