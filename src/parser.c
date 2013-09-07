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
			fprintf(debug_stream, "Did you mean %s, %s or %s?\n", guesses.guesses[0].instance->name,
					guesses.guesses[1].instance->name,
					guesses.guesses[2].instance->name); // dangerous
			return -1; // resolution problem
		case TYPE_END:
			return 0; // done
		case TYPE_COMMAND:
			if (execute(container.instance, arguments + 1) == -1) {
				return -1; // execution problem
			}
		case TYPE_FLAG:
			break;
		}
		arguments += 1 + container.instance.arity;
	}
}
