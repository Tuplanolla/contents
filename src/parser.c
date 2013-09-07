/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#include "parser.h"

#include <stdio.h> // FILE, fprintf, stdout

#include "data.h" // struct action, struct container
#include "resolver.h" // resolve
#include "executor.h" // execute

int parse(const char* const* arguments) {
	FILE* const debug_stream = stderr;

	fprintf(debug_stream, "Resolving: %s\n", *arguments);
	const struct container container = resolve(*arguments, 3);
	fprintf(debug_stream, "Type: %u\n", container.type);
	switch (container.type) {
	case TYPE_ERROR:
		fprintf(debug_stream, "Resolution failed!\n");
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
	return parse(arguments + 1 + container.instance.arity); // next with tco
}
