/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#include "parser.h"

#include <stdio.h> // FILE, fprintf, stdout

#include "data.h" // struct container
#include "resolver.h" // resolve
#include "helper.h" // help

int execute(const struct action resolution) {
	FILE* const debug_stream = stdout;
	FILE* const stream = stdout;

	switch (resolution.thing.c) {
	case COMMAND_HELP:
		return print_help(stream);
	case COMMAND_VERSION:
		return print_summary(stream);
	}
	return -1;
}

int parse(const char* const* arguments) {
	FILE* const debug_stream = stdout;
	FILE* const stream = stdout;

	fprintf(debug_stream, "Resolving: %s\n", *arguments);
	const struct container container = resolve(*arguments, 3);
	fprintf(debug_stream, "Type: %d\n", container.type);
	switch (container.type) {
	case TYPE_ERROR:
		fprintf(debug_stream, "Resolution failed!\n");
		return -1; // resolution problem
	case TYPE_END:
		fprintf(debug_stream, "That's all.\n");
		return 0; // done
	case TYPE_COMMAND:
		fprintf(debug_stream, "Instance: %s (%s)\n", container.instance.name, container.instance.abbreviation);
		/*
		if (execute(container.instance, arguments + 1) != 0)
			return -1; // execution problem
			*/
	case TYPE_FLAG:
		break;
	}
	fprintf(debug_stream, "\n");
	return parse(arguments + 1 + container.instance.arity); // next with tco
}
