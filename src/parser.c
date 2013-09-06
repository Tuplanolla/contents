/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#include "parser.h"

#include <stdio.h> // FILE, fprintf, stdout

#include "data.h" // struct container
#include "resolver.h" // resolve
#include "project.h" // project_name, project_version, project_target
#include "helper.h" // help

int execute(const struct action resolution) {
	FILE* const debug_stream = stdout;
	FILE* const stream = stdout;

	/*
	switch (resolution.thing) {
	case COMMAND_HELP:
		return print_help(stream);
	case COMMAND_VERSION:
	}
	*/
	return -1;
}

int parse(const char* const* arguments) {
	printf("Resolving: %s\n", *arguments);
	const struct container container = resolve(*arguments, 3);
	printf("Type: %d\n", container.type);
	if (container.type > 1)
		printf("Instance: %s (%s)\n", container.instance.name, container.instance.abbreviation);
	switch (container.type) {
	case TYPE_ERROR:
		break; // return -1; // resolvation problem
	case TYPE_END:
		return 0; // done
	case TYPE_COMMAND:
		/*
		if (execute(container.instance, arguments + 1) != 0)
			return -1; // execution problem
			*/
	case TYPE_FLAG:
		break;
	}
	printf("\n");
	return parse(arguments + 1); // next with tco
}
