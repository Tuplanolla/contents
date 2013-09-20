/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#include "helper.h"

#include <stdio.h> // FILE, fprintf()
#include <stddef.h> // NULL

#include "project.h" // project_name, project_version, project_target

static const char usage[] = "\
Usage: indefinix (flags) (command) (arguments) (...) (flags)\n\
Configuration Commands: configure\n\
                        set (key) (values)\n\
                        pop (key)\n\
                        get (key)\n\
                        obliterate\n\
Indexing Commands: make (template)\n\
                   edit\n\
                   add (entry) (description)\n\
                   remove (entry)\n\
                   update (entry) (description)\n\
                   lookup (entry)\n\
                   find (string)\n\
                   touch\n\
                   destroy\n\
General Commands: help\n\
                  version\n\
Special Commands: bind (command) (arguments)\n\
Configuration Keys: location (name)\n\
                    editor (path)\n\
                    completion (number)\n\
                    order (sorting) (grouping) (hiding)\n\
                    wrapping (continuation)\n\
                    justification (alignment) (alignment)\n\
                    filling (padding) (padding)\n\
                    interaction (answer)\n\
                    affix (string) (string) (string)\n\
                    headaffix (string) (string) (string)\n\
                    tailaffix (string) (string) (string)\n\
                    unusual (string) (string)\n\
Special Configuration Keys: preset (selection)";

int print_help(FILE* const stream) {
	if (stream == NULL)
		return -1;
	return -(fprintf(stream, "%s\n", usage) <= 0);
}

int print_summary(FILE* const stream) {
	if (stream == NULL)
		return -1;
	return -(fprintf(stream, "%s version %s for %s\n", project_name, project_version, project_target) <= 0);
}

int print_suggestions(FILE* const stream, const struct proposal* const proposal) {
	const size_t count = proposal->count;
	if (stream == NULL || count == 0)
		return -1;
	int result = 0;
	result |= fprintf(stream, "Did you mean") <= 0;
	const size_t last = count - 1;
	for (size_t current = 0;
			current < count;
			++current) {
		if (current == 0)
			result |= fprintf(stream, " ");
		else if (current < last)
			result |= fprintf(stream, ", ");
		else
			result |= fprintf(stream, " or ");
		const struct suggestion* const guess = &proposal->suggestions[current];
		result |= fprintf(stream, "%s", guess->action->name);
	}
	result |= fprintf(stream, "?\n");
	return -(result != 0);
}
