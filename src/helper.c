/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#include "helper.h"

#include <stddef.h> // NULL
#include <stdio.h> // FILE, fprintf()

#include "array.h" // struct array, array_count(), array_read()
#include "project.h" // project_name(), project_target(), project_version()
#include "suggestion.h" // struct suggestion
#include "syntax.h" // of ()

static char const usage[] = "\
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
Special Commands: help\n\
                  version\n\
                  infer\n\
                  bind (command) (arguments)\n\
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

int helper_usage
(FILE* const stream) {
	if (stream == NULL)
		return -1;
	if (fprintf(stream, "%s\n", usage) < 0)
		return -1;
	return 0;
}

int helper_summary
(FILE* const stream) {
	if (stream == NULL)
		return -1;
	if (fprintf(stream, "%s version %s for %s\n", project_name(), project_version(), project_target()) < 0)
		return -1;
	return 0;
}

int helper_suggestions
(FILE* const stream, struct array* of (struct suggestion) const suggestions, char const* (* const accessor)(void const*)) {
	size_t const count = array_count(suggestions);
	if (stream == NULL || count == 0)
		return -1;
	if (fprintf(stream, "Did you mean") < 0)
		return -1;
	size_t const last = count - 1;
	for (size_t current = 0;
			current < count;
			++current) {
		if (current == 0) {
			if (fprintf(stream, " ") < 0)
				return -1;
		}
		else if (current < last) {
			if (fprintf(stream, ", ") < 0)
				return -1;
		}
		else
			if (fprintf(stream, " or ") < 0)
				return -1;
		struct suggestion suggestion;
		if (array_read(&suggestion, suggestions, current) == -1)
			return -1;
		if (fprintf(stream, "%s", accessor(suggestion.instance)) < 0)
			return -1;
	}
	if (fprintf(stream, "?\n") < 0)
		return -1;
	return 0;
}
