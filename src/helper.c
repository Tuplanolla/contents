/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#include "helper.h"

#include <stdio.h> // fprintf

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
	return fprintf(stream, "%s\n", usage) <= 0;
}

int print_summary(FILE* const stream) {
	return fprintf(stream, "%s version %s for %s\n", project_name, project_version, project_target) <= 0;
}
