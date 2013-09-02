/**
@author Sampsa "Tuplanolla" Kiiskinen
@file
**/

#include "helper.h"

#include <stdio.h> // printf

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
Special Configuration Keys: preset (selection)\n\
";

int help(void) {
	return printf("%s", usage) <= 0;
}
