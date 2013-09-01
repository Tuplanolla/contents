/**
@author Sampsa "Tuplanolla" Kiiskinen
@file
**/

#include "helper.h"

#include <stdio.h> // printf

static const char usage[] = "\
Usage: indefinix (flags) (command) (arguments) (...) (flags)\n\
Configuration Commands: configure\n\
                        set (key) (value)\n\
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
Special Commands: bind\n\
Configuration Keys: location (name)\n\
                    editor (path)\n\
                    completion (number)\n\
                    order (sorting) (grouping) (hiding)\n\
                    wrapping (wrapping)\n\
                    justification (alignment) (alignment)\n\
                    filling (padding) (padding)\n\
                    yes\n\
                    no\n\
                    infix (string)\n\
                    prefix (string)\n\
                    suffix (string)\n\
                    headinfix (string)\n\
                    headprefix (string)\n\
                    headsuffix (string)\n\
                    tailinfix (string)\n\
                    tailprefix (string)\n\
                    tailsuffix (string)\n\
                    unusual (string) (string)\n\
Special Configuration Keys: all\n\
";

int help(void) {
	return printf("%s", usage) <= 0;
}
