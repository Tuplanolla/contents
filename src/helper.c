/**
@author Sampsa "Tuplanolla" Kiiskinen
**/

#ifndef HELPER_H
#define HELPER_H

#include "helper.h"

#include <stdio.h> // printf

static const char usage[] = "\
Usage: indefinix (flags) (command) (arguments) (...) (flags)\n\
Configuring: configure\n\
             set (key) (value)\n\
             pop (key)\n\
             get (key)\n\
             obliterate\n\
Indexing: make (template)\n\
          edit\n\
          add (entry) (description)\n\
          remove (entry)\n\
          update (entry) (description)\n\
          lookup (entry)\n\
          find (string)\n\
          touch\n\
          destroy\n\
";

int help(void) {
	return printf("%s", usage) <= 0;
}

#endif
