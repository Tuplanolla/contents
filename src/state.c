/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#include "state.h"

#include <stdbool.h> // bool, true
#include <stdio.h> // FILE, stdout, stderr

bool verbose_printing;

FILE* log_stream;

FILE* target_stream;

int initialize(void) {
	verbose_printing = true;
	log_stream = stderr;
	target_stream = stdout;
	return 0;
}
