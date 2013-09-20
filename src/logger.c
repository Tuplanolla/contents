/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#include "logger.h"

#include <stdarg.h> // va_list, va_start(), va_end()
#include <stdio.h> // vfprintf()

void vtrack(FILE* const stream, const char * format, va_list arguments) {
	vfprintf(stream, format, arguments);
}

void track(FILE* const stream, const char * format, ...) {
	va_list arguments;
	va_start(arguments, format);
	vtrack(stream, format, arguments);
	va_end(arguments);
}
