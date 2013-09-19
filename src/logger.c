/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#include "logger.h"

#include <stdarg.h> // va_list, va_start, va_end
#include <stdio.h> // vfprintf

#include "state.h" // verbose_printing, log_stream

void vtrack(const char * format, va_list arguments) {
	if (verbose_printing)
		vfprintf(log_stream, format, arguments);
}

void track(const char * format, ...) {
	va_list arguments;
	va_start(arguments, format);
	vtrack(format, arguments);
	va_end(arguments);
}
