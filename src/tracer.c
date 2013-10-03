/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#include "tracer.h"

#include <stdarg.h> // va_list, va_end(), va_start()
#include <stddef.h> // NULL
#include <stdio.h> // FILE, vsnprintf()
#include <stdlib.h> // free(), malloc()
#include <string.h> // memcpy()

int trace_va
(char const* const format, va_list arguments) {
	int const length = vsnprintf(NULL, 0, format, arguments);
	if (length < 1)
		return -1;
	size_t const actual_length = (size_t )length;
	char* const actual_format = malloc(actual_length + 2);
	if (actual_format == NULL)
		return -1;
	memcpy(actual_format, format, actual_length);
	memcpy(actual_format + actual_length, "\n\0", 2);
	if (vprintf(actual_format, arguments) < 0) {
		free(actual_format);
		return -1;
	}
	free(actual_format);
	return 0;
}

int trace
(char const* const format, ...) {
	va_list arguments;
	va_start(arguments, format);
	int const status = trace_va(format, arguments);
	va_end(arguments);
	return status;
}
