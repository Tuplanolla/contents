/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#include "truncation.h"

#include <stddef.h> // NULL, size_t
#include <stdlib.h> // free(), malloc()
#include <string.h> // memcpy(), strlen()

#include "calculator.h" // minimum()

int truncation_create
(char** const result, char const* const string, size_t const limit) {
	size_t const length = minimum(limit, strlen(string));
	char* const truncation = malloc(length + 1);
	if (truncation == NULL)
		return -1;
	memcpy(truncation, string, length);
	truncation[length] = '\0';
	*result = truncation;
	return 0;
}

int truncation_destroy
(char* const string) {
	free(string);
	return 0;
}
