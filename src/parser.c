/**
@author Sampsa "Tuplanolla" Kiiskinen
@file
**/

#include "parser.h"

#include <stdio.h> // printf

#include "project.h" // get_name, get_version, get_target

int parse(const char* const* arguments) {
	const char* argument;
	while ((argument = *arguments++)) {
		printf("%s\n", argument);
	}
	return 0;
}
