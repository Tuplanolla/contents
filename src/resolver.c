/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#include "resolver.h"

#include <stddef.h> // NULL, size_t
#include <string.h> // strlen()
#include <stdlib.h> // qsort()

#include "data.h" // COMMAND_*, TYPE_*, commands, struct action, struct guess

struct container resolve(const char* const argument, const size_t limit) {
	struct container result;
	if (argument == NULL) {
		result.type = TYPE_END;
		return result;
	}
	result.type = TYPE_ERROR;
	const struct action* matching_commands[COMMAND_COUNT];
	size_t matching_command_count = COMMAND_COUNT;
	for (size_t match = 0;
			match < COMMAND_COUNT;
			++match)
		matching_commands[match] = &commands[match];
	for (size_t character = 0, length = strlen(argument);
			character < length;
			++character) {
		size_t matches = 0;
		for (size_t attempt = 0;
				attempt < matching_command_count;
				) {
			if (matching_commands[attempt]->name[character] == argument[character]) {
				++matches;
				++attempt;
			}
			else {
				--matching_command_count;
				for (size_t match = attempt;
						match < matching_command_count;
						++match)
					matching_commands[match] = matching_commands[match + 1];
			}
		}
		if (matches == 0) {
			result.type = TYPE_ERROR;
			break;
		} else if (matches == 1
				&& ((limit == 0
				&& character == length - 1)
				|| character >= limit - 1)) {
			result.type = TYPE_COMMAND;
			result.instance = *matching_commands[0];
		}
	}
	return result;
}

static int comparator(const struct guess* const x, const struct guess* const y) {
	if (x->distance < y->distance)
		return -1;
	if (x->distance > y->distance)
		return 1;
	return 0;
}

struct holder approximate(const char* argument, size_t limit) {
	struct holder result;
	for (size_t command = 0;
			command < COMMAND_COUNT;
			++command) {
		result.guesses[command].distance = distance(argument, commands[command].name);
		result.guesses[command].instance = &commands[command];
	}
	qsort(&result.guesses, COMMAND_COUNT, sizeof result.guesses[0], comparator);
	for (size_t command = 0;
			command < COMMAND_COUNT;
			++command)
		printf("Found %s (%zu).\n", result.guesses[command].instance->name, result.guesses[command].distance);
	return result;
}
