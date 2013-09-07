/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#include "resolver.h"

#include <stddef.h> // NULL, size_t
#include <string.h> // strlen()

#include "data.h" // COMMAND_*, TYPE_*, commands

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

struct holder approximate(const char* argument, size_t limit) {
	return (struct holder ){}; // TODO this
}
