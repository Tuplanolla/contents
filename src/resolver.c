/**
Incomplete!

@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#include "resolver.h"

#include <stddef.h> // NULL, size_t
#include <string.h> // strlen()
#include <stdlib.h> // qsort(), malloc(), free()

#include "data.h" // COMMAND_*, TYPE_*, commands, struct action, struct guess
#include "calculator.h" // distance()

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
			} else { // TODO shift
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

static void destructive_truncate(char* const x, const size_t limit) {
	size_t character;
	for (character = 0;
			character < limit;
			++character) {
		if (x[character] == '\0')
			return;
	}
	x[character] = '\0';
}

static char* allocating_truncate(const char* const x, const size_t limit) {
	char* const result = malloc(strlen(x) + 1);
	size_t character;
	for (character = 0;
			character < limit;
			++character) {
		result[character] = x[character];
		if (x[character] == '\0')
			return result;
	}
	result[character] = '\0';
	return result;
}

struct holder correct(const char* const argument, size_t limit) { // TODO null
	struct holder result = {
		.count = COMMAND_COUNT
	};
	char* const truncated_argument = allocating_truncate(argument, limit); // TODO rethink limit
	for (size_t command = 0;
			command < result.count;
			++command) {
		const char* const guess = commands[command].name;
		char* const truncated_guess = allocating_truncate(guess, limit);
//		result.guesses[command].distance = distance(truncated_argument, truncated_guess);
		result.guesses[command].distance = distance(argument, guess);
		result.guesses[command].instance = &commands[command];
		free(truncated_guess);
	}
	free(truncated_argument);
	return result;
}

static int comparator(const void* const x, const void* const y) {
	const struct guess* const actual_x = x,
			* const actual_y = y;
	if (actual_x->distance < actual_y->distance)
		return -1;
	if (actual_x->distance > actual_y->distance)
		return 1;
	return 0;
}

struct holder filter(struct holder result, const size_t limit, const size_t score) {
	qsort(&result.guesses, result.count, sizeof result.guesses[0], comparator);
	for (size_t now = 0;
			now < result.count;
			++now) {
		if (now >= limit
				|| result.guesses[now].distance > score) {
			result.count = now;
			break;
		}
	}
	return result;
}
