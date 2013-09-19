/**
Incomplete!

@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#include "resolver.h"

#include <stddef.h> // NULL, size_t
#include <string.h> // strlen()
#include <stdlib.h> // qsort(), malloc(), free()

#include "data.h" // COMMAND_*, RESOLUTION_*, actions, struct action, struct guess
#include "calculator.h" // distance(), maximum()

struct maybe resolve(const char* const argument, const size_t limit) {
	struct maybe result;
	if (argument == NULL) {
		result.type = RESOLUTION_END;
		return result;
	}
	result.type = RESOLUTION_ERROR;
	const struct action* matching_commands[COMMAND_COUNT];
	size_t matching_command_count = COMMAND_COUNT;
	for (size_t match = 0;
			match < COMMAND_COUNT;
			++match)
		matching_commands[match] = &actions[match];
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
			result.type = RESOLUTION_ERROR;
			break;
		} else if (matches == 1
				&& ((limit == 0
				&& character == length - 1)
				|| character >= limit - 1)) {
			result.type = RESOLUTION_COMMAND;
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
	char* const result = malloc(strlen(x) + 1); // TODO short circuit
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

struct proposal correct(const char* const argument, size_t limit) { // TODO null
	struct proposal proposal = {
		.count = COMMAND_COUNT
	};
	const size_t autocompletion = limit == 0 ? 1024 : limit; // TODO bad
	const size_t argument_length = strlen(argument);
	for (size_t command = 0;
			command < proposal.count;
			++command) {
		const char* const guess = actions[command].name;
		char* const truncated_guess = allocating_truncate(guess, maximum(autocompletion, argument_length));
		proposal.guesses[command].distance = distance(argument, truncated_guess);
		proposal.guesses[command].instance = &actions[command];
		free(truncated_guess);
	}
	return proposal;
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

struct proposal filter(struct proposal proposal, const size_t limit, const size_t distance) {
	qsort(&proposal.guesses, proposal.count, sizeof proposal.guesses[0], comparator);
	for (size_t guess = 0;
			guess < proposal.count;
			++guess) {
		if (guess >= limit
				|| proposal.guesses[guess].distance > distance) {
			proposal.count = guess;
			break;
		}
	}
	return proposal;
}
