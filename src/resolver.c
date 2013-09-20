/**
Incomplete!

@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#include "resolver.h"

#include <stddef.h> // size_t, NULL
#include <string.h> // strlen(), memcpy()
#include <stdlib.h> // qsort(), malloc(), free()

#include "state.h" // COMMAND_*, RESOLUTION_*, struct action, struct suggestion
#include "calculator.h" // distance(), minimum(), maximum()

struct maybe* resolve(const struct action* const actions, const char* const argument, const size_t limit) {
	if (actions == NULL)
		return NULL;
	struct maybe* const maybe = malloc(sizeof *maybe);
	if (maybe == NULL)
		return NULL;
	if (argument == NULL) {
		maybe->type = RESOLUTION_END;
		return maybe;
	}
	maybe->type = RESOLUTION_ERROR;
	const struct action** const matching_actions = malloc(COMMAND_COUNT * sizeof *matching_actions);
	size_t matching_action_count = COMMAND_COUNT;
	for (size_t match = 0;
			match < COMMAND_COUNT;
			++match)
		matching_actions[match] = &actions[match];
	for (size_t character = 0, length = strlen(argument);
			character < length;
			++character) {
		size_t matches = 0;
		for (size_t potential_match = 0;
				potential_match < matching_action_count;
				) {
			if (matching_actions[potential_match]->name[character] == argument[character]) {
				++matches;
				++potential_match;
			} else {
				--matching_action_count;
				for (size_t match = potential_match;
						match < matching_action_count;
						++match)
					matching_actions[match] = matching_actions[match + 1];
			}
		}
		if (matches == 0) {
			maybe->type = RESOLUTION_ERROR;
			break;
		} else if (matches == 1
				&& ((limit == 0
				&& character == length - 1)
				|| character >= limit - 1)) {
			maybe->type = RESOLUTION_COMMAND;
			maybe->instance = matching_actions[0];
		}
	}
	free(matching_actions);
	return maybe;
}

static char* truncate(const char* const str, const size_t limit) {
	const size_t length = minimum(limit, strlen(str));
	char* const result = malloc(length + 1);
	memcpy(result, str, length);
	result[length] = '\0';
	return result;
}

struct proposal* correct(const struct action* actions, const char* const argument, const size_t limit) {
	if (argument == NULL)
		return NULL;
	struct proposal* const proposal = malloc(sizeof *proposal);
	if (proposal == NULL)
		return NULL;
	proposal->count = COMMAND_COUNT;
	size_t truncation_length;
	if (limit != 0)
		truncation_length = maximum(limit, strlen(argument));
	for (size_t action = 0;
			action < proposal->count;
			++action) {
		const char* const suggestion = actions[action].name;
		if (limit != 0) {
			char* const truncated_guess = truncate(suggestion, truncation_length);
			proposal->guesses[action].distance = distance(argument, truncated_guess);
			free(truncated_guess);
		} else
			proposal->guesses[action].distance = distance(argument, suggestion);
		proposal->guesses[action].instance = &actions[action];
	}
	return proposal;
}

static int comparator(const void* const x, const void* const y) {
	const struct suggestion* const actual_x = x;
	const struct suggestion* const actual_y = y;
	if (actual_x->distance < actual_y->distance)
		return -1;
	if (actual_x->distance > actual_y->distance)
		return 1;
	return 0;
}

struct proposal* filter(const struct proposal* const proposal, const size_t limit, const size_t distance) {
	const size_t count = proposal->count;
	const size_t size = count * sizeof *proposal->guesses;
	struct suggestion* const suggestions = malloc(size);
	memcpy(suggestions, proposal->guesses, size);
	qsort(suggestions, count, sizeof *suggestions, comparator);
	struct proposal* const result = malloc(sizeof *result);
	result->guesses = suggestions;
	for (size_t suggestion = 0;
			suggestion < count;
			++suggestion) {
		if (suggestion >= limit
				|| suggestions[suggestion].distance > distance) {
			result->count = suggestion;
			break;
		}
	}
	return result;
}
