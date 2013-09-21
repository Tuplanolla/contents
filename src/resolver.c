/**
Incomplete!

@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#include "resolver.h"

#include <stddef.h> // size_t, NULL
#include <string.h> // strlen(), memcpy(), memmove()
#include <stdlib.h> // qsort(), malloc(), free()

#include "state.h" // COMMAND_*, TYPE_*, struct action, struct suggestion
#include "calculator.h" // distance(), minimum(), maximum()

int destroy_resolution(struct resolution* const resolution) {
	free(resolution);
	return 0;
}

struct resolution* create_resolution(const struct actions* const actions, const char* const argument, const size_t limit) {
	struct resolution* const resolution = malloc(sizeof *resolution);
	if (resolution == NULL)
		return NULL;
	resolution->action = NULL;
	const struct action** const candidates = malloc(actions->count * sizeof *candidates);
	if (candidates == NULL) {
		free(resolution);
		return NULL;
	}
	for (size_t action = 0;
			action < actions->count;
			++action)
		candidates[action] = &actions->actions[action];
	resolution->type = TYPE_ERROR;
	const size_t argument_length = strlen(argument);
	for (size_t character = 0, candidate_count = actions->count;
			character < argument_length;
			++character) {
		for (size_t candidate = 0;
				candidate < candidate_count;
				) {
			if (candidates[candidate]->name[character] == argument[character]) {
				++candidate;
			} else {
				--candidate_count;
				memmove(&candidates[candidate], &candidates[candidate + 1], (candidate_count - candidate) * sizeof *candidates);
			}
		}
		if (candidate_count == 0) {
			resolution->type = TYPE_ERROR;
			break;
		} else if (candidate_count == 1
				&& ((limit == 0 && character == argument_length - 1)
				|| (limit != 0 && character >= limit - 1))) {
			resolution->type = TYPE_COMMAND;
			resolution->action = candidates[0];
		}
	}
	free(candidates);
	return resolution;
}

static int destroy_truncation(char* const string) {
	free(string);
	return 0;
}

static char* create_truncation(const char* const string, const size_t limit) {
	const size_t length = minimum(limit, strlen(string));
	char* const result = malloc(length + 1);
	memcpy(result, string, length);
	result[length] = '\0';
	return result;
}

struct proposal* correct(const struct actions* actions, const char* const argument, const size_t limit) {
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
		const char* const suggestion = actions->actions[action].name;
		if (limit != 0) {
			char* const truncated_guess = create_truncation(suggestion, truncation_length);
			proposal->suggestions[action].distance = distance(argument, truncated_guess);
			destroy_truncation(truncated_guess);
		} else
			proposal->suggestions[action].distance = distance(argument, suggestion);
		proposal->suggestions[action].action = &actions->actions[action];
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
	const size_t size = count * sizeof *proposal->suggestions;
	struct suggestion* const suggestions = malloc(size);
	memcpy(suggestions, proposal->suggestions, size);
	qsort(suggestions, count, sizeof *suggestions, comparator);
	struct proposal* const result = malloc(sizeof *result);
	result->suggestions = suggestions;
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
