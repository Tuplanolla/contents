/**
Incomplete!

@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#include "parser.h"

#include <stddef.h> // size_t
#include <stdlib.h> // malloc(), free()
#include <stdio.h> // FILE, stdout, fprintf()

#include "state.h" // struct state, hold()
#include "data.h" // struct maybe, struct action, struct proposal
#include "resolver.h" // resolve(), correct(), filter()
#include "helper.h" // print_suggestions(), print_help()

int parse(struct state* const state) {
	if (state == NULL)
		return -1;
	for (size_t position = 0;
			;
			++position) {
		const char* const argument = state->arguments[position];
		struct maybe* const maybe = resolve(state->actions, argument, state->automatic_completion_length);
		if (maybe == NULL)
			return -1;
		int result = 0;
		switch (maybe->type) {
		case RESOLUTION_ERROR: {
			struct proposal* const proposal = correct(state->actions, argument, state->automatic_completion_length);
			struct proposal* const filtered_proposal = filter(proposal, state->suggestion_count, state->maximum_suggestion_distance);
			free(proposal);
			print_suggestions(state->target_stream, filtered_proposal);
			free(filtered_proposal);
			result = -1;
			goto done;
		}
		case RESOLUTION_END:
			if (position == 0)
				print_help(state->target_stream);
			result = 0;
			goto done;
		case RESOLUTION_COMMAND:
			if (hold(state, maybe) == -1) {
				result = -1;
				goto done;
			}
		}
		position += maybe->instance->arity; // TODO handle variadic commands and premature sentinels etc
		free(maybe);
		continue;
	done:
		free(maybe);
		return result;
	}
	return -1; // impossible
}
