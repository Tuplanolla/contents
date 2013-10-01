/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#include "indefinix.h"

#include <stddef.h> // NULL, size_t
#include <stdio.h> // stderr, stdout
#include <stdlib.h> // free(), malloc()
#include <string.h> // strcpy()

#include "state.h" // struct state

int indefinix_invoke
(char const* const* const arguments, size_t const count) {

#define EMPTY {\
	.capacity = 0,\
	.count = 0,\
	.elements = NULL\
}

	struct state state = {
		.status = 0,
		.invocation = EMPTY,
		.configuration = {
			.developer = {
				.place = EMPTY,
				.behavior = BEHAVIOR_ABORT,
				.verbosity = VERBOSITY_MAXIMAL,
				.suggestion = {
					.amount = 3,
					.distance = 2
				},
				.output = stdout
			},
			.user = {
				.location = EMPTY,
				.editor = EMPTY,
				.completion = 1,
				.order = {
					.sorting = SORTING_NORMAL,
					.grouping = GROUPING_DIRECTORIES,
					.hiding = HIDING_HIDDEN
				},
				.wrapping = CONTINUATION_WRAP,
				.justification = {
					.first = ALIGNMENT_LEFT,
					.second = ALIGNMENT_LEFT
				},
				.filling = {
					.first = PADDING_FILL,
					.second = PADDING_FILL
				},
				.interaction = ANSWER_NONE,
				.affix = {
					.prefix = EMPTY,
					.infix = EMPTY,
					.suffix = EMPTY
				},
				.headaffix = {
					.prefix = EMPTY,
					.infix = EMPTY,
					.suffix = EMPTY
				},
				.tailaffix = {
					.prefix = EMPTY,
					.infix = EMPTY,
					.suffix = EMPTY
				},
				.unusual = {
					.indexed = EMPTY,
					.present = EMPTY
				}
			}
		}
	};

#undef EMPTY

	int status = 0;

#define CREATE(name, array) do {\
	name.elements = malloc(sizeof array);\
	if (name.elements == NULL) {\
		status = -1;\
		goto destroy;\
	}\
	name.capacity = sizeof array;\
	memcpy(name.elements, array, sizeof array);\
	name.count = sizeof array;\
} while (0)

	CREATE(state.configuration.developer.place, "~/.indefinix");
	CREATE(state.configuration.user.location, "./INDEX");
	CREATE(state.configuration.user.affix.prefix, "");
	CREATE(state.configuration.user.affix.infix, "   ");
	CREATE(state.configuration.user.affix.suffix, "");
	CREATE(state.configuration.user.headaffix.infix, " - ");
	CREATE(state.configuration.user.unusual.indexed, "not indexed");
	CREATE(state.configuration.user.unusual.present, "not present");

#undef CREATE

#define MODIFY(name, array) do {\
	if (sizeof array > name.capacity) {\
		name.elements = malloc(sizeof array);\
		if (name.elements == NULL) {\
			status = -1;\
			goto destroy;\
		}\
		name.capacity = sizeof array;\
	}\
	memcpy(name.elements, array, sizeof array);\
	name.count = sizeof array;\
} while (0)

	MODIFY(state.configuration.user.affix.suffix, "!");

#undef MODIFY

destroy:

#define DESTROY(name) do {\
	if (name.elements != NULL) {\
		free(name.elements);\
		name.capacity = 0;\
		name.elements = NULL;\
		name.count = 0;\
	}\
} while (0)

	DESTROY(state.configuration.user.unusual.present);
	DESTROY(state.configuration.user.unusual.indexed);
	DESTROY(state.configuration.user.tailaffix.suffix);
	DESTROY(state.configuration.user.tailaffix.infix);
	DESTROY(state.configuration.user.tailaffix.prefix);
	DESTROY(state.configuration.user.headaffix.suffix);
	DESTROY(state.configuration.user.headaffix.infix);
	DESTROY(state.configuration.user.headaffix.prefix);
	DESTROY(state.configuration.user.affix.suffix);
	DESTROY(state.configuration.user.affix.infix);
	DESTROY(state.configuration.user.affix.prefix);
	DESTROY(state.configuration.user.editor);
	DESTROY(state.configuration.user.location);
	DESTROY(state.configuration.developer.place);
	DESTROY(state.invocation);

#undef DESTROY

	return status;
}
