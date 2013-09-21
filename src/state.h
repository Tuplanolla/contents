/**
Keeps the state away from the user.

@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#ifndef STATE_H
#define STATE_H

#include <stddef.h> // size_t
#include <stdbool.h> // bool
#include <stdio.h> // FILE

enum type {
	TYPE_ERROR,
	TYPE_END,
	TYPE_COMMAND,
	TYPE_FLAG,

	TYPE_COUNT
};

enum arity {
	ARITY_NILADIC,
	ARITY_MONADIC,
	ARITY_DYADIC,
	ARITY_TRIADIC,
	ARITY_TETRADIC,
	ARITY_PENTADIC,
	ARITY_HEXADIC,
	ARITY_HEPTADIC,
	ARITY_OCTADIC,
	ARITY_NONADIC,
	ARITY_DECADIC,
	ARITY_VARIADIC,

	ARITY_COUNT
};

enum command {
	COMMAND_CONFIGURE,
	COMMAND_SET,
	COMMAND_POP,
	COMMAND_GET,
	COMMAND_OBLITERATE,
	COMMAND_MAKE,
	COMMAND_EDIT,
	COMMAND_ADD,
	COMMAND_REMOVE,
	COMMAND_UPDATE,
	COMMAND_LOOKUP,
	COMMAND_FIND,
	COMMAND_TOUCH,
	COMMAND_DESTROY,
	COMMAND_HELP,
	COMMAND_VERSION,
	COMMAND_BIND,
	COMMAND_SUGGEST,

	COMMAND_COUNT
};

enum key {
	KEY_LOCATION,
	KEY_EDITOR,
	KEY_COMPLETION,
	KEY_ORDER,
	KEY_WRAPPING,
	KEY_JUSTIFICATION,
	KEY_FILLING,
	KEY_INTERACTION,
	KEY_AFFIX,
	KEY_HEADAFFIX,
	KEY_TAILAFFIX,
	KEY_UNUSUAL,
	KEY_PRESET,

	KEY_COUNT
};

struct action {
	const char* name;
	enum arity arity;
	enum command command;
};

struct actions {
	size_t count;
	const struct action* actions;
};

struct property {
	const char* name;
	const char* abbreviation;
	enum arity arity;
	enum key key;
};

struct invocation {
	const struct action* command;
	const char* const* arguments;
};

struct resolution {
	enum type type;
	const struct action* action;
};

struct suggestion {
	size_t distance;
	const struct action* action;
};

struct proposal {
	size_t count;
	struct suggestion* suggestions;
};

struct executable {
	struct executable* next;
	struct resolution* maybe;
};

struct state {
	const struct action* actions;
	const struct property* properties;
	const char* const* arguments;
	size_t automatic_completion_length;
	size_t suggestion_count;
	size_t maximum_suggestion_distance;
	struct executable* first_executable;
	struct executable* last_executable;

	bool verbose_printing;
	FILE* log_stream;
	FILE* target_stream;
};

/**
Destroys a state.

@param state The state.
**/
int
destroy_state(struct state* state);

/**
Creates a new state.

Leaks memory if
 the return value isn't <code>NULL</code> and
 isn't given to <code>destroy_state()</code>.

Fails if <code>malloc()</code> fails.

@return The new state if successful or
 <code>NULL</code> otherwise.
**/
struct state*
create_state(void);

/**
Removes the first executable from the schedule of the given state.

Frees memory.

Fails if
 the state is <code>NULL</code>,
 the schedule is empty.

@return The executable if successful or
 <code>NULL</code> otherwise.
**/
const struct invocation*
release(struct state* const state)
		__attribute__ ((nonnull));

/**
Adds the given executable to the end of the schedule of the given state.

Allocates memory.

Fails if
 the state is <code>NULL</code>,
 the executable is <code>NULL</code> or
 <code>malloc()</code> fails.

@return The number <code>0</code> if successful or
 <code>-1</code> otherwise.
**/
int
hold(struct state* state, const struct invocation* command)
		__attribute__ ((nonnull));

#endif
