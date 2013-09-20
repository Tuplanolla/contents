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

/**
The results of argument resolution.
**/
enum resolution {
	RESOLUTION_ERROR,
	RESOLUTION_END,
	RESOLUTION_COMMAND,
	RESOLUTION_FLAG,

	RESOLUTION_COUNT
};

/**
The amounts of arguments actions and properties consume.
**/
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

/**
The commands.
**/
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

/**
The configuration keys.
**/
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

/**
An action, which is essentially a name bound to a command and its parameters.
**/
struct action {
	const char* name;
	enum arity arity;
	enum command command;
};

/**
The result of resolving an argument, which can be an error.
**/
struct maybe {
	enum resolution type;
	const struct action* instance;
};

/**
A property, which is essentially a name bound to a key and its values.
**/
struct property {
	const char* name;
	const char* abbreviation;
	enum arity arity;
	enum key key;
};

/**
The result of correcting an argument, if resolution failed.
**/
struct suggestion {
	size_t distance;
	const struct action* instance;
};

/**
A list of argument corrections.
**/
struct proposal {
	size_t count;
	struct suggestion* guesses;
};

/**
An executable.
**/
struct executable {
	struct executable* next;
	struct maybe* maybe;
};

/**
A mutable state.
**/
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

Frees memory.

@param state The state.
**/
void destroy_state(struct state* state);

/**
Creates a new state.

Allocates memory.

Fails if <code>malloc()</code> fails.

@return The new state if successful or
 <code>NULL</code> otherwise.
**/
struct state* create_state(void);

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
int hold(struct state* const state, struct maybe* const maybe)
		__attribute__ ((nonnull));

/**
Removes the first executable from the schedule of the given state.

Frees memory.

Fails if
 the state is <code>NULL</code>,
 the schedule is empty.

@return The executable if successful or
 <code>NULL</code> otherwise.
**/
const struct maybe* release(struct state* const state)
		__attribute__ ((nonnull));

#endif
