/**
Provides common data structures to the user.

@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#ifndef DATA_H
#define DATA_H

#include <stddef.h> // size_t

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

	KEY_COUNT
};

struct action {
	const char* name;
	enum arity arity;
	enum command command;
};

struct container {
	enum type type;
	struct action instance;
};

struct property {
	const char* name;
	const char* abbreviation;
	enum arity arity;
	enum key key;
};

struct guess {
	size_t distance;
	const struct action* instance;
};

struct holder {
	size_t count;
	struct guess guesses[COMMAND_COUNT];
};

/**
The commands.
**/
extern const struct action commands[COMMAND_COUNT];

#endif
