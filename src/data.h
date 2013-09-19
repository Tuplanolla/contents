/**
Provides common data structures for the user.

@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#ifndef DATA_H
#define DATA_H

#include <stddef.h> // size_t

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
	struct action instance;
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
struct guess {
	size_t distance;
	const struct action* instance;
};

/**
A list of argument corrections.
**/
struct proposal {
	size_t count;
	struct guess guesses[COMMAND_COUNT];
};

/**
The actions.
**/
extern const struct action actions[COMMAND_COUNT];

/**
The properties.
**/
extern const struct property properties[KEY_COUNT];

#endif
