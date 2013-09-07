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

enum command {
	COMMAND_HELP,
	COMMAND_VERSION,
	COMMAND_CONFIGURE,

	COMMAND_COUNT
};

enum flag {
	FLAG_HELP,
	FLAG_VERSION,

	FLAG_COUNT
};

struct action {
	const char* name;
	const char* abbreviation;
	size_t arity;
	union {
		enum command c;
		enum flag f;
	} thing;
};

struct container {
	enum type type;
	struct action instance;
};

/**
The commands.
**/
extern const struct action commands[COMMAND_COUNT];

#endif
