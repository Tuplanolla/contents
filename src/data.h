/**
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
	TYPE_FLAG
};

enum command {
	COMMAND_HELP,
	COMMAND_VERSION,
	COMMAND_CONFIGURE
};

enum flag {
	FLAG_HELP,
	FLAG_VERSION
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
The amount of commands.
**/
#define COMMAND_COUNT 3

/**
The commands.
**/
extern const struct action commands[COMMAND_COUNT];

#endif
