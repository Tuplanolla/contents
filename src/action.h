/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#ifndef ACTION_H
#define ACTION_H

#include "arity.h" // enum arity

#include "gnu.h" // __attribute__ (())

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
	COMMAND_INFER,

	COMMAND_COUNT
};

typedef int (* procedure)();

struct action {
	const char* name;
	enum arity arity;
	procedure instance;
	enum command command; // ?
};

const char* action_name
(const struct action* action)
__attribute__ ((nonnull));

enum arity action_arity
(const struct action* action)
__attribute__ ((nonnull));

procedure action_instance
(const struct action* action)
__attribute__ ((nonnull));

int action_create
(struct action** result, const char* name, enum arity arity, procedure procedure)
__attribute__ ((nonnull));

int action_destroy
(struct action* action)
__attribute__ ((nonnull));

#endif
