/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#ifndef ACTION_H
#define ACTION_H

#include "arity.h" // enum arity
#include "gnu.h" // __attribute__ (())
#include "state.h" // struct state

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

typedef int (* procedure)(struct state* state, ...);

struct action {
	char const* name;
	enum arity arity;
	procedure instance;
	enum command command; // ?
};

char const* action_name
(struct action const* action)
__attribute__ ((nonnull));

enum arity action_arity
(struct action const* action)
__attribute__ ((nonnull));

procedure action_instance
(struct action const* action)
__attribute__ ((nonnull));

int action_create
(struct action** result, char const* name, enum arity arity, procedure procedure)
__attribute__ ((nonnull));

int action_destroy
(struct action* action)
__attribute__ ((nonnull));

#endif
