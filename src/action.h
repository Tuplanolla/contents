/**
Represents a named command.

@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#ifndef ACTION_H
#define ACTION_H

#include "arity.h" // enum arity
#include "array.h" // struct array*, of ()
#include "command.h" // enum command
#include "extensions.h" // __*__, __attribute__ (())

struct action {
	char const* name;
	enum arity arity;
	enum command command;
};

char const* action_name
(struct action const* action)
__attribute__ ((__nonnull__));

enum arity action_arity
(struct action const* action)
__attribute__ ((__nonnull__));

enum command action_command
(struct action const* action)
__attribute__ ((__nonnull__));

#endif
