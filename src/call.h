/**
Represents an invocation of a command.

@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#ifndef CALL_H
#define CALL_H

#include "command.h" // enum command
#include "extensions.h" // __*__, __attribute__ (())
#include "state.h" // struct state

struct call {
	enum command command;
	void* arguments;
};

int call_execute
(struct state* state, struct call const* call)
__attribute__ ((__nonnull__));

#endif
