/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#include "action.h" // enum command, struct action

#include <stddef.h> // NULL
#include <stdlib.h> // free(), malloc()

#include "arity.h" // enum arity

char const* action_name
(struct action const* const action) {
	return action->name;
}

enum arity action_arity
(struct action const* const action) {
	return action->arity;
}

enum command action_command
(struct action const* const action) {
	return action->command;
}
