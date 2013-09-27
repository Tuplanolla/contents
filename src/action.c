/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#include "action.h" // struct action

#include <stddef.h> // NULL
#include <stdlib.h> // free(), malloc()

#include "arity.h" // enum arity
#include "types.h" // procedure

char const* action_name
(struct action const* const action) {
	return action->name;
}

enum arity action_arity
(struct action const* const action) {
	return action->arity;
}

procedure action_instance
(struct action const* const action) {
	return action->instance;
}

int action_create
(struct action** const result, char const* const name, enum arity const arity, procedure instance) {
	struct action* const action = malloc(sizeof *action);
	if (action == NULL)
		return -1;
	action->name = name;
	action->arity = arity;
	action->instance = instance;
	*result = action;
	return 0;
}

void action_destroy
(struct action* const action) {
	free(action);
}
