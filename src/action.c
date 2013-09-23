/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#include "action.h" // struct action, procedure
#include "arity.h" // enum arity

#include <stdlib.h> // malloc(), free()
#include <stddef.h> // NULL

const char* action_name
(const struct action* const action) {
	return action->name;
}

enum arity action_arity
(const struct action* const action) {
	return action->arity;
}

procedure action_instance
(const struct action* const action) {
	return action->instance;
}

int action_create
(struct action** const result, const char* const name, const enum arity arity, procedure instance) {
	struct action* const action = malloc(sizeof *action);
	if (action == NULL)
		return -1;
	action->name = name;
	action->arity = arity;
	action->instance = instance;
	*result = action;
	return 0;
}

int action_destroy
(struct action* const action) {
	free(action);
	return 0;
}
