/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#include "indefinix.h"

#include "syntax.h" // of ()
#include "array.h" // struct array
#include "action.h" // struct action
#include "property.h" // struct property
#include "data.h" // actions_create(), properties_create(), properties_destroy(), actions_destroy()
#include "state.h" // struct state, state_create(), state_parse(), state_execute(), state_destroy()

int indefinix_run
(struct array* of (char*) arguments) {
	int status = 0;
	struct array* of (struct action*) actions;
	if (actions_create(&actions) == -1) {
		status = -1;
		goto actions;
	}
	struct array* of (struct property*) properties;
	if (properties_create(&properties) == -1) {
		status = -1;
		goto properties;
	}
	struct state* state;
	if (state_create(&state, actions, properties) == -1) {
		status = -1;
		goto state;
	}
	if (state_parse(state, arguments) == -1) {
		status = -1;
		goto all;
	}
	if (state_execute(state) == -1) {
		status = -1;
		goto all;
	}
all:
	if (state_destroy(state) == -1)
		status = -1;
state:
	if (properties_destroy(properties) == -1)
		status = -1;
properties:
	if (actions_destroy(actions) == -1)
		status = -1;
actions:
	return status;
}
