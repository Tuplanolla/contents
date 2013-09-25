/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#include "indefinix.h"

#include "syntax.h" // of ()
#include "array.h" // struct array
#include "action.h" // struct action
#include "property.h" // struct property
#include "actions.h" // actions_create(), actions_destroy()
#include "properties.h" // properties_create(), properties_destroy()
#include "state.h" // struct state, state_create(), state_parse(), state_execute(), state_destroy()

int indefinix_run
(struct array* of (char*) const arguments) {
	int status = 0;
	struct array* of (struct action*) actions;
	if (actions_create(&actions) == -1) {
		status = -1;
		goto nothing;
	}
	struct array* of (struct property*) properties;
	if (properties_create(&properties) == -1) {
		status = -1;
		goto actions;
	}
	struct state* state;
	if (state_create(&state, actions, properties) == -1) {
		status = -1;
		goto properties;
	}
	if (state_parse(state, arguments) == -1) {
		status = -1;
		goto state;
	}
	if (state_execute(state) == -1) {
		status = -1;
		goto state;
	}
state:
	if (state_destroy(state) == -1)
		status = -1;
properties:
	if (properties_destroy(properties) == -1)
		status = -1;
actions:
	if (actions_destroy(actions) == -1)
		status = -1;
nothing:
	return status;
}
