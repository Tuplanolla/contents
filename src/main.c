/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#include "gnu.h" // __attribute__ (())
#include "data.h" // struct action, actions_create(), properties_create()
#include "array.h" // struct array
#include "syntax.h" // of ()
#include "state.h" // struct state, state_create(), state_parse(), state_destroy(), state_execute()

int main
(const int count __attribute__ ((unused)), const char* const* const arguments) {
	int status = 0;
	const struct array* of (const struct action*) actions;
	if (actions_create(&actions) == -1) {
		status = 1;
		goto actions;
	}
	const struct array* of (const struct property*) properties;
	if (properties_create(&properties) == -1) {
		status = 1;
		goto properties;
	}
	struct state* state;
	if (state_create(&state, actions, properties) == -1) {
		status = 1;
		goto state;
	}
	if (state_parse(state, arguments + 1) == -1) { // TODO wrap arguments
		status = 1;
		goto all;
	}
	if (state_execute(state) == -1) {
		status = 1;
		goto all;
	}
all:
	if (state_destroy(state) == -1)
		status = 1;
state:
	if (properties_destroy(properties) == -1)
		status = 1;
properties:
	if (actions_destroy(actions) == -1)
		status = 1;
actions:
	return status;
}
