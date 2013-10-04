/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#include "actions.h"

#include "action.h" // enum command, struct action
#include "arity.h" // enum arity
#include "array.h" // struct array*, of ()

struct action const actions[COMMAND_COUNT] = {{
			.name = "configure",
			.arity = ARITY_NILADIC,
			.command = COMMAND_CONFIGURE
		}, {
			.name = "set",
			.arity = ARITY_VARIADIC,
			.command = COMMAND_SET
		}, {
			.name = "pop",
			.arity = ARITY_MONADIC,
			.command = COMMAND_POP
		}, {
			.name = "get",
			.arity = ARITY_MONADIC,
			.command = COMMAND_GET
		}, {
			.name = "obliterate",
			.arity = ARITY_NILADIC,
			.command = COMMAND_OBLITERATE
		}, {
			.name = "make",
			.arity = ARITY_MONADIC,
			.command = COMMAND_MAKE
		}, {
			.name = "edit",
			.arity = ARITY_NILADIC,
			.command = COMMAND_EDIT
		}, {
			.name = "add",
			.arity = ARITY_DYADIC,
			.command = COMMAND_ADD
		}, {
			.name = "remove",
			.arity = ARITY_MONADIC,
			.command = COMMAND_REMOVE
		}, {
			.name = "update",
			.arity = ARITY_DYADIC,
			.command = COMMAND_UPDATE
		}, {
			.name = "lookup",
			.arity = ARITY_MONADIC,
			.command = COMMAND_LOOKUP
		}, {
			.name = "find",
			.arity = ARITY_MONADIC,
			.command = COMMAND_FIND
		}, {
			.name = "touch",
			.arity = ARITY_NILADIC,
			.command = COMMAND_TOUCH
		}, {
			.name = "destroy",
			.arity = ARITY_NILADIC,
			.command = COMMAND_DESTROY
		}, {
			.name = "help",
			.arity = ARITY_NILADIC,
			.command = COMMAND_HELP
		}, {
			.name = "version",
			.arity = ARITY_NILADIC,
			.command = COMMAND_VERSION
		}, {
			.name = "infer",
			.arity = ARITY_MONADIC,
			.command = COMMAND_INFER
		}, {
			.name = "bind",
			.arity = ARITY_VARIADIC,
			.command = COMMAND_BIND
		}};

int actions_create
(struct array_const** of (struct action const) const result) {
	struct array_const* of (struct action const) array;
	if (array_const_create(&array, COMMAND_COUNT, sizeof (struct action)) == -1)
		return -1;
	if (array_const_add_all_last(array, &actions[0], COMMAND_COUNT) == -1) {
		array_const_destroy(array);
		return -1;
	}
	*result = array;
	return 0;
}

void actions_destroy
(struct array_const* of (struct action const) const array) {
	array_const_destroy(array);
}
