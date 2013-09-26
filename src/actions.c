/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#include "actions.h"

#include "action.h" // struct action
#include "arity.h" // enum arity
#include "array.h" // struct array, array_add_last(), array_create(), array_destroy()
#include "syntax.h" // of ()

static struct action const actions[] = {{
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
(struct array** of (struct action) const result) {
	struct array* of (struct action) array;
	if (array_create(&array, COMMAND_COUNT, sizeof (struct action)) == -1)
		return -1;
	for (size_t position = 0; position < COMMAND_COUNT; ++position)
		if (array_add_last(array, &actions[position]) == -1) {
			if (array_destroy(array) == -1)
				return -1;
			return -1;
		}
	*result = array;
	return 0;
}

int actions_destroy
(struct array* of (struct action) const array) {
	return array_destroy(array);
}
