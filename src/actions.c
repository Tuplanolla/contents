/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#include "actions.h"

#include "action.h" // struct action
#include "arity.h" // enum arity
#include "array.h" // struct array, array_add_last(), array_create(), array_destroy()
#include "executor.h" // execute_help(), execute_nothing(), execute_version()
#include "syntax.h" // of ()

static struct action const actions[] = {{
		.name = "configure",
		.arity = ARITY_NILADIC,
		.instance = &execute_nothing
		}, {
		.name = "set",
		.arity = ARITY_VARIADIC,
		.instance = &execute_nothing
		}, {
		.name = "pop",
		.arity = ARITY_MONADIC,
		.instance = &execute_nothing
		}, {
		.name = "get",
		.arity = ARITY_MONADIC,
		.instance = &execute_nothing
		}, {
		.name = "obliterate",
		.arity = ARITY_NILADIC,
		.instance = &execute_nothing
		}, {
		.name = "make",
		.arity = ARITY_MONADIC,
		.instance = &execute_nothing
		}, {
		.name = "edit",
		.arity = ARITY_NILADIC,
		.instance = &execute_nothing
		}, {
		.name = "add",
		.arity = ARITY_DYADIC,
		.instance = &execute_nothing
		}, {
		.name = "remove",
		.arity = ARITY_MONADIC,
		.instance = &execute_nothing
		}, {
		.name = "update",
		.arity = ARITY_DYADIC,
		.instance = &execute_nothing
		}, {
		.name = "lookup",
		.arity = ARITY_MONADIC,
		.instance = &execute_nothing
		}, {
		.name = "find",
		.arity = ARITY_MONADIC,
		.instance = &execute_nothing
		}, {
		.name = "touch",
		.arity = ARITY_NILADIC,
		.instance = &execute_nothing
		}, {
		.name = "destroy",
		.arity = ARITY_NILADIC,
		.instance = &execute_nothing
		}, {
		.name = "help",
		.arity = ARITY_NILADIC,
		.instance = &execute_help
		}, {
		.name = "version",
		.arity = ARITY_NILADIC,
		.instance = &execute_version
		}, {
		.name = "infer",
		.arity = ARITY_MONADIC,
		.instance = &execute_nothing
		}, {
		.name = "bind",
		.arity = ARITY_VARIADIC,
		.instance = &execute_nothing
		}};

int actions_create
(struct array** of (struct action) const result) {
	struct array* of (struct action) array;
	size_t const count = sizeof actions / sizeof *actions;
	if (array_create(&array, count, sizeof (struct action)) == -1)
		return -1;
	for (size_t position = 0; position < count; ++position)
		if (array_add_last(array, &actions[position]) == -1) {
			array_destroy(array);
			return -1;
		}
	*result = array;
	return 0;
}

void actions_destroy
(struct array* of (struct action) const array) {
	array_destroy(array);
}
