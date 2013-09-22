/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#include "data.h"

#include "syntax.h" // of ()
#include "array.h" // array_destroy(), array_create(), array_add()

static const struct action actions[] = {{
		.name = "configure",
		.command = COMMAND_CONFIGURE,
		.arity = ARITY_NILADIC
		}, {
		.name = "set",
		.command = COMMAND_SET,
		.arity = ARITY_VARIADIC
		}, {
		.name = "pop",
		.command = COMMAND_POP,
		.arity = ARITY_MONADIC
		}, {
		.name = "get",
		.command = COMMAND_GET,
		.arity = ARITY_MONADIC
		}, {
		.name = "obliterate",
		.command = COMMAND_OBLITERATE,
		.arity = ARITY_NILADIC
		}, {
		.name = "make",
		.command = COMMAND_MAKE,
		.arity = ARITY_MONADIC
		}, {
		.name = "edit",
		.command = COMMAND_EDIT,
		.arity = ARITY_NILADIC
		}, {
		.name = "add",
		.command = COMMAND_ADD,
		.arity = ARITY_DYADIC
		}, {
		.name = "remove",
		.command = COMMAND_REMOVE,
		.arity = ARITY_MONADIC
		}, {
		.name = "update",
		.command = COMMAND_UPDATE,
		.arity = ARITY_DYADIC
		}, {
		.name = "lookup",
		.command = COMMAND_LOOKUP,
		.arity = ARITY_MONADIC
		}, {
		.name = "find",
		.command = COMMAND_FIND,
		.arity = ARITY_MONADIC
		}, {
		.name = "touch",
		.command = COMMAND_TOUCH,
		.arity = ARITY_NILADIC
		}, {
		.name = "destroy",
		.command = COMMAND_DESTROY,
		.arity = ARITY_NILADIC
		}, {
		.name = "help",
		.command = COMMAND_HELP,
		.arity = ARITY_NILADIC
		}, {
		.name = "version",
		.command = COMMAND_VERSION,
		.arity = ARITY_NILADIC
		}, {
		.name = "bind",
		.command = COMMAND_BIND,
		.arity = ARITY_VARIADIC
		}, {
		.name = "suggest",
		.command = COMMAND_SUGGEST,
		.arity = ARITY_MONADIC
		}};

int actions_destroy
(struct array* of (const struct action*) actions) {
	return array_destroy(actions);
}

int actions_create
(struct array** of (const struct action*) result) {
	int status = 0;
	struct array* of (const struct action*) array;
	if (array_create(&array) == -1) {
		status = -1;
		goto array;
	}
	for (size_t action = 0; action < COMMAND_COUNT; ++action) {
		if (array_add_last(array, &actions[action]) == -1) {
			status = -1;
			goto all;
		}
	}
	*result = array;
all:
	if (array_destroy(array))
		status = -1;
array:
	return status;
}

static const struct property properties[] = {{
		.name = "location",
		.abbreviation = "l",
		.key = KEY_LOCATION,
		.arity = ARITY_MONADIC
		}, {
		.name = "editor",
		.abbreviation = "e",
		.key = KEY_EDITOR,
		.arity = ARITY_MONADIC
		}, {
		.name = "completion",
		.abbreviation = "c",
		.key = KEY_COMPLETION,
		.arity = ARITY_MONADIC
		}, {
		.name = "order",
		.abbreviation = "o",
		.key = KEY_ORDER,
		.arity = ARITY_TRIADIC
		}, {
		.name = "wrapping",
		.abbreviation = "w",
		.key = KEY_WRAPPING,
		.arity = ARITY_MONADIC
		}, {
		.name = "justification",
		.abbreviation = "j",
		.key = KEY_JUSTIFICATION,
		.arity = ARITY_DYADIC
		}, {
		.name = "filling",
		.abbreviation = "f",
		.key = KEY_FILLING,
		.arity = ARITY_DYADIC
		}, {
		.name = "interaction",
		.abbreviation = "i",
		.key = KEY_INTERACTION,
		.arity = ARITY_MONADIC
		}, {
		.name = "affix",
		.abbreviation = "a",
		.key = KEY_AFFIX,
		.arity = ARITY_TRIADIC
		}, {
		.name = "headaffix",
		.abbreviation = "ha",
		.key = KEY_HEADAFFIX,
		.arity = ARITY_TRIADIC
		}, {
		.name = "tailaffix",
		.abbreviation = "ta",
		.key = KEY_TAILAFFIX,
		.arity = ARITY_TRIADIC
		}, {
		.name = "unusual",
		.abbreviation = "u",
		.key = KEY_UNUSUAL,
		.arity = ARITY_DYADIC
		}, {
		.name = "preset",
		.abbreviation = "p",
		.key = KEY_PRESET,
		.arity = ARITY_MONADIC
		}};

int properties_destroy
(struct array* of (const struct property*) properties) {
	return array_destroy(properties);
}

int properties_create
(struct array** of (const struct property*) result) {
	int status = 0;
	struct array* of (const struct property*) array;
	if (array_create(&array) == -1) {
		status = -1;
		goto array;
	}
	for (size_t property = 0; property < KEY_COUNT; ++property) {
		if (array_add_last(array, &properties[property]) == -1) {
			status = -1;
			goto all;
		}
	}
	*result = array;
all:
	if (array_destroy(array))
		status = -1;
array:
	return status;
}
