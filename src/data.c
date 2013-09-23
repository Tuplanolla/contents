/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#include "data.h"

#include "syntax.h" // of ()
#include "array.h" // array_destroy(), array_create(), array_add_last()

static struct action actions[] = {{
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
		.name = "bind",
		.arity = ARITY_VARIADIC,
		.command = COMMAND_BIND
		}, {
		.name = "infer",
		.arity = ARITY_MONADIC,
		.command = COMMAND_INFER
		}};

int actions_create
(struct array** of (struct action*) result) {
	struct array* of (struct action*) array;
	if (array_create(&array, KEY_COUNT, sizeof (struct action*)) == -1) {
		return -1;
	}
	for (size_t action = 0; action < COMMAND_COUNT; ++action) {
		if (array_add_last(array, &actions[action]) == -1) {
			if (array_destroy(array) == -1)
				return -1;
			return -1;
		}
	}
	*result = array;
	return 0;
}

int actions_destroy
(struct array* const of (struct action*) array) {
	return array_destroy(array);
}

static struct property properties[] = {{
		.name = "location",
		.abbreviation = "l",
		.arity = ARITY_MONADIC,
		.key = KEY_LOCATION
		}, {
		.name = "editor",
		.abbreviation = "e",
		.arity = ARITY_MONADIC,
		.key = KEY_EDITOR
		}, {
		.name = "completion",
		.abbreviation = "c",
		.arity = ARITY_MONADIC,
		.key = KEY_COMPLETION
		}, {
		.name = "order",
		.abbreviation = "o",
		.arity = ARITY_TRIADIC,
		.key = KEY_ORDER
		}, {
		.name = "wrapping",
		.abbreviation = "w",
		.arity = ARITY_MONADIC,
		.key = KEY_WRAPPING
		}, {
		.name = "justification",
		.abbreviation = "j",
		.arity = ARITY_DYADIC,
		.key = KEY_JUSTIFICATION
		}, {
		.name = "filling",
		.abbreviation = "f",
		.arity = ARITY_DYADIC,
		.key = KEY_FILLING
		}, {
		.name = "interaction",
		.abbreviation = "i",
		.arity = ARITY_MONADIC,
		.key = KEY_INTERACTION
		}, {
		.name = "affix",
		.abbreviation = "a",
		.arity = ARITY_TRIADIC,
		.key = KEY_AFFIX
		}, {
		.name = "headaffix",
		.abbreviation = "ha",
		.arity = ARITY_TRIADIC,
		.key = KEY_HEADAFFIX
		}, {
		.name = "tailaffix",
		.abbreviation = "ta",
		.arity = ARITY_TRIADIC,
		.key = KEY_TAILAFFIX
		}, {
		.name = "unusual",
		.abbreviation = "u",
		.arity = ARITY_DYADIC,
		.key = KEY_UNUSUAL
		}, {
		.name = "preset",
		.abbreviation = "p",
		.arity = ARITY_MONADIC,
		.key = KEY_PRESET
		}};

int properties_create
(struct array** of (struct property*) result) {
	struct array* of (struct property*) array;
	if (array_create(&array, KEY_COUNT, sizeof (struct property*)) == -1) {
		return -1;
	}
	for (size_t property = 0; property < KEY_COUNT; ++property) {
		if (array_add_last(array, &properties[property]) == -1) {
			if (array_destroy(array) == -1)
				return -1;
			return -1;
		}
	}
	*result = array;
	return 0;
}

int properties_destroy
(struct array* const of (struct property*) array) {
	return array_destroy(array);
}
