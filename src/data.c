/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#include "data.h"

const struct action commands[COMMAND_COUNT] = {
	{
		.name = "configure",
		.arity = ARITY_NILADIC,
		.command = COMMAND_CONFIGURE
	},
	{
		.name = "set",
		.arity = ARITY_VARIADIC,
		.command = COMMAND_SET
	},
	{
		.name = "pop",
		.arity = ARITY_MONADIC,
		.command = COMMAND_POP
	},
	{
		.name = "get",
		.arity = ARITY_MONADIC,
		.command = COMMAND_GET
	},
	{
		.name = "obliterate",
		.arity = ARITY_NILADIC,
		.command = COMMAND_OBLITERATE
	},
	{
		.name = "make",
		.arity = ARITY_MONADIC,
		.command = COMMAND_MAKE
	},
	{
		.name = "edit",
		.arity = ARITY_NILADIC,
		.command = COMMAND_EDIT
	},
	{
		.name = "add",
		.arity = ARITY_DYADIC,
		.command = COMMAND_ADD
	},
	{
		.name = "remove",
		.arity = ARITY_MONADIC,
		.command = COMMAND_REMOVE
	},
	{
		.name = "update",
		.arity = ARITY_DYADIC,
		.command = COMMAND_UPDATE
	},
	{
		.name = "lookup",
		.arity = ARITY_MONADIC,
		.command = COMMAND_LOOKUP
	},
	{
		.name = "find",
		.arity = ARITY_MONADIC,
		.command = COMMAND_FIND
	},
	{
		.name = "touch",
		.arity = ARITY_NILADIC,
		.command = COMMAND_TOUCH
	},
	{
		.name = "destroy",
		.arity = ARITY_NILADIC,
		.command = COMMAND_DESTROY
	},
	{
		.name = "help",
		.arity = ARITY_NILADIC,
		.command = COMMAND_HELP
	},
	{
		.name = "version",
		.arity = ARITY_NILADIC,
		.command = COMMAND_VERSION
	},
	{
		.name = "bind",
		.arity = ARITY_VARIADIC,
		.command = COMMAND_BIND
	}
};

const struct property properties[] = {
	{
		.name = "location",
		.abbreviation = "l",
		.arity = ARITY_MONADIC,
		.key = KEY_LOCATION
	},
	{
		.name = "editor",
		.abbreviation = "e",
		.arity = ARITY_MONADIC,
		.key = KEY_EDITOR
	},
	{
		.name = "completion",
		.abbreviation = "c",
		.arity = ARITY_MONADIC,
		.key = KEY_COMPLETION
	},
	{
		.name = "order",
		.abbreviation = "o",
		.arity = ARITY_TRIADIC,
		.key = KEY_ORDER
	},
	{
		.name = "wrapping",
		.abbreviation = "w",
		.arity = ARITY_MONADIC,
		.key = KEY_WRAPPING
	},
	{
		.name = "justification",
		.abbreviation = "j",
		.arity = ARITY_DYADIC,
		.key = KEY_JUSTIFICATION
	},
	{
		.name = "filling",
		.abbreviation = "f",
		.arity = ARITY_DYADIC,
		.key = KEY_FILLING
	},
	{
		.name = "interaction",
		.abbreviation = "i",
		.arity = ARITY_MONADIC,
		.key = KEY_INTERACTION
	},
	{
		.name = "affix",
		.abbreviation = "a",
		.arity = ARITY_TRIADIC,
		.key = KEY_AFFIX
	},
	{
		.name = "headaffix",
		.abbreviation = "ha",
		.arity = ARITY_TRIADIC,
		.key = KEY_HEADAFFIX
	},
	{
		.name = "tailaffix",
		.abbreviation = "ta",
		.arity = ARITY_TRIADIC,
		.key = KEY_TAILAFFIX
	},
	{
		.name = "unusual",
		.abbreviation = "u",
		.arity = ARITY_DYADIC,
		.key = KEY_UNUSUAL
	},
};
