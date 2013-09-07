/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#include "data.h"

const struct action commands[COMMAND_COUNT] = {
	{
		.name = "configure",
		.abbreviation = "c",
		.arity = 0,
		.command = COMMAND_CONFIGURE
	},
	{
		.name = "set",
		.abbreviation = "s",
		.arity = 0,
		.command = COMMAND_SET
	},
	{
		.name = "pop",
		.abbreviation = "p",
		.arity = 0,
		.command = COMMAND_POP
	},
	{
		.name = "get",
		.abbreviation = "g",
		.arity = 0,
		.command = COMMAND_GET
	},
	{
		.name = "obliterate",
		.abbreviation = "o",
		.arity = 0,
		.command = COMMAND_OBLITERATE
	},
	{
		.name = "make",
		.abbreviation = "m",
		.arity = 0,
		.command = COMMAND_MAKE
	},
	{
		.name = "edit",
		.abbreviation = "e",
		.arity = 0,
		.command = COMMAND_EDIT
	},
	{
		.name = "add",
		.abbreviation = "a",
		.arity = 0,
		.command = COMMAND_ADD
	},
	{
		.name = "remove",
		.abbreviation = "r",
		.arity = 0,
		.command = COMMAND_REMOVE
	},
	{
		.name = "update",
		.abbreviation = "u",
		.arity = 0,
		.command = COMMAND_UPDATE
	},
	{
		.name = "lookup",
		.abbreviation = "l",
		.arity = 0,
		.command = COMMAND_LOOKUP
	},
	{
		.name = "find",
		.abbreviation = "f",
		.arity = 0,
		.command = COMMAND_FIND
	},
	{
		.name = "touch",
		.abbreviation = "t",
		.arity = 0,
		.command = COMMAND_TOUCH
	},
	{
		.name = "destroy",
		.abbreviation = "d",
		.arity = 0,
		.command = COMMAND_DESTROY
	},
	{
		.name = "help",
		.abbreviation = "h",
		.arity = 0,
		.command = COMMAND_HELP
	},
	{
		.name = "version",
		.abbreviation = "v",
		.arity = 0,
		.command = COMMAND_VERSION
	},
	{
		.name = "bind",
		.abbreviation = "b",
		.arity = 0,
		.command = COMMAND_BIND
	}
};
