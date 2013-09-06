/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#include "data.h"

const struct action commands[COMMAND_COUNT] = {
	{
		.name = "help",
		.abbreviation = "h",
		.arity = 0,
		.thing = {
			.c = COMMAND_HELP
		}
	},
	{
		.name = "version",
		.abbreviation = "v",
		.arity = 0,
		.thing = {
			.c = COMMAND_VERSION
		}
	}
};
