/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#ifndef STATE_H
#define STATE_H

#include <stddef.h> // size_t
#include <stdbool.h> // bool
#include <stdio.h> // FILE

#include "gnu.h" // __attribute__ (())
#include "syntax.h" // of ()
#include "array.h" // struct array

struct invocation {
	enum command action;
	struct array* of (const void*) arguments;
};

struct state {
	struct array* of (const struct action*) actions;
	struct array* of (const struct property*) properties;
	struct array* of (const struct invocation*) invocations;
	size_t automatic_completion_length;
	size_t suggestion_count;
	size_t maximum_suggestion_distance;
};

size_t state_automatic_completion_length
(struct state* state)
__attribute__ ((nonnull));

int state_destroy
(struct state* state)
__attribute__ ((nonnull));

int state_create
(struct state** result, struct array* of (const struct action*) actions, struct array* of (const struct property*) properties)
__attribute__ ((nonnull));

int state_parse
(struct state* state, const char* const* arguments)
__attribute__ ((nonnull));

int state_execute
(struct state* state)
__attribute__ ((nonnull));

#endif
