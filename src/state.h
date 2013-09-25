/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#ifndef STATE_H
#define STATE_H

#include <stddef.h> // size_t
#include <stdio.h> // FILE

#include "gnu.h" // __attribute__ (())
#include "syntax.h" // of ()
#include "array.h" // struct array
#include "action.h" // procedure, struct action
#include "property.h" // variable, struct property

struct invocation { // TODO invocation.h
	procedure procedure;
	struct array* of (void*) arguments;
};

struct state {
	struct array* of (struct action*) actions;
	struct array* of (struct property*) properties;
	struct array* of (struct invocation*) invocations;
	size_t automatic_completion_limit;
	size_t suggestion_count;
	size_t suggestion_edit_distance;
	FILE* output_stream;
};

size_t state_automatic_completion_length
(struct state* state)
__attribute__ ((nonnull));

int state_create
(struct state** result, struct array* of (struct action*) actions, struct array* of (struct property*) properties)
__attribute__ ((nonnull));

int state_destroy
(struct state* state)
__attribute__ ((nonnull));

int state_parse
(struct state* state, struct array* of (char*) arguments)
__attribute__ ((nonnull));

int state_schedule
(struct state* state, struct invocation* invocation)
__attribute__ ((nonnull));

int state_execute
(struct state* state)
__attribute__ ((nonnull));

#endif
