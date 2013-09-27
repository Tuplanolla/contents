/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#ifndef EXECUTOR_H
#define EXECUTOR_H

#include "array.h" // struct array
#include "state.h" // struct state
#include "syntax.h" // of ()

int execute_nothing
(struct state* state, struct array* of (char const*) arguments);

int execute_help
(struct state* state, struct array* of (char const*) arguments);

int execute_version
(struct state* state, struct array* of (char const*) arguments);

int execute_infer
(struct state* state, struct array* of (char const*) arguments);

#endif
