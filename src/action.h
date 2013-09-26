/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#ifndef ACTION_H
#define ACTION_H

#include "arity.h" // enum arity
#include "gnu.h" // __attribute__ (())
#include "state.h" // procedure

struct action {
	char const* name;
	enum arity arity;
	procedure instance;
};

char const* action_name
(struct action const* action)
__attribute__ ((nonnull));

enum arity action_arity
(struct action const* action)
__attribute__ ((nonnull));

procedure action_instance
(struct action const* action)
__attribute__ ((nonnull));

int action_create
(struct action** result, char const* name, enum arity arity, procedure procedure)
__attribute__ ((nonnull));

int action_destroy
(struct action* action)
__attribute__ ((nonnull));

#endif
