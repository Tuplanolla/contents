/**
Provides an interface for the default commands.

@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#ifndef ACTIONS_H
#define ACTIONS_H

#include "action.h" // struct action
#include "arity.h" // enum arity
#include "array.h" // struct array*, of ()
#include "extensions.h" // __*__, __attribute__ (())

int actions_create
(struct array_const** of (struct action const) result)
__attribute__ ((__nonnull__));

void actions_destroy
(struct array_const* of (struct action const) actions)
__attribute__ ((__nonnull__));

#endif
