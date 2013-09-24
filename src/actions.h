/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#ifndef ACTIONS_H
#define ACTIONS_H

#include "gnu.h" // __attribute__ (())
#include "syntax.h" // of ()
#include "array.h" // struct array
#include "action.h" // struct action

int actions_create
(struct array** of (struct action*) result)
__attribute__ ((nonnull));

int actions_destroy
(struct array* of (struct action*) actions)
__attribute__ ((nonnull));

#endif
