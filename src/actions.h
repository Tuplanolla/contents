/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#ifndef ACTIONS_H
#define ACTIONS_H

#include "action.h" // struct action
#include "array.h" // struct array
#include "gnu.h" // __attribute__ (())
#include "syntax.h" // of ()

int actions_create
(struct array** of (struct action) result)
__attribute__ ((nonnull));

void actions_destroy
(struct array* of (struct action) actions)
__attribute__ ((nonnull));

#endif
