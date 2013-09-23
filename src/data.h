/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#ifndef DATA_H
#define DATA_H

#include "gnu.h" // __attribute__ (())
#include "syntax.h" // of ()
#include "array.h" // struct array
#include "action.h" // struct action
#include "property.h" // struct property

int actions_create
(struct array** of (struct action*) result)
__attribute__ ((nonnull));

int actions_destroy
(struct array* of (struct action*) actions)
__attribute__ ((nonnull));

int properties_create
(struct array** of (struct property*) result)
__attribute__ ((nonnull));

int properties_destroy
(struct array* of (struct property*) properties)
__attribute__ ((nonnull));

#endif
