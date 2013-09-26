/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#ifndef SUGGESTIONS_H
#define SUGGESTIONS_H

#include <stddef.h> // size_t

#include "array.h" // struct array
#include "gnu.h" // __attribute__ (())
#include "suggestion.h" // struct suggestion
#include "syntax.h" // of ()

int suggestions_create
(struct array** of (struct suggestion) result, struct array* array, char const* (* accessor)(void const*), char const* argument, size_t completion, size_t count, size_t distance)
__attribute__ ((nonnull));

int suggestions_destroy
(struct array* of (struct suggestion) suggestions)
__attribute__ ((nonnull));

#endif
