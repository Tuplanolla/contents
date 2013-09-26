/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#ifndef HELPER_H
#define HELPER_H

#include <stdio.h> // FILE

#include "array.h" // struct array
#include "gnu.h" // __attribute__ (())
#include "suggestion.h" // struct suggestion
#include "syntax.h" // of ()

int helper_usage
(FILE* stream);

int helper_summary
(FILE* stream);

int helper_suggestions
(FILE* const stream, struct array* of (struct suggestion) suggestions, char const* (* accessor)(void const*))
__attribute__ ((nonnull (2, 3)));

#endif
