/**
Prints things.

@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#ifndef PRINTER_H
#define PRINTER_H

#include <stdio.h> // FILE

#include "array.h" // struct array*, of ()
#include "extensions.h" // __*__, __attribute__ (())
// #include "suggestion.h" // struct suggestion

int print_usage
(FILE* stream);

int print_summary
(FILE* stream);

/*
int print_suggestions
(FILE* const stream, struct array* of (struct suggestion) suggestions, char const* (* accessor)(void const*))
__attribute__ ((nonnull (2, 3)));
*/

#endif
