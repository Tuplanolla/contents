/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#ifndef TRUNCATION_H
#define TRUNCATION_H

#include <stddef.h> // size_t

#include "gnu.h" // __attribute__ (())

int truncation_create
(char** result, char const* string, size_t limit)
__attribute__ ((nonnull));

int truncation_destroy
(char* string)
__attribute__ ((nonnull));

#endif
