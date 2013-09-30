/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#ifndef INDEFINIX_H
#define INDEFINIX_H

#include <stddef.h> // size_t

#include "gnu.h" // __attribute__ (())

int indefinix_invoke
(char const* const* arguments, size_t count)
__attribute__ ((__nonnull__));

#endif
