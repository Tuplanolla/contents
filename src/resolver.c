/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#include "resolver.h"

#include <stddef.h> // size_t

#include "array.h" // struct array

void const* resolve
(struct array* const array, char const* (* accessor)(void const*), char const* const argument, size_t const limit) {
	return NULL; // TODO this
}
