/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#include "resolver.h"

#include <stddef.h> // size_t

#include "gnu.h" // __attribute__ (())
#include "array.h" // struct array

const void* resolve
(struct array* const array, const char* (* accessor)(const void*), const char* const argument, const size_t limit) {
	return NULL; // TODO this
}
