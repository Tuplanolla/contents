/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#ifndef SUGGESTION_H
#define SUGGESTION_H

#include <stddef.h> // size_t

#include "gnu.h" // __attribute__ (())

struct suggestion {
	size_t edit_distance;
	void* instance;
};

size_t suggestion_edit_distance
(struct suggestion const* suggestion)
__attribute__ ((nonnull));

void* suggestion_instance
(struct suggestion const* suggestion)
__attribute__ ((nonnull));

int suggestion_create
(struct suggestion** result, size_t edit_distance, void* instance)
__attribute__ ((nonnull));

void suggestion_destroy
(struct suggestion* suggestion)
__attribute__ ((nonnull));

#endif
