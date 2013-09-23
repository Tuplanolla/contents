/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#ifndef ARRAY_H
#define ARRAY_H

#include <stddef.h> // size_t

#include "gnu.h" // __attribute__ (())

struct array {
	size_t capacity;
	size_t unit;
	size_t count;
	void* elements;
};

size_t array_capacity
(struct array* array)
__attribute__ ((nonnull));

size_t array_unit
(struct array* array)
__attribute__ ((nonnull));

size_t array_count
(struct array* array)
__attribute__ ((nonnull));

int array_create
(struct array** result, size_t capacity, size_t unit)
__attribute__ ((nonnull));

int array_destroy
(struct array* array)
__attribute__ ((nonnull));

int array_add
(struct array* array, void* element, size_t position)
__attribute__ ((nonnull));

int array_remove
(void* result, struct array* array, size_t position)
__attribute__ ((nonnull (2)));

int array_add_last
(struct array* array, void* element)
__attribute__ ((nonnull));

int array_remove_last
(void* result, struct array* array)
__attribute__ ((nonnull (2)));

int array_read
(void* result, struct array* array, size_t position)
__attribute__ ((nonnull (2)));

int array_write
(struct array* array, void* element, size_t position)
__attribute__ ((nonnull));

#endif
