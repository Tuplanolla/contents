/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#ifndef LIST_H
#define LIST_H

#include <stddef.h> // size_t

#include "gnu.h" // __attribute__

struct list_item {
	struct list* next;
	struct list* previous;
	void* element;
};

struct list {
	size_t capacity;
	size_t unit;
	size_t count;
	struct list_item* first;
	struct list_item* last;
};

int
list_destroy
(struct list* list)
__attribute__ ((nonnull));

int
list_create
(struct list** result, size_t capacity, size_t unit)
__attribute__ ((nonnull));

int
list_remove
(void* result, struct list* list, size_t position)
__attribute__ ((nonnull (2)));

int
list_add
(struct list* list, void* element, size_t position)
__attribute__ ((nonnull));

int
list_write
(struct list* list, void* element, size_t position)
__attribute__ ((nonnull));

int
list_read
(void* result, struct list* list, size_t position)
__attribute__ ((nonnull (2)));

size_t
list_capacity
(struct list* list)
__attribute__ ((nonnull));

size_t
list_unit
(struct list* list)
__attribute__ ((nonnull));

size_t
list_count
(struct list* list)
__attribute__ ((nonnull));

#endif
