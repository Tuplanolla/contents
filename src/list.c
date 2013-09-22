/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#include "list.h"

#include <stddef.h> // size_t, NULL
#include <stdlib.h> // free(), malloc(), realloc()
#include <string.h> // memcpy(), memmove()

static size_t
maximum
(const size_t x, const size_t y) {
	if (x > y)
		return x;
	return y;
}

typedef unsigned char byte;

#define SIZE_MAX ((size_t )-1)

int
list_destroy
(struct list* const list) {
	free(list->elements);
	free(list);
	return 0;
}

int
list_create
(struct list** const result, const size_t capacity, const size_t unit) {
	if (unit < 1)
		return -1;
	struct list* const list = malloc(sizeof *list);
	if (list == NULL)
		return -1;
	const size_t actual_capacity = maximum(1, capacity);
	void* const elements = malloc(actual_capacity * unit);
	if (elements == NULL) {
		free(list);
		return -1;
	}
	list->capacity = actual_capacity;
	list->unit = unit;
	list->count = 0;
	list->elements = elements;
	*result = list;
	return 0;
}

int
list_remove
(void* const result, struct list* const list, const size_t position) {
	const size_t count = list->count;
	if (count < 1 || position >= count)
		return -1;
	const size_t capacity = list->capacity;
	const size_t unit = list->unit;
	if (count <= capacity / 4) {
		const size_t lower_capacity = maximum(1, capacity / 2);
		void* const elements = realloc(list->elements, lower_capacity * unit);
		if (elements != NULL) {
			list->capacity = lower_capacity;
			list->elements = elements;
		}
	}
	--list->count;
	byte* const split = (byte* )list->elements + position * unit;
	if (result != NULL)
		memcpy(result, split, unit);
	const size_t displaced = count - position;
	if (displaced > 0)
		memmove(split, split + unit, displaced * unit);
	return 0;
}

int
list_add
(struct list* const list, void* const element, const size_t position) {
	const size_t count = list->count;
	if (position > count)
		return -1;
	const size_t capacity = list->capacity;
	const size_t unit = list->unit;
	if (count >= capacity) {
		if (capacity > SIZE_MAX / 2)
			return -1;
		const size_t higher_capacity = maximum(1, 2 * capacity);
		void* const elements = realloc(list->elements, higher_capacity * unit);
		if (elements == NULL)
			return -1;
		list->capacity = higher_capacity;
		list->elements = elements;
	}
	byte* const split = (byte* )list->elements + position * unit;
	const size_t displaced = count - position;
	if (displaced > 0)
		memmove(split + unit, split, displaced * unit);
	memcpy(split, element, unit);
	++list->count;
	return 0;
}

int
list_write
(struct list* const list, void* const element, const size_t position) {
	const size_t count = list->count;
	if (count < 1 || position >= count)
		return -1;
	const size_t unit = list->unit;
	byte* const split = (byte* )list->elements + position * unit;
	memcpy(split, element, unit);
	return 0;
}

int
list_read
(void* const result, struct list* const list, const size_t position) {
	const size_t count = list->count;
	if (count < 1 || position >= count)
		return -1;
	const size_t unit = list->unit;
	byte* const split = (byte* )list->elements + position * unit;
	if (result != NULL)
		memcpy(result, split, unit);
	return 0;
}

size_t
list_capacity
(struct list* list) {
	return list->capacity;
}

size_t
list_unit
(struct list* list) {
	return list->unit;
}

size_t
list_count
(struct list* list) {
	return list->count;
}
