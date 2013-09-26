/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#include "array.h" // struct array

#include <stddef.h> // NULL, size_t
#include <stdlib.h> // free(), malloc(), realloc(), qsort()
#include <string.h> // memcpy(), memmove()

#include "calculator.h" // maximum()

size_t array_capacity
(struct array const* const array) {
	return array->capacity;
}

size_t array_unit
(struct array const* const array) {
	return array->unit;
}

size_t array_count
(struct array const* const array) {
	return array->count;
}

int array_create
(struct array** const result, size_t const capacity, size_t const unit) {
	if (unit < 1)
		return -1;
	struct array* const array = malloc(sizeof *array);
	if (array == NULL)
		return -1;
	size_t const actual_capacity = maximum(1, capacity);
	void* const elements = malloc(actual_capacity * unit);
	if (elements == NULL) {
		free(array);
		return -1;
	}
	array->capacity = actual_capacity;
	array->unit = unit;
	array->count = 0;
	array->elements = elements;
	*result = array;
	return 0;
}

int array_create_copy
(struct array** const result, struct array const* const array) {
	struct array* const copy = malloc(sizeof *array);
	if (copy == NULL)
		return -1;
	size_t const capacity = array->capacity;
	size_t const unit = array->unit;
	void* const elements = malloc(capacity * unit);
	if (elements == NULL) {
		free(copy);
		return -1;
	}
	size_t const count = array->count;
	memcpy(elements, array->elements, count * unit);
	copy->capacity = capacity;
	copy->unit = unit;
	copy->count = count;
	copy->elements = elements;
	*result = copy;
	return 0;
}

void array_destroy
(struct array* const array) {
	free(array->elements);
	free(array);
}

static int array_expand
(struct array* const array, size_t const count) {
	size_t const unit = array->unit;
	size_t capacity;
	while ((capacity = array->capacity) < count) {
		size_t const higher_capacity = maximum(1, 2 * capacity);
		void* const elements = realloc(array->elements, higher_capacity * unit);
		if (elements == NULL)
			return -1;
		array->capacity = higher_capacity;
		array->elements = elements;
	}
	return 0;
}

static int array_contract
(struct array* const array, size_t const count) {
	size_t const unit = array->unit;
	size_t capacity;
	while ((capacity = array->capacity) / 4 > count) {
		size_t const lower_capacity = maximum(1, capacity / 2);
		void* const elements = realloc(array->elements, lower_capacity * unit);
		if (elements == NULL)
			break;
		array->capacity = lower_capacity;
		array->elements = elements;
	}
	return 0;
}

typedef unsigned char byte;

int array_read
(void* const result, struct array* const array, size_t const position) {
	size_t const count = array->count;
	if (position >= count)
		return -1;
	size_t const unit = array->unit;
	byte* const split = (byte* )array->elements + position * unit;
	if (result != NULL)
		memcpy(result, split, unit);
	return 0;
}

int array_write
(struct array* const array, const void* const element, size_t const position) {
	size_t const count = array->count;
	if (position >= count)
		return -1;
	size_t const unit = array->unit;
	byte* const split = (byte* )array->elements + position * unit;
	memcpy(split, element, unit);
	return 0;
}

static int array_move_right
(struct array* const array, size_t const position, size_t const size) {
	size_t const count = array->count;
	if (position > count)
		return -1;
	if (array_expand(array, count + size) == -1)
		return -1;
	if (size > 0) {
		size_t const unit = array->unit;
		byte* const split = (byte* )array->elements + position * unit;
		size_t const displaced = count - position;
		if (displaced > 0)
			memmove(split + size * unit, split, displaced * unit);
		array->count += size;
	}
	return 0;
}

static int array_move_left
(struct array* const array, size_t const position, size_t const size) {
	size_t const count = array->count;
	if (position > count
			|| size > count - position)
		return -1;
	if (array_contract(array, count - size) == -1)
		return -1;
	if (size > 0) {
		size_t const unit = array->unit;
		byte* const split = (byte* )array->elements + position * unit;
		size_t const displaced = count - position - size;
		if (displaced > 0)
			memmove(split, split + size * unit, displaced * unit);
		array->count -= size;
	}
	return 0;
}

int array_add
(struct array* const array, const void* const element, size_t const position) {
	if (array_move_right(array, position, 1) == -1)
		return -1;
	return array_write(array, element, position);
}

int array_add_last
(struct array* const array, const void* const element) {
	return array_add(array, element, array->count);
}

int array_remove
(void* const result, struct array* const array, size_t const position) {
	size_t const count = array->count;
	if (position >= count)
		return -1;
	if (result != NULL)
		if (array_read(result, array, position) == -1)
			return -1;
	return array_move_left(array, position, 1);
}

int array_remove_last
(void* const result, struct array* const array) {
	return array_remove(result, array, array->count - 1);
}

int array_truncate
(struct array* const array, size_t const count) {
	if (count > array->count)
		return -1;
	if (array_contract(array, count) == -1)
		return -1;
	array->count = count;
	return 0;
}

int array_sort
(struct array* const array, int (* const comparator)(const void*, const void*)) {
	qsort(array, array->count, array->unit, comparator);
	return 0;
}
