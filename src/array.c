/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#include "array.h" // struct array

#include <stddef.h> // size_t, NULL
#include <stdlib.h> // malloc(), free(), realloc()
#include <string.h> // memcpy(), memmove()

#include "calculator.h" // maximum, SIZE_MAX

typedef unsigned char byte;

size_t array_capacity
(struct array* array) {
	return array->capacity;
}

size_t array_unit
(struct array* array) {
	return array->unit;
}

size_t array_count
(struct array* array) {
	return array->count;
}

int array_create
(struct array** const result, const size_t capacity, const size_t unit) {
	if (unit < 1)
		return -1;
	struct array* const array = malloc(sizeof *array);
	if (array == NULL)
		return -1;
	const size_t actual_capacity = maximum(1, capacity);
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

int array_destroy
(struct array* const array) {
	free(array->elements);
	free(array);
	return 0;
}

int array_add
(struct array* const array, void* const element, const size_t position) {
	const size_t count = array->count;
	if (position > count)
		return -1;
	const size_t capacity = array->capacity;
	const size_t unit = array->unit;
	if (count >= capacity) {
		if (capacity > SIZE_MAX / 2)
			return -1;
		const size_t higher_capacity = maximum(1, 2 * capacity);
		void* const elements = realloc(array->elements, higher_capacity * unit);
		if (elements == NULL)
			return -1;
		array->capacity = higher_capacity;
		array->elements = elements;
	}
	byte* const split = (byte* )array->elements + position * unit;
	const size_t displaced = count - position;
	if (displaced > 0)
		memmove(split + unit, split, displaced * unit);
	memcpy(split, element, unit);
	++array->count;
	return 0;
}

int array_remove
(void* const result, struct array* const array, const size_t position) {
	const size_t count = array->count;
	if (count < 1 || position >= count)
		return -1;
	const size_t capacity = array->capacity;
	const size_t unit = array->unit;
	if (count <= capacity / 4) {
		const size_t lower_capacity = maximum(1, capacity / 2);
		void* const elements = realloc(array->elements, lower_capacity * unit);
		if (elements != NULL) {
			array->capacity = lower_capacity;
			array->elements = elements;
		}
	}
	--array->count;
	byte* const split = (byte* )array->elements + position * unit;
	if (result != NULL)
		memcpy(result, split, unit);
	const size_t displaced = count - position;
	if (displaced > 0)
		memmove(split, split + unit, displaced * unit);
	return 0;
}

int array_add_last
(struct array* const array, void* const element) {
	return array_add(array, element, array->count);
}

int array_remove_last
(void* const result, struct array* const array) {
	return array_remove(result, array, array->count - 1);
}

int array_read
(void* const result, struct array* const array, const size_t position) {
	const size_t count = array->count;
	if (count < 1 || position >= count)
		return -1;
	const size_t unit = array->unit;
	byte* const split = (byte* )array->elements + position * unit;
	if (result != NULL)
		memcpy(result, split, unit);
	return 0;
}

int array_write
(struct array* const array, void* const element, const size_t position) {
	const size_t count = array->count;
	if (count < 1 || position >= count)
		return -1;
	const size_t unit = array->unit;
	byte* const split = (byte* )array->elements + position * unit;
	memcpy(split, element, unit);
	return 0;
}
