/**
Provides an interface for
 a safe and automatically resizing array of mutable or constant values.

Examples may use the variables
<pre>
int element;
struct array* array;
</pre>
without declaring them explicitly.

@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#ifndef ARRAY_H
#define ARRAY_H

#include <stddef.h> // size_t

#include "gnu.h" // __nonnull__, __attribute__ (())

#ifndef ARRAY_NO_KEYWORDS

/**
Annotates the type of array elements.

This annotation can be disabled by defining
<pre>
#define ARRAY_NO_KEYWORDS
</pre>
before including this header.

For example
<pre>
struct array* of (int*) array;
</pre>
declares an array that contains pointers to integers.
**/
#define of(...) // of (...)

#endif

/**
An array of mutable values.
**/
struct array {
	size_t capacity;
	size_t unit;
	size_t count;
	void* elements;
};

/**
An array of constant values.

A constant array can be cast into a mutable array and vice versa.
**/
struct array_const {
	size_t capacity;
	size_t unit;
	size_t count;
	void const* elements;
};

/*
The following declarations are for mutable arrays.
*/

/**
Calculates the current capacity of the given array in constant time.

@param array The array.
@return The current capacity.
**/
size_t array_capacity
(struct array const* array)
__attribute__ ((__nonnull__));

/**
Calculates the element size of the given array in constant time.

@param array The array.
@return The element size.
**/
size_t array_unit
(struct array const* array)
__attribute__ ((__nonnull__));

/**
Calculates the amount of elements in the given array in constant time.

@param array The array.
@return The amount of elements.
**/
size_t array_count
(struct array const* array)
__attribute__ ((__nonnull__));

/**
Calculates the memory footprint of the given array in constant time.

The memory may not be continuously allocated.

@param array The array.
@return The memory footprint.
**/
size_t array_size
(struct array const* array)
__attribute__ ((__nonnull__));

/**
Creates a new array with the given initial capacity and the given element size
 in constant time.

The new array has to be destroyed later.

@param result A pointer to the destination of the new array.
@param capacity The initial capacity.
@param unit The element size.
@return The number <code>0</code> if successful or <code>-1</code> otherwise.
**/
int array_create
(struct array** result, size_t capacity, size_t unit)
__attribute__ ((__nonnull__));

/**
Copies the given array in linear time.

The copy has to be destroyed later.

@param result The location of a pointer to the new array.
@param array The array to copy.
@return The number <code>0</code> if successful or <code>-1</code> otherwise.
**/
int array_create_copy
(struct array** result, struct array const* array)
__attribute__ ((__nonnull__));

/**
Destroys the given array in constant time.

@param array The array.
**/
void array_destroy
(struct array* array)
__attribute__ ((__nonnull__));

/**
Reads an element from the given position of the given array in constant time.

@param result A pointer to the destination of the element or <code>NULL</code>.
@param array The array.
@param position The position.
@return The number <code>0</code> if successful or <code>-1</code> otherwise.
**/
int array_read
(void* result, struct array const* array, size_t position)
__attribute__ ((__nonnull__ (2)));

/**
Writes the given element to the given position of the given array
 in constant time.

@param array The array.
@param element The element.
@param position The position.
@return The number <code>0</code> if successful or <code>-1</code> otherwise.
**/
int array_write
(struct array* array, void const* element, size_t position)
__attribute__ ((__nonnull__));

/**
Splits the given array at the given position and
 moves the second part towards the end by the given size in linear time.

For example
<pre>
array_create(&array, 4, 1);
array_add_last(array, 'A');
array_add_last(array, 'B');
array_add_last(array, 'C');
array_add_last(array, 'D');
array_move_left(array, 2, 3);
</pre>
results in
<pre>
array_read(&element, array, 0), element == 'A';
array_read(&element, array, 1), element == 'E';
array_read(&element, array, 5), element == 'F';
array_read(&element, array, 6), element == 'D';
</pre>
where three elements are undefined.

@param array The array.
@param position The position.
@param size The size.
@return The number <code>0</code> if successful or <code>-1</code> otherwise.
**/
int array_move_out
(struct array* array, size_t position, size_t size)
__attribute__ ((__nonnull__));

/**
Splits the given array at the given position and
 moves the second part towards the beginning by the given size in linear time.

For example
<pre>
array_create(&array, 6, 1);
array_add_last(array, 'A');
array_add_last(array, 'B');
array_add_last(array, 'C');
array_add_last(array, 'D');
array_add_last(array, 'E');
array_add_last(array, 'F');
array_move_left(array, 4, 3);
</pre>
results in
<pre>
array_read(&element, array, 0), element == 'A';
array_read(&element, array, 1), element == 'E';
array_read(&element, array, 2), element == 'F';
array_read(&element, array, 3), element == 'D';
</pre>
where two elements are removed.

@param array The array.
@param position The position.
@param size The size.
@return The number <code>0</code> if successful or <code>-1</code> otherwise.
**/
int array_move_in
(struct array* array, size_t position, size_t size)
__attribute__ ((__nonnull__));

/**
Adds the given element to the given position of the given array in linear time.

If the array's capacity is reached, it'll be doubled.

@param array The array.
@param element The element.
@param position The position.
@return The number <code>0</code> if successful or <code>-1</code> otherwise.
**/
int array_add
(struct array* array, void const* element, size_t position)
__attribute__ ((__nonnull__));

/**
Adds the given element to the end of the given array in constant time.

If the array's capacity is reached, it'll be doubled.

@param array The array.
@param element The element.
@param position The position.
@return The number <code>0</code> if successful or <code>-1</code> otherwise.
**/
int array_add_last
(struct array* array, void const* element)
__attribute__ ((__nonnull__));

/**
Removes an element from the given position of the given array in linear time.

If the array's capacity is four times too much, it'll be halved.

@param result A pointer to the destination of the element or <code>NULL</code>.
@param array The array.
@param position The position.
@return The number <code>0</code> if successful or <code>-1</code> otherwise.
**/
int array_remove
(void* result, struct array* array, size_t position)
__attribute__ ((__nonnull__ (2)));

/**
Removes the given element to the end of the given array in constant time.

If the array's capacity is four times too much, it'll be halved.

@param result A pointer to the destination of the element or <code>NULL</code>.
@param array The array.
@return The number <code>0</code> if successful or <code>-1</code> otherwise.
**/
int array_remove_last
(void* result, struct array* array)
__attribute__ ((__nonnull__ (2)));

/**
Removes elements from the end of the given array until
 it has the given amount of elements in constant time.

@param array The array.
@param count The amount of elements.
@return The number <code>0</code> if successful or <code>-1</code> otherwise.
**/
int array_truncate
(struct array* array, size_t count)
__attribute__ ((__nonnull__));

/**
Sorts the given array based on the given comparator in logarithmic time.

@param array The array.
@param comparator The comparator.
@return The number <code>0</code> if successful or <code>-1</code> otherwise.
**/
int array_sort
(struct array* array, int (* comparator)(void const*, void const*))
__attribute__ ((__nonnull__));

/*
The following declarations are for constant arrays.
*/

size_t array_const_capacity
(struct array_const const* array)
__attribute__ ((__nonnull__));

size_t array_const_unit
(struct array_const const* array)
__attribute__ ((__nonnull__));

size_t array_const_count
(struct array_const const* array)
__attribute__ ((__nonnull__));

int array_const_create
(struct array_const** result, size_t capacity, size_t unit)
__attribute__ ((__nonnull__));

int array_const_create_copy
(struct array_const** result, struct array_const const* array)
__attribute__ ((__nonnull__));

void array_const_destroy
(struct array_const* array)
__attribute__ ((__nonnull__));

int array_const_read
(void const* result, struct array_const const* array, size_t position)
__attribute__ ((__nonnull__ (2)));

int array_const_write
(struct array_const* array, void const* element, size_t position)
__attribute__ ((__nonnull__));

int array_const_move_out
(struct array_const* array, size_t position, size_t size)
__attribute__ ((__nonnull__));

int array_const_move_in
(struct array_const* array, size_t position, size_t size)
__attribute__ ((__nonnull__));

int array_const_add
(struct array_const* array, void const* element, size_t position)
__attribute__ ((__nonnull__));

int array_const_add_last
(struct array_const* array, void const* element)
__attribute__ ((__nonnull__));

int array_const_remove
(void const* result, struct array_const* array, size_t position)
__attribute__ ((__nonnull__ (2)));

int array_const_remove_last
(void const* result, struct array_const* array)
__attribute__ ((__nonnull__ (2)));

int array_const_truncate
(struct array_const* array, size_t count)
__attribute__ ((__nonnull__));

int array_const_sort
(struct array_const* array, int (* comparator)(void const*, void const*))
__attribute__ ((__nonnull__));

#endif
