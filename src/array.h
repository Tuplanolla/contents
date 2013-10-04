/**
Provides an interface for
 a safe and automatically resizing array of mutable or constant values.

Fails silently if the size of the array exceeds <code>SIZE_MAX</code>
 which is equal to <code>(size_t )-1</code>.

@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#ifndef ARRAY_H
#define ARRAY_H

#include <stddef.h> // size_t

#include "extensions.h" // __*__, __attribute__ (())

#ifndef ARRAY_NO_KEYWORDS

/**
Annotates the type of array elements.

This annotation can be disabled by defining
@code
#define ARRAY_NO_KEYWORDS
@endcode
before including this header.

For example
@code
struct array* of (int*) array;
@endcode
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

@param array The array or <code>NULL</code>.
**/
void array_destroy
(struct array* array);

/**
Reads the given amount of elements from
 the given position of the given array in constant time.

@param result A pointer to the destination of the elements or <code>NULL</code>.
@param array The array.
@param position The position.
@param count The amount of elements.
@return The number <code>0</code> if successful or <code>-1</code> otherwise.
**/
int array_read_all
(void* result, struct array const* array, size_t position, size_t count)
__attribute__ ((__nonnull__ (2)));

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
Writes the given amount of elements to
 the given position of the given array in constant time.

@param array The array.
@param elements The elements.
@param position The position.
@param count The amount of elements.
@return The number <code>0</code> if successful or <code>-1</code> otherwise.
**/
int array_write_all
(struct array* array, void const* elements, size_t position, size_t count)
__attribute__ ((__nonnull__));

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
@code
char element;
struct array* array;
array_create(&array, 4, sizeof element);
(element = 'A', array_add(array, &element, 0));
(element = 'B', array_add(array, &element, 1));
(element = 'F', array_add(array, &element, 2));
(element = 'G', array_add(array, &element, 3));
array_move_left(array, 2, 3);
@endcode
results in
@code
(array_read(&element, array, 0), element == 'A');
(array_read(&element, array, 1), element == 'B');
(array_read(&element, array, 5), element == 'F');
(array_read(&element, array, 6), element == 'G');
@endcode
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
Splits the given array at the given position plus the given size and
 moves the second part towards the beginning by the given size in linear time.

For example
@code
char element;
struct array* array;
array_create(&array, 7, sizeof element);
(element = 'A', array_add(array, &element, 0));
(element = 'B', array_add(array, &element, 1));
(element = 'C', array_add(array, &element, 2));
(element = 'D', array_add(array, &element, 3));
(element = 'E', array_add(array, &element, 4));
(element = 'F', array_add(array, &element, 5));
(element = 'G', array_add(array, &element, 6));
array_move_left(array, 2, 3);
@endcode
results in
@code
(array_read(&element, array, 0), element == 'A');
(array_read(&element, array, 1), element == 'B');
(array_read(&element, array, 2), element == 'F');
(array_read(&element, array, 3), element == 'G');
@endcode
where three elements are removed.

@param array The array.
@param position The position.
@param size The size.
@return The number <code>0</code> if successful or <code>-1</code> otherwise.
**/
int array_move_in
(struct array* array, size_t position, size_t size)
__attribute__ ((__nonnull__));

/**
Adds the given amount of elements to
 the given position of the given array in linear time.

If the array's capacity is reached, it'll be doubled.

@param array The array.
@param elements The elements.
@param position The position.
@param count The amount of elements.
@return The number <code>0</code> if successful or <code>-1</code> otherwise.
**/
int array_add_all
(struct array* array, void const* elements, size_t position, size_t count)
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
Adds the given amount of elements to
 the end of the given array in constant time.

If the array's capacity is reached, it'll be doubled.

@param array The array.
@param elements The elements.
@param count The amount of elements.
@return The number <code>0</code> if successful or <code>-1</code> otherwise.
**/
int array_add_all_last
(struct array* array, void const* elements, size_t count)
__attribute__ ((__nonnull__));

/**
Adds the given element to the end of the given array in constant time.

If the array's capacity is reached, it'll be doubled.

@param array The array.
@param element The element.
@return The number <code>0</code> if successful or <code>-1</code> otherwise.
**/
int array_add_last
(struct array* array, void const* element)
__attribute__ ((__nonnull__));

/**
Removes the given amount of elements from
 the given position of the given array in linear time.

If the array's capacity is four times too much, it'll be halved.

@param result A pointer to the destination of the element or <code>NULL</code>.
@param array The array.
@param position The position.
@param count The amount of elements.
@return The number <code>0</code> if successful or <code>-1</code> otherwise.
**/
int array_remove_all
(void* result, struct array* array, size_t position, size_t count)
__attribute__ ((__nonnull__ (2)));

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
Removes the given amount of elements from
 the end of the given array in constant time.

If the array's capacity is four times too much, it'll be halved.

@param result A pointer to the destination of the element or <code>NULL</code>.
@param array The array.
@param count The amount of elements.
@return The number <code>0</code> if successful or <code>-1</code> otherwise.
**/
int array_remove_all_last
(void* result, struct array* array, size_t count)
__attribute__ ((__nonnull__ (2)));

/**
Removes the given element from the end of the given array in constant time.

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
Removes elements from the end of the given array until
 it has no elements in constant time.

@param array The array.
@return The number <code>0</code> if successful or <code>-1</code> otherwise.
**/
int array_truncate_whole
(struct array* array)
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

/**
@copydoc array_capacity()
**/
size_t array_const_capacity
(struct array_const const* array)
__attribute__ ((__nonnull__));

/**
@copydoc array_unit()
**/
size_t array_const_unit
(struct array_const const* array)
__attribute__ ((__nonnull__));

/**
@copydoc array_count()
**/
size_t array_const_count
(struct array_const const* array)
__attribute__ ((__nonnull__));

/**
@copydoc array_size()
**/
size_t array_const_size
(struct array_const const* array)
__attribute__ ((__nonnull__));

/**
@copydoc array_create()
**/
int array_const_create
(struct array_const** result, size_t capacity, size_t unit)
__attribute__ ((__nonnull__));

/**
@copydoc array_create_copy()
**/
int array_const_create_copy
(struct array_const** result, struct array_const const* array)
__attribute__ ((__nonnull__));

/**
@copydoc array_destroy()
**/
void array_const_destroy
(struct array_const* array)
__attribute__ ((__nonnull__));

/**
@copydoc array_read_all()
**/
int array_const_read_all
(void const* result, struct array_const const* array, size_t position, size_t count)
__attribute__ ((__nonnull__ (2)));

/**
@copydoc array_read()
**/
int array_const_read
(void const* result, struct array_const const* array, size_t position)
__attribute__ ((__nonnull__ (2)));

/**
@copydoc array_write_all()
**/
int array_const_write_all
(struct array_const* array, void const* elements, size_t position, size_t count)
__attribute__ ((__nonnull__));

/**
@copydoc array_write()
**/
int array_const_write
(struct array_const* array, void const* element, size_t position)
__attribute__ ((__nonnull__));

/**
@copydoc array_move_out()
**/
int array_const_move_out
(struct array_const* array, size_t position, size_t size)
__attribute__ ((__nonnull__));

/**
@copydoc array_move_in()
**/
int array_const_move_in
(struct array_const* array, size_t position, size_t size)
__attribute__ ((__nonnull__));

/**
@copydoc array_add_all()
**/
int array_const_add_all
(struct array_const* array, void const* elements, size_t position, size_t count)
__attribute__ ((__nonnull__));

/**
@copydoc array_add()
**/
int array_const_add
(struct array_const* array, void const* element, size_t position)
__attribute__ ((__nonnull__));

/**
@copydoc array_add_all_last()
**/
int array_const_add_all_last
(struct array_const* array, void const* elements, size_t count)
__attribute__ ((__nonnull__));

/**
@copydoc array_add_last()
**/
int array_const_add_last
(struct array_const* array, void const* element)
__attribute__ ((__nonnull__));

/**
@copydoc array_remove_all()
**/
int array_const_remove_all
(void const* result, struct array_const* array, size_t position, size_t count)
__attribute__ ((__nonnull__ (2)));

/**
@copydoc array_remove()
**/
int array_const_remove
(void const* result, struct array_const* array, size_t position)
__attribute__ ((__nonnull__ (2)));

/**
@copydoc array_remove_all_last()
**/
int array_const_remove_all_last
(void const* result, struct array_const* array, size_t count)
__attribute__ ((__nonnull__ (2)));

/**
@copydoc array_remove_last()
**/
int array_const_remove_last
(void const* result, struct array_const* array)
__attribute__ ((__nonnull__ (2)));

/**
@copydoc array_truncate()
**/
int array_const_truncate
(struct array_const* array, size_t count)
__attribute__ ((__nonnull__));

/**
@copydoc array_truncate_whole()
**/
int array_const_truncate_whole
(struct array_const* array)
__attribute__ ((__nonnull__));

/**
@copydoc array_sort()
**/
int array_const_sort
(struct array_const* array, int (* comparator)(void const*, void const*))
__attribute__ ((__nonnull__));

#endif
