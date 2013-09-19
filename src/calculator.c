/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#include "calculator.h"

#include <stddef.h> // size_t
#include <string.h> // strlen()
#include <stdlib.h> // malloc(), free()

size_t minimum(const size_t x, const size_t y) {
	if (x < y)
		return x;
	return y;
}

size_t maximum(const size_t x, const size_t y) {
	if (x > y)
		return x;
	return y;
}

size_t distance(const char* const x, const char* const y) {
	const size_t x_length = strlen(x), y_length = strlen(y);
#define AT(x, y) ((x) * y_length + (y))
	size_t* const distances = malloc((x_length + 1) * (y_length + 1) * sizeof *distances);
	for (size_t character = 0;
			character <= x_length;
			++character)
		distances[AT(character, 0)] = character;
	for (size_t character = 0;
			character <= y_length;
			++character)
		distances[AT(0, character)] = character;
	for (size_t row = 0;
			row < x_length;
			++row) {
		for (size_t column = 0;
				column < y_length;
				++column) {
			size_t cost;
			if (x[row] == y[column])
				cost = 0;
			else
				cost = 1;
			distances[AT(row + 1, column + 1)] = minimum(minimum(
					distances[AT(row, column + 1)] + 1, // deleted
					distances[AT(row + 1, column)] + 1), // inserted
					distances[AT(row, column)] + cost); // modified
			if (row > 1
					&& column > 1
					&& x[row] == y[column - 1]
					&& x[row - 1] == y[column])
				distances[AT(row + 1, column + 1)] = minimum(
						distances[AT(row + 1, column + 1)], // ignored
						distances[AT(row - 1, column - 1)] + cost); // swapped
		}
	}
	const size_t result = distances[AT(x_length, y_length)];
	free(distances);
#undef AT
	return result;
}
