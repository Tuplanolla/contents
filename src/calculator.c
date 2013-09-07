/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#include <stddef.h> // size_t
#include <string.h> // strlen()

static size_t minimum(const size_t x, const size_t y) {
	if (x < y)
		return x;
	return y;
}

size_t distance(const char* const x, const char* const y) {
	const size_t x_length = strlen(x),
			y_length = strlen(y);
	size_t distances[x_length + 1][y_length + 1]; // -Wvla
	for (size_t character = 0;
			character <= x_length;
			++character)
		distances[character][0] = character;
	for (size_t character = 0;
			character <= y_length;
			++character)
		distances[0][character] = character;
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
			distances[row + 1][column + 1] = minimum(
					distances[row][column + 1] + 1, // deletion
					minimum(distances[row + 1][column] + 1, // insertion
					distances[row][column] + cost)); // substitution
			if (row > 1
					&& column > 1
					&& x[row] == y[column - 1]
					&& x[row - 1] == y[column]) {
				distances[row + 1][column + 1] = minimum(
						distances[row + 1][column + 1],
						distances[row - 1][column - 1] + cost); // swapping
			}
		}
	}
	return distances[x_length][y_length];
}
