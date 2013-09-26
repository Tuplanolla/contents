/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#include "properties.h"

#include "arity.h" // enum arity
#include "array.h" // array_add_last(), array_create(), array_destroy()
#include "property.h" // struct property
#include "syntax.h" // of ()

static struct property const properties[] = {{
		.name = "location",
		.abbreviation = "l",
		.arity = ARITY_MONADIC
		}, {
		.name = "editor",
		.abbreviation = "e",
		.arity = ARITY_MONADIC
		}, {
		.name = "completion",
		.abbreviation = "c",
		.arity = ARITY_MONADIC
		}, {
		.name = "order",
		.abbreviation = "o",
		.arity = ARITY_TRIADIC
		}, {
		.name = "wrapping",
		.abbreviation = "w",
		.arity = ARITY_MONADIC
		}, {
		.name = "justification",
		.abbreviation = "j",
		.arity = ARITY_DYADIC
		}, {
		.name = "filling",
		.abbreviation = "f",
		.arity = ARITY_DYADIC
		}, {
		.name = "interaction",
		.abbreviation = "i",
		.arity = ARITY_MONADIC
		}, {
		.name = "affix",
		.abbreviation = "a",
		.arity = ARITY_TRIADIC
		}, {
		.name = "headaffix",
		.abbreviation = "ha",
		.arity = ARITY_TRIADIC
		}, {
		.name = "tailaffix",
		.abbreviation = "ta",
		.arity = ARITY_TRIADIC
		}, {
		.name = "unusual",
		.abbreviation = "u",
		.arity = ARITY_DYADIC
		}, {
		.name = "preset",
		.abbreviation = "p",
		.arity = ARITY_MONADIC
		}};

int properties_create
(struct array** of (struct property) const result) {
	struct array* of (struct property) array;
	size_t const count = sizeof properties / sizeof *properties;
	if (array_create(&array, count, sizeof (struct property)) == -1)
		return -1;
	for (size_t position = 0; position < count; ++position)
		if (array_add_last(array, &properties[position]) == -1) {
			array_destroy(array);
			return -1;
		}
	*result = array;
	return 0;
}

void properties_destroy
(struct array* of (struct property) const array) {
	array_destroy(array);
}
