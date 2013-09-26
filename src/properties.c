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
		.arity = ARITY_MONADIC,
		.key = KEY_LOCATION
		}, {
		.name = "editor",
		.abbreviation = "e",
		.arity = ARITY_MONADIC,
		.key = KEY_EDITOR
		}, {
		.name = "completion",
		.abbreviation = "c",
		.arity = ARITY_MONADIC,
		.key = KEY_COMPLETION
		}, {
		.name = "order",
		.abbreviation = "o",
		.arity = ARITY_TRIADIC,
		.key = KEY_ORDER
		}, {
		.name = "wrapping",
		.abbreviation = "w",
		.arity = ARITY_MONADIC,
		.key = KEY_WRAPPING
		}, {
		.name = "justification",
		.abbreviation = "j",
		.arity = ARITY_DYADIC,
		.key = KEY_JUSTIFICATION
		}, {
		.name = "filling",
		.abbreviation = "f",
		.arity = ARITY_DYADIC,
		.key = KEY_FILLING
		}, {
		.name = "interaction",
		.abbreviation = "i",
		.arity = ARITY_MONADIC,
		.key = KEY_INTERACTION
		}, {
		.name = "affix",
		.abbreviation = "a",
		.arity = ARITY_TRIADIC,
		.key = KEY_AFFIX
		}, {
		.name = "headaffix",
		.abbreviation = "ha",
		.arity = ARITY_TRIADIC,
		.key = KEY_HEADAFFIX
		}, {
		.name = "tailaffix",
		.abbreviation = "ta",
		.arity = ARITY_TRIADIC,
		.key = KEY_TAILAFFIX
		}, {
		.name = "unusual",
		.abbreviation = "u",
		.arity = ARITY_DYADIC,
		.key = KEY_UNUSUAL
		}, {
		.name = "preset",
		.abbreviation = "p",
		.arity = ARITY_MONADIC,
		.key = KEY_PRESET
		}};

int properties_create
(struct array** of (struct property) const result) {
	struct array* of (struct property) array;
	if (array_create(&array, KEY_COUNT, sizeof (struct property)) == -1)
		return -1;
	for (size_t position = 0; position < KEY_COUNT; ++position)
		if (array_add_last(array, &properties[position]) == -1) {
			if (array_destroy(array) == -1)
				return -1;
			return -1;
		}
	*result = array;
	return 0;
}

int properties_destroy
(struct array* of (struct property) const array) {
	return array_destroy(array);
}
