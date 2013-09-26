/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#include "property.h" // struct property

#include <stddef.h> // NULL
#include <stdlib.h> // free(), malloc()

#include "arity.h" // enum arity
#include "state.h" // variable

char const* property_name
(struct property const* const property) {
	return property->name;
}

char const* property_abbreviation
(struct property const* const property) {
	return property->abbreviation;
}

enum arity property_arity
(struct property const* const property) {
	return property->arity;
}

variable property_instance
(struct property const* const property) {
	return property->instance;
}

int property_create
(struct property** const result, char const* const name, char const* const abbreviation, enum arity const arity) {
	struct property* const property = malloc(sizeof *property);
	if (property == NULL)
		return -1;
	property->name = name;
	property->arity = arity;
	property->abbreviation = abbreviation;
	*result = property;
	return 0;
}

void property_destroy
(struct property* const property) {
	free(property);
}
