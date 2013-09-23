/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#include "property.h" // struct property, variable
#include "arity.h" // enum arity

#include <stdlib.h> // malloc(), free()
#include <stddef.h> // NULL

const char* property_name
(const struct property* const property) {
	return property->name;
}

const char* property_abbreviation
(const struct property* const property) {
	return property->abbreviation;
}

enum arity property_arity
(const struct property* const property) {
	return property->arity;
}

variable property_instance
(const struct property* const property) {
	return property->instance;
}

int property_create
(struct property** const result, const char* const name, const char* const abbreviation, const enum arity arity) {
	struct property* const property = malloc(sizeof *property);
	if (property == NULL)
		return -1;
	property->name = name;
	property->arity = arity;
	property->abbreviation = abbreviation;
	*result = property;
	return 0;
}

int property_destroy
(struct property* const property) {
	free(property);
	return 0;
}
