/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#ifndef PROPERTY_H
#define PROPERTY_H

#include "arity.h" // enum arity

#include "gnu.h" // __attribute__ (())

enum key {
	KEY_LOCATION,
	KEY_EDITOR,
	KEY_COMPLETION,
	KEY_ORDER,
	KEY_WRAPPING,
	KEY_JUSTIFICATION,
	KEY_FILLING,
	KEY_INTERACTION,
	KEY_AFFIX,
	KEY_HEADAFFIX,
	KEY_TAILAFFIX,
	KEY_UNUSUAL,
	KEY_PRESET,

	KEY_COUNT
};

typedef const char* variable;

struct property {
	const char* name;
	const char* abbreviation;
	enum arity arity;
	variable instance;
	enum key key; // ?
};

const char* property_name
(const struct property* property)
__attribute__ ((nonnull));

const char* property_abbreviation
(const struct property* property)
__attribute__ ((nonnull));

enum arity property_arity
(const struct property* property)
__attribute__ ((nonnull));

variable property_instance
(const struct property* property)
__attribute__ ((nonnull));

int property_create
(struct property** result, const char* name, const char* abbreviation, enum arity arity)
__attribute__ ((nonnull));

int property_destroy
(struct property* property)
__attribute__ ((nonnull));

#endif
