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

typedef char const* variable;

struct property {
	char const* name;
	char const* abbreviation;
	enum arity arity;
	variable instance;
	enum key key; // ?
};

char const* property_name
(struct property const* property)
__attribute__ ((nonnull));

char const* property_abbreviation
(struct property const* property)
__attribute__ ((nonnull));

enum arity property_arity
(struct property const* property)
__attribute__ ((nonnull));

variable property_instance
(struct property const* property)
__attribute__ ((nonnull));

int property_create
(struct property** result, char const* name, char const* abbreviation, enum arity arity)
__attribute__ ((nonnull));

int property_destroy
(struct property* property)
__attribute__ ((nonnull));

#endif
