/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#ifndef ARITY_H
#define ARITY_H

#include <stddef.h> // size_t

#include "gnu.h" // __attribute__ (())

enum arity {
	ARITY_NILADIC,
	ARITY_MONADIC,
	ARITY_DYADIC,
	ARITY_TRIADIC,
	ARITY_TETRADIC,
	ARITY_PENTADIC,
	ARITY_HEXADIC,
	ARITY_HEPTADIC,
	ARITY_OCTADIC,
	ARITY_NONADIC,
	ARITY_DECADIC,
	ARITY_VARIADIC,

	ARITY_COUNT
};

int arity_to_integral
(size_t* result, enum arity arity)
__attribute__ ((nonnull));

int arity_from_integral
(enum arity* result, size_t integral)
__attribute__ ((nonnull));

#endif
