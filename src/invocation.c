/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#include "invocation.h" // struct invocation

#include "array.h" // struct array
#include "syntax.h" // of ()
#include "types.h" // procedure

procedure invocation_instance
(struct invocation const* invocation) {
	return invocation->instance;
}

struct array* of (char const*) invocation_arguments
(struct invocation const* invocation) {
	return invocation->arguments;
}
