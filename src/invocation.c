/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#include "invocation.h" // struct invocation

#include "action.h" // procedure
#include "array.h" // struct array
#include "syntax.h" // of ()

procedure invocation_instance
(struct invocation const* invocation) {
	return invocation->instance;
}

struct array* of (void*) invocation_arguments
(struct invocation const* invocation) {
	return invocation->arguments;
}
