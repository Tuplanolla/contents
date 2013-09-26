/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#ifndef PROPERTIES_H
#define PROPERTIES_H

#include "array.h" // struct array
#include "gnu.h" // __attribute__ (())
#include "property.h" // struct property
#include "syntax.h" // of ()

int properties_create
(struct array** of (struct property) result)
__attribute__ ((nonnull));

void properties_destroy
(struct array* of (struct property) properties)
__attribute__ ((nonnull));

#endif
