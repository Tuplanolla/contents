/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#ifndef PROPERTIES_H
#define PROPERTIES_H

#include "gnu.h" // __attribute__ (())
#include "syntax.h" // of ()
#include "array.h" // struct array
#include "property.h" // struct property

int properties_create
(struct array** of (struct property*) result)
__attribute__ ((nonnull));

int properties_destroy
(struct array* of (struct property*) properties)
__attribute__ ((nonnull));

#endif
