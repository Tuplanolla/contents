/**
Interprets the user's commands.

@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#ifndef RESOLVER_H
#define RESOLVER_H

#include <stddef.h> // size_t

#include "data.h"

/**
Returns the command the given argument refers to if possible.
Automatically completes the given argument if it's longer than the given limit or the given limit is zero.

@param argument The argument.
@param argument The automatic completion limit.
@return The command in a container.
**/
struct container resolve(const char* argument, size_t limit);

/**
Returns the distances of all commands from the given argument.
Automatically completes the given argument if it's longer than the given limit or the given limit is zero.

@param argument The argument.
@param argument The automatic completion limit.
@return The guesses in a container.
**/
struct holder approximate(const char* argument, size_t limit);

#endif
