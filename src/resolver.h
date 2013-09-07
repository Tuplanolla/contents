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
Tries to find which command corresponds to the given argument.
Automatically completes the given argument if it's longer than the given limit or the given limit is zero.

@param argument The argument.
@param argument The automatic completion limit.
@return The command in a container.
**/
struct container resolve(const char* argument, size_t limit);

/**
Finds which commands are closest to the given argument.
Automatically completes the given argument if it's longer than the given limit or the given limit is zero.

@param argument The argument.
@param argument The automatic completion limit.
@return The guesses in a container.
**/
struct holder approximate(const char* argument, size_t limit);

#endif
