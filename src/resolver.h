/**
Incomplete!
Interprets the user's commands.

@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#ifndef RESOLVER_H
#define RESOLVER_H

#include <stddef.h> // size_t

#include "state.h" // struct action, struct maybe, struct proposal
#include "gnu.h" // __attribute__

/**
Finds which of the given actions the given argument refers to.
Automatically completes the given argument if
 it's longer than the given limit or
 the given limit is zero.

Allocates memory.

Fails if
 the actions are <code>NULL</code> or
 <code>malloc()</code> fails.

@param actions The actions.
@param argument The argument.
@param limit The automatic completion limit.
@return A resolution, which may contain the action.
**/
struct maybe* resolve(const struct action* actions, const char* argument, size_t limit)
		__attribute__ ((nonnull (1, 2)));

/**
Finds the edit distances of the given actions from the given argument.
Automatically completes the given argument if
 it's longer than the given limit or
 the given limit is zero.

Allocates memory.

Fails if .

@param argument The argument.
@param limit The automatic completion limit.
@return The guesses in a container.
**/
struct proposal* correct(const struct action* actions, const char* argument, size_t limit)
		__attribute__ ((nonnull (1, 2)));

/**
Picks at most the given amount of suggestions less than the given distance away,
 sorting them in the process.

Allocates memory.

Fails if .

@param fail The failure.
@param limit The amount.
@param distance The distance.
@return The guesses in a container.
**/
struct proposal* filter(const struct proposal* proposal, size_t limit, size_t distance)
		__attribute__ ((nonnull (1)));

#endif
