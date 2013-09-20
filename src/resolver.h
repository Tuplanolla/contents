/**
Incomplete!
Interprets the user's commands.

@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#ifndef RESOLVER_H
#define RESOLVER_H

#include <stddef.h> // size_t

#include "state.h" // struct action, struct resolution, struct proposal
#include "gnu.h" // __attribute__

int destroy_resolution(struct resolution* resolution);

/**
Finds which of the given actions the given argument refers to.
Automatically completes the given argument if
 it's longer than the given limit or
 the given limit is zero.

Leaks memory if
 the return value isn't <code>NULL</code> and
 isn't given to <code>destroy_state()</code>.

Fails if <code>malloc()</code> fails.

@param actions The actions.
@param argument The argument.
@param limit The automatic completion limit.
@return A resolution, which may contain the action.
**/
struct resolution* create_resolution(const struct actions* actions, const char* argument, size_t limit)
		__attribute__ ((nonnull (1)));

/**
Finds the edit distances of the given actions from the given argument.
Automatically completes the given argument if
 it's longer than the given limit or
 the given limit is zero.

Allocates memory.

Fails if .

@param argument The argument.
@param limit The automatic completion limit.
@return The suggestions in a container.
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
@return The suggestions in a container.
**/
struct proposal* filter(const struct proposal* proposal, size_t limit, size_t distance)
		__attribute__ ((nonnull (1)));

#endif
