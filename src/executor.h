/**
Incomplete!
Carries out the user's requests.

@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#ifndef EXECUTOR_H
#define EXECUTOR_H

#include "data.h" // struct action

/**
Does something.
**/
int execute(struct action resolution, const char* const* arguments);

#endif
