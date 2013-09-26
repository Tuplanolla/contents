/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#ifndef IMPLEMENTATIONS_H
#define IMPLEMENTATIONS_H

#include "gnu.h" // __attribute__ (())
#include "state.h" // struct state

int execute_help
(struct state* state)
__attribute__ ((nonnull));

int execute_version
(struct state* state)
__attribute__ ((nonnull));

#endif
