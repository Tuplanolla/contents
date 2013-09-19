/**
Keeps the mutable state away from the user.

@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#ifndef STATE_H
#define STATE_H

#include <stdbool.h> // bool
#include <stdio.h> // FILE

/**
Whether diagnostic messages are written.
**/
extern bool verbose_printing;

/**
The stream where diagnostic messages can be written.
**/
extern FILE* log_stream;

/**
The stream where output is written.
**/
extern FILE* target_stream;

/**
Resets the mutable state.
**/
int initialize(void);

#endif
