/**
Provides immutable data for the user.

@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#ifndef DATA_H
#define DATA_H

#include "state.h" // struct action, struct property

/**
The actions.
**/
extern const struct action actions[COMMAND_COUNT];

/**
The properties.
**/
extern const struct property properties[KEY_COUNT];

#endif
