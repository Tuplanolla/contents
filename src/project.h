/**
Shares project information.

@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#ifndef PROJECT_H
#define PROJECT_H

#include "extensions.h" // __*__, __attribute__ (())

/**
Returns the project's name.

@return The name.
**/
char const* project_name
(void)
__attribute__ ((__pure__));

/**
Returns the project's version.

@return The version.
**/
char const* project_version
(void)
__attribute__ ((__pure__));

/**
Returns the project's target platform.

@return The target platform.
**/
char const* project_platform
(void)
__attribute__ ((__pure__));

#endif
