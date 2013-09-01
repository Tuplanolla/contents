/**
Provides project information for the user.

@author Sampsa "Tuplanolla" Kiiskinen
@file
**/

#ifndef PROJECT_H
#define PROJECT_H

/**
Returns the project's full name.

@return The name.
**/
const char* get_name(void);

/**
Returns the project's version.

@return The version.
**/
const char* get_version(void);

/**
Returns the project's target platform.

@return The target.
**/
const char* get_target(void);

#endif
