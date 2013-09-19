/**
Keeps track of the user.

@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#ifndef LOGGER_H
#define LOGGER_H

#include <stdarg.h> // va_list

/**
Logs the given message like <code>vfprintf</code>.
**/
void vtrack(const char * message, va_list arguments)
		__attribute__ ((format(printf, 1, 0)));

/**
Logs the given message like <code>fprintf</code>.
**/
void track(const char * fmt, ...)
		__attribute__ ((format(printf, 1, 2)));

#endif
