/**
Keeps track of the user.

@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#ifndef LOGGER_H
#define LOGGER_H

#include <stdio.h> // FILE
#include <stdarg.h> // va_list

/**
Logs the given message like <code>vfprintf()</code>.

@param stream The destination stream.
@param format The message format.
@param arguments The format arguments.
**/
void vtrack(FILE* stream, const char * format, va_list arguments)
		__attribute__ ((format(__printf__, 2, 0)));

/**
Logs the given message like <code>fprintf()</code>.

@param stream The destination stream.
@param format The message format.
@param ... The format arguments.
**/
void track(FILE* stream, const char * format, ...)
		__attribute__ ((format(__printf__, 2, 3)));

#endif
