/**
Conveniently logs diagnostic messages.

@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#ifndef TRACER_H
#define TRACER_H

#include <stdarg.h> // va_list

#include "extensions.h" // __attribute__ (())

/**
Logs the given message.

@param format The message format.
@param arguments The format arguments.
@return The number <code>0</code> if successful or <code>-1</code> otherwise.
**/
int trace_va
(char const* format, va_list arguments)
__attribute__ ((format(__printf__, 1, 0)));

/**
Logs the given message.

@param format The message format.
@param ... The format arguments.
@return The number <code>0</code> if successful or <code>-1</code> otherwise.
**/
int trace
(char const* format, ...)
__attribute__ ((format(__printf__, 1, 2)));

#ifdef DEBUG

/**
Enables logging.
**/
#define TRACE trace

#else

/**
Disables logging.
**/
#define TRACE (void )

#endif

#endif
