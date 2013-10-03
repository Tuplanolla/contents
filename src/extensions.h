/**
Disables unsupported compiler extensions.

@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#ifndef EXTENSIONS_H
#define EXTENSIONS_H

#ifndef __GNUC__

#define __attribute__(...) // __attribute__ ((...))

#endif

#endif
