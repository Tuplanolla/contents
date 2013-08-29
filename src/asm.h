/**
Injects assembly instructions.

@author Sampsa "Tuplanolla" Kiiskinen
**/

#ifndef ASM_H
#define ASM_H

/**
Replaces the save command with a custom function.

The call is left out if the function is <code>NULL</code>.

@param function The custom function.
@return The error code.
**/
int inject_save(void (* function)(void));

#endif
