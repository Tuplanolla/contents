/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#include "gnu.h" // __attribute__
#include "state.h" // struct state, create_state(), destroy_state()
#include "parser.h" // parse()
#include "executor.h" // execute()

/**
Manages the state.

Fails if
 <code>create_state()</code>,
 <code>parse()</code> or
 <code>execute()</code> fails.

@param count The amount of elements in <code>arguments</code>.
@param arguments A <code>NULL</code> terminated array of
 the executable's name and
 its command line arguments.
@return The number <code>0</code> if successful or
 a positive number less than <code>128</code> otherwise.
**/
int main(const int count __attribute__ ((unused)),
		const char* const* const arguments) {
	struct state* const state = create_state();
	if (state == NULL)
		return 1;
	state->arguments = arguments + 1;
	if (parse(state) == -1) {
		destroy_state(state);
		return 2;
	}
	if (execute(state) == -1) {
		destroy_state(state);
		return 3;
	}
	return 0;
}
