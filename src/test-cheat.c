/**
Tests the unit testing framework.

@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#ifndef __BASE_FILE__
#define __BASE_FILE__ __FILE__
#endif

#include <cheat.h>

GLOBALS()

SET_UP({})

TEAR_DOWN({})

TEST(cheat, {
	cheat_assert(1);
})
