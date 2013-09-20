#ifndef __BASE_FILE__
#define __BASE_FILE__ __FILE__
#endif

#include <stdlib.h> // malloc(), free()

#include "cheat.h"

GLOBALS(
	char* string;
)

SET_UP({
	string = malloc(1);
})

TEAR_DOWN({
	free(string);
})

TEST(yes, {
	cheat_assert(1 + 1 == 2);
})

TEST(no, {
	cheat_assert(1 == 2);
})
