/**
@author Sampsa "Tuplanolla" Kiiskinen
**/

#ifndef PARSER_C
#define PARSER_C

#include "parser.h"

#include <stdio.h> // printf

/**
Parses the
 the commands
  configure,
  set,
  pop,
  get,
  obliterate,
  make,
  edit,
  add,
  remove,
  update,
  lookup,
  find,
  touch and
  destroy and
 the keys
  location,
  editor,
  completion,
  order,
  wrapping,
  alignment,
  filling,
  infix,
  prefix,
  suffix,
  headinfix,
  headprefix,
  headsuffix,
  tailinfix,
  tailprefix,
  tailsuffix and
  unusual.
**/
int parse(const char* const* arguments) {
	const char* argument;
	while ((argument = *arguments++)) {
		printf("%s\n", argument);
	}
	return 0;
}

#endif
