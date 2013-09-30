/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#ifndef STATE_H
#define STATE_H

#include <stddef.h> // size_t
#include <stdio.h> // FILE

enum policy {
	POLICY_ABORT,
	POLICY_CONTINUE,

	POLICY_COUNT
};

enum verbosity {
	VERBOSITY_MINIMAL,
	VERBOSITY_MAXIMAL,

	VERBOSITY_COUNT
};

enum command {
	COMMAND_CONFIGURE,
	COMMAND_SET,
	COMMAND_POP,
	COMMAND_GET,
	COMMAND_OBLITERATE,
	COMMAND_MAKE,
	COMMAND_EDIT,
	COMMAND_ADD,
	COMMAND_REMOVE,
	COMMAND_UPDATE,
	COMMAND_LOOKUP,
	COMMAND_FIND,
	COMMAND_TOUCH,
	COMMAND_DESTROY,
	COMMAND_HELP,
	COMMAND_VERSION,
	COMMAND_INFER,
	COMMAND_BIND,

	COMMAND_COUNT
};

enum key {
	KEY_LOCATION,
	KEY_EDITOR,
	KEY_COMPLETION,
	KEY_ORDER,
	KEY_WRAPPING,
	KEY_JUSTIFICATION,
	KEY_FILLING,
	KEY_INTERACTION,
	KEY_YES,
	KEY_NO,
	KEY_AFFIX,
	KEY_HEADAFFIX,
	KEY_TAILAFFIX,
	KEY_UNUSUAL,
	KEY_PRESET,

	KEY_COUNT
};

enum sorting {
	SORTING_NORMAL,
	SORTING_REVERSE,
	SORTING_NONE,

	SORTING_COUNT
};

enum grouping {
	GROUPING_DIRECTORIES,
	GROUPING_FILES,
	GROUPING_NONE,

	GROUPING_COUNT
};

enum hiding {
	HIDING_HIDDEN,
	HIDING_NONE,

	HIDING_COUNT
};

enum continuation {
	CONTINUATION_WRAP,
	CONTINUATION_NONE,

	CONTINUATION_COUNT
};

enum alignment {
	ALIGNMENT_LEFT,
	ALIGNMENT_RIGHT,
	ALIGNMENT_CENTER,

	ALIGNMENT_COUNT
};

enum padding {
	PADDING_FILL,
	PADDING_NONE,

	PADDING_COUNT
};

enum answer {
	ANSWER_YES,
	ANSWER_NO,
	ANSWER_NONE,

	ANSWER_COUNT
};

enum selection {
	SELECTION_DEFAULT,

	SELECTION_COUNT
};

#define ARRAY(type, name) struct {\
	size_t capacity;\
	size_t count;\
	type* elements;\
} name;

struct call {
	enum command command;
	ARRAY(void*, arguments);
};

struct state {
	ARRAY(struct call, invocation);
	struct {
		struct {
			ARRAY(char, place);
			enum policy policy;
			enum verbosity verbosity;
			struct {
				size_t amount;
				size_t distance;
			} suggestion;
			FILE* output;
		} developer;
		struct {
			ARRAY(char, location);
			ARRAY(char, editor);
			size_t completion;
			struct {
				enum sorting sorting;
				enum grouping grouping;
				enum hiding hiding;
			} order;
			enum continuation wrapping;
			struct {
				enum alignment first;
				enum alignment second;
			} justification;
			struct {
				enum padding first;
				enum padding second;
			} filling;
			enum answer interaction;
			struct {
				ARRAY(char, prefix);
				ARRAY(char, infix);
				ARRAY(char, suffix);
			} affix;
			struct {
				ARRAY(char, prefix);
				ARRAY(char, infix);
				ARRAY(char, suffix);
			} headaffix;
			struct {
				ARRAY(char, prefix);
				ARRAY(char, infix);
				ARRAY(char, suffix);
			} tailaffix;
			struct {
				ARRAY(char, indexed);
				ARRAY(char, present);
			} unusual;
		} user;
	} configuration;
};

#undef ARRAY

#endif
