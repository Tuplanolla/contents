/**
Provides an interface for a state.

@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#ifndef STATE_H
#define STATE_H

#include <stddef.h> // size_t
#include <stdio.h> // FILE

#include "action.h" // struct call
#include "array.h" // struct array*, of ()
#include "extensions.h" // __*__, __attribute__ (())

enum status {
	STATUS_NORMAL,
	STATUS_ERROR,

	STATUS_COUNT
};

enum behavior {
	BEHAVIOR_ABORT,
	BEHAVIOR_CONTINUE,

	BEHAVIOR_COUNT
};

enum verbosity {
	VERBOSITY_DIAGNOSTIC,
	VERBOSITY_NONE,

	VERBOSITY_COUNT
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

struct execution {
	enum status status;
	struct array_const* of (struct call const) calls;
};

struct configuration {
	struct {
		struct array* of (char) place;
		struct {
			size_t amount;
			size_t distance;
		} suggestion;
		enum behavior behavior;
		enum verbosity verbosity;
		FILE* output;
	} invisible;
	struct {
		struct array* of (char) location;
		struct array* of (char) editor;
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
			struct array* of (char) prefix;
			struct array* of (char) infix;
			struct array* of (char) suffix;
		} affix;
		struct {
			struct array* of (char) prefix;
			struct array* of (char) infix;
			struct array* of (char) suffix;
		} headaffix;
		struct {
			struct array* of (char) prefix;
			struct array* of (char) infix;
			struct array* of (char) suffix;
		} tailaffix;
		struct {
			struct array* of (char) indexed;
			struct array* of (char) present;
		} unusual;
	} visible;
};

struct state {
	struct execution execution;
	struct configuration configuration;
};

enum status state_execution_status
(struct state const* state)
__attribute__ ((__nonnull__));

int state_create
(struct state** result)
__attribute__ ((__nonnull__));

void state_destroy
(struct state* state);

int state_parse
(struct state* state, struct array_const* of (char const*) arguments)
__attribute__ ((__nonnull__));

int state_execute
(struct state* state)
__attribute__ ((__nonnull__));

#endif
