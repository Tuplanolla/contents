/**
@author Sampsa "Tuplanolla" Kiiskinen
@file
**/

#include "project.h"

static const char name[] = "Indefinix";

static const char version[] = "0.0.0";

#define TARGET static const char target[]
#ifdef __linux__
TARGET = "Linux";
#elif _WIN32
TARGET = "Windows";
#else
TARGET = "Unsupported";
#endif
#undef TARGET

const char* get_name(void) {
	return name;
}

const char* get_version(void) {
	return version;
}

const char* get_target(void) {
	return target;
}
