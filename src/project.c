/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#include "project.h"

static char const* const name = "Indefinix";

static char const* const version = "0.0.0";

#define TARGET static char const* const target
#if defined __linux__
TARGET = "Linux";
#elif defined _WIN32
TARGET = "Windows";
#else
TARGET = "Unknown";
#endif
#undef TARGET

char const* project_name(void) {
	return name;
}

char const* project_version(void) {
	return version;
}

char const* project_target(void) {
	return target;
}
