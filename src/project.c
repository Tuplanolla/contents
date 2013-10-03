/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#include "project.h"

static char const name[] = "Indefinix";

static char const version[] = "0.0.0";

#define PLATFORM static char const platform[]
#if defined __linux__
PLATFORM = "Linux";
#elif defined _WIN32
PLATFORM = "Windows";
#else
PLATFORM = "Unknown";
#endif
#undef PLATFORM

char const* project_name(void) {
	return &name[0];
}

char const* project_version(void) {
	return &version[0];
}

char const* project_platform(void) {
	return &platform[0];
}
