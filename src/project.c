/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#include "project.h"

char const* const name = "Indefinix";

char const* const version = "0.0.0";

#define TARGET char const* const target
#if defined __linux__
TARGET = "Linux";
#elif defined _WIN32
TARGET = "Windows";
#else
TARGET = "Unknown";
#endif
#undef TARGET

char const* const project_name(void) {
	return name;
}

char const* const project_version(void) {
	return version;
}

char const* const project_target(void) {
	return target;
}
