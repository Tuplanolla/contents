/**
Incomplete!

@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#include "project.h"

#include "target.h" // TARGET_*

const char* const project_name = "Indefinix";

const char* const project_version = "0.0.0";

#define TARGET const char* const project_target
#if defined TARGET_LINUX
TARGET = "Linux";
#elif defined TARGET_WINDOWS
TARGET = "Windows";
#else
TARGET = "Unknown";
#endif
#undef TARGET
