/**
Incomplete!
Brings target information to the user.

@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#ifndef TARGET_H
#define TARGET_H

#if defined __linux__
#define TARGET_LINUX
#elif defined _WIN32
#define TARGET_WINDOWS
#else
#define TARGET_UNKNOWN
#endif

#endif
