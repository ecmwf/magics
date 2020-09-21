#ifndef _WIN_TIME_H
#define _WIN_TIME_H

#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN  // otherwise windows.h includes winsock.h which clashes with winsock2.h
#define NOMINMAX             // otherwise min/max macros in windows.h clash
#include <sys/timeb.h>
#include <sys/types.h>
#include <time.h>
#include <windows.h>
#include <winsock2.h>

int gettimeofday(struct timeval* t, void* timezone);

#endif
#endif
