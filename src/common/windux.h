#ifndef MAG_WINDUX
#define MAG_WINDUX

#define WIN32_LEAN_AND_MEAN  // otherwise windows.h includes winsock.h which clashes with winsock2.h
#define NOMINMAX             // otherwise min/max macros in windows.h clash
#include <sys/timeb.h>
#include <sys/types.h>
#include <time.h>
#include <windows.h>
#include <winsock2.h>

struct DIR;

struct dirent {
    const char* d_name;
};

int gettimeofday(struct timeval* t, void* timezone);

DIR* opendir(const char* path);
struct dirent* readdir(DIR* dir);
void closedir(DIR* dir);

#endif
