// adapted from: https://www.codefull.org/2015/12/systime-h-replacement-for-windows/

#ifndef _WIN_TIME_H
#define _WIN_TIME_H

#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN // otherwise windows.h includes winsock.h which clashes with winsock2.h
#define NOMINMAX // otherwise min/max macros in windows.h clash
#include <windows.h>
#include <sys/timeb.h>
#include <sys/types.h>
#include <winsock2.h>

int gettimeofday(struct timeval* t, void* timezone);

#define __need_clock_t
#include <time.h>

/* Structure describing CPU time used by a process and its children.  */
struct tms
{
    clock_t tms_utime;          /* User CPU time.  */
    clock_t tms_stime;          /* System CPU time.  */

    clock_t tms_cutime;         /* User CPU time of dead children.  */
    clock_t tms_cstime;         /* System CPU time of dead children.  */
};

/* Store the CPU time used by this process and all its
   dead children (and their dead children) in BUFFER.
   Return the elapsed real time, or (clock_t) -1 for errors.
   All times are in CLK_TCKths of a second.  */
clock_t times (struct tms *__buffer);

typedef long long suseconds_t ;

#endif
#endif
