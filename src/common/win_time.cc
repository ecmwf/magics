// from https://www.linuxjournal.com/article/5574
#include "win_time.h"

int gettimeofday(struct timeval* t, void* timezone)
{
    struct _timeb timebuffer;
    _ftime( &timebuffer );
    t->tv_sec=timebuffer.time;
    t->tv_usec=1000*timebuffer.millitm;
    return 0;
}
