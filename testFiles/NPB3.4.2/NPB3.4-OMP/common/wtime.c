#include "wtime.h"
#include <time.h>
#ifndef DOS
#include <sys/time.h>
#endif

void wtime(double *t)
{
   /* a generic timer */
   static int sec = -1;
   struct timeval tv;
   gettimeofday(&tv, (void *)0);
   if (sec < 0) sec = tv.tv_sec;
   *t = (tv.tv_sec - sec) + 1.0e-6*tv.tv_usec;
}

