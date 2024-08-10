#include "wtime.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifdef _OPENMP
#include <omp.h>
#endif

/*  Prototype  */
void wtime( double * );


/*****************************************************************/
/******         E  L  A  P  S  E  D  _  T  I  M  E          ******/
/*****************************************************************/
double elapsed_time( void )
{
    double t;

#if defined(_OPENMP) && (_OPENMP > 200010)
/*  Use the OpenMP timer if we can */
    t = omp_get_wtime();
#else
    wtime( &t );
#endif
    return( t );
}


static double start[64], elapsed[64];
#ifdef _OPENMP
#pragma omp threadprivate(start, elapsed)
#endif

/*****************************************************************/
/******            T  I  M  E  R  _  C  L  E  A  R          ******/
/*****************************************************************/
void timer_clear( int n )
{
    elapsed[n] = 0.0;
}


/*****************************************************************/
/******            T  I  M  E  R  _  S  T  A  R  T          ******/
/*****************************************************************/
void timer_start( int n )
{
    start[n] = elapsed_time();
}


/*****************************************************************/
/******            T  I  M  E  R  _  S  T  O  P             ******/
/*****************************************************************/
void timer_stop( int n )
{
    double t, now;

    now = elapsed_time();
    t = now - start[n];
    elapsed[n] += t;

}


/*****************************************************************/
/******            T  I  M  E  R  _  R  E  A  D             ******/
/*****************************************************************/
double timer_read( int n )
{
    return( elapsed[n] );
}


/*****************************************************************/
/******            C H E C K _ T I M E R _ F L A G          ******/
/*****************************************************************/
int check_timer_flag( void )
{
    int timer_on = 0;
    char *ev = getenv("NPB_TIMER_FLAG");

    if (ev) {
        if (*ev == '\0')
            timer_on = 1;
        else if (*ev >= '1' && *ev <= '9')
            timer_on = 1;
        else if (strcmp(ev, "on") == 0 || strcmp(ev, "ON") == 0 ||
                 strcmp(ev, "yes") == 0 || strcmp(ev, "YES") == 0 ||
                 strcmp(ev, "true") == 0 || strcmp(ev, "TRUE") == 0)
            timer_on = 1;
    }
    else {
        FILE *fp = fopen("timer.flag", "r");
        if (fp != NULL) {
            fclose(fp);
            timer_on = 1;
        }
    }

    return timer_on;
}

