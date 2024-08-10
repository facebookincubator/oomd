#ifndef __C_TIMERS_H
#define __C_TIMERS_H

extern void   timer_clear( int n );
extern void   timer_start( int n );
extern void   timer_stop( int n );
extern double timer_read( int n );
extern int    check_timer_flag( void );

#endif

