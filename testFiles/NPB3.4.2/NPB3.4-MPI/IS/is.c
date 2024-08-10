/*************************************************************************
 *                                                                       * 
 *        N  A  S     P A R A L L E L     B E N C H M A R K S  3.4       *
 *                                                                       * 
 *                                  I S                                  * 
 *                                                                       * 
 ************************************************************************* 
 *                                                                       * 
 *   This benchmark is part of the NAS Parallel Benchmark 3.4 suite.     *
 *   It is described in NAS Technical Report 95-020.                     * 
 *                                                                       * 
 *   Permission to use, copy, distribute and modify this software        * 
 *   for any purpose with or without fee is hereby granted.  We          * 
 *   request, however, that all derived work reference the NAS           * 
 *   Parallel Benchmarks 3.4. This software is provided "as is"          *
 *   without express or implied warranty.                                * 
 *                                                                       * 
 *   Information on NPB 3.4, including the technical report, the         *
 *   original specifications, source code, results and information       * 
 *   on how to submit new results, is available at:                      * 
 *                                                                       * 
 *          http://www.nas.nasa.gov/Software/NPB                         * 
 *                                                                       * 
 *   Send comments or suggestions to  npb@nas.nasa.gov                   * 
 *   Send bug reports to              npb-bugs@nas.nasa.gov              * 
 *                                                                       * 
 *         NAS Parallel Benchmarks Group                                 * 
 *         NASA Ames Research Center                                     * 
 *         Mail Stop: T27A-1                                             * 
 *         Moffett Field, CA   94035-1000                                * 
 *                                                                       * 
 *         E-mail:  npb@nas.nasa.gov                                     * 
 *         Fax:     (650) 604-3957                                       * 
 *                                                                       * 
 ************************************************************************* 
 *                                                                       * 
 *   Author: M. Yarrow                                                   * 
 *           H. Jin                                                      * 
 *                                                                       * 
 *************************************************************************/

#include "mpi.h"
#include "npbparams.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

/******************/
/* default values */
/******************/
#ifndef CLASS
#define CLASS 'S'
#define NUM_PROCS            1                 
#endif
#define MIN_PROCS            1
#define ONE                  1


/*************/
/*  CLASS S  */
/*************/
#if CLASS == 'S'
#define  TOTAL_KEYS_LOG_2    16
#define  MAX_KEY_LOG_2       11
#define  NUM_BUCKETS_LOG_2   9
#endif


/*************/
/*  CLASS W  */
/*************/
#if CLASS == 'W'
#define  TOTAL_KEYS_LOG_2    20
#define  MAX_KEY_LOG_2       16
#define  NUM_BUCKETS_LOG_2   10
#endif

/*************/
/*  CLASS A  */
/*************/
#if CLASS == 'A'
#define  TOTAL_KEYS_LOG_2    23
#define  MAX_KEY_LOG_2       19
#define  NUM_BUCKETS_LOG_2   10
#endif


/*************/
/*  CLASS B  */
/*************/
#if CLASS == 'B'
#define  TOTAL_KEYS_LOG_2    25
#define  MAX_KEY_LOG_2       21
#define  NUM_BUCKETS_LOG_2   10
#endif


/*************/
/*  CLASS C  */
/*************/
#if CLASS == 'C'
#define  TOTAL_KEYS_LOG_2    27
#define  MAX_KEY_LOG_2       23
#define  NUM_BUCKETS_LOG_2   10
#endif


/*************/
/*  CLASS D  */
/*************/
#if CLASS == 'D'
#define  TOTAL_KEYS_LOG_2    29     /* 2^31 */
#define  MAX_KEY_LOG_2       27
#define  NUM_BUCKETS_LOG_2   10
#undef   MIN_PROCS
#define  MIN_PROCS           4
#endif


/*************/
/*  CLASS E  */
/*************/
#if CLASS == 'E'
#define  TOTAL_KEYS_LOG_2    29     /* 2^35 */
#define  MAX_KEY_LOG_2       31
#define  NUM_BUCKETS_LOG_2   10
#undef   MIN_PROCS
#define  MIN_PROCS           64
#undef   ONE
#define  ONE                 1L
#endif


/*******************************************************************
 * Defining MIN_PROCS is to avoid integer overflow for large problem 
 * sizes without using a larger integer type, such as long int.
 * The actual total keys = TOTAL_KEYS * MIN_PROCS
 *******************************************************************/
#define  TOTAL_KEYS          (1 << TOTAL_KEYS_LOG_2)

#define  MAX_KEY             (ONE << MAX_KEY_LOG_2)
#define  NUM_BUCKETS         (1 << NUM_BUCKETS_LOG_2)

/*****************************************************************/
/* NOTE: THIS CODE CANNOT BE RUN ON ARBITRARILY LARGE NUMBERS OF */
/* PROCESSORS. THE LARGEST VERIFIED NUMBER IS 1024. INCREASE     */
/* MAX_PROCS AT YOUR PERIL                                       */
/*****************************************************************/
#if CLASS == 'S'
#define  MAX_PROCS           128
#else
#define  MAX_PROCS           1024
#endif

#define  MAX_ITERATIONS      100
#define  TEST_ARRAY_SIZE     5


/* Number of keys assigned to each processor
 * #define  NUM_KEYS            (TOTAL_KEYS/NUM_PROCS*MIN_PROCS)
 */
int num_keys;

/*****************************************************************/
/* On larger number of processors, since the keys are (roughly)  */ 
/* gaussian distributed, the first and last processor sort keys  */ 
/* in a large interval, requiring array sizes to be larger. Note */
/* that for large NUM_PROCS, NUM_KEYS is, however, a small number*/
/* The required array size also depends on the bucket size used. */
/* The following values are validated for the 1024-bucket setup. */
/*****************************************************************/
/*
 * #if   NUM_PROCS < 256
 * #define  SIZE_OF_BUFFERS     3*NUM_KEYS/2
 * #elif NUM_PROCS < 512
 * #define  SIZE_OF_BUFFERS     5*NUM_KEYS/2
 * #elif NUM_PROCS < 1024
 * #define  SIZE_OF_BUFFERS     4*NUM_KEYS
 * #else
 * #define  SIZE_OF_BUFFERS     13*NUM_KEYS/2
 * #endif
 */
int size_of_buffers;


/***********************************/
/* Enable separate communication,  */
/* computation timing and printout */
/***********************************/
#define  TIMING_ENABLED
#ifdef NO_MTIMERS
#undef TIMINIG_ENABLED
#define TIMER_START( x )
#define TIMER_STOP( x )
#else
#define TIMER_START( x ) if (timeron) timer_start( x )
#define TIMER_STOP( x ) if (timeron) timer_stop( x )
#define T_TOTAL  0
#define T_RANK   1
#define T_RCOMM  2
#define T_VERIFY 3
#define T_LAST   3
#endif
int timeron;


/*************************************/
/* Typedef: if necessary, change the */
/* size of int here by changing the  */
/* int type to, say, long            */
/*************************************/
typedef  int  INT_TYPE;
#if CLASS == 'D' || CLASS == 'E'
typedef  long KEY_TYPE;
#else
typedef  int  KEY_TYPE;
#endif
#define MP_KEY_TYPE MPI_INT



/********************/
/* MPI properties:  */
/********************/
int      my_rank, np_total,
         comm_size;
MPI_Comm comm_work;


/********************/
/* Some global info */
/********************/
INT_TYPE *key_buff_ptr_global,         /* used by full_verify to get */
         total_local_keys,             /* copies of rank info        */
         total_lesser_keys;


int      passed_verification;
                                 


/************************************/
/* These are the three main arrays. */
/* See SIZE_OF_BUFFERS def above    */
/************************************/
INT_TYPE *key_array,    
         *key_buff1,    
         *key_buff2,
         bucket_size[NUM_BUCKETS+TEST_ARRAY_SIZE],     /* Top 5 elements for */
         bucket_size_totals[NUM_BUCKETS+TEST_ARRAY_SIZE], /* part. ver. vals */
         bucket_ptrs[NUM_BUCKETS],
         process_bucket_distrib_ptr1[NUM_BUCKETS+TEST_ARRAY_SIZE],   
         process_bucket_distrib_ptr2[NUM_BUCKETS+TEST_ARRAY_SIZE];   
int      *send_count, *recv_count,
         *send_displ, *recv_displ;


/**********************/
/* Partial verif info */
/**********************/
KEY_TYPE test_index_array[TEST_ARRAY_SIZE],
         test_rank_array[TEST_ARRAY_SIZE];

int      S_test_index_array[TEST_ARRAY_SIZE] = 
                             {48427,17148,23627,62548,4431},
         S_test_rank_array[TEST_ARRAY_SIZE] = 
                             {0,18,346,64917,65463},

         W_test_index_array[TEST_ARRAY_SIZE] = 
                             {357773,934767,875723,898999,404505},
         W_test_rank_array[TEST_ARRAY_SIZE] = 
                             {1249,11698,1039987,1043896,1048018},

         A_test_index_array[TEST_ARRAY_SIZE] = 
                             {2112377,662041,5336171,3642833,4250760},
         A_test_rank_array[TEST_ARRAY_SIZE] = 
                             {104,17523,123928,8288932,8388264},

         B_test_index_array[TEST_ARRAY_SIZE] = 
                             {41869,812306,5102857,18232239,26860214},
         B_test_rank_array[TEST_ARRAY_SIZE] = 
                             {33422937,10244,59149,33135281,99}, 

         C_test_index_array[TEST_ARRAY_SIZE] = 
                             {44172927,72999161,74326391,129606274,21736814},
         C_test_rank_array[TEST_ARRAY_SIZE] = 
                             {61147,882988,266290,133997595,133525895};

long     D_test_index_array[TEST_ARRAY_SIZE] = 
                             {1317351170,995930646,1157283250,1503301535,1453734525},
         D_test_rank_array[TEST_ARRAY_SIZE] = 
                             {1,36538729,1978098519,2145192618,2147425337},

         E_test_index_array[TEST_ARRAY_SIZE] = 
                             {21492309536L,24606226181L,12608530949L,4065943607L,3324513396L},
         E_test_rank_array[TEST_ARRAY_SIZE] = 
                             {3L,27580354L,3248475153L,30048754302L,31485259697L};



/***********************/
/* function prototypes */
/***********************/
double	randlc( double *X, double *A );

void full_verify( void );

void c_print_results( char   *name,
                      char   class,
                      int    n1, 
                      int    n2,
                      int    n3,
                      int    niter,
                      int    nprocs_active,
                      int    nprocs_total,
                      double t,
                      double mops,
		      char   *optype,
                      int    passed_verification,
                      char   *npbversion,
                      char   *compiletime,
                      char   *mpicc,
                      char   *clink,
                      char   *cmpi_lib,
                      char   *cmpi_inc,
                      char   *cflags,
                      char   *clinkflags );

#include "../common/c_timers.h"


/*****************************************************************/
/*     Dynamically allocate space for main arrays                */
/*****************************************************************/
void alloc_space(void)
{
   /* problem size after partition */
   num_keys = (TOTAL_KEYS/comm_size) * MIN_PROCS;

   /* buffer size for communication */
   if ( comm_size < 256 )
      size_of_buffers = 3*num_keys/2;
   else if ( comm_size < 512 )
      size_of_buffers = 5*num_keys/2;
   else if ( comm_size < 1024 )
      size_of_buffers = 4*num_keys;
   else
      size_of_buffers = 13*num_keys/2;

   /* allocate space */
   key_array = (INT_TYPE *)malloc(sizeof(INT_TYPE)*size_of_buffers);
   key_buff1 = (INT_TYPE *)malloc(sizeof(INT_TYPE)*size_of_buffers);
   key_buff2 = (INT_TYPE *)malloc(sizeof(INT_TYPE)*size_of_buffers);

   send_count = (int *)malloc(sizeof(int)*comm_size);
   recv_count = (int *)malloc(sizeof(int)*comm_size);
   send_displ = (int *)malloc(sizeof(int)*comm_size);
   recv_displ = (int *)malloc(sizeof(int)*comm_size);

   if (!key_array || !key_buff1 || !key_buff2 ||
       !send_count || !recv_count || !send_displ || !recv_displ) {
      printf("ERROR: memoy allocation failed\n");
      MPI_Abort(MPI_COMM_WORLD, 1);
      exit(1);
   }
}


/*
 *    FUNCTION RANDLC (X, A)
 *
 *  This routine returns a uniform pseudorandom double precision number in the
 *  range (0, 1) by using the linear congruential generator
 *
 *  x_{k+1} = a x_k  (mod 2^46)
 *
 *  where 0 < x_k < 2^46 and 0 < a < 2^46.  This scheme generates 2^44 numbers
 *  before repeating.  The argument A is the same as 'a' in the above formula,
 *  and X is the same as x_0.  A and X must be odd double precision integers
 *  in the range (1, 2^46).  The returned value RANDLC is normalized to be
 *  between 0 and 1, i.e. RANDLC = 2^(-46) * x_1.  X is updated to contain
 *  the new seed x_1, so that subsequent calls to RANDLC using the same
 *  arguments will generate a continuous sequence.
 *
 *  This routine should produce the same results on any computer with at least
 *  48 mantissa bits in double precision floating point data.  On Cray systems,
 *  double precision should be disabled.
 *
 *  David H. Bailey     October 26, 1990
 *
 *     IMPLICIT DOUBLE PRECISION (A-H, O-Z)
 *     SAVE KS, R23, R46, T23, T46
 *     DATA KS/0/
 *
 *  If this is the first call to RANDLC, compute R23 = 2 ^ -23, R46 = 2 ^ -46,
 *  T23 = 2 ^ 23, and T46 = 2 ^ 46.  These are computed in loops, rather than
 *  by merely using the ** operator, in order to insure that the results are
 *  exact on all systems.  This code assumes that 0.5D0 is represented exactly.
 */


/*****************************************************************/
/*************           R  A  N  D  L  C             ************/
/*************                                        ************/
/*************    portable random number generator    ************/
/*****************************************************************/

double	randlc( double *X, double *A )
{
      static int        KS=0;
      static double	R23, R46, T23, T46;
      double		T1, T2, T3, T4;
      double		A1;
      double		A2;
      double		X1;
      double		X2;
      double		Z;
      int     		i, j;

      if (KS == 0) 
      {
        R23 = 1.0;
        R46 = 1.0;
        T23 = 1.0;
        T46 = 1.0;
    
        for (i=1; i<=23; i++)
        {
          R23 = 0.50 * R23;
          T23 = 2.0 * T23;
        }
        for (i=1; i<=46; i++)
        {
          R46 = 0.50 * R46;
          T46 = 2.0 * T46;
        }
        KS = 1;
      }

/*  Break A into two parts such that A = 2^23 * A1 + A2 and set X = N.  */

      T1 = R23 * *A;
      j  = T1;
      A1 = j;
      A2 = *A - T23 * A1;

/*  Break X into two parts such that X = 2^23 * X1 + X2, compute
    Z = A1 * X2 + A2 * X1  (mod 2^23), and then
    X = 2^23 * Z + A2 * X2  (mod 2^46).                            */

      T1 = R23 * *X;
      j  = T1;
      X1 = j;
      X2 = *X - T23 * X1;
      T1 = A1 * X2 + A2 * X1;
      
      j  = R23 * T1;
      T2 = j;
      Z = T1 - T23 * T2;
      T3 = T23 * Z + A2 * X2;
      j  = R46 * T3;
      T4 = j;
      *X = T3 - T46 * T4;
      return(R46 * *X);
} 



/*****************************************************************/
/************   F  I  N  D  _  M  Y  _  S  E  E  D    ************/
/************                                         ************/
/************ returns parallel random number seq seed ************/
/*****************************************************************/

/*
 * Create a random number sequence of total length nn residing
 * on np number of processors.  Each processor will therefore have a 
 * subsequence of length nn/np.  This routine returns that random 
 * number which is the first random number for the subsequence belonging
 * to processor rank kn, and which is used as seed for proc kn ran # gen.
 */

double   find_my_seed( int  kn,       /* my processor rank, 0<=kn<=num procs */
                       int  np,       /* np = num procs                      */
                       long nn,       /* total num of ran numbers, all procs */
                       double s,      /* Ran num seed, for ex.: 314159265.00 */
                       double a )     /* Ran num gen mult, try 1220703125.00 */
{

  long   i;

  double t1,t2,t3,an;
  long   mq,nq,kk,ik;



      nq = nn / np;

      for( mq=0; nq>1; mq++,nq/=2 )
          ;

      t1 = a;

      for( i=1; i<=mq; i++ )
        t2 = randlc( &t1, &t1 );

      an = t1;

      kk = kn;
      t1 = s;
      t2 = an;

      for( i=1; i<=100; i++ )
      {
        ik = kk / 2;
        if( 2 * ik !=  kk ) 
            t3 = randlc( &t1, &t2 );
        if( ik == 0 ) 
            break;
        t3 = randlc( &t2, &t2 );
        kk = ik;
      }

      return( t1 );

}




/*****************************************************************/
/*************      C  R  E  A  T  E  _  S  E  Q      ************/
/*****************************************************************/

void	create_seq( double seed, double a )
{
	double x;
	int    i, k;

        k = MAX_KEY/4;

	for (i=0; i<num_keys; i++)
	{
	    x = randlc(&seed, &a);
	    x += randlc(&seed, &a);
    	    x += randlc(&seed, &a);
	    x += randlc(&seed, &a);  

            key_array[i] = k*x;
	}
}




/*****************************************************************/
/*************    F  U  L  L  _  V  E  R  I  F  Y     ************/
/*****************************************************************/


void full_verify( void )
{
    MPI_Status  status;
    MPI_Request request;
    
    INT_TYPE    i, j;
    INT_TYPE    k, last_local_key;

    
    TIMER_START( T_VERIFY );

/*  Now, finally, sort the keys:  */
    for( i=0; i<total_local_keys; i++ )
        key_array[--key_buff_ptr_global[key_buff2[i]]-
                                 total_lesser_keys] = key_buff2[i];
    last_local_key = (total_local_keys<1)? 0 : (total_local_keys-1);

/*  Send largest key value to next processor  */
    if( my_rank > 0 )
        MPI_Irecv( &k,
                   1,
                   MP_KEY_TYPE,
                   my_rank-1,
                   1000,
                   comm_work,
                   &request );                   
    if( my_rank < comm_size-1 )
        MPI_Send( &key_array[last_local_key],
                  1,
                  MP_KEY_TYPE,
                  my_rank+1,
                  1000,
                  comm_work );
    if( my_rank > 0 )
        MPI_Wait( &request, &status );

/*  Confirm that neighbor's greatest key value 
    is not greater than my least key value       */              
    j = 0;
    if( my_rank > 0 && total_local_keys > 0 )
        if( k > key_array[0] )
            j++;


/*  Confirm keys correctly sorted: count incorrectly sorted keys, if any */
    for( i=1; i<total_local_keys; i++ )
        if( key_array[i-1] > key_array[i] )
            j++;


    if( j != 0 )
    {
        printf( "Processor %d:  Full_verify: number of keys out of sort: %d\n",
                my_rank, j );
    }
    else
        passed_verification++;
           
    TIMER_STOP( T_VERIFY );

}




/*****************************************************************/
/*************             R  A  N  K             ****************/
/*****************************************************************/


void rank( int iteration )
{

    INT_TYPE    i, k;

    INT_TYPE    shift = MAX_KEY_LOG_2 - NUM_BUCKETS_LOG_2;
    INT_TYPE    key;
    KEY_TYPE    bucket_sum_accumulator, j, m;
    INT_TYPE    local_bucket_sum_accumulator;
    INT_TYPE    min_key_val, max_key_val;
    INT_TYPE    *key_buff_ptr;



    TIMER_START( T_RANK );

/*  Iteration alteration of keys */  
    if(my_rank == 0 )                    
    {
      key_array[iteration] = iteration;
      key_array[iteration+MAX_ITERATIONS] = MAX_KEY - iteration;
    }


/*  Initialize */
    for( i=0; i<NUM_BUCKETS+TEST_ARRAY_SIZE; i++ )  
    {
        bucket_size[i] = 0;
        bucket_size_totals[i] = 0;
        process_bucket_distrib_ptr1[i] = 0;
        process_bucket_distrib_ptr2[i] = 0;
    }


/*  Determine where the partial verify test keys are, load into  */
/*  top of array bucket_size                                     */
    for( i=0; i<TEST_ARRAY_SIZE; i++ )
        if( (test_index_array[i]/num_keys) == my_rank )
            bucket_size[NUM_BUCKETS+i] = 
                          key_array[test_index_array[i] % num_keys];


/*  Determine the number of keys in each bucket */
    for( i=0; i<num_keys; i++ )
        bucket_size[key_array[i] >> shift]++;


/*  Accumulative bucket sizes are the bucket pointers */
    bucket_ptrs[0] = 0;
    for( i=1; i< NUM_BUCKETS; i++ )  
        bucket_ptrs[i] = bucket_ptrs[i-1] + bucket_size[i-1];


/*  Sort into appropriate bucket */
    for( i=0; i<num_keys; i++ )  
    {
        key = key_array[i];
        key_buff1[bucket_ptrs[key >> shift]++] = key;
    }

    TIMER_STOP( T_RANK );
    TIMER_START( T_RCOMM );

/*  Get the bucket size totals for the entire problem. These 
    will be used to determine the redistribution of keys      */
    MPI_Allreduce( bucket_size, 
                   bucket_size_totals, 
                   NUM_BUCKETS+TEST_ARRAY_SIZE, 
                   MP_KEY_TYPE,
                   MPI_SUM,
                   comm_work );

    TIMER_STOP( T_RCOMM );
    TIMER_START( T_RANK );

/*  Determine Redistibution of keys: accumulate the bucket size totals 
    till this number surpasses NUM_KEYS (which the average number of keys
    per processor).  Then all keys in these buckets go to processor 0.
    Continue accumulating again until supassing 2*NUM_KEYS. All keys
    in these buckets go to processor 1, etc.  This algorithm guarantees
    that all processors have work ranking; no processors are left idle.
    The optimum number of buckets, however, does not result in as high
    a degree of load balancing (as even a distribution of keys as is
    possible) as is obtained from increasing the number of buckets, but
    more buckets results in more computation per processor so that the
    optimum number of buckets turns out to be 1024 for machines tested.
    Note that process_bucket_distrib_ptr1 and ..._ptr2 hold the bucket
    number of first and last bucket which each processor will have after   
    the redistribution is done.                                          */

    bucket_sum_accumulator = 0;
    local_bucket_sum_accumulator = 0;
    send_displ[0] = 0;
    process_bucket_distrib_ptr1[0] = 0;
    for( i=0, j=0; i<NUM_BUCKETS; i++ )  
    {
        bucket_sum_accumulator       += bucket_size_totals[i];
        local_bucket_sum_accumulator += bucket_size[i];
        if( bucket_sum_accumulator >= (j+1)*num_keys )  
        {
            send_count[j] = local_bucket_sum_accumulator;
            if( j != 0 )
            {
                send_displ[j] = send_displ[j-1] + send_count[j-1];
                process_bucket_distrib_ptr1[j] = 
                                        process_bucket_distrib_ptr2[j-1]+1;
            }
            process_bucket_distrib_ptr2[j++] = i;
            local_bucket_sum_accumulator = 0;
        }
    }

/*  When NUM_PROCS approaching NUM_BUCKETS, it is highly possible
    that the last few processors don't get any buckets.  So, we
    need to set counts properly in this case to avoid any fallouts.    */
    while( j < comm_size )
    {
        send_count[j] = 0;
        process_bucket_distrib_ptr1[j] = 1;
        j++;
    }

    TIMER_STOP( T_RANK );
    TIMER_START( T_RCOMM ); 

/*  This is the redistribution section:  first find out how many keys
    each processor will send to every other processor:                 */
    MPI_Alltoall( send_count,
                  1,
                  MPI_INT,
                  recv_count,
                  1,
                  MPI_INT,
                  comm_work );

/*  Determine the receive array displacements for the buckets */    
    recv_displ[0] = 0;
    for( i=1; i<comm_size; i++ )
        recv_displ[i] = recv_displ[i-1] + recv_count[i-1];


/*  Now send the keys to respective processors  */    
    MPI_Alltoallv( key_buff1,
                   send_count,
                   send_displ,
                   MP_KEY_TYPE,
                   key_buff2,
                   recv_count,
                   recv_displ,
                   MP_KEY_TYPE,
                   comm_work );

    TIMER_STOP( T_RCOMM ); 
    TIMER_START( T_RANK );

/*  The starting and ending bucket numbers on each processor are
    multiplied by the interval size of the buckets to obtain the 
    smallest possible min and greatest possible max value of any 
    key on each processor                                          */
    min_key_val = process_bucket_distrib_ptr1[my_rank] << shift;
    max_key_val = ((process_bucket_distrib_ptr2[my_rank] + 1) << shift)-1;

/*  Clear the work array */
    for( i=0; i<max_key_val-min_key_val+1; i++ )
        key_buff1[i] = 0;

/*  Determine the total number of keys on all other 
    processors holding keys of lesser value         */
    m = 0;
    for( k=0; k<my_rank; k++ )
        for( i= process_bucket_distrib_ptr1[k];
             i<=process_bucket_distrib_ptr2[k];
             i++ )  
            m += bucket_size_totals[i]; /*  m has total # of lesser keys */

/*  Determine total number of keys on this processor */
    j = 0;                                 
    for( i= process_bucket_distrib_ptr1[my_rank];
         i<=process_bucket_distrib_ptr2[my_rank];
         i++ )  
        j += bucket_size_totals[i];     /* j has total # of local keys   */


/*  Ranking of all keys occurs in this section:                 */
/*  shift it backwards so no subtractions are necessary in loop */
    key_buff_ptr = key_buff1 - min_key_val;

/*  In this section, the keys themselves are used as their 
    own indexes to determine how many of each there are: their
    individual population                                       */
    for( i=0; i<j; i++ )
        key_buff_ptr[key_buff2[i]]++;  /* Now they have individual key   */
                                       /* population                     */

/*  To obtain ranks of each key, successively add the individual key
    population, not forgetting the total of lesser keys, m.
    NOTE: Since the total of lesser keys would be subtracted later 
    in verification, it is no longer added to the first key population 
    here, but still needed during the partial verify test.  This is to 
    ensure that 32-bit key_buff can still be used for class D.           */
/*    key_buff_ptr[min_key_val] += m;    */
    for( i=min_key_val; i<max_key_val; i++ )   
        key_buff_ptr[i+1] += key_buff_ptr[i];  


/* This is the partial verify test section */
/* Observe that test_rank_array vals are   */
/* shifted differently for different cases */
    for( i=0; i<TEST_ARRAY_SIZE; i++ )
    {                                             
        k = bucket_size_totals[i+NUM_BUCKETS];    /* Keys were hidden here */
        if( min_key_val <= k  &&  k <= max_key_val )
        {
            /* Add the total of lesser keys, m, here */
            KEY_TYPE key_rank = key_buff_ptr[k-1] + m;
            KEY_TYPE test_rank = test_rank_array[i];
            int failed = 0;

            switch( CLASS )
            {
                case 'S':
                    if( i <= 2 )
                        test_rank += iteration;
                    else
                        test_rank -= iteration;
                    break;
                case 'W':
                    if( i < 2 )
                        test_rank += iteration - 2;
                    else
                        test_rank -= iteration;
                    break;
                case 'A':
                    if( i <= 2 )
                        test_rank += iteration - 1;
                    else
                        test_rank -= iteration - 1;
                    break;
                case 'B':
                    if( i == 1 || i == 2 || i == 4 )
                        test_rank += iteration;
                    else
                        test_rank -= iteration;
                    break;
                case 'C':
                    if( i <= 2 )
                        test_rank += iteration;
                    else
                        test_rank -= iteration;
                    break;
                case 'D':
                    if( i < 2 )
                        test_rank += iteration;
                    else
                        test_rank -= iteration;
                    break;
                 case 'E':
                    if( i < 2 )
                        test_rank += iteration - 2;
                    else if( i == 2 )
                    {
                        test_rank += iteration - 2;
                        if (iteration > 4)
                            test_rank -= 2;
                        else if (iteration > 2)
                            test_rank -= 1;
                    }
                    else
                        test_rank -= iteration - 2;
                    break;
            }
            if( key_rank != test_rank )
                failed = 1;
            else
                passed_verification++;
            if( failed == 1 )
                printf( "Failed partial verification: "
                        "iteration %d, processor %d, test key %d, key rank %ld\n", 
                         iteration, my_rank, (int)i, (long)key_rank );
        }
    }


    TIMER_STOP( T_RANK ); 


/*  Make copies of rank info for use by full_verify: these variables
    in rank are local; making them global slows down the code, probably
    since they cannot be made register by compiler                        */

    if( iteration == MAX_ITERATIONS ) 
    {
        key_buff_ptr_global = key_buff_ptr;
        total_local_keys    = j;
        total_lesser_keys   = 0;  /* no longer set to 'm', see note above */
    }

}      


/*****************************************************************/
/*************             M  A  I  N             ****************/
/*****************************************************************/

int main( int argc, char **argv )
{

    int             i, iteration, itemp, active;

    double          timecounter, maxtime;


/*  Initialize MPI */
    MPI_Init( &argc, &argv );
    MPI_Comm_rank( MPI_COMM_WORLD, &my_rank );
    MPI_Comm_size( MPI_COMM_WORLD, &np_total );


/*  Check to see whether total number of processes is within bounds.
    This could in principle be checked in setparams.c, but it is more
    convenient to do it here                                               */
    if( np_total < MIN_PROCS || np_total > MAX_PROCS)
    {
       if( my_rank == 0 )
           printf( "\n ERROR: number of processes %d not within range %d-%d"
                   "\n Exiting program!\n\n", np_total, MIN_PROCS, MAX_PROCS);
       MPI_Finalize();
       exit( 1 );
    }


/*  comm_size needs to be power of two */
    for (comm_size = 1; comm_size < np_total; comm_size *= 2);
    if (comm_size > np_total) comm_size /= 2;

/*  If the actual number of processes doesn't agree with comm_size,
    check if excess ranks need to be masked */
    active = 1;
    if( comm_size != np_total )
    {
        /* check if NPB_NPROCS_STRICT is set */
        if( my_rank == 0 ) {
            char *ep = getenv("NPB_NPROCS_STRICT");
            if (ep && *ep) {
               if (strchr("nNfF-", *ep) || strcmp(ep, "0") == 0)
                  active = 0;
               else if (strcmp(ep, "off") == 0 || strcmp(ep, "OFF") == 0)
                  active = 0;
            }
        }
        MPI_Bcast(&active, 1, MPI_INT, 0, MPI_COMM_WORLD);

        /* abort if a strict NPROCS enforcement is required */
        if (active) {
            if( my_rank == 0 )
               printf( "\n ERROR: Number of processes (%d)"
                       " is not a power of two (%d?)\n"
                       " Exiting program!\n\n", np_total, comm_size );
            MPI_Abort(MPI_COMM_WORLD, MPI_ERR_OTHER);
            exit( 1 );
        }

        /* mark excess ranks as inactive */
        active = ( my_rank >= comm_size )? 0 : 1;
        MPI_Comm_split(MPI_COMM_WORLD, active, my_rank, &comm_work);
    }
    else
        MPI_Comm_dup(MPI_COMM_WORLD, &comm_work);

    if (!active) {
        MPI_Finalize();
        exit( 0 );
    }


/*  Initialize the verification arrays if a valid class */
    for( i=0; i<TEST_ARRAY_SIZE; i++ )
        switch( CLASS )
        {
            case 'S':
                test_index_array[i] = S_test_index_array[i];
                test_rank_array[i]  = S_test_rank_array[i];
                break;
            case 'A':
                test_index_array[i] = A_test_index_array[i];
                test_rank_array[i]  = A_test_rank_array[i];
                break;
            case 'W':
                test_index_array[i] = W_test_index_array[i];
                test_rank_array[i]  = W_test_rank_array[i];
                break;
            case 'B':
                test_index_array[i] = B_test_index_array[i];
                test_rank_array[i]  = B_test_rank_array[i];
                break;
            case 'C':
                test_index_array[i] = C_test_index_array[i];
                test_rank_array[i]  = C_test_rank_array[i];
                break;
            case 'D':
                test_index_array[i] = D_test_index_array[i];
                test_rank_array[i]  = D_test_rank_array[i];
                break;
            case 'E':
                test_index_array[i] = E_test_index_array[i];
                test_rank_array[i]  = E_test_rank_array[i];
                break;
        };
        

/*  Printout initial NPB info */
    if( my_rank == 0 )
    {
        printf( "\n\n NAS Parallel Benchmarks 3.4 -- IS Benchmark\n\n" );
        printf( " Size:  %ld  (class %c)\n", (long)TOTAL_KEYS*MIN_PROCS, CLASS );
        printf( " Iterations:   %d\n", MAX_ITERATIONS );
        printf( " Total number of processes:  %d\n", np_total );
        if ( comm_size != np_total )
            printf( " WARNING: Number of processes"
                    " is not a power of two (%d active)\n", comm_size );

        timeron = check_timer_flag();
    }

    MPI_Bcast(&timeron, 1, MPI_INT, 0, comm_work);

#ifdef  TIMING_ENABLED 
    for( i=1; i<=T_LAST; i++ ) timer_clear( i );
#endif

/*  allocate space for work arrays */
    alloc_space();

/*  Generate random number sequence and subsequent keys on all procs */
    create_seq( find_my_seed( my_rank, 
                              comm_size, 
                              4*(long)TOTAL_KEYS*MIN_PROCS,
                              314159265.00,      /* Random number gen seed */
                              1220703125.00 ),   /* Random number gen mult */
                1220703125.00 );                 /* Random number gen mult */


/*  Do one interation for free (i.e., untimed) to guarantee initialization of  
    all data and code pages and respective tables */
    rank( 1 );  

/*  Start verification counter */
    passed_verification = 0;

    if( my_rank == 0 && CLASS != 'S' ) printf( "\n   iteration\n" );

/*  Initialize timer  */             
    timer_clear( 0 );

/*  Initialize separate communication, computation timing */
#ifdef  TIMING_ENABLED 
    for( i=1; i<=T_LAST; i++ ) timer_clear( i );
#endif

/*  Start timer  */             
    timer_start( 0 );


/*  This is the main iteration */
    for( iteration=1; iteration<=MAX_ITERATIONS; iteration++ )
    {
        if( my_rank == 0 && CLASS != 'S' ) printf( "        %d\n", iteration );
        rank( iteration );
    }


/*  Stop timer, obtain time for processors */
    timer_stop( 0 );

    timecounter = timer_read( 0 );

/*  End of timing, obtain maximum time of all processors */
    MPI_Reduce( &timecounter,
                &maxtime,
                1,
                MPI_DOUBLE,
                MPI_MAX,
                0,
                comm_work );


/*  This tests that keys are in sequence: sorting of last ranked key seq
    occurs here, but is an untimed operation                             */
    full_verify();


/*  Obtain verification counter sum */
    itemp = passed_verification;
    MPI_Reduce( &itemp,
                &passed_verification,
                1,
                MPI_INT,
                MPI_SUM,
                0,
                comm_work );



/*  The final printout  */
    if( my_rank == 0 )
    {
        if( passed_verification != 5*MAX_ITERATIONS + comm_size )
            passed_verification = 0;
        c_print_results( "IS",
                         CLASS,
                         (int)(TOTAL_KEYS),
                         MIN_PROCS,
                         0,
                         MAX_ITERATIONS,
                         comm_size,
                         np_total,
                         maxtime,
                         ((double) (MAX_ITERATIONS)*TOTAL_KEYS*MIN_PROCS)
                                                      /maxtime/1000000.,
                         "keys ranked", 
                         passed_verification,
                         NPBVERSION,
                         COMPILETIME,
                         MPICC,
                         CLINK,
                         CMPI_LIB,
                         CMPI_INC,
                         CFLAGS,
                         CLINKFLAGS );
    }
                    

#ifdef  TIMING_ENABLED
    if (timeron)
    {
        double    t1[T_LAST+1], tmin[T_LAST+1], tsum[T_LAST+1], tmax[T_LAST+1];
        char      t_recs[T_LAST+1][9];
    
        for( i=0; i<=T_LAST; i++ )
            t1[i] = timer_read( i );

        MPI_Reduce( t1,
                    tmin,
                    T_LAST+1,
                    MPI_DOUBLE,
                    MPI_MIN,
                    0,
                    comm_work );
        MPI_Reduce( t1,
                    tsum,
                    T_LAST+1,
                    MPI_DOUBLE,
                    MPI_SUM,
                    0,
                    comm_work );
        MPI_Reduce( t1,
                    tmax,
                    T_LAST+1,
                    MPI_DOUBLE,
                    MPI_MAX,
                    0,
                    comm_work );

        if( my_rank == 0 )
        {
            strcpy( t_recs[T_TOTAL],  "total" );
            strcpy( t_recs[T_RANK],   "rcomp" );
            strcpy( t_recs[T_RCOMM],  "rcomm" );
            strcpy( t_recs[T_VERIFY], "verify");
            printf( " nprocs = %6d     ", comm_size);
            printf( "     minimum     maximum     average\n" );
            for( i=0; i<=T_LAST; i++ )
            {
                printf( " timer %2d (%-8s):  %10.4f  %10.4f  %10.4f\n",
                        i+1, t_recs[i], tmin[i], tmax[i], 
                        tsum[i]/((double) comm_size) );
            }
            printf( "\n" );
        }
    }
#endif

    MPI_Finalize();


    return 0;
         /**************************/
}        /*  E N D  P R O G R A M  */
         /**************************/
