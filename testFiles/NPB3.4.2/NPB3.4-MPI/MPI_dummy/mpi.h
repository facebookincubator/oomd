#define MPI_DOUBLE          1
#define MPI_INT             2
#define MPI_BYTE            3
#define MPI_FLOAT           4
#define MPI_LONG            5

#define MPI_COMM_WORLD      0

#define MPI_MAX             1
#define MPI_SUM             2
#define MPI_MIN             3

#define MPI_SUCCESS         0
#define MPI_ANY_SOURCE     -1
#define MPI_ERR_OTHER      -1
#define MPI_STATUS_SIZE     3


/* 
   Status object.  It is the only user-visible MPI data-structure 
   The "count" field is PRIVATE; use MPI_Get_count to access it. 
 */
typedef struct { 
    int count;
    int MPI_SOURCE;
    int MPI_TAG;
    int MPI_ERROR;
} MPI_Status;


/* MPI request objects */
typedef int MPI_Request;

/* MPI datatype */
typedef int MPI_Datatype;

/* MPI comm */
typedef int MPI_Comm;

/* MPI operation */
typedef int MPI_Op;



/* Prototypes: */
void  mpi_error( void );

int MPI_Abort( MPI_Comm comm, int ecode );

int   MPI_Irecv( void         *buf,
                 int          count,
                 MPI_Datatype datatype,
                 int          source,
                 int          tag,
                 MPI_Comm     comm,
                 MPI_Request  *request );

int   MPI_Send( void         *buf,
                int          count,
                MPI_Datatype datatype,
                int          dest,
                int          tag,
                MPI_Comm     comm );

int   MPI_Recv( void         *buf,
                int          count,
                MPI_Datatype datatype,
                int          source,
                int          tag,
                MPI_Comm     comm,
                MPI_Status   *status );

int   MPI_Wait( MPI_Request *request,
                MPI_Status  *status );

int   MPI_Init( int  *argc,
                char ***argv );

int   MPI_Comm_rank( MPI_Comm comm, 
                     int      *rank );

int   MPI_Comm_size( MPI_Comm comm, 
                     int      *size );

int   MPI_Comm_split( MPI_Comm comm, int color, int key, MPI_Comm *newcomm );

int   MPI_Comm_dup( MPI_Comm comm, MPI_Comm *newcomm );

double MPI_Wtime( void );

int  MPI_Barrier( MPI_Comm comm );

int  MPI_Bcast( void         *buf,
                int          count,
                MPI_Datatype datatype,
                int          root,
                MPI_Comm     comm );

int  MPI_Finalize( void );

int  MPI_Allreduce( void         *sendbuf,
                    void         *recvbuf,
                    int          nitems,
                    MPI_Datatype type,
                    MPI_Op       op,
                    MPI_Comm     comm );

int  MPI_Reduce( void         *sendbuf,
                 void         *recvbuf,
                 int          nitems,
                 MPI_Datatype type,
                 MPI_Op       op,
                 int          root,
                 MPI_Comm     comm );

int  MPI_Alltoall( void         *sendbuf,
                   int          sendcount,
                   MPI_Datatype sendtype,
                   void         *recvbuf,
                   int          recvcount,
                   MPI_Datatype recvtype,
                   MPI_Comm     comm );

int  MPI_Alltoallv( void         *sendbuf,
                    int          *sendcounts,
                    int          *senddispl,
                    MPI_Datatype sendtype,
                    void         *recvbuf,
                    int          *recvcounts,
                    int          *recvdispl,
                    MPI_Datatype recvtype,
                    MPI_Comm     comm );
