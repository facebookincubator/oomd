#if !adc_h
#define adc_h 1

/* For checking of L2-cache performance influence */ 
/*#define IN_CORE_*/
/*#define VIEW_FILE_OUTPUT*/ /* it can be used with IN_CORE only */

/* Optimizations: prefixed views and share-sorted views */
/*#define OPTIMIZATION*/

#ifdef WINNT
#ifndef HAS_INT64
typedef __int64             int64;
typedef int                 int32;
#endif
typedef unsigned __int64   uint64;
typedef unsigned int       uint32;
#else
#ifndef HAS_INT64
typedef long long           int64;
typedef int                 int32;
#endif
typedef unsigned long long uint64;
typedef unsigned int       uint32;
#endif

#include "adcc.h"
#include "rbt.h"

static int measbound=31415;   /* upper limit on a view measre bound */

enum { smallestParent, prefixedParent, sharedSortParent, noneParent };

static const char* adcKeyword[]={
  "attrNum",
  "measuresNum",
  "tuplesNum",
  "INVERSE_ENDIAN",
  "fileName",
  "class",
  NULL
};

typedef struct ADCpar{
  int ndid;
  int dim;
  int mnum;
  long long int tuplenum;
  int inverse_endian;
  const char *filename;
  char clss;
} ADC_PAR;

typedef struct {
    int32 ndid;
   char   clss;
   char          adcName[MAX_FILE_FULL_PATH_SIZE];
   char   adcInpFileName[MAX_FILE_FULL_PATH_SIZE];
   uint32 nd; 
   uint32 nm;
   uint32 nInputRecs;
   uint32 memoryLimit;
   uint32 nTasks;
   /*  FILE *statf; */
} ADC_VIEW_PARS;

typedef struct job_pool{ 
   uint32 grpb; 
   uint32 nv;
   uint32 nRows; 
    int64 viewOffset; 
} JOB_POOL;

typedef struct layer{
   uint32 layerIndex;
   uint32 layerQuantityLimit;
   uint32 layerCurrentPopulation;
} LAYER;

typedef struct chunks{
   uint32 curChunkNum;
    int64 chunkOffset;
   uint32 posSubChunk;
   uint32 curSubChunk;
} CHUNKS;

typedef struct tuplevsize {
    uint64 viewsize;
    uint64 tuple;
} TUPLE_VIEWSIZE;

typedef struct tupleones {
    uint32 nOnes;
    uint64 tuple;
} TUPLE_ONES;

typedef struct {
   char adcName[MAX_FILE_FULL_PATH_SIZE];
   uint32 retCode;
   uint32 verificationFailed;
   uint32 swapIt;
   uint32 nTasks;
   uint32 taskNumber;
    int32 ndid;

   uint32 nTopDims; /* given number of dimension attributes */
   uint32 nm;       /* number of measures */ 
   uint32 nd;       /* number of parent's dimensions */
   uint32 nv;       /* number of child's dimensions */

   uint32 nInputRecs;
   uint32 nViewRows; 
   uint32 totalOfViewRows;
   uint32 nParentViewRows;

    int64 viewOffset;
    int64 accViewFileOffset;

   uint32 inpRecSize;
   uint32 outRecSize;

   uint32 memoryLimit;
 unsigned char * memPool;
   uint32 * inpDataBuffer;

   RBTree *tree;

   uint32 numberOfChunks;
   CHUNKS *chunksParams;

     char       adcLogFileName[MAX_FILE_FULL_PATH_SIZE];
     char          inpFileName[MAX_FILE_FULL_PATH_SIZE];
     char         viewFileName[MAX_FILE_FULL_PATH_SIZE];
     char       chunksFileName[MAX_FILE_FULL_PATH_SIZE];
     char      groupbyFileName[MAX_FILE_FULL_PATH_SIZE];
     char adcViewSizesFileName[MAX_FILE_FULL_PATH_SIZE];
     char    viewSizesFileName[MAX_FILE_FULL_PATH_SIZE];

     FILE *logf;
     FILE *inpf;
     FILE *viewFile;   
     FILE *fileOfChunks;
     FILE *groupbyFile;
     FILE *adcViewSizesFile;
     FILE *viewSizesFile;
   
    int64     mSums[MAX_NUM_OF_MEAS];
   uint32 selection[MAX_NUM_OF_DIMS];
    int64 checksums[MAX_NUM_OF_MEAS]; /* view checksums */
    int64 totchs[MAX_NUM_OF_MEAS];    /* checksums of a group of views */

 JOB_POOL *jpp;
    LAYER *lpp;
   uint32 nViewLimit;
   uint32 groupby;
   uint32 smallestParentLevel;
   uint32 parBinRepTuple;
   uint32 nRowsToRead;
   uint32 fromParent;

   uint64 totalViewFileSize; /* in bytes */
   uint32 numberOfMadeViews;
   uint32 numberOfViewsMadeFromInput;
   uint32 numberOfPrefixedGroupbys;
   uint32 numberOfSharedSortGroupbys;
} ADC_VIEW_CNTL;
#endif /* adc_h */
