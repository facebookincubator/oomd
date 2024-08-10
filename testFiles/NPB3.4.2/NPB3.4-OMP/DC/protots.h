 int32 ReadWholeInputData(ADC_VIEW_CNTL *avp, FILE *inpf);
 
 int32 ComputeMemoryFittedView (ADC_VIEW_CNTL *avp);

 int32 MultiWayMerge(ADC_VIEW_CNTL *avp);

 int32 GetPrefixedParent(ADC_VIEW_CNTL *avp, uint32 binRepTuple);

 int32 WriteChunkToDisk(
       uint32     recordSize, 
       FILE      *fileOfChunks, 
       treeNode  *t, 
       FILE      *logFile);

 int32 DeleteOneFile(const char * file_name);

  void WriteOne64Tuple(char * t, uint64 s, uint32 l, FILE * logf);

 int32 ViewSizesVerification(ADC_VIEW_CNTL *adccntlp);

  void CreateBinTuple(
       uint64  *binRepTuple, 
       uint32  *selTuple, 
       uint32   numDims);

  void AdcCntlLog(ADC_VIEW_CNTL *adccntlp);

  void swap8(void *a);

  void WriteOne32Tuple(char * t, uint32 s, uint32 l, FILE * logf);

  void JobPoolUpdate(ADC_VIEW_CNTL *avp);

 int32 WriteViewToDisk(ADC_VIEW_CNTL *avp, treeNode *t);

uint32 GetSmallestParent(ADC_VIEW_CNTL *avp, uint32 binRepTuple);

 int32 GetParent(ADC_VIEW_CNTL *avp, uint32 binRepTuple);

  void GetRegTupleFromBin64(
       uint64   binRepTuple, 
       uint32  *selTuple, 
       uint32   numDims, 
       uint32  *numOfUnits); 

  void GetRegTupleFromParent(
       uint64   bin64RepTuple,
       uint32   bin32RepTuple,
       uint32  *selTuple,
       uint32   nd);

  void JobPoolInit(JOB_POOL *jpp, uint32 n, uint32 nd);

uint32 NumOfCombsFromNbyK (uint32 n, uint32 k);

  void InitializeTree(RBTree *tree, uint32 nd, uint32 nm);

 int32 CheckTree(
       treeNode  *t , 
       uint32    *px, 
       uint32     nv, 
       uint32     nm, 
       FILE      *logFile);

 int32 KeyComp(const uint32 *a, const uint32 *b, uint32 n);

 int32 TreeInsert(RBTree *tree, uint32 *attrs);

  void InitializeTree(RBTree *tree, uint32 nd, uint32 nm);

 int32 WriteChunkToDisk(
       uint32     recordSize, 
       FILE      *fileOfChunks, 
       treeNode  *t, 
       FILE      *logFile);

  void SelectToView(
       uint32  *ib, 
       uint32  *ix, 
       uint32  *viewBuf, 
       uint32   nd, 
       uint32   nm, 
       uint32   nv);

 int32 MultiWayBufferSnap(
       uint32   nv, 
       uint32   nm,  
       uint32  *multiChunkBuffer, 
       uint32	numberOfChunks, 
       uint32	regSubChunkSize, 
       uint32	nRecords);

 RBTree *CreateEmptyTree(
       uint32          nd, 
       uint32          nm, 
       uint32          memoryLimit, 
       unsigned char  *memPool);

int32 PrefixedAggregate(ADC_VIEW_CNTL *avp, FILE *iof);

