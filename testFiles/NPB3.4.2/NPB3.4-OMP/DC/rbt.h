#ifndef _ADC_PARVIEW_TREE_DEF_H_
#define _ADC_PARVIEW_TREE_DEF_H_

#define MAX_TREE_HEIGHT	64
enum{BLACK,RED};

typedef struct treeNode{
  struct treeNode *left;
  struct treeNode *right;
  uint32 clr;
  int64 nodeMemPool[1];
} treeNode;

typedef struct RBTree{
  treeNode root;	
  treeNode * mp;
  uint32 count;       
  uint32 treeNodeSize;
  uint32 nodeDataSize;
  uint32 memoryLimit; 
  uint32 memaddr;
  uint32 memoryIsFull;
  uint32 freeNodeCounter;
  uint32 nNodesLimit;
  uint32 nd;
  uint32 nm;
  uint32   *drcts;
  treeNode **nodes;
  unsigned char * memPool;
} RBTree;

#define NEW_TREE_NODE(node_ptr,memPool,memaddr,treeNodeSize, \
 freeNodeCounter,memoryIsFull) \
 node_ptr=(struct treeNode*)(memPool+memaddr); \
 memaddr+=treeNodeSize; \
 (freeNodeCounter)--; \
 if( freeNodeCounter == 0 ) { \
     memoryIsFull = 1; \
 }

int32 TreeInsert(RBTree *tree, uint32 *attrs);

#endif /* _ADC_PARVIEW_TREE_DEF_H_ */
