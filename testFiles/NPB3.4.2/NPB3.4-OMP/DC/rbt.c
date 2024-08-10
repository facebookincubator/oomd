#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "adc.h"
#include "macrodef.h"

int32 KeyComp( const uint32 *a, const uint32 *b, uint32 n ) {
  uint32 i;
  for ( i = 0; i < n; i++ ) {
    if (a[i] < b[i]) return(-1);
    else if (a[i] > b[i]) return(1);
  }
  return(0);
}
int32 TreeInsert(RBTree *tree, uint32 *attrs){
   uint32  sl = 1;			    	
   uint32 *attrsP;
    int32  cmpres;
 treeNode *xNd, *yNd, *tmp;

  tmp = &tree->root;
  xNd = tmp->left;

  if (xNd == NULL){
    tree->count++;
    NEW_TREE_NODE(tree->mp,tree->memPool,
        	      tree->memaddr,tree->treeNodeSize,
        	      tree->freeNodeCounter,tree->memoryIsFull)
    xNd = tmp->left = tree->mp;
    memcpy(&(xNd->nodeMemPool[0]), &attrs[0], tree->nodeDataSize);
    xNd->left = xNd->right = NULL;
    xNd->clr = BLACK;
    return 0;
  }

  tree->drcts[0] = 0;
  tree->nodes[0] = &tree->root;

  while(1){
    attrsP = (uint32*) &(xNd->nodeMemPool[tree->nm]);
    cmpres = KeyComp( &attrs[tree->nm<<1], attrsP, tree->nd );

    if (cmpres < 0){
      tree->nodes[sl] = xNd;
      tree->drcts[sl++] = 0;
      yNd = xNd->left;

      if(yNd == NULL){
	    NEW_TREE_NODE(tree->mp,tree->memPool,
	  	              tree->memaddr,tree->treeNodeSize,
	  	              tree->freeNodeCounter,tree->memoryIsFull)
        xNd = xNd->left = tree->mp;
        break;
      }
    }else if (cmpres > 0){
      tree->nodes[sl] = xNd;
      tree->drcts[sl++] = 1;
      yNd = xNd->right;
      if(yNd == NULL){
        NEW_TREE_NODE(tree->mp,tree->memPool,
		              tree->memaddr,tree->treeNodeSize,
		              tree->freeNodeCounter,tree->memoryIsFull)
        xNd = xNd->right = tree->mp; 
        break;
      }
    }else{  
      uint64 ii; 
      int64 *mx;
      mx = (int64*) &attrs[0];
      for ( ii = 0; ii < tree->nm; ii++ ) xNd->nodeMemPool[ii] += mx[ii];
      return 0; 
    }
    xNd = yNd;
  }
  tree->count++;
  memcpy(&(xNd->nodeMemPool[0]), &attrs[0], tree->nodeDataSize);
  xNd->left = xNd->right = NULL;
  xNd->clr  = RED;

  while(1){
    if ( tree->nodes[sl-1]->clr != RED || sl<3 ) break;
      
    if (tree->drcts[sl-2] == 0){
      yNd = tree->nodes[sl-2]->right;
      if (yNd != NULL && yNd->clr == RED){
        tree->nodes[sl-1]->clr = BLACK;
        yNd->clr = BLACK;
        tree->nodes[sl-2]->clr = RED;
        sl -= 2;
      }else{
        if (tree->drcts[sl-1] == 1){
	      xNd = tree->nodes[sl-1];
	      yNd = xNd->right;
	      xNd->right = yNd->left;
	      yNd->left  = xNd;
	      tree->nodes[sl-2]->left = yNd;
        }else
          yNd = tree->nodes[sl-1];
	  
        xNd = tree->nodes[sl-2];
        xNd->clr = RED;
        yNd->clr = BLACK;

        xNd->left  = yNd->right;
        yNd->right = xNd;

        if(tree->drcts[sl-3])
          tree->nodes[sl-3]->right = yNd;
	    else  
          tree->nodes[sl-3]->left = yNd;
        break;
      }
    }else{
      yNd = tree->nodes[sl-2]->left;
      if (yNd != NULL && yNd->clr == RED){
         tree->nodes[sl-1]->clr = BLACK;
         yNd->clr = BLACK;
         tree->nodes[sl-2]->clr = RED;
         sl -= 2;
      }else{
    	if(tree->drcts[sl-1] == 0){
          xNd = tree->nodes[sl-1];
          yNd = xNd->left;
          xNd->left  = yNd->right;
          yNd->right = xNd;
          tree->nodes[sl-2]->right = yNd;
   	    }else
          yNd = tree->nodes[sl-1];

   	    xNd = tree->nodes[sl-2];
     	xNd->clr = RED;
    	yNd->clr = BLACK;

    	xNd->right = yNd->left;
    	yNd->left  = xNd;

   	    if (tree->drcts[sl-3])
   	      tree->nodes[sl-3]->right = yNd;
     	else  
   	      tree->nodes[sl-3]->left  = yNd;
   	    break;
      }
    }
  }
  tree->root.left->clr = BLACK;
  return 0;
}
int32 WriteViewToDisk(ADC_VIEW_CNTL *avp, treeNode *t){
  uint32 i;
  if(!t) return ADC_OK;
  if(WriteViewToDisk( avp, t->left)) return ADC_WRITE_FAILED;
  for(i=0;i<avp->nm;i++){
    avp->mSums[i] += t->nodeMemPool[i];  
  }	   
  WriteToFile(t->nodeMemPool,avp->outRecSize,1,avp->viewFile,avp->logf);
  if(WriteViewToDisk( avp, t->right)) return ADC_WRITE_FAILED;
  return ADC_OK;
}
int32 WriteViewToDiskCS(ADC_VIEW_CNTL *avp, treeNode *t,uint64 *ordern){
  uint32 i;
  if(!t) return ADC_OK;
  if(WriteViewToDiskCS( avp, t->left,ordern)) return ADC_WRITE_FAILED;
  for(i=0;i<avp->nm;i++){
    avp->mSums[i] += t->nodeMemPool[i];  
    avp->checksums[i] += (++(*ordern))*t->nodeMemPool[i]%measbound;
  }	   
  WriteToFile(t->nodeMemPool,avp->outRecSize,1,avp->viewFile,avp->logf);
  if(WriteViewToDiskCS( avp, t->right,ordern)) return ADC_WRITE_FAILED;
  return ADC_OK;
}
int32 computeChecksum(ADC_VIEW_CNTL *avp, treeNode *t,uint64 *ordern){
  uint32 i;
  if(!t) return ADC_OK;
  if(computeChecksum(avp,t->left,ordern)) return ADC_WRITE_FAILED;
  for(i=0;i<avp->nm;i++){
    avp->checksums[i] += (++(*ordern))*t->nodeMemPool[i]%measbound;
  }	   
  if(computeChecksum(avp,t->right,ordern)) return ADC_WRITE_FAILED;
  return ADC_OK;
}
int32 WriteChunkToDisk(uint32 recordSize,FILE *fileOfChunks,
		       treeNode *t, FILE *logFile){   
  if(!t) return ADC_OK;
  if(WriteChunkToDisk( recordSize, fileOfChunks, t->left, logFile)) 
    return ADC_WRITE_FAILED; 
  WriteToFile( t->nodeMemPool, recordSize, 1, fileOfChunks, logFile);
  if(WriteChunkToDisk( recordSize, fileOfChunks, t->right, logFile)) 
    return ADC_WRITE_FAILED;
  return ADC_OK;
}
RBTree * CreateEmptyTree(uint32 nd, uint32 nm, 
                         uint32 memoryLimit, unsigned char * memPool){
  RBTree *tree = (RBTree*)  malloc(sizeof(RBTree));
  if (!tree) return NULL;

  tree->root.left = NULL;    
  tree->root.right = NULL;     
  tree->count = 0;
  tree->memaddr = 0;
  tree->treeNodeSize = sizeof(struct treeNode) + DIM_FSZ*(nd-1)+MSR_FSZ*nm;
  if (tree->treeNodeSize%8 != 0) tree->treeNodeSize += 4;
  tree->memoryLimit = memoryLimit;
  tree->memoryIsFull = 0;
  tree->nodeDataSize = DIM_FSZ*nd + MSR_FSZ*nm;
  tree->mp = NULL;
  tree->nNodesLimit = tree->memoryLimit/tree->treeNodeSize;
  tree->freeNodeCounter = tree->nNodesLimit;
  tree->nd = nd;
  tree->nm = nm;
  tree->memPool = memPool;
  tree->nodes = (treeNode**) malloc(sizeof(treeNode*)*MAX_TREE_HEIGHT);
  if (!(tree->nodes)) return NULL;
  tree->drcts = (uint32*) malloc( sizeof(uint32)*MAX_TREE_HEIGHT);
  if (!(tree->drcts)) return NULL;
  return tree;
}
void InitializeTree(RBTree *tree, uint32 nd, uint32 nm){
  tree->root.left = NULL;    
  tree->root.right = NULL;     
  tree->count = 0;
  tree->memaddr = 0;
  tree->treeNodeSize = sizeof(struct treeNode) + DIM_FSZ*(nd-1)+MSR_FSZ*nm;
  if (tree->treeNodeSize%8 != 0) tree->treeNodeSize += 4;
  tree->memoryIsFull = 0;
  tree->nodeDataSize = DIM_FSZ*nd + MSR_FSZ*nm;
  tree->mp = NULL;
  tree->nNodesLimit = tree->memoryLimit/tree->treeNodeSize;
  tree->freeNodeCounter = tree->nNodesLimit;
  tree->nd = nd;
  tree->nm = nm;
}
int32 DestroyTree(RBTree *tree) {
  if (tree==NULL) return ADC_TREE_DESTROY_FAILURE;
  if (tree->memPool!=NULL) free(tree->memPool);
  if (tree->nodes) free(tree->nodes);
  if (tree->drcts) free(tree->drcts);
  free(tree);
  return ADC_OK;
}

