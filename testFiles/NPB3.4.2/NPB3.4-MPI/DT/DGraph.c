#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "DGraph.h"

DGArc *newArc(DGNode *tl,DGNode *hd){
  DGArc *ar=(DGArc *)malloc(sizeof(DGArc));
  ar->tail=tl;
  ar->head=hd;
  return ar;
}
void arcShow(DGArc *ar){
  DGNode *tl=(DGNode *)ar->tail,
         *hd=(DGNode *)ar->head;
  fprintf(stderr,"%d. |%s ->%s\n",ar->id,tl->name,hd->name);
}

DGNode *newNode(char *nm){
  DGNode *nd=(DGNode *)malloc(sizeof(DGNode));
  nd->attribute=0;
  nd->color=0;
  nd->inDegree=0;
  nd->outDegree=0;
  nd->maxInDegree=SMALL_BLOCK_SIZE;
  nd->maxOutDegree=SMALL_BLOCK_SIZE;
  nd->inArc=(DGArc **)malloc(nd->maxInDegree*sizeof(DGArc*));
  nd->outArc=(DGArc **)malloc(nd->maxOutDegree*sizeof(DGArc*));
  nd->name=strdup(nm);
  nd->feat=NULL;
  return nd;
}
void nodeShow(DGNode* nd){
  fprintf( stderr,"%3d.%s: (%d,%d)\n",
	           nd->id,nd->name,nd->inDegree,nd->outDegree);
/*
  if(nd->verified==1) fprintf(stderr,"%ld.%s\t: usable.",nd->id,nd->name);
  else if(nd->verified==0)  fprintf(stderr,"%ld.%s\t: unusable.",nd->id,nd->name);
  else  fprintf(stderr,"%ld.%s\t: notverified.",nd->id,nd->name);   
*/
}

DGraph* newDGraph(char* nm){
  DGraph *dg=(DGraph *)malloc(sizeof(DGraph));
  dg->numNodes=0;
  dg->numArcs=0;
  dg->maxNodes=BLOCK_SIZE;
  dg->maxArcs=BLOCK_SIZE;
  dg->node=(DGNode **)malloc(dg->maxNodes*sizeof(DGNode*));
  dg->arc=(DGArc **)malloc(dg->maxArcs*sizeof(DGArc*));
  dg->name=strdup(nm);
  return dg;
}
int AttachNode(DGraph* dg, DGNode* nd) {
  int i=0,j,len=0;
  DGNode **nds =NULL, *tmpnd=NULL;
  DGArc **ar=NULL;

	if (dg->numNodes == dg->maxNodes-1 ) {
	  dg->maxNodes += BLOCK_SIZE;
          nds =(DGNode **) calloc(dg->maxNodes,sizeof(DGNode*));
	  memcpy(nds,dg->node,(dg->maxNodes-BLOCK_SIZE)*sizeof(DGNode*));
	  free(dg->node);
	  dg->node=nds;
	}

        len = strlen( nd->name);
	for (i = 0; i < dg->numNodes; i++) {
	  tmpnd =dg->node[ i];
	  ar=NULL;
	  if ( strlen( tmpnd->name) != len ) continue;
	  if ( strncmp( nd->name, tmpnd->name, len) ) continue;
	  if ( nd->inDegree > 0 ) {
	    tmpnd->maxInDegree += nd->maxInDegree;
            ar =(DGArc **) calloc(tmpnd->maxInDegree,sizeof(DGArc*));
	    memcpy(ar,tmpnd->inArc,(tmpnd->inDegree)*sizeof(DGArc*));
	    free(tmpnd->inArc);
	    tmpnd->inArc=ar;
	    for (j = 0; j < nd->inDegree; j++ ) {
	      nd->inArc[ j]->head = tmpnd;
	    }
	    memcpy( &(tmpnd->inArc[ tmpnd->inDegree]), nd->inArc, nd->inDegree*sizeof( DGArc *));
	    tmpnd->inDegree += nd->inDegree;
	  } 	
	  if ( nd->outDegree > 0 ) {
	    tmpnd->maxOutDegree += nd->maxOutDegree;
            ar =(DGArc **) calloc(tmpnd->maxOutDegree,sizeof(DGArc*));
	    memcpy(ar,tmpnd->outArc,(tmpnd->outDegree)*sizeof(DGArc*));
	    free(tmpnd->outArc);
	    tmpnd->outArc=ar;
	    for (j = 0; j < nd->outDegree; j++ ) {
	      nd->outArc[ j]->tail = tmpnd;
	    }			
	    memcpy( &(tmpnd->outArc[tmpnd->outDegree]),nd->outArc,nd->outDegree*sizeof( DGArc *));
	    tmpnd->outDegree += nd->outDegree;
	  } 
	  free(nd); 
	  return i;
	}
	nd->id = dg->numNodes;
	dg->node[dg->numNodes] = nd;
	dg->numNodes++;
return nd->id;
}
int AttachArc(DGraph *dg,DGArc* nar){
int	arcId = -1;
int i=0,newNumber=0;
DGNode	*head = nar->head,
	*tail = nar->tail; 
DGArc **ars=NULL,*probe=NULL;
/*fprintf(stderr,"AttachArc %ld\n",dg->numArcs); */
	if ( !tail || !head ) return arcId;
	if ( dg->numArcs == dg->maxArcs-1 ) {
	  dg->maxArcs += BLOCK_SIZE;
          ars =(DGArc **) calloc(dg->maxArcs,sizeof(DGArc*));
	  memcpy(ars,dg->arc,(dg->maxArcs-BLOCK_SIZE)*sizeof(DGArc*));
	  free(dg->arc);
	  dg->arc=ars;
	}
	for(i = 0; i < tail->outDegree; i++ ) { /* parallel arc */
	  probe = tail->outArc[ i];
	  if(probe->head == head
	     &&
	     probe->length == nar->length
            ){
            free(nar);
	    return probe->id;   
	  }
	}
	
	nar->id = dg->numArcs;
	arcId=dg->numArcs;
	dg->arc[dg->numArcs] = nar;
	dg->numArcs++;
	
	head->inArc[ head->inDegree] = nar;
	head->inDegree++;
	if ( head->inDegree >= head->maxInDegree ) {
	  newNumber = head->maxInDegree + SMALL_BLOCK_SIZE;
          ars =(DGArc **) calloc(newNumber,sizeof(DGArc*));
	  memcpy(ars,head->inArc,(head->inDegree)*sizeof(DGArc*));
	  free(head->inArc);
	  head->inArc=ars;
	  head->maxInDegree = newNumber;
	}
	tail->outArc[ tail->outDegree] = nar;
	tail->outDegree++;
	if(tail->outDegree >= tail->maxOutDegree ) {
	  newNumber = tail->maxOutDegree + SMALL_BLOCK_SIZE;
          ars =(DGArc **) calloc(newNumber,sizeof(DGArc*));
	  memcpy(ars,tail->outArc,(tail->outDegree)*sizeof(DGArc*));
	  free(tail->outArc);
	  tail->outArc=ars;
	  tail->maxOutDegree = newNumber;
	}
/*fprintf(stderr,"AttachArc: head->in=%d tail->out=%ld\n",head->inDegree,tail->outDegree);*/
return arcId;
}
void graphShow(DGraph *dg,int DetailsLevel){
  int i=0,j=0;
  fprintf(stderr," %d.%s: (%d,%d)\n",dg->id,dg->name,dg->numNodes,dg->numArcs);
  if ( DetailsLevel < 1) return;
  for (i = 0; i < dg->numNodes; i++ ) {
    DGNode *focusNode = dg->node[ i];
    if(DetailsLevel >= 2) {
      for (j = 0; j < focusNode->inDegree; j++ ) {
	fprintf(stderr,"\t ");
	nodeShow(focusNode->inArc[ j]->tail);
      }
    }
    nodeShow(focusNode);
    if ( DetailsLevel < 2) continue;
    for (j = 0; j < focusNode->outDegree; j++ ) {
      fprintf(stderr, "\t ");
      nodeShow(focusNode->outArc[ j]->head);
    }	
    fprintf(stderr, "---\n");
  }
  fprintf(stderr,"----------------------------------------\n");
  if ( DetailsLevel < 3) return;
}



