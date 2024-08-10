/*************************************************************************
 *                                                                       * 
 *        N  A  S     P A R A L L E L     B E N C H M A R K S  3.4       *
 *                                                                       * 
 *                                  D T					 * 
 *                                                                       * 
 ************************************************************************* 
 *                                                                       * 
 *   This benchmark is part of the NAS Parallel Benchmark 3.4 suite.     *
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
 *          http:  www.nas.nasa.gov/Software/NPB                         * 
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
 *   Author: M. Frumkin							 *						 * 
 *                                                                       * 
 *************************************************************************/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "mpi.h"
#include "npbparams.h"

#ifndef CLASS
#define CLASS 'S'
#endif

int      passed_verification;
extern double randlc( double *X, double *A );
extern
void c_print_results( char   *name,
                      char   class,
                      int    n1, 
                      int    n2,
                      int    n3,
                      int    niter,
                      int    nprocs_compiled,
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
int timer_on=0,timers_tot=64;

int verify(char *bmname,double rnm2){
    double verify_value=0.0;
    double epsilon=1.0E-8;
    char cls=CLASS;
    int verified=-1;
    if (cls != 'U') {
       if(cls=='S') {
         if(strstr(bmname,"BH")){
           verify_value=30892725.0;
         }else if(strstr(bmname,"WH")){
           verify_value=67349758.0;
         }else if(strstr(bmname,"SH")){
           verify_value=58875767.0;
         }else{
           fprintf(stderr,"No such benchmark as %s.\n",bmname);
         }
         verified = 0;
       }else if(cls=='W') {
         if(strstr(bmname,"BH")){
  	   verify_value = 4102461.0;
         }else if(strstr(bmname,"WH")){
  	   verify_value = 204280762.0;
         }else if(strstr(bmname,"SH")){
  	   verify_value = 186944764.0;
         }else{
           fprintf(stderr,"No such benchmark as %s.\n",bmname);
         }
         verified = 0;
       }else if(cls=='A') {
         if(strstr(bmname,"BH")){
  	   verify_value = 17809491.0;
         }else if(strstr(bmname,"WH")){
  	   verify_value = 1289925229.0;
         }else if(strstr(bmname,"SH")){
  	   verify_value = 610856482.0;
         }else{
           fprintf(stderr,"No such benchmark as %s.\n",bmname);
         }
  	 verified = 0;
       }else if(cls=='B') {
         if(strstr(bmname,"BH")){
  	   verify_value = 4317114.0;
         }else if(strstr(bmname,"WH")){
  	   verify_value = 7877279917.0;
         }else if(strstr(bmname,"SH")){
  	   verify_value = 1836863082.0;
         }else{
           fprintf(stderr,"No such benchmark as %s.\n",bmname);
  	   verified = 0;
         }
       }else if(cls=='C') {
         if(strstr(bmname,"BH")){
  	   verify_value = 0.0;
         }else if(strstr(bmname,"WH")){
  	   verify_value = 0.0;
         }else if(strstr(bmname,"SH")){
  	   verify_value = 0.0;
         }else{
           fprintf(stderr,"No such benchmark as %s.\n",bmname);
  	   verified = -1;
         }
       }else if(cls=='D') {
         if(strstr(bmname,"BH")){
  	   verify_value = 0.0;
         }else if(strstr(bmname,"WH")){
  	   verify_value = 0.0;
         }else if(strstr(bmname,"SH")){
  	   verify_value = 0.0;
         }else{
           fprintf(stderr,"No such benchmark as %s.\n",bmname);
         }
         verified = -1;
       }else{
         fprintf(stderr,"No such class as %c.\n",cls);
       }
       fprintf(stderr," %s L2 Norm = %f\n",bmname,rnm2);
       if(verified==-1){
  	 fprintf(stderr," No verification was performed.\n");
       }else if( rnm2 - verify_value < epsilon &&
                 rnm2 - verify_value > -epsilon) {  /* abs here does not work on ALTIX */
  	  verified = 1;
  	  fprintf(stderr," Deviation = %f\n",(rnm2 - verify_value));
       }else{
  	 verified = 0;
  	 fprintf(stderr," The correct verification value = %f\n",verify_value);
  	 fprintf(stderr," Got value = %f\n",rnm2);
       }
    }else{
       verified = -1;
    }
    return  verified;  
  }

int ipowMod(int a,long long int n,int md){ 
  int seed=1,q=a,r=1;
  if(n<0){
    fprintf(stderr,"ipowMod: exponent must be nonnegative exp=%lld\n",n);
    n=-n; /* temp fix */
/*    return 1; */
  }
  if(md<=0){
    fprintf(stderr,"ipowMod: module must be positive mod=%d",md);
    return 1;
  }
  if(n==0) return 1;
  while(n>1){
    int n2 = n/2;
    if (n2*2==n){
       seed = (q*q)%md;
       q=seed;
       n = n2;
    }else{
       seed = (r*q)%md;
       r=seed;
       n = n-1;
    }
  }
  seed = (r*q)%md;
  return seed;
}

#include "DGraph.h"
DGraph *buildSH(char cls){
/*
  Nodes of the graph must be topologically sorted
  to avoid MPI deadlock.
*/
  DGraph *dg;
  int numSources=NUM_SOURCES; /* must be power of 2 */
  int numOfLayers=0,tmpS=numSources>>1;
  int firstLayerNode=0;
  DGArc *ar=NULL;
  DGNode *nd=NULL;
  int mask=0x0,ndid=0,ndoff=0;
  int i=0,j=0;
  char nm[BLOCK_SIZE];
  
  sprintf(nm,"DT_SH.%c",cls);
  dg=newDGraph(nm);

  while(tmpS>1){
    numOfLayers++;
    tmpS>>=1;
  }
  for(i=0;i<numSources;i++){
    sprintf(nm,"Source.%d",i);
    nd=newNode(nm);
    AttachNode(dg,nd);
  }
  for(j=0;j<numOfLayers;j++){
    mask=0x00000001<<j;
    for(i=0;i<numSources;i++){
      sprintf(nm,"Comparator.%d",(i+j*firstLayerNode));
      nd=newNode(nm);
      AttachNode(dg,nd);
      ndoff=i&(~mask);
      ndid=firstLayerNode+ndoff;
      ar=newArc(dg->node[ndid],nd);     
      AttachArc(dg,ar);
      ndoff+=mask;
      ndid=firstLayerNode+ndoff;
      ar=newArc(dg->node[ndid],nd);     
      AttachArc(dg,ar);
    }
    firstLayerNode+=numSources;
  }
  mask=0x00000001<<numOfLayers;
  for(i=0;i<numSources;i++){
    sprintf(nm,"Sink.%d",i);
    nd=newNode(nm);
    AttachNode(dg,nd);
    ndoff=i&(~mask);
    ndid=firstLayerNode+ndoff;
    ar=newArc(dg->node[ndid],nd);     
    AttachArc(dg,ar);
    ndoff+=mask;
    ndid=firstLayerNode+ndoff;
    ar=newArc(dg->node[ndid],nd);     
    AttachArc(dg,ar);
  }
return dg;
}
DGraph *buildWH(char cls){
/*
  Nodes of the graph must be topologically sorted
  to avoid MPI deadlock.
*/
  int i=0,j=0;
  int numSources=NUM_SOURCES,maxInDeg=4;
  int numLayerNodes=numSources,firstLayerNode=0;
  int totComparators=0;
  int numPrevLayerNodes=numLayerNodes;
  int id=0,sid=0;
  DGraph *dg;
  DGNode *nd=NULL,*source=NULL,*tmp=NULL,*snd=NULL;
  DGArc *ar=NULL;
  char nm[BLOCK_SIZE];

  sprintf(nm,"DT_WH.%c",cls);
  dg=newDGraph(nm);

  for(i=0;i<numSources;i++){
    sprintf(nm,"Sink.%d",i);
    nd=newNode(nm);
    AttachNode(dg,nd);
  }
  totComparators=0;
  numPrevLayerNodes=numLayerNodes;
  while(numLayerNodes>maxInDeg){
    numLayerNodes=numLayerNodes/maxInDeg;
    if(numLayerNodes*maxInDeg<numPrevLayerNodes)numLayerNodes++;
    for(i=0;i<numLayerNodes;i++){
      sprintf(nm,"Comparator.%d",totComparators);
      totComparators++;
      nd=newNode(nm);
      id=AttachNode(dg,nd);
      for(j=0;j<maxInDeg;j++){
        sid=i*maxInDeg+j;
	if(sid>=numPrevLayerNodes) break;
        snd=dg->node[firstLayerNode+sid];
        ar=newArc(dg->node[id],snd);
        AttachArc(dg,ar);
      }
    }
    firstLayerNode+=numPrevLayerNodes;
    numPrevLayerNodes=numLayerNodes;
  }
  source=newNode("Source");
  AttachNode(dg,source);   
  for(i=0;i<numPrevLayerNodes;i++){
    nd=dg->node[firstLayerNode+i];
    ar=newArc(source,nd);
    AttachArc(dg,ar);
  }

  for(i=0;i<dg->numNodes/2;i++){  /* Topological sorting */
    tmp=dg->node[i];
    dg->node[i]=dg->node[dg->numNodes-1-i];
    dg->node[i]->id=i;
    dg->node[dg->numNodes-1-i]=tmp;
    dg->node[dg->numNodes-1-i]->id=dg->numNodes-1-i;
  }
return dg;
}
DGraph *buildBH(char cls){
/*
  Nodes of the graph must be topologically sorted
  to avoid MPI deadlock.
*/
  int i=0,j=0;
  int numSources=NUM_SOURCES,maxInDeg=4;
  int numLayerNodes=numSources,firstLayerNode=0;
  DGraph *dg;
  DGNode *nd=NULL, *snd=NULL, *sink=NULL;
  DGArc *ar=NULL;
  int totComparators=0;
  int numPrevLayerNodes=numLayerNodes;
  int id=0, sid=0;
  char nm[BLOCK_SIZE];

  sprintf(nm,"DT_BH.%c",cls);
  dg=newDGraph(nm);

  for(i=0;i<numSources;i++){
    sprintf(nm,"Source.%d",i);
    nd=newNode(nm);
    AttachNode(dg,nd);
  }
  while(numLayerNodes>maxInDeg){
    numLayerNodes=numLayerNodes/maxInDeg;
    if(numLayerNodes*maxInDeg<numPrevLayerNodes)numLayerNodes++;
    for(i=0;i<numLayerNodes;i++){
      sprintf(nm,"Comparator.%d",totComparators);
      totComparators++;
      nd=newNode(nm);
      id=AttachNode(dg,nd);
      for(j=0;j<maxInDeg;j++){
        sid=i*maxInDeg+j;
	if(sid>=numPrevLayerNodes) break;
        snd=dg->node[firstLayerNode+sid];
        ar=newArc(snd,dg->node[id]);
        AttachArc(dg,ar);
      }
    }
    firstLayerNode+=numPrevLayerNodes;
    numPrevLayerNodes=numLayerNodes;
  }
  sink=newNode("Sink");
  AttachNode(dg,sink);   
  for(i=0;i<numPrevLayerNodes;i++){
    nd=dg->node[firstLayerNode+i];
    ar=newArc(nd,sink);
    AttachArc(dg,ar);
  }
return dg;
}

typedef struct{
  int len;
  double* val;
} Arr;
Arr *newArr(int len){
  Arr *arr=(Arr *)malloc(sizeof(Arr));
  arr->len=len;
  arr->val=(double *)malloc(len*sizeof(double));
  return arr;
}
void arrShow(Arr* a){
  if(!a) fprintf(stderr,"-- NULL array\n");
  else{
    fprintf(stderr,"-- length=%d\n",a->len);
  }
}
double CheckVal(Arr *feat){
  double csum=0.0;
  int i=0;
  for(i=0;i<feat->len;i++){
    csum+=feat->val[i]*feat->val[i]/feat->len; /* The truncation does not work since 
                                                  result will be 0 for large len  */
  }
   return csum;
}
int GetFNumDPar(int* mean, int* stdev){
  *mean=NUM_SAMPLES;
  *stdev=STD_DEVIATION;
  return 0;
}
int GetFeatureNum(char *mbname,int id){
  double tran=314159265.0;
  double A=2*id+1;
  double denom=randlc(&tran,&A);
  char cval='S';
  int mean=NUM_SAMPLES,stdev=128;
  int rtfs=0,len=0;
  GetFNumDPar(&mean,&stdev);
  rtfs=ipowMod((int)(1/denom)*(int)cval,(long long int) (2*id+1),2*stdev);
  if(rtfs<0) rtfs=-rtfs;
  len=mean-stdev+rtfs;
  return len;
}
Arr* RandomFeatures(char *bmname,int fdim,int id){
  int len=GetFeatureNum(bmname,id)*fdim;
  Arr* feat=newArr(len);
  int nxg=2,nyg=2,nzg=2,nfg=5;
  int nx=421,ny=419,nz=1427,nf=3527;
  long long int expon=(len*(id+1))%3141592;
  int seedx=ipowMod(nxg,expon,nx),
      seedy=ipowMod(nyg,expon,ny),
      seedz=ipowMod(nzg,expon,nz),
      seedf=ipowMod(nfg,expon,nf);
  int i=0;
  if(timer_on){
    timer_clear(id+1);
    timer_start(id+1);
  }
  for(i=0;i<len;i+=fdim){
    seedx=(seedx*nxg)%nx;
    seedy=(seedy*nyg)%ny;
    seedz=(seedz*nzg)%nz;
    seedf=(seedf*nfg)%nf;
    feat->val[i]=seedx;
    feat->val[i+1]=seedy;
    feat->val[i+2]=seedz;
    feat->val[i+3]=seedf;
  }
  if(timer_on){
    timer_stop(id+1);
    fprintf(stderr,"** RandomFeatures time in node %d = %f\n",id,timer_read(id+1));
  }
  return feat;   
}
void Resample(Arr *a,int blen){
    long long int i=0,j=0,jlo=0,jhi=0;
    double avval=0.0;
    double *nval=(double *)malloc(blen*sizeof(double));
    Arr *tmp=newArr(10);
    for(i=0;i<blen;i++) nval[i]=0.0;
    for(i=1;i<a->len-1;i++){
      jlo=(int)(0.5*(2*i-1)*(blen/a->len)); 
      jhi=(int)(0.5*(2*i+1)*(blen/a->len));

      avval=a->val[i]/(jhi-jlo+1);    
      for(j=jlo;j<=jhi;j++){
        nval[j]+=avval;
      }
    }
    nval[0]=a->val[0];
    nval[blen-1]=a->val[a->len-1];
    free(a->val);
    a->val=nval;
    a->len=blen;
}
#define fielddim 4
Arr* WindowFilter(Arr *a, Arr* b,int w){
  int i=0,j=0,k=0;
  double rms0=0.0,rms1=0.0,rmsm1=0.0;
  double weight=((double) (w+1))/(w+2);
 
  w+=1;
  if(timer_on){
    timer_clear(w);
    timer_start(w);
  }
  if(a->len<b->len) Resample(a,b->len);
  if(a->len>b->len) Resample(b,a->len);
  for(i=fielddim;i<a->len-fielddim;i+=fielddim){
    rms0=(a->val[i]-b->val[i])*(a->val[i]-b->val[i])
	+(a->val[i+1]-b->val[i+1])*(a->val[i+1]-b->val[i+1])
	+(a->val[i+2]-b->val[i+2])*(a->val[i+2]-b->val[i+2])
	+(a->val[i+3]-b->val[i+3])*(a->val[i+3]-b->val[i+3]);
    j=i+fielddim;
    rms1=(a->val[j]-b->val[j])*(a->val[j]-b->val[j])
    	+(a->val[j+1]-b->val[j+1])*(a->val[j+1]-b->val[j+1])
    	+(a->val[j+2]-b->val[j+2])*(a->val[j+2]-b->val[j+2])
    	+(a->val[j+3]-b->val[j+3])*(a->val[j+3]-b->val[j+3]);
    j=i-fielddim;
    rmsm1=(a->val[j]-b->val[j])*(a->val[j]-b->val[j])
	 +(a->val[j+1]-b->val[j+1])*(a->val[j+1]-b->val[j+1])
	 +(a->val[j+2]-b->val[j+2])*(a->val[j+2]-b->val[j+2])
	 +(a->val[j+3]-b->val[j+3])*(a->val[j+3]-b->val[j+3]);
    k=0;
    if(rms1<rms0){
      k=1;
      rms0=rms1;
    }
    if(rmsm1<rms0) k=-1;
    if(k==0){
      j=i+fielddim;
      a->val[i]=weight*b->val[i];
      a->val[i+1]=weight*b->val[i+1];
      a->val[i+2]=weight*b->val[i+2];
      a->val[i+3]=weight*b->val[i+3];  
    }else if(k==1){
      j=i+fielddim;
      a->val[i]=weight*b->val[j];
      a->val[i+1]=weight*b->val[j+1];
      a->val[i+2]=weight*b->val[j+2];
      a->val[i+3]=weight*b->val[j+3];  
    }else { /*if(k==-1)*/
      j=i-fielddim;
      a->val[i]=weight*b->val[j];
      a->val[i+1]=weight*b->val[j+1];
      a->val[i+2]=weight*b->val[j+2];
      a->val[i+3]=weight*b->val[j+3];  
    }	   
  }
  if(timer_on){
    timer_stop(w);
    fprintf(stderr,"** WindowFilter time in node %d = %f\n",(w-1),timer_read(w));
  }
  return a;
}

int SendResults(DGraph *dg,DGNode *nd,Arr *feat){
  int i=0,tag=0;
  DGArc *ar=NULL;
  DGNode *head=NULL;
  if(!feat) return 0;
  for(i=0;i<nd->outDegree;i++){
    ar=nd->outArc[i];
    if(ar->tail!=nd) continue;
    head=ar->head;
    tag=ar->id;
    if(head->address!=nd->address){
      MPI_Send(&feat->len,1,MPI_INT,head->address,tag,MPI_COMM_WORLD);
      MPI_Send(feat->val,feat->len,MPI_DOUBLE,head->address,tag,MPI_COMM_WORLD);
    }
  }
  return 1;
}
Arr* CombineStreams(DGraph *dg,DGNode *nd){
  Arr *resfeat=newArr(NUM_SAMPLES*fielddim);
  int i=0,len=0,tag=0;
  DGArc *ar=NULL;
  DGNode *tail=NULL;
  MPI_Status status;
  Arr *feat=NULL,*featp=NULL;

  if(nd->inDegree==0) return NULL;
  for(i=0;i<nd->inDegree;i++){
    ar=nd->inArc[i];
    if(ar->head!=nd) continue;
    tail=ar->tail;
    if(tail->address!=nd->address){
      len=0;
      tag=ar->id;
      MPI_Recv(&len,1,MPI_INT,tail->address,tag,MPI_COMM_WORLD,&status);
      feat=newArr(len);
      MPI_Recv(feat->val,feat->len,MPI_DOUBLE,tail->address,tag,MPI_COMM_WORLD,&status);
      resfeat=WindowFilter(resfeat,feat,nd->id);
      free(feat);
    }else{
      featp=(Arr *)tail->feat;
      feat=newArr(featp->len);
      memcpy(feat->val,featp->val,featp->len*sizeof(double));
      resfeat=WindowFilter(resfeat,feat,nd->id);  
      free(feat);
    }
  }
  for(i=0;i<resfeat->len;i++) resfeat->val[i]=((int)resfeat->val[i])/nd->inDegree;
  nd->feat=resfeat;
  return nd->feat;
}
double Reduce(Arr *a,int w){
  double retv=0.0;
  if(timer_on){
    timer_clear(w);
    timer_start(w);
  }
  retv=(int)(w*CheckVal(a));/* The casting needed for node  
                               and array dependent verifcation */
  if(timer_on){
    timer_stop(w);
    fprintf(stderr,"** Reduce time in node %d = %f\n",(w-1),timer_read(w));
  }
  return retv;
}

double ReduceStreams(DGraph *dg,DGNode *nd){
  double csum=0.0;
  int i=0,len=0,tag=0;
  DGArc *ar=NULL;
  DGNode *tail=NULL;
  Arr *feat=NULL;
  double retv=0.0;

  for(i=0;i<nd->inDegree;i++){
    ar=nd->inArc[i];
    if(ar->head!=nd) continue;
    tail=ar->tail;
    if(tail->address!=nd->address){
      MPI_Status status;
      len=0;
      tag=ar->id;
      MPI_Recv(&len,1,MPI_INT,tail->address,tag,MPI_COMM_WORLD,&status);
      feat=newArr(len);
      MPI_Recv(feat->val,feat->len,MPI_DOUBLE,tail->address,tag,MPI_COMM_WORLD,&status);
      csum+=Reduce(feat,(nd->id+1));  
      free(feat);
    }else{
      csum+=Reduce(tail->feat,(nd->id+1));  
    }
  }
  if(nd->inDegree>0)csum=(((long long int)csum)/nd->inDegree);
  retv=(nd->id+1)*csum;
  return retv;
}

int ProcessNodes(DGraph *dg,int me){
  double chksum=0.0;
  Arr *feat=NULL;
  int i=0,verified=0,tag;
  DGNode *nd=NULL;
  double rchksum=0.0;
  MPI_Status status;

  for(i=0;i<dg->numNodes;i++){
    nd=dg->node[i];
    if(nd->address!=me) continue;
    if(strstr(nd->name,"Source")){
      nd->feat=RandomFeatures(dg->name,fielddim,nd->id); 
      SendResults(dg,nd,nd->feat);
    }else if(strstr(nd->name,"Sink")){
      chksum=ReduceStreams(dg,nd);
      tag=dg->numArcs+nd->id; /* make these to avoid clash with arc tags */
      MPI_Send(&chksum,1,MPI_DOUBLE,0,tag,MPI_COMM_WORLD);
    }else{
      feat=CombineStreams(dg,nd);
      SendResults(dg,nd,feat);
    }
  }
  if(me==0){ /* Report node */
    rchksum=0.0;
    chksum=0.0;
    for(i=0;i<dg->numNodes;i++){
      nd=dg->node[i];
      if(!strstr(nd->name,"Sink")) continue;
       tag=dg->numArcs+nd->id; /* make these to avoid clash with arc tags */
      MPI_Recv(&rchksum,1,MPI_DOUBLE,nd->address,tag,MPI_COMM_WORLD,&status);
      chksum+=rchksum;
    }
    verified=verify(dg->name,chksum);
  }
return verified;
}

int main(int argc,char **argv ){
  int my_rank,comm_size;
  int i;
  DGraph *dg=NULL;
  int verified=0, featnum=0;
  double bytes_sent=2.0,tot_time=0.0;

    MPI_Init( &argc, &argv );
    MPI_Comm_rank( MPI_COMM_WORLD, &my_rank );
    MPI_Comm_size( MPI_COMM_WORLD, &comm_size );

     if(argc!=2||
                (  strncmp(argv[1],"BH",2)!=0
                 &&strncmp(argv[1],"WH",2)!=0
                 &&strncmp(argv[1],"SH",2)!=0
                )
      ){
      if(my_rank==0){
        fprintf(stderr,"** Usage: mpirun -np N ../bin/dt.S GraphName\n");
        fprintf(stderr,"** Where \n   - N is integer number of MPI processes\n");
        fprintf(stderr,"   - S is the class S, W, or A \n");
        fprintf(stderr,"   - GraphName is the communication graph name BH, WH, or SH.\n");
        fprintf(stderr,"   - the number of MPI processes N should not be be less than \n");
        fprintf(stderr,"     the number of nodes in the graph\n");
      }
      MPI_Finalize();
      exit(1);
    } 
   if(strncmp(argv[1],"BH",2)==0){
      dg=buildBH(CLASS);
    }else if(strncmp(argv[1],"WH",2)==0){
      dg=buildWH(CLASS);
    }else if(strncmp(argv[1],"SH",2)==0){
      dg=buildSH(CLASS);
    }

    if(timer_on&&dg->numNodes+1>timers_tot){
      timer_on=0;
      if(my_rank==0)
        fprintf(stderr,"Not enough timers. Node timeing is off. \n");
    }
    if(dg->numNodes>comm_size){
      if(my_rank==0){
        fprintf(stderr,"**  The number of MPI processes should not be less than \n");
        fprintf(stderr,"**  the number of nodes in the graph\n");
        fprintf(stderr,"**  Number of MPI processes = %d\n",comm_size);
        fprintf(stderr,"**  Number nodes in the graph = %d\n",dg->numNodes);
      }
      MPI_Finalize();
      exit(1);
    }
    for(i=0;i<dg->numNodes;i++){ 
      dg->node[i]->address=i;
    }
    if( my_rank == 0 ){
      printf( "\n\n NAS Parallel Benchmarks 3.4 -- DT Benchmark\n\n" );
      graphShow(dg,0);
      timer_clear(0);
      timer_start(0);
    }
    verified=ProcessNodes(dg,my_rank);
    
    featnum=NUM_SAMPLES*fielddim;
    bytes_sent=featnum*dg->numArcs;
    bytes_sent/=1048576;
    if(my_rank==0){
      timer_stop(0);
      tot_time=timer_read(0);
      c_print_results( dg->name,
        	       CLASS,
        	       featnum,
        	       0,
        	       0,
        	       dg->numNodes,
        	       0,
        	       comm_size,
        	       tot_time,
        	       bytes_sent/tot_time,
        	       "bytes transmitted", 
        	       verified,
        	       NPBVERSION,
        	       COMPILETIME,
        	       MPICC,
        	       CLINK,
        	       CMPI_LIB,
        	       CMPI_INC,
        	       CFLAGS,
        	       CLINKFLAGS );
    }          
    MPI_Finalize();
  return 0;
}
