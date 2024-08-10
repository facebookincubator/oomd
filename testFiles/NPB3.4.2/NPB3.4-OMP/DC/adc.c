#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "adc.h"

#define BlockSize 1024

void swap4(void * num){
  char t, *p;
  p = (char *) num;
  t = *p; *p = *(p + 3); *(p + 3) = t;
  t = *(p + 1); *(p + 1) = *(p + 2); *(p + 2) = t;
}
void swap8(void * num){
  char t, *p;
  p = (char *) num;	  
  t = *p; *p = *(p + 7); *(p + 7) = t;
  t = *(p + 1); *(p + 1) = *(p + 6); *(p + 6) = t;
  t = *(p + 2); *(p + 2) = *(p + 5); *(p + 5) = t;
  t = *(p + 3); *(p + 3) = *(p + 4); *(p + 4) = t;
}
void initADCpar(ADC_PAR *par){
  par->ndid=0;
  par->dim=5;
  par->mnum=1;
  par->tuplenum=100;
/*  par->isascii=1; */
  par->inverse_endian=0;
  par->filename="ADC";
  par->clss='U';
}
int ParseParFile(char* parfname,ADC_PAR *par);
int GenerateADC(ADC_PAR *par);

typedef struct Factorization{
  long int *mlt;
  long int *exp;
  long int dim;
} Factorization;

void ShowFactorization(Factorization *nmbfct){
  int i=0;
  for(i=0;i<nmbfct->dim;i++){
    if(nmbfct->mlt[i]==1){
      if(i==0) fprintf(stdout,"prime.");
      break;
    }
    if(i>0) fprintf(stdout,"*");
    if(nmbfct->exp[i]==1)
      fprintf(stdout,"%ld",nmbfct->mlt[i]);    
    else 
      fprintf(stdout,"%ld^%ld",nmbfct->mlt[i],
                               nmbfct->exp[i]);
  }
  fprintf(stdout,"\n");
}

long int adcprime[]={
  421,601,631,701,883,
  419,443,647,21737,31769,
  1427,18353,22817,34337,98717,
  3527,8693,9677,11093,18233};
  
long int ListFirstPrimes(long int mpr,long int *prlist){
/*
  fprintf(stdout,"ListFirstPrimes: listing primes less than %ld...\n",
                 mpr);
*/
  long int prnum=0;
  int composed=0;
  long int nmb=0,j=0;
  prlist[prnum++]=2;
  prlist[prnum++]=3;
  prlist[prnum++]=5;
  prlist[prnum++]=7;
  for(nmb=8;nmb<mpr;nmb++){
    composed=0;
    for(j=0;prlist[j]*prlist[j]<=nmb;j++){
      if(nmb-prlist[j]*((long int)(nmb/prlist[j]))==0){
        composed=1;
	break;
      }
    }
    if(composed==0) prlist[prnum++]=nmb;
  }
/*  fprintf(stdout,"ListFirstPrimes: Done.\n"); */
  return prnum;
}

long long int LARGE_NUM=0x4FFFFFFFFFFFFFFFLL;
long long int maxprmfctr=59;

long long int GetLCM(long long int mask,
                     Factorization **fctlist,
		     long int *adcexpons){
  int i=0,j=0,k=0;
  int* expons=(int*) calloc(maxprmfctr+1,sizeof(int));
  long long int LCM=1;
  long int pr=2;
  int genexp=1,lexp=1,fct=2;

  for(i=0;i<maxprmfctr+1;i++)expons[i]=0;
  i=0;
  while(mask>0){
    if(mask==2*(mask/2)){
      mask=mask>>1;
      i++;  
      continue;
    }
    pr=adcprime[i];
    genexp=adcexpons[i];
/*
  fprintf(stdout,"[%ld,%ld]\n",pr,genexp);
  ShowFactorization(fctlist[genexp]);
*/
    for(j=0;j<fctlist[pr-1]->dim;j++){
      fct=fctlist[pr-1]->mlt[j];
      lexp=fctlist[pr-1]->exp[j];

      for(k=0;k<fctlist[genexp]->dim;k++){
        if(fctlist[genexp]->mlt[k]==1) break;
        if(fct!=fctlist[genexp]->mlt[k]) continue;
        lexp-=fctlist[genexp]->exp[k];
	break;
      }
      if(expons[fct]<lexp)expons[fct]=lexp;
    }
    mask=mask>>1;
    i++;
  }
/*
for(i=0;i<maxprmfctr;i++){
  if(expons[i]>0) fprintf(stdout,"*%ld^%ld",i,expons[i]);
}
fprintf(stdout,"\n");
*/
  for(i=0;i<=maxprmfctr;i++){
    while(expons[i]>0){
      LCM*=i;
      if(LCM>LARGE_NUM/maxprmfctr) return LCM;
      expons[i]--;
    }
  }
/*  fprintf(stdout,"==== %lld\n",LCM); */
  free(expons);
  return LCM;
}
void ExtendFactors(long int nmb,long int firstdiv,
                   Factorization *nmbfct,Factorization **fctlist){
  Factorization *divfct=fctlist[nmb/firstdiv];
  int fdivused=0;
  int multnum=0;
  int i=0;
/*  fprintf(stdout,"==== %lld %ld %ld\n",divfct->dim,nmb,firstdiv); */
   for(i=0;i<divfct->dim;i++){
    if(divfct->mlt[i]==1){
      if(fdivused==0){
        nmbfct->mlt[multnum]=firstdiv;
        nmbfct->exp[multnum]=1;   
      }
      break;
    }
    if(divfct->mlt[i]<firstdiv){
      nmbfct->mlt[i]=divfct->mlt[i];
      nmbfct->exp[i]=divfct->exp[i];
      multnum++;
    }else if(divfct->mlt[i]==firstdiv){
      nmbfct->mlt[i]=divfct->mlt[i];
      nmbfct->exp[i]=divfct->exp[i]+1;   
      fdivused=1;
    }else{
      int j=i;
      if(fdivused==0) j=i+1;
      nmbfct->mlt[j]=divfct->mlt[i];
      nmbfct->exp[j]=divfct->exp[i];    
    }
  }
}
void GetFactorization(long int prnum,long int *prlist,
                            Factorization **fctlist){
/*fprintf(stdout,"GetFactorization: factorizing first %ld numbers.\n",
                prnum);*/
  long int i=0,j=0;
  Factorization *fct=(Factorization*)malloc(2*sizeof(Factorization)); 
  long int len=0,isft=0,div=1,firstdiv=1;

  fct->dim=2;
  fct->mlt=(long int*)malloc(2*sizeof(long int));
  fct->exp=(long int*)malloc(2*sizeof(long int));
  for(i=0;i<fct->dim;i++){
    fct->mlt[i]=1;
    fct->exp[i]=0;
  }
  fct->mlt[0]=2;
  fct->exp[0]=1;
  fctlist[2]=fct;

  fct=(Factorization*)malloc(2*sizeof(Factorization));
  fct->dim=2;
  fct->mlt=(long int*)malloc(2*sizeof(long int));
  fct->exp=(long int*)malloc(2*sizeof(long int));
  for(i=0;i<fct->dim;i++){
    fct->mlt[i]=1;
    fct->exp[i]=0;
  }
  fct->mlt[0]=3;
  fct->exp[0]=1;
  fctlist[3]=fct;
 
  for(i=0;i<prlist[prnum-1];i++){
    len=0;
    isft=i;
    while(isft>0){
      len++;
      isft=isft>>1;
    }
    fct=(Factorization*)malloc(2*sizeof(Factorization));
    fct->dim=len;
    if (len==0) len=1;
    fct->mlt=(long int*)malloc(len*sizeof(long int));
    fct->exp=(long int*)malloc(len*sizeof(long int));
    for(j=0;j<fct->dim;j++){
      fct->mlt[j]=1;
      fct->exp[j]=0;
    }
    div=1;
    for(j=0;prlist[j]*prlist[j]<=i;j++){
      firstdiv=prlist[j];
      if(i-firstdiv*((long int)i/firstdiv)==0){
        div=firstdiv;
        if(firstdiv*firstdiv==i){
          fct->mlt[0]=firstdiv;
          fct->exp[0]=2;	  
	}else{
	  ExtendFactors(i,firstdiv,fct,fctlist);
        }
	break;
      }
    }
    if(div==1){
      fct->mlt[0]=i;
      fct->exp[0]=1;   
    }
    fctlist[i]=fct;
/*
     ShowFactorization(fct);
*/
  }
/*  fprintf(stdout,"GetFactorization: Done.\n"); */
}

long int adcexp[]={
  11,13,17,19,23,
  23,29,31,37,41,	     	  
  41,43,47,53,59,	     	  
  3,5,7,11,13};
long int adcexpS[]={
  11,13,17,19,23};
long int adcexpW[]={  
  2*2,2*2*2*5,2*3,2*2*5,2*3*7,
  23,29,31,2*2,2*2*19};
long int adcexpA[]={  
  2*2,2*2*2*5,2*3,2*2*5,2*3*7,
  2*19,2*13,2*19,2*2*2*13*19,2*2*2*19*19,                    
  2*23,2*2*2*2,2*2*2*2*2*23,2*2*2*2*2,2*2*23};
long int adcexpB[]={  
  2*2*7,2*2*2*5,2*3*7,2*2*5*7,2*3*7*7,
  2*19,2*13,2*19,2*2*2*13*19,2*2*2*19*19,                      
  2*31,2*2*2*2*31,2*2*2*2*2*31,2*2*2*2*2*29,2*2*29,
  2*43,2*2,2*2,2*2*47,2*2*2*43};  
long int UpPrimeLim=100000;

typedef struct dc_view{
  long long int vsize;
  long int vidx;
} DC_view;

int CompareSizesByValue( const void* sz0, const void* sz1) {
long long int *size0=(long long int*)sz0,
              *size1=(long long int*)sz1;
  int res=0;
  if(*size0-*size1>0) res=1;
  else if(*size0-*size1<0) res=-1;
  return res;
}
int CompareViewsBySize( const void* vw0, const void* vw1) {
DC_view *lvw0=(DC_view *)vw0, *lvw1=(DC_view *)vw1;
  int res=0;
  if(lvw0->vsize>lvw1->vsize) res=1;
  else if(lvw0->vsize<lvw1->vsize) res=-1;
  else if(lvw0->vidx>lvw1->vidx) res=1;
  else if(lvw0->vidx<lvw1->vidx) res=-1;
  return res;
}

int CalculateVeiwSizes(ADC_PAR *par){
  unsigned long long totalInBytes = 0;
  unsigned long long nViewDims, nCubeTuples = 0;
 
  const char *adcfname=par->filename;
  int NDID=par->ndid;
  char clss=par->clss;
  int dcdim=par->dim;
  long long int tnum=par->tuplenum;
  long long int i=0,j=0;
  Factorization  
    **fctlist=(Factorization **) calloc(UpPrimeLim,sizeof(Factorization *));
  long int *prlist=(long int *) calloc(UpPrimeLim,sizeof(long int));
  int prnum=ListFirstPrimes(UpPrimeLim,prlist);
  DC_view *dcview=(DC_view *)calloc((1<<dcdim),sizeof(DC_view));
  const char* vszefname0;
  char *vszefname=NULL;
  FILE* view=NULL;
  int minvn=1, maxvn=(1<<dcdim), vinc=1;
  long idx=0;

  GetFactorization(prnum,prlist,fctlist); 
  for(i=1;i<(1<<dcdim);i++){   
    long long int LCM=1;
    switch(clss){
      case 'U':
        LCM=GetLCM(i,fctlist,adcexp);
      break;
      case 'S':
        LCM=GetLCM(i,fctlist,adcexpS);
      break;
      case 'W':
        LCM=GetLCM(i,fctlist,adcexpW);
      break;
      case 'A':
        LCM=GetLCM(i,fctlist,adcexpA);
      break;
      case 'B':
        LCM=GetLCM(i,fctlist,adcexpB);
      break;
    }
    if(LCM>tnum) LCM=tnum;
    dcview[i].vsize=LCM;
    dcview[i].vidx=i;
  }
  for(i=0;i<UpPrimeLim;i++){
    if(!fctlist[i]) continue;
    if(fctlist[i]->mlt) free(fctlist[i]->mlt); 
    if(fctlist[i]->exp) free(fctlist[i]->exp); 
    free(fctlist[i]);
  }
  free(fctlist);
  free(prlist);
   
  vszefname0="view.sz";
  vszefname=(char*)calloc(BlockSize,sizeof(char));
  sprintf(vszefname,"%s.%s.%d",adcfname,vszefname0,NDID);
  if(!(view = fopen(vszefname, "w+")) ) {
    fprintf(stderr,"CalculateVeiwSizes: Can't open file: %s\n",vszefname);
    return 0;
  }
  qsort( dcview, (1<<dcdim), sizeof(DC_view),CompareViewsBySize);	

  switch(clss){
    case 'U':
      vinc=1<<3;
    break;
    case 'S':
    break;
    case 'W':
    break;
    case 'A':
      vinc=1<<6;
    break;
    case 'B':
      vinc=1<<14;
    break;
  }
   for(i=minvn;i<maxvn;i+=vinc){   
    nViewDims = 0;
    fprintf(view,"Selection:");
    idx=dcview[i].vidx;
    for(j=0;j<dcdim;j++) 
      if((idx>>j)&0x1==1) { fprintf(view," %lld",j+1); nViewDims++;}
    fprintf(view,"\nView Size: %lld\n",dcview[i].vsize);

    totalInBytes += (8+4*nViewDims)*dcview[i].vsize;
    nCubeTuples += dcview[i].vsize;

  }
  fprintf(view,"\nTotal in bytes: %lld  Number of tuples: %lld\n", 
          totalInBytes, nCubeTuples);
  
  fclose(view);
  free(dcview);
  fprintf(stdout,"View sizes are written into %s\n",vszefname);
  free(vszefname);
  return 1;
}

int ParseParFile(char* parfname,ADC_PAR *par){
  char line[BlockSize];
  FILE* parfile=NULL;
  char* pos=strchr(parfname,'.');
  int linenum=0,i=0;
  const char *kwd;

  if(!(parfile = fopen(parfname, "r")) ) {
    fprintf(stderr,"ParseParFile: Can't open file: %s\n",parfname);
    return 0;
  }
  if(pos) pos=strchr(pos+1,'.');
  if(pos) sscanf(pos+1,"%d",&(par->ndid));
  linenum=0;
  while(fgets(&line[0],BlockSize,parfile)){
    i=0;
    kwd=adcKeyword[i];
    while(kwd){
      if(strstr(line,"#")) {
        ;/*comment line, do nothing*/
      }else if(strstr(line,kwd)){
        char *pos=line+strlen(kwd)+1;
        switch(i){
          case 0:
            sscanf(pos,"%d",&(par->dim));
          break;
          case 1:
            sscanf(pos,"%d",&(par->mnum));
          break;
          case 2:
            sscanf(pos,"%lld",&(par->tuplenum));
          break;
          case 3:
/*            sscanf(pos,"%d",&(par->isascii));*/
          break;
          case 4:
            sscanf(pos,"%d",&(par->inverse_endian));
          break;
          case 5:
            par->filename=(char*) malloc(strlen(pos)*sizeof(char));
            sscanf(pos,"%s",par->filename);
          break;
          case 6:
            sscanf(pos,"%c",&(par->clss));
          break;
        }
        break;        
      }
      i++;
      kwd=adcKeyword[i];
    }
    linenum++;
  }
  fclose(parfile);
  switch(par->clss){/* overwriting parameters according the class */
    case 'S':
      par->dim=5;
      par->mnum=1;
      par->tuplenum=1000;
    break;
    case 'W':
      par->dim=10;
      par->mnum=1;
      par->tuplenum=100000;
    break;
    case 'A':
      par->dim=15;
      par->mnum=1;
      par->tuplenum=1000000;
    break;
    case 'B':
      par->dim=20;
      par->mnum=1;
      par->tuplenum=10000000;
    break;
  }  
  return 1;
}
int WriteADCPar(ADC_PAR *par,char* fname){
  char *lname=(char*) calloc(BlockSize,sizeof(char));
  FILE *parfile=NULL;

  sprintf(lname,"%s",fname);
  parfile=fopen(lname,"w");
  if(!parfile){
    fprintf(stderr,"WriteADCPar: can't open file %s\n",lname);
    return 0;
  }
  fprintf(parfile,"attrNum=%d\n",par->dim);
  fprintf(parfile,"measuresNum=%d\n",par->mnum);
  fprintf(parfile,"tuplesNum=%lld\n",par->tuplenum);
  fprintf(parfile,"class=%c\n",par->clss);
/*  fprintf(parfile,"isASCII=%d\n",par->isascii); */
  fprintf(parfile,"INVERSE_ENDIAN=%d\n",par->inverse_endian);
  fprintf(parfile,"fileName=%s\n",par->filename);
  fclose(parfile);
  return 1;
}
void ShowADCPar(ADC_PAR *par){
  fprintf(stdout,"********************* ADC paramters\n");
  fprintf(stdout," id		%d\n",par->ndid);
  fprintf(stdout," attributes 	%d\n",par->dim);
  fprintf(stdout," measures   	%d\n",par->mnum);
  fprintf(stdout," tuples     	%lld\n",par->tuplenum);
  fprintf(stdout," class	\t%c\n",par->clss);
  fprintf(stdout," filename       %s\n",par->filename);
  fprintf(stdout,"***********************************\n");
}

long int adcgen[]={
  2,7,3,2,2,
  2,2,5,31,7,
  2,3,3,3,2,
  5,2,2,2,3};
  
int GetNextTuple(int dcdim, int measnum,
                 long long int* attr,long long int* meas,
		 char clss){
  static int tuplenum=0;
  static const int maxdim=20;
  static int measbound=31415;
  int i=0,j=0;
  int maxattr=0;
  static long int seed[20];
  long int *locexp=NULL;

  if(dcdim>maxdim){
    fprintf(stderr,"GetNextTuple: number of dcdim is too large:%d",
                    dcdim);
    return 0;
  }
  if(measnum>measbound){
    fprintf(stderr,"GetNextTuple: number of mes is too large:%d",
                    measnum);
    return 0;
  }
  locexp=adcexp;
  switch(clss){
    case 'S':
    locexp=adcexpS;
    break;
    case 'W':
    locexp=adcexpW;
    break;
    case 'A':
    locexp=adcexpA;
    break;
    case 'B':
    locexp=adcexpB;
    break;
  }  
  if(tuplenum==0){
    for(i=0;i<dcdim;i++){
      int tmpgen=adcgen[i];
      for(j=0;j<locexp[i]-1;j++){
        tmpgen*=adcgen[i];
	tmpgen=tmpgen%adcprime[i];
      }
      adcgen[i]=tmpgen;
    }
    fprintf(stdout,"Prime \tGenerator \tSeed\n");
    for(i=0;i<dcdim;i++){
      seed[i]=(adcprime[i]+1)/2;
      fprintf(stdout," %ld\t %ld\t\t %ld\n",adcprime[i],adcgen[i],seed[i]);
     }
  }
  tuplenum++;
  maxattr=0;
  for(i=0;i<dcdim;i++){
    attr[i]=seed[i]*adcgen[i];
    attr[i]-=adcprime[i]*((long long int)attr[i]/adcprime[i]); 
    seed[i]=attr[i];
    if(seed[i]>maxattr) maxattr=seed[i];
  }		     	  
  for(i=0;i<measnum;i++){
    meas[i]=(long long int)(seed[i]*maxattr);
    meas[i]-=measbound*(meas[i]/measbound);
  }		     	  
  return 1;
}

int GenerateADC(ADC_PAR *par){
  int dcdim=par->dim,
      mesnum=par->mnum,
      tplnum=par->tuplenum;
  char *adcfname=(char*)calloc(BlockSize,sizeof(char));
  
  FILE *adc;
  int i=0,j=0;
  long long int* attr=NULL,*mes=NULL; 
/*
   if(par->isascii==1){
    sprintf(adcfname,"%s.tpl.%d",par->filename,par->ndid);
    if(!(adc = fopen(adcfname, "w+"))) {
      fprintf(stderr,"GenerateADC: Can't open file: %s\n",adcfname);
      return 0;
    }
  }else{
*/
  sprintf(adcfname,"%s.dat.%d",par->filename,par->ndid);
    if(!(adc = fopen(adcfname, "wb+"))){
      fprintf(stderr,"GenerateADC: Can't open file: %s\n",adcfname);
       return 0;
    }
/*  } */
  attr=(long long int *)malloc(dcdim*sizeof(long long int));
  mes=(long long int *)malloc(mesnum*sizeof(long long int));

  fprintf(stdout,"\nGenerateADC: writing %d tuples of %d attributes and %d measures to %s\n",
		  tplnum,dcdim,mesnum,adcfname);
   for(i=0;i<tplnum;i++){
    if(!GetNextTuple(dcdim,mesnum,attr,mes,par->clss)) return 0;
/*
     if(par->isascii==1){
      for(int j=0;j<dcdim;j++)fprintf(adc,"%lld ",attr[j]);
      for(int j=0;j<mesnum;j++)fprintf(adc,"%lld ",mes[j]);
      fprintf(adc,"\n");
    }else{
*/
      for(j=0;j<mesnum;j++){ 
    	long long mv =  mes[j];
	    if(par->inverse_endian==1) swap8(&mv);
	    fwrite(&mv, 8, 1, adc); 
      }
      for(j=0;j<dcdim;j++){ 
    	int av = attr[j]; 
	if(par->inverse_endian==1) swap4(&av);
	fwrite(&av, 4, 1, adc); 
      }
    }
/*  } */
  fclose(adc);
  fprintf(stdout,"Binary ADC file %s ",adcfname);
  fprintf(stdout,"have been generated.\n");
  free(attr);
  free(mes);
  free(adcfname);
  CalculateVeiwSizes(par);
  return 1;
}
