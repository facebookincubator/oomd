#define PutErrMsg(msg) {fprintf(stderr," %s, errno = %d\n", msg, errno);}

#define WriteToFile(ptr,size,nitems,stream,logf) if( fwrite(ptr,size,nitems,stream) != nitems )\
       {\
        fprintf(stderr,"\n Write error from WriteToFile()\n"); return ADC_WRITE_FAILED; \
       }

#ifdef WINNT
#define FSEEK(stream,offset,whence)  fseek(stream, (long)offset,whence);
#else
#define FSEEK(stream,offset,whence)  fseek(stream,offset,whence); 
#endif

#define GetRecSize(nd,nm) (DIM_FSZ*nd+MSR_FSZ*nm)
