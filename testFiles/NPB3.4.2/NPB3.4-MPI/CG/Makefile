SHELL=/bin/sh
BENCHMARK=cg
BENCHMARKU=CG

include ../config/make.def

OBJS = cg.o cg_data.o mpinpb.o ${COMMON}/print_results.o  \
       ${COMMON}/get_active_nprocs.o \
       ${COMMON}/${RAND}.o ${COMMON}/timers.o

include ../sys/make.common

${PROGRAM}: config ${OBJS}
	${FLINK} ${FLINKFLAGS} -o ${PROGRAM} ${OBJS} ${FMPI_LIB}

.f90.o:
	${FCOMPILE} $<

cg.o:		cg.f90  cg_data.o mpinpb.o
cg_data.o:	cg_data.f90 mpinpb.o npbparams.h
mpinpb.o:	mpinpb.f90

clean:
	- rm -f *.o *.mod *~ 
	- rm -f npbparams.h core



