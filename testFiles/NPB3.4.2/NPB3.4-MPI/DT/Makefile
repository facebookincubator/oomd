SHELL=/bin/sh
BENCHMARK=dt
BENCHMARKU=DT

include ../config/make.def

include ../sys/make.common
#Override PROGRAM
DTPROGRAM  = $(BINDIR)/$(BENCHMARK).$(CLASS).x

OBJS = dt.o DGraph.o \
	${COMMON}/c_print_results.o ${COMMON}/c_timers.o ${COMMON}/c_randdp.o


${PROGRAM}: config ${OBJS}
	${CLINK} ${CLINKFLAGS} -o ${DTPROGRAM} ${OBJS} ${CMPI_LIB}

.c.o:
	${CCOMPILE} $<

dt.o:             dt.c  npbparams.h
DGraph.o:	DGraph.c DGraph.h

clean:
	- rm -f *.o *~ mputil*
	- rm -f dt npbparams.h core
