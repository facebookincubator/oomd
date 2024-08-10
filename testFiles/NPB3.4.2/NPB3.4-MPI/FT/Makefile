SHELL=/bin/sh
BENCHMARK=ft
BENCHMARKU=FT

include ../config/make.def

include ../sys/make.common

OBJS = ft.o ft_data.o mpinpb.o ${COMMON}/get_active_nprocs.o \
	${COMMON}/${RAND}.o ${COMMON}/print_results.o ${COMMON}/timers.o

${PROGRAM}: config ${OBJS}
	${FLINK} ${FLINKFLAGS} -o ${PROGRAM} ${OBJS} ${FMPI_LIB}


.f90.o:
	${FCOMPILE} $<

ft.o:		ft.f90  ft_data.o mpinpb.o
ft_data.o:	ft_data.f90  mpinpb.o npbparams.h
mpinpb.o:	mpinpb.f90

clean:
	- rm -f *.o *.mod *~ mputil*
	- rm -f ft npbparams.h core
