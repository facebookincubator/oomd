SHELL=/bin/sh
BENCHMARK=lu
BENCHMARKU=LU
VEC=

include ../config/make.def

OBJS = lu.o lu_data$(VEC).o init_comm.o read_input.o bcast_inputs.o \
       proc_grid.o neighbors.o nodedim.o subdomain.o setcoeff.o \
       setbv.o exact.o setiv.o erhs.o ssor$(VEC).o exchange_1.o exchange_3.o \
       exchange_4.o exchange_5.o exchange_6.o rhs.o l2norm.o \
       jacld$(VEC).o blts$(VEC).o jacu$(VEC).o buts$(VEC).o mpinpb.o \
       error.o pintgr.o verify.o ${COMMON}/get_active_nprocs.o \
       ${COMMON}/print_results.o ${COMMON}/timers.o

include ../sys/make.common


# npbparams.h is included by lu_data module (via lu_data.o)

${PROGRAM}: config
	@if [ x$(VERSION) = xvec ] ; then	\
		${MAKE} VEC=_vec exec;		\
	elif [ x$(VERSION) = xVEC ] ; then	\
		${MAKE} VEC=_vec exec;		\
	else					\
		${MAKE} exec;			\
	fi

exec: $(OBJS)
	${FLINK} ${FLINKFLAGS} -o ${PROGRAM} ${OBJS} ${FMPI_LIB}

.f90.o :
	${FCOMPILE} $<

lu.o:		lu.f90 lu_data.o mpinpb.o
bcast_inputs.o:	bcast_inputs.f90 lu_data.o mpinpb.o
blts$(VEC).o:	blts$(VEC).f90 lu_data.o
buts$(VEC).o:	buts$(VEC).f90 lu_data.o
erhs.o:		erhs.f90 lu_data.o
error.o:	error.f90 lu_data.o mpinpb.o
exact.o:	exact.f90 lu_data.o
exchange_1.o:	exchange_1.f90 lu_data.o mpinpb.o
exchange_3.o:	exchange_3.f90 lu_data.o mpinpb.o
exchange_4.o:	exchange_4.f90 lu_data.o mpinpb.o
exchange_5.o:	exchange_5.f90 lu_data.o mpinpb.o
exchange_6.o:	exchange_6.f90 lu_data.o mpinpb.o
init_comm.o:	init_comm.f90 lu_data.o mpinpb.o 
jacld$(VEC).o:	jacld$(VEC).f90 lu_data.o
jacu$(VEC).o:	jacu$(VEC).f90 lu_data.o
l2norm.o:	l2norm.f90 lu_data.o mpinpb.o
neighbors.o:	neighbors.f90 lu_data.o
nodedim.o:	nodedim.f90
pintgr.o:	pintgr.f90 lu_data.o mpinpb.o
proc_grid.o:	proc_grid.f90 lu_data.o mpinpb.o
read_input.o:	read_input.f90 lu_data.o mpinpb.o
rhs.o:		rhs.f90 lu_data.o
setbv.o:	setbv.f90 lu_data.o
setiv.o:	setiv.f90 lu_data.o
setcoeff.o:	setcoeff.f90 lu_data.o
ssor$(VEC).o:	ssor$(VEC).f90 lu_data.o mpinpb.o
subdomain.o:	subdomain.f90 lu_data.o mpinpb.o
verify.o:	verify.f90 lu_data.o
lu_data.o:      lu_data$(VEC).f90 mpinpb.o npbparams.h
	${FCOMPILE} -o $@ lu_data$(VEC).f90
mpinpb.o:       mpinpb.f90

clean:
	- rm -f npbparams.h
	- rm -f *.o *.mod *~
