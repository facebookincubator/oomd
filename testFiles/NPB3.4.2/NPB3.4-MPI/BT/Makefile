SHELL=/bin/sh
BENCHMARK=bt
BENCHMARKU=BT
VEC=

include ../config/make.def


OBJS = bt.o bt_data.o make_set.o initialize.o exact_solution.o \
       exact_rhs.o set_constants.o adi.o define.o copy_faces.o \
       rhs.o solve_subs.o x_solve$(VEC).o y_solve$(VEC).o z_solve$(VEC).o \
       add.o error.o verify.o setup_mpi.o mpinpb.o \
       ${COMMON}/get_active_nprocs.o \
       ${COMMON}/print_results.o ${COMMON}/timers.o

include ../sys/make.common

# npbparams.h is included by bt_data module (via bt_data.o)

${PROGRAM}: config
	@if [ x$(VERSION) = xvec ] ; then	\
		${MAKE} VEC=_vec exec;		\
	elif [ x$(VERSION) = xVEC ] ; then	\
		${MAKE} VEC=_vec exec;		\
	else					\
		${MAKE} exec;			\
	fi

exec: $(OBJS)
	@if [ x$(SUBTYPE) = xfull -o x$(SUBTYPE) = xFULL ] ; then	\
		${MAKE} bt-full;		\
	elif [ x$(SUBTYPE) = xsimple -o x$(SUBTYPE) = xSIMPLE ] ; then	\
		${MAKE} bt-simple;		\
	elif [ x$(SUBTYPE) = xfortran -o x$(SUBTYPE) = xFORTRAN ] ; then \
		${MAKE} bt-fortran;		\
	elif [ x$(SUBTYPE) = xepio -o x$(SUBTYPE) = xEPIO ] ; then	\
		${MAKE} bt-epio;		\
	else					\
		${MAKE} bt-bt;			\
	fi

bt-bt: ${OBJS} btio.o
	${FLINK} ${FLINKFLAGS} -o ${PROGRAM} ${OBJS} btio.o ${FMPI_LIB}

bt-full: ${OBJS} full_mpiio.o btio_common.o
	${FLINK} ${FLINKFLAGS} -o ${PROGRAM}.mpi_io_full ${OBJS} btio_common.o full_mpiio.o ${FMPI_LIB}

bt-simple: ${OBJS} simple_mpiio.o btio_common.o
	${FLINK} ${FLINKFLAGS} -o ${PROGRAM}.mpi_io_simple ${OBJS} btio_common.o simple_mpiio.o ${FMPI_LIB}

bt-fortran: ${OBJS} fortran_io.o btio_common.o
	${FLINK} ${FLINKFLAGS} -o ${PROGRAM}.fortran_io ${OBJS} btio_common.o fortran_io.o ${FMPI_LIB}

bt-epio: ${OBJS} epio.o btio_common.o
	${FLINK} ${FLINKFLAGS} -o ${PROGRAM}.ep_io ${OBJS} btio_common.o epio.o ${FMPI_LIB}

.f90.o:
	${FCOMPILE} $<

.c.o:
	${CCOMPILE} $<


bt.o:             bt.f90  bt_data.o mpinpb.o
make_set.o:       make_set.f90  bt_data.o mpinpb.o
initialize.o:     initialize.f90  bt_data.o
exact_solution.o: exact_solution.f90  bt_data.o
exact_rhs.o:      exact_rhs.f90  bt_data.o
set_constants.o:  set_constants.f90  bt_data.o
adi.o:            adi.f90  bt_data.o
define.o:         define.f90  bt_data.o
copy_faces.o:     copy_faces.f90  bt_data.o mpinpb.o
rhs.o:            rhs.f90  bt_data.o
x_solve$(VEC).o:  x_solve$(VEC).f90  bt_data.o mpinpb.o
y_solve$(VEC).o:  y_solve$(VEC).f90  bt_data.o mpinpb.o
z_solve$(VEC).o:  z_solve$(VEC).f90  bt_data.o mpinpb.o
solve_subs.o:     solve_subs.f90
add.o:            add.f90  bt_data.o
error.o:          error.f90  bt_data.o mpinpb.o
verify.o:         verify.f90  bt_data.o mpinpb.o
setup_mpi.o:      setup_mpi.f90  bt_data.o mpinpb.o
btio.o:           btio.f90  bt_data.o
btio_common.o:    btio_common.f90  bt_data.o mpinpb.o
fortran_io.o:     fortran_io.f90  bt_data.o mpinpb.o
simple_mpiio.o:   simple_mpiio.f90  bt_data.o mpinpb.o
full_mpiio.o:     full_mpiio.f90  bt_data.o mpinpb.o
epio.o:           epio.f90  bt_data.o mpinpb.o
bt_data.o:        bt_data$(VEC).f90 mpinpb.o npbparams.h
	${FCOMPILE} -o $@ bt_data$(VEC).f90
mpinpb.o:         mpinpb.f90

clean:
	- rm -f *.o *.mod *~ mputil*
	- rm -f  npbparams.h core
