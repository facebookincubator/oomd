SHELL=/bin/sh
BENCHMARK=sp
BENCHMARKU=SP

include ../config/make.def


OBJS = sp.o sp_data.o make_set.o initialize.o exact_solution.o \
       exact_rhs.o set_constants.o adi.o define.o copy_faces.o \
       rhs.o lhsx.o lhsy.o lhsz.o x_solve.o ninvr.o y_solve.o pinvr.o \
       z_solve.o tzetar.o add.o txinvr.o error.o verify.o setup_mpi.o \
       mpinpb.o ${COMMON}/get_active_nprocs.o \
       ${COMMON}/print_results.o ${COMMON}/timers.o

include ../sys/make.common

# npbparams.h is included by sp_data module (via sp_data.o)

${PROGRAM}: config ${OBJS}
	${FLINK} ${FLINKFLAGS} -o ${PROGRAM} ${OBJS} ${FMPI_LIB}

.f90.o:
	${FCOMPILE} $<

sp.o:             sp.f90  sp_data.o mpinpb.o
make_set.o:       make_set.f90  sp_data.o mpinpb.o
initialize.o:     initialize.f90  sp_data.o
exact_solution.o: exact_solution.f90  sp_data.o
exact_rhs.o:      exact_rhs.f90  sp_data.o
set_constants.o:  set_constants.f90  sp_data.o
adi.o:            adi.f90  sp_data.o
define.o:         define.f90  sp_data.o
copy_faces.o:     copy_faces.f90  sp_data.o mpinpb.o
rhs.o:            rhs.f90  sp_data.o
lhsx.o:           lhsx.f90  sp_data.o
lhsy.o:           lhsy.f90  sp_data.o
lhsz.o:           lhsz.f90  sp_data.o
x_solve.o:        x_solve.f90  sp_data.o mpinpb.o
ninvr.o:          ninvr.f90  sp_data.o
y_solve.o:        y_solve.f90  sp_data.o mpinpb.o
pinvr.o:          pinvr.f90  sp_data.o
z_solve.o:        z_solve.f90  sp_data.o mpinpb.o
tzetar.o:         tzetar.f90  sp_data.o
add.o:            add.f90  sp_data.o
txinvr.o:         txinvr.f90  sp_data.o
error.o:          error.f90  sp_data.o mpinpb.o
verify.o:         verify.f90  sp_data.o mpinpb.o
setup_mpi.o:      setup_mpi.f90  sp_data.o mpinpb.o
sp_data.o:        sp_data.f90  mpinpb.o npbparams.h
mpinpb.o:         mpinpb.f90


clean:
	- rm -f *.o *.mod *~ mputil*
	- rm -f npbparams.h core
