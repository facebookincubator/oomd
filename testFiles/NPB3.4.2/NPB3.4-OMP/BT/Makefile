SHELL=/bin/sh
BENCHMARK=bt
BENCHMARKU=BT
BLK=
BLKFAC=0

include ../config/make.def


OBJS = bt.o bt_data.o initialize.o exact_solution.o \
       exact_rhs.o set_constants.o adi.o  rhs.o      \
       x_solve$(BLK).o y_solve$(BLK).o solve_subs$(BLK).o  \
       z_solve$(BLK).o add.o error.o verify.o work_lhs$(BLK).o \
       ${COMMON}/print_results.o ${COMMON}/timers.o ${COMMON}/wtime.o

include ../sys/make.common

# npbparams.h is included by bt_data module (via bt_data.o)

${PROGRAM}: config
	@ver=$(VERSION); bfac=`echo $$ver|sed -e 's/^blk//' -e 's/^BLK//'`; \
	if [ x$$ver != x$$bfac ] ; then		\
		${MAKE} BLK=_blk BLKFAC=$${bfac:-8} exec;	\
	else					\
		${MAKE} exec;			\
	fi

exec: $(OBJS)
	${FLINK} ${FLINKFLAGS} -o ${PROGRAM} ${OBJS} ${F_LIB}

.f90.o:
	${FCOMPILE} $<


blk_par.h: FORCE
	sed -e 's/= 0/= $(BLKFAC)/' blk_par0.h > blk_par.h_wk
	@ if ! `diff blk_par.h_wk blk_par.h > /dev/null 2>&1`; then \
	mv -f blk_par.h_wk blk_par.h; else rm -f blk_par.h_wk; fi
FORCE:

bt.o:             bt.f90  bt_data.o blk_par.h
initialize.o:     initialize.f90  bt_data.o
exact_solution.o: exact_solution.f90  bt_data.o
exact_rhs.o:      exact_rhs.f90  bt_data.o
set_constants.o:  set_constants.f90  bt_data.o
adi.o:            adi.f90  bt_data.o
rhs.o:            rhs.f90  bt_data.o
x_solve$(BLK).o:  x_solve$(BLK).f90  bt_data.o work_lhs$(BLK).o
y_solve$(BLK).o:  y_solve$(BLK).f90  bt_data.o work_lhs$(BLK).o
z_solve$(BLK).o:  z_solve$(BLK).f90  bt_data.o work_lhs$(BLK).o
solve_subs$(BLK).o: solve_subs$(BLK).f90  work_lhs$(BLK).o
work_lhs$(BLK).o: work_lhs$(BLK).f90  bt_data.o blk_par.h
add.o:            add.f90  bt_data.o
error.o:          error.f90  bt_data.o
verify.o:         verify.f90  bt_data.o
bt_data.o:        bt_data.f90  npbparams.h

clean:
	- rm -f *.o *~ *.mod mputil*
	- rm -f npbparams.h core blk_par.h
	- if [ -d rii_files ]; then rm -r rii_files; fi
