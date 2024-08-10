SHELL=/bin/sh
BENCHMARK=ua
BENCHMARKU=UA
UPDATE=$(VERSION)
UXT=

include ../config/make.def


OBJS = ua.o ua_data.o convect.o diffuse.o adapt.o move.o mason.o \
       precond.o utils.o verify.o setup.o transfer$(UXT).o \
       ${COMMON}/print_results.o ${COMMON}/timers.o ${COMMON}/wtime.o

include ../sys/make.common

# npbparams.h is included by ua_data module (via ua_data.o)

${PROGRAM}: config ${OBJS}
	@if [ x$(UPDATE) = xau -o x$(UPDATE) = xAU ] ; then	\
		${MAKE} UXT=_au ua-def;	\
	elif [ x$(UPDATE) = xrd -o x$(UPDATE) = xRD ] ; then	\
		${MAKE} UXT=_rd ua-rd;	\
	else				\
		${MAKE} ua-def;		\
	fi

ua-def: ${OBJS}
	${FLINK} ${FLINKFLAGS} -o ${PROGRAM} ${OBJS} ${F_LIB}

ua-rd: ${OBJS} tmorwork.o
	${FLINK} ${FLINKFLAGS} -o ${PROGRAM} ${OBJS} tmorwork.o ${F_LIB}

.f90.o:
	${FCOMPILE} $<

ua.o:        ua.f90       ua_data.o
setup.o:     setup.f90    ua_data.o
convect.o:   convect.f90  ua_data.o
adapt.o:     adapt.f90    ua_data.o
move.o:      move.f90     ua_data.o
diffuse.o:   diffuse.f90  ua_data.o
mason.o:     mason.f90    ua_data.o
precond.o:   precond.f90  ua_data.o
transfer.o:  transfer.f90 ua_data.o
transfer_au.o:  transfer_au.f90 ua_data.o
transfer_rd.o:  transfer_rd.f90 ua_data.o tmorwork.o
utils.o:     utils.f90    ua_data.o
verify.o:    verify.f90   ua_data.o
ua_data.o:   ua_data.f90  npbparams.h
tmorwork.o:  tmorwork.f90

clean:
	- rm -f *.o *~ *.mod mputil*
	- rm -f npbparams.h core
