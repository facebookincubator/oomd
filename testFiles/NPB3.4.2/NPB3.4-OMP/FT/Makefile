SHELL=/bin/sh
BENCHMARK=ft
BENCHMARKU=FT
BLKFAC=32

include ../config/make.def

include ../sys/make.common

OBJS = ft.o ft_data.o ${COMMON}/${RAND}.o ${COMMON}/print_results.o \
       ${COMMON}/timers.o ${COMMON}/wtime.o

${PROGRAM}: config
	@ver=$(VERSION); bfac=`echo $$ver|sed -e 's/^blk//' -e 's/^BLK//'`; \
	if [ x$$ver != x$$bfac ] ; then		\
		${MAKE} BLKFAC=$${bfac:-32} exec;	\
	else					\
		${MAKE} exec;			\
	fi

exec: $(OBJS)
	${FLINK} ${FLINKFLAGS} -o ${PROGRAM} ${OBJS} ${F_LIB}


.f90.o:
	${FCOMPILE} $<

blk_par.h: FORCE
	sed -e 's/=0/=$(BLKFAC)/' blk_par0.h > blk_par.h_wk
	@ if ! `diff blk_par.h_wk blk_par.h > /dev/null 2>&1`; then \
	mv -f blk_par.h_wk blk_par.h; else rm -f blk_par.h_wk; fi
FORCE:

ft.o:		ft.f90  ft_data.o
ft_data.o:	ft_data.f90  npbparams.h blk_par.h

clean:
	- rm -f *.o *~ mputil* *.mod
	- rm -f ft npbparams.h core blk_par.h
	- if [ -d rii_files ]; then rm -r rii_files; fi
