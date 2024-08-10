SHELL=/bin/sh
BENCHMARK=dc
BENCHMARKU=DC

include ../config/make.def

include ../sys/make.common

OBJS = adc.o dc.o extbuild.o rbt.o jobcntl.o \
	${COMMON}/c_print_results.o  \
	${COMMON}/c_timers.o ${COMMON}/c_wtime.o


# npbparams.h is provided for backward compatibility with NPB compilation
# header.h: npbparams.h

${PROGRAM}: config ${OBJS} 
	${CLINK} ${CLINKFLAGS} -o ${PROGRAM} ${OBJS} ${C_LIB}

.c.o:
	${CCOMPILE} $<

adc.o:      adc.c npbparams.h
dc.o:       dc.c adcc.h adc.h macrodef.h npbparams.h
extbuild.o: extbuild.c adcc.h adc.h macrodef.h npbparams.h
rbt.o:      rbt.c adcc.h adc.h rbt.h macrodef.h npbparams.h
jobcntl.o:  jobcntl.c adcc.h adc.h macrodef.h npbparams.h

clean:
	- rm -f *.o 
	- rm -f npbparams.h core
	- rm -f {../,}ADC.{logf,view,dat,viewsz,groupby,chunks}.* 

