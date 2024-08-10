SHELL=/bin/sh
CLASS=W
VERSION=
SFILE=config/suite.def

default: header
	@ sys/print_instructions

BT: bt
bt: header
	cd BT; $(MAKE) CLASS=$(CLASS) VERSION=$(VERSION)

SP: sp		       
sp: header	       
	cd SP; $(MAKE) CLASS=$(CLASS)

LU: lu		       
lu: header	       
	cd LU; $(MAKE) CLASS=$(CLASS) VERSION=$(VERSION)

MG: mg		       
mg: header	       
	cd MG; $(MAKE) CLASS=$(CLASS)

FT: ft		       
ft: header	       
	cd FT; $(MAKE) CLASS=$(CLASS)

IS: is		       
is: header	       
	cd IS; $(MAKE) CLASS=$(CLASS)

CG: cg		       
cg: header	       
	cd CG; $(MAKE) CLASS=$(CLASS)

EP: ep		       
ep: header	       
	cd EP; $(MAKE) CLASS=$(CLASS)

UA: ua
ua: header	       
	cd UA; $(MAKE) CLASS=$(CLASS)

DC: dc
dc: header	       
	cd DC; $(MAKE) CLASS=$(CLASS)

# Awk script courtesy cmg@cray.com, modified by Haoqiang Jin
suite:
	@ awk -f sys/suite.awk SMAKE=$(MAKE) $(SFILE) | $(SHELL)


# It would be nice to make clean in each subdirectory (the targets
# are defined) but on a really clean system this will won't work
# because those makefiles need config/make.def
clean:
	- rm -f core *~ */core */*~
	- rm -f */*.o */*.obj */*.exe */*.mod */npbparams.h */blk_par.h
	- rm -f sys/setparams sys/makesuite sys/setparams.h
	- rm -rf */rii_files

veryclean: clean
	- rm -f config/make.def config/suite.def 
	- rm -f bin/sp.* bin/lu.* bin/mg.* bin/ft.* bin/bt.* bin/is.*
	- rm -f bin/ep.* bin/cg.* bin/ua.* bin/dc.* bin/ADC.*

header:
	@ sys/print_header



