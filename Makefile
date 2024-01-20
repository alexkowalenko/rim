# definitions for LINUX rim
RANLIB=ranlib
F2CLIB=/usr/lib/f2cmain.a -lf2c
EXTRA=time_date.o
LIBEXTRA=time_date.o
PILE = CC_f2c=$(CC)  f77
LIBRIM=librim.a
AR = ar rv
RAN = ranlib

# make rim (generic instructions)
#

all: rim librim.a

# creates rim, rime, rimh, 
#    executes rimh (creation of help files), 
#        installs programs and files
rim::
	run.setup
	rm -f src/rimlib
	find src -name "*.o" -exec rm -f {} \;
	(cd src; make -f Makefile rim; mv rim $(UNIX_BIN);)
	(cd src; make -f Makefile rime;	mv rime $(UNIX_BIN);)
	rim src/rim_help.schema
	make -f Makefile rimh
#not required:	make -f Makefile help
	$(GMAKE) -makeparentdir librim.a
	(cd src; make -f Makefile librim.a; mv 'librim.a' ../$(LIBARCH)/librim.a;)
	cp rim_help.rimdb1 rim_help.rimdb2 rim_help.rimdb3 /usr/local/lib
	cp rim.1 /usr/man/man1/rim.1
	find src -name "*.o" -exec rm -f {} \;
	rm -f rimh src/rimh
