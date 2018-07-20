#
#  Makefile file for ciftbx 4.1.0
#  Version of 30 November 2009
#  H. J. Bernstein, Bernstein+Sons
#
#
#
#  provide a definition of LN
LN = ln -s -f
# LN = cp
#
#  provide here a path to cif_mm.dic and cif_core.dic.
#
MMDICPATH	= ../dictionaries/cif_mm.dic
COREDICPATH	= ../dictionaries/cif_core.dic
CIFEXPPATH	= ../dictionaries/cif_expanded_jun06.dic
#
#  provide here a path to mkdecomln and rmdecompln
#  (define variable CSHELL as tcsh if csh is broken)
#
#CSHELL		= tcsh
MKDECOMPLN	= $(CSHELL) ../mkdecompln
RMDECOMPLN	= $(CSHELL) ../rmdecompln
#
#  The variable EXPAND defines a directory to use for
#  decompressed files, ususally /tmp or /var/tmp or .
#EXPAND		= /var/tmp
EXPAND		= .
#
#  Define the shell to be used by Makefile
#
SHELL		= /bin/sh
#
# provide flags for fortran compilations here
#FFLAGS		= -g -O0 -fbounds-check -Wall -Wno-unused-variable -Wimplicit
#FFLAGS		= -g -O0 -fbounds-check -Wall -Wno-unused-variable
FFLAGS		= -g -O2 -fbounds-check -Wall -Wno-unused-variable  -Wuninitialized
#FFLAGS		= -g -O0
#
# provide the name for the command timer here
TIMER		= time


all:		ciftbx.o hash_funcs.o tbx_ex tbx_exm tbx_ex3 testrle postshar

ciftbx.o:	ciftbx.f ciftbx.sys ciftbx.cmv clearfp.f
		$(FC) $(FFLAGS) -c ciftbx.f

hash_funcs.o:	hash_funcs.f
		$(FC) $(FFLAGS) -c hash_funcs.f

tbx_exm.o:	tbx_exm.f ciftbx.cmn ciftbx.cmv ciftbx.cmf
		$(FC) $(FFLAGS) -c tbx_exm.f

tbx_ex.o:       tbx_ex.f ciftbx.cmn ciftbx.f ciftbx.cmf ciftbx.cmf \
                hash_funcs.f clearfp.f
		$(FC) $(FFLAGS) -c tbx_ex.f

tbx_ex3.o:      tbx_ex3.f ciftbx.cmn ciftbx.f ciftbx.cmf ciftbx.cmf \
                hash_funcs.f clearfp.f
		$(FC) $(FFLAGS) -c tbx_ex3.f

testrle.o:      testrle.f ciftbx.cmn ciftbx.f ciftbx.cmf ciftbx.cmf \
                hash_funcs.f clearfp.f
		$(FC) $(FFLAGS) -c testrle.f

tbx_exm:	ciftbx.o hash_funcs.o tbx_exm.o ciftbx.cmn ciftbx.cmf \
		ciftbx.cmv
		$(FC) $(FFLAGS) tbx_exm.o ciftbx.o hash_funcs.o -o tbx_exm

tbx_ex:		tbx_ex.o
		$(FC) $(FFLAGS) tbx_ex.o  -o tbx_ex

tbx_ex3:	tbx_ex3.o
		$(FC) $(FFLAGS) tbx_ex3.o  -o tbx_ex3

testrle:	testrle.o
		$(FC) $(FFLAGS) testrle.o  -o testrle

clean:		
		-rm -f *.o
		-rm -f tbx_exm
		-rm -f tbx_ex
		-rm -f testrle
		-rm -f *.lst
		-rm -f *.new
		-rm -f *.diff
		-rm -f *.BAK
		-rm -f *.bak
		-$(RMDECOMPLN) *.uZ
		-$(RMDECOMPLN) .DECOMP/*.uZ

shars:		MANIFEST Makefile README.ciftbx \
	 	ciftbx.cmn  ciftbx.cmf ciftbx.cmv \
                ciftbx.f  ciftbx.sys clearfp.f clearfp_sun.f \
		hash_funcs.f mtest.out  mtest.prt mtest.xml\
		tbx_ex.f tbx_exm.f test.cif test.out test.prt test.req \
		testrle.f testrle.prt ../NOTICE ../COPYING \
		tbx_ex3.f m3test.out m3test.prt
		-rm -f ../Part*
		-rm -f ../ciftbx.cshar
		-rm -f ../ciftbx.shar
		-rm -f ../ciftbx.cshar.Z
		-rm -f ../ciftbx.shar.Z
		( cd .. ; makekit -c -iciftbx.src/MANIFEST \
		  -ociftbx.src/MANIFEST -h2 -p -s 1000k )
		mv ../Part01 ../ciftbx.cshar
		( cd .. ; makekit -iciftbx.src/MANIFEST \
		  -ociftbx.src/MANIFEST -h2 -p -s 1000k )
		mv ../Part01 ../ciftbx.shar
		-rm -f MANIFEST.BAK
		compress ../ciftbx.cshar
		compress ../ciftbx.shar

testrle.prt:
		touch testrle.prt

tests:		tbx_ex tbx_exm tbx_ex3 \
		  cif_mm.dic cif_core.dic cif_expanded.dic \
		  test.prt test.out mtest.prt mtest.out mtest.xml \
		  testrle testrle.prt
		@-rm -f *.lst
		@-rm -f *.new
		@-rm -f *.xew
		@-rm -f *.diff
		$(TIMER) ./tbx_ex > test.lst
		$(TIMER) ./tbx_exm > mtest.lst
		$(TIMER) ./tbx_ex3 > m3test.lst
		$(TIMER) ./testrle < cif_core.dic > testrle.lst
		-diff -b -c test.prt test.lst > test_prt_lst.diff
		-diff -b -c mtest.prt mtest.lst > mtest_prt_lst.diff
		-diff -b -c test.out test.new > test_out_new.diff
		-diff -b -c mtest.out mtest.new > mtest_out_new.diff
		-diff -b -c mtest.xml mtest.xew > mtest_xml_xew.diff
		-diff -b -c testrle.prt testrle.lst > testrle_prt_lst.diff
		-diff -b -c m3test.prt m3test.lst > m3test_prt_lst.diff
		-diff -b -c m3test.out m3test.new > m3test_out_new.diff
		cat *.diff			

cif_mm.dic:	$(MMDICPATH).Z
		$(LN) $(MMDICPATH).Z cif_mm.dic.Z
		$(MKDECOMPLN) cif_mm.dic $(EXPAND)

cif_core.dic:	$(COREDICPATH).Z
		$(LN) $(COREDICPATH).Z cif_core.dic.Z
		$(MKDECOMPLN) cif_core.dic $(EXPAND)
		
cif_expanded.dic:  $(CIFEXPPATH).Z
		$(LN) $(CIFEXPPATH).Z cif_expanded.dic.Z
		$(MKDECOMPLN) cif_expanded.dic $(EXPAND)

postshar:	$(MKDECOMPLN) $(RMDECOMPLN)
		chmod 755 $(MKDECOMPLN)
		chmod 755 $(RMDECOMPLN)
		touch postshar
