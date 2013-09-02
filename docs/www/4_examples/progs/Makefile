# ----------------------------------------------------------------
# Makefile for the Magics test suite
#
# It is the responsibility of the user to ensure that the proper
# environment is set up before running the test suite.
# ----------------------------------------------------------------

.PHONY: static shared-make shared-run clean-progs clean-mpp-only clean-html clean-ps clean



# --- the targets for building and running the test suite ----

#shared-make:
#	./setbin -shared
#	./compile_magplus -noodb -ps_gif               # compile & run only changed tests

#shared-run:
#	./setbin -shared
#	./compile_magplus -noodb -ps_gif -nocompilempp # run all tests

all: shared-quick c magml python
	
	
shared: #shared-make shared-run
	./setbin -shared
	./cleanmpponly
	./make_testsuite -nomag69 

shared-quick:
	./make_testsuite -nomag69 -quick -nocompilempp	
	
static:
	./setbin -static
	./cleanmpponly
#	./compile_magplus -static -noodb -ps_gif #  compile & run only changed tests
	./make_testsuite -quick -static

c:
	./make_c_testsuite

magml:
	./make_magml_testsuite

python:
	./make_python_testsuite


# -----------  the 'clean' targets  ----------------

clean-progs:
	./clean                      # removes all the binaries

clean-mpp-only:
	./cleanmpponly               # removes only the m++ binaries

clean-html:
	-rm -f ../../html/test/*   # removes any html pages

clean-ps:
	-rm -f ps/*                # removes all output files

clean:
	./clean                      # removes everything
	-rm -f ../../html/test/*/*.html
	-rm -f ../../html/test/*/*.log
	-rm -f ../../html/test/*/*.vlog
	-rm -f ../../html/test/*/*.time
	-rm -f ../../html/test/*/*.err
	-rm -f ps/*.ps
	-rm -f ps/*.gif
	-rm -f ps/*.pdf
