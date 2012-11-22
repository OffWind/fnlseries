#----------------------------------------------------------------------#
# Makefile for FNLSERIES                                               #
#----------------------------------------------------------------------#
#
#
# INTEL compiler
CMF   =  ifort -c -O0
CMF_EX   = ifort -O0 -vec_report0

# GNU Compiler
# CMF   =  g95 -c -O0
# CMF_EX   = g95

fnlseries: fnlseries_mod.o fnlseries.o 
	$(CMF_EX) -o fnlseries fnlseries.o fnlseries_mod.o ; \mv fnlseries .. 

all:
	 make fnlseries

clean:
	rm -f *.o *.mod fnlseries *~

fnlseries_mod.o :  fnlseries_mod.f90
	$(CMF)  fnlseries_mod.f90

fnlseries.o :  fnlseries.f90
	$(CMF)  fnlseries.f90
