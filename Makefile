IRPF90 = irpf90  #-a -d
FC     = ifort
#FCFLAGS= -O2
FCFLAGS= -g -O2 -axAVX  -traceback -C #-heap-arrays

SRC=
OBJ=
LIB= -mkl=sequential

include irpf90.make

irpf90.make: $(wildcard *.irp.f)
	$(IRPF90)
