TARGET	= JACOBI_SOLVER
OBJS	= read_arg.o declare_var.o Jacobi_Solver.o main.o

FC  	= f95
FFLAGS = -free

OUTPUT_FILE = Test.dat


all: $(TARGET)

$(TARGET): $(OBJS)
	$(FC) $(FFLAGS) $(OBJS) -o $(TARGET)


clean:
	@/bin/rm -fv $(OBJS)
	@/bin/rm -fv declare_var.mod


run: $(OUTPUT_FILE)
	$(target)

.PHONY: run

%.o: %.f90
	$(FC) $(FFLAGS) -c $<

# dependencies
#
main.o : main.f90 
declare_var.o: declare_var.f90
Jacobi_Solver.o: Jacobi_Solver.f90
read_arg.o: read_arg.f90
