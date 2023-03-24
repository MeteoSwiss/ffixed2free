# Executable
EXE = ffixed2free

# Compiler and flags
FC = gfortran
CC = gcc
F90FLAGS = -g
CCFLAGS =

# Objects
FOBJECTS = count_substring.o ffixed2free.o m_ffixed2free.o m_do_loops.o \
	m_type_declaration.o m_indent_subroutines.o \
	m_linked_list_do_loops.o m_linked_list_indent_subroutines.o \
	m_stringpattern.o tolower.o toupper.o
COBJECTS = stringpatternc.o
OBJS = $(FOBJECTS) $(COBJECTS)

# suffixes
.SUFFIXES:
.SUFFIXES: .o .f90 .c

# implicit rules
.f90.o:
	@ echo "$(FC) -c $(F90FLAGS) $< "
	@ $(FC) -c $(F90FLAGS) $<
.c.o:
	@ echo "$(CC) -c $(CCFLAGS) $<"
	@ $(CC) -c $(CCFLAGS) $<

# Default target
# Executable
$(EXE): $(OBJS)
	$(FC) $(F90FLAGS) -o $(EXE) $(OBJS)

# Module dependencies
ffixed2free.f90: m_ffixed2free.o m_indent_subroutines.o m_type_declaration.o \
	m_do_loops.o
m_ffixed2free.o: count_substring.o m_stringpattern.o
m_do_loops.o: m_linked_list_do_loops.o m_stringpattern.o toupper.o
m_do_type_declaration.o: m_stringpattern.o toupper.o
m_indent_subroutines.o: m_linked_list_indent_subroutines.o \
	m_stringpattern.o tolower.o toupper.o
m_stringpattern.o: stringpatternc.o

# Target clean
clean:
	rm -f $(EXE) *.o *.oo *.mod
