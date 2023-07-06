ffixed2free
===========

Convert Fortran fixed-format source code to free-format source code

`ffixed2free` is a Fortran program to convert FORTRAN fixed-format source code
(FORTRAN 77 style) to free-format source code (Fortran 90 style).

Fixed-format source code is the old FORTRAN 77 format for source code,
with 6 blanks (or 5 blanks and continuation characters or a label)
at the beginning of each line.

Free-format source code is the newer Fortran 90 and later style source code,
which relies on commands and names being written without embedded blanks, and
the presence of blanks separating them - just the way any halfway decent
programmer would write code anyways.

We have not written that it converts FORTRAN77 to Fortran 90 on purpose - the
language standard and the format of the source files are two different
things. Not completely unrelated, but many programmers started using new Fortran
features while still writing fixed-format source code. `ffixed2fre` does not
require the fixed-format code to be restricted to the FORTRAN 77 standard.

Authors of `ffixed2free`: Pirmin Kaufmann and Martin Schraner, MeteoSwiss (at the
time of writing the code).


Compilation
-----------

Change to the src directory and compile the code with the help
of the `Makefile`. The latter assumes that `gfortran` is the command
to invoke the Fortran compiler.
```
cd src
make
```
The code has been tested with GNU Fortran (GCC) 8.3.0


Usage
-----

    ffixed2free file...

Further information
-------------------

The file [fortran_example.f](example/fortran_example.f) in the `example`
directory contains some FORTRAN 77 example code, suitable for testing
`ffidex2free` and giving some examples on how the code would be is converted.