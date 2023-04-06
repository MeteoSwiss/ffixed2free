# ffixed2free
Convert Fortran fixed format source code to free format source code
<<<<<<< HEADadasdasdadsaasdasadadadasdsds   
discard test
=======

Ffixed2free is a Fortran program to convert FORTRAN fixed-format source code
(the FORTRAN 77 style with 6 blank at the beginning of each line) to free-format
source code.

Fixed-format source code is the old FORTRAN 77 style source code with 6 blanks
(or 5 blanks and continuation characters or a label) at the beginning of each
line.

Free-format source code is the newer Fortran 90 and later style source code,
which relies on commands and names being written without embedded blanks, and
the presence of blank separating them - just the way any halfway decent
programmer would write code anyways.

I have not written that it converts FORTRAN77 to Fortran 90 on purpose - the
language standard and the format of the source files are two different
things. Not completely unrelated, but many programmers started using new Fortran
features while still writing fixed-format source code.

Authors of ffixed2free: Pirmin Kaufmann and Martin Schraner, MeteoSwiss (at the
time of writing the code).
>>>>>>> origin/import
