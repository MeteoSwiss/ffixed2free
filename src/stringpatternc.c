/*
  Description:

  Fortran Wrapper for pattern matching.
  Check match of string <str> with regular expression <pattern>. In case of
  a match, return start and end indices <spos> and <epos> of the first match 
  (as Fortan indices) and zero, zero otherwise.

  M. Schraner, MeteoSwiss, 27 October 2011
*/
#include <stdio.h>
#include <string.h>
#include <regex.h>

int stringpatternc_(char* str, char* pattern, int* spos, int* epos)

  {
    int t;
    regex_t re;
    enum {nmatch=1}; // the maximum number of matches to record in match_ranges
    regmatch_t match_ranges[nmatch];

    if ((t=regcomp( &re, pattern, REG_EXTENDED )) != 0) {
      regerror(t, &re, str, sizeof str);
      fprintf(stderr,"grep: %s (%s)\n", str, pattern);
      return(0);
    }
    if( regexec( &re, str, nmatch, match_ranges, 0 ) == 0 ) {
      *spos = (int) match_ranges[0].rm_so + 1;
      *epos = (int) match_ranges[0].rm_eo;
    } else {
      *spos = 0;
      *epos = 0;     
    }

    regfree( &re );

    return(1);
  }
