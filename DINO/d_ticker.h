/* FILE NAME:   ticker.h

   Copyright (C) 1997-2016 Vladimir Makarov.

   Written by Vladimir Makarov <vmakarov@gcc.gnu.org>

   This is part of package for work with tickers; you can redistribute
   it and/or modify it under the terms of the GNU Library General
   Public License as published by the Free Software Foundation; either
   version 2, or (at your option) any later version.

   This software is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with GNU CC; see the file COPYING.  If not, write to
   the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
   MA 02111-1307, USA.

   TITLE:       Include file of package for work with tickers (timers)

   DESCRIPTION: This file contains type definition and ANSI C
       functions of the ticker.

*/

#ifndef __TICKER__

#define __TICKER__

#include <time.h>

/* The following structure describes a ticker. */

struct ticker {
  /* The following member value is time of the ticker creation with
     taking into account time when the ticker is off.  Active time of
     the ticker is current time minus the value. */
  clock_t modified_creation_time;
  /* The following member value is time (incremented by one) when the
     ticker was off.  Zero value means that now the ticker is on. */
  clock_t incremented_off_time;
};

/* The ticker is represented by the following type. */

typedef struct ticker ticker_t;

/* We don't use times or getrusage here because they have the same
   accuracy as clock on major machines Linux, Solaris, AIX.  The
   single difference is IRIX on which getrusage is more accurate. */

/* The following macro is necessary for non standard include files of
   SUNOS 4..., linux */

#ifndef CLOCKS_PER_SECOND
#ifdef CLOCKS_PER_SEC
#define CLOCKS_PER_SECOND CLOCKS_PER_SEC
#elif __linux__
#define CLOCKS_PER_SECOND 100
#elif sun
#define CLOCKS_PER_SECOND 1000000
#elif CLK_TCK
#define CLOCKS_PER_SECOND CLK_TCK
#else
#error define macro CLOCKS_PER_SECOND
#endif
#endif /* CLOCKS_PER_SECOND */

/* The following function creates ticker and makes it active. */
static inline ticker_t create_ticker (void) {
  ticker_t ticker;

  ticker.modified_creation_time = clock ();
  ticker.incremented_off_time = 0;
  return ticker;
}

/* The following function switches off given ticker. */
static inline void ticker_off (ticker_t *ticker) {
  if (ticker->incremented_off_time == 0) ticker->incremented_off_time = clock () + 1;
}

/* The following function switches on given ticker. */
static inline void ticker_on (ticker_t *ticker) {
  if (ticker->incremented_off_time != 0) {
    ticker->modified_creation_time += clock () - ticker->incremented_off_time + 1;
    ticker->incremented_off_time = 0;
  }
}

/* The following function returns current time since the moment when
   given ticker was created.  The result is measured in seconds as
   float number. */
static inline double active_time (ticker_t ticker) {
  if (ticker.incremented_off_time != 0)
    return (((double) (ticker.incremented_off_time - 1 - ticker.modified_creation_time))
            / CLOCKS_PER_SECOND);
  else
    return (((double) (clock () - ticker.modified_creation_time)) / CLOCKS_PER_SECOND);
}

/* The following function returns string representation of active time
   of given ticker.  The result is string representation of seconds
   with accuracy of 1/100 second.  Only result of the last call of the
   function exists.  Therefore the following code is not correct:
      printf ("parser time: %s\ngeneration time: %s\n",
              active_time_string (parser_ticker),
              active_time_string (generation_ticker));

   Correct code has to be the following:
      printf ("parser time: %s\n", active_time_string (parser_ticker));
      printf ("generation time: %s\n",
              active_time_string (generation_ticker));

*/
static inline const char *active_time_string (ticker_t ticker) {
  static char str[40];

  sprintf (str, "%.2f", active_time (ticker));
  return str;
}

#endif /* __TICKER__ */
