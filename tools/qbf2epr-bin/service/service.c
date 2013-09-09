/*
 This file is based on DepQBF. DepQBF, a solver for quantified 
 boolean formulae (QBF).        
 Copyright 2010, 2011, 2012 Florian Lonsing, Johannes Kepler University, Linz,
 Austria and Vienna University of Technology, Vienna, Austria.
 fmv.jku.at/depqbf

 DepService is free software: you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation, either version 3 of the License, or (at
 your option) any later version.

 DepService is distributed in the hope that it will be useful, but
 WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with DepQBF.  If not, see <http://www.gnu.org/licenses/>.
*/


#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <limits.h>
#include <dirent.h>
#include <assert.h>
#include <string.h>
#include <stdarg.h>
#include <signal.h>
#include <unistd.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>

#include "qdpll_exit.h"
#include "qdpll_config.h"
#include "qdpll.h"
#include "qdpll_dep_man_generic.h"
#include "qdpll_dep_man_qdag.h"
#include "qdpll_pcnf.h"
#include "qdpll_exit.h"
#include "qdpll_stack.h"
#include "qdpll_internals.h"


#define VERSION						\
  "depService\n"					\
  "Copyright 2012, Martina Seidl, Florian Lonsing,\n"	\
  "Johannes Kepler University, Linz, Austria \n"\



#define USAGE \
"usage: depService  <in-file> [ <port> ]\n"\





#define QDPLL_ABORT_APP(cond,msg) \
  do {                                  \
    if (cond)                                                                \
      {                                                                        \
        fprintf (stderr, "[QDPLL-APP] %s at line %d: %s\n", __func__,        \
                 __LINE__, msg);                                        \
        fflush (stderr);                                                \
        abort();                                                        \
      }                                                                        \
  } while (0)


int BSIZE = 255;
struct QDPLLApp
{
  struct
  {
    char *in_filename;
    FILE *in;
    int port;
    int trace;
    unsigned int print_usage;
    unsigned int print_version;

  } options;
};

typedef struct QDPLLApp QDPLLApp;

/* We keep a static pointer to the library object. Currently, this is
   used for calling library functions from within a signal handler. */
static QDPLL *qdpll = 0;

static int
isnumstr (char *str)
{
  /* Empty string is not considered as number-string. */
  if (!*str)
    return 0;
  char *p;
  for (p = str; *p; p++)
    {
      if (!isdigit (*p))
        return 0;
    }
  return 1;
}



static void
print_abort_err (QDPLLApp * app, char *msg, ...)
{
  va_list list;
  assert (msg != NULL);
  fprintf (stderr, "qdpll-app: ");
  va_start (list, msg);
  vfprintf (stderr, msg, list);
  va_end (list);
  fflush (stderr);
  abort ();
}

static void
parse_cmd_line_options (QDPLLApp * app, QDPLL * qdpll, int argc, char **argv)
{
  char *result;
  int opt_cnt;
  for (opt_cnt = 1; opt_cnt < argc; opt_cnt++)
    {
      char *opt_str = argv[opt_cnt];

      if (!strcmp (opt_str, "-h") || !strcmp (opt_str, "--help"))
        {
          app->options.print_usage = 1;
        }
      else if (!strcmp (opt_str, "--version"))
        {
          app->options.print_version = 1;
        }
      else if (isnumstr (opt_str))
        {
          app->options.port = atoi (opt_str);
          if (app->options.port < 1)
            {
              result = "Expecting non-zero value for max-time";
              print_abort_err (app, "%s!\n\n", result);
            }
        }      else if (!app->options.in_filename)
        {
          app->options.in_filename = opt_str;
          /* Check input file. */
          DIR *dir;
          if ((dir = opendir (app->options.in_filename)) != NULL)
            {
              closedir (dir);
              print_abort_err (app, "input file '%s' is a directory!\n\n",
                               app->options.in_filename);
            }
          FILE *input_file = fopen (app->options.in_filename, "r");
          if (!input_file)
            {
              print_abort_err (app, "could not open input file '%s'!\n\n",
                               app->options.in_filename);
            }
          else
            app->options.in = input_file;
        }
      else
        {
          print_abort_err (app, "unknown option '%s'!\n\n", opt_str);
	}
    }        

}




/* -------------------- START: PARSING -------------------- */
#define PARSER_READ_NUM(num, c)                        \
  assert (isdigit (c));                                \
  num = 0;					       \
  do						       \
    {						       \
      num = num * 10 + (c - '0');		       \
    }						       \
  while (isdigit ((c = getc (in))));

#define PARSER_SKIP_SPACE_DO_WHILE(c)		     \
  do						     \
    {                                                \
      c = getc (in);				     \
    }                                                \
  while (isspace (c));

#define PARSER_SKIP_SPACE_WHILE(c)		     \
  while (isspace (c))                                \
    c = getc (in);


static void
parse (QDPLLApp * app, QDPLL * qdpll, FILE * in, int trace)
{
  int col = 0, line = 0, neg = 0, preamble_found = 0;
  LitID num = 0;
  QDPLLQuantifierType scope_type = QDPLL_QTYPE_UNDEF;

  assert (in);

  char c;
  while ((c = getc (in)) != EOF)
    {
      PARSER_SKIP_SPACE_WHILE (c);

      while (c == 'c')
	{
	  while ((c = getc (in)) != '\n' && c != EOF)
	    ;
	  c = getc (in);
	}

      PARSER_SKIP_SPACE_WHILE (c);

      if (c == 'p')
	{
	  /* parse preamble */
	  PARSER_SKIP_SPACE_DO_WHILE (c);
	  if (c != 'c')
	    goto MALFORMED_PREAMBLE;
	  PARSER_SKIP_SPACE_DO_WHILE (c);
	  if (c != 'n')
	    goto MALFORMED_PREAMBLE;
	  PARSER_SKIP_SPACE_DO_WHILE (c);
	  if (c != 'f')
	    goto MALFORMED_PREAMBLE;
	  PARSER_SKIP_SPACE_DO_WHILE (c);
	  if (!isdigit (c))
	    goto MALFORMED_PREAMBLE;

	  /* read number of variables */
	  PARSER_READ_NUM (num, c);
	  if (trace == TRACE_QRP)
//	    fprintf (stdout, "p %s %u", trace == TRACE_QRP ? "qrp" : "bqrp", num);
      fprintf (stdout, "p qrp %u", num); //TODO
    else if (trace == TRACE_BQRP)
      fprintf (stdout, "p bqrp %u", num); //TODO

	  PARSER_SKIP_SPACE_WHILE (c);
	  if (!isdigit (c))
	    goto MALFORMED_PREAMBLE;

	  /* read number of clauses */
	  PARSER_READ_NUM (num, c);

	  /* NOTE: number of steps is number of orig. clauses, as we can't tell
	     the actual, future number of steps when tracing on-the-fly! */
	  if (trace)
	    fprintf (stdout, " %u%c", num, trace == TRACE_QRP ? '\n' : 0);

	  preamble_found = 1;
	  goto PARSE_SCOPE_OR_CLAUSE;

	MALFORMED_PREAMBLE:
	  QDPLL_ABORT_APP (1, "malformed preamble!\n");
	  return;
	}
      else
	{
	  QDPLL_ABORT_APP (1, "expecting preamble!\n");
	  return;
	}

    PARSE_SCOPE_OR_CLAUSE:

      PARSER_SKIP_SPACE_WHILE (c);

      if (c == 'a' || c == 'e')
	{
	  /* open a new scope */
	  if (c == 'a')
	    scope_type = QDPLL_QTYPE_FORALL;
	  else
	    scope_type = QDPLL_QTYPE_EXISTS;

	  qdpll_new_scope (qdpll, scope_type);

	  PARSER_SKIP_SPACE_DO_WHILE (c);
	}

      if (!isdigit (c) && c != '-')
	{
	  if (c == EOF)
	    return;
	  QDPLL_ABORT_APP (1, "expecting digit or '-'!\n");
	  return;
	}
      else
	{
	  if (c == '-')
	    {
	      neg = 1;
	      if (!isdigit ((c = getc (in))))
		{
		  QDPLL_ABORT_APP (1, "expecting digit!\n");
		  return;
		}
	    }

	  /* parse a literal or variable ID */
	  PARSER_READ_NUM (num, c);
	  num = neg ? -num : num;

	  qdpll_add (qdpll, num);

	  neg = 0;

	  goto PARSE_SCOPE_OR_CLAUSE;
	}
    }

  if (!preamble_found)
    QDPLL_ABORT_APP (1, "preamble missing!\n");
}

/* -------------------- END: PARSING -------------------- */




static void
set_default_options (QDPLLApp * app)
{
  app->options.in_filename = 0;
  app->options.in = stdin;
  app->options.print_usage = 0;
  app->options.port = 2020;
  app->options.trace = 0;
}


void
error (const char *msg)
{
  fprintf (stderr, "%s\n", msg);
  fflush (stderr);
  exit (1);
}


static void com_quit(int newsockfd) {
  int n = write (newsockfd, "OK", 2);
  if (n < 0)
    error ("ERROR writing to socket");
  close (newsockfd);
}

static void com_comp(int newsockfd, QDPLLDepManGeneric *dm) {
  char buffer[BSIZE + 1];

  int n = write (newsockfd, "OK\n", 3);
  if (n < 0)
    error ("ERROR writing to socket");


  memset (buffer, 0, BSIZE + 1);
  n = read (newsockfd, buffer, BSIZE);
  
  int v = atoi (buffer);
  n = write (newsockfd, "OK\n", 3);
  if (n < 0)
    error ("ERROR writing to socket");


  memset (buffer, 0, BSIZE + 1);
  n = read (newsockfd, buffer, BSIZE);
  int v2 = atoi (buffer);

  Var *x = qdpll->pcnf.vars + v;
  Var *y = qdpll->pcnf.vars + v2;

  if (((x->pos_occ_clauses.start) ||
       (x->neg_occ_clauses.start)) &&
      ((y->pos_occ_clauses.start) ||
       (y->neg_occ_clauses.start)) && (dm->depends (dm, v, v2) > 0))
    {
      n = write (newsockfd, "YES\n", 4);
      if (n < 0)
	error ("ERROR writing to socket");
    }
  else
    {
      n = write (newsockfd, "NO\n", 3);
      if (n < 0)
	error ("ERROR writing to socket");
    }

}


static void com_deps(int newsockfd, QDPLLDepManGeneric *dm) {

  char buffer[BSIZE + 1];
  int  n = write (newsockfd, "OK\n", 3);
  if (n < 0)
    error ("ERROR writing to socket");


  memset (buffer, 0, BSIZE + 1);
  n = read (newsockfd, buffer, BSIZE);
  int v2 = atoi (buffer);

  int stdo = dup (STDOUT_FILENO);
  dup2 (newsockfd, 1);
  qdpll_print_deps (qdpll, v2);
  printf("\n");
  fflush (stdout);
  dup2 (stdo, 1);
  fflush (stdout);
}

static void
start_deps_service (QDPLLApp * app, QDPLL * qdpll, int portno)
{
  /* socket management variables */

  int sockfd, newsockfd;
  socklen_t clilen;
  char buffer[BSIZE + 1];
  struct sockaddr_in serv_addr, cli_addr;
  int n;			// number of read tokens


  int v;			// variable to be checked 

  int v2, v3;			// auxiliary variables

  // recent scope and var for next
  Scope *scope = qdpll->pcnf.scopes.first;
  VarID *var = scope->vars.start;

  /* dependency manager */

  QDPLLDepManGeneric *dm = qdpll->dm;
  assert (dm);
  QDPLL_ABORT_APP (!dm->is_init (dm),
		   "dependency manager is not initialized!");


  sockfd = socket (AF_INET, SOCK_STREAM, 0);

  if (sockfd < 0)
    error ("ERROR opening socket");

  int on = 1;

  if (setsockopt (sockfd, SOL_SOCKET, SO_REUSEADDR, &on, sizeof (on)) < 0)
    {
      error ("setsockopt(SO_REUSEADDR) failed");
    }


  memset ((char *) &serv_addr, 0, sizeof (serv_addr));

  serv_addr.sin_family = AF_INET;
  serv_addr.sin_addr.s_addr = INADDR_ANY;
  serv_addr.sin_port = htons (portno);

  if (bind (sockfd, (struct sockaddr *) &serv_addr, sizeof (serv_addr)) < 0)
    error ("ERROR on binding");
  listen (sockfd, 5);
  clilen = sizeof (cli_addr);

  newsockfd = accept (sockfd, (struct sockaddr *) &cli_addr, &clilen);

  if (newsockfd < 0)
    error ("ERROR on accept");

						/*** implementation of communication protocol ***/

  while (1)
    {
      memset (buffer, 0, BSIZE + 1);
      n = read (newsockfd, buffer, BSIZE);

      if (n < 0)
	error ("ERROR reading from socket");


      if (!strncmp (buffer, "quit", 4))
	{			// exit and close connection
	  com_quit(newsockfd);
	  break;
	}
      if (!strncmp (buffer, "cmp", 3))
	{			// compare two variables
	  com_comp(newsockfd, dm);
	  continue;

	}
      if (!strncmp (buffer, "deps", 4))
	{			// get dependencies of a variable
	  com_deps(newsockfd,dm);
	  continue;
	}

    }
  close (sockfd);



}



static void
print_usage ()
{
  fprintf (stdout, USAGE);
}


static void
cleanup (QDPLL * qdpll, QDPLLApp * app)
{
  qdpll_delete (qdpll);

  if (app->options.in_filename)
    fclose (app->options.in);
}

static void
print_version ()
{
  fprintf (stderr, VERSION);
}



int
main (int argc, char **argv)
{
  QDPLLResult result = QDPLL_RESULT_UNKNOWN;
  QDPLLApp app;
  memset (&app, 0, sizeof (QDPLLApp));
  set_default_options (&app);

  qdpll = qdpll_create ();

  parse_cmd_line_options (&app, qdpll, argc, argv);
  qdpll_configure(qdpll,"--dep-man=qdag");

  if (app.options.print_usage)
    {
      print_usage ();
      cleanup (qdpll, &app);
      return result;
    }
 if (app.options.print_version)
    {
      print_version ();
      cleanup (qdpll, &app);
      return result;
    }



  parse (&app, qdpll, app.options.in, app.options.trace);
  qdpll_init_deps (qdpll);
  start_deps_service (&app, qdpll, app.options.port);


  cleanup (qdpll, &app);

  return result;
}
