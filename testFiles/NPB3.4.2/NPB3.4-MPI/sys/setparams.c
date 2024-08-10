/* 
 * This utility configures a NPB to be built for a specific number
 * of nodes and a specific class. It creates a file "npbparams.h" 
 * in the source directory. This file keeps state information about 
 * which size of benchmark is currently being built (so that nothing
 * if unnecessarily rebuilt) and defines (through PARAMETER statements)
 * the number of nodes and class for which a benchmark is being built. 

 * The utility takes 2 arguments: 
 *       setparams benchmark-name class
 *    benchmark-name is "sp", "bt", etc
 *    class is the size of the benchmark
 * These parameters are checked for the current benchmark. If they
 * are invalid, this program prints a message and aborts. 
 * If the parameters are ok, the current npbsize.h (actually just
 * the first line) is read in. If the new parameters are the same as 
 * the old, nothing is done, but an exit code is returned to force the
 * user to specify (otherwise the make procedure succeeds but builds a
 * binary of the wrong name).  Otherwise the file is rewritten. 
 * Errors write a message (to stdout) and abort. 
 * 
 * This program makes use of two extra benchmark "classes"
 * class "X" means an invalid specification. It is returned if
 * there is an error parsing the config file. 
 * class "U" is an external specification meaning "unknown class"
 * 
 * Unfortunately everything has to be case sensitive. This is
 * because we can always convert lower to upper or v.v. but
 * can't feed this information back to the makefile, so typing
 * make CLASS=a and make CLASS=A will produce different binaries.
 *
 * 
 */

#include <sys/types.h>
#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <time.h>

/*
 * This is the master version number for this set of 
 * NPB benchmarks. It is in an obscure place so people
 * won't accidentally change it. 
 */

#define VERSION "3.4.2"

/* controls verbose output from setparams */
/* #define VERBOSE */

#define FILENAME "npbparams.h"
#define DESC_LINE "! CLASS = %c\n"
#define BT_DESC_LINE "! CLASS = %c SUBTYPE = %s\n"
#define DEF_CLASS_LINE     "#define CLASS '%c'\n"
#define FINDENT  "        "
#define CONTINUE "     & "

#ifdef FORTRAN_REC_SIZE
int fortran_rec_size = FORTRAN_REC_SIZE;
#else
int fortran_rec_size = 0;
#endif

void get_info(int argc, char *argv[], int *typep, char *classp,
	      int* subtypep);
void check_info(int type, char class);
void read_info(int type, char *classp, int *subtypep);
void write_info(int type, char class, int subtype);
void write_sp_info(FILE *fp, char class);
void write_bt_info(FILE *fp, char class, int io);
void write_lu_info(FILE *fp, char class);
void write_mg_info(FILE *fp, char class);
void write_cg_info(FILE *fp, char class);
void write_ft_info(FILE *fp, char class);
void write_ep_info(FILE *fp, char class);
void write_is_info(FILE *fp, char class);
void write_dt_info(FILE *fp, char class);
void write_compiler_info(int type, FILE *fp);
void write_convertdouble_info(int type, FILE *fp);
void check_line(char *line, char *label, char *val);
int  check_include_line(char *line, char *filename);
void put_string(FILE *fp, char *name, char *val);
void put_def_string(FILE *fp, char *name, char *val);
void put_def_variable(FILE *fp, char *name, char *val);
int isqrt(int i);
int ilog2(int i);
int ipow2(int i);
int isqrt2(int i);

enum benchmark_types {SP, BT, LU, MG, FT, IS, DT, EP, CG};
enum iotypes { NONE = 0, FULL, SIMPLE, EPIO, FORTRAN};

int main(int argc, char *argv[])
{
  int type;
  char class, class_old;
  int subtype = -1, old_subtype = -1;
  
  /* Get command line arguments. Make sure they're ok. */
  get_info(argc, argv, &type, &class, &subtype);
  if (class != 'U') {
#ifdef VERBOSE
    printf("setparams: For benchmark %s: class = %c\n", 
	   argv[1], class); 
#endif
    check_info(type, class);
  }

  /* Get old information. */
  read_info(type, &class_old, &old_subtype);
  if (class != 'U') {
    if (class_old != 'X') {
#ifdef VERBOSE
      printf("setparams:     old settings: class = %c\n", 
	     class_old); 
#endif
    }
  } else {
    printf("setparams:\n\
  ************************************************************\n\
  * You must specify CLASS to build this benchmark           *\n\
  * For example, to build a class A benchmark, type          *\n\
  *       make {benchmark-name} CLASS=A                      *\n\
  ************************************************************\n\n"); 

    if (class_old != 'X') {
#ifdef VERBOSE
      printf("setparams: Previous settings were CLASS=%c\n", 
	     class_old); 
#endif
    }
    exit(1); /* exit on class==U */
  }

  /* Write out new information if it's different. */
  if (class != class_old || subtype != old_subtype) {
#ifdef VERBOSE
    printf("setparams: Writing %s\n", FILENAME); 
#endif
    write_info(type, class, subtype);
  } else {
#ifdef VERBOSE
    printf("setparams: Settings unchanged. %s unmodified\n", FILENAME); 
#endif
  }

  return 0;
}


/*
 *  get_info(): Get parameters from command line 
 */

void get_info(int argc, char *argv[], int *typep, char *classp,
	      int *subtypep) 
{

  if (argc < 3) {
    printf("Usage: %s (%d) benchmark-name class\n", argv[0], argc);
    exit(1);
  }

  *classp = *argv[2];

  if      (!strcmp(argv[1], "sp") || !strcmp(argv[1], "SP")) *typep = SP;
  else if (!strcmp(argv[1], "ft") || !strcmp(argv[1], "FT")) *typep = FT;
  else if (!strcmp(argv[1], "lu") || !strcmp(argv[1], "LU")) *typep = LU;
  else if (!strcmp(argv[1], "mg") || !strcmp(argv[1], "MG")) *typep = MG;
  else if (!strcmp(argv[1], "is") || !strcmp(argv[1], "IS")) *typep = IS;
  else if (!strcmp(argv[1], "dt") || !strcmp(argv[1], "DT")) *typep = DT;
  else if (!strcmp(argv[1], "ep") || !strcmp(argv[1], "EP")) *typep = EP;
  else if (!strcmp(argv[1], "cg") || !strcmp(argv[1], "CG")) *typep = CG;
  else if (!strcmp(argv[1], "bt") || !strcmp(argv[1], "BT")) {
    *typep = BT;
    if (argc != 4) {
      /* printf("Usage: %s (%d) benchmark-name class\n", argv[0], argc); */
      /* exit(1); */
      *subtypep = NONE;
    } else {
      char *sstr = argv[3];
      if (!strcmp(sstr, "full") || !strcmp(sstr, "FULL")) {
        *subtypep = FULL;
      } else if (!strcmp(sstr, "simple") || !strcmp(sstr, "SIMPLE")) {
        *subtypep = SIMPLE;
      } else if (!strcmp(sstr, "epio") || !strcmp(sstr, "EPIO")) {
        *subtypep = EPIO;
      } else if (!strcmp(sstr, "fortran") || !strcmp(sstr, "FORTRAN")) {
        *subtypep = FORTRAN;
      } else if (!strcmp(sstr, "none") || !strcmp(sstr, "NONE")) {
        *subtypep = NONE;
      } else {
        printf("setparams: Error: unknown btio type %s\n", sstr);
        printf("valid types - full, simple, epio, fortran, none\n");
        exit(1);
      }
      if (*classp == 'F') {
        printf("setparams: Error: btio type %s not defined for class %c\n", 
               sstr, *classp);
        exit(1);
      }
    }
  } else {
    printf("setparams: Error: unknown benchmark type %s\n", argv[1]);
    exit(1);
  }
}

/*
 *  check_info(): Make sure command line data is ok for this benchmark 
 */

void check_info(int type, char class) 
{
  /* check class */
  if (class != 'S' && 
      class != 'W' && 
      class != 'A' && 
      class != 'B' && 
      class != 'C' && 
      class != 'D' && 
      class != 'E' && 
      class != 'F') {
    printf("setparams: Unknown benchmark class %c\n", class); 
    printf("setparams: Allowed classes are \"S\", \"W\", and \"A\" through \"F\"\n");
    exit(1);
  }

  if ((class == 'E' && type == DT) ||
      (class == 'F' && (type == IS || type == DT))) {
    printf("setparams: Benchmark class %c not defined for %s\n", 
           class, (type == IS)? "IS" : "DT");
    exit(1);
  }
}


/* 
 * read_info(): Read previous information from file. 
 *              Not an error if file doesn't exist, because this
 *              may be the first time we're running. 
 *              Assumes the first line of the file is in a special
 *              format that we understand (since we wrote it). 
 */

void read_info(int type, char *classp, int *subtypep)
{
  int nread = 0;
  FILE *fp;
  fp = fopen(FILENAME, "r");
  if (fp == NULL) {
#ifdef VERBOSE
    printf("setparams: INFO: configuration file %s does not exist (yet)\n", FILENAME); 
#endif
    goto abort;
  }
  
  /* first line of file contains info (fortran), first two lines (C) */

  switch(type) {
      case BT: {
	  char subtype_str[100];
          nread = fscanf(fp, BT_DESC_LINE, classp, subtype_str);
          if (nread != 2) {
            if (nread != 1) {
              printf("setparams: Error parsing config file %s. Ignoring previous settings\n", FILENAME);
              goto abort;
	    }
	    *subtypep = 0;
	    break;
          }
          if (!strcmp(subtype_str, "full") || !strcmp(subtype_str, "FULL")) {
		*subtypep = FULL;
          } else if (!strcmp(subtype_str, "simple") ||
		     !strcmp(subtype_str, "SIMPLE")) {
		*subtypep = SIMPLE;
          } else if (!strcmp(subtype_str, "epio") || !strcmp(subtype_str, "EPIO")) {
		*subtypep = EPIO;
          } else if (!strcmp(subtype_str, "fortran") ||
		     !strcmp(subtype_str, "FORTRAN")) {
		*subtypep = FORTRAN;
          } else {
		*subtypep = -1;
	  }
          break;
      }

      case SP:
      case FT:
      case MG:
      case LU:
      case EP:
      case CG:
          nread = fscanf(fp, DESC_LINE, classp);
          if (nread != 1) {
            printf("setparams: Error parsing config file %s. Ignoring previous settings\n", FILENAME);
            goto abort;
          }
          break;
      case IS:
      case DT:
          nread = fscanf(fp, DEF_CLASS_LINE, classp);
          if (nread != 1) {
            printf("setparams: Error parsing config file %s. Ignoring previous settings\n", FILENAME);
            goto abort;
          }
          break;
      default:
        /* never should have gotten this far with a bad name */
        printf("setparams: (Internal Error) Benchmark type %d unknown to this program\n", type); 
        exit(1);
  }

  fclose(fp);


  return;

 abort:
  *classp = 'X';
  *subtypep = -1;
  return;
}


/* 
 * write_info(): Write new information to config file. 
 *               First line is in a special format so we can read
 *               it in again. Then comes a warning. The rest is all
 *               specific to a particular benchmark. 
 */

void write_info(int type, char class, int subtype) 
{
  FILE *fp;
  char *BT_TYPES[] = {"NONE", "FULL", "SIMPLE", "EPIO", "FORTRAN"};

  fp = fopen(FILENAME, "w");
  if (fp == NULL) {
    printf("setparams: Can't open file %s for writing\n", FILENAME);
    exit(1);
  }

  switch(type) {
      case BT:
          /* Write out the header */
	  if (subtype == -1 || subtype == 0) {
            fprintf(fp, DESC_LINE, class);
	  } else {
            fprintf(fp, BT_DESC_LINE, class, BT_TYPES[subtype]);
	  }
          /* Print out a warning so bozos don't mess with the file */
          fprintf(fp, "\
!  \n\
!  \n\
!  This file is generated automatically by the setparams utility.\n\
!  It sets the number of processors and the class of the NPB\n\
!  in this directory. Do not modify it by hand.\n\
!  \n");

          break;
	
      case SP:
      case FT:
      case MG:
      case LU:
      case EP:
      case CG:
          /* Write out the header */
          fprintf(fp, DESC_LINE, class);
          /* Print out a warning so bozos don't mess with the file */
          fprintf(fp, "\
!  \n\
!  \n\
!  This file is generated automatically by the setparams utility.\n\
!  It sets the number of processors and the class of the NPB\n\
!  in this directory. Do not modify it by hand.\n\
!  \n");

          break;
      case IS:
      case DT:
          fprintf(fp, DEF_CLASS_LINE, class);
          fprintf(fp, "\
/*\n\
   This file is generated automatically by the setparams utility.\n\
   It sets the number of processors and the class of the NPB\n\
   in this directory. Do not modify it by hand.   */\n\
   \n");
          break;
      default:
          printf("setparams: (Internal error): Unknown benchmark type %d\n", 
                                                                         type);
          exit(1);
  }

  /* Now do benchmark-specific stuff */
  switch(type) {
  case SP:
    write_sp_info(fp, class);
    break;
  case LU:
    write_lu_info(fp, class);
    break;
  case MG:
    write_mg_info(fp, class);
    break;
  case IS:
    write_is_info(fp, class);  
    break;
  case DT:
    write_dt_info(fp, class);  
    break;
  case FT:
    write_ft_info(fp, class);
    break;
  case EP:
    write_ep_info(fp, class);
    break;
  case CG:
    write_cg_info(fp, class);
    break;
  case BT:
    write_bt_info(fp, class, subtype);
    break;
  default:
    printf("setparams: (Internal error): Unknown benchmark type %d\n", type);
    exit(1);
  }
  write_convertdouble_info(type, fp);
  write_compiler_info(type, fp);
  fclose(fp);
  return;
}


/* 
 * write_sp_info(): Write SP specific info to config file
 */

void write_sp_info(FILE *fp, char class) 
{
  int problem_size, niter;
  char *dt;

  if      (class == 'S') { problem_size = 12;  dt = "0.015d0";   niter = 100; }
  else if (class == 'W') { problem_size = 36;  dt = "0.0015d0";  niter = 400; }
  else if (class == 'A') { problem_size = 64;  dt = "0.0015d0";  niter = 400; }
  else if (class == 'B') { problem_size = 102; dt = "0.001d0";   niter = 400; }
  else if (class == 'C') { problem_size = 162; dt = "0.00067d0"; niter = 400; }
  else if (class == 'D') { problem_size = 408; dt = "0.00030d0"; niter = 500; }
  else if (class == 'E') { problem_size = 1020; dt = "0.0001d0"; niter = 500; }
  else if (class == 'F') { problem_size = 2560; dt = "0.15d-4";  niter = 500; }
  else {
    printf("setparams: Internal error: invalid class %c\n", class);
    exit(1);
  }
  fprintf(fp, "%sinteger problem_size, niter_default\n", FINDENT);
  fprintf(fp, "%sparameter (problem_size=%d, niter_default=%d)\n", 
	       FINDENT, problem_size, niter);
  fprintf(fp, "%sdouble precision dt_default\n", FINDENT);
  fprintf(fp, "%sparameter (dt_default = %s)\n", FINDENT, dt);
}
  
/* 
 * write_bt_info(): Write BT specific info to config file
 */

void write_bt_info(FILE *fp, char class, int io) 
{
  int problem_size, niter, wr_interval;
  char *dt;

  if      (class == 'S') { problem_size = 12;  dt = "0.010d0";    niter = 60;  }
  else if (class == 'W') { problem_size = 24;  dt = "0.0008d0";   niter = 200; }
  else if (class == 'A') { problem_size = 64;  dt = "0.0008d0";   niter = 200; }
  else if (class == 'B') { problem_size = 102; dt = "0.0003d0";   niter = 200; }
  else if (class == 'C') { problem_size = 162; dt = "0.0001d0";   niter = 200; }
  else if (class == 'D') { problem_size = 408; dt = "0.00002d0";  niter = 250; }
  else if (class == 'E') { problem_size = 1020; dt = "0.4d-5";    niter = 250; }
  else if (class == 'F') { problem_size = 2560; dt = "0.6d-6";    niter = 250; }
  else {
    printf("setparams: Internal error: invalid class %c\n", class);
    exit(1);
  }
  wr_interval = 5;
  fprintf(fp, "%sinteger problem_size, niter_default\n", FINDENT);
  fprintf(fp, "%sparameter (problem_size=%d, niter_default=%d)\n", 
	       FINDENT, problem_size, niter);
  fprintf(fp, "%sdouble precision dt_default\n", FINDENT);
  fprintf(fp, "%sparameter (dt_default = %s)\n", FINDENT, dt);
  fprintf(fp, "%sinteger wr_default\n", FINDENT);
  fprintf(fp, "%sparameter (wr_default = %d)\n", FINDENT, wr_interval);
  fprintf(fp, "%sinteger iotype\n", FINDENT);
  fprintf(fp, "%sparameter (iotype = %d)\n", FINDENT, io);
  if (io) {
    fprintf(fp, "%scharacter*(*) filenm\n", FINDENT);
    switch (io) {
	case FULL:
	    fprintf(fp, "%sparameter (filenm = 'btio.full.out')\n", FINDENT);
	    break;
	case SIMPLE:
	    fprintf(fp, "%sparameter (filenm = 'btio.simple.out')\n", FINDENT);
	    break;
	case EPIO:
	    fprintf(fp, "%sparameter (filenm = 'btio.epio.out')\n", FINDENT);
	    break;
	case FORTRAN:
	    fprintf(fp, "%sparameter (filenm = 'btio.fortran.out')\n", FINDENT);
	    fprintf(fp, "%sinteger fortran_rec_sz\n", FINDENT);
	    fprintf(fp, "%sparameter (fortran_rec_sz = %d)\n",
		    FINDENT, fortran_rec_size);
	    break;
	default:
	    break;
    }
  }
}
  


/* 
 * write_lu_info(): Write SP specific info to config file
 */

void write_lu_info(FILE *fp, char class) 
{
  int itmax, inorm, problem_size;
  char *dt_default;

  if      (class == 'S') { problem_size = 12;  dt_default = "0.5d0";  itmax = 50; }
  else if (class == 'W') { problem_size = 33;  dt_default = "1.5d-3"; itmax = 300; }
  else if (class == 'A') { problem_size = 64;  dt_default = "2.0d0";  itmax = 250; }
  else if (class == 'B') { problem_size = 102; dt_default = "2.0d0";  itmax = 250; }
  else if (class == 'C') { problem_size = 162; dt_default = "2.0d0";  itmax = 250; }
  else if (class == 'D') { problem_size = 408; dt_default = "1.0d0";  itmax = 300; }
  else if (class == 'E') { problem_size = 1020; dt_default = "0.5d0"; itmax = 300; }
  else if (class == 'F') { problem_size = 2560; dt_default = "0.2d0"; itmax = 300; }
  else {
    printf("setparams: Internal error: invalid class %c\n", class);
    exit(1);
  }
  inorm = itmax;

  fprintf(fp, "\n! full problem size\n");
  fprintf(fp, "%sinteger isiz01, isiz02, isiz03\n", FINDENT);
  fprintf(fp, "%sparameter (isiz01=%d, isiz02=%d, isiz03=%d)\n", 
	  FINDENT, problem_size, problem_size, problem_size);

  fprintf(fp, "\n! number of iterations and how often to print the norm\n");
  fprintf(fp, "%sinteger itmax_default, inorm_default\n", FINDENT);
  fprintf(fp, "%sparameter (itmax_default=%d, inorm_default=%d)\n", 
	  FINDENT, itmax, inorm);

  fprintf(fp, "%sdouble precision dt_default\n", FINDENT);
  fprintf(fp, "%sparameter (dt_default = %s)\n", FINDENT, dt_default);
  
}

/* 
 * write_mg_info(): Write MG specific info to config file
 */

void write_mg_info(FILE *fp, char class) 
{
  int problem_size, nit, log2_size, lt_default;

  if      (class == 'S') { problem_size = 32;   nit = 4; }
  else if (class == 'W') { problem_size = 128;  nit = 4; }
  else if (class == 'A') { problem_size = 256;  nit = 4; }
  else if (class == 'B') { problem_size = 256;  nit = 20; }
  else if (class == 'C') { problem_size = 512;  nit = 20; }
  else if (class == 'D') { problem_size = 1024; nit = 50; }
  else if (class == 'E') { problem_size = 2048; nit = 50; }
  else if (class == 'F') { problem_size = 4096; nit = 50; }
  else {
    printf("setparams: Internal error: invalid class type %c\n", class);
    exit(1);
  }
  log2_size = ilog2(problem_size);
  lt_default = log2_size;

  fprintf(fp, "%sinteger nx_default, ny_default, nz_default\n", FINDENT);
  fprintf(fp, "%sparameter (nx_default=%d, ny_default=%d, nz_default=%d)\n", 
	  FINDENT, problem_size, problem_size, problem_size);
  fprintf(fp, "%sinteger nit_default, lt_default\n", FINDENT);
  fprintf(fp, "%sparameter (nit_default=%d, lt_default=%d)\n", 
	  FINDENT, nit, lt_default);
  fprintf(fp, "%sinteger debug_default\n", FINDENT);
  fprintf(fp, "%sparameter (debug_default=%d)\n", FINDENT, 0);
}


/* 
 * write_dt_info(): Write DT specific info to config file
 */

void write_dt_info(FILE *fp, char class) 
{
  int num_samples,deviation,num_sources;
  if      (class == 'S') { num_samples=1728; deviation=128; num_sources=4; }
  else if (class == 'W') { num_samples=1728*8; deviation=128*2; num_sources=4*2; }
  else if (class == 'A') { num_samples=1728*64; deviation=128*4; num_sources=4*4; }
  else if (class == 'B') { num_samples=1728*512; deviation=128*8; num_sources=4*8; }
  else if (class == 'C') { num_samples=1728*4096; deviation=128*16; num_sources=4*16; }
  else if (class == 'D') { num_samples=1728*4096*8; deviation=128*32; num_sources=4*32; }
  else {
    printf("setparams: Internal error: invalid class type %c\n", class);
    exit(1);
  }
  fprintf(fp, "#define NUM_SAMPLES %d\n", num_samples);
  fprintf(fp, "#define STD_DEVIATION %d\n", deviation);
  fprintf(fp, "#define NUM_SOURCES %d\n", num_sources);
}

/* 
 * write_is_info(): Write IS specific info to config file
 */

void write_is_info(FILE *fp, char class) 
{
  if( class != 'S' &&
      class != 'W' &&
      class != 'A' &&
      class != 'B' &&
      class != 'C' &&
      class != 'D' &&
      class != 'E' )
  {
    printf("setparams: Internal error: invalid class type %c\n", class);
    exit(1);
  }
}

/* 
 * write_cg_info(): Write CG specific info to config file
 */

void write_cg_info(FILE *fp, char class) 
{
  int na,nonzer,niter;
  char *shift,*rcond="1.0d-1";

  if( class == 'S' )
  { na=1400;    nonzer=7;  niter=15;  shift="10."; }
  else if( class == 'W' )
  { na=7000;    nonzer=8;  niter=15;  shift="12."; }
  else if( class == 'A' )
  { na=14000;   nonzer=11; niter=15;  shift="20."; }
  else if( class == 'B' )
  { na=75000;   nonzer=13; niter=75;  shift="60."; }
  else if( class == 'C' )
  { na=150000;  nonzer=15; niter=75;  shift="110."; }
  else if( class == 'D' )
  { na=1500000; nonzer=21; niter=100; shift="500."; }
  else if( class == 'E' )
  { na=9000000; nonzer=26; niter=100; shift="1.5d3"; }
  else if( class == 'F' )
  { na=54000000; nonzer=31; niter=100; shift="5.0d3"; }
  else
  {
    printf("setparams: Internal error: invalid class type %c\n", class);
    exit(1);
  }
  fprintf( fp, "%sinteger            na, nonzer, niter\n", FINDENT );
  fprintf( fp, "%sdouble precision   shift, rcond\n", FINDENT );
  fprintf( fp, "%sparameter(  na=%d, &\n", FINDENT, na );
  fprintf( fp, "%s             nonzer=%d, &\n", CONTINUE, nonzer );
  fprintf( fp, "%s             niter=%d, &\n", CONTINUE, niter );
  fprintf( fp, "%s             shift=%s, &\n", CONTINUE, shift );
  fprintf( fp, "%s             rcond=%s )\n", CONTINUE, rcond );
}


/* 
 * write_ft_info(): Write FT specific info to config file
 */

void write_ft_info(FILE *fp, char class) 
{
  /* easiest way (given the way the benchmark is written)
   * is to specify log of number of grid points in each
   * direction m1, m2, m3. nt is the number of iterations
   */
  int nx, ny, nz, maxdim, niter;
  if      (class == 'S') { nx = 64;   ny = 64;   nz = 64;   niter = 6;}
  else if (class == 'W') { nx = 128;  ny = 128;  nz = 32;   niter = 6;}
  else if (class == 'A') { nx = 256;  ny = 256;  nz = 128;  niter = 6;}
  else if (class == 'B') { nx = 512;  ny = 256;  nz = 256;  niter =20;}
  else if (class == 'C') { nx = 512;  ny = 512;  nz = 512;  niter =20;}
  else if (class == 'D') { nx = 2048; ny = 1024; nz = 1024; niter =25;}
  else if (class == 'E') { nx = 4096; ny = 2048; nz = 2048; niter =25;}
  else if (class == 'F') { nx = 8192; ny = 4096; nz = 4096; niter =25;}
  else {
    printf("setparams: Internal error: invalid class type %c\n", class);
    exit(1);
  }
  maxdim = nx;
  if (ny > maxdim) maxdim = ny;
  if (nz > maxdim) maxdim = nz;
  fprintf(fp, "%sinteger nx, ny, nz, maxdim, niter_default\n", FINDENT);
  fprintf(fp, "%sparameter (nx=%d, ny=%d, nz=%d, maxdim=%d)\n", 
          FINDENT, nx, ny, nz, maxdim);
  fprintf(fp, "%sparameter (niter_default=%d)\n", FINDENT, niter);
}

/*
 * write_ep_info(): Write EP specific info to config file
 */

void write_ep_info(FILE *fp, char class)
{
  /* easiest way (given the way the benchmark is written)
   * is to specify log of number of grid points in each
   * direction m1, m2, m3. nt is the number of iterations
   */
  int m;
  if      (class == 'S') { m = 24; }
  else if (class == 'W') { m = 25; }
  else if (class == 'A') { m = 28; }
  else if (class == 'B') { m = 30; }
  else if (class == 'C') { m = 32; }
  else if (class == 'D') { m = 36; }
  else if (class == 'E') { m = 40; }
  else if (class == 'F') { m = 44; }
  else {
    printf("setparams: Internal error: invalid class type %c\n", class);
    exit(1);
  }
  /* number of processors given by "npm" */


  fprintf(fp, "%scharacter class\n",FINDENT);
  fprintf(fp, "%sparameter (class =\'%c\')\n",
                  FINDENT, class);
  fprintf(fp, "%sinteger m\n", FINDENT);
  fprintf(fp, "%sparameter (m=%d)\n",
          FINDENT, m);
}


/* 
 * This is a gross hack to allow the benchmarks to 
 * print out how they were compiled. Various other ways
 * of doing this have been tried and they all fail on
 * some machine - due to a broken "make" program, or
 * Fortran limitations, of whatever. Hopefully this will
 * always work because it uses very portable C. Unfortunately
 * it relies on parsing the make.def file - YUK. 
 * If your machine doesn't have <string.h> or <ctype.h>, happy hacking!
 * 
 */

#define VERBOSE
#define LL 400
#include <stdio.h>
#define DEFFILE "../config/make.def"
#define DEFAULT_MESSAGE "(none)"
FILE *deffile;
void write_compiler_info(int type, FILE *fp)
{
  char line[LL];
  char mpifc[LL], flink[LL], fmpi_lib[LL], fmpi_inc[LL], fflags[LL], flinkflags[LL];
  char compiletime[LL], randfile[LL];
  char mpicc[LL], cflags[LL], clink[LL], clinkflags[LL],
       cmpi_lib[LL], cmpi_inc[LL];
  struct tm *tmp;
  time_t t;
  deffile = fopen(DEFFILE, "r");
  if (deffile == NULL) {
    printf("\n\
setparams: File %s doesn't exist. To build the NAS benchmarks\n\
           you need to create is according to the instructions\n\
           in the README in the main directory and comments in \n\
           the file config/make.def.template\n", DEFFILE);
    exit(1);
  }
  strcpy(mpifc, DEFAULT_MESSAGE);
  strcpy(flink, DEFAULT_MESSAGE);
  strcpy(fmpi_lib, DEFAULT_MESSAGE);
  strcpy(fmpi_inc, DEFAULT_MESSAGE);
  strcpy(fflags, DEFAULT_MESSAGE);
  strcpy(flinkflags, DEFAULT_MESSAGE);
  strcpy(randfile, DEFAULT_MESSAGE);
  strcpy(mpicc, DEFAULT_MESSAGE);
  strcpy(cflags, DEFAULT_MESSAGE);
  strcpy(clink, DEFAULT_MESSAGE);
  strcpy(clinkflags, DEFAULT_MESSAGE);
  strcpy(cmpi_lib, DEFAULT_MESSAGE);
  strcpy(cmpi_inc, DEFAULT_MESSAGE);

  while (fgets(line, LL, deffile) != NULL) {
    if (*line == '#') continue;
    /* yes, this is inefficient. but it's simple! */
    check_line(line, "MPIFC", mpifc);
    check_line(line, "FLINK", flink);
    check_line(line, "FMPI_LIB", fmpi_lib);
    check_line(line, "FMPI_INC", fmpi_inc);
    check_line(line, "FFLAGS", fflags);
    check_line(line, "FLINKFLAGS", flinkflags);
    check_line(line, "RAND", randfile);
    check_line(line, "MPICC", mpicc);
    check_line(line, "CFLAGS", cflags);
    check_line(line, "CLINK", clink);
    check_line(line, "CLINKFLAGS", clinkflags);
    check_line(line, "CMPI_LIB", cmpi_lib);
    check_line(line, "CMPI_INC", cmpi_inc);
    /* if the dummy library is used by including make.dummy, we set the
       Fortran and C paths to libraries and headers accordingly     */
    if(check_include_line(line, "../config/make.dummy")) {
       strcpy(fmpi_lib, "-L../MPI_dummy -lmpi");
       strcpy(fmpi_inc, "-I../MPI_dummy");
       strcpy(cmpi_lib, "-L../MPI_dummy -lmpi");
       strcpy(cmpi_inc, "-I../MPI_dummy");
    }
  }

  
  (void) time(&t);
  tmp = localtime(&t);
  (void) strftime(compiletime, (size_t)LL, "%d %b %Y", tmp);


  switch(type) {
      case FT:
      case SP:
      case BT:
      case MG:
      case LU:
      case EP:
      case CG:
          put_string(fp, "compiletime", compiletime);
          put_string(fp, "npbversion", VERSION);
          put_string(fp, "cs1", mpifc);
          put_string(fp, "cs2", flink);
          put_string(fp, "cs3", fmpi_lib);
          put_string(fp, "cs4", fmpi_inc);
          put_string(fp, "cs5", fflags);
          put_string(fp, "cs6", flinkflags);
	  put_string(fp, "cs7", randfile);
          break;
      case IS:
      case DT:
          put_def_string(fp, "COMPILETIME", compiletime);
          put_def_string(fp, "NPBVERSION", VERSION);
          put_def_string(fp, "MPICC", mpicc);
          put_def_string(fp, "CFLAGS", cflags);
          put_def_string(fp, "CLINK", clink);
          put_def_string(fp, "CLINKFLAGS", clinkflags);
          put_def_string(fp, "CMPI_LIB", cmpi_lib);
          put_def_string(fp, "CMPI_INC", cmpi_inc);
          break;
      default:
          printf("setparams: (Internal error): Unknown benchmark type %d\n", 
                                                                         type);
          exit(1);
  }

}

void check_line(char *line, char *label, char *val)
{
  char *original_line;
  int n;
  original_line = line;
  /* compare beginning of line and label */
  while (*label != '\0' && *line == *label) {
    line++; label++; 
  }
  /* if *label is not EOS, we must have had a mismatch */
  if (*label != '\0') return;
  /* if *line is not a space, actual label is longer than test label */
  if (!isspace(*line) && *line != '=') return ; 
  /* skip over white space */
  while (isspace(*line)) line++;
  /* next char should be '=' */
  if (*line != '=') return;
  /* skip over white space */
  while (isspace(*++line));
  /* if EOS, nothing was specified */
  if (*line == '\0') return;
  /* finally we've come to the value */
  strcpy(val, line);
  /* chop off the newline at the end */
  n = strlen(val)-1;
  if (n >= 0 && val[n] == '\n')
    val[n--] = '\0';
  if (n >= 0 && val[n] == '\r')
    val[n--] = '\0';
  /* treat continuation */
  while (val[n] == '\\' && fgets(original_line, LL, deffile)) {
     line = original_line;
     while (isspace(*line)) line++;
     if (isspace(*original_line)) val[n++] = ' ';
     while (*line && *line != '\n' && *line != '\r' && n < LL-1)
       val[n++] = *line++;
     val[n] = '\0';
     n--;
  }
/*  if (val[strlen(val) - 1] == '\\') {
    printf("\n\
setparams: Error in file make.def. Because of the way in which\n\
           command line arguments are incorporated into the\n\
           executable benchmark, you can't have any continued\n\
           lines in the file make.def, that is, lines ending\n\
           with the character \"\\\". Although it may be ugly, \n\
           you should be able to reformat without continuation\n\
           lines. The offending line is\n\
  %s\n", original_line);
    exit(1);
  } */
}

int check_include_line(char *line, char *filename)
{
  char *include_string = "include";
  /* compare beginning of line and "include" */
  while (*include_string != '\0' && *line == *include_string) {
    line++; include_string++; 
  }
  /* if *include_string is not EOS, we must have had a mismatch */
  if (*include_string != '\0') return(0);
  /* if *line is not a space, first word is not "include" */
  if (!isspace(*line)) return(0); 
  /* skip over white space */
  while (isspace(*++line));
  /* if EOS, nothing was specified */
  if (*line == '\0') return(0);
  /* next keyword should be name of include file in *filename */
  while (*filename != '\0' && *line == *filename) {
    line++; filename++; 
  }  
  if (*filename != '\0' || 
      (*line != ' ' && *line != '\0' && *line !='\n')) return(0);
  else return(1);
}


#define MAXL 46
void put_string(FILE *fp, char *name, char *val)
{
  int len;
  len = strlen(val);
  if (len > MAXL) {
    val[MAXL] = '\0';
    val[MAXL-1] = '.';
    val[MAXL-2] = '.';
    val[MAXL-3] = '.';
    len = MAXL;
  }
  fprintf(fp, "%scharacter*%d %s\n", FINDENT, len, name);
  fprintf(fp, "%sparameter (%s=\'%s\')\n", FINDENT, name, val);
}

/* need to escape quote (") in val */
int fix_string_quote(char *val, char *newval, int maxl)
{
  int len;
  int i, j;
  len = strlen(val);
  i = j = 0;
  while (i < len && j < maxl) {
    if (val[i] == '"')
      newval[j++] = '\\';
    if (j < maxl)
      newval[j++] = val[i++];
  }
  newval[j] = '\0';
  return j;
}

/* NOTE: is the ... stuff necessary in C? */
void put_def_string(FILE *fp, char *name, char *val0)
{
  int len;
  char val[MAXL+3];
  len = fix_string_quote(val0, val, MAXL+2);
  if (len > MAXL) {
    val[MAXL] = '\0';
    val[MAXL-1] = '.';
    val[MAXL-2] = '.';
    val[MAXL-3] = '.';
    len = MAXL;
  }
  fprintf(fp, "#define %s \"%s\"\n", name, val);
}

void put_def_variable(FILE *fp, char *name, char *val)
{
  int len;
  len = strlen(val);
  if (len > MAXL) {
    val[MAXL] = '\0';
    val[MAXL-1] = '.';
    val[MAXL-2] = '.';
    val[MAXL-3] = '.';
    len = MAXL;
  }
  fprintf(fp, "#define %s %s\n", name, val);
}



#if 0

/* this version allows arbitrarily long lines but 
 * some compilers don't like that and they're rarely
 * useful 
 */

#define LINELEN 65
void put_string(FILE *fp, char *name, char *val)
{
  int len, nlines, pos, i;
  char line[100];
  len = strlen(val);
  nlines = len/LINELEN;
  if (nlines*LINELEN < len) nlines++;
  fprintf(fp, "%scharacter*%d %s\n", FINDENT, nlines*LINELEN, name);
  fprintf(fp, "%sparameter (%s = &\n", FINDENT, name);
  for (i = 0; i < nlines; i++) {
    pos = i*LINELEN;
    if (i == 0) fprintf(fp, "%s\'", CONTINUE);
    else        fprintf(fp, "%s", CONTINUE);
    /* number should be same as LINELEN */
    fprintf(fp, "%.65s", val+pos);
    if (i == nlines-1) fprintf(fp, "\')\n");
    else             fprintf(fp, " &\n");
  }
}

#endif


/* integer square root. Return error if argument isn't
 * a perfect square or is less than or equal to zero 
 */

int isqrt(int i)
{
  int root, square;
  if (i <= 0) return(-1);
  square = 0;
  for (root = 1; square <= i; root++) {
    square = root*root;
    if (square == i) return(root);
  }
  return(-1);
}

int isqrt2(int i)
{
  int xdim, ydim, square;
  if (i <= 0) return(-1);
  square = 0;
  for (xdim = 1; square <= i; xdim++) {
    square = xdim*xdim;
    if (square == i) return(xdim);
  }
  ydim = i / (--xdim);
  while (xdim*ydim != i && 2*ydim >= xdim) {
    xdim++;
    ydim = i / xdim;
  }
  if (xdim*ydim == i && 2*ydim >= xdim)
    return(xdim);
  return(-1);
}
  

/* integer log base two. Return error is argument isn't
 * a power of two or is less than or equal to zero 
 */

int ilog2(int i)
{
  int log2;
  int exp2 = 1;
  if (i <= 0) return(-1);

  for (log2 = 0; log2 < 30; log2++) {
    if (exp2 == i) return(log2);
    if (exp2 > i) break;
    exp2 *= 2;
  }
  return(-1);
}

int ipow2(int i)
{
  int pow2 = 1;
  if (i < 0) return(-1);
  if (i == 0) return(1);
  while(i--) pow2 *= 2;
  return(pow2);
}
 


void write_convertdouble_info(int type, FILE *fp)
{
  switch(type) {
  case SP:
  case BT:
  case LU:
  case FT:
  case MG:
  case EP:
  case CG:
    fprintf(fp, "%slogical  convertdouble\n", FINDENT);
#ifdef CONVERTDOUBLE
    fprintf(fp, "%sparameter (convertdouble = .true.)\n", FINDENT);
#else
    fprintf(fp, "%sparameter (convertdouble = .false.)\n", FINDENT);
#endif
    break;
  }
}
