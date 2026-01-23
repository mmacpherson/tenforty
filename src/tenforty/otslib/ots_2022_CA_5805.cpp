#ifdef _MSC_VER
#include <string.h>
#define strcasecmp _stricmp
#define strncasecmp _strnicmp
#else
#include <strings.h>
#endif

#include <ctype.h>
#include <locale.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#define printf(...)
#define system(...)
namespace OpenTaxSolver2022 {
namespace taxsolve_CA_5805_2022 {

#define MAX_LINES 1000
/************************************************************************/
/* TaxSolve_Routines.c - General purpose reusable routines for making	*/
/*  tax programs.  These routines are not specific to any particular	*/
/*  tax form or country.  This file is usually compiled-with, linked-	*/
/*  with, or included-in a form-specific program.			*/
/* 									*/
/* Copyright (C) 2003-2023 - Aston Roberts				*/
/* 									*/
/* GNU Public License - GPL:						*/
/* This program is free software; you can redistribute it and/or	*/
/* modify it under the terms of the GNU General Public License as	*/
/* published by the Free Software Foundation; either version 2 of the	*/
/* License, or (at your option) any later version.			*/
/* 									*/
/* This program is distributed in the hope that it will be useful,	*/
/* but WITHOUT ANY WARRANTY; without even the implied warranty of	*/
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU	*/
/* General Public License for more details.				*/
/* 									*/
/* You should have received a copy of the GNU General Public License	*/
/* along with this program; if not, write to the Free Software		*/
/* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA		*/
/* 02111-1307 USA							*/
/* 									*/
/* Aston Roberts 1-1-2023	aston_roberts@yahoo.com			*/
/************************************************************************/



double L[MAX_LINES];	/* Declare the Line entry variables. */
char errmsg[15000];

FILE *infile=0,	 /* Main input file to be used for reading tax input data. */
     *outfile=0; /* Main output file. */
int verbose=0;	 /* Declare and set the "verbosity" flag. */
int notappvalue=0;
int single_line_entry=0;
int whole_line_entry=0;
int round_to_whole_dollars=0;	/* Option to use whole-dollars. */
int value_was_detected=0;

struct date_rec  /* Used by get_gain_and_losses  and  gen_date_rec */
{
  int month;
  int day;
  int year;
};


/********************************************************************************/
/* Input routines. 								*/
/********************************************************************************/

void show_errmsg( char *emsg )
{
 printf("%s\n", emsg );
 if (outfile != 0)
  fprintf(outfile,"%s\n", emsg );
}

int Round( double x )
{ int y; if (x<0.0) y = x - 0.5; else y = x + 0.5;  return y; }


double Conditional_Round( double x )
{	/* Round only if round_to_whole_dollars flag is set. */
  if (!round_to_whole_dollars)
   return x;
  else
   return Round( x );
}


void intercept_any_pragmas( FILE *infile, char *word );	/* Prototype. */
void consume_leading_trailing_whitespace( char *line );


/*------------------------------------------------------------------------------*/
/* Get_Word - Read next word from input file, while ignoring any comments.	*/
/*------------------------------------------------------------------------------*/
void get_word( FILE *infile, char *word )	/* Absorb comments. */
{
 int j=0;
 char ltc='\n';	 /* Line termination character. */
 char spc=' ';

 if (single_line_entry)
  ltc = ' ';
 if (whole_line_entry)
  spc='\n';
 do
  {  /*Absorb any leading white-space.*/
     word[j]=getc(infile);
     if (word[j]=='{')
      {
       do word[j]=getc(infile); while ((word[j]!='}') && (!feof(infile)));
       word[j]=getc(infile);
      }
  }
 while ((!feof(infile)) && ((word[j]==' ') || (word[j]=='\t') || (word[j]==ltc) || (word[j]=='\r')));
 if (word[j]=='$')
  word[j] = getc(infile);
 if (word[j]==';')
  j++;
 else
 if (word[j]=='\n')
  word[j] = '\0';	/* Terminate empty single-line entries. */
 else
 if (word[j]=='"')
  { /* Get quoted string. */
    j = 0;
    do
     word[j++] = getc(infile);
    while ((word[j-1] != '"') && (!feof(infile)));
    if (word[j-1] == '"') j--;	/* Remove trailing quote. */
  }
 else
  { /* Normal case. */
   do {	/*Get word until white-space or ;.*/
        j++;  word[j] = getc(infile);
        if (word[j]=='{')
	 {
	  do { word[j] = getc(infile); }
	  while ((!feof(infile)) && (word[j] != '}'));
	  word[j] = ' ';
	 }
	if (word[j]==',') word[j] = getc(infile);
      }
   while ((!feof(infile)) && ((word[j]!=spc) && (word[j]!='\t') && (word[j]!='\n') && (word[j] !='\r') && (word[j]!=';')));
   if (word[j]==';') ungetc(word[j],infile);
  }
 word[j] = '\0';	/* Add termination character. */
 if (verbose) printf("Read: '%s'\n", word);
 intercept_any_pragmas( infile, word );	/* Intercept any pragmas. */
}


#ifdef microsoft	   /* Apparently Microsoft doesn't know of strcasecmp(), define one. */
int strcasecmp( char *str1, char *str2 )
{
 char *tstr1, *tstr2;  int i=0;
 tstr1 = (char *)malloc(strlen(str1+1)*sizeof(char));
 do { tstr1[i] = toupper(str1[i]); i++; } while (str1[i-1]!='\0');
 tstr2 = (char *)malloc(strlen(str2+1)*sizeof(char));
 i = 0;
 do { tstr2[i] = toupper(str2[i]); i++; } while (str2[i-1]!='\0');
 i = strcmp(tstr1,tstr2);
 free(tstr1); free(tstr2);
 return i;
}
int strncasecmp( char *str1, char *str2, int len )
{
 char *tstr1, *tstr2;  int i=0;
 tstr1 = (char *)malloc(strlen(str1+1)*sizeof(char));
 do { tstr1[i] = toupper(str1[i]); i++; } while ((str1[i-1]!='\0') && (i<len));
 tstr2 = (char *)malloc(strlen(str2+1)*sizeof(char));
 i = 0;
 do { tstr2[i] = toupper(str2[i]); i++; } while ((str2[i-1]!='\0') && (i<len));
 i = strcmp(tstr1,tstr2);
 free(tstr1); free(tstr2);
 return i;
}
#endif

char *mystrcasestr( char *haystack, char *needle )
{
 int j=0;
 char *hs, *ndl, *pt;
 hs = strdup( haystack );
 while (hs[j] != '\0') { hs[j] = toupper( hs[j] );  j++; }
 ndl = strdup( needle );
 j = 0;
 while (ndl[j] != '\0') { ndl[j] = toupper( ndl[j] );  j++; }
 pt = strstr( hs, ndl );
 if (pt != 0)
  {
   j = 0;
   while (pt != &(hs[j])) j++;
   pt = &(haystack[j]);
  }
 free( ndl );
 free( hs );
 return pt;
}


void intercept_any_pragmas( FILE *infile, char *word )	/* Intercept any special command pragmas. */
{
 if (strcmp( word, "Round_to_Whole_Dollars" ) == 0)	/* Intercept any mode-setting commands. */
  {
   round_to_whole_dollars = 1;
   get_word( infile, word );
  }
}


int valid_int( char *word )	/* Check for a valid integer in string, and nothing else following it. */
{	/* Returns 1 if valid, 0 if invalid. */
 int j=0, state=0;
 while (word[j] != '\0')
  {
   switch( state )
    {
     case 0: if ((word[j] == '-') || (word[j] == '+'))
		state = 1;
	     else
	     if ((word[j] >= '0') && (word[j] <= '9'))
		state = 2;
	     else
		return 0;
	break;
      case 1: if ((word[j] >= '0') && (word[j] <= '9'))
                state = 2;
             else
		return 0;
	break;
      case 2: if ((word[j] >= '0') && (word[j] <= '9'))
		state = 2;
	      else
	      if (word[j] == '.')
		state = 3;
	      else
		return 0;
	break;
      case 3:  return 0;
	break;
      default:  return 0;
    }
   j++;
  }
 if (j != 0)
  return 1;
 else
  return 0;
}


int valid_float( char *word )	/* Check for a valid decimal value in string, and nothing else following it. */
{	/* Returns 1 if valid, 0 if invalid. */
 int j=0, state=0;
 while (word[j] != '\0')
  {
   switch( state )
    {
     case 0: if ((word[j] == '-') || (word[j] == '+'))
		state = 1;
	     else
	     if ((word[j] >= '0') && (word[j] <= '9'))
		state = 2;
	     else
	     if (word[j] == '.')
		state = 3;
	     else
		return 0;
	break;
      case 1: if ((word[j] >= '0') && (word[j] <= '9'))
                state = 2;
             else
		return 0;
	break;
      case 2: if ((word[j] >= '0') && (word[j] <= '9'))
		state = 2;
	      else
	      if (word[j] == '.')
		state = 3;
	      else
	      if ((word[j] == 'e') || (word[j] == 'E'))
		state = 4;
	      else
		return 0;
	break;
      case 3: if ((word[j] >= '0') && (word[j] <= '9'))
		state = 3;
	      else
	      if ((word[j] == 'e') || (word[j] == 'E'))
		state = 4;
	      else
		return 0;
	break;
      case 4: if ((word[j] >= '0') && (word[j] <= '9'))
		state = 5;
	      else
      	      if ((word[j] == '-') || (word[j] == '+'))
		state = 5;
	      else
		return 0;
        break;
      case 5:  if ((word[j] >= '0') && (word[j] <= '9'))
                state = 5;
              else
		return 0;
	break;
      default:  return 0;
    }
   j++;
  }
 if (j != 0)
  return 1;
 else
  return 0;
}


/*------------------------------------------------------------------------------*/
/* Get Parameter - Get a single value.						*/
/*   Expect value kinds:  'i'=integer, 'f'=float, 's'=string, 'b'=boolean.	*/
/*------------------------------------------------------------------------------*/
void get_parameter( FILE *infile, char kind, void *x, char *emssg )
{
 char word[4096], *owrd;
 int i, *ii;
 double y, *yy;

 if (kind=='w')
  { single_line_entry = 1;  whole_line_entry = 1; }
 value_was_detected = 0;

 get_word(infile, word);

 if (feof(infile))
  {
   printf("ERROR: Unexpected EOF on '%s'\n",emssg);
   if (outfile) fprintf(outfile,"ERROR: Unexpected EOF on '%s'\n",emssg);
   exit(1);
  }
 if (word[0] != '\0')
  value_was_detected = 1;
 if (kind=='i')
  {
   if ((!valid_int(word)) || (sscanf(word,"%d",&i)!=1))
    {printf("ERROR: Bad integer '%s', reading %s.\n", word, emssg); fprintf(outfile,"ERROR: Bad integer '%s', reading %s.\n", word, emssg); exit(1); }
   ii = (int *)x;
   *ii = i;
  }
 else
 if (kind=='f')
  {
   if ((!valid_float(word)) || (sscanf(word,"%lf",&y)!=1))
    {printf("ERROR: Bad float '%s', reading %s.\n", word, emssg); fprintf(outfile,"ERROR: Bad float '%s', reading %s.\n", word, emssg); exit(1); }
   if (round_to_whole_dollars)
    y = Round( y );
   yy = (double *)x;
   *yy = y;
  }
 else
 if (kind=='s')
  {
   owrd = (char *)x;
   strcpy( owrd, word );
   if (emssg[0]!='\0')
    { if (strcmp(word,emssg)!=0)
       {printf("ERROR1: Found '%s' when expecting '%s'\n", word, emssg); fprintf(outfile,"ERROR1: Found '%s' when expecting '%s'\n", word, emssg); exit(1); }
    }
  }
 else
 if (kind=='w')
  {
   owrd = (char *)x;
   owrd[0] = '\0';
   strcat( owrd, word );
   strcat( owrd, " " );
   single_line_entry = 0;
   whole_line_entry = 0;
  }
 else
 if (kind=='l')		/* Literal string. Do not check for match. */
  {
   owrd = (char *)x;
   strcpy( owrd, word );
  }
 else
 if (kind=='b')
  {
   if (strcasecmp(word,"y") == 0) i = 1;  else  if (strcasecmp(word,"n") == 0) i = 0;
   else
   if ((strcasecmp(word,"TRUE")==0) || (strcasecmp(word,"YES")==0) || (strcasecmp(word,"Y")==0) || (strcmp(word,"1")==0))
    i = 1;
   else
   if ((strcasecmp(word,"FALSE")==0) || (strcasecmp(word,"NO")==0) || (strcasecmp(word,"N")==0) || (strcmp(word,"0")==0))
    i = 0;
   else if (strcasecmp(word,"N/A")==0) i = notappvalue;
   else if ((single_line_entry) && (strlen( word ) == 0)) i = notappvalue;
   else {printf("ERROR1: Bad boolean '%s', reading %s.\n", word, emssg); fprintf(outfile,"ERROR: Bad boolean '%s', reading %s.\n", word, emssg); exit(1);}
   ii = (int *)x;
   *ii = i;
  }
 else
  {printf("ERROR: Unknown type '%c'\n", kind); fprintf(outfile,"ERROR: Unknown type '%c'\n", kind); exit(1);}
}


void get_param_single_line( FILE *infile, char kind, void *x, char *emssg )
{
 single_line_entry = 1;
 get_parameter( infile, kind, x, emssg );
 single_line_entry = 0;
}



/*------------------------------------------------------------------------------*/
/* Get Parameters - Get sum of list of values terminated by ";".		*/
/*   Expect value kinds:  'i'=integer, 'f'=float, 's'=string, 'b'=boolean.	*/
/*------------------------------------------------------------------------------*/
void get_parameters( FILE *infile, char kind, void *x, char *emssg )
{
 char word[2048], *owrd=0;
 int j, *ii;
 double y, *yy;

 if (kind == 'f') { yy = (double *)x;  *yy = 0.0; }
 else
 if (kind == 'w') { owrd = (char *)x;  owrd[0] = '\0'; }
 value_was_detected = 0;

 get_word(infile,word);
 while (word[0] != ';')
 {
 if (feof(infile))
  {printf("ERROR: Unexpected EOF on '%s'\n",emssg); fprintf(outfile,"ERROR: Unexpected EOF on '%s'\n",emssg); exit(1);}
 if (kind=='i')
  {
   if ((!valid_int(word)) || (sscanf(word,"%d",&j)!=1))
    {printf("ERROR: Bad integer '%s', reading %s.\n", word, emssg); fprintf(outfile,"ERROR: Bad integer '%s', reading %s.\n", word, emssg); exit(1); }
   ii = (int *)x;
   *ii = j;
  }
 else
 if (kind=='f')
  {
   if ((!valid_float(word)) || ((sscanf(word,"%lf",&y))!=1))
    {printf("ERROR: Bad float '%s', reading %s.\n", word, emssg); fprintf(outfile,"ERROR: Bad float '%s', reading %s.\n", word, emssg); exit(1); }
   if (round_to_whole_dollars)
    y = Round( y );
   yy = (double *)x;
   *yy = *yy + y;
   /*  printf("	+ %f = %f\n", y, *yy); */
  }
 else
 if (kind=='s')
  {
   owrd = (char *)x;
   strcpy( owrd, word );
   if (emssg[0]!='\0')
    { if (strcmp(word,emssg)!=0)
       {printf("ERROR2: Found '%s' when expecting '%s'\n", word, emssg); fprintf(outfile,"ERROR2: Found '%s' when expecting '%s'\n", word, emssg); exit(1); }
    }
  }
 else
 if (kind=='w')
  {
   strcat( owrd, word );
   strcat( owrd, " " );
  }
 else
 if (kind=='b')
  {
   if ((strcasecmp(word,"TRUE")==0) || (strcasecmp(word,"YES")==0) || (strcasecmp(word,"Y")==0) || (strcmp(word,"1")==0))
	j = 1;
   else
   if ((strcasecmp(word,"FALSE")==0) || (strcasecmp(word,"NO")==0) || (strcasecmp(word,"N")==0) || (strcmp(word,"0")==0))
	j = 0;
   else
   if (strcasecmp(word,"n/a")==0)
     {
	get_word(infile,word);
	ii = (int *)x;
	*ii = notappvalue;
	return;
     }
   else
    {printf("ERROR2: Bad boolean '%s', reading %s.\n", word, emssg);
     fprintf(outfile,"ERROR: Bad boolean '%s', reading %s.\n", word, emssg);
     exit(1);
    }
   ii = (int *)x;
   *ii = j;
  }
 else
  {printf("ERROR: Unknown type '%c'\n", kind); fprintf(outfile,"ERROR: Unknown type '%c'\n", kind); exit(1);}
 value_was_detected++;
 get_word(infile,word);
 }
}




/*.......................................................................
  .     NEXT_WORD - accepts a line of text, and returns with the        .
  . next word in that text in the third parameter, the original line    .
  . is shortened from the beginning so that the word is removed.        .
  . If the line encountered is empty, then the word returned will be    .
  . empty.                                                              .
  . NEXTWORD can parse on an arbitrary number of delimiters, and it 	.
  . returns everthing that was cut away in the second parameter.	.
  . Parameters:								.
  .   line - input character string, on output shortened by word.	.
  .   word - output of this routine, single word, without delimiters.   .
  .   delim - list of delimiters, whitepace chars, etc..		.
  .......................................................................*/
void next_word( char *line, char *word, char *delim )
{
 int i=0, j=0, m=0, flag=1;

 /* Eat away preceding garbage */
 while ((line[i] !='\0') && (flag))
  {
   j = 0;
   while ((delim[j] != '\0') && (line[i] != delim[j])) j = j + 1;
   if (line[i] == delim[j]) i++;
   else  flag = 0;
  }
 while ((line[i] != '\0') && (!flag))
  {
   word[m++] = line[i++];
   if (line[i] != '\0')
    {
     j = 0;
     while ((delim[j] != '\0') && (line[i] != delim[j])) j = j + 1;
     if (line[i] == delim[j]) flag = 1;
    }
  }
 /* Shorten line. */
 j = 0;
 while (line[i]!='\0') { line[j++] = line[i++]; }
 /* Terminate the char-strings. */
 line[j] = '\0';
 word[m] = '\0';
}


/***************************************************************************/
/* Next_CSV - 								   */
/* Parse a line of text which is either comma-delimited, or tab-delimited. */
/* Return the next item, and remove the item to shorten the string. 	   */
/***************************************************************************/
void next_csv( char *line, char *word, char delim )
{ /* Expect delim to be either ',' (comma) for CSV, or '\t' (tab) for Tab-Delimited. */
 int j=0, k=0, m=0;

 /* Consume any preceding white-space */
 while ((line[j] !='\0') && ((line[j] == ' ') || (line[j] == '\t') || (line[j] == '\n') || (line[j] == '\r')))
  j++;
 /* Grab the next item until delimiter. */
 while ((line[j] != '\0') && (line[j] != delim))
  {
   if (line[j] == '"')	/* Accept a double-quoted phrase. */
    {
     do { j++;  word[m++] = line[j]; }
     while ((line[j] != '\0') && (line[j] != '"'));
     if (line[j] == '"') { j++;  m--; }
    }
   else
   if (line[j] == '\'')	/* Accept a single-quoted phrase. */
    {
     do { j++;  word[m++] = line[j]; }
     while ((line[j] != '\0') && (line[j] != '\''));
     if (line[j] == '\'') { j++;  m--; }
    }
   else
    word[m++] = line[j++];
  }
 if (line[j] == delim)
  j++;	/* Advance past next delimiter. */
 /* Shorten line. */
 k = 0;
 while (line[j] != '\0') { line[k++] = line[j++]; }
 /* Terminate the char-strings. */
 line[k] = '\0';
 word[m] = '\0';
}


struct date_record
 {
   int month, day, year;
 } yourDOB, spouseDOB, DL;


char *format_mmddyyyy( int month, int day, int year )
{
 char datestr[100], dd[20], yy[20];
 if (month < 10) sprintf(datestr, "0%d", month );  else  sprintf(datestr, "%d", month );
 if (day < 10)  sprintf(dd, "0%d", day );  else    sprintf(dd, "%d", day );
 strcat( datestr, dd );
 sprintf( yy, "%d", year );
 strcat( datestr, yy );
 return strdup( datestr );
}


int isleapyear (int year)  /* Used by interpret_date and get_gain_and_losses */
{ /* Returns 1 on TRUE.  0 on false. */
 if (((year % 4 == 0) && (year % 100!= 0)) || (year%400 == 0) )
  return 1;
 else
  return 0;
}


/* Handy routine for interpreting dates in various formats. */
int interpret_date( char *datestr, int *month, int *day, int *year, char *emssg )
{ /* Returns 1 on success. 0 on failure. */
 char word1[500], *owrd;
 int monthdays;
 int std_days[13]={0,31,28,31,30,31,30,31,31,30,31,30,31};  /* Array of Days in each month */
  /* Expect month-day-year as in: 3-3-01, Feb 3, 2022, or 3/3/2008, etc. */
 owrd = strdup( datestr);
 next_word( owrd, word1, " /,-\t\n\r" );
 if (strncasecmp( word1, "Jan", 3 ) == 0)  *month = 1;  else
 if (strncasecmp( word1, "Feb", 3 ) == 0)  *month = 2;  else
 if (strncasecmp( word1, "Mar", 3 ) == 0)  *month = 3;  else
 if (strncasecmp( word1, "Apr", 3 ) == 0)  *month = 4;  else
 if (strncasecmp( word1, "May", 3 ) == 0)  *month = 5;  else
 if (strncasecmp( word1, "Jun", 3 ) == 0)  *month = 6;  else
 if (strncasecmp( word1, "Jul", 3 ) == 0)  *month = 7;  else
 if (strncasecmp( word1, "Aug", 3 ) == 0)  *month = 8;  else
 if (strncasecmp( word1, "Sep", 3 ) == 0)  *month = 9;  else
 if (strncasecmp( word1, "Oct", 3 ) == 0)  *month = 10;  else
 if (strncasecmp( word1, "Nov", 3 ) == 0)  *month = 11;  else
 if (strncasecmp( word1, "Dec", 3 ) == 0)  *month = 12;  else
 if ((sscanf( word1, "%d", month) != 1) || (*month < 1) || (*month > 12))
  {printf("DATA ERROR: Bad month '%s' in '%s' at '%s'\n", word1, datestr, emssg );
   fprintf(outfile,"DATA ERROR: Bad month '%s' in '%s' at '%s'\n", word1, datestr, emssg );
   return 0;
  }
 next_word( owrd, word1, " /,-\t\n\r" );
 if ((sscanf( word1, "%d", day) != 1) || (*day < 1) )
  {printf("DATA ERROR: Bad day '%s' in '%s' at '%s'\n", word1, datestr, emssg );
   fprintf(outfile,"DATA ERROR: Bad day '%s' in '%s' at '%s'\n", word1, datestr, emssg );
   return 0;
  }
 next_word( owrd, word1, " /,-\t\n\r" );
 if ((sscanf( word1, "%d", year) != 1) || (*year < 0) || (*year > 3000))
  {printf("DATA ERROR: Bad year '%s' in '%s' at '%s'\n", word1, datestr, emssg );
   fprintf(outfile,"DATA ERROR: Bad year '%s' in '%s' at '%s'\n", word1, datestr, emssg );
   return 0;
  }
 free( owrd );
 if (*year < 40)	/* Convert any 2-digit year to four digits. */
   *year = *year + 2000;  /* Assumes any 2-digit years are after 1940. */
 else
 if (*year < 1900)
  *year = *year + 1900;
 /* Enhanced Day Validity Check,  Check for February in a Leap Year */
 if ((*month == 2) && (isleapyear ( *year)) ) monthdays=29;
  else monthdays=std_days[*month];
 if (*day > monthdays)
  {printf("DATA ERROR: Invalid day '%d' in '%s' at '%s'\n", *day, datestr, emssg );
   fprintf(outfile,"DATA ERROR: Invalid day '%d' in '%s' at '%s'\n", *day, datestr, emssg );
   return 0;
  }
 return 1;
}


/**********************************************************************************************/
/* gen_date_rec - Input is "datestr". Generates a record (structure variable)                 */
/*   "date_rec" with numerical (integer) month, day, year.                   	              */
/* Date Record used in get_gain_and_losses function to determine if holding period is greater */
/* than one Calendar Year and for sell-before-buy check.				      */
/**********************************************************************************************/
void gen_date_rec(char *datestr, char *emssg, struct date_rec *date )
{
 int month, day, year, result ;
 /* Expect month-day-year, 3-3-01 */
 result = interpret_date( datestr, &month, &day, &year, emssg );
 if (result != 1)
  exit(1);
 if ((year<1980) || (year>2050))
  printf("Warning:  Unusual year in '%s' .  Use mm-dd-yy date like 5-23-02.   '%s'\n", datestr, emssg );
 /* Put the results in the "date_rec" record via pointer */
 date->month = month;
 date->day=day;
 date->year=year;
}


void read_line( FILE *infile, char *line )
{
 int j=0;
 do  line[j++] = getc(infile);  while ((!feof(infile)) && (line[j-1] != '\n'));
 if ((j > 1) && (line[j-2] == '\r')) j--;
 line[j-1] = '\0';
}


void read_line_safe( FILE *infile, char *line, int maxlen )
{
 char ch;
 int j=0;
 do
  {
   ch = getc(infile);
   if (j < maxlen - 1)
    line[j++] = ch;
  }
 while ((!feof(infile)) && (ch != '\n'));
 if ((j > 1) && (line[j-2] == '\r')) j--;
 line[j-1] = '\0';
}


void read_comment_filtered_line( FILE *infile, char *line, int maxlen )
{ /* Read next line, while filtering-out any comments. */
 int j=0;
 do
  {
   line[j] = getc(infile);
   if (line[j]=='{')
    {
     do line[j] = getc(infile);
     while ((line[j] != '}') && (!feof(infile)));
     line[j] = ' ';
    }
   j++;
  }
 while ((!feof(infile)) && (line[j-1] != '\n') && (j < maxlen-2));
 j--;
 line[j] = '\0';
 consume_leading_trailing_whitespace( line );
}


/* Show a line-number and it's value. */
void showline( int j )
{ fprintf(outfile, "L%d = %6.2f\n", j, L[j]); }

/* Show an integer valued line. */
void shownum( int j )
{ fprintf(outfile, "L%d = %d\n", j, (int)L[j]); }

/* Show line only if non-zero. */	/* Depricated in favor of ShowLineNonZero (clearer name). */
void ShowLine( int j )
{ if (L[j]!=0) showline( j ); }

/* Show line only if non-zero. */
void ShowLineNonZero( int j )
{ if (L[j]!=0) showline( j ); }

/* Show-Line with a message. */
void showline_wmsg( int j, char *msg )
{ fprintf(outfile,"L%d = %6.2f\t\t%s\n", j, L[j], msg); }

/* Show line with a message, only if non-zero. */
void ShowLineNonZero_wMsg( int j, char *msg )
{ if (L[j]!=0) showline_wmsg( j, msg ); }

/* For worksheet calculations, indent and show special line character. */
void showline_wrksht( char wrksht, int j, double *x )
{ fprintf(outfile," %c%d = %6.2f\n", wrksht, j, x[j]); }

/* For worksheet calculations, indent and show special line character, if not zero. */
void showline_wrksht_nz( char *wrksht, int j, double *x )
{ if (x[j] != 0.0) fprintf(outfile," %s%d = %6.2f\n", wrksht, j, x[j] ); }

/* Show-line with specified label and value. */
void showline_wlabel( char *label, double value )
{ fprintf(outfile, "%s = %6.2f\n", label, value ); }

/* Show-line with specified label and value. */
void showline_wlabelnz( char *label, double value )
{
 if (value != 0.0)
  fprintf(outfile, "%s = %6.2f\n", label, value );
}

/* Show-line with specified label, value, and message. */
void showline_wlabelmsg( char *label, double value, char *msg )
{ fprintf(outfile, "%s = %6.2f\t\t%s\n", label, value, msg ); }

/* Show a character-string line with specified label, if string is not empty. */
void Show_String_wLabel( char *label, char *msg )
{
 if ((msg != 0) && (msg[0] != '\0'))
  fprintf(outfile, "%s %s\n", label, msg );
}


/* Get a line value, or sum.  Must be terminated by ";". */
void GetLine( char *linename, double *value )
{
 char word[2048];
 get_parameter( infile, 's', word, linename );
 get_parameters( infile, 'f', value, linename );
}

/* Get a single line value. (No ";") */
void GetLine1( char *linename, double *value )
{
 char word[2048];
 get_parameter( infile, 's', word, linename );
 get_parameter( infile, 'f', value, linename );
}

/* Get a line value, or sum, and print it to file. */
void GetLineF( char *linename, double *value )
{
 GetLine( linename, value );
 fprintf(outfile, "%s = %6.2f\n", linename, *value );
}

/* Get a line value, and print it to file if not zero. */
void GetLineFnz( char *linename, double *value )
{
 GetLine( linename, value );
 if (*value != 0.0) fprintf(outfile, "%s = %6.2f\n", linename, *value );
}

/* Get optional line with label. */
void GetOptionalLine( char *linename, char *label, double *value )
{
 get_parameter( infile, 'l', label, linename );
 get_parameters( infile, 'f', value, linename );
}

void GetYesNo( char *linename, int *answer )	/* Get a boolean Yes/No, or True/False value, followed by ';'. */
{
 char word[1024];
 get_parameter( infile, 's', word, linename);
 get_parameters( infile, 'b', answer, linename );
}

void GetYesNo1( char *linename, int *answer )	/* Get a boolean Yes/No, or True/False value. */
{
 char word[1024];
 get_parameter( infile, 's', word, linename);
 get_parameter( infile, 'b', answer, linename );
}

void GetYesNoSL( char *linename, int *answer )	/* Get a boolean Yes/No, or True/False value. */
{
 char word[1024];
 get_parameter( infile, 's', word, linename);
 get_param_single_line( infile, 'b', answer, linename );
}

void GetInteger( char *linename, int *answer )	/* Get an integer number value. (Must end with ";".) */
{
 char word[1024];
 get_parameter( infile, 's', word, linename);
 get_parameters( infile, 'i', answer, linename );
}

void GetString( char *linename, char *chstr )	/* Get a character-string value. */
{
 char word[1024];
 get_parameter( infile, 's', word, linename);
 get_parameters( infile, 'w', chstr, linename );
}


double smallerof( double a, double b ) { if (a<b) return a; else return b; }
double SmallerOf( double a, double b ) { if (a<b) return a; else return b; }
double largerof( double a, double b )  { if (a>b) return a; else return b; }
double LargerOf( double a, double b )  { if (a>b) return a; else return b; }
double NotLessThanZero( double a )    { if (a<0.0) return 0.0; else return a; }

double absolutev( double val ) { if (val >= 0.0)  return val;  else  return -val; }
		/* Convenience function - avoids needing to link with math-lib merely to get fabs(). */


void Display_File( char *filename )
{
 FILE *infile;
 char line[500];

 infile = fopen(filename,"r");
 if (infile==0) {printf("Could not open %s\n", filename); return;}
 fgets(line, 500, infile);
 while (!feof(infile))
  {
   printf("%s", line);
   fgets(line, 500, infile);
  }
 fclose(infile);
}




/*------------------------------------------------------------------------------*/
/* Get_Comment - Read next Comment, if any, from input file.			*/
/*------------------------------------------------------------------------------*/
void get_comment( FILE *infile, char *word )
{
 int j=0;

 do  /*Absorb any leading white-space.*/
     word[j] = getc(infile);
 while ((!feof(infile)) && ((word[j]==' ') || (word[j]=='\t') || (word[j]=='\n') || (word[j]=='\r')));
 if (word[j] == '{')
  {
   do  /*Get words until end of comment.*/
       word[j++] = getc(infile);
   while ((!feof(infile)) && (word[j-1] != '}'));
   if (word[j-1] == '}')
    word[j-1] = '\0';
   else
    word[j] = '\0';
  }
 else
  {
   ungetc(word[j], infile);
   word[0] = '\0';
  }
 if (verbose) printf("Read Coment: {%s}\n", word);
}


void consume_leading_trailing_whitespace( char *line )
{ int j, k;
  while (isspace( line[0] ))
   {
    j = 0;
    do { line[j] = line[j+1];  j++; }
    while (line[j-1] != '\0');
   }
 k = strlen( line ) - 1;
 while ((k >= 0) && (isspace( line[k] )))
  {
   line[k] = '\0';
   k--;
  }
}


int do_all_caps=0;
int writeout_line=1;

void capitalize( char *word )
{
 int k=0;
 while (word[k] != '\0')
  {
   word[k] = toupper( word[k] );
   k++;
  }
}

/*------------------------------------------------------------------------------*/
/* GetTextLineF - Read line with specified label name, and put the contents	*/
/*  of the remainder of the line to the output file.				*/
/*------------------------------------------------------------------------------*/
char *GetTextLineF( char *linename )
{
 int k=0;
 char line[5000];
 get_parameter( infile, 's', line, linename );
 line[k] = getc(infile);
 while ((!feof(infile)) && (line[k] != '\n'))
  {
   if (line[k] == '{')
    {
     do line[k] = getc(infile); while ((!feof(infile)) && (line[k] != '}'));
     if (line[k] == '}') line[k] = getc(infile);
    }
   else
    {
     k++;
     if (k >= 5000)
      {
        line[k-1] = '\0';
        while ((!feof(infile)) && (getc(infile) != '\n'));
        consume_leading_trailing_whitespace( line );
	fprintf(outfile, "%s %s\n", linename, line );
        return strdup( line );
      }
     line[k] = getc(infile);
    }
  }
 line[k] = '\0';
 consume_leading_trailing_whitespace( line );
 if (do_all_caps)
  capitalize( line );
 if (writeout_line)
  fprintf(outfile, "%s %s\n", linename, line );
 return strdup( line );
}


char *GetTextLine( char *linename )
{
 int priorstate;
 char *chstr;
 priorstate = writeout_line;
 writeout_line = 0;
 chstr = GetTextLineF( linename );
 writeout_line = priorstate;
 return chstr;
}


void format_socsec( char *line, int kind )
{ /* Expect 3+2+4=9 digits.  Kind = 0 places space after 3rd+5th chars. */
  char buf[20]="";	  /* Kind = 1 forces 9-consecutive digits w/no spaces. */
  int j=0, k=0;
  while ((line[j] != '\0') && (k < 11))
   {
    if (isdigit( line[j] ))
     {
      buf[k++] = line[j];
      if ((kind == 0) && ((k == 3) || (k == 6)))
	buf[k++] = ' ';
     }
    j++;
   }
 strcpy( line, buf );
}


void remove_certain_chars( char *line, char *badchars )
{ /* Removes any specified characters from a string, so as to uniformly format dates, ID numbers, etc.. */
  int j=0, k, m=0;
  while (line[j] != '\0')
   {
    k = 0;
    while ((badchars[k] != '\0') && (line[j] != badchars[k]))
     k++;
    if (badchars[k] == '\0')
     line[m++] = line[j];
    j++;
   }
  line[m] = '\0';
}


void substitute_chars( char *line, char *badchars, char replace_char )
{ /* Replaces any specified characters from a string, so as to uniformly format dates, ID numbers, etc.. */
  int j=0, k;
  while (line[j] != '\0')
   {
    k = 0;
    while ((badchars[k] != '\0') && (line[j] != badchars[k]))
     k++;
    if (badchars[k] != '\0')
     line[j] = replace_char;
    j++;
   }
}


int whitespace_invariant_strstr( char *haystack, char *needle )	/* Return 1 if match, otherwise 0 if mismatch. */
{
 int ret;
 char *hay, *ne, *wrd1, *wrd2;
 hay = strdup( haystack );
 wrd1 = (char *)malloc( strlen( haystack ) + 1 );
 ne = strdup( needle );
 wrd2 = (char *)malloc( strlen( needle ) + 1 );
 do
  {
   next_word( hay, wrd1, " \t\n\r" );
   next_word( ne, wrd2, " \t\n\r" );
   // printf("Comparing '%s' to '%s'\n", wrd1, wrd2 );
  }
 while ((wrd2[0] != '\0') && (strcmp(wrd1,wrd2) == 0));
 if (wrd2[0] != '\0')
  ret = 0;	/* Mismatch. */
 else
  ret = 1;	/* Matched. */
 free( hay );
 free( ne );
 free( wrd1 );
 free( wrd2 );
 return ret;
}


int check_form_version( char *title_as_read_in, char *expected_title )
{ /* Check that Form input file matched intended Program.  Return 1 if good.  Or 0 if not-match. */
 if (whitespace_invariant_strstr( title_as_read_in, expected_title ) == 0)
  {
   printf("\nWarning: Looks like wrong Program for this Form-file.\n");
   printf("     Expecting: '%s'\n", expected_title );
   printf("     But found: '%s'.\n\n", title_as_read_in );
   fprintf(outfile,"\nWarning: Looks like wrong Program for this Form-file.\n");
   fprintf(outfile,"    Expecting: '%s'\n", expected_title );
   fprintf(outfile,"    But found: '%s'.\n\n\n", title_as_read_in );
   #ifdef microsoft
    system( "start bin\\notify_popup -delay 3 -expire 20 \"Warning: Warning: Looks like wrong Program for this Form-file.\"" );
   #else
    system( "bin/notify_popup -delay 3 -expire 20 \"Warning: Warning: Looks like wrong Program for this Form-file.\" &" );
   #endif
   return 0;
  }
 else
  return 1;
}

//==================================================
//====== Import Return Data Support Functions ======
//==================================================

void GrabLineValue( char *label, char *fline, double *value )
{
 char twrd[2048];
 next_word(fline, twrd, " \t=;");
 if ((twrd[0] != '\0') && (sscanf(twrd,"%lf", value) != 1))
  {
   printf("Error: Reading Fed %s '%s%s'\n", label, twrd, fline);
   fprintf(outfile,"Error: Reading Fed %s '%s%s'\n", label, twrd, fline);
  }
}


void GrabLineString( char *fline, char *strng )
{ /* Grab a string and copy it into pre-allocated character array. */
 char twrd[2048];
 strng[0] = '\0';
 do
  {
   next_word(fline, twrd, " \t=" );
   if (twrd[0] != ';')
    { strcat( strng, twrd );  strcat( strng, " " ); }
  }
 while ((fline[0] != '\0') && (strstr( twrd, ";" ) == 0));
}


void GrabLineAlloc( char *fline, char **strng )
{ /* Grab a string and allocate space for it. */
 char twrd[4096];
 GrabLineString( fline, twrd );
 if (twrd[0] != '\0')
  *strng = strdup( twrd );
}

void ConvertSlashes( char *fname )
{ /* Convert slashes in file name based on machine type. */
  char *ptr;
 #ifdef __MINGW32__
  char slash_sreach='/', slash_replace='\\';
 #else
  char slash_sreach='\\', slash_replace='/';
 #endif
  ptr = strchr( fname, slash_sreach );
  while (ptr)
   {
    ptr[0] = slash_replace;
    ptr = strchr( fname, slash_sreach );
   }
}

//==================================================
//====== Import Return Data Definition Struct ======
//==================================================

typedef struct FORM_IMPORT_DEF_T {
    char *field_name;
    double *p_field_val;    // Pointer to location for numeric value found in return
    char **p_field_string;  // Pointer to string value pointer
} FORM_IMPORT_DEF, *P_FORM_IMPORT_DEF;


//================================
//====== Import Return Data ======
//================================

// Returns 0 for success, 1 for error
int ImportReturnData(char *return_filename, P_FORM_IMPORT_DEF p_form_imp_def, int num_imp_defs)
{
    char word[6000], fline[2000];
    int d;
    FILE *infile;
    // Zero all values, and set all strings to "". This ensures reasonable
    // values, whether or not the file exists, and whether or not the fields exist.
    for (d = 0; d < num_imp_defs; ++d) {
        if ( (p_form_imp_def + d)->p_field_val != NULL )
            *(p_form_imp_def + d)->p_field_val = 0.0;
        if ( (p_form_imp_def + d)->p_field_string != NULL )
            *(p_form_imp_def + d)->p_field_string = "";
    }

    ConvertSlashes( return_filename );
    infile = fopen(return_filename, "r");
    if (infile==0) {
        printf("Error: Could not open return '%s'\n", return_filename);
        fprintf(outfile,"Error: Could not open return '%s'\n", return_filename);
        return( 1 );
    }
    read_line(infile,fline);
    while (!feof(infile)) {
        next_word(fline, word, " \t=");
        // Search through the table for field name
        for (d = 0; d < num_imp_defs; ++d) {
            if (strcmp(word, (p_form_imp_def + d)->field_name) == 0) {
                // Found a matching field; run the call the correct function
                P_FORM_IMPORT_DEF pd = p_form_imp_def + d;
                if (pd->p_field_val != NULL)
                    GrabLineValue( word, fline, pd->p_field_val);
                if (pd->p_field_string != NULL)
                    GrabLineAlloc( fline, pd->p_field_string) ;
                // Match was found; stop searching and go to next line
                break;
            }
        }
        read_line(infile,fline);
    }

    fclose( infile );
    return( 0 );

}


/* --- PDF Markup Support --- */
/* This class supports the ability to intercept "MarkupPDF" commands in a Tax Input File,
   and to forward them to the Tax Output File, where they can be interpretted by the
   universal_pdf_file_modifer to place the desired markups onto the resulting PDF form pages.
   It gives users the ability to add and maintain their own markups in their tax-input files,
   which then show up on their printed tax-forms.
   For example, you could use this feature to fill-in answers on the forms that are not
   provided by the default OTS programs.   You could also use it to re-position or
   override the values provided by the default programs.
   Example Usage - Syntax of line(s) you would add to your tax input file(s):
    1. To specify a value for an existing PDF-tag-name (whose page and position is already
	defined in the default src/forms metadata.dat file):
		MarkupPDF  tag_name  = value
	Example:
		MarkupPDF  L22 = 567.12
   2. To specify a new mark-up (or to re-position an existing one):
		MarkupPDF( page_number, xpos, ypos ) tag_name  = value
	Example:
		MarkupPDF( 5, 344, 800 ) CountyName = Warthberry
      Note that the page-number refers to the page generated, starting from page 1.
	The xpos and ypos are the positions from the top-left of the page,
	assuming 110 units per inch.
   You can add such markup commands to your saved tax file.
*/
struct pdf_markup_record
 {
  char *tagname, *value;
  int page, fontsz, setcol;
  float xpos, ypos, txtred, txtgrn, txtblu;
  struct pdf_markup_record *next;
 } *pdf_markup_list=0;

void add_pdf_markup( char *tagname, int page, float xpos, float ypos,
			int fontsz, int setcol, float txtred, float txtgrn, float txtblu, char *value )
{
 struct pdf_markup_record *_new;
 _new = (struct pdf_markup_record *)calloc( 1, sizeof( struct pdf_markup_record ) );
 _new->next = pdf_markup_list;
 pdf_markup_list = _new;
 _new->tagname = strdup( tagname );
 _new->value = strdup( value );
 _new->page = page;
 _new->xpos = xpos;
 _new->ypos = ypos;
 _new->fontsz = fontsz;
 _new->setcol = setcol;
 _new->txtred = txtred;
 _new->txtgrn = txtgrn;
 _new->txtblu = txtblu;
}

void process_pdf_markup_command( char *line )
{
 char word[4096], word2[4096], tagname[4096], value[4096];
 int pgnum=-1, fsz=10, setcol=0;
 float xpos=0.0, ypos=0.0, txtred=0.0, txtgrn=0.0, txtblu=0.0;
 if (mystrcasestr( line, "MarkupPDF" ) == 0) return;
 if (mystrcasestr( line, "MarkupPDF(" ) != 0)
  {
   next_word( line, word2, "(" );
   next_word( line, word2, "()" );
   next_word( word2, word, " \t(," );
   if (sscanf( word, "%d", &pgnum ) != 1)
    { printf("Error reading MarkupPDF page-num '%s'\n", word );
      fprintf(outfile,"Error reading MarkupPDF page-num '%s'\n", word );
      return;
    }
   next_word( word2, word, " \t," );
   if (sscanf( word, "%f", &xpos ) != 1)
    { printf("Error reading MarkupPDF Xposition '%s'\n", word );
      fprintf(outfile,"Error reading MarkupPDF Xposition '%s'\n", word );
      return;
    }
   next_word( word2, word, " \t,)" );
   if (sscanf( word, "%f", &ypos ) != 1)
    { printf("Error reading MarkupPDF Yposition '%s'\n", word );
      fprintf(outfile,"Error reading MarkupPDF Yposition '%s'\n", word );
      return;
    }
   next_word( word2, word, " \t,)" );
   if ((word[0] != '\0') && (sscanf( word, "%d", &fsz ) != 1))
    { printf("Error reading MarkupPDF fontsz '%s'\n", word );
      fprintf(outfile,"Error reading MarkupPDF fontsz '%s'\n", word );
    }
   next_word( word2, word, " \t,)" );
   if ((word[0] != '\0') && (sscanf( word, "%g", &txtred ) != 1))
    { printf("Error reading MarkupPDF txtred '%s'\n", word );
      fprintf(outfile,"Error reading MarkupPDF textred '%s'\n", word );
    }
   else
    setcol = 1;
   next_word( word2, word, " \t,)" );
   if ((word[0] != '\0') && (sscanf( word, "%g", &txtgrn ) != 1))
    { printf("Error reading MarkupPDF txtgrn '%s'\n", word );
      fprintf(outfile,"Error reading MarkupPDF txtgrn '%s'\n", word );
    }
   next_word( word2, word, " \t,)" );
   if ((word[0] != '\0') && (sscanf( word, "%g", &txtblu ) != 1))
    { printf("Error reading MarkupPDF txtblue '%s'\n", word );
      fprintf(outfile,"Error reading MarkupPDF txtblue '%s'\n", word );
    }
   next_word( line, word, " \t,)=" );
  }
 else
  {
   next_word( line, word, " \t" );
   next_word( line, word, " \t=" );
  }
 strcpy( tagname, word );		/* Grab tag-name. */
 next_word( line, value, " \t=" );	/* Grab 1st word of value after '=', if any. */
 // strcat( value, " " );			/* Add white-space in case other words on line. */
 strcat( value, line );			/* Add any following words on the remainder of the line. */
 add_pdf_markup( tagname, pgnum, xpos, ypos, fsz, setcol, txtred, txtgrn, txtblu, value );
}

void intercept_any_pdf_markups( FILE *infile )
{
 char line[8192];
 if (!outfile) return;
 read_comment_filtered_line( infile, line, 8192 );
 while (!feof(infile))
  {
   if (strstr( line, "MarkupPDF" ) != 0)
    process_pdf_markup_command( line );
   read_comment_filtered_line( infile, line, 8192 );
  }
}

void exude_pdf_markups( FILE *outfile )
{ /* Add any intercepted PDF-markups to the tax-output file. */
  struct pdf_markup_record *old;
  if (!outfile) return;
  while (pdf_markup_list)
   {
    if (pdf_markup_list->page > 0)
     {
      if ((pdf_markup_list->fontsz == 10) && (pdf_markup_list->setcol == 0))
       fprintf(outfile,"NewPDFMarkup( %d, %g, %g ) %s\n", pdf_markup_list->page,
		pdf_markup_list->xpos, pdf_markup_list->ypos, pdf_markup_list->tagname );
      else
       fprintf(outfile,"NewPDFMarkup( %d, %g, %g, %d, %d, %g, %g, %g ) %s\n",
		pdf_markup_list->page,
		pdf_markup_list->xpos, pdf_markup_list->ypos,
		pdf_markup_list->fontsz, pdf_markup_list->setcol,
		pdf_markup_list->txtred, pdf_markup_list->txtgrn, pdf_markup_list->txtblu,
		pdf_markup_list->tagname );
     }
    if (strstr( pdf_markup_list->tagname, ":" ) == 0)
     fprintf(outfile,"%s = %s\n", pdf_markup_list->tagname, pdf_markup_list->value );
    else
     fprintf(outfile,"%s \t%s\n", pdf_markup_list->tagname, pdf_markup_list->value );
    old = pdf_markup_list;
    pdf_markup_list = pdf_markup_list->next;
    free( old->tagname );
    free( old->value );
    free( old );
   }
}

void grab_any_pdf_markups( char *infname, FILE *outfile )
{
 FILE *infile;
 infile = fopen( infname, "rb" );
 if (infile == 0) { printf("GAPM: Cannot open '%s' for reading.\n", infname );  return; }
 intercept_any_pdf_markups( infile );
 fclose( infile );
 exude_pdf_markups( outfile );
}

/* --- End PDF Markup Support --- */


#define SINGLE 		        1
#define MARRIED_FILING_JOINTLY 2
#define MARRIED_FILING_SEPARAT 3
#define HEAD_OF_HOUSEHOLD       4
#define WIDOW		        5
#define INDIVIDUAL	1
#define ESTATE	2
#define Yes 1
#define No  0
#define NotApplicable 3
#define Short 1
#define Annualized 2
/************************************************************************/
/* TaxSolve_CA_5805-2022.c						*/
/*  User contributed.							*/
/*                                                                      */
/* GNU Public License - GPL:                                            */
/* This program is free software; you can redistribute it and/or        */
/* modify it under the terms of the GNU General Public License as       */
/* published by the Free Software Foundation; either version 2 of the   */
/* License, or (at your option) any later version.                      */
/*                                                                      */
/* This program is distributed in the hope that it will be useful,      */
/* but WITHOUT ANY WARRANTY; without even the implied warranty of       */
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU     */
/* General Public License for more details.                             */
/*                                                                      */
/* You should have received a copy of the GNU General Public License    */
/* along with this program; if not, write to the Free Software          */
/* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA             */
/* 02111-1307 USA.                                                      */
/*                                                                      */
/************************************************************************/

float thisversion=4.01;




/* The following two tax functions copied from taxsolve_CA_540_2022.c. */
/* 2022 tax rates are used in California for calculating estimated taxes */

double TaxRateFormula( double income, int status )
{											/* Updated for 2022. */
 double tax;
 if ((status==SINGLE) || (status==MARRIED_FILING_SEPARAT))
  {
   if (income <  10099.00)  tax =             0.01 * income;                else
   if (income <  23942.00)  tax =   100.99 +  0.02 * (income -  10099.00);  else
   if (income <  37788.00)  tax =   377.85 +  0.04 * (income -  23942.00);  else
   if (income <  52455.00)  tax =   931.69 +  0.06 * (income -  37788.00);  else
   if (income <  66295.00)  tax =  1811.71 +  0.08 * (income -  52455.00);  else
   if (income < 338639.00)  tax =  2918.91 + 0.093 * (income -  66295.00);  else
   if (income < 406364.00)  tax = 28246.90 + 0.103 * (income - 338639.00);  else
   if (income < 677275.00)  tax = 32222.58 + 0.113 * (income - 406364.00);
   else                     tax = 65835.52 + 0.123 * (income - 677275.00);
  }
 else
 if ((status==MARRIED_FILING_JOINTLY) || (status==WIDOW))
  {
   if (income <   20198.00)  tax =              0.01 * income;                 else
   if (income <   47884.00)  tax =    201.98 +  0.02 * (income -   20198.00);  else
   if (income <   75576.00)  tax =    755.70 +  0.04 * (income -   47884.00);  else
   if (income <  104910.00)  tax =   1863.38 +  0.06 * (income -   75576.00);  else
   if (income <  132590.00)  tax =   3623.42 +  0.08 * (income -  104910.00);  else
   if (income <  677278.00)  tax =   5837.82 + 0.093 * (income -  132590.00);  else
   if (income <  812728.00)  tax =  56493.80 + 0.103 * (income -  677278.00);  else
   if (income < 1354550.00)  tax =  70445.15 + 0.113 * (income -  812728.00);
   else                      tax = 131671.04 + 0.123 * (income - 1354550.00);
  }
 else
  { /* Head of Household. */
   if (income <  20212.00)  tax =             0.01 * income;                else
   if (income <  47887.00)  tax =   202.12 +  0.02 * (income -  20212.00);  else
   if (income <  61730.00)  tax =   755.62 +  0.04 * (income -  47887.00);  else
   if (income <  76397.00)  tax =  1309.34 +  0.06 * (income -  61730.00);  else
   if (income <  90240.00)  tax =  2189.36 +  0.08 * (income -  76397.00);  else
   if (income < 460547.00)  tax =  3296.80 + 0.093 * (income -  90240.00);  else
   if (income < 552658.00)  tax = 37735.35 + 0.103 * (income - 460547.00);  else
   if (income < 921095.00)  tax = 47222.78 + 0.113 * (income - 552658.00);
   else                     tax = 88856.16 + 0.123 * (income - 921095.00);
  }
 return (int)(tax+0.5);
}

double TaxRateFunction( double income, int status )     /* Emulates table lookup or function appropriately. */
{
 double x, tx;
 int k;

 if (income < 100000.0)   /* Quantize to match tax-table exactly. */
  {
   if (income < 99951.0)
    {
     k = (income + 49) / 100;
     x = 100 * (double)k;
    }
   else x = 99975.0;
   tx = (int)(TaxRateFormula( x, status ));
  }
 else
  tx = TaxRateFormula( income, status );
 return tx;
}


double		/* Reoccuring threshold values. */	/* Updated for 2022. */
	thresh_sep_single =	229908.0,
	thresh_mfj =		459821.0,
	thresh_HoH =		344867.0;

double		/* Other recurring values */		/* Updated for 2022 */
	line_11_multiplier = 0.02672055,
	line_12_multiplier = 0.00014;

double L6WS(int column, double IIIL4, double ScdA, double IIIL5, double FAIWSL3, int status){

	double L[14];
	int i;

	L[1] = IIIL4;
	L[2] = ScdA;
	L[3] = L[1] - L[2];
	L[4] = IIIL5;
	L[5] = L[1] * L[4];
	if(L[3] == 0){
		fprintf(outfile, " Line 6 Worksheet - Column (%c),\n", column);
		for(i = 1; i <= 5; i++)
			fprintf(outfile, "L6WS_%d%c %0.2lf\n", i, column, L[i]);
		return(L[5]);
	}
	L[6] = L[3] * L[4];
	L[7] = Round(L[6] * 0.80);
	L[8] = FAIWSL3;
	if((status == MARRIED_FILING_JOINTLY) || (status == WIDOW))
		L[9] = thresh_mfj;
	else if((status == SINGLE) || (status == MARRIED_FILING_SEPARAT))
		L[9] = thresh_sep_single;
	else if(status == HEAD_OF_HOUSEHOLD)
		L[9] = thresh_HoH;
	L[10] = L[8] - L[9];
	L[11] = Round(L[10] * 0.06);
	L[12] = SmallerOf(L[7], L[11]);
	L[13] = Round(L[5] - L[12]);

	fprintf(outfile, " Line 6 Worksheet - Column (%c),\n", column);
	for(i = 1; i <= 13; i++)
		fprintf(outfile, "L6WS_%d%c %0.2lf\n", i, column, L[i]);
	return(L[13]);
}

/*----------------------------------------------------------------------------*/

int main( int argc, char *argv[] )
{
  int i, j, k, status, entity = INDIVIDUAL, Quest4 = Yes;
 char word[4000], outfname[4000], *infname=0;
 time_t now;

 int Quest2, Quest3 = 0, Num_Days = -1;		/* negative Num_Days used as a flag below */
 double Wthd_Per_1, Wthd_Per_2, Wthd_Per_3, Wthd_Per_4, CA_AGI;
 double a10add, b10add, c10add, d10add;


 /* line entry variables L[n] are declared in taxsolve_routines.c */

 double a[24], b[24], c[24], d[24];	/* cells in grid of Part III Annualizd Income Installment Method Schedule */
					/* comprised of lines 1-13 and 15-23; e.g., cell 1(a) will be in variable a[1] */
					/*  lines 14a-14e declared individually */

 double L14aa = 0, L14ab = 0, L14ac = 0, L14ad = 0;
 double L14ba = 0, L14bb = 0, L14bc = 0, L14bd = 0;
 double L14ca = 0, L14cb = 0, L14cc = 0, L14cd = 0;
 double L14da = 0, L14db = 0, L14dc = 0, L14dd = 0;
 double L14ea = 0, L14eb = 0, L14ec = 0, L14ed = 0;

 double FAIWS_a[4], FAIWS_b[4], FAIWS_c[4], FAIWS_d[4];		/* cells in grid for Federal Annualized Income Worksheet */

 double L6WS_a[14], L6WS_b[14], L6WS_c[14], L6WS_d[14]; 	/* cells in grid for Line 6 Worksheet */

 double A[15], B[15], C[15], D[15];	/* cells in grid of Worksheet II */
					/* e.g., cell 1(a) will be in variable A[1] */

  printf("Form 5805, 2022 - v%3.2f\n", thisversion);

 /* Decode any command-line arguments. */
 i = 1;  k=1;
 while (i < argc)
 {
  if (strcmp(argv[i],"-verbose")==0)  { verbose = 1; }
  else
  if (k==1)
   {
    infname = strdup(argv[i]);
    infile = fopen(infname,"r");
    if (infile==0) {printf("ERROR: Parameter file '%s' could not be opened.\n", infname ); exit(1);}
    k = 2;
    /* Base name of output file on input file. */
    strcpy(outfname,infname);
    j = strlen(outfname)-1;
    while ((j>=0) && (outfname[j]!='.')) j--;
    if (j<0) strcat(outfname,"_out.txt"); else strcpy(&(outfname[j]),"_out.txt");
    outfile = fopen(outfname,"w");
    if (outfile==0) {printf("ERROR: Output file '%s' could not be opened.\n", outfname); exit(1);}
    printf("Writing results to file:  %s\n", outfname);
   }
  else
   {printf("Unknown command-line parameter '%s'\n", argv[i]); exit(1);}
  i = i + 1;
 }
 if (infile==0) {printf("Error: No input file on command line.\n"); exit(1);}

 /* Pre-initialize all lines to zeros. */

 for (i=0; i<MAX_LINES; i++) { L[i] = 0.0; }

 for(i = 0; i <= 14; i++){
	A[i] = 0.0;
	B[i] = 0.0;
	C[i] = 0.0;
	D[i] = 0.0;
 }

 for(i = 0; i <= 23; i++){
	a[i] = 0.0;
	b[i] = 0.0;
	c[i] = 0.0;
	d[i] = 0.0;
 }

 for(i = 0; i <= 3; i++){
	FAIWS_a[i] = 0.0;
	FAIWS_b[i] = 0.0;
	FAIWS_c[i] = 0.0;
	FAIWS_d[i] = 0.0;
 }

for(i = 0; i <= 13; i++){
	L6WS_a[i] = 0.0;
	L6WS_b[i] = 0.0;
	L6WS_c[i] = 0.0;
	L6WS_d[i] = 0.0;
 }

 Wthd_Per_1 = 0.0;
 Wthd_Per_2 = 0.0;
 Wthd_Per_3 = 0.0;
 Wthd_Per_4 = 0.0;

 /* Accept parameters from input file. */
 /* Expect lines, something like:
        Title:  Form XXXX Return
        L2              {Returns and Allowances}
        . . .
 */

 /* Accept Form's "Title" line, and put out with date-stamp for your records. */
 read_line( infile, word );
 now = time(0);
 fprintf(outfile,"\n%s,  v%2.2f, %s\n", word, thisversion, ctime( &now ));
 check_form_version( word, "Title:  Form 5805 for Tax Year 2022" );

// add_pdf_markup( "NotReady", 1, 240, 40, 17, 1, 1.0, 0, 0, "\"This program is NOT ready for 2022.\"" );
// fprintf(outfile, "This program is NOT ready for 2022.\n\n");

 fprintf(outfile, "%s\n", "==================================================");
 fprintf(outfile, "%s\n", "                                                            CAUTION\nThis program fills out Form 5805 to determine WHETHER OR NOT you owe a penalty\nfor underpayment of estimated tax.  It calculates the AMOUNT of any penalty you\nmay owe for the MOST LIKELY CASE in which up to four estimated tax payments have\n been made.  You should carefully review the instructions for Form 5805 to see if the\ncalculations are correct for your particular tax situation.  DO NOT INTERPRET a\ndefault zero value for the penalty on the filled PDF to indicate that you do not owe\na penalty, especially if you have not input all required information, including the\nactual dates on which you made your payments. Scroll down to the end of this\nresults file to see if you had an underpayment for any period.  If so, you may owe\na penalty.\n\nItemized deductions are limited for high-income taxpayers.  When you are\nchecking calculations, values on line 6 of Part III may appear to be in error due to\nthese limitations.  See this results file and the last page of the output PDF for the\nlimitation calculations.  Also note that the annualization factor values on lines 4\nof the last page of the output PDF round by default to the nearest integer.  Use\nthe values in this results file for lines L6WS_4a, L6WS_4b, L6WS_4c, and L6WS_4d,\n which for most individual taxpayers will be 4.0, 2.4, 1.5, and 1.0, respectively.\n\nThis program does not calculate the phase-out of exemption credits for high-income\ntaxpayers as collection of the necessary information for each period would\nunnecessarily complicate this program for most users and the impact of exemption\nlimitations on the tax estimates would be negligible.  See instructions for line 11.");
 fprintf(outfile, "%s\n\n", "==================================================");

 /* ----- Place all your form-specific code below here .... ------ */

 // Exam
 //  GetLineF( "L2", &L[2] );
 //  GetLineF( "L3", &L[3] );
 //  L[4] = L[2] - L[3];
 //  showline_wlabel( "L4", L[4] );

 GetTextLineF( "YourName:" );
 GetTextLineF( "YourSocSec#:" );

 get_parameter( infile, 's', word, "Entity" );
 get_parameter( infile, 'w', word, "Entity?");
 if (strncasecmp(word,"Individual",3)==0){
 	entity = INDIVIDUAL;
 }
 else if(strncasecmp(word,"Estate/Trust",3)==0){
	entity = ESTATE;
 }
 fprintf(outfile,"Entity = %s (%d)\n", word, entity);


 get_parameter( infile, 's', word, "Status" );	/* Single, Married/joint, Married/sep, Head house, Widow(er) */
 get_parameter( infile, 'l', word, "Status?");
 if (strncasecmp(word,"Single",4)==0) status = SINGLE; else
 if (strncasecmp(word,"Married/Joint",13)==0) status = MARRIED_FILING_JOINTLY; else
 if (strncasecmp(word,"Married/Sep",11)==0) status = MARRIED_FILING_SEPARAT; else
 if (strncasecmp(word,"Head_of_House",4)==0) status = HEAD_OF_HOUSEHOLD; else
 if (strncasecmp(word,"Widow",4)==0) status = WIDOW;
 else
  {
   printf("Error: unrecognized status '%s'. Exiting.\n", word);
   fprintf(outfile,"Error: unrecognized status '%s'. Exiting.\n", word);
   exit(1);
  }

 if((entity == ESTATE) && (status != SINGLE)){

	#ifdef microsoft
	 system( "start bin\\notify_popup -delay 3 -expire 60 \"Setting Status to Single as required for estates/trusts.\"" );
	#else
	 system( "bin/notify_popup -delay 3 -expire 60 \"Setting Status to Single as required for estates/trusts.\" &" );
	#endif

	strcpy(word, "Single");
	status = SINGLE;
	fprintf(outfile, "Setting Status to Single as required for estates/trusts\n");
 }
 fprintf(outfile,"Status = %s (%d)\n", word, status);

 get_parameter( infile, 's', word, "Quest1" );
 get_parameter( infile, 'w', word, "Quest1?");
 if (strncasecmp(word,"Yes",1)==0){
	// Quest1 = Yes;
	fprintf(outfile,"CkQuest1Yes X\n");
}
else {
	fprintf(outfile,"CkQuest1No X\n");
}

 get_parameter( infile, 's', word, "Quest2" );
 get_parameter( infile, 'w', word, "Quest2?");
 if (strncasecmp(word,"Yes",1)==0){
	 Quest2 = Yes;
	fprintf(outfile,"CkQuest2Yes X\n");
}
else {
	Quest2 = No;
	fprintf(outfile,"CkQuest2No X\n");
}

 get_parameter( infile, 's', word, "Quest3" );
 get_parameter( infile, 'w', word, "Quest3?");
 if (strncasecmp(word,"Yes",1)==0){
	Quest3 = Yes;
	fprintf(outfile,"CkQuest3Yes X\n");
}
else if (strncasecmp(word,"No",2)==0){
	Quest3 = No;
	fprintf(outfile,"CkQuest3No X\n");
}
else {
	// Quest3 = NotApplicable;
	fprintf(outfile,"CkQuest3NA X\n");
}

 GetLineF( "Wthd_Per_1", &Wthd_Per_1 );
 GetLineF( "Wthd_Per_2", &Wthd_Per_2 );
 GetLineF( "Wthd_Per_3", &Wthd_Per_3 );
 GetLineF( "Wthd_Per_4", &Wthd_Per_4 );

 get_parameter( infile, 's', word, "Quest4" );
 get_parameter( infile, 'w', word, "Quest4?");
 if (strncasecmp(word,"Yes",1)==0){
	 Quest4 = Yes;
	fprintf(outfile,"CkQuest4Yes X\n");
}
else if (strncasecmp(word,"No",2)==0){
	Quest4 = No;
	fprintf(outfile,"CkQuest4No X\n");
}

 if((entity == ESTATE) && (Quest4 == Yes)){

	fprintf(outfile, "Estates and grantor trusts, which receive the residue of the decedent's estate,\nare required to make estimated income tax payments for any year ending two or\nmore years after the date of the decedent's death. If you answer \"Yes\" to\nPart I, Question 4, complete Part I only and attach form FTB 5805 to the\nback of your tax return.\n");

  exit(1);

 }


/* Part II - Required Annual Payment */

 GetLineF( "L1", &L[1] );
  L[2] = L[1] * 0.90;
  showline( 2 );
 GetLine( "L3", &L[3] );
	if(Quest3 == Yes){
		L[3] = Wthd_Per_1 + Wthd_Per_2 + Wthd_Per_3 + Wthd_Per_4;
	}
 showline( 3 );
 L[4] = L[1]  -  L[3];
 showline( 4 );
 if((L[4] < 250.00) && (status == MARRIED_FILING_SEPARAT)){
		fprintf(outfile, "Status is \"Married Filing Separately\" and line 4 is less than $250.  Stop here.\nYou do not owe the penalty. Do not file form FTB 5805.\n");
		exit(0);
 }
 else if((L[4] < 500.00) && (status != MARRIED_FILING_SEPARAT)){
		fprintf(outfile, "Status is not \"Married Filing Separately\" and line 4 is less than $500.  Stop here.\nYou do not owe the penalty. Do not file form FTB 5805.\n");
		exit(0);
 }
  GetLineF( "L5", &L[5] );

 GetLineF( "CA_AGI", &CA_AGI );

 if((CA_AGI >= 1000000.00) || (status == MARRIED_FILING_SEPARAT && CA_AGI >= 500000.00))
	L[6] = L[2];
else
	L[6] = SmallerOf(L[2], L[5]);
showline( 6 );

/* Short Method */

/* Need to read these Short Method inputs even if Short Method is not used; */
/* otherwise, error message re: unexpected input item is thrown */

  GetLineF( "L8", &L[8] );

  GetInteger( "Num_Days", &Num_Days);

	if(Quest2 == No){			/* Not using Annualized Income Installment Method */

		L[7] = L[3];
		L[9] = L[7 ]+ L[8];
		L[10] = L[6] - L[9];

		for(i = 7; i <= 10; i++)
			showline( i );

		if(L[10] <= 0){
			fprintf(outfile, "Line 10 is zero or less.  Stop here.  You do not owe the penalty.\nDo not file form FTB 5805.\n");
			exit(0);
		}

		L[11] = L[10] * line_11_multiplier;

		if(Num_Days > -1){

			L[12] = L[10] * Num_Days * line_12_multiplier;
			L[13] = L[11] - L[12];

			showline( 11 );
			showline( 12 );

			showline_wmsg(13, "PENALTY.  Enter this amount on Form 540, line 113; Form 540NR, line 123; or Form 541, line 44.\n Also, check the box for FTB 5805.\n");
		}
	}
	else{					/* Using the Annualized Income Installment Method */

	/* Annualized Income Installment Method */

	/* Inputs must be read even if Annualized Income Installment Method is not used; */
	/* otherwise, error message re: unexpected input is thrown */

	   GetLine( "SchdAI_1a", &a[1] );
	   GetLine( "SchdAI_1b", &b[1] );
	   GetLine( "SchdAI_1c", &c[1] );
	   GetLine( "SchdAI_1d", &d[1] );

	   if(entity == ESTATE){

		a[2] = 6.0;
		b[2] = 3.0;
		c[2] = 1.71429;
		d[2] = 1.09091;

	   }
	   else{
		a[2] = 4.0;
		b[2] = 2.4;
		c[2] = 1.5;
		d[2] = 1.0;
	   }

//	   GetLine1( "SchdAI_2a", &a[2] );
//	   GetLine1( "SchdAI_2b", &b[2] );
//	   GetLine1( "SchdAI_2c", &c[2] );
//	   GetLine1( "SchdAI_2d", &d[2] );

	   GetLine( "SchdAI_4a", &a[4] );
	   GetLine( "SchdAI_4b", &b[4] );
	   GetLine( "SchdAI_4c", &c[4] );
	   GetLine( "SchdAI_4d", &d[4] );

//	   GetLine( "SchdAI_6a", &a[6] );
//	   GetLine( "SchdAI_6b", &b[6] );
//	   GetLine( "SchdAI_6c", &c[6] );
//	   GetLine( "SchdAI_6d", &d[6] );

	   GetLine( "SchdAI_7a", &a[7] );
	   b[7] = a[7];
	   c[7] = a[7];
	   d[7] = a[7];

	   GetLine( "SchdAI_10a_add", &a10add );
	   GetLine( "SchdAI_10b_add", &b10add );
	   GetLine( "SchdAI_10c_add", &c10add );
	   GetLine( "SchdAI_10d_add", &d10add );

	   GetLine( "SchdAI_11a", &a[11] );
	   GetLine( "SchdAI_11b", &b[11] );
	   GetLine( "SchdAI_11c", &c[11] );
	   GetLine( "SchdAI_11d", &d[11] );

	   GetLine( "SchdAI_13a", &a[13] );
	   GetLine( "SchdAI_13b", &b[13] );
	   GetLine( "SchdAI_13c", &c[13] );
	   GetLine( "SchdAI_13d", &d[13] );

	   GetLine( "SchdAI_14ba", &L14ba );
	   GetLine( "SchdAI_14bb", &L14bb );
	   GetLine( "SchdAI_14bc", &L14bc );
	   GetLine( "SchdAI_14bd", &L14bd );

	   GetLine( "SchdAI_14da", &L14da );
	   GetLine( "SchdAI_14db", &L14db );
	   GetLine( "SchdAI_14dc", &L14dc );
	   GetLine( "SchdAI_14dd", &L14dd );

	   GetLine( "FAIWS_1a", &FAIWS_a[1]);
	   GetLine( "FAIWS_1b", &FAIWS_b[1]);
	   GetLine( "FAIWS_1c", &FAIWS_c[1]);
	   GetLine( "FAIWS_1d", &FAIWS_d[1]);

	   GetLine( "L6WS_2a", &L6WS_a[2]);
	   GetLine( "L6WS_2b", &L6WS_b[2]);
	   GetLine( "L6WS_2c", &L6WS_c[2]);
	   GetLine( "L6WS_2d", &L6WS_d[2]);

        GetLine( "WSII_2a", &A[2]);
	GetLine( "WSII_2b", &B[2]);
	GetLine( "WSII_2c", &C[2]);
	GetLine( "WSII_2d", &D[2]);

	GetLine( "WSII_10a", &A[10]);
	GetLine( "WSII_10b", &B[10]);
	GetLine( "WSII_10c", &C[10]);

	GetLine( "WSII_12a", &A[12]);
	GetLine( "WSII_12b", &B[12]);
	GetLine( "WSII_12c", &C[12]);
	GetLine( "WSII_12d", &D[12]);

	a[3] = a[1] * a[2];
	b[3] = b[1] * b[2];
	c[3] = c[1] * c[2];
	d[3] = d[1] * d[2];

	a[5] = a[2];
	b[5] = b[2];
	c[5] = c[2];
	d[5] = d[2];

	a[6] = a[4] * a[5];		/* calculation for lower income filers */
	b[6] = b[4] * b[5];		/* these values will be overwritten if higher income thresholds are exceeded */
	c[6] = c[4] * c[5];
	d[6] = d[4] * d[5];

	FAIWS_a[3] = FAIWS_a[1] * a[5];
	fprintf(outfile, "FAIWS_1a\t%0.2lf\n", FAIWS_a[1]);
	fprintf(outfile, "FAIWS_2a\t%0.2lf\n", a[5]);
	fprintf(outfile, "FAIWS_3a\t%0.2lf\n", FAIWS_a[3]);

	if(a[4] > 0){
		if((((status == MARRIED_FILING_JOINTLY) || (status == WIDOW)) && (FAIWS_a[3] > thresh_mfj)) || \
	(((status == SINGLE) || (status == MARRIED_FILING_SEPARAT)) && (FAIWS_a[3] > thresh_sep_single)) || \
	((status == HEAD_OF_HOUSEHOLD) && (FAIWS_a[3] > thresh_HoH))){
			a[6] = L6WS('a', a[4], L6WS_a[2], a[5], FAIWS_a[3], status);
		}
	}

	FAIWS_b[3] = FAIWS_b[1] * b[5];
	fprintf(outfile, "FAIWS_1b\t%0.2lf\n", FAIWS_b[1]);
	fprintf(outfile, "FAIWS_2b\t%0.2lf\n", b[5]);
	fprintf(outfile, "FAIWS_3b\t%0.2lf\n", FAIWS_b[3]);

	if(b[4] > 0){
		if((((status == MARRIED_FILING_JOINTLY) || (status == WIDOW)) && (FAIWS_b[3] > thresh_mfj)) || \
	(((status == SINGLE) || (status == MARRIED_FILING_SEPARAT)) && (FAIWS_b[3] > thresh_sep_single)) || \
	((status == HEAD_OF_HOUSEHOLD) && (FAIWS_b[3] > thresh_HoH))){
			b[6] = L6WS('b', b[4], L6WS_b[2], b[5], FAIWS_b[3], status);
		}
	}

	FAIWS_c[3] = FAIWS_c[1] * c[5];
	fprintf(outfile, "FAIWS_1c\t%0.2lf\n", FAIWS_c[1]);
	fprintf(outfile, "FAIWS_2c\t%0.2lf\n", c[5]);
	fprintf(outfile, "FAIWS_3c\t%0.2lf\n", FAIWS_c[3]);

	if(c[4] > 0){
		if((((status == MARRIED_FILING_JOINTLY) || (status == WIDOW)) && (FAIWS_c[3] > thresh_mfj)) || \
	(((status == SINGLE) || (status == MARRIED_FILING_SEPARAT)) && (FAIWS_c[3] > thresh_sep_single)) || \
	((status == HEAD_OF_HOUSEHOLD) && (FAIWS_c[3] > thresh_HoH))){
			c[6] = L6WS('c', c[4], L6WS_c[2], c[5], FAIWS_c[3], status);
		}
	}

	FAIWS_d[3] = FAIWS_d[1] * d[5];
 	fprintf(outfile, "FAIWS_1d\t%0.2lf\n", FAIWS_d[1]);
	fprintf(outfile, "FAIWS_2d\t%0.2lf\n", d[5]);
	fprintf(outfile, "FAIWS_3d\t%0.2lf\n", FAIWS_d[3]);

	if(d[4] > 0){
		if((((status == MARRIED_FILING_JOINTLY) || (status == WIDOW)) && (FAIWS_d[3] > thresh_mfj)) || \
	(((status == SINGLE) || (status == MARRIED_FILING_SEPARAT)) && (FAIWS_d[3] > thresh_sep_single)) || \
	((status == HEAD_OF_HOUSEHOLD) && (FAIWS_d[3] > thresh_HoH))){
			d[6] = L6WS('d', d[4], L6WS_d[2], d[5], FAIWS_d[3], status);
		}
	}

	a[8] = LargerOf(a[6], a[7]);
	b[8] = LargerOf(b[6], b[7]);
	c[8] = LargerOf(c[6], c[7]);
	d[8] = LargerOf(d[6], d[7]);

	a[9] = a[3] - a[8];
	b[9] = b[3] - b[8];
	c[9] = c[3] - c[8];
	d[9] = d[3] - d[8];

	a[10] = TaxRateFunction( a[9], status);
	b[10] = TaxRateFunction( b[9], status);
	c[10] = TaxRateFunction( c[9], status);
	d[10] = TaxRateFunction( d[9], status);

	a[10] += a10add;
	b[10] += b10add;
	c[10] += c10add;
	d[10] += d10add;

	a[12] = a[10] - a[11];
	b[12] = b[10] - b[11];
	c[12] = c[10] - c[11];
	d[12] = d[10] - d[11];

	L14aa = NotLessThanZero(a[12] - a[13]);
	L14ab = NotLessThanZero(b[12] - b[13]);
	L14ac = NotLessThanZero(c[12] - c[13]);
	L14ad = NotLessThanZero(d[12] - d[13]);

	L14ca = L14aa + L14ba;		/* first lower case letter is row; second lower case letter is column */
	L14cb = L14ab + L14bb;
	L14cc = L14ac + L14bc;
	L14cd = L14ad + L14bd;

	L14ea = L14ca - L14da;
	L14eb = L14cb - L14db;
	L14ec = L14cc - L14dc;
	L14ed = L14cd - L14dd;

	a[16] = Round(L14ea * 0.27);
	b[16] = Round(L14eb * 0.63);
	c[16] = Round(L14ec * 0.63);
	d[16] = Round(L14ed * 0.90);

	a[18] = NotLessThanZero(a[16] - a[17]);
	a[19] = Round(L[6] * 0.30);
	a[21] = a[19];
	a[22] = NotLessThanZero(a[21] - a[18]);
	a[23] = SmallerOf(a[18], a[21]);
	A[1] = a[23];

	b[17] = a[23];
	b[18] = NotLessThanZero(b[16] - b[17]);
	b[19] = Round(L[6] * 0.40);
	b[20] = a[22];
	b[21] = b[19] + b[20];
	b[22] = NotLessThanZero(b[21] - b[18]);
	b[23] = SmallerOf(b[18], b[21]);
	B[1] = b[23];

	c[17] = a[23] + b[23];
	c[18] = NotLessThanZero(c[16] - c[17]);
	c[19] = 0;
	c[20] = b[22];
	c[21] = c[19] + c[20];
	c[22] = NotLessThanZero(c[21] - c[18]);
	c[23] = SmallerOf(c[18], c[21]);
	C[1] = c[23];

	d[17] = a[23] + b[23] + c[23];
	d[18] = NotLessThanZero(d[16] - d[17]);
	d[19] = Round(L[6] * 0.30);
	d[20] = c[22];
	d[21] = d[19] + d[20];
	d[22] = NotLessThanZero(d[21] - d[18]);
	d[23] = SmallerOf(d[18], d[21]);
	D[1] = d[23];

	}
		/* WORKSHEET II */

	/* A[2], B[2], C[2], D[2] contain estimated taxes paid each period.  Add uneven withholding in period */
	/* or total of evenly withheld amounts allocated to each period based on number of days in period */
	/* Withholding is either uneven or even (in which case Wthd_Per_1, Wthd_Per_2, Wthd_Per_3, and */
	/* Wthd_Per_4 should all be blank or zero. */

	if(Quest3 == Yes){
		A[2] = A[2] + Wthd_Per_1;
		B[2] = B[2] + Wthd_Per_2;
		C[2] = C[2] + Wthd_Per_3;
		D[2] = D[2] + Wthd_Per_4;
	}
	else{
		A[2] = A[2] + L[3] * (90.0 / 365.0);
		B[2] = B[2] + L[3] * (61.0 / 365.0);
		C[2] = C[2] + L[3] * (92.0 / 365.0);
		D[2] = D[2] + L[3] * (122.0 / 365.0);
	}

	A[6] = A[2];

	if(A[1] >= A[6])
		A[8] = A[1] - A[6];
	else
		A[9] = A[6] - A[1];

	A[11] = A[8] * A[10]/365 * 0.03;
	A[13] = A[8] * A[12]/365 * 0.05;

	B[3] = A[9];
	B[4] = B[2] + B[3];
	B[5] = A[7] + A[8];
	B[6] = NotLessThanZero(B[4] - B[5]);
	if(B[6] == 0)
		B[7] = B[5] - B[4];
	else
		B[7] = 0;
	if(B[1] >= B[6])
		B[8] = B[1] - B[6];
	else
		B[9] = B[6] - B[1];

	B[11] = B[8] * B[10]/365 * 0.03;
	B[13] = B[8] * B[12]/365 * 0.05;

	C[3] = B[9];
	C[4] = C[2] + C[3];
	C[5] = B[7] + B[8];
	C[6] = NotLessThanZero(C[4] - C[5]);
	if(C[6] == 0)
		C[7] = C[5] - C[4];
	else
		C[7] = 0;
	if(C[1] >= C[6])
		C[8] = C[1] - C[6];
	else
		C[9] = C[6] - C[1];

	C[11] = C[8] * C[10]/365 * 0.03;
	C[13] = C[8] * C[12]/365 * 0.05;

	D[3] = C[9];
	D[4] = D[2] + D[3];
	D[5] = C[7] + C[8];
	D[6] = NotLessThanZero(D[4] - D[5]);

	if(D[1] >= D[6])
		D[8] = D[1] - D[6];
	else
		D[9] = D[6] - D[1];

	D[13] = D[8] * D[12]/365 * 0.05;

	A[0] = A[11] + B[11] + C[11] + A[13] + B[13] + C[13] + D[13];	/* line 14 of WSII */

	if(Quest2 == Yes){

		for(i = 18; i <= 14; i++){
			fprintf(outfile, "SecA_%d%s %0.2lf\n", i, "a", A[i]);
			fprintf(outfile, "SecA_%d%s %0.2lf\n", i, "b", B[i]);
			fprintf(outfile, "SecA_%d%s %0.2lf\n", i, "c", C[i]);
			fprintf(outfile, "SecA_%d%s %0.2lf\n", i, "d", D[i]);
		}

		i = 1;
			fprintf(outfile, "SchdAI_%d%s %0.2lf\n", i, "a", a[i]);
			fprintf(outfile, "SchdAI_%d%s %0.2lf\n", i, "b", b[i]);
			fprintf(outfile, "SchdAI_%d%s %0.2lf\n", i, "c", c[i]);
			fprintf(outfile, "SchdAI_%d%s %0.2lf\n", i, "d", d[i]);

		i = 2;
			fprintf(outfile, "SchdAI_%d%s %0.1lf\n", i, "a", a[i]);
			fprintf(outfile, "SchdAI_%d%s %0.1lf\n", i, "b", b[i]);
			fprintf(outfile, "SchdAI_%d%s %0.5lf\n", i, "c", c[i]);
			fprintf(outfile, "SchdAI_%d%s %0.5lf\n", i, "d", d[i]);

		for(i = 3; i <= 9; i++){
			fprintf(outfile, "SchdAI_%d%s %0.2lf\n", i, "a", a[i]);
			fprintf(outfile, "SchdAI_%d%s %0.2lf\n", i, "b", b[i]);
			fprintf(outfile, "SchdAI_%d%s %0.2lf\n", i, "c", c[i]);
			fprintf(outfile, "SchdAI_%d%s %0.2lf\n", i, "d", d[i]);
		}

		i = 10;
			fprintf(outfile, "SchdAI_%d%s %0.2lf     = calculated: %0.2lf + additions: %0.2lf\n", i, "a", a[i], a[10] - a10add, a10add);
			fprintf(outfile, "SchdAI_%d%s %0.2lf     = calculated: %0.2lf + additions: %0.2lf\n", i, "b", b[i], b[10] - b10add, b10add);
			fprintf(outfile, "SchdAI_%d%s %0.2lf     = calculated: %0.2lf + additions: %0.2lf\n", i, "c", c[i], c[10] - c10add, c10add);
			fprintf(outfile, "SchdAI_%d%s %0.2lf     = calculated: %0.2lf + additions: %0.2lf\n", i, "d", d[i], d[10] - d10add, d10add);

		for(i = 11; i <= 13; i++){
			fprintf(outfile, "SchdAI_%d%s %0.2lf\n", i, "a", a[i]);
			fprintf(outfile, "SchdAI_%d%s %0.2lf\n", i, "b", b[i]);
			fprintf(outfile, "SchdAI_%d%s %0.2lf\n", i, "c", c[i]);
			fprintf(outfile, "SchdAI_%d%s %0.2lf\n", i, "d", d[i]);
		}

		fprintf(outfile, "%s %0.2lf\n", "L14aa", L14aa);
		fprintf(outfile, "%s %0.2lf\n", "L14ab", L14ab);
		fprintf(outfile, "%s %0.2lf\n", "L14ac", L14ac);
		fprintf(outfile, "%s %0.2lf\n", "L14ad", L14ad);
		fprintf(outfile, "%s %0.2lf\n", "L14ba", L14ba);
		fprintf(outfile, "%s %0.2lf\n", "L14bb", L14bb);
		fprintf(outfile, "%s %0.2lf\n", "L14bc", L14bc);
		fprintf(outfile, "%s %0.2lf\n", "L14bd", L14bd);
		fprintf(outfile, "%s %0.2lf\n", "L14ca", L14ca);
		fprintf(outfile, "%s %0.2lf\n", "L14cb", L14cb);
		fprintf(outfile, "%s %0.2lf\n", "L14cc", L14cc);
		fprintf(outfile, "%s %0.2lf\n", "L14cd", L14cd);
		fprintf(outfile, "%s %0.2lf\n", "L14da", L14da);
		fprintf(outfile, "%s %0.2lf\n", "L14db", L14db);
		fprintf(outfile, "%s %0.2lf\n", "L14dc", L14dc);
		fprintf(outfile, "%s %0.2lf\n", "L14dd", L14dd);
		fprintf(outfile, "%s %0.2lf\n", "L14ea", L14ea);
		fprintf(outfile, "%s %0.2lf\n", "L14eb", L14eb);
		fprintf(outfile, "%s %0.2lf\n", "L14ec", L14ec);
		fprintf(outfile, "%s %0.2lf\n", "L14ed", L14ed);

		for(i = 15; i <= 23; i++){
			fprintf(outfile, "SchdAI_%d%s %0.2lf\n", i, "a", a[i]);
			fprintf(outfile, "SchdAI_%d%s %0.2lf\n", i, "b", b[i]);
			fprintf(outfile, "SchdAI_%d%s %0.2lf\n", i, "c", c[i]);
			fprintf(outfile, "SchdAI_%d%s %0.2lf\n", i, "d", d[i]);
		}

		for(i = 1; i <= 13; i++){
			if((i != 3) && (i != 4) && (i != 5) && (i != 7))
				fprintf(outfile, "WSII_%d%s %0.2lf\n", i, "a", A[i]);
			fprintf(outfile, "WSII_%d%s %0.2lf\n", i, "b", B[i]);
			fprintf(outfile, "WSII_%d%s %0.2lf\n", i, "c", C[i]);
			if((i != 7) && (i != 9))
				fprintf(outfile, "WSII_%d%s %0.2lf\n", i, "d", D[i]);
		}
		fprintf(outfile, "WSII_%d %0.2lf     %s\n", 14, A[0], "PENALTY");

		if((A[8] == 0) && (B[8] == 0) && (C[8] == 0) && (D[8] == 0))
			fprintf(outfile, "As line 8 on WSII is zero for all payment periods, you don't owe a penalty.\n");
		else
			fprintf(outfile, "%s\n%s\n%s\n", "There is an underpayment for one or more periods.  See the  instructions and if", "you have not already done so, enter the number of days any payment was late", "into the GUI so this program can calculate the penalty.");
	}

  /***
    Summary of useful functions:
	GetLine( "label", &variable )	- Looks for "label" in input file, and places the corresponding sum of
					  values following that label (until ";") into variable.
	GetLineF( "label", &variable )	- Like GetLine() above, but also writes the result to the output file.
	GetLineFnz(( "label", &variable ) - Like GetLine(), but only writes non-zero values to the output file.
	GetLine1( "label", &variable )  - Like GetLine() above, but expects single value (no sum, no ";" in input file).

	c = SmallerOf( a, b );		- Selects smaller of two values.
	c = LargerOf( a, b );		- Selects larger of two values.
	c = NotLessThanZero( a );	- Selects positive value or zero. Prevents negative values.

	showline( j )			- Writes currency value of L[j] to output file with label in nice format.
	shownum( j )			- Writes integer value of L[j] to output file with label in nice format.
	showline_wmsg( j, "msg" )	- Like showline, but adds the provided message to the output line.
	ShowLineNonZero( j )		- Like showline, but only writes non-zero values.
	ShowLineNonZero_wMsg( j, "msg" ) - Like showline_wmsg, but only writes non-zero values.
	showline_wlabel( "label", value ) - For custom line names and variables not in the default L[] array.
	showline_wlabelnz( "label", value ) - Like showline_wlabel, but only writes non-zero values.
	showline_wlabelmsg( "label", value, "msg" ) - Like showline_wlabel,but adds the provided message to the output line.

  ***/

 fclose(infile);
 grab_any_pdf_markups( infname, outfile );
 fclose(outfile);

 printf("\nListing results from file: %s\n\n", outfname);
 Display_File( outfname );

 return 0;
}

#undef SINGLE
#undef MARRIED_FILING_JOINTLY
#undef MARRIED_FILING_SEPARAT
#undef HEAD_OF_HOUSEHOLD
#undef WIDOW
#undef INDIVIDUAL
#undef ESTATE
#undef Yes
#undef No
#undef NotApplicable
#undef Short
#undef Annualized

#undef MAX_LINES

} // namespace taxsolve_CA_5805_2022
} // namespace OpenTaxSolver2022

#undef printf
#undef system
#ifdef _MSC_VER
#undef strcasecmp
#undef strncasecmp
#endif
