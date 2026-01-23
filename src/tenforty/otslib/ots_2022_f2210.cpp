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
namespace taxsolve_f2210_2022 {

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
#define MARRIED_FILING_JOINTLY  2
#define MARRIED_FILING_SEPARAT  3
#define HEAD_OF_HOUSEHOLD       4
#define WIDOW		        5
#define Yes 1
#define No  0
#define Neither    0
#define Short      1
#define Regular    2
#define INDIVIDUAL 1
#define ESTATE	   2
/************************************************************************/
/* TaxSolve_Form_2210.c - 2022                                          */
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




int BoxA = 0, BoxB = 0, BoxC = 0, BoxD = 0, BoxE = 0, Num_Days = 0;

/*-----------Tax Routines Copied From taxsolve_US_1040_2022.c----------------*/

			/* Following values taken from 1040-Instructions pg 109. */	/* Updated for 2022. */
  double brkpt[4][9]={
		{ 0.0,  10275.0,  41775.0,  89075.0, 170050.0, 215950.0, 539900.0, 9e19 },  /* Single */
		{ 0.0,  20550.0,  83550.0, 178150.0, 340100.0, 431900.0, 647850.0, 9e19 },  /* Married, filing jointly. */
		{ 0.0,  10275.0,  41775.0,  89075.0, 170050.0, 215950.0, 323925.0, 9e19 },  /* Married, filing separate. */
		{ 0.0,  14650.0,  55900.0,  89050.0, 170050.0, 215950.0, 539900.0, 9e19 },  /* Head of Household. */
		     };
  double txrt[4][9] ={
		{ 0.1, 0.12, 0.22, 0.24, 0.32, 0.35, 0.37 },	/* Single */
		{ 0.1, 0.12, 0.22, 0.24, 0.32, 0.35, 0.37 },	/* Married, filing jointly. */
		{ 0.1, 0.12, 0.22, 0.24, 0.32, 0.35, 0.37 },	/* Married, filing separate. */
		{ 0.1, 0.12, 0.22, 0.24, 0.32, 0.35, 0.37 },	/* Head of Household. */
		     };


double TaxRateFormula( double x, int status )  /* Returns tax due. */
{		
  double sum=0.0;
  int   bracket=0;
  if (status == WIDOW) status = MARRIED_FILING_JOINTLY;  /* Handle case of widow(er). */
  status = status - 1;  /* Arrays start at zero; not one. */
  while (brkpt[status][bracket+1] < x)
   {
    sum = sum + (brkpt[status][bracket+1] - brkpt[status][bracket]) * txrt[status][bracket];
    bracket = bracket + 1;
   }
  return (x - brkpt[status][bracket]) * txrt[status][bracket] + sum;
}

double TaxRateFunction( double income, int status )     /* Emulates table lookup or function appropriately. */
{
 double x, dx, tx;
 int k;
 if (income < 100000.0)	/* Quantize to match tax-table exactly. */
  {
   if (income < 25.0) x = 5.0; else
   if (income < 3000.0) x = 25.0; else x = 50.0;
   dx = 0.5 * x;
   k = income / x;
   x = x * (double)k + dx;
   tx = (int)(TaxRateFormula( x, status ) + 0.5);
  }
 else
  tx = TaxRateFormula( income, status );
 return tx;
}

/* 2022 Tax Rate Schedule for Estates/Trusts - From 2022 Form 1041 Instructions, page 30 */

double Estate_Trust_TaxRateFunction( double income )
{
 if (income < 2650.0) return income * 0.10; else
 if (income < 9550.0) return  265.0 + (income - 2650.0) * 0.24; else
 if (income < 13050.0) return 1921.0 + (income - 9550.0) * 0.35; else
 return 3146.0 + (income - 13050.0) * 0.37; 
}

/*----------------------------------------------------------------------------*/

int main( int argc, char *argv[] )
{
  int i, j, k, status, entity=INDIVIDUAL;
 char word[4000], outfname[4000], *infname=0;
 time_t now;

 /* line entry variables L[n] are declared in taxsolve_routines.c */

 double A[19], B[19], C[19], D[19];	/* cells in grid of Part III, Section A, comprised of lines 10 through 18 */
					/* e.g., cell 18(a) will be be variable A[18] */

 double a[37], b[37], c[37], d[37];	/* cells in grid of Schedule AI comprised of lines 1 through 36 */
					/* e.g., cell 1(a) will be in variable a[1] */

 printf("Form 2210, 2022 - v%3.2f\n", thisversion);

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

 for(i = 0; i <= 18; i++){
	A[i] = 0.0;
	B[i] = 0.0;
	C[i] = 0.0;
	D[i] = 0.0;
 }

 for(i = 0; i <= 36; i++){
	a[i] = 0.0;
	b[i] = 0.0;
	c[i] = 0.0;
	d[i] = 0.0;
 }

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
 check_form_version( word, "Title:  Form 2210 for Tax Year 2022" );

 // fprintf(outfile,"\n--- THIS IS PRELIMINARY USER-CONTRIBUTED FORM ---\n");
 // MarkupPDF( 1, 240, 40, 17, 1.0, 0, 0 ) NotReady "This program is NOT updated for 2022."
 // add_pdf_markup( "NotReady", 1, 240, 40, 17, 1, 1.0, 0, 0, "\"This program is NOT ready for 2022.\"" );


 fprintf(outfile, "%s\n", "==================================================");
 fprintf(outfile, "%s\n", "    CAUTION\nThis program fills out Form 2210 to determine WHETHER OR NOT you owe a penalty\nfor underpayment of estimated tax.  It does NOT calculate the AMOUNT of any\npenalty you may owe.  If you owe a penalty, you may need to fill out the Penalty\nWorksheet in the Form 2210 instructions to calculate the amount of your penalty.\nDO NOT INTERPRET the default zero value shown on page 2, line 19, of the filled\nPDF to indicate that you do not owe a penalty.  Scroll down about 1/3 of the way in\nthis results file (to BELOW the SecA results lines) to see if you might owe a penalty.");
 fprintf(outfile, "%s\n\n", "==================================================");

 /* ----- Accept form data and process the numbers.         ------ */
 /* ----- Place all your form-specific code below here .... ------ */

 // Example:
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
 else
  fprintf(outfile,"Error: Unexpected Entity '%s', assuming 'Individual'.\n", word );
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
 fprintf(outfile,"Status = %s (%d)\n", word, status);

 GetLineF( "L1", &L[1] );
 GetLineF( "L2", &L[2] );
 GetLineF( "L3", &L[3] );
 L[4] = L[1] + L[2] - L[3];
 showline( 4 );
 L[5] = L[4] * 0.90;
 showline( 5 );
 GetLineF( "L6", &L[6] );
 L[7] = L[4] - L[6];
 showline( 7 );
 GetLineF( "L8", &L[8] );
 if(L[8] == 0){
	#ifdef microsoft
	 system( "start bin\\notify_popup -delay 3 -expire 60 \"Line 8 is zero or blank, indicating you had no tax liability last year.  Is this correct?\"" );
	#else
	 system( "bin/notify_popup -delay 3 -expire 60 \"Line 8 is zero or blank, indicating you had no tax liability last year.  Is this correct?\" &" );
	#endif
 }
 L[9] = SmallerOf(L[5], L[8]);
 showline( 9 );
 if(L[9] > L[6])
	fprintf(outfile,  "CkL9Yes X\n");	/* Yes, may owe penalty */
 else
 	fprintf(outfile,  "CkL9No X\n");	/* No, don't owe penalty */

 get_parameter( infile, 's', word, "BoxA" );
 get_parameter( infile, 'w', word, "BoxA?");
 if (strncasecmp(word,"Yes",1)==0){
	BoxA = Yes;
	fprintf(outfile,"CkBoxA X\n");
}

get_parameter( infile, 's', word, "BoxB" );
get_parameter( infile, 'w', word, "BoxB?");
 if (strncasecmp(word,"Yes",1)==0){
	BoxB = Yes;
	fprintf(outfile,"CkBoxB X\n");
}

 get_parameter( infile, 's', word, "BoxC" );
 get_parameter( infile, 'w', word, "BoxC?");
 if (strncasecmp(word,"Yes",1)==0){
	BoxC = Yes;
	fprintf(outfile,"CkBoxC X\n");
}

get_parameter( infile, 's', word, "BoxD" );
 get_parameter( infile, 'w', word, "BoxD?");
 if (strncasecmp(word,"Yes",1)==0){
	BoxD = Yes;
	fprintf(outfile,"CkBoxD X\n");
}

get_parameter( infile, 's', word, "BoxE" );
 get_parameter( infile, 'w', word, "BoxE?");
 if (strncasecmp(word,"Yes",1)==0){
	BoxE = Yes;
	fprintf(outfile,"CkBoxE X\n");
}

if((L[4] < 1000) || (L[7] < 1000))
	fprintf(outfile, "Line 4 or Line 7 less than $1,000.  Don't file Form 2210. You don't owe a penalty.\n");

else if(L[6] >= L[9]){
	fprintf(outfile, "Line 6 is equal to or more than Line 9.\n");
	if(BoxE == Yes){

		fprintf(outfile, "You don't owe a penalty.  Because Box E in Part II applies, file page 1 of Form 2210.\n");
	}
	else{
		fprintf(outfile, "You don't owe a penalty.  Don't file Form 2210.\n");
	}
}
else if((BoxA == Yes) || (BoxB == Yes) || (BoxC == Yes) || (BoxD == Yes) || (BoxE == Yes)){
	fprintf(outfile, "You MUST file Form 2210.\n");
	if((BoxB == Yes) || (BoxC == Yes) || (BoxD == Yes))
		fprintf(outfile, "You must figure your penalty.\n");
	else
		fprintf(outfile, "%s", "You aren't required to figure your penalty because the IRS\nwill figure it and send you a bill for any unpaid amount.\nIf you want to figure it, you may use Part III as a\nworksheet and enter your penalty amount on your tax return,\nbut FILE ONLY PAGE 1 OF FORM 2210.\n");
}
else
	fprintf(outfile, "%s", "Don't file Form 2210. You aren't required to figure\nyour penalty because the IRS will figure it and send\nyou a bill for any unpaid amount. If you want to figure\nit, you may use Part III as a worksheet and\nenter your penalty amount on your tax return, but\ndon't file Form 2210.\n");


	/* Inputs must be read; */
	/* otherwise, error message re: unexpected input is thrown */
	
	   GetLine( "SecA_11a", &A[11] );
	   GetLine( "SecA_11b", &B[11] );
	   GetLine( "SecA_11c", &C[11] );
	   GetLine( "SecA_11d", &D[11] );
	
	   GetLine( "SchdAI_1a", &a[1] );
	   GetLine( "SchdAI_1b", &b[1] );
	   GetLine( "SchdAI_1c", &c[1] );
	   GetLine( "SchdAI_1d", &d[1] );  
	
	   GetLine( "SchdAI_4a", &a[4] );
	   GetLine( "SchdAI_4b", &b[4] );
	   GetLine( "SchdAI_4c", &c[4] );
	   GetLine( "SchdAI_4d", &d[4] ); 
	
	    GetLine( "SchdAI_7a", &a[7] );
	    b[7] = a[7];
	    c[7] = a[7];
	    d[7] = a[7];
	
	    GetLine( "SchdAI_9a", &a[9] );
	   GetLine( "SchdAI_9b", &b[9] );
	   GetLine( "SchdAI_9c", &c[9] );
	   GetLine( "SchdAI_9d", &d[9] ); 
	
	    GetLine( "SchdAI_12a", &a[12] );
	    b[12] = a[12];
	    c[12] = a[12];
	    d[12] = a[12];
	
	    GetLine( "SchdAI_14a", &a[14] );
	   GetLine( "SchdAI_14b", &b[14] );
	   GetLine( "SchdAI_14c", &c[14] );
	   GetLine( "SchdAI_14d", &d[14] );
	
	    GetLine( "SchdAI_16a", &a[16] );
	   GetLine( "SchdAI_16b", &b[16] );
	   GetLine( "SchdAI_16c", &c[16] );
	   GetLine( "SchdAI_16d", &d[16] );
	
	    GetLine( "SchdAI_18a", &a[18] );
	   GetLine( "SchdAI_18b", &b[18] );
	   GetLine( "SchdAI_18c", &c[18] );
	   GetLine( "SchdAI_18d", &d[18] );
	
	    GetLine( "SchdAI_28a", &a[28] );
	   GetLine( "SchdAI_28b", &b[28] );
	   GetLine( "SchdAI_28c", &c[28] );
	   GetLine( "SchdAI_28d", &d[28] );
	
	    GetLine( "SchdAI_30a", &a[30] );
	   GetLine( "SchdAI_30b", &b[30] );
	   GetLine( "SchdAI_30c", &c[30] );
	   GetLine( "SchdAI_30d", &d[30] );
	
	    GetLine( "SchdAI_32a", &a[32] );
	   GetLine( "SchdAI_32b", &b[32] );
	   GetLine( "SchdAI_32c", &c[32] );
	   GetLine( "SchdAI_32d", &d[32] );
	
	 GetLine( "L19", &L[19] );
	
	  /* Schedule AI - PART 1 */

  if(BoxC == Yes){

	if(entity == ESTATE){

		a[2] = 6.0;
		b[2] = 3.0;
		c[2] = 1.71429;
		d[2] = 1.09091;

		a[3] = a[1] * a[2];
		b[3] = b[1] * b[2];
		c[3] = c[1] * c[2];
		d[3] = d[1] * d[2];

		for(i = 4; i <= 8; i++){
		
			a[i] = 0;
			b[i] = 0;
			c[i] = 0;
			d[i] = 0;
		}

		a[10] = 0;
		b[10] = 0;
		c[10] = 0;
		d[10] = 0;
	
	       	a[11] = a[3] - a[9];	
		b[11] = b[3] - b[9];
		c[11] = c[3] - c[9];
		d[11] = d[3] - d[9];

		a[13] = NotLessThanZero(a[11] - a[12]);
		b[13] = NotLessThanZero(b[11] - b[12]);
		c[13] = NotLessThanZero(c[11] - c[12]);
		d[13] = NotLessThanZero(d[11] - d[12]);

		if(a[14] < 0)
			a[14] = Estate_Trust_TaxRateFunction(a[13]);	/* else defaults to the entered value */
		if(b[14] < 0)		
			b[14] = Estate_Trust_TaxRateFunction(b[13]);
		if(c[14] < 0)
			c[14] = Estate_Trust_TaxRateFunction(c[13]);
		if(d[14] < 0)
			d[14] = Estate_Trust_TaxRateFunction(d[13]);
	}
	else{
		a[2] = 4.0;
		b[2] = 2.4;
		c[2] = 1.5;
		d[2] = 1.0;

		a[3] = a[1] * a[2];
		b[3] = b[1] * b[2];
		c[3] = c[1] * c[2];
		d[3] = d[1] * d[2];
	
		a[5] = a[2];
		b[5] = b[2];
		c[5] = c[2];
		d[5] = d[2];
	
		a[6] = a[4] * a[5];
		b[6] = b[4] * b[5];
		c[6] = c[4] * c[5];
		d[6] = d[4] * d[5];

		a[8] = LargerOf(a[6], a[7]);
		b[8] = LargerOf(b[6], b[7]);
		c[8] = LargerOf(c[6], c[7]);
		d[8] = LargerOf(d[6], d[7]);
	
		a[10] = a[8] + a[9];		
		b[10] = b[8] + b[9];
		c[10] = c[8] + c[9];
		d[10] = d[8] + d[9];
	
       	        a[11] = a[3] - a[10];	
		b[11] = b[3] - b[10];
		c[11] = c[3] - c[10];
		d[11] = d[3] - d[10];

		a[13] = NotLessThanZero(a[11] - a[12]);
		b[13] = NotLessThanZero(b[11] - b[12]);
		c[13] = NotLessThanZero(c[11] - c[12]);
		d[13] = NotLessThanZero(d[11] - d[12]);

		if(a[14] < 0)
			a[14] = TaxRateFunction(a[13], status);
		if(b[14] < 0)
			b[14] = TaxRateFunction(b[13], status);
		if(c[14] < 0)
			c[14] = TaxRateFunction(c[13], status);
		if(d[14] < 0)
			d[14] = TaxRateFunction(d[13], status);
	}
	
		/* Interrupt Part I to Calculate Line 15 */
	
		/* Schedule AI - Part II - Annualized Self-Employment Tax */
	
		a[29] = 36750;			/* Updated for 2022 */
		b[29] = 61250;
		c[29] = 98000;
		d[29] = 147000;
	
		a[31] = NotLessThanZero(a[29] - a[30]);
		b[31] = NotLessThanZero(b[29] - b[30]);	
		c[31] = NotLessThanZero(c[29] - c[30]);
		d[31] = NotLessThanZero(d[29] - d[30]);

		a[32] = 0.496;
		b[32] = 0.2976;
		c[32] = 0.186;
		d[32] = 0.124;
	
		a[33] = a[32] * SmallerOf(a[28], a[31]);
		b[33] = b[32] * SmallerOf(b[28], b[31]);
		c[33] = c[32] * SmallerOf(c[28], c[31]);
		d[33] = d[32] * SmallerOf(d[28], d[31]);
	
		a[34] = 0.116;
		b[34] = 0.0696;
		c[34] = 0.0435;
		d[34] = 0.029;
	
		a[35] = Round(a[28] * a[34]);
		b[35] = Round(b[28] * b[34]);	
		c[35] = Round(c[28] * c[34]);
		d[35] = Round(d[28] * d[34]);
	
		a[36] = a[33] + a[35];
		b[36] = b[33] + b[35];	
		c[36] = c[33] + c[35];
		d[36] = d[33] + d[35];
	
		/* End Part II Annualized Self-Employment Tax */
		/* Continue Part I */
	
		a[15] = a[36];
		b[15] = b[36];
		c[15] = c[36];
		d[15] = d[36];	
		
		a[17] = a[14] + a[15] + a[16];
		b[17] = b[14] + b[15] + b[16];
		c[17] = c[14] + c[15] + c[16];
		d[17] = d[14] + d[15] + d[16];
	
		a[19] = NotLessThanZero(a[17] - a[18]);
		b[19] = NotLessThanZero(b[17] - b[18]);
		c[19] = NotLessThanZero(c[17] - c[18]);
		d[19] = NotLessThanZero(d[17] - d[18]);
	
		a[20] = 0.225;
		b[20] = 0.45;
		c[20] = 0.675;
		d[20] = 0.90;
	
		a[21] = Round(a[19] * a[20]);
		b[21] = Round(b[19] * b[20]);
		c[21] = Round(c[19] * c[20]);
		d[21] = Round(d[19] * d[20]);
	
		a[23] = NotLessThanZero(a[21]);
		a[24] = Round(L[9] * 0.25);
		a[26] = a[24];
		a[27] = SmallerOf(a[23], a[26]);
		A[10] = a[27];
	
		b[22] = a[27];
		b[23] = NotLessThanZero(b[21] - b[22]);
		b[24] = Round(L[9] * 0.25);
		b[25] = a[26] - a[27];
		b[26] = b[24] + b[25];
		b[27] = SmallerOf(b[23], b[26]);
		B[10] = b[27];
	
		c[22] = a[27] + b[27];
		c[23] = NotLessThanZero(c[21] - c[22]);
		c[24] = Round(L[9] * 0.25);
		c[25] = b[26] - b[27];
		c[26] = c[24] + c[25];
		c[27] = SmallerOf(c[23], c[26]);
		C[10] = c[27];
	
		d[22] = a[27] + b[27] + c[27];
		d[23] = NotLessThanZero(d[21] - d[22]);
		d[24] = Round(L[9] * 0.25);
		d[25] = c[26] - c[27];
		d[26] = d[24] + d[25];
		d[27] = SmallerOf(d[23], d[26]);
		D[10] = d[27];
  }
		/* Penalty Computation - PART III */

		else if(BoxC == No){

			A[10] = Round(L[9] * 0.25);
			B[10] = Round(L[9] * 0.25);
			C[10] = Round(L[9] * 0.25);
			D[10] = Round(L[9] * 0.25);
		}
	
		A[15] = A[11];
		if(A[10] >= A[15])
			A[17] = A[10] - A[15];
		else
			A[18] = A[15] - A[10];
	
		B[12] = A[18];
		B[13] = B[11] + B[12];
		B[14] = A[16] + A[17];
		B[15] = NotLessThanZero(B[13] - B[14]);
		if(B[15] == 0)
			B[16] = B[14] - B[13];
		else
			B[16] = 0;
		if(B[10] >= B[15])
			B[17] = B[10] - B[15];
		else
			B[18] = B[15] - B[10];
	
		C[12] = B[18];
		C[13] = C[11] + C[12];
		C[14] = B[16] + B[17];
		C[15] = NotLessThanZero(C[13] - C[14]);
		if(C[15] == 0)
			C[16] = C[14] - C[13];
		else
			C[16] = 0;
		if(C[10] >= C[15])
			C[17] = C[10] - C[15];
		else
			C[18] = C[15] - C[10];
	
		D[12] = C[18];
		D[13] = D[11] + D[12];
		D[14] = C[16] + C[17];
		D[15] = NotLessThanZero(D[13] - D[14]);
		if(D[10] >= D[15])
			D[17] = D[10] - D[15];
		else
			D[18] = D[15] - D[10];

		for(i = 10; i <= 18; i++){
			fprintf(outfile, "SecA_%d%s %0.2lf\n", i, "a", A[i]);
			fprintf(outfile, "SecA_%d%s %0.2lf\n", i, "b", B[i]);
			fprintf(outfile, "SecA_%d%s %0.2lf\n", i, "c", C[i]);
			fprintf(outfile, "SecA_%d%s %0.2lf\n", i, "d", D[i]);
		}

		if((A[17] == 0) && (B[17] == 0) && (C[17] == 0) && (D[17] == 0)){
			fprintf(outfile, "\n%s\n", "If line 17 on page 2 is zero for all payment periods, you don't owe a penalty.\nBut if you checked box C or D in Part II, you must file Form 2210 with your return.\nIf you checked box E, you must file page 1 of Form 2210 with your return.\nIn certain circumstances, the IRS will waive all or part of the underpayment\npenalty.  See Waiver of Penalty in the instructions.\n");

		fprintf(outfile, "L19 %0.2lf\n", 0.0);
		}
		else{
			fprintf(outfile, "\n%s\n", "There is an underpayment for one or more periods.\nIf you are required to calculate the penalty, or choose to do so, use the\nWorksheet for Form 2210, Part III, Section B-Figure the Penalty\n(Penalty Worksheet), in the instructions to figure your penalty.\nEnter the penalty amount in the OTS GUI for Form 2210 (last line).\n");

		fprintf(outfile, "L19 %0.2lf     %s\n", L[19], "This is the penalty amount YOU have entered from the Penalty Worksheet.\nIt defaults to zero until YOU enter your penalty.\nDO NOT INTERPRET THE DEFAULT ZERO VALUE TO INDICATE YOU DO NOT OWE\nA PENALTY.\nSee above instructions and the form 2210 instructions to determine if you need\nto calculate a penalty.");

		fprintf(outfile, "%s\n", "In certain circumstances, the IRS will waive all or part of the underpayment\npenalty.  See Waiver of Penalty in the form 2210 instructions.\n");
		}

  if(BoxC == Yes){
		for(i = 1; i <= 27; i++){
			fprintf(outfile, "SchdAI_%d%s %0.2lf\n", i, "a", a[i]);
			fprintf(outfile, "SchdAI_%d%s %0.2lf\n", i, "b", b[i]);
			fprintf(outfile, "SchdAI_%d%s %0.2lf\n", i, "c", c[i]);
			fprintf(outfile, "SchdAI_%d%s %0.2lf\n", i, "d", d[i]);
		}

		if((a[28] > 0.0) || (b[28] > 0.0) || (c[28] > 0.0) || (d[28] > 0.0)){
			for(i = 18; i <= 31; i++){
				fprintf(outfile, "SchdAI_%d%s %0.2lf\n", i, "a", a[i]);
				fprintf(outfile, "SchdAI_%d%s %0.2lf\n", i, "b", b[i]);
				fprintf(outfile, "SchdAI_%d%s %0.2lf\n", i, "c", c[i]);
				fprintf(outfile, "SchdAI_%d%s %0.2lf\n", i, "d", d[i]);
			}

			for(i = 33; i <= 36; i++){
				fprintf(outfile, "SchdAI_%d%s %0.2lf\n", i, "a", a[i]);
				fprintf(outfile, "SchdAI_%d%s %0.2lf\n", i, "b", b[i]);
				fprintf(outfile, "SchdAI_%d%s %0.2lf\n", i, "c", c[i]);
				fprintf(outfile, "SchdAI_%d%s %0.2lf\n", i, "d", d[i]);
			}
		}
  }

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
#undef Yes
#undef No
#undef Neither
#undef Short
#undef Regular
#undef INDIVIDUAL
#undef ESTATE

#undef MAX_LINES

} // namespace taxsolve_f2210_2022
} // namespace OpenTaxSolver2022

#undef printf
#undef system
#ifdef _MSC_VER
#undef strcasecmp
#undef strncasecmp
#endif


