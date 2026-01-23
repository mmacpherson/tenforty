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
namespace taxsolve_CA_540_2022 {

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
/************************************************************************/
/* TaxSolve_CA_540_2022.c - California state 540 tax form.		*/
/* Copyright (C) 2022 - Aston Roberts					*/
/* 									*/
/* Compile:   gcc taxsolve_CA_540_2022.c -o taxsolve_CA_540_2022	*/
/* Run:	      ./taxsolve_CA_540_2022  CA_540_2022.txt 			*/
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
/* Aston Roberts 1-2-2023	aston_roberts@yahoo.com			*/
/************************************************************************/

float thisversion=20.01;




int status=0;	/* Value for filing status. */
double 	sched540part2[MAX_LINES], sched540part2_sub[MAX_LINES], sched540part2_add[MAX_LINES],
	sched540part2_5a=0.0, sched540part2_5b=0.0, sched540part2_5c=0.0, sched540part2_5d=0.0,
	sched540part2_8a=0.0, sched540part2_8b=0.0, sched540part2_8c=0.0, sched540part2_8d=0.0,
	sched540part2_add8a=0.0, sched540part2_add8b=0.0, sched540part2_add8c=0.0, sched540part2_sub8d=0.0;
 char 	*Your1stName="", *YourLastName="", *your_socsec="", 
	*Spouse1stName="", *SpouseLastName="", *spouse_socsec="",
	*street_address="", *apartment="", *town="", *zipcode="";


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
   if (income < 677275.00)  tax = 35222.58 + 0.113 * (income - 406364.00);
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


void Report_bracket_info( double income, int status )
{
 double tx, rate;
 tx = TaxRateFormula( income, status );
 if ((status==SINGLE) || (status==MARRIED_FILING_SEPARAT))
  {
   if (income <   10099.00)  rate = 0.01;  else
   if (income <   23942.00)  rate = 0.02;  else
   if (income <   37788.00)  rate = 0.04;  else
   if (income <   52455.00)  rate = 0.06;  else
   if (income <   66295.00)  rate = 0.08;  else
   if (income <  338639.00)  rate = 0.093;  else
   if (income <  406364.00)  rate = 0.103;  else
   if (income <  677275.00)  rate = 0.113;  else  rate = 0.123;
  }
 else
 if ((status==MARRIED_FILING_JOINTLY) || (status==WIDOW))
  {
   if (income <   20198.00)  rate = 0.01;  else
   if (income <   47884.00)  rate = 0.02;  else
   if (income <   75576.00)  rate = 0.04;  else
   if (income <  104910.00)  rate = 0.06;  else
   if (income <  132590.00)  rate = 0.08;  else
   if (income <  677278.00)  rate = 0.093;  else
   if (income <  812728.00)  rate = 0.103;  else
   if (income < 1354550.00)  rate = 0.113;  else  rate = 0.123;
  }
 else
  {
   if (income <  20212.00)  rate = 0.01;  else
   if (income <  47887.00)  rate = 0.02;  else
   if (income <  61730.00)  rate = 0.04;  else
   if (income <  76397.00)  rate = 0.06;  else
   if (income <  90240.00)  rate = 0.08;  else
   if (income < 460547.00)  rate = 0.093;  else
   if (income < 552658.00)  rate = 0.103;  else
   if (income < 921095.00)  rate = 0.113;  else  rate = 0.123;
  }
 printf(" You are in the %2.1f%% marginal tax bracket,\n and you are paying an effective %2.1f%% tax on your total income.\n",
	  100.0 * rate, 100.0 * tx / income );
 fprintf(outfile," You are in the %2.1f%% marginal tax bracket,\n and you are paying an effective %2.1f%% tax on your total income.\n",
	  100.0 * rate, 100.0 * tx / income );
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


void test_tax_function()
{
 double income;
 for (income=50.0; income < 100000.0; income = income + 100.0)
  printf("%g: %8g %8g %8g\n", income,
		TaxRateFunction( income, SINGLE ),
		TaxRateFunction( income, MARRIED_FILING_JOINTLY ), 
		TaxRateFunction( income, HEAD_OF_HOUSEHOLD ) );
 exit(0);
}


/*----------------------------------------------------------------------------*/


struct FedReturnData
 {
  double fedline[MAX_LINES], schedA[MAX_LINES], 
	fed_L1a, fed_L1b, fed_L1c, fed_L1d, fed_L1e, fed_L1f, fed_L1g, fed_L1h, fed_L1z, 
	fed_L2a, fed_L3a,
	fed_L4a, fed_L4b, fed_L5a, fed_L5b, fed_L6a, fed_L6b,
	schedA5a, schedA5b, schedA5c, schedA5,
	schedA8a, schedA8b, schedA8c, schedA8d,
	sched1[MAX_LINES], s1_8[30], s1_24[30], s2_17[30], s3_6[30], s3_13[30],
	fedl8b, fedl9b, fedl15a, fedl16a, fedl20a;
  int Exception, Itemized;
  char AlimRecipSSN[512], *AlimRecipName, OtherIncomeType[512], 
	OtherAdjustmentsType[512], OtherTaxesType[512],
	Dep1stName[10][512], DepLastName[10][512], 
	DepSocSec[10][512], DepRelation[10][512];
 } PrelimFedReturn;


void convert_slashes( char *fname )
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


void grab_line_value( char *label, char *fline, double *value )
{
 char twrd[2048];
 next_word(fline, twrd, " \t=;");
 if ((twrd[0] != '\0') && (sscanf(twrd,"%lf", value) != 1))
  {
   printf("Error: Reading Fed %s '%s%s'\n", label, twrd, fline);
   fprintf(outfile,"Error: Reading Fed %s '%s%s'\n", label, twrd, fline);
  }
}


void grab_line_string( char *fline, char *strng )
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


void grab_line_alloc( char *fline, char **strng )
{ /* Grab a string and allocate space for it. */
 char twrd[4096];
 grab_line_string( fline, twrd );
 if (twrd[0] != '\0')
  *strng = strdup( twrd );
}


int ImportFederalReturnData( char *fedlogfile, struct FedReturnData *fed_data )
{
 FILE *infile;
 char fline[2000], word[2000], tword[2000];
 int linenum, j;

 for (linenum=0; linenum<MAX_LINES; linenum++) 
  { 
   fed_data->fedline[linenum] = 0.0;
   fed_data->schedA[linenum] = 0.0;
   fed_data->sched1[linenum] = 0.0;
  }
 fed_data->fed_L1a = 0;
 fed_data->fed_L1b = 0;
 fed_data->fed_L1c = 0;
 fed_data->fed_L1d = 0;
 fed_data->fed_L1e = 0;
 fed_data->fed_L1f = 0;
 fed_data->fed_L1g = 0;
 fed_data->fed_L1h = 0;
 fed_data->fed_L1z = 0;
 fed_data->fed_L2a = 0;
 fed_data->fed_L3a = 0;
 fed_data->fed_L4a = 0;
 fed_data->fed_L4b = 0;
 fed_data->fed_L5a = 0;
 fed_data->fed_L5b = 0;
 fed_data->fed_L6a = 0;
 fed_data->fed_L6b = 0;
 fed_data->schedA5a = 0.0;
 fed_data->schedA5b = 0.0;
 fed_data->schedA5c = 0.0;
 fed_data->schedA8a = 0.0;
 fed_data->schedA8b = 0.0;
 fed_data->schedA8c = 0.0;
 fed_data->schedA8d = 0.0;

 fed_data->fedl8b = 0.0;
 fed_data->fedl9b = 0.0;
 fed_data->fedl15a = 0.0;
 fed_data->fedl16a = 0.0;
 fed_data->fedl20a = 0.0;
 for (j=0; j < 30; j++)
  {
   fed_data->s1_8[j] = 0.0;
   fed_data->s1_24[j] = 0.0;
   fed_data->s2_17[j] = 0.0;
   fed_data->s3_6[j] = 0.0;
   fed_data->s3_13[j] = 0.0;
  }
 strcpy( fed_data->AlimRecipSSN, "" );
 fed_data->AlimRecipName = strdup( "" );
 strcpy( fed_data->OtherIncomeType, "" );
 strcpy( fed_data->OtherAdjustmentsType, "" );
 strcpy( fed_data->OtherTaxesType, "" );
 for (j=0; j < 5; j++)
  {
   strcpy( fed_data->Dep1stName[j], "" );
   strcpy( fed_data->DepLastName[j], "" );
   strcpy( fed_data->DepSocSec[j], "" );
   strcpy( fed_data->DepRelation[j], "" );
  }
 convert_slashes( fedlogfile );
 infile = fopen(fedlogfile, "r");
 if (infile==0)
  {
   printf("Error: Could not open Federal return '%s'\n", fedlogfile);
   fprintf(outfile,"Error: Could not open Federal return '%s'\n", fedlogfile);
   exit( 1 ); 
  }
 fed_data->Itemized = 1; /* Set initial default values. */
 read_line(infile,fline);  linenum = 0;
 while (!feof(infile))
  {
   if (strstr(fline,"Use standard deduction.")!=0) fed_data->Itemized = 0;
   next_word(fline, word, " \t=");
   if ((word[0] == 'L') && (strstr(fline," = ")!=0))
    { /*L*/
     if (strcmp(word,"L1a") == 0)
      grab_line_value( word, fline, &(fed_data->fed_L1a) );
     else
     if (strcmp(word,"L1b") == 0)
      grab_line_value( word, fline, &(fed_data->fed_L1b) );
     else
     if (strcmp(word,"L1c") == 0)
      grab_line_value( word, fline, &(fed_data->fed_L1c) );
     else
     if (strcmp(word,"L1d") == 0)
      grab_line_value( word, fline, &(fed_data->fed_L1d) );
     else
     if (strcmp(word,"L1e") == 0)
      grab_line_value( word, fline, &(fed_data->fed_L1e) );
     else
     if (strcmp(word,"L1f") == 0)
      grab_line_value( word, fline, &(fed_data->fed_L1f) );
     else
     if (strcmp(word,"L1g") == 0)
      grab_line_value( word, fline, &(fed_data->fed_L1g) );
     else
     if (strcmp(word,"L1h") == 0)
      grab_line_value( word, fline, &(fed_data->fed_L1h) );
     else
     if (strcmp(word,"L1z") == 0)
      grab_line_value( word, fline, &(fed_data->fed_L1z) );
     else
     if (strcmp(word,"L2a") == 0)
      grab_line_value( word, fline, &(fed_data->fed_L2a) );
     else
     if (strcmp(word,"L3a") == 0)
      grab_line_value( word, fline, &(fed_data->fed_L3a) );
     else
     if (strcmp(word,"L4a") == 0)
      grab_line_value( word, fline, &(fed_data->fed_L4a) );
     else
     if (strcmp(word,"L4b") == 0)
      grab_line_value( word, fline, &(fed_data->fed_L4b) );
     else
     if (strcmp(word,"L5a") == 0)
      grab_line_value( word, fline, &(fed_data->fed_L5a) );
     else
     if (strcmp(word,"L5b") == 0)
      grab_line_value( word, fline, &(fed_data->fed_L5b) );
     else
     if (strcmp(word,"L6a") == 0)
      grab_line_value( word, fline, &(fed_data->fed_L6a) );
     else
     if (strcmp(word,"L6b") == 0)
      grab_line_value( word, fline, &(fed_data->fed_L6b) );
     else
     if (strcmp(word,"L8a") == 0)
      grab_line_value( word, fline, &(fed_data->fedline[8]) );
     else
     if (strcmp(word,"L8b") == 0)
      grab_line_value( word, fline, &(fed_data->fedl8b) );
     else
     if (strcmp(word,"L9a") == 0)
      grab_line_value( word, fline, &(fed_data->fedline[9]) );
     else
     if (strcmp(word,"L9b") == 0)
      grab_line_value( word, fline, &(fed_data->fedl9b) );
     else
     if (strcmp(word,"L15a") == 0)
      grab_line_value( word, fline, &(fed_data->fedl15a) );
     else
     if (strcmp(word,"L15b") == 0)
      grab_line_value( word, fline, &(fed_data->fedline[15]) );
     else
     if (strcmp(word,"L16a") == 0)
      grab_line_value( word, fline, &(fed_data->fedl16a) );
     else
     if (strcmp(word,"L16b") == 0)
      grab_line_value( word, fline, &(fed_data->fedline[16]) );
     else
     if (strcmp(word,"L20a") == 0)
      grab_line_value( word, fline, &(fed_data->fedl20a) );
     else
     if (strcmp(word,"L20b") == 0)
      grab_line_value( word, fline, &(fed_data->fedline[20]) );
     else
      {
       if (sscanf(&word[1],"%d",&linenum)!=1)
	{
	 printf("Error: Reading Fed line number '%s%s'\n",word,fline);
	 fprintf(outfile,"Error: Reading Fed line number '%s%s'\n",word,fline);
	}
       next_word(fline, word, " \t=");
       if (sscanf(word,"%lf", &fed_data->fedline[linenum])!=1)
	{
	 printf("Error: Reading Fed line %d '%s%s'\n",linenum,word,fline);
	 fprintf(outfile,"Error: Reading Fed line %d '%s%s'\n",linenum,word,fline);
	}
       if (verbose) printf("FedLin[%d] = %2.2f\n", linenum, fed_data->fedline[linenum]);
      }
    } /*L*/
   else

   if (strcmp(word, "Your1stName:" ) == 0)
    grab_line_alloc( fline, &Your1stName );
   else
   if (strcmp(word, "YourLastName:" ) == 0)
    grab_line_alloc( fline, &YourLastName );
   else
   if (strcmp(word, "YourSocSec#:" ) == 0)
    grab_line_alloc( fline, &your_socsec );
   else
   if (strcmp(word, "Spouse1stName:" ) == 0)
    grab_line_alloc( fline, &Spouse1stName );
   else
   if (strcmp(word, "SpouseLastName:" ) == 0)
    grab_line_alloc( fline, &SpouseLastName );
   else
   if (strcmp(word, "SpouseSocSec#:" ) == 0)
    grab_line_alloc( fline, &spouse_socsec );
   else
   if (strcmp(word, "Number&Street:" ) == 0)
    grab_line_alloc( fline, &street_address );
   else
   if (strcmp(word, "Apt#:" ) == 0)
    grab_line_alloc( fline, &apartment );
   else
   if (strcmp(word, "Town/City:" ) == 0)
    grab_line_alloc( fline, &town );
   else
   if (strcmp(word, "ZipCode:" ) == 0)
    grab_line_alloc( fline, &zipcode );
   else

   if (strncmp(word, "AlimRecipSSN", 12) == 0)
    grab_line_string( fline, fed_data->AlimRecipSSN );
   else
   if (strcmp( word, "S1_8z_Type:" ) == 0)
    grab_line_string( fline, fed_data->OtherIncomeType );
   else
   if (strcmp( word, "S1_24z_Type:" ) == 0)
    grab_line_string( fline, fed_data->OtherAdjustmentsType );
   else
   if (strcmp( word, "S2_17z_Type:" ) == 0)
    grab_line_string( fline, fed_data->OtherTaxesType );
   else

   if (strcmp(word, "Dep1_FirstName:") == 0)
    grab_line_string( fline, fed_data->Dep1stName[1] );
   else
   if (strcmp(word, "Dep1_LastName:") == 0)
    grab_line_string( fline, fed_data->DepLastName[1] );
   else
   if (strcmp(word, "Dep1_SocSec#:") == 0)
    grab_line_string( fline, fed_data->DepSocSec[1] );
   else
   if (strcmp(word, "Dep1_Relation:") == 0)
    grab_line_string( fline, fed_data->DepRelation[1] );
   else

   if (strcmp(word, "Dep2_FirstName:") == 0)
    grab_line_string( fline, fed_data->Dep1stName[2] );
   else
   if (strcmp(word, "Dep2_LastName:") == 0)
    grab_line_string( fline, fed_data->DepLastName[2] );
   else
   if (strcmp(word, "Dep2_SocSec#:") == 0)
    grab_line_string( fline, fed_data->DepSocSec[2] );
   else
   if (strcmp(word, "Dep2_Relation:") == 0)
    grab_line_string( fline, fed_data->DepRelation[2] );
   else

   if (strcmp(word, "Dep3_FirstName:") == 0)
    grab_line_string( fline, fed_data->Dep1stName[3] );
   else
   if (strcmp(word, "Dep3_LastName:") == 0)
    grab_line_string( fline, fed_data->DepLastName[3] );
   else
   if (strcmp(word, "Dep3_SocSec#:") == 0)
    grab_line_string( fline, fed_data->DepSocSec[3] );
   else
   if (strcmp(word, "Dep3_Relation:") == 0)
    grab_line_string( fline, fed_data->DepRelation[3] );
   else

   if ((word[0] == 'A') && (strstr(word,"AMT")!=word) && (strstr(fline," = ")!=0))
    {
     if (strcmp(word,"A5a") == 0)
      grab_line_value( word, fline, &(fed_data->schedA5a) );
     else
     if (strcmp(word,"A5b") == 0)
      grab_line_value( word, fline, &(fed_data->schedA5b) );
     else
     if (strcmp(word,"A5c") == 0)
      grab_line_value( word, fline, &(fed_data->schedA5c) );
     else
     if (strcmp(word,"A8a") == 0)
      grab_line_value( word, fline, &(fed_data->schedA8a) );
     else
     if (strcmp(word,"A8b") == 0)
      grab_line_value( word, fline, &(fed_data->schedA8b) );
     else
     if (strcmp(word,"A8c") == 0)
      grab_line_value( word, fline, &(fed_data->schedA8c) );
     else
     if (strcmp(word,"A8d") == 0)
      grab_line_value( word, fline, &(fed_data->schedA8d) );
     else
      {
       if (sscanf(&word[1],"%d",&linenum)!=1)
        {
	 printf("Error: Reading Fed line number '%s%s'\n",word,fline);
	 fprintf(outfile,"Error: Reading Fed line number '%s%s'\n",word,fline);
        }
       next_word(fline, word, " \t=");
       if (sscanf(word,"%lf", &fed_data->schedA[linenum])!=1) 
        {
 	 printf("Error: Reading Fed schedA %d '%s%s'\n",linenum,word,fline);
	 fprintf(outfile, "Error: Reading Fed schedA %d '%s%s'\n",linenum,word,fline);
        }
       if (verbose) printf("FedLin.A[%d] = %2.2f\n", linenum, fed_data->schedA[linenum]);
      }
    }
   else
   if ((strncmp( word, "S1_", 3 ) == 0) && (strstr(fline," = ")!=0))
    {
       next_word( &(word[3]), tword, " \t: =" );
       if ((tword[0] == '8') && (tword[1] >= 'a') && (tword[1] <= 'z'))
	{ int j;
	  if ((tword[1] >= 'a') && (tword[1] <= 'z'))
	   {
	    j = tword[1] - 'a';
	    next_word(fline, word, " \t=");
	    if (sscanf( word, "%lf", &fed_data->s1_8[j] ) != 1)
	     {
	      printf("Error: Reading Fed s1_8%c '%s%s'\n", 'a' + j, word, fline);
	      fprintf(outfile, "Error: Reading Fed s1_8%c '%s%s'\n", 'a' + j, word, fline);
	     }
	    if (verbose) printf("FedLin.S1_8%c] = %2.2f\n", 'a' + j, fed_data->s1_8[j] );
	   }
	  else
	   printf("Error: Unexpected line '%s'\n", word );
	}
       else
       if ((strncmp( tword, "24", 2 ) == 0) && (tword[2] >= 'a') && (tword[2] <= 'z'))
	{ int j;
	  if ((tword[2] >= 'a') && (tword[2] <= 'z'))
	   {
	    j = tword[2] - 'a';
	    next_word(fline, word, " \t=");
	    if (sscanf( word, "%lf", &fed_data->s1_24[j] ) != 1)
	     {
	      printf("Error: Reading Fed s1_24%c '%s%s'\n", 'a' + j, word, fline);
	      fprintf(outfile, "Error: Reading Fed s1_24%c '%s%s'\n", 'a' + j, word, fline);
	     }
	    if (verbose) printf("FedLin.S1_24%c = %2.2f\n", 'a' + j, fed_data->s1_24[j] );
	   }
	  else
	   printf("Error: Unexpected line '%s'\n", word );
	}
       else
	{
         if (sscanf( tword, "%d", &linenum ) != 1)
          {
  	   printf("Error: Reading Fed line number 'S1_%s %s'\n", tword, fline);
	   fprintf(outfile,"Error: Reading Fed line number 'S1_%s %s'\n", tword, fline);
           }
         next_word(fline, word, " \t=");
         if (sscanf(word,"%lf", &fed_data->sched1[linenum])!=1) 
          {
    	   printf("Error: Reading Fed sched1 %d '%s%s'\n", linenum, word, fline);
	   fprintf(outfile, "Error: Reading Fed sched1 %d '%s%s'\n", linenum, word, fline);
          }
         if (verbose) printf("FedLin.S1[%d] = %2.2f\n", linenum, fed_data->sched1[linenum]);
        }
    }
   else

   if ((strncmp( word, "S2_", 3 ) == 0) && (strstr(fline," = ")!=0))
    {
       next_word( &(word[3]), tword, " \t: =" );
       if ((strncmp( tword, "17", 2 ) == 0) && (tword[2] >= 'a') && (tword[2] <= 'z'))
	{ int j;
	  if ((tword[2] >= 'a') && (tword[2] <= 'z'))
	   {
	    j = tword[2] - 'a';
	    next_word(fline, word, " \t=");
	    if (sscanf( word, "%lf", &fed_data->s2_17[j] ) != 1)
	     {
	      printf("Error: Reading Fed s2_17%c '%s%s'\n", 'a' + j, word, fline);
	      fprintf(outfile, "Error: Reading Fed s2_17%c '%s%s'\n", 'a' + j, word, fline);
	     }
	    if (verbose) printf("FedLin.S2_17%c = %2.2f\n", 'a' + j, fed_data->s2_17[j] );
	   }
	  else
	   printf("Error: Unexpected line '%s'\n", word );
	}
    }
   else

   if ((strncmp( word, "S3_", 3 ) == 0) && (strstr(fline," = ")!=0))
    {
       next_word( &(word[3]), tword, " \t: =" );
       if ((strncmp( tword, "6", 1 ) == 0) && (tword[1] >= 'a') && (tword[1] <= 'z'))
	{ int j;
	  if ((tword[1] >= 'a') && (tword[1] <= 'z'))
	   {
	    j = tword[1] - 'a';
	    next_word(fline, word, " \t=");
	    if (sscanf( word, "%lf", &fed_data->s3_6[j] ) != 1)
	     {
	      printf("Error: Reading Fed s3_6%c '%s%s'\n", 'a' + j, word, fline);
	      fprintf(outfile, "Error: Reading Fed s3_6%c '%s%s'\n", 'a' + j, word, fline);
	     }
	    if (verbose) printf("FedLin.S3_6%c = %2.2f\n", 'a' + j, fed_data->s3_6[j] );
	   }
	  else
	   printf("Error: Unexpected line '%s'\n", word );
	}
       else
       if ((strncmp( tword, "13", 2 ) == 0) && (tword[2] >= 'a') && (tword[2] <= 'z'))
	{ int j;
	  if ((tword[2] >= 'a') && (tword[2] <= 'z'))
	   {
	    j = tword[2] - 'a';
	    next_word(fline, word, " \t=");
	    if (sscanf( word, "%lf", &fed_data->s3_13[j] ) != 1)
	     {
	      printf("Error: Reading Fed s3_13%c '%s%s'\n", 'a' + j, word, fline);
	      fprintf(outfile, "Error: Reading Fed s3_13%c '%s%s'\n", 'a' + j, word, fline);
	     }
	    if (verbose) printf("FedLin.S3_13%c = %2.2f\n", 'a' + j, fed_data->s3_13[j] );
	   }
	  else
	   printf("Error: Unexpected line '%s'\n", word );
	}
    }

   else
   if (strcmp(word,"Status") == 0)
    {
     next_word(fline, word, " \t=");
     if (strncasecmp(word,"Single",4)==0) status = SINGLE; else
     if (strncasecmp(word,"Married/Joint",13)==0) status = MARRIED_FILING_JOINTLY; else
     if (strncasecmp(word,"Married/Sep",11)==0) status = MARRIED_FILING_SEPARAT; else
     if (strncasecmp(word,"Head_of_House",4)==0) status = HEAD_OF_HOUSEHOLD; else
     if (strncasecmp(word,"Widow",4)==0) status = WIDOW;
     else 
      { 
       printf("Error: unrecognized status '%s'. Exiting.\n", word); 
       fprintf(outfile,"Error: unrecognized status '%s'. Exiting.\n", word);
       return 0; 
      }
    }
   read_line(infile,fline);
  }
 fclose(infile);
 return 1;
}


char *pull_initial( char *name )
{ /* Expect names like:  "John, D.", and pull initial out. */
  int j=0;
  char midinitial[10];
  while ((name[j] != '\0') && (name[j] == ' '))
   j++;
  while ((name[j] != '\0') && (name[j] != ',') && (name[j] != ' '))
   j++;
  if ((name[j] == ',') || (name[j] == ' '))
   {
    name[j++] = '\0';
    while ((name[j] != '\0') && (isspace( name[j] )))
     j++;
    midinitial[0] = name[j];
    midinitial[1] = '\0';
   }
  else
   strcpy( midinitial, "" );
  return strdup( midinitial );
}

void display_part2column( int j, int col )
{
 switch (col)
  {
   case 0: 
	if (sched540part2[j] != 0.0)
	 fprintf(outfile," SchedCA540_Part2_%d = %6.2f\n", j, sched540part2[j] );
	break;
   case 'a':
	if (sched540part2[j] != 0.0)
	 fprintf(outfile," SchedCA540_Part2_%da = %6.2f\n", j, sched540part2[j] );
	break;
   case 'b':
	if (sched540part2_sub[j] != 0.0)
	 fprintf(outfile," SchedCA540_Part2_%db = %6.2f\n", j, sched540part2_sub[j] );
	break;
   case 'c':
	if (sched540part2_add[j] != 0.0)
	 fprintf(outfile," SchedCA540_Part2_%dc = %6.2f\n", j, sched540part2_add[j] );
	break;
   default:  fprintf(outfile," Bad Case\n");
  }
}


void display_part2( int j )
{
 display_part2column( j, 'a' );
 display_part2column( j, 'b' );
 display_part2column( j, 'c' );
}


/*----------------------------------------------------------------------------*/
/* ---				Main					  --- */
/*----------------------------------------------------------------------------*/
int main( int argc, char *argv[] )
{
 int argk, j, k, iline7, iline8, iline9, iline10, CkFYHealthCoverage=0, L7a=0;
 double min2file=0.0, sched540A[MAX_LINES], sched540B[MAX_LINES], sched540C[MAX_LINES],
	sched540Ab[MAX_LINES], sched540Ac[MAX_LINES],
	sched540Bb[MAX_LINES], sched540Bc[MAX_LINES],
	sched540Cb[MAX_LINES], sched540Cc[MAX_LINES],
	threshA=0, std_ded=0;
 char word[4000], *infname=0, outfname[4000], prelim_1040_outfilename[5000];
 char	YourName[2048]="", YourNames[2048]="", 
	*YourMidInitial="", *SpouseMidInitial="";
 double  sched540A1a_sub=0.0, sched540A1b_sub=0.0, sched540A1c_sub=0.0, sched540A1d_sub=0.0,
	 sched540A1e_sub=0.0, sched540A1f_sub=0.0, sched540A1g_sub=0.0, sched540A1h_sub=0.0,
	 sched540A1z_sub=0.0;
 double  sched540A1a_add=0.0, sched540A1b_add=0.0, sched540A1c_add=0.0, sched540A1d_add=0.0,
	 sched540A1e_add=0.0, sched540A1f_add=0.0, sched540A1g_add=0.0, sched540A1h_add=0.0,
	 sched540A1i_add=0.0, sched540A1z_add=0.0;
 double  sched540Bc8a=0.0, sched540Bb8b=0.0, sched540Bb8c=0.0, sched540Bc8c=0.0, sched540Bc8d=0.0,
	 sched540Bc8e=0.0, sched540Bb8f=0.0, sched540Bc8k=0.0, sched540Bb8n=0.0, 
	 sched540Bb8o=0.0, sched540Bb8p=0.0, sched540Bc8p=0.0, sched540Bb8z=0.0, sched540Bc8z=0.0,
	 sched540Cb24b=0.0, sched540Cc24b=0.0, sched540Cb24c=0.0, sched540Cb24d=0.0, 
	 sched540Cb24f=0.0, sched540Cc24f=0.0, sched540Cb24g=0.0,  sched540Cc24g=0.0, 
	 sched540Cb24i=0.0, sched540Cb24j=0.0, sched540Cb24k=0.0, sched540Cb24z=0.0,
	 sched540Cc24z=0.0;
 int CkPayedUseTaxCDTFA=0;
 time_t now;

 /* Decode any command-line arguments. */
 argk = 1;  k=1;
 while (argk < argc)
 {
  if (strcmp(argv[argk],"-verbose")==0)  { verbose = 1; }
  else
  if (strcmp(argv[argk],"-round_to_whole_dollars")==0)  { round_to_whole_dollars = 1; }
  else
  if (k==1)
   {
    infname = strdup(argv[argk]);
    infile = fopen(argv[argk],"r");
    if (infile==0) {printf("ERROR: Parameter file '%s' could not be opened.\n", argv[argk]); exit(1);}
    k = 2;
    /* Base name of output file on input file. */
    strcpy(outfname,argv[argk]);
    j = strlen(outfname)-1;
    while ((j>=0) && (outfname[j]!='.')) j--;
    if (j<0) strcat(outfname,"_out.txt"); else strcpy(&(outfname[j]),"_out.txt");
    outfile = fopen(outfname,"w");
    if (outfile==0) {printf("ERROR: Output file '%s' could not be opened.\n", outfname); exit(1);}
    printf("Writing results to file:  %s\n", outfname);
   }
  else
   {printf("Unknown command-line parameter '%s'\n", argv[argk]); exit(1);}
  argk = argk + 1;
 }
 // test_tax_function();

 if (infile==0) {printf("Error: No input file on command line.\n"); exit(1);}

 /* Pre-initialize all lines to zeros. */
 for (j=0; j<MAX_LINES; j++) 
  { 
    L[j] = 0.0;  
    sched540A[j] = 0.0; 
    sched540Ab[j] = 0.0; 
    sched540Ac[j] = 0.0; 
    sched540B[j] = 0.0; 
    sched540Bb[j] = 0.0; 
    sched540Bc[j] = 0.0; 
    sched540C[j] = 0.0; 
    sched540Cb[j] = 0.0; 
    sched540Cc[j] = 0.0; 
    sched540part2[j] = 0.0; 
    sched540part2_sub[j] = 0.0; 
    sched540part2_add[j] = 0.0; 
  }

 /* Accept parameters from input file. */
 /* Expect  CA-540 lines, something like:
	Title:  CA 540 1999 Return
	L12	34900.0  {Wages}
 */

 printf("CA-540 2022 - v%3.2f\n", thisversion);

 // MarkupPDF( 1, 240, 40, 17, 1.0, 0, 0 ) NotReady "This program is NOT updated for 2022."
 // add_pdf_markup( "NotReady", 1, 240, 40, 17, 1, 1.0, 0, 0, "\"This program is NOT ready for 2022.\"" );
 #ifdef microsoft
  // system( "start bin\\notify_popup -delay 3 -expire 10 \"Warning: This program is NOT ready for 2022.\"" );
 #else
  // system( "bin/notify_popup -delay 3 -expire 10 \"Warning: This program is NOT ready for 2022.\" &" );
 #endif


 /* Accept Form's "Title" line, and put out with date-stamp for your records. */
 read_line( infile, word );
 now = time(0);
 fprintf(outfile,"\n%s,	 v%2.2f, %s\n", word, thisversion, ctime( &now ));
 check_form_version( word, "Title:  CA State Tax Form 540 - 2022" );

 get_parameter( infile, 's', word, "FileName" );      /* Preliminary Fed Return Output File-name. */
 get_word(infile, prelim_1040_outfilename );
 ImportFederalReturnData( prelim_1040_outfilename, &PrelimFedReturn );


 /* Only for 2021, handle next line(s) optionally, due to change in template. */
 get_parameter( infile, 's', word, "CountyName:" );
 get_parameter( infile, 'w', word, "CountyName:" );
 consume_leading_trailing_whitespace( word );
 if (word[0] != '\0')
  fprintf(outfile, " CountyName: %s\n", word );
   
 get_parameter( infile, 's', word, "CkSameAddress:" );
 get_parameter( infile, 'b', &j, "CkSameAddress:");
 if (j != 0)
  fprintf(outfile, "CkSameAddress: X\n");

 get_parameter( infile, 's', word, "L6" );   /* Are you a dependent? (yes/No). */


 /* Filing Status. */
 fprintf(outfile,"Fill-in Filing-Status box %d\n", status );
 switch (status)
 {
  case SINGLE: 		       fprintf(outfile,"Status = Single (%d)\nCkSingle: X\n", status );      L7a = 1; break;
  case MARRIED_FILING_JOINTLY: fprintf(outfile,"Status = Married/Joint (%d)\nCkMFJ: X\n", status );  L7a = 2; break;
  case MARRIED_FILING_SEPARAT: fprintf(outfile,"Status = Married/Sep (%d)\nCkMFS: X\n", status );    L7a = 1; break;
  case HEAD_OF_HOUSEHOLD:      fprintf(outfile,"Status = Head_of_Household (%d)\nCkHH: X\n", status); L7a = 1; break;
  case WIDOW: 		       fprintf(outfile,"Status = Widow(er) (%d)\nCkQW: X\n", status );       L7a = 1; break;
 }


 /* Exemptions. */
 // get_parameter( infile, 's', word, "L6" );	/* Are you a dependent? (yes/No). */
 get_parameter( infile, 'b', &j, "L6");
 L[6] = j;
 if (L[6] == 0)
  fprintf(outfile,"L6 = no\n");
 else
  fprintf(outfile,"L6 = yes, (check box on line 6).\n  CkDep: X\n");

 if ((status==SINGLE) || (status==MARRIED_FILING_SEPARAT) || (status==HEAD_OF_HOUSEHOLD))
  iline7 = 1;  else  iline7 = 2;
 if (L[6] != 0.0) iline7 = 0; /* <-- Possible exceptions here. */
 L[7] = 140.0 * iline7;							/* Updated for 2022. */
 showline(7);

 fprintf(outfile,"L7a = %d\n", L7a );

 get_parameter( infile, 's', word, "L8" );	/* Blind?, 1 if you or spouse, 2 if both. */
 get_parameter( infile, 'i', &iline8, "L8" );
 L[8] = iline8 * 140.0;							/* Updated for 2022. */
 showline(8);
 if (iline8 > 0) fprintf(outfile,"  L8a = %d\n", iline8 );

 get_parameter( infile, 's', word, "L9" );	/* Senior?, 1 if you or spouse, 2 if both. */
 get_parameter( infile, 'i', &iline9, "L9" );
 L[9] = iline9 * 140.0;							/* Updated for 2022. */
 showline(9);
 if (iline9 > 0) fprintf(outfile,"  L9a = %d\n", iline9 );

 get_parameter( infile, 's', word, "L10" );  /* Number of Dependents. */
 get_parameter( infile, 'i', &iline10, "L10"); 
 L[10] = iline10 * 433.0;						/* Updated for 2022. */
 showline(10);
 if (iline10 > 0) fprintf(outfile,"  L10a = %d\n", iline10 );

 L[11] = L[7] + L[8] + L[9] + L[10];
 showline_wmsg(11, "Exemption amount");

 /* Taxable Income. */
 GetLineF( "L12", &L[12] );		/* State Wages (W2 box 16). */

 L[13] = PrelimFedReturn.fedline[11];	/* Fed Wages (Fed 1040 line 11). */
 showline(13);


 /* -- Sched540 Part I -- */

  GetLine("CA540_Subtr_A1a", &sched540A1a_sub );
  GetLine("CA540_Addit_A1a", &sched540A1a_add );

  GetLine("CA540_Subtr_A1b", &sched540A1b_sub );
  GetLine("CA540_Addit_A1b", &sched540A1b_add );

  GetLine("CA540_Subtr_A1c", &sched540A1c_sub );
  GetLine("CA540_Addit_A1c", &sched540A1c_add );

  GetLine("CA540_Subtr_A1d", &sched540A1d_sub );
  GetLine("CA540_Addit_A1d", &sched540A1d_add );

  GetLine("CA540_Subtr_A1e", &sched540A1e_sub );
  GetLine("CA540_Addit_A1e", &sched540A1e_add );

  GetLine("CA540_Subtr_A1f", &sched540A1f_sub );
  GetLine("CA540_Addit_A1f", &sched540A1f_add );

  GetLine("CA540_Subtr_A1g", &sched540A1g_sub );
  GetLine("CA540_Addit_A1g", &sched540A1g_add );

  GetLine("CA540_Subtr_A1h", &sched540A1h_sub );
  GetLine("CA540_Addit_A1h", &sched540A1h_add );

  GetLine("CA540_Addit_A1i", &sched540A1i_add );

  GetLine("CA540_Subtr_A2", &(sched540Ab[2]) );
  GetLine("CA540_Addit_A2", &(sched540Ac[2]) );
  GetLine("CA540_Subtr_A3", &(sched540Ab[3]) );
  GetLine("CA540_Addit_A3", &(sched540Ac[3]) );
  GetLine("CA540_Subtr_A4", &(sched540Ab[4]) );
  GetLine("CA540_Addit_A4", &(sched540Ac[4]) );
  GetLine("CA540_Subtr_A5", &(sched540Ab[5]) );
  GetLine("CA540_Addit_A5", &(sched540Ac[5]) );
  // GetLine("CA540_Subtr_A6", &(sched540Ab[6]) );	/* Soc Sec subtraction handled below. */
  GetLine("CA540_Subtr_A7", &(sched540Ab[7]) );
  GetLine("CA540_Addit_A7", &(sched540Ac[7]) );
 
  GetLine("CA540_Subtr_B1", &(sched540Bb[1]) );
  GetLine("CA540_Addit_B2", &(sched540Bc[2]) );
  GetLine("CA540_Subtr_B3", &(sched540Bb[3]) );
  GetLine("CA540_Addit_B3", &(sched540Bc[3]) );
  GetLine("CA540_Subtr_B4", &(sched540Bb[4]) );
  GetLine("CA540_Addit_B4", &(sched540Bc[4]) );
  GetLine("CA540_Subtr_B5", &(sched540Bb[5]) );
  GetLine("CA540_Addit_B5", &(sched540Bc[5]) );
  GetLine("CA540_Subtr_B6", &(sched540Bb[6]) );
  GetLine("CA540_Addit_B6", &(sched540Bc[6]) );
  GetLine("CA540_Subtr_B7", &(sched540Bb[7]) );

  GetLine("CA540_Addit_B8a", &sched540Bc8a );
  GetLine("CA540_Subtr_B8b", &sched540Bb8b );

  GetLine("CA540_Subtr_B8c", &sched540Bb8c );
  GetLine("CA540_Addit_B8c", &sched540Bc8c );
  GetLine("CA540_Addit_B8d", &sched540Bc8d );
  GetLine("CA540_Addit_B8e", &sched540Bc8e );
  GetLine("CA540_Subtr_B8f", &sched540Bb8f );
  GetLine("CA540_Addit_B8k", &sched540Bc8k );
  GetLine("CA540_Subtr_B8n", &sched540Bb8n );
  GetLine("CA540_Subtr_B8o", &sched540Bb8o );
  GetLine("CA540_Subtr_B8p", &sched540Bb8p );
  GetLine("CA540_Addit_B8p", &sched540Bc8p );
  GetLine("CA540_Subtr_B8z", &sched540Bb8z );
  GetLine("CA540_Addit_B8z", &sched540Bc8z );

  GetLine("CA540_Subtr_C11", &(sched540Cb[11]) );
  GetLine("CA540_Subtr_C12", &(sched540Cb[12]) );
  GetLine("CA540_Addit_C12", &(sched540Cc[12]) );
  GetLine("CA540_Subtr_C13", &(sched540Cb[13]) );
  GetLine("CA540_Addit_C14", &(sched540Cc[14]) );
  GetLine("CA540_Subtr_C15", &(sched540Cb[15]) );
  GetLine("CA540_Subtr_C17", &(sched540Cb[17]) );
  GetLine("CA540_Addit_C19", &(sched540Cc[19]) );
  PrelimFedReturn.AlimRecipName = GetTextLine( "CA540_AlimonyRecipName:" );
  GetLine("CA540_Subtr_C20", &(sched540Cb[20]) );
  GetLine("CA540_Addit_C20", &(sched540Cc[20]) );
  GetLine("CA540_Addit_C21", &(sched540Cc[21]) );

  GetLine("CA540_Subtr_C24b", &sched540Cb24b );
  GetLine("CA540_Addit_C24b", &sched540Cc24b );
  GetLine("CA540_Subtr_C24c", &sched540Cb24c );
  GetLine("CA540_Subtr_C24d", &sched540Cb24d );
  GetLine("CA540_Subtr_C24f", &sched540Cb24f );
  GetLine("CA540_Addit_C24f", &sched540Cc24f );
  GetLine("CA540_Subtr_C24g", &sched540Cb24g );
  GetLine("CA540_Addit_C24g", &sched540Cc24g );
  GetLine("CA540_Subtr_C24i", &sched540Cb24i );
  GetLine("CA540_Subtr_C24j", &sched540Cb24j );
  GetLine("CA540_Subtr_C24k", &sched540Cb24k );
  GetLine("CA540_Subtr_C24z", &sched540Cb24z );
  GetLine("CA540_Addit_C24z", &sched540Cc24z );

  showline_wlabelnz( " SchedCA540_A1a", PrelimFedReturn.fed_L1a );
  showline_wlabelnz( " SchedCA540_A1b", PrelimFedReturn.fed_L1b );
  showline_wlabelnz( " SchedCA540_A1c", PrelimFedReturn.fed_L1c );
  showline_wlabelnz( " SchedCA540_A1d", PrelimFedReturn.fed_L1d );
  showline_wlabelnz( " SchedCA540_A1e", PrelimFedReturn.fed_L1e );
  showline_wlabelnz( " SchedCA540_A1f", PrelimFedReturn.fed_L1f );
  showline_wlabelnz( " SchedCA540_A1g", PrelimFedReturn.fed_L1g );
  showline_wlabelnz( " SchedCA540_A1h", PrelimFedReturn.fed_L1h );
  showline_wlabelnz( " SchedCA540_A1z", PrelimFedReturn.fed_L1z );

  showline_wlabelnz( " SchedCA540_A1ab", sched540A1a_sub );
  showline_wlabelnz( " SchedCA540_A1bb", sched540A1b_sub );
  showline_wlabelnz( " SchedCA540_A1cb", sched540A1c_sub );
  showline_wlabelnz( " SchedCA540_A1db", sched540A1d_sub );
  showline_wlabelnz( " SchedCA540_A1eb", sched540A1e_sub );
  showline_wlabelnz( " SchedCA540_A1fb", sched540A1f_sub );
  showline_wlabelnz( " SchedCA540_A1gb", sched540A1g_sub );
  showline_wlabelnz( " SchedCA540_A1hb", sched540A1h_sub );
  sched540A1z_sub = sched540A1a_sub + sched540A1b_sub + sched540A1c_sub + sched540A1d_sub +
		    sched540A1e_sub + sched540A1f_sub + sched540A1g_sub + sched540A1h_sub;
  showline_wlabelnz( " SchedCA540_A1zb", sched540A1z_sub );

  showline_wlabelnz( " SchedCA540_A1ac", sched540A1a_add );
  showline_wlabelnz( " SchedCA540_A1bc", sched540A1b_add );
  showline_wlabelnz( " SchedCA540_A1cc", sched540A1c_add );
  showline_wlabelnz( " SchedCA540_A1dc", sched540A1d_add );
  showline_wlabelnz( " SchedCA540_A1ec", sched540A1e_add );
  showline_wlabelnz( " SchedCA540_A1fc", sched540A1f_add );
  showline_wlabelnz( " SchedCA540_A1gc", sched540A1g_add );
  showline_wlabelnz( " SchedCA540_A1hc", sched540A1h_add );
  showline_wlabelnz( " SchedCA540_A1ic", sched540A1i_add );
  sched540A1z_add = sched540A1a_add + sched540A1b_add + sched540A1c_add + sched540A1d_add +
		    sched540A1e_add + sched540A1f_add + sched540A1g_add + sched540A1h_add +
		    sched540A1i_add;
  showline_wlabelnz( " SchedCA540_A1zc", sched540A1z_add );


  sched540A[1] = PrelimFedReturn.fed_L1z;
  sched540B[10] = sched540B[10] + sched540A[1];

  sched540Bb[10] = sched540Bb[10] + sched540A1z_sub;

  sched540Bc[10] = sched540Bc[10] + sched540A1z_add;


  showline_wlabelnz( " SchedCA540_A2a", PrelimFedReturn.fed_L2a );

  sched540A[2] = PrelimFedReturn.fedline[2];
  sched540B[10] = sched540B[10] + sched540A[2];
  showline_wlabelnz( " SchedCA540_A2", sched540A[2] );

  sched540Bb[10] = sched540Bb[10] + sched540Ab[2];
  showline_wlabelnz( " SchedCA540_A2b", sched540Ab[2] );

  sched540Bc[10] = sched540Bc[10] + sched540Ac[2];
  showline_wlabelnz( " SchedCA540_A2c", sched540Ac[2] );

  showline_wlabelnz( " SchedCA540_A3a", PrelimFedReturn.fed_L3a );

  sched540A[3] = PrelimFedReturn.fedline[3];
  sched540B[10] = sched540B[10] + sched540A[3];
  showline_wlabelnz( " SchedCA540_A3", sched540A[3] );

  sched540Bb[10] = sched540Bb[10] + sched540Ab[3];
  showline_wlabelnz( " SchedCA540_A3b", sched540Ab[3] );

  sched540Bc[10] = sched540Bc[10] + sched540Ac[3];
  showline_wlabelnz( " SchedCA540_A3c", sched540Ac[3] );


  showline_wlabelnz( " SchedCA540_A4a", PrelimFedReturn.fed_L4a );

  sched540A[4] = PrelimFedReturn.fed_L4b;
  sched540B[10] = sched540B[10] + sched540A[4];
  showline_wlabelnz( " SchedCA540_A4",  sched540A[4] );

  sched540Bb[10] = sched540Bb[10] + sched540Ab[4];
  showline_wlabelnz( " SchedCA540_A4b", sched540Ab[4] );

  sched540Bc[10] = sched540Bc[10] + sched540Ac[4];
  showline_wlabelnz( " SchedCA540_A4c", sched540Ac[4] );


  showline_wlabelnz( " SchedCA540_A5a", PrelimFedReturn.fed_L5a );

  sched540A[5] = PrelimFedReturn.fed_L5b;
  sched540B[10] = sched540B[10] + sched540A[5];
  showline_wlabelnz( " SchedCA540_A5", sched540A[5] );

  sched540Bb[10] = sched540Bb[10] + sched540Ab[5];
  showline_wlabelnz( " SchedCA540_A5b", sched540Ab[5] );

  sched540Bc[10] = sched540Bc[10] + sched540Ac[5];
  showline_wlabelnz( " SchedCA540_A5c", sched540Ac[5] );

  showline_wlabelnz( " SchedCA540_A6a", PrelimFedReturn.fed_L6a );

  sched540A[6] = PrelimFedReturn.fed_L6b;
  sched540B[10] = sched540B[10] + sched540A[6];
  showline_wlabelnz( " SchedCA540_A6", sched540A[6] );

  sched540Ab[6] = sched540A[6];			/* Subtract SocSec payments from AGI in CA. */
  sched540Bb[10] = sched540Bb[10] + sched540Ab[6];
  showline_wlabelnz( " SchedCA540_A6b", sched540Ab[6] );

  sched540A[7] = PrelimFedReturn.fedline[7];
  sched540B[10] = sched540B[10] + sched540A[7];
  showline_wlabelnz( " SchedCA540_A7", sched540A[7] );

  sched540Bb[10] = sched540Bb[10] + sched540Ab[7];
  showline_wlabelnz( " SchedCA540_A7b", sched540Ab[7] );

  sched540Bc[10] = sched540Bc[10] + sched540Ac[7];
  showline_wlabelnz( " SchedCA540_A7c", sched540Ac[7] );

 for (j=1; j <= 7; j++)
  {
   sched540B[j] = PrelimFedReturn.sched1[j];
   sched540B[10] = sched540B[10] + sched540B[j];
   if (sched540B[j] != 0.0)
    fprintf(outfile," SchedCA540_B%d = %6.2f\n", j, sched540B[j] );

   sched540Bb[10] = sched540Bb[10] + sched540Bb[j];
   if (sched540Bb[j] != 0.0)
    fprintf(outfile," SchedCA540_B%db = %6.2f\n", j, sched540Bb[j] );

   sched540Bc[10] = sched540Bc[10] + sched540Bc[j];
   if (sched540Bc[j] != 0.0)
    fprintf(outfile," SchedCA540_B%dc = %6.2f\n", j, sched540Bc[j] );
  }

 for (j=0; j < 26; j++)
  {
   if (PrelimFedReturn.s1_8[j] != 0.0)
    fprintf(outfile," SchedCA540_B8%c = %6.2f\n", 'A' + j, PrelimFedReturn.s1_8[j] );
  }

  if (PrelimFedReturn.OtherIncomeType[0] != '\0')
   fprintf(outfile," SchedCA540_B8Za: %s\n", PrelimFedReturn.OtherIncomeType );

  sched540Bc[9] = sched540Bc[9] + sched540Bc8a;
  showline_wlabelnz( " SchedCA540_B8Ac",  sched540Bc8a );

  sched540Bb[9] = sched540Bb[9] + sched540Bb8b;
  showline_wlabelnz( " SchedCA540_B8Bb", sched540Bb8b );

  sched540Bb[9] = sched540Bb[9] + sched540Bb8c;
  showline_wlabelnz( " SchedCA540_B8Cb",  sched540Bb8c );

  sched540Bc[9] = sched540Bc[9] + sched540Bc8c;
  showline_wlabelnz( " SchedCA540_B8Cc",  sched540Bc8c );

  sched540Bc[9] = sched540Bc[9] + sched540Bc8d;
  showline_wlabelnz( " SchedCA540_B8Dc",  sched540Bc8d );

  sched540Bc[9] = sched540Bc[9] + sched540Bc8e;
  showline_wlabelnz( " SchedCA540_B8Ec", sched540Bc8e );

  sched540Bb[9] = sched540Bb[9] + sched540Bb8f;
  showline_wlabelnz( " SchedCA540_B8Fb", sched540Bb8f );

  sched540Bc[9] = sched540Bc[9] + sched540Bc8k;
  showline_wlabelnz( " SchedCA540_B8Kc", sched540Bc8k );

  sched540Bb[9] = sched540Bb[9] + sched540Bb8n;
  showline_wlabelnz( " SchedCA540_B8Nb", sched540Bb8n );

  sched540Bb[9] = sched540Bb[9] + sched540Bb8o;
  showline_wlabelnz( " SchedCA540_B8Ob",  sched540Bb8o );

  sched540Bb[9] = sched540Bb[9] + sched540Bb8p;
  showline_wlabelnz( " SchedCA540_B8Pb", sched540Bb8p );

  sched540Bc[9] = sched540Bc[9] + sched540Bc8p;
  showline_wlabelnz( " SchedCA540_B8Pc", sched540Bc8p );

  sched540Bb[9] = sched540Bb[9] + sched540Bb8z;
  showline_wlabelnz( " SchedCA540_B8Zb", sched540Bb8z );

  sched540Bc[9] = sched540Bc[9] + sched540Bc8z;
  showline_wlabelnz( " SchedCA540_B8Zc",  sched540Bc8z );

  sched540B[9] = PrelimFedReturn.sched1[9];
  showline_wlabelnz( " SchedCA540_B9A", sched540B[9] );
  showline_wlabelnz( " SchedCA540_B9Ab", sched540Bb[9] );
  showline_wlabelnz( " SchedCA540_B9Ac", sched540Bc[9] );

  sched540Bb[10] = sched540Bb[10] + sched540Bb[9];
  sched540Bc[10] = sched540Bc[10] + sched540Bc[9];

  sched540B[10] = sched540B[10] + sched540B[9];
  showline_wlabelnz( " SchedCA540_B10", sched540B[10] );
  showline_wlabelnz( " SchedCA540_B10b", sched540Bb[10] );
  showline_wlabelnz( " SchedCA540_B10c", sched540Bc[10] );


  for (j=11; j <= 23; j++)
   {
    sched540C[j] = PrelimFedReturn.sched1[j];
    sched540C[26] = sched540C[26] + sched540C[j];
    if (sched540C[j] != 0.0)
     fprintf(outfile," SchedCA540_C%d = %6.2f\n", j, sched540C[j] );

    sched540Cb[26] = sched540Cb[26] + sched540Cb[j];
    if (sched540Cb[j] != 0.0)
     fprintf(outfile," SchedCA540_C%db = %6.2f\n", j, sched540Cb[j] );

    sched540Cc[26] = sched540Cc[26] + sched540Cc[j];
    if (sched540Cc[j] != 0.0)
     fprintf(outfile," SchedCA540_C%dc = %6.2f\n", j, sched540Cc[j] );
   }

  if (PrelimFedReturn.AlimRecipSSN[0] != '\0')
   fprintf(outfile," AlimRecipSSN: %s\n", PrelimFedReturn.AlimRecipSSN );
  if (PrelimFedReturn.AlimRecipName[0] != '\0')
   fprintf(outfile," AlimRecipName: %s\n", PrelimFedReturn.AlimRecipName );

 for (j=0; j < 30; j++)
  {
   if (PrelimFedReturn.s1_24[j] != 0.0)
    fprintf(outfile," SchedCA540_C24%c = %6.2f\n", 'A' + j, PrelimFedReturn.s1_24[j] );
  }

  sched540Cb[25] = sched540Cb[25] + sched540Cb24b;
  showline_wlabelnz( " SchedCA540_C24Bb", sched540Cb24b );

  sched540Cc[25] = sched540Cc[25] + sched540Cc24b;
  showline_wlabelnz( " SchedCA540_C24Bc", sched540Cc24b );

  sched540Cb[25] = sched540Cb[25] + sched540Cb24c;
  showline_wlabelnz( " SchedCA540_C24Cb", sched540Cb24c );

  sched540Cb[25] = sched540Cb[25] + sched540Cb24d;
  showline_wlabelnz( " SchedCA540_C24Db", sched540Cb24d );

  sched540Cb[25] = sched540Cb[25] + sched540Cb24f;
  showline_wlabelnz( " SchedCA540_C24Fb", sched540Cb24f );

  sched540Cc[25] = sched540Cc[25] + sched540Cc24f;
  showline_wlabelnz( " SchedCA540_C24Fc", sched540Cc24f );

  sched540Cb[25] = sched540Cb[25] + sched540Cb24g;
  showline_wlabelnz( " SchedCA540_C24Gb", sched540Cb24g );

  sched540Cc[25] = sched540Cc[25] + sched540Cc24g;
  showline_wlabelnz( " SchedCA540_C24Gc", sched540Cc24g );

  sched540Cb[25] = sched540Cb[25] + sched540Cb24i;
  showline_wlabelnz( " SchedCA540_C24Ib", sched540Cb24i );

  sched540Cb[25] = sched540Cb[25] + sched540Cb24j;
  showline_wlabelnz( " SchedCA540_C24Jb", sched540Cb24j );

  sched540Cb[25] = sched540Cb[25] + sched540Cb24k;
  showline_wlabelnz( " SchedCA540_C24Kb", sched540Cb24k );

  sched540Cb[25] = sched540Cb[25] + sched540Cb24z;
  showline_wlabelnz( " SchedCA540_C24Zb", sched540Cb24z );

  sched540Cc[25] = sched540Cc[25] + sched540Cc24z;
  showline_wlabelnz( " SchedCA540_C24Zc", sched540Cc24z );

  if (PrelimFedReturn.OtherAdjustmentsType[0] != '\0')
   fprintf(outfile," SchedCA540_C24Ztype: %s\n", PrelimFedReturn.OtherAdjustmentsType );

  sched540C[25] = PrelimFedReturn.sched1[25];
  showline_wlabelnz( " SchedCA540_C25", sched540C[25] );
  showline_wlabelnz( " SchedCA540_C25b", sched540Cb[25] );
  showline_wlabelnz( "SchedCA540_C25c", sched540Cc[25] );

  sched540C[26] = sched540C[26] + sched540C[25];
  sched540Cb[26] = sched540Cb[26] + sched540Cb[25];
  sched540Cc[26] = sched540Cc[26] + sched540Cc[25];

  showline_wlabelnz( " SchedCA540_C26", sched540C[26] );
  showline_wlabelnz( " SchedCA540_C26b", sched540Cb[26] );
  showline_wlabelnz( "SchedCA540_C26c", sched540Cc[26] );

  sched540C[27] = sched540B[10] - sched540C[26];
  sched540Cb[27] = sched540Bb[10] - sched540Cb[26];
  sched540Cc[27] = sched540Bc[10] - sched540Cc[26];

  showline_wlabelnz( " SchedCA540_C27", sched540C[27] );
  showline_wlabelnz( " SchedCA540_C27b", sched540Cb[27] );
  showline_wlabelnz( "SchedCA540_C27c", sched540Cc[27] );


 /* -- Sched540 Part II -- */

 // GetLine("CA540_P2_1", &(sched540part2[1]) );	/* Medical and dental expenses */
 sched540part2[1] = PrelimFedReturn.schedA[1];
 sched540part2[2] = PrelimFedReturn.fedline[11];
 sched540part2[3] = 0.075 * sched540part2[2];
 sched540part2[4] = NotLessThanZero( sched540part2[1] - sched540part2[3] );
 GetLine("CA540_P2_Add_4", &(sched540part2_add[4]) );

 sched540part2_5a = PrelimFedReturn.schedA5a;
 sched540part2_5b = PrelimFedReturn.schedA5b;
 sched540part2_5c = PrelimFedReturn.schedA5c;
 sched540part2_5d = sched540part2_5a + sched540part2_5b + sched540part2_5c;
 if (status != MARRIED_FILING_SEPARAT)
  sched540part2[5] = smallerof( sched540part2_5d, 10000.0 );	/* Will be Line 5e. */
 else
  sched540part2[5] = smallerof( sched540part2_5d, 5000.0 );

 GetLine("CA540_P2_Sub_5a", &(sched540part2_sub[5]) );
 sched540part2_sub[5] = sched540part2_5a;
 sched540part2_add[5] = sched540part2_5d - sched540part2[5];

 sched540part2[6] = PrelimFedReturn.schedA[6];
 GetLine("CA540_P2_Sub_6", &(sched540part2_sub[6]) );
 GetLine("CA540_P2_Add_6", &(sched540part2_add[6]) );

 if (PrelimFedReturn.OtherTaxesType[0] != '\0')
   fprintf(outfile," SchedCA540_Part2_6type: %s\n", PrelimFedReturn.OtherTaxesType );

 sched540part2[7] = sched540part2[5] + sched540part2[6];
 sched540part2_sub[7] = sched540part2_sub[5] + sched540part2_sub[6];
 sched540part2_add[7] = sched540part2_add[5] + sched540part2_add[6];

 sched540part2_8a = PrelimFedReturn.schedA8a;
 GetLine("CA540_P2_Add_8a", &sched540part2_add8a );
 sched540part2_8b = PrelimFedReturn.schedA8b;
 GetLine("CA540_P2_Add_8b", &sched540part2_add8b );
 sched540part2_8c = PrelimFedReturn.schedA8c;
 GetLine("CA540_P2_Add_8c", &sched540part2_add8c );
 sched540part2_8d = PrelimFedReturn.schedA8d;
 GetLine("CA540_P2_Sub_8d", &sched540part2_sub8d );
 sched540part2[8] = sched540part2_8a + sched540part2_8b + sched540part2_8c + sched540part2_8d;
 sched540part2_sub[8] = sched540part2_sub8d; 
 sched540part2_add[8] = sched540part2_add8a + sched540part2_add8b + sched540part2_add8c;
 sched540part2[9] = PrelimFedReturn.schedA[9];
 GetLine("CA540_P2_Sub_9", &(sched540part2_sub[9]) );
 GetLine("CA540_P2_Add_9", &(sched540part2_add[9]) );
 sched540part2[10] = sched540part2[8] + sched540part2[9];
 sched540part2_sub[10] = sched540part2_sub[8] + sched540part2_sub[9];
 sched540part2_add[10] = sched540part2_add[8] + sched540part2_add[9];
 sched540part2[11] = PrelimFedReturn.schedA[11];
 GetLine("CA540_P2_Sub_11", &(sched540part2_sub[11]) );
 GetLine("CA540_P2_Add_11", &(sched540part2_add[11]) );
 sched540part2[12] = PrelimFedReturn.schedA[12];
 GetLine("CA540_P2_Sub_12", &(sched540part2_sub[12]) );
 GetLine("CA540_P2_Add_12", &(sched540part2_add[12]) );
 sched540part2[13] = PrelimFedReturn.schedA[13];
 GetLine("CA540_P2_Sub_13", &(sched540part2_sub[13]) );
 GetLine("CA540_P2_Add_13", &(sched540part2_add[13]) );
 sched540part2[14] = sched540part2[11] + sched540part2[12]+ sched540part2[13];
 sched540part2_sub[14] = sched540part2_sub[11] + sched540part2_sub[12]+ sched540part2_sub[13];
 sched540part2_add[14] = sched540part2_add[11] + sched540part2_add[12]+ sched540part2_add[13];
 sched540part2[15] = PrelimFedReturn.schedA[15];
 GetLine("CA540_P2_Sub_15", &(sched540part2_sub[15]) );
 GetLine("CA540_P2_Add_15", &(sched540part2_add[15]) );
 sched540part2[16] = PrelimFedReturn.schedA[16];
 GetLine("CA540_P2_Sub_16", &(sched540part2_sub[16]) );
 GetLine("CA540_P2_Add_16", &(sched540part2_add[16]) );

 sched540part2[17] = sched540part2[4] + sched540part2[7] + sched540part2[10] + sched540part2[14]
	+ sched540part2[15] + sched540part2[16];
 sched540part2_sub[17] = sched540part2_sub[4] + sched540part2_sub[7] + sched540part2_sub[10] + sched540part2_sub[14]
	+ sched540part2_sub[15] + sched540part2_sub[16];
 sched540part2_add[17] = sched540part2_add[4] + sched540part2_add[7] + sched540part2_add[10] + sched540part2_add[14]
	+ sched540part2_add[15] + sched540part2_add[16];

 sched540part2[18] = sched540part2[17] - sched540part2_sub[17] + sched540part2_add[17];

 GetLine("CA540_P2_19", &(sched540part2[19]) );
 GetLine("CA540_P2_20", &(sched540part2[20]) );
 GetLine("CA540_P2_21", &(sched540part2[21]) );
 sched540part2[22] = sched540part2[19] + sched540part2[20] + sched540part2[21];
 sched540part2[23] = PrelimFedReturn.fedline[11];
 sched540part2[24] = NotLessThanZero( 0.02 * sched540part2[23] );
 sched540part2[25] = NotLessThanZero( sched540part2[22] - sched540part2[24] );
 sched540part2[26] = sched540part2[18] + sched540part2[25];
 // GetLine( "Adj", &sched540part2[27] ); 	/* Now read above. */
 sched540part2[28] = sched540part2[26] + sched540part2[27];
 switch (status)
  {
   case SINGLE:
   case MARRIED_FILING_SEPARAT:  threshA = 229908.0;	std_ded = 5202.0;  break;	/* Updated for 2022. */
   case MARRIED_FILING_JOINTLY:
   case WIDOW:                   threshA = 459821.0;	std_ded = 10404.0;  break;
   case HEAD_OF_HOUSEHOLD:       threshA = 344867.0;	std_ded = 10404.0;  break;
  }
 if (L[13] > threshA)
  { /*Itemized Deductions Worksheet*/	/* Page 56. */
    double ws[40];
    // printf("Yes, Fed AGI (%6.2f) is more than threshold (%6.2f).\n", L[13], threshA );
    for (j=1; j <= 10; j++) ws[j] = 0.0;    
    ws[1] = sched540part2[28];
    ws[2] = PrelimFedReturn.schedA[4] + PrelimFedReturn.schedA[9] + PrelimFedReturn.schedA[15] + PrelimFedReturn.schedA[16];
    ws[3] = ws[1] - ws[2];
    if (ws[3] == 0.0)
     sched540part2[29] = ws[1];
    else
     {
      ws[4] = 0.8 * ws[3];
      ws[5] = L[13];
      ws[6] = threshA;
      ws[7] = ws[5] - ws[6];
      if (ws[7] == 0.0)
       sched540part2[29] = ws[1];
      else
       {
	ws[8] = 0.06 * ws[7];
	ws[9] = smallerof( ws[4], ws[8] );
	ws[10] = ws[1] - ws[9];
	sched540part2[29] = ws[10];
       }
     }
    for (j=1; j <= 10; j++)
     if (ws[j] != 0.0) fprintf(outfile,"  ItemizedDedWS%d = %6.2f\n", j, ws[j] );
  } /*Itemized Deductions Worksheet*/
 else
  {
   // printf("No, Fed AGI (%6.2f) is less than threshold (%6.2f).\n", L[13], threshA );
   sched540part2[29] = sched540part2[28];
  }
 sched540part2[30] = largerof( sched540part2[29], std_ded );

 /* Display the Part-II calculations. */
 display_part2column( 1, 0 );
 display_part2column( 2, 0 );
 display_part2column( 3, 0 );
 fprintf(outfile," SchedCA540_Part2_%da = %6.2f\n", 4, sched540part2[4] );
 fprintf(outfile," SchedCA540_Part2_%dc = %6.2f\n", 4, sched540part2_add[4] );
 if (sched540part2_5a != 0.0)
  fprintf(outfile," SchedCA540_Part2_5aa = %6.2f\n", sched540part2_5a );
 if (sched540part2_sub[5] != 0.0)
  fprintf(outfile," SchedCA540_Part2_5ab = %6.2f\n", sched540part2_sub[5] );
 if (sched540part2_5b != 0.0)
  fprintf(outfile," SchedCA540_Part2_5ba = %6.2f\n", sched540part2_5b );
 if (sched540part2_5c != 0.0)
  fprintf(outfile," SchedCA540_Part2_5ca = %6.2f\n", sched540part2_5c );
 if (sched540part2_5d != 0.0)
  fprintf(outfile," SchedCA540_Part2_5da = %6.2f\n", sched540part2_5d );
 if (sched540part2[5] != 0.0)
  fprintf(outfile," SchedCA540_Part2_5ea = %6.2f\n", sched540part2[5] );
 if (sched540part2_sub[5] != 0.0)
  fprintf(outfile," SchedCA540_Part2_5eb = %6.2f\n", sched540part2_sub[5] );
 if (sched540part2_add[5] != 0.0)
  fprintf(outfile," SchedCA540_Part2_5ec = %6.2f\n", sched540part2_add[5] );

 display_part2( 6 );
 display_part2( 7 );

 if (sched540part2_8a != 0.0)
  fprintf(outfile," SchedCA540_Part2_8aa = %6.2f\n", sched540part2_8a );
 if (sched540part2_add8a != 0.0)
  fprintf(outfile," SchedCA540_Part2_8ac = %6.2f\n", sched540part2_add8a );
 if (sched540part2_8b != 0.0)
  fprintf(outfile," SchedCA540_Part2_8ba = %6.2f\n", sched540part2_8b );
 if (sched540part2_add8b != 0.0)
  fprintf(outfile," SchedCA540_Part2_8bc = %6.2f\n", sched540part2_add8b );
 if (sched540part2_8c != 0.0)
  fprintf(outfile," SchedCA540_Part2_8ca = %6.2f\n", sched540part2_8c );
 if (sched540part2_add8c != 0.0)
  fprintf(outfile," SchedCA540_Part2_8cc = %6.2f\n", sched540part2_add8c );
 if (sched540part2_8d != 0.0)
  fprintf(outfile," SchedCA540_Part2_8da = %6.2f\n", sched540part2_8d );
 if (sched540part2_sub8d != 0.0)
  fprintf(outfile," SchedCA540_Part2_8db = %6.2f\n", sched540part2_sub8d );
 if (sched540part2[8] != 0.0)
  fprintf(outfile," SchedCA540_Part2_8ea = %6.2f\n", sched540part2[8] );
 if (sched540part2_sub[8] != 0.0)
  fprintf(outfile," SchedCA540_Part2_8eb = %6.2f\n", sched540part2_sub[8] );
 if (sched540part2_add[8] != 0.0)
  fprintf(outfile," SchedCA540_Part2_8ec = %6.2f\n", sched540part2_add[8] );
 
 for (j=9; j <= 17; j++)
   display_part2( j );
 display_part2column( 18, 0 );

 L[18] = sched540part2[30];

 for (j=19; j <= 30; j++)
   display_part2column( j, 0 );

 /* -- End Sched540 Part II -- */


 L[14] = sched540Cb[27];	/* CA Adjustments, Schedule CA 540 line 27 column B. */
 showline(14);

 L[15] = L[13] - L[14];
 if (L[15] < 0.0) fprintf(outfile,"L15 = (%f6.2)\n", -L[15] );
 else showline(15);

 L[16] = sched540Cc[27];	/* CA Adjustments, Schedule CA 540 line 37 column C. */
 showline(16);

 L[17] = L[15] + L[16];		/* CA Adjusted Gross Income (AGI). */
 showline(17);

 switch (status)
  {		/* Minimum AGI (Line 17) required to file. */		/* Updated for 2022. */
    case SINGLE:
    case HEAD_OF_HOUSEHOLD:
		if (iline9 == 0)		
		  min2file = 16730.0;		/*Under65*/
		else
		  min2file = 23730.0;		/*65over*/
	  break;
   case MARRIED_FILING_JOINTLY: 
		if (iline9 == 0)		
		  min2file = 33466.0;		/*BothUnder65*/
		else
		if (iline9 == 1)		
		  min2file = 40466.0;		/*OneUnder65*/
		else
		  min2file = 47466.0;		/*Both65over*/
	  break;
   case WIDOW:
		if (iline9 == 0)		
		  min2file = 31163.0;		/*Under65*/
		else
		  min2file = 34555.0;		/*65over*/
	  break;
   default:	min2file = 5.0;
  }
 if (L[17] <= min2file)
  fprintf(outfile,"You may not need to file CA Taxes, due to your California Adjusted Gross Income (%6.2f <= %6.2f).\n", 
	L[17], min2file );

 showline(18);	/* Computed above. */

 L[19] = NotLessThanZero( L[17] - L[18] );
 showline_wmsg(19,"Taxable Income");		/* Taxable income. */

 /* Tax. */
 if (L[19] < 100000.00)
  fprintf(outfile,"Check box from: Tax Table.\nCkTxTable: X\n");
 else
  fprintf(outfile,"Check box from: Tax Rate Schedule.\nCkTxRateSchd: X\n");
 L[31] = TaxRateFunction( L[19], status );
 showline( 31 );
 Report_bracket_info( L[19], status );

 if (L[13] > threshA)
  { /*Line32-Exemption-credits-worksheet*/
    double ws_a, ws_b, ws_c, ws_d, ws_e, ws_f, ws_g, ws_h, ws_i, ws_j, ws_k, ws_l, ws_m, ws_n;
    printf(" Doing AGI Limitations worksheet.\n"); 
    ws_a = L[13];
    ws_b = threshA;
    ws_c = ws_a - ws_b;
    if (status != MARRIED_FILING_SEPARAT)
	ws_d = Round(ws_c / 2500.0);  
    else 
	ws_d = Round(ws_c / 1250.0);
    ws_e = 6.0 * ws_d;  
    ws_f = iline7 + iline8 + iline9; 
    ws_g = ws_e * ws_f;
    ws_h = L[7] + L[8] + L[9];
    ws_i = NotLessThanZero( ws_h - ws_g );
    ws_j = iline10;
    ws_k = ws_e * ws_j;
    ws_l = L[10];
    ws_m = NotLessThanZero( ws_l - ws_k );
    ws_n = ws_i + ws_m;
    fprintf(outfile," AGI Worksheet:\n   a: %6.2f\n", ws_a);
    fprintf(outfile,"  b: %6.2f\n", ws_b);
    fprintf(outfile,"  c: %6.2f\n", ws_c);
    fprintf(outfile,"  d: %6.2f\n", ws_d);
    fprintf(outfile,"  e: %6.2f\n", ws_e);
    fprintf(outfile,"  f: %6.2f\n", ws_f);
    fprintf(outfile,"  g: %6.2f\n", ws_g);
    fprintf(outfile,"  h: %6.2f\n", ws_h);
    fprintf(outfile,"  i: %6.2f\n", ws_i);
    fprintf(outfile,"  j: %6.2f\n", ws_j);
    fprintf(outfile,"  k: %6.2f\n", ws_k);
    fprintf(outfile,"  l: %6.2f\n", ws_l);
    fprintf(outfile,"  m: %6.2f\n", ws_m);
    fprintf(outfile,"  n: %6.2f\n", ws_n);
    fprintf(outfile," Your exemptions may be limited. Used Exemptions-Credits-Worksheet for Line 21.\n");
    fprintf(outfile,"   WorkSheet[n]=%6.2f (vs. L11=%6.2f)\n", ws_n, L[11] );
    L[32] = ws_n;
  }
 else  L[32] = L[11];
 showline(32);

 L[33] = NotLessThanZero( L[31] - L[32] );
 showline(33);

 GetLineF( "L34", &L[34] );
 showline(34);		/* Taxes on distributions (sched G-1 or form FTB 5870A) */

 L[35] = L[33] + L[34];
 showline(35);

 /* Special Credits. */
 GetLineF( "L40", &L[40] );	/* Nonrefundable Child + Dependent Care Expenses Credit (pg 11). */
 fprintf(outfile," "); /* Indent next entry. */
 GetLineF( "L43", &L[43] );	/* Special credit 1 */
 GetLineF( "L44", &L[44] );	/* Special credit 2 */
 GetLineF( "L45", &L[45] );	/* Special credit 3+ */
 GetLineF( "L46", &L[46] );	/* Nonrefundable renter's credit */

 L[47] = L[40] + L[43] + L[44] + L[45] + L[46];
 showline(47);			/* Total credits. */

 L[48] = NotLessThanZero( L[35] - L[47] );
 showline(48);

 /* Other taxes. */
 GetLineF( "L61", &L[61] );	/* Alternative minimum tax Sched P. */

 GetLineF( "L62", &L[62] );	/* Mental Health Services Tax. */

 GetLineF( "L63", &L[63] );	/* Other taxes and credit recapture. */

 L[64] = L[48] + L[61] + L[62] + L[63];
 showline_wmsg( 64, "Total Tax" );	/* Total tax. */

 /* Payments. */
 GetLineF( "L71", &L[71] ); 	/* CA income tax withheld. */
 
 GetLineF( "L72", &L[72] ); 	/* Estimated tax paid. */
 
 GetLineF( "L73", &L[73] ); 	/* Realestate withholding. */
 
 GetLineF( "L74", &L[74] ); 	/* Excess SDI. */

 GetLineF( "L75", &L[75] ); 	/* Earned Income Tax Credit (EITC). */

 GetLineF( "L76", &L[76] ); 	/* Young Child Tax Credit (YCTC). */

 GetLineF( "L77", &L[77] ); 	/* Net Premium Assistance Subsidy (PAS). */
 
 L[78] = L[71] + L[72] + L[73] + L[74] + L[75] + L[76] + L[77];
 showline_wmsg(78,"Total Payments");

 GetLineF( "L91", &L[91] );	/* Use Tax. */

 // GetYesNo( "CkPayedUseTaxCDTFA", &CkPayedUseTaxCDTFA );
 // GetYesNo( "CkFYHealthCoverage", &CkFYHealthCoverage );
 // GetLineF( "L92", &L[92] );	/* Individual Shared Responsibility (ISR) Penalty. */

 get_parameter( infile, 'l', word, "CkFYHealthCoverage" );
 get_parameters( infile, 'b', &CkPayedUseTaxCDTFA, "CkPayedUseTaxCDTFA" );
 get_parameter( infile, 'l', word, "CkFYHealthCoverage" );
 if (L[91] == 0.0)
  {
    if (CkPayedUseTaxCDTFA == 0)
     fprintf(outfile," CkNoUseTaxOwed = X\n");
    else
     fprintf(outfile," CkPayedUseTaxCDTFA = X\n");
  }
 if (strcmp( word, "CkFYHealthCoverage" ) == 0)
  {
   get_parameters( infile, 'b', &CkFYHealthCoverage, "CkFYHealthCoverage" );
   if (CkFYHealthCoverage != 0)
    fprintf(outfile, "CkFYHealthCoverage X\n");
   /* Now go ahead and get the expected next line. */
   GetLineF( "L92", &L[92] );  /* Individual Shared Responsibility (ISR) Penalty. */
  }
 else
 if (strcmp( word, "L92" ) == 0)
  { /* Get remaining part of line. */
   get_parameters( infile, 'f', &L[92], "L92" );
   fprintf(outfile, "L92 = %6.2f\n", L[92] );
  }


 if (L[78] > L[91])
  {
   L[93] = L[78] - L[91];
   showline(93);
  }
 else
 if (L[91] > L[78])
  {
   L[94] = L[91] - L[78];
   showline_wmsg(94, "Use Tax balance" );
  }

 if (L[93] > L[92])
  {
   L[95] = L[93] - L[92];
   showline(95);
  }
 else
 if (L[92] > L[93])
  {
   L[96] = L[92] - L[93];
   showline(96);    
  }

 GetLine( "L98", &L[98] );	/* Amount of refund to apply to next tear's estimated withholding. */
 GetLine( "L112", &L[112] );	/* Interest, late penalties. */
 GetLine( "L113", &L[113] );	/* Underpayment of estimated tax penalty. (FTB 5805) */

 /* Refund / Tax-Due. */
 if (L[95] > L[64])
  {
   L[97] = L[95] - L[64];
   fprintf(outfile,"L97 = %6.2f  REFUND!!!\n", L[97] );
   showline(98);
   L[99] = L[97]  - L[98];
   showline(99);
   showline(112);
   showline(113);
   L[115] = L[99] - (L[110] + L[112] + L[113]);
   showline(115);
  }
 else
  {
   L[100] = L[64] - L[95];
   fprintf(outfile,"L100 = %6.2f  DUE !!!\n", L[100] );
   fprintf(outfile,"         (Which is %2.1f%% of your total tax.)\n", 100.0 * L[100] / (L[64] + 1e-9) );
   L[111] = L[94] + L[96] + L[100] + L[110];
   showline(111);
   showline(112);
   showline(113);
   L[114] = L[111] + L[112] + L[113];
   showline(114);
  }
 
 fprintf(outfile,"\nSelect any charity contributions and complete\n form accordingly.\n");

 fprintf(outfile,"\n{ --------- }\n");
 writeout_line = 0;
 // Your1stName = GetTextLineF( "Your1stName:" );
 YourMidInitial = pull_initial( Your1stName );
 fprintf(outfile,"Your1stName: %s\n", Your1stName );
 fprintf(outfile,"YourMidInit: %s\n", YourMidInitial );
 // YourLastName   = GetTextLineF( "YourLastName:" );
 fprintf(outfile,"YourLastName: %s\n", YourLastName );
 // your_socsec = GetTextLineF( "YourSocSec#:" );
 format_socsec( your_socsec, 1 );
 fprintf(outfile,"YourSocSec#: %s\n", your_socsec );

 if ((status != SINGLE) && (status != WIDOW))
  {
   // Spouse1stName = GetTextLineF( "Spouse1stName:" );
   SpouseMidInitial = pull_initial( Spouse1stName );
   fprintf(outfile,"Spouse1stName: %s\n", Spouse1stName );
   fprintf(outfile,"SpouseMidInit: %s\n", SpouseMidInitial );
   // SpouseLastName = GetTextLineF( "SpouseLastName:" );
   fprintf(outfile,"SpouseLastName: %s\n", SpouseLastName );
   // spouse_socsec = GetTextLineF( "SpouseSocSec#:" );
   format_socsec( spouse_socsec, 1 );
   fprintf(outfile,"SpouseSocSec#: %s\n", spouse_socsec );
  }
 else
  {
   Spouse1stName[0] = '\0';
   SpouseLastName[0] = '\0';
  }
 writeout_line = 1;

 if (strlen( YourLastName ) > 0)
  {
   strcpy( YourName, Your1stName );
   strcat( YourName, " " );
   strcat( YourName, YourLastName );
   YourName[26] = '\0';		/* Limit to no longer than about 25 characters. */
   fprintf(outfile,"YourName: %s\n", YourName );

   if (strcmp( YourLastName, SpouseLastName ) == 0)
    sprintf(YourNames,"%s & %s, %s", Your1stName, Spouse1stName, YourLastName );
   else
   if (strlen( SpouseLastName ) > 0)
    sprintf(YourNames,"%s %s & %s %s", Your1stName, YourLastName, Spouse1stName, SpouseLastName );
   else
    sprintf(YourNames,"%s %s", Your1stName, YourLastName );
   fprintf(outfile,"YourNames: %s\n", YourNames );
  }
 // GetTextLineF( "Number&Street:" );
 Show_String_wLabel( "Number&Street:", street_address );
 // GetTextLineF( "Apt#:" );
 Show_String_wLabel( "Apt#:", apartment );
 // GetTextLineF( "Town:" );
 Show_String_wLabel( "Town:", town );
 fprintf(outfile,"State: CA\n");
 // GetTextLineF( "Zipcode:" );
 Show_String_wLabel( "ZipCode:", zipcode );
 GetTextLineF( "YourDOB:" );
 GetTextLineF( "SpouseDOB:" );

 for (j=1; j <=3; j++)
  if (strlen(PrelimFedReturn.Dep1stName[j]) > 0)
   {
    fprintf(outfile,"L10Dep%dFrstName: %s\n", j, PrelimFedReturn.Dep1stName[j] );
    fprintf(outfile,"L10Dep%dLastName: %s\n", j, PrelimFedReturn.DepLastName[j] );
    fprintf(outfile,"L10Dep%dSSN: %s\n", j, PrelimFedReturn.DepSocSec[j] );
    fprintf(outfile,"L10Dep%dRelation: %s\n", j, PrelimFedReturn.DepRelation[j] );
   }

 fclose(infile);
 grab_any_pdf_markups( infname, outfile );
 fclose(outfile);
 Display_File( outfname );
 return 0;
}

#undef SINGLE
#undef MARRIED_FILING_JOINTLY
#undef MARRIED_FILING_SEPARAT
#undef HEAD_OF_HOUSEHOLD
#undef WIDOW

#undef MAX_LINES

} // namespace taxsolve_CA_540_2022
} // namespace OpenTaxSolver2022

#undef printf
#undef system
#ifdef _MSC_VER
#undef strcasecmp
#undef strncasecmp
#endif


