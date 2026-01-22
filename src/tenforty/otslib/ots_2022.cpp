#include <ctype.h>
#include <locale.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <time.h>
#define printf(...)
#define system(...)
namespace OpenTaxSolver2022 {
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

namespace taxsolve_HSA_f8889 {
#define Yes 1
#define No  0
/************************************************************************/
/* TaxSolve_HSA_f8889.c - 						*/
/* Copyright (C)  2022 - A. Roberts					*/
/* 									*/
/* Compile:								*/
/*  cc taxsolve_HSA_f8889.c -o ../bin/taxsolve_HSA_f8889		*/
/*									*/
/* Documentation & Updates:						*/
/*        http://opentaxsolver.sourceforge.net/				*/
/*									*/
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
/* Updated for 2022 tax year:						*/
/************************************************************************/

float thisversion=20.00;



/*----------------------------------------------------------------------------*/

int main( int argc, char *argv[] )
{
 int i, j, k;
 char word[4000], outfname[4000], *answ, *infname=0;
 time_t now;
 double L14a=0.0, L14b=0.0, L14c=0.0, L17b=0.0;

 printf("Form 8889 HSA, 2022 - v%3.2f\n", thisversion );

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

 /* Accept parameters from input file. */
 /* Expect  Sched C lines, something like:
	Title:  f8889 Return
	L2		{Returns and Allowances}
	. . .
 */

 /* Accept Form's "Title" line, and put out with date-stamp for your records. */
 read_line( infile, word );
 now = time(0);
 fprintf(outfile,"\n%s,	 v%2.2f, %s\n", word, thisversion, ctime( &now ));
 check_form_version( word, "Title: 8889 HSA Form - 2022" );

 GetTextLineF( "YourName:" );
 GetTextLineF( "YourSocSec#:" );
 
 writeout_line = 0;     /* Suppress GetLineF's from immediately writing to outfile. */
 answ = GetTextLineF( "L1:" );
 next_word( answ, word, " \t;" );
 if (strcasecmp( word, "Self-Only" ) == 0)
  fprintf(outfile,"CkSelf-Only: X\n");
 else
  fprintf(outfile,"CkFamily: X\n");
 writeout_line = 1;

 GetLineF( "L2", &L[2] );
 GetLineF( "L3", &L[3] );
 GetLineF( "L4", &L[4] );
 L[5] = NotLessThanZero( L[3] - L[4] );
 showline( 5 );

 GetLine( "L6", &L[6] );
 if (!value_was_detected)
  L[6] = L[5];			/* If user did not supply a value for L6, then L6 must be set to L5. */
 showline( 6 );

 GetLineF( "L7", &L[7] );
 L[8] = L[6] + L[7];
 showline( 8 );
 GetLineF( "L9", &L[9] );
 GetLineF( "L10", &L[10] );
 L[11] = L[9] + L[10];
 showline( 11 );
 L[12] = NotLessThanZero( L[8] - L[11] );
 showline( 12 );
 L[13] = SmallerOf( L[2], L[12] );
 showline_wmsg( 13, "HSA Deduction.  Enter this on Sched-1 Part II, Line 13 on your 1040 Form." );
 if (L[2] > L[13])
  fprintf(outfile,"Caution: Since L2 > L13, you may have to pay additional tax. See instructions.\n\n");

 GetLineF( "L14a", &L14a );
 GetLineF( "L14b", &L14b );
 L14c = L14a - L14b;
 showline_wlabel( "L14c", L14c ); 
 GetLineF( "L15", &L[15] );
 L[16] = NotLessThanZero( L14c - L[15] );
 showline_wmsg( 16, "Taxable HSA distributions. Include this on Sched-1 Line 8e on your 1040 Form." );

 writeout_line = 0;     /* Suppress GetLineF's from immediately writing to outfile. */
 answ = GetTextLineF( "L17a:" );
 next_word( answ, word, " \t;" );
 if (toupper( word[0] ) == 'Y')
  fprintf(outfile,"Ck17a: X\n");
 writeout_line = 1;

 if (toupper( word[0] ) != 'Y')
  {
   L17b = 0.20 * L[16];
   showline_wlabel( "L17b", L17b );
  }

 GetLineF( "L18", &L[18] );
 GetLineF( "L19", &L[19] );
 L[20] = L[18] + L[19];
 showline_wmsg( 20, "Total income. Include this on Sched-1 Line 8z on your 1040 Form." );

 L[21] = 0.10 * L[20];
 showline_wmsg( 21, "Additional tax. Include this on Sched-2 Line 17d on your 1040 Form." );

 fclose(infile);
 grab_any_pdf_markups( infname, outfile );
 fclose(outfile);

 printf("\nListing results from file: %s\n\n", outfname);
 Display_File( outfname );

 return 0;
}

#undef Yes
#undef No
}
namespace taxsolve_MA_1_2022 {
#define SINGLE 		        1
#define MARRIED_FILING_JOINTLY 2
#define MARRIED_FILING_SEPARAT 3
#define HEAD_OF_HOUSEHOLD       4
#define WIDOW		        5
#define Yes 1
#define No  0
/************************************************************************/
/* taxsolve_ma_1_2022.c - OpenTaxSolver for Mass Form 1 		*/
/* Copyright (C) 2022 							*/
/* 									*/
/* OTS Project Home Page and Updates:  					*/
/*		http://opentaxsolver.sourceforge.com/			*/
/* 									*/
/* Compile:   cc taxsolve_ma_1_2022.c -o taxsolve_ma_1_2022		*/
/* Run:       ./taxsolve_ma_1_2022  Mass1_2022.txt			*/
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
/* Robert Heller 2-10-2004	heller@deepsoft.com			*/
/* Updated 1-26-05	Aston Roberts & Robert Heller			*/
/*  ...									*/
/* Updated 1-2-2023	Aston Roberts 					*/
/************************************************************************/


float thisversion=20.00;




double Sum( double *v, int start_slot, int end_slot )
{
 int j;
 double result = 0.0;
 for (j=start_slot; j <= end_slot; j++) result += v[j];
 return result;
}


double ComputeTax(double taxableIncome)
{ double taxrate=0.05;					/* Updated for 2022. */
 if (taxableIncome < 24000.0)
  return (int)(taxrate * (taxableIncome + 25.0) + 0.5);
 else
  return taxableIncome * taxrate;
}


void check_if_yes( char *label )
{
 char word[999];
 int flag;
 get_parameter( infile, 's', word, label );
 get_param_single_line( infile, 'b', &flag, label );
 if (flag) 
  fprintf(outfile,"%s X\n", label );
}



/*----------------------------------------------------------------------------*/
/* ---				Main					  --- */
/*----------------------------------------------------------------------------*/
int main( int argc, char *argv[] )
{
 int i, j, k, status=0, i65, iblind, ndep, ndep12=0;
 int flag, notaxstatus=0;
 char word[4000], *infname=0, outfname[4000], *answ;
 time_t now;
 double Exemptions[10], L_a=0.0, L_b=0.0;
 double MassBankInterest, Iexempt, AGI;
 double Unemployment, Lottery;
 double MassRetirement[2];
 double L23a=0.0, L33[6], L35a=0.0, L35b=0.0, L38a=0.0, L38b=0.0, L38c=0.0;
 double L43a=0.0, L43b=0.0;
 
 printf("Massachusetts Form-1 2022 - v%3.2f\n", thisversion);
 
 /* Decode any command-line arguments. */
 i = 1;  k=1;
 while (i < argc)
 {
  if (strcmp(argv[i],"-verbose")==0)  { verbose = 1; }
  else
  if (k==1)
   {
    infname = strdup(argv[i]);
    infile = fopen(argv[i],"r");
    if (infile==0) {printf("ERROR: Parameter file '%s' could not be opened.\n", argv[i]); exit(1);}
    k = 2;
    /* Base name of output file on input file. */
    strcpy(outfname,argv[i]);
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

 /* Accept parameters from input file. */
 /* Expect  Mass. Form-1 lines, something like:
	Title:  Mass Form 1 Return
	L2		{Exemptions}
	L3		{Wages}
 */

 /* Accept Form's "Title" line, and put out with date-stamp for your records. */
 read_line( infile, word );
 now = time(0);
 fprintf(outfile,"\n%s,	 v%2.2f, %s\n", word, thisversion, ctime( &now ));
 check_form_version( word, "Title:  Massachusetts Form 1 Tax Form - 2022" );

 /* Get status as:  Single, Married/joint, Head house, Married/sep. */
 get_parameter( infile, 's', word, "Status" );
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

 for (i=0; i<10; i++) {Exemptions[i] = 0.0;}

 switch (status)
  {
   case SINGLE: 
	Exemptions[0] = 4400.0;
	fprintf(outfile," Check_single x\n");
   	break;
   case MARRIED_FILING_SEPARAT:
	Exemptions[0] = 4400.0;
	fprintf(outfile," Check_sep x\n");
   	break;
   case HEAD_OF_HOUSEHOLD:
	Exemptions[0] = 6800.0;
	fprintf(outfile," Check_hh x\n");
	break;
   case MARRIED_FILING_JOINTLY: 
	Exemptions[0] = 8800.0;
	fprintf(outfile," Check_mfj x\n");
	break;
  }   

 GetLineF( "La", &L_a );	/* Only for PDF form.  Not used otherwise. */
 GetLineF( "Lb", &L_b );	/* Only for PDF form.  Not used otherwise. */
 
 fprintf(outfile,"L2. Exemptions: \n");
 fprintf(outfile,"  2a.  = %6.2f   Personal exemptions\n", Exemptions[0]);

 get_parameter( infile, 's', word, "Dependents" );
 get_parameter( infile, 'i', &ndep, "Dependents"); 
 Exemptions[1] = (double)ndep * 1000.0;
 if (Exemptions[1] > 0.0)
  {
   fprintf(outfile,"  2bnum  = %d  Number of dependents\n", ndep );
   fprintf(outfile,"  2b.  = %6.2f  %d x 1,000\n", Exemptions[1], ndep );
  }

 i65 = 0; iblind = 0;
 get_parameter( infile, 's', word, "Age65You");
 get_parameter( infile, 'b', &flag, "Your age over 65?");
 if (flag)
  {
   i65++;
   fprintf(outfile,"Check_2cyou X\n");
  }
 get_parameter( infile, 's', word, "Age65Spouse");
 get_param_single_line( infile, 'b', &flag, "Spouse age over 65?");
 if (flag) 
  {
   i65++;
   fprintf(outfile,"Check_2csp X\n");
  }
 Exemptions[2] = (double)i65 * 700.0;
 if (Exemptions[2] > 0)
  {
   fprintf(outfile,"  2cnum = %d    Age 65 or over\n", i65 );
   fprintf(outfile,"  2c. = %6.2f    %d x 700\n", Exemptions[2], i65 );
  }

 get_parameter( infile, 's', word, "BlindYou");
 get_parameter( infile, 'b', &flag, "Your Blindness?");
 if (flag) 
  {
   iblind++;
   fprintf(outfile,"Check_2dyou X\n");
  }
 get_parameter( infile, 's', word, "BlindSpouse");
 get_param_single_line( infile, 'b', &flag, "Spouse Blindness?");
 if (flag) 
  {
   iblind++;
   fprintf(outfile,"Check_2dsp X\n");
  }
 Exemptions[3] = (double)iblind * 2200.0;
 if (Exemptions[3] > 0)
  {
   fprintf(outfile,"  2dnum = %d    Blindness\n", iblind );
   fprintf(outfile,"  2d. = %6.2f     %d x 2,200\n", Exemptions[3], iblind );
  }

 GetLine( "Med/Dental", &(Exemptions[4]) );
 fprintf(outfile,"  2e. = %6.2f\n", Exemptions[4] );

 GetLine( "Adoption", &(Exemptions[5]) );
 fprintf(outfile,"  2f. = %6.2f\n", Exemptions[5] );

 L[2] = Sum( Exemptions, 0, 5 );
 fprintf(outfile,"  2g. = %6.2f Total Exemptions\n", L[2] );

 GetLine( "L3", &L[3] );	/* Wages, salery, tips (W-2). */
 showline(3);

 GetLine( "L4", &L[4] );	/* Taxable pensions. */
 ShowLineNonZero(4);

 GetLineF( "L5a", &MassBankInterest );
 if (status == MARRIED_FILING_JOINTLY)
  Iexempt = 200;
 else
  Iexempt = 100;
 fprintf(outfile,"L5b = %6.2f\n", Iexempt );
 L[5] = MassBankInterest - Iexempt;
 if (L[5] < 0.0) L[5] = 0.0;
 if (L[5] > 0.0)
  {
   sprintf(word,"Mass. Bank Interest: a. %6.2f - b. exemption %6.2f",
	MassBankInterest,Iexempt);
   showline_wmsg( 5, word );
  }

 GetLine( "L6", &L[6] );	/* Business income/loss. */
 ShowLineNonZero(6);

 GetLine( "L7", &L[7] );	/* Rental, royality, REMIC. */
 ShowLineNonZero(7);

 GetLineF( "L8a", &Unemployment );	/* Unemployment */
 GetLineF( "L8b", &Lottery );		/* Lottery */
 L[8] = Unemployment + Lottery;
 if (L[8] > 0)
  {
   sprintf(word,"a. %6.2f + b. %6.2f", Unemployment, Lottery);
   showline_wmsg( 8, word );
  }

 GetLine( "L9", &L[9] );	/* Other Income, Alimony received. */
 ShowLineNonZero(9);

 L[10] = Sum( L, 3, 9 );
 showline_wmsg( 10, "TOTAL 5.0% INCOME" );
 
 /* Amount paid to SS, Medicare, RR, US, or Mass retirement */
 GetLine( "L11a", &MassRetirement[0] ); /* You */
 if (MassRetirement[0] > 2000) MassRetirement[0] = 2000;
 showline_wlabel( "L11a", MassRetirement[0] );

 GetLine( "L11b", &MassRetirement[1] ); /* Spouse */
 if (MassRetirement[1] > 2000) MassRetirement[1] = 2000;
 showline_wlabel( "L11b", MassRetirement[1] );

 L[11] = Sum(MassRetirement, 0, 1);
 if (L[11] > 0)
  {
   sprintf(word,"you %6.2f + spouse %6.2f", MassRetirement[0], MassRetirement[1]);
   showline_wmsg(11,word);
  }

 GetLine( "L14a", &L[14] );	/* Rental Paid */
 showline_wlabel( "L14a", L[14] );
 L[14] = L[14] / 2.0;
 if (status == MARRIED_FILING_SEPARAT)
  L[14] = smallerof( L[14] , 1500.0 );
 else
  L[14] = smallerof( L[14] , 3000.0 );
 ShowLineNonZero(14);

 GetLine( "L15", &L[15] );	/* Other Deductions (sched Y, L19) */
 ShowLineNonZero(15);

 L[16] = Sum( L, 11, 15 );
 showline_wmsg(16,"Total Deductions");

 L[17] = NotLessThanZero( L[10] - L[16] );
 showline(17);

 L[18] = L[2];
 showline(18);

 L[19] = NotLessThanZero( L[17] - L[18] );
 showline(19);

 GetLine( "L20", &L[20] );	/* Interest and Dividends -- Sched B */
 L[20] = NotLessThanZero( L[20] );
 showline(20);

 L[21] = L[19] + L[20];
 showline_wmsg(21, "Total 5.0% Taxable Income");

 L[22] = ComputeTax( L[21] );
 showline_wmsg(22,"5.0% Tax");

 GetLineF( "L23a", &L23a ); 	/* 12% income */
 L[23] = NotLessThanZero( L23a * 0.12 );
 if (L23a > 0.0)
  {
   sprintf(word,"12%% Income tax: a. %6.2f x 0.12", L23a);
   showline_wmsg(23, word);
  }

 GetLine( "L24", &L[24] ); 	/* Tax on long-term capital gains, sched D */
 ShowLineNonZero(24);

 GetLine( "L25", &L[25] ); 	/* Credit Capture amount Sch. H-2 */
 ShowLineNonZero(25);

 GetLine( "L26", &L[26] ); 	/* Additional tax on installment sale */
 ShowLineNonZero(26);

 L[28] = Sum( L, 22, 26 );

 if ((status == SINGLE) || (status == HEAD_OF_HOUSEHOLD) || (status == MARRIED_FILING_JOINTLY))
 { /* AGI Worksheet pg 13. */
   double ws[20], threshA, threshB;
   for (j=0; j<20; j++) ws[j] = 0.0;
   ws[1] = NotLessThanZero( L[10] );
   ws[2] = 0.0;		/* Sched Y lines 1-10.  Assumed zero, adjust otherwise. */
   ws[3] = NotLessThanZero( ws[1] - ws[2] );
   ws[4] = smallerof( MassBankInterest, Iexempt );
   if (L[10] < 0.0)
    ws[4] = NotLessThanZero( ws[4] + L[10] );
   ws[5] = L[20];
   ws[6] = 0.0;   /* Assumed zero. */
   ws[7] = ws[3] + ws[4] + ws[5] + ws[6];
   AGI = ws[7];
   for (j=1; j<=7; j++)
    fprintf(outfile,"     AGI_Worksheet[%d] = %6.2f\n", j, ws[j] );
   fprintf(outfile,"   AGI = %6.2f\n", AGI );
   if (status != MARRIED_FILING_SEPARAT)
    { /*not_sep*/
     switch (status)
      {
       case SINGLE:  
		threshA = 8000.0;
		threshB = 14000.0;
		break;
       case HEAD_OF_HOUSEHOLD:
		threshA = 14400.0 + 1000.0 * ndep;
		threshB = 25200.0 + 1750.0 * ndep;
		break;
       case MARRIED_FILING_JOINTLY: 
		threshA = 16400.0 + 1000.0 * ndep;
		threshB = 28700.0 + 1750.0 * ndep;
		break;
	default: fprintf(outfile,"Bad filing status.\n"); 
		printf("Bad filing status.\n");  exit(1); break;
      }
     if (AGI <= threshA)
      {
	notaxstatus = 1;
	fprintf(outfile,"    (%6.2f <= %6.2f)\n", AGI, threshA );
	fprintf(outfile,"You qualify for No Tax Status.\n");
      } else
     if (AGI <= threshB)
      fprintf(outfile,"See Form 1 Line 29 special instructions for Limited Income.\n");
    } /*not_sep*/
}

 if (notaxstatus) L[28] = 0.0;
 showline_wmsg(28, "Total Income Tax");

 GetLine1( "L29", &L[29] ); 	/* Limited Income Credit */
 GetLine1( "L30", &L[30] ); 	/* Income tax paid to another state or jurisdiction (from Schedule OJC). */
 GetLine1( "L31", &L[31] ); 	/* Other credits from Sch Z, line 14 */
 if (notaxstatus) { L[29] = 0.0;  L[3] = 0.0; }
 ShowLineNonZero(29);
 ShowLineNonZero(30);
 ShowLineNonZero(31);

 L[32] = NotLessThanZero( L[28] - (L[29] + L[30] + L[31]) );
 showline_wmsg(32,"Income Tax After Credits");

 GetLine1( "L33a", &L33[0] ); /* Endangered Wildlife */
 if (L33[0] != 0) showline_wlabel( "L33a", L33[0] );
 GetLine1( "L33b", &L33[1] ); /* Organ Transplant */
 if (L33[1] != 0) showline_wlabel( "L33b", L33[1] );
 GetLine1( "L33c", &L33[2] ); /* Mass AIDS */
 if (L33[2] != 0) showline_wlabel( "L33c", L33[2] );
 GetLine1( "L33d", &L33[3] ); /* Mass US Olympic */
 if (L33[3] != 0) showline_wlabel( "L33d", L33[3] );
 GetLine1( "L33e", &L33[4] ); /* Mass Military Family Relief */
 if (L33[4] != 0) showline_wlabel( "L33e", L33[4] );
 GetLine1( "L33f", &L33[5] ); /* Homeless Animal Prevention And Care */
 if (L33[5] != 0) showline_wlabel( "L33f", L33[5] );
 L[33] = Sum( L33, 0, 5 );
 ShowLineNonZero( 33 );

 GetLine1( "L34", &L[34] ); 	/* Use tax due on out-of-state purchases */
 showline(34);

 GetLine1( "L35a", &L35a ); 	/* Health Care Penalty (you) */
 showline_wlabel( "L35a", L35a );
 GetLine1( "L35b", &L35b ); 	/* Health Care Penalty (spouse) */
 showline_wlabel( "L35b", L35b );
 L[35] = L35a + L35b;
 if (L[35] != 0)
  showline_wmsg( 35, "Health Care penalty" );

 GetLine1( "L36", &L[36] );	/* AMENDED RETURN ONLY. Overpayment from original return. */
 L[36] = NotLessThanZero( L[36] );
 ShowLineNonZero( 36 );	 

 L[37] = Sum( L, 32, 36 );
 showline_wmsg(37,"Income Tax After Credits Contributions, Use Tax + HC Penalty");
 
 /* Payments section. */

 GetLineFnz( "L38a", &L38a );	/* Mass income tax withheld, Forms W-2 */
 GetLineFnz( "L38b", &L38b );	/* Mass income tax withheld, Forms 1099 */
 GetLineFnz( "L38c", &L38c );	/* Mass income tax withheld, Other forms. */
 L[38] = L38a + L38b + L38c;
 ShowLineNonZero(38);

 GetLine( "L39", &L[39] );	/* Last year's overpayment you want applied to 2022 estimated tax */
 ShowLineNonZero(39);

 GetLine( "L40", &L[40] );	/* 2022 estimated tax payments */
 ShowLineNonZero(40);

 GetLine( "L41", &L[41] );	/* Payments made with extension */
 ShowLineNonZero(41);

 GetLine( "L42", &L[42] );	/* Payments w/original return. Use only if amending return. */
 ShowLineNonZero(42);

 GetLineF( "L43a", &L43a );	/* Earned income credit (EIC): Number of dependent children.  */

 GetLineF( "L43b", &L43b );	/* Earned income credit (EIC): amount from US Return */
 if (L43b != 0.0) fprintf(outfile, " L43b = %6.2f\n", L43b );
 L[43] = L43b * 0.30;
 ShowLineNonZero(43);

 GetLine( "L44", &L[44] );	/* Senior Circuit Breaker Credit, sched CB */
 ShowLineNonZero(44);

 GetLine( "L45", &L[45] );	/* Child under 13, or disabled dep/spouse credit, from worksheet. */
 ShowLineNonZero(45);

 get_parameter( infile, 's', word, "L46num" );	/* Number of dependent household members under 13 or over 65. */
 get_parameters( infile, 'i', &ndep12, "L46num"); 
 fprintf(outfile,"L46num = %d\n", ndep12 ); 
 L[46] = ndep12 * 180.0;
 ShowLineNonZero(46);

 GetLine( "L47", &L[47] );	/* Refundable credits, Sched CMS. */
 ShowLineNonZero(47);

 L[48] = Sum( L, 43, 47 );
 showline_wmsg( 48, "total refundable credits");

 GetLine( "L49", &L[49] );	/* Excess Paid Family Leave withholding. */
 ShowLineNonZero(49);

 L[50] = Sum( L, 38, 42 ) + Sum( L, 48, 49 );
 showline_wmsg( 50, "total");

 GetLine( "L52", &L[52] );	/* Overpayment to be applied to next year's estimated tax */

 /* Refund or Owe section. */
 if (L[37] < L[50]) 
  {
   L[51] = L[50] - L[37];
   fprintf(outfile,"L51 = %6.2f  Overpayment!\n", L[51] );
   if (L[52] > L[51])
    L[52] = L[51];
   showline_wmsg( 52, "Overpayment to be applied to next year's estimated tax" );
   L[53] = L[51] - L[52];
   fprintf(outfile,"L53 = %6.2f  THIS IS YOUR REFUND\n", L[53] );
  }
 else 
  {
   L[54] = L[37] - L[50];
   fprintf(outfile,"L54 = %6.2f  TAX DUE !!!\n", L[54] );
   fprintf(outfile,"         (Which is %2.1f%% of your total tax.)\n", 100.0 * L[54] / (L[37] + 1e-9) );
   if ((L[54] > 400.0) && (L[50] < 0.80 * L[37]))
    fprintf(outfile," You may owe Underpayment of Estimated Tax penalty.\n");
  }

 fprintf(outfile,"\n{ --------- }\n");
 GetTextLineF( "Your1stName:" );
 GetTextLineF( "YourInitial:" );
 GetTextLineF( "YourLastName:" );
 answ = GetTextLine( "YourSocSec#:" );
 format_socsec( answ , 1 );
 fprintf(outfile,"YourSocSec#: %s\n", answ );
 GetTextLineF( "Spouse1stName:" );
 GetTextLineF( "SpouseInitial:" );
 GetTextLineF( "SpouseLastName:" );
 answ = GetTextLine( "SpouseSocSec#:" );
 format_socsec( answ , 1 );
 fprintf(outfile,"SpouseSocSec#: %s\n", answ );
 GetTextLineF( "Number&Street:" );
 GetTextLineF( "Town:" );
 GetTextLineF( "State:" );
 GetTextLineF( "Zipcode:" );

 GetTextLineF( "RoutingNum:" );
 GetTextLineF( "AccntNum:" );
 answ = GetTextLine( "AccountType:" ); 
 if (strcasecmp( answ, "Savings" ) == 0)
  fprintf(outfile," Check_SavingsAccnt X\n");
 if (strcasecmp( answ, "Checking" ) == 0)
  fprintf(outfile," Check_CheckingAccnt X\n");
 GetTextLineF( "Payment_Interest:" );
 GetTextLineF( "Payment_Penalty:" );                // Payment penalty you calculated.
 GetTextLineF( "M2210_Amount:" );                   // M-2210 amount.
 check_if_yes( "Check_SelfEmployed:" );	// Yes, if Self-Employed. (answer: Yes, No, n/a)
 check_if_yes( "Check_DORdiscuss:" );	// Yes, if you wish to discuss w/DOR. (answer: Yes, No, n/a)
 check_if_yes( "Check_DoNotEfile:" );	// Yes, if you do not want preparer to e-file. (answer: Yes, No, n/a)

 fclose(infile);
 grab_any_pdf_markups( infname, outfile );
 fclose(outfile);

 printf("\nListing results from file: %s\n\n", outfname);
 Display_File( outfname );
 printf("\nResults writen to file: %s\n", outfname);

 return 0;
}

#undef SINGLE
#undef MARRIED_FILING_JOINTLY
#undef MARRIED_FILING_SEPARAT
#undef HEAD_OF_HOUSEHOLD
#undef WIDOW
#undef Yes
#undef No
}
namespace taxsolve_NJ_1040_2022 {
#define SINGLE 		        1
#define MARRIED_FILING_JOINTLY  2
#define MARRIED_FILING_SEPARAT  3
#define HEAD_OF_HOUSEHOLD       4
#define WIDOW		        5
/************************************************************************/
/* TaxSolve_NJ_1040_2022.c - 						*/
/* Copyright (C) 2022 - Aston Roberts					*/
/* 									*/
/* Compile:   gcc taxsolve_NJ_1040_2022.c -o taxsolve_NJ_1040_2022	*/
/* Run:	      ./taxsolve_NJ_1040_2022  NJ_1040_2022.txt 		*/
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
/* Aston Roberts 3-12-2023	aston_roberts@yahoo.com			*/
/************************************************************************/

float thisversion=20.02;



double COJ[MAX_LINES], S[MAX_LINES], F[MAX_LINES];



double TaxRateFormula( double x, int status )
{
 if ((status==SINGLE) || (status==MARRIED_FILING_SEPARAT))	/* Single, Married/sep */
  {
   if (x < 20000.0)   return x * 0.014;               else
   if (x < 35000.0)   return x * 0.0175  -     70.0;  else
   if (x < 40000.0)   return x * 0.035   -    682.5;  else
   if (x < 75000.0)   return x * 0.05525 -   1492.5;  else
   if (x < 500000.0)  return x * 0.0637  -   2126.25; else
   if (x < 1000000.0) return x * 0.0897  -  15126.25;
   else		      return x * 0.1075  -  32926.25;
  }
 else
 if ((status==MARRIED_FILING_JOINTLY) || (status==HEAD_OF_HOUSEHOLD) || (status==WIDOW))
  {								/* Married/Joint, HouseHead, widower. */
   if (x < 20000.0)   return x * 0.014;              else
   if (x < 50000.0)   return x * 0.0175  -     70.0; else
   if (x < 70000.0)   return x * 0.0245  -    420.0; else
   if (x < 80000.0)   return x * 0.035   -   1154.5; else
   if (x < 150000.0)  return x * 0.05525 -   2775.0; else
   if (x < 500000.0)  return x * 0.0637  -   4042.5; else
   if (x < 1000000.0) return x * 0.0897  -  17042.5;
   else		      return x * 0.1075  -  34842.5;
  }
 else { printf("Status not covered.\n"); exit(1); }
}


void Report_bracket_info( double x, int status )
{
 double tx, rate;
 tx = TaxRateFormula( x, status );
 if ((status==SINGLE) || (status==MARRIED_FILING_SEPARAT))	/* Single, Married/sep */
  {
   if (x < 20000.0)   rate = 0.014;	else
   if (x < 35000.0)   rate = 0.0175;	else
   if (x < 40000.0)   rate = 0.035;	else
   if (x < 75000.0)   rate = 0.05525;	else
   if (x < 500000.0)  rate = 0.0637;    else
   if (x < 1000000.0) rate = 0.0897;
   else		      rate = 0.1075;
  }
 else
  {								/* Married/Joint, HouseHead, widower. */
   if (x < 20000.0)   rate = 0.014;	else
   if (x < 50000.0)   rate = 0.0175;	else
   if (x < 70000.0)   rate = 0.0245;	else
   if (x < 80000.0)   rate = 0.035;	else
   if (x < 150000.0)  rate = 0.05525;	else
   if (x < 500000.0)  rate = 0.0637;    else
   if (x < 100000.0)  rate = 0.0897;
   else		      rate = 0.1075;
  }
 printf(" You are in the %2.1f%% marginal tax bracket,\n and you are paying an effective %2.1f%% tax on your total income.\n",
	  100.0 * rate, 100.0 * tx / x );
 fprintf(outfile," You are in the %2.1f%% marginal tax bracket,\n and you are paying an effective %2.1f%% tax on your total income.\n",
	  100.0 * rate, 100.0 * tx / x );
}


double TaxRateFunction( double income, int status )     /* Emulates table lookup or function appropriately. */
{
 double x, dx, tx;
 int k;

 if (income < 100000.0)  /* Quantize to match tax-table exactly. */
  {
   x = 50.0;
   dx = 0.5 * x;
   k = (income - 0.000001) / x;
   x = x * (double)k + dx;
   tx = (int)(TaxRateFormula( x, status ) + 0.5);
  }
 else
  tx = TaxRateFormula( income, status );
 return tx;
}


void place_blocked_value( char *phrase, int numpre, int numpost, char *label )
{ /* Pad and quote a string so there are numpre chars before the radix point. */
 int j=0, k=0;
 char *buf;
 while ((phrase[j] != '.') && (phrase[j] != '\0')) j++;
 buf = (char *)malloc( strlen( phrase ) + numpre + numpost + 1 );
 if (j < numpre)
  k = numpre - j;
 for (j = 0; j < k; j++)
  buf[j] = ' ';
 buf[j] = '\0';
 strcat( buf, phrase );
 fprintf(outfile, "%s = \"%s\"\n", label, buf );
 free( buf );
}


/*----------------------------------------------------------------------------*/

int main( int argc, char *argv[] )
{
 int i, j, k, status=0, answer=0, HomeOwner=0, Tenant=0, eligible_proptax_ded=1;
 char word[1000], *infname=0, outfname[4000];
 time_t now;
 double L16b=0.0, L20b=0.0, L28a=0.0, L28b=0.0, L37a=0.0, L37b=0.0, L37c=0.0;
 double COJ_b[10], COJ_9a=0.0, proptxcredit, filing_threshold;
 double H[10], Hb[10];	/* Worksheet H, added by BWB. */
 double I[10], Ib[10];	/* Worksheet I. */
 char *Your1stName="", *YourLastName="", *YourInitial="", *Spouse1stName="", *SpouseLastName="", *SpouseInitial="";
 char YourNames[2048]="";

 /* Intercept any command-line arguments. */
 printf("NJ 1040 2022 - v%3.1f\n", thisversion);
 i = 1;  k=1;
 while (i < argc)
 {
  if (strcmp(argv[i],"-verbose")==0)  verbose = 1;
  else
  if (strcmp(argv[i],"-round_to_whole_dollars")==0)  { round_to_whole_dollars = 1; }
  else
  if (k==1)
   {
    infname = strdup(argv[i]);
    infile = fopen(argv[i],"r");
    if (infile==0) {printf("ERROR: Parameter file '%s' could not be opened.\n", argv[i]); exit(1);}
    k = 2;
    /* Base name of output file on input file. */
    strcpy(outfname,argv[i]);
    j = strlen(outfname)-1;
    while ((j>=0) && (outfname[j]!='.')) j--;
    if (j<0) strcat(outfname,"_out.txt"); else strcpy(&(outfname[j]),"_out.txt");
    outfile = fopen(outfname,"w");
    if (outfile==0) {printf("ERROR: Output file '%s' could not be opened.\n", outfname); exit(1);}
    printf("Writing results to file:  %s\n", outfname);
   }
  else {printf("Unknown command-line parameter '%s'\n", argv[i]); exit(1);}
  i = i + 1;
 }

 if (infile==0) {printf("Error: No input file on command line.\n"); exit(1);}

 /* Pre-initialize all lines to zeros. */
 for (i=0; i<MAX_LINES; i++) 
  {
   L[i] = 0.0;
   COJ[i] = 0.0;
   S[i] = 0.0;
   F[i] = 0.0;
  }

 /* Accept parameters from input file. */
 /* Expect  NJ-1040 lines, something like:
	Title:  NJ 1040 1999 Return
	L14		{Wages}
	L15a		{Interest}
	L16		{Dividends}
	L18		{Capital Gains}
	S1		{Property Tax}
	L42		{Witheld tax, from W-2}
*/

 /* Accept Form's "Title" line, and put out with date-stamp for records. */
 read_line( infile, word );
 now = time(0);
 fprintf(outfile,"\n%s,	 v%2.2f, %s\n", word, thisversion, ctime( &now ) );
 check_form_version( word, "Title:  NJ-1040 State 2022" );

 /* get_parameter(infile, kind, x, mesage ) */
 get_parameter( infile, 's', word, "Status" );
 get_parameter( infile, 'l', word, "Status ?");
 if (strncasecmp(word,"Single",4)==0) status = SINGLE; else
 if (strncasecmp(word,"Married/Joint",13)==0) status = MARRIED_FILING_JOINTLY; else
 if (strncasecmp(word,"Married/Sep",11)==0) status = MARRIED_FILING_SEPARAT; else
 if (strncasecmp(word,"Head_of_House",4)==0) status = HEAD_OF_HOUSEHOLD; else
 if (strncasecmp(word,"Widow",4)==0) status = WIDOW;
 else
  { 
   printf("Error: unrecognized status '%s'. Must be: Single, Married/joint, Married/sep, Head_of_house, Widow(er)\nExiting.\n", word); 
   fprintf(outfile,"Error: unrecognized status '%s'. Must be: Single, Married/joint, Married/sep, Head_of_house, Widow(er)\nExiting.\n", word); 
   exit(1); 
  }
 switch (status)
 {
  case SINGLE: 			fprintf(outfile,"Status = Single (%d)\n", status); 
				L[6] = 1;
				break;
  case MARRIED_FILING_JOINTLY: fprintf(outfile,"Status = Married/Joint (%d)\n", status); 
				fprintf(outfile," Check_Spouse = X\n"); 
				L[6] = 2;
				break;
  case MARRIED_FILING_SEPARAT: fprintf(outfile,"Status = Married/Sep (%d)\n", status);
				L[6] = 1;
				break;
  case HEAD_OF_HOUSEHOLD: 	fprintf(outfile,"Status = Head_of_Household (%d)\n", status); 
				L[6] = 1;
				break;
  case WIDOW: 		  	fprintf(outfile,"Status = Widow(er) (%d)\n", status); 
				L[6] = 1;
				break;
 }

 fprintf(outfile, "L6a = %d\n", (int)(L[6]) );
 L[6] = 1000.0 * L[6];
 shownum(6); 

 GetYesNoSL( "YouOver65", &answer );			/* Exemptions, Over 65. */
 L[7] = answer;
 if (answer) fprintf(outfile," Check_Over65 = X\n");

 GetYesNoSL( "SpouseOver65", &answer );			/* Exemptions, Spouse Over 65. */
 if (status == MARRIED_FILING_JOINTLY)
  {
   L[7] = L[7] + answer;
   if (answer) fprintf(outfile," Check_SpOver65 = X\n");
  }
 fprintf(outfile, "L7a = %d\n", (int)(L[7]) );
 L[7] = 1000.0 * L[7];
 shownum(7); 

 GetYesNoSL( "YouBlindDisa", &answer );   		/* Exemptions, Blind/disabled. */
 L[8] = answer;
 if (answer) fprintf(outfile," Check_Blind = X\n");

 GetYesNoSL( "SpouseBlindDisa", &answer );		/* Exemptions, Spouse Blind/disabled. */
 if (status == MARRIED_FILING_JOINTLY)
  {
   L[8] = L[8] + answer;
   if (answer) fprintf(outfile," Check_SpBlind = X\n");
  }
 fprintf(outfile, "L8a = %d\n", (int)(L[8]) );
 L[8] = 1000.0 * L[8];
 shownum(8); 

 GetYesNoSL( "YouVeteran", &answer );			/* Exemptions, Veteran */
 L[9] = answer;
 if (answer) fprintf(outfile," Check_Vet = X\n");

 GetYesNoSL( "SpouseVeteran", &answer );		/* Exemptions, Spouse Veteran */
 if (status == MARRIED_FILING_JOINTLY)
  {
   L[9] = L[9] + answer;
   if (answer) fprintf(outfile," Check_SpVet = X\n");
  }
 fprintf(outfile, "L9a = %d\n", (int)(L[9]) );
 L[9] = 6000.0 * L[9];
 shownum(9); 

 get_parameter( infile, 's', word, "L10" );	/* Exemptions, children. */
 get_param_single_line( infile, 'i', &j, "L10");
 fprintf(outfile, "L10a = %d\n", j );
 L[10] = 1500.0 * j;
 shownum(10); 

 get_parameter( infile, 's', word, "L11" );	/* Exemptions, other dependents. */
 get_param_single_line( infile, 'i', &j, "L11"); 
 fprintf(outfile, "L11a = %d\n", j );
 L[11] = 1500.0 * j;
 shownum(11); 

 get_parameter( infile, 's', word, "L12" );	/* Exemptions, college kids. */
 get_param_single_line( infile, 'i', &j, "L12"); 
 fprintf(outfile, "L11a = %d\n", j );
 L[12] = 1000.0 * j;
 shownum(12); 

 fprintf(outfile," FillOutForm_wRoundedNumbers_wZerosAfterDecPt\n" );

 L[13] = L[6] + L[7] + L[8] + L[9] + L[10] + L[11] + L[12];
 showline(13); 

 GetLineF( "L15", &L[15] );	/* Wages. */

 GetLineF( "L16a", &L[16] );	/* Taxable Interest. */

 /* Form asks for tax-exempt income, but does not use it. */
 GetLineF( "L16b", &L16b );	/* Tax-exempt Interest. */

 GetLineF( "L17", &L[17] );	/* Dividends. */

 GetLine( "L18", &L[18] );	/* Business profits, Fed Sched C. */
 if (L[18] < 0.0) L[18] = 0.0;
 showline(18);

 GetLine( "L19", &L[19] );	/* Capital Gains . */
 if (L[19] < 0.0) L[19] = 0.0;
 showline(19);

 GetLineF( "L20a", &L[20] );	/* Pensions, Annuities, and IRA Withdrawals (pg 20). */
 GetLineF( "L20b", &L20b );	/* Excludable Pensions, Annuities, and IRA Withdrawals (pg 20). */

 GetLineF( "L21", &L[21] );	/* Partnership income. (See pg 24.) */

 GetLineF( "L22", &L[22] );	/* S Corporation income. (See pg 24.) */

 GetLineF( "L23", &L[23] );	/* Rent, royalty, patents income. (Sched NJ-BUS-1, Part IV, Line 4.) */

 GetLineF( "L24", &L[24] );	/* Net gambling winnings. */

 GetLineF( "L25", &L[25] );	/* Alimony and maintenance payments RECEIVED. */

 GetLineF( "L26", &L[26] );	/* Other (See pg 24). */

 for (j=15; j <= 26; j++)
  L[27] = L[27] + L[j];
 showline_wmsg(27,"Total Income");	/* Total Income. */

 GetLineF( "L28a", &L28a );	/* Pension Exclusion (See pg 26). */
 GetLineF( "L28b", &L28b );	/* Other Retirement Income Exclusion (See worksheet pg 26). */
 L[28] = L28a + L28b;
 showline(28);

 L[29] = L[27] - L[28];
 showline_wmsg(29,"NJ Gross Income");

 if ((status == SINGLE) || (status == MARRIED_FILING_SEPARAT))		/* Min2File */
  filing_threshold = 10000.0;
 else
  filing_threshold = 20000.0;

 if (L[29] < filing_threshold)
   fprintf(outfile," --- You do not need to file, (except to get refund).  Income < $%6.2f. ---\n", filing_threshold );

 L[30] = L[13];
 showline(30);

 fprintf(outfile,"\n");
 GetLine( "F1", &F[1] );	/* Worksheet F Medical Expenses (See pg 27). */
 showline_wrksht('F',1,F);
 F[2] = 0.02 * L[29];
 showline_wrksht('F',2,F);
 F[3] = NotLessThanZero( F[1] - F[2] );
 showline_wrksht('F',3,F);
 GetLine( "F4", &F[4] );      /* Qualified Archer MSA contributions from Federal Form 8853 */
 showline_wrksht('F',4,F);
 GetLine( "F5", &F[5] );      /* Amount of self-employed health insurance deduction */
 showline_wrksht('F',5,F);
 F[6] = NotLessThanZero( F[3] + F[4] + F[5] );
 showline_wrksht('F',6,F);
 fprintf(outfile,"\n");
 L[31] = F[6];
 if (L[31] != 0.0)
  showline_wmsg(31," Medical Expenses Worksheet F (See pg 27)");
 /* end of Worksheet F */

 GetLineF( "L32", &L[32] );	/* Alimony and maintenance payments PAYED. */

 GetLineF( "L33", &L[33] );	/* Qualified Conservation Contribution. */

 GetLineF( "L34", &L[34] );	/* Health Enterprise Zone Deduction. */

 GetLineF( "L35", &L[35] );	/* Alternative Business Calc Adj (Sched NJ-BUS-2, Line 11). */

 GetLineF( "L36", &L[36] );	/* Organ/Bone Marrow Donation Deduction */

 GetLineF( "L37a", &L37a );	/* NJBEST Deduction */
 GetLineF( "L37b", &L37b );	/* NJCLASS Deduction */
 GetLineF( "L37c", &L37c );	/* NJ Higher Ed Tuition Deduction */

 for (j=30; j <= 36; j++)
  L[38] = L[38] + L[j];
 L[38] = L[38] + L37a + L37b + L37c;
 showline_wmsg( 38, "Total Exemptions and Deductions" );

 /* Taxable income. */
 L[39] = L[29] - L[38];
 if (L[39] < 0.0)
  L[39] = 0.0;
 showline_wmsg( 39, "Taxable Income" );

 GetLineF( "L40a", &L[40] );	/* Property Tax Paid. */
 GetYesNoSL( "HomeOwner:", &HomeOwner );	/* Y/N */
 GetYesNoSL( "Tenant:", &Tenant );		/* Y/N */
 if (HomeOwner && Tenant)
  fprintf(outfile," Check_Both_OwnerTenant = X\n");
 else
 if (HomeOwner)
  fprintf(outfile," Check_HomeOwner = X\n");
 else
 if (Tenant)
  fprintf(outfile," Check_Tenant = X\n");

 GetLine( "COJ1", &COJ[1] );	/* Income taxed by other jurisdictions, if any. */
 GetLine( "COJ9a",&COJ_9a );	/* Tax paid to other jurisdictions on that income, if any. */

 fprintf(outfile,"\n");  /* Property Tax Deduction Worksheet H (pg 30). */
 H[1] = L[40];
 showline_wrksht('H',1,H);
 if (status != MARRIED_FILING_SEPARAT)
  H[2] = smallerof( H[1], 15000.0 );
 else
  H[2] = smallerof( H[1],  7500.0 );
 showline_wrksht( 'H', 2, H );

 if (status != MARRIED_FILING_SEPARAT)
  proptxcredit = 50.0;
 else
  proptxcredit = 25.0;

 if ((L[29] < filing_threshold) && ((L[7] > 0) || (L[8] > 0)))
  { /* Not eligible for property tax deduction as per right column on p. 23 of instructions. */
    eligible_proptax_ded = 0;
    if (L[39] != 0.0)
     {
      printf("You are not eligible for property tax deduction as per right column on p. 23 of instructions.\n");
      fprintf(outfile,"You are not eligible for property tax deduction as per right column on p. 23 of instructions.\n");
     }
  }

 if (COJ_9a == 0.0)
  { /*Worksheet-H*/
    H[3] = L[39];	 Hb[3] = L[39];
    fprintf(outfile," H3a = %6.2f	H3b = %6.2f\n", H[3], Hb[3]);
    H[4] = H[2];	 Hb[4] = 0.0;
    fprintf(outfile," H4a = %6.2f	H4b = %6.2f\n", H[4], Hb[4]);
    H[5] = H[3] - H[4];  Hb[5] = Hb[3] - Hb[4];
    fprintf(outfile," H5a = %6.2f	H5b = %6.2f\n", H[5], Hb[5]);
    H[6] = TaxRateFunction( H[5], status );
    Hb[6] = TaxRateFunction( Hb[5], status );
    fprintf(outfile," H6a = %6.2f	H6b = %6.2f\n", H[6], Hb[6]);
    H[7] = Hb[6] - H[6];
    showline_wrksht('H',7,H);
    if (eligible_proptax_ded)
     { /*eligible*/
      if (H[7] >= proptxcredit)
       { /*yes*/
         fprintf(outfile," H8. Yes. (Take Property Tax Deduction.)\n");
         L[41] = H[4];
         L[42] = H[5];
         L[43] = H[6];
         L[57] = 0.0;
       } /*yes*/
      else
       { /*no*/
         fprintf(outfile," H8. No. (Take Property Tax Credit.)\n");
         L[41] = 0.0;
         L[42] = Hb[5];
         L[43] = Hb[6];
         L[57] = proptxcredit;
       } /*no*/
     } /*eligible*/
    else
     { /*not_eligble*/
       L[41] = 0.0;
       L[42] = Hb[5];
       L[43] = Hb[6];
       L[57] = 0.0;
     } /*not_eligble*/
  } /*Worksheet-H*/
 else
  { /*Sched COJ +Worksheet-I*/
    fprintf(outfile,"\nSchedule COJ Credit for Income or Wage Taxes Paid to Other Jurisdiction (Previously Sched A):\n");
    showline_wlabel("COJ_1", COJ[1]); 
    COJ[2] = L[29];
    showline_wlabel("COJ_2", COJ[2]);
    COJ[3] = smallerof( 1.0, (COJ[1] / COJ[2]) );
    fprintf(outfile," COJ_3 = %6.2f %%\n", 100.0 * COJ[3] );
    COJ[4] = L[39];
    fprintf(outfile," COJ_4a = %6.2f	COJ_4b = %6.2f\n", COJ[4], COJ[4] );
    fprintf(outfile," (5a = %6.2f)\n", H[1] );
    COJ[5] = H[2];
    fprintf(outfile," COJ_5a = %6.2f	COJ_5b = %6.2f\n", COJ[5], 0.0);
    COJ[6]  = COJ[4] - COJ[5];
    COJ_b[6] = COJ[4] - 0.0;
    fprintf(outfile," COJ_6a = %6.2f	COJ_6b = %6.2f\n", COJ[6], COJ_b[6]);
    COJ[7]  = TaxRateFunction( COJ[6], status );
    COJ_b[7] = TaxRateFunction( COJ_b[6], status );
    fprintf(outfile," COJ_7a = %6.2f	COJ_7b = %6.2f\n", COJ[7], COJ_b[7] );
    COJ[8]  = COJ[3] * COJ[7];
    COJ_b[8] = COJ[3] * COJ_b[7];
    fprintf(outfile," COJ_8a = %6.2f	COJ_8b = %6.2f\n", COJ[8], COJ_b[8] );
    fprintf(outfile,"  (9a = %6.2f)\n", COJ_9a );
    COJ[9] = smallerof( smallerof( COJ_9a, COJ[8] ), COJ[7] );
    COJ_b[9] = smallerof( smallerof( COJ_9a, COJ_b[8] ), COJ_b[7] );
    fprintf(outfile," COJ_9a = %6.2f	COJ_9b = %6.2f\n", COJ[9], COJ_b[9] );

    fprintf(outfile,"\nWorksheet I:\n");
    I[1] = COJ[7];	Ib[1] = COJ_b[7];
    fprintf(outfile," I1a = %6.2f	I1b = %6.2f\n", I[1], Ib[1] );
    I[2] = COJ[9];	Ib[2] = COJ_b[9];
    fprintf(outfile," I2a = %6.2f	I2b = %6.2f\n", I[2], Ib[2] );

    I[3]  = I[1] - I[2];
    Ib[3] = Ib[1] - Ib[2];
    fprintf(outfile," I3a = %6.2f	I3b = %6.2f\n", I[3], Ib[3] );

    Ib[4] = Ib[3] - I[3];
    showline_wrksht('I', 4, Ib);

    if (eligible_proptax_ded)
     { /*eligible*/
      if (Ib[4] >= proptxcredit)
       {
        fprintf(outfile," Sched-I, Yes:  Take PropTax Deduction\n\n");
        L[41] = COJ[5];	// fprintf(outfile,"L36c = %6.2f\n", L[36]);
        L[42] = COJ[6];
        L[43] = COJ[7];
        L[44] = I[2];
        L[57] = 0.0;
       }
      else
       {
        fprintf(outfile," Sched-I, No:  Take PropTax Credit\n\n");
        L[41] = 0.0;
        L[42] = COJ_b[6];
        L[43] = COJ_b[7];
        L[44] = Ib[2];
        L[57] = proptxcredit;
       }
     } /*eligible*/
    else
     { /*not_eligible*/
      L[41] = 0.0;
      L[42] = COJ_b[6];
      L[43] = COJ_b[7];
      L[44] = Ib[2];
      L[57] = 0.0;
     } /*not_eligible*/
  } /*SchedA+Worksheet-I*/

 if (L[40] > 0.0)
  fprintf(outfile, "L40a = %6.2f\n", L[40]);

 showline_wmsg( 41, "Property Tax Deduction" );

 fprintf(outfile,"\n");  /* NJ Taxable Income.*/
 // L[42] = L[39] - L[41];  /* Handled above in Sched-1. */
 if (L[42] > 0.0)
  showline_wmsg( 42, "NJ Taxable Income" );

 // L[43] = TaxRateFunction( L[42], status );  /* Handled above in Schedules+Worksheets, A, G, H, I. */
 if ((L[29] < filing_threshold) || (L[43] < 0.0))
  L[43] = 0.0;
 showline_wmsg(43, "TAX");
 Report_bracket_info( L[42], status );

 if (COJ[1] > 0.0)
  showline_wmsg( 44, "Credit for Taxes paid to other jurisdictions." );

 L[45] = L[43] - L[44];
 showline_wmsg( 45, "Balance of Tax");

 GetLineF( "L46", &L[46] );	/* Sheltered Workshop Tax Credit. */
 GetLineF( "L47", &L[47] );	/* Gold Star Family Counseling Credit. */
 GetLineF( "L48", &L[48] );	/* Credit for Employer of Organ/Bone Marrow Donor */

 L[49] = L[46] + L[47] + L[48];
 showline_wmsg( 49, "Total Credits." );

 L[50] = NotLessThanZero( L[45] - L[49] );
 showline_wmsg( 50, "Balance of Tax after Credits." );

 GetLineF( "L51", &L[51] );	/* Use-Tax Due on Internet, Mail-Order or other out-of-state purchaes. */
 GetLineF( "L52", &L[52] );	/* Interest on underpayment of estimated tax. */
 GetLineF( "L53", &L[53] );	/* Shared Responsibility (Med. Insurance) Payment. */

 L[54] = L[50] + L[51] + L[52] + L[53];
 showline_wmsg( 54, "Total Tax Due" );			/* Total Tax + Penalty. */

 GetLine( "L55", &L[55] );	/* Withheld amount. */
 showline_wmsg( 55, "Total NJ Income Tax Withheld" );

 showline_wmsg( 56, "Property tax Credit" );

 GetLineF( "L57", &L[57] );	/* NJ Estimated Tax Payments/Credit from last year's return. */
 GetLineF( "L58", &L[58] );	/* NJ Earned Income Tax Credit. (See Sched pg 38.) */
 GetLineF( "L59", &L[59] );	/* EXCESS NJ UI/HC/WD Withheld, (See pg 38.) */
 GetLineF( "L60", &L[60] );	/* EXCESS NJ Disability Insurance Withheld, (See pg 38.) */
 GetLineF( "L61", &L[61] );	/* EXCESS NJ Family Leave Insurance Withheld, (See pg 38.) */
 GetLineF( "L62", &L[62] );	/* Wounded Warrior Caregivers Credit */
 GetLineF( "L63", &L[63] );	/* Pass-Through Business Alternatve Income Tax Credit */
 GetLineF( "L64", &L[64] );	/* Child and Dependent Care Credit. */
 GetLineF( "L65", &L[65] );	/* NJ Child Tax Credit. */

 for (j=55; j <= 65; j++)
  L[66] = L[66] + L[j];
 showline_wmsg( 66, "Total Withholding Payments & Credits" );

 for (j=69; j <= 77; j++)
  L[78] = L[78] + L[j];
 
 if (L[66] < L[54])
  {
   L[67] = L[54] - L[66];
   fprintf(outfile, "L67 = %6.2f	DUE !!!\n", L[67] );
   fprintf(outfile,"         (Which is %2.1f%% of your total tax.)\n", 100.0 * L[67] / (L[54] + 1e-9) );
   showline_wmsg( 78, "( Total Adjustments to tax due )");
   L[79] = L[67] + L[78];
   showline_wmsg( 79, "Balance Due" );
  }
 else
  {
   L[68] = L[66] - L[54];
   fprintf(outfile, "L68 = %6.2f	Overpayment\n", L[68] );

   showline_wmsg( 78, "( Total Adjustments to overpayment )");
   L[80] = L[68] - L[78];
   showline_wmsg( 80, "Refund !!!" );
  }
 
 fprintf(outfile,"\n{ --------- }\n");
 Your1stName    = GetTextLineF( "Your1stName:" );
 YourInitial    = GetTextLineF( "YourInitial:" );
 YourLastName   = GetTextLineF( "YourLastName:" );
 GetTextLineF( "YourSocSec#:" );
 Spouse1stName  = GetTextLineF( "Spouse1stName:" );
 SpouseInitial  = GetTextLineF( "SpouseInitial:" );
 SpouseLastName = GetTextLineF( "SpouseLastName:" );
 GetTextLineF( "SpouseSocSec#:" );
 if (strlen( YourLastName ) > 0)
  {
   strcpy( YourNames, YourLastName );
   strcat( YourNames, ", " );
   strcat( YourNames, Your1stName );
   if (YourInitial[0] != '\0')
    {
     strcat( YourNames, ", " );
     strcat( YourNames, YourInitial );
    }
   if (Spouse1stName[0] != '\0')
    {
     strcat( YourNames, ", " );
     if ((SpouseLastName[0] != '\0') && (strcmp( YourLastName, SpouseLastName ) != 0))
      {
       strcat( YourNames, SpouseLastName );
       strcat( YourNames, ", " );
      }
     strcat( YourNames, Spouse1stName );
     if (SpouseInitial[0] != '\0')
      {
       strcat( YourNames, ", " );
       strcat( YourNames, SpouseInitial );
      }
    }
   fprintf(outfile,"YourNames: %s\n", YourNames );
  }
 GetTextLineF( "Number&Street:" );
 GetTextLineF( "Town:" );
 GetTextLineF( "State:" );
 GetTextLineF( "Zipcode:" );

 fclose(infile);
 grab_any_pdf_markups( infname, outfile );
 fclose(outfile);
 Display_File( outfname );
 printf("\nResults written to file:  %s\n", outfname);
 return 0;
}

#undef SINGLE
#undef MARRIED_FILING_JOINTLY
#undef MARRIED_FILING_SEPARAT
#undef HEAD_OF_HOUSEHOLD
#undef WIDOW
}
namespace taxsolve_f8959_2022 {
#define SINGLE 		        1
#define MARRIED_FILING_JOINTLY  2
#define MARRIED_FILING_SEPARAT  3
#define HEAD_OF_HOUSEHOLD       4
#define WIDOW		        5
#define Yes 1
#define No  0
/************************************************************************/
/* TaxSolve_Form_8959.c - 2022						*/
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

float thisversion=3.00;



int status;

/*----------------------------------------------------------------------------*/

int main( int argc, char *argv[] )
{
 int i, j, k;
 char word[6000], outfname[6000], *infname=0;
 time_t now;
/*
 double L[25];
*/
 printf("Form 8959, 2022 - v%3.2f\n", thisversion);

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
 check_form_version( word, "Title: 2022 Form 8959" );

 /* ----- Accept form data and process the numbers.         ------ */
 /* ----- Place all your form-specific code below here .... ------ */

 // fprintf(outfile,"\n--- THIS IS PRELIMINARY USER-CONTRIBUTED FORM ---\n");
 // fprintf(outfile,"--- NOT YET FULLY UPDATED FOR 2022. ---\n\n");

 // MarkupPDF( 1, 240, 40, 17, 1.0, 0, 0 ) NotReady "This program is NOT updated for 2022."
 // add_pdf_markup( "NotReady", 1, 240, 40, 17, 1, 1.0, 0, 0, "\"This program is NOT ready for 2022.\"" );


 // Example:
 //  GetLineF( "L2", &L[2] );
 //  GetLineF( "L3", &L[3] );
 //  L[4] = L[2] - L[3];
 //  showline_wlabel( "L4", L[4] );

 GetTextLineF( "YourName:" );
 GetTextLineF( "YourSocSec#:" );

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
 L[4] = L[1] + L[2] + L[3];
 showline( 4 );
 if(status == MARRIED_FILING_JOINTLY)
	L[5] = 250000.00;
 else if(status == MARRIED_FILING_SEPARAT)
	L[5] = 125000.00;
 else if((status == SINGLE) || (status == HEAD_OF_HOUSEHOLD) || (status == WIDOW))
	L[5] = 200000.00;
 showline( 5 );
 L[6] = NotLessThanZero( L[4] - L[5]);
 showline( 6 );
 L[7] = L[6] * 0.009;
 showline( 7 );
 GetLineF( "L8", &L[8] );
 if(status == MARRIED_FILING_JOINTLY)
	L[9] = 250000.00;
 else if(status == MARRIED_FILING_SEPARAT)
	L[9] = 125000.00;
 else if((status == SINGLE) || (status == HEAD_OF_HOUSEHOLD) || (status == WIDOW))
	L[9] = 200000.00;
 showline( 9 );
 L[10] = L[4];
 showline( 10 );
 L[11] = NotLessThanZero( L[9] - L[10]);
 showline( 11 );
 L[12] = NotLessThanZero( L[8] - L[11]);
 showline( 12 );
 L[13] = L[12] * 0.009;
 showline( 13 );
 GetLineF( "L14", &L[14] );
 if(status == MARRIED_FILING_JOINTLY)
	L[15] = 250000.00;
 else if(status == MARRIED_FILING_SEPARAT)
	L[15] = 125000.00;
 else if((status == SINGLE) || (status == HEAD_OF_HOUSEHOLD) || (status == WIDOW))
	L[15] = 200000.00;
 showline( 15 );
 L[16] = NotLessThanZero( L[14] - L[15]);
 showline( 16 );
 L[17] = L[16] * 0.009;
 showline( 17 );
 L[18] = L[7] + L[13] + L[17];
 showline_wmsg( 18, "include this amount on Schedule 2 (Form 1040), line 11 \
(Form 1040-PR or 1040-SS filers, see instructions)" );
 GetLineF( "L19", &L[19] );
 L[20] = L[1];
 showline( 20 );
 L[21] = L[20] * 0.0145;
 showline_wmsg( 21, "This is your regular Medicare tax \
withholding on Medicare wages" );
 L[22] = NotLessThanZero( L[19] - L[21]);
 showline_wmsg( 22, "This is your Additional Medicare Tax \
withholding on Medicare wages" );
 GetLineF( "L23", &L[23] );
 L[24] = L[22] + L[23];
 showline_wmsg( 24, "include this amount with \
federal income tax withholding on Form 1040, 1040-SR, or 1040-NR, line 25c (Form 1040-PR or \
1040-SS filers, see instructions)" );  

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
#undef Yes
#undef No
}
namespace taxsolve_f8960_2022 {
#define SINGLE 		        1
#define MARRIED_FILING_JOINTLY  2
#define MARRIED_FILING_SEPARAT  3
#define HEAD_OF_HOUSEHOLD       4
#define WIDOW		        5
#define Yes 1
#define No  0
/************************************************************************/
/* TaxSolve_Form_8960.c - 2022						*/
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

float thisversion=3.00;



/*----------------------------------------------------------------------------*/

int main( int argc, char *argv[] )
{
 int i, j, k;
 char word[6000], outfname[4000], *infname=0;
 time_t now;

 double L4a = 0.0,  L4b = 0.0, L4c = 0.0;
 double L5a = 0.0, L5b = 0.0, L5c = 0.0, L5d = 0.0;
 double L9a = 0.0, L9b = 0.0, L9c = 0.0, L9d = 0.0;
 double L18a = 0.0,  L18b = 0.0, L18c = 0.0;
 double L19a = 0.0,  L19b = 0.0, L19c = 0.0;

 int status, individual = No;

 printf("Form 8960, 2022 - v%3.2f\n", thisversion);

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
 check_form_version( word, "Title: 2022 Form 8960" );


 /* ----- Accept form data and process the numbers.         ------ */
 /* ----- Place all your form-specific code below here .... ------ */

 // fprintf(outfile,"\n--- THIS IS PRELIMINARY USER-CONTRIBUTED FORM ---\n");
 // fprintf(outfile,"--- NOT YET FULLY UPDATED FOR 2022. ---\n\n");
 // MarkupPDF( 1, 240, 40, 17, 1.0, 0, 0 ) NotReady "This program is NOT updated for 2022."
 // add_pdf_markup( "NotReady", 1, 240, 40, 17, 1, 1.0, 0, 0, "\"This program is NOT ready for 2022.\"" );


 // Example:
 //  GetLineF( "L2", &L[2] );
 //  GetLineF( "L3", &L[3] );
 //  L[4] = L[2] - L[3];
 //  showline_wlabel( "L4", L[4] );

 GetTextLineF( "YourName:" );
 GetTextLineF( "YourSocSec#:" );

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

 get_parameter( infile, 's', word, "Entity" );
 get_parameter( infile, 'l', word, "Entity?");
 if (strncasecmp(word,"Individual",3)==0) individual = Yes;
 fprintf(outfile,"Entity = %s (%d)\n", word, individual);
     
 get_parameter( infile, 's', word, "Sec6013g" );
 get_parameter( infile, 'l', word, "Sec6013g?");
 if (strncasecmp(word,"Yes",1)==0)
   fprintf(outfile, "CkSec6013g X\n");
  
 get_parameter( infile, 's', word, "Sec6013h" );
 get_parameter( infile, 'l', word, "Sec6013h?");
 if (strncasecmp(word,"Yes",1)==0)
   fprintf(outfile, "CkSec6013h X\n");
  
 get_parameter( infile, 's', word, "Sec1141_10g" );
 get_parameter( infile, 'l', word, "Sec1141_10g?");
 if (strncasecmp(word,"Yes",1)==0)
   fprintf(outfile, "CkSec1141_10g X\n");  

 GetLineF( "L1", &L[1] );
 GetLineF( "L2", &L[2] );
 GetLineF( "L3", &L[3] );
 GetLineF( "L4a", &L4a );
 GetLineF( "L4b", &L4b );
 L4c = L4a + L4b;
 showline_wlabel( "L4c", L4c );
 GetLineF( "L5a", &L5a );
 GetLineF( "L5b", &L5b );
 GetLineF( "L5c", &L5c );
 L5d = L5a + L5b + L5c;
 showline_wlabel( "L5d", L5d );
 GetLineF( "L6", &L[6] );
 GetLineF( "L7", &L[7] );
 L[8] = L[1] + L[2] + L[3] + L4c + L5d + L[6] + L[7];
 showline( 8 );
 GetLineF( "L9a", &L9a );
 GetLineF( "L9b", &L9b );
 GetLineF( "L9c", &L9c );
 L9d = L9a + L9b + L9c;
 showline_wlabel( "L9d", L9d );
 GetLineF( "L10", &L[10] );
 L[11] = L9d + L[10];
 showline( 11 );
 L[12] = NotLessThanZero(L[8] - L[11]);
 showline( 12 );

 if(individual == Yes){
   GetLineF( "L13", &L[13] );
   if( status == MARRIED_FILING_JOINTLY)
	  L[14] = 250000.00;
   else if(status == WIDOW)
	  L[14] = 250000.00;
   else if(status == MARRIED_FILING_SEPARAT)
	  L[14] = 125000.00;
   else if((status == SINGLE) || (status == HEAD_OF_HOUSEHOLD))
	  L[14] = 200000.00;
   showline( 14 );
   L[15] = NotLessThanZero(L[13] - L[14]);
   showline( 15 );
   L[16] = SmallerOf(L[12], L[15]);
   showline( 16 );
   L[17] = L[16] * 0.038;
   showline_wmsg( 17, "Include on your tax return see instructions)" );
 }
 else{
   GetLine( "L13", &L[13] );	/* consume unneeded line from input file */
   L18a = L[12];
   showline_wlabel( "L18a", L18a );
   GetLineF( "L18b", &L18b );
   L18c = NotLessThanZero( L18a - L18b );
   showline_wlabel( "L18c", L18c );
   GetLineF( "L19a", &L19a );
   GetLineF( "L19b", &L19b );
   L19c = NotLessThanZero( L19a - L19b );
   showline_wlabel( "L19c", L19c );
   L[20] = SmallerOf( L18c, L19c );
   L[21] = L[20] * 0.038;
   showline_wmsg( 21, "Include on your tax return see instructions)" );
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
#undef Yes
#undef No
}
namespace taxsolve_f8606 {
#define Yes 1
#define No 0
/************************************************************************/
/* TaxSolve_Form_8606.c -                                               */
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
/* 02111-1307 USA                                                       */
/*									*/
/* Provided by Fred Robinson - 2022-7-8.				*/	
/************************************************************************/

float thisversion = 2.00;



/*----------------------------------------------------------------------------*/

int main(int argc, char *argv[])
{
  int i, j, k;
  int complete_part_one, complete_part_two, complete_part_three, dist_or_conv;
  char word[4000], outfname[4000], *infname = 0;
  time_t now;
  double L15a = 0.0, L15b = 0.0, L15c = 0.0;
  double L25a = 0.0, L25b = 0.0, L25c = 0.0;

  printf("Form 8606, 2022 - v%3.2f\n", thisversion);

  /* Decode any command-line arguments. */
  i = 1;
  k = 1;
  while (i < argc) {
    if (strcmp(argv[i], "-verbose") == 0) {
      verbose = 1;
    } else if (k == 1) {
      infname = strdup(argv[i]);
      infile = fopen(infname, "r");
      if (infile == 0) {
        printf("ERROR: Parameter file '%s' could not be opened.\n", infname);
        exit(1);
      }
      k = 2;
      /* Base name of output file on input file. */
      strcpy(outfname, infname);
      j = strlen(outfname) - 1;
      while ((j >= 0) && (outfname[j] != '.'))
        j--;
      if (j < 0)
        strcat(outfname, "_out.txt");
      else
        strcpy(&(outfname[j]), "_out.txt");
      outfile = fopen(outfname, "w");
      if (outfile == 0) {
        printf("ERROR: Output file '%s' could not be opened.\n", outfname);
        exit(1);
      }
      printf("Writing results to file:  %s\n", outfname);
    } else {
      printf("Unknown command-line parameter '%s'\n", argv[i]);
      exit(1);
    }
    i = i + 1;
  }
  if (infile == 0) {
    printf("Error: No input file on command line.\n");
    exit(1);
  }

  /* Pre-initialize all lines to zeros. */
  for (i = 0; i < MAX_LINES; i++) {
    L[i] = 0.0;
  }

  /* Accept parameters from input file. */
  /* Expect lines, something like:
         Title:  Form XXXX Return
         L2              {Returns and Allowances}
         . . .
  */

  /* Accept Form's "Title" line, and put out with date-stamp for your records.
   */
  read_line(infile, word);
  now = time(0);
  fprintf(outfile, "\n%s,  v%2.2f, %s\n", word, thisversion, ctime(&now));
  check_form_version( word, "Title: Form 8606 for tax-year 2022" );

  // Begin form-specific code

  // Part I

  /* Are you completing Part I of this form? (Y/N) */
  get_parameter(infile, 's', word, "Complete_Part_One?");
  get_param_single_line(infile, 'b', &complete_part_one, "Complete_Part_One?");

  GetLine("L1", &L[1]);
  GetLine("L2", &L[2]);

  /* Did you take a distribution or make a conversion? (Y/N) */
  get_parameter(infile, 's', word, "Dist_or_Conv?");
  get_param_single_line(infile, 'b', &dist_or_conv, "Dist_or_Conv?");

  GetLine("L4", &L[4]);
  GetLine("L6", &L[6]);
  GetLine("L7", &L[7]);
  GetLine("L8", &L[8]);
  GetLine("L15b", &L15b);

  // All inputs are read, now produce output for Part I
  if (complete_part_one) {
    showline(1);
    showline(2);
    L[3] = L[1] + L[2];
    showline(3);

    if (!dist_or_conv) {
      L[14] = L[3];
      showline(14);
    } else {

      showline(4);
      L[5] = L[3] - L[4];
      showline(5);
      showline(6);
      showline(7);
      showline(8);
      L[9] = L[6] + L[7] + L[8];
      showline(9);

      L[10] = L[5] / L[9];
      if (L[10] > 1.00) {
        L[10] = 1.0;
      }

      double integral;
      double fractional = modf(L[10], &integral);
      int integral_int, fractional_int;
      integral_int = integral;
      fractional_int = abs( (int)(1000.0 * fractional) );
      fprintf(outfile, "L10intpart %d\n", integral_int);
      fprintf(outfile, "L10rest %03d\n", fractional_int);

      L[11] = L[8] * L[10];
      showline(11);

      L[12] = L[7] * L[10];
      showline(12);

      L[13] = L[11] + L[12];
      showline(13);

      L[14] = L[3] - L[13];
      showline(14);

      L15a = L[7] - L[12];
      showline_wlabel("L15a", L15a);

      showline_wlabel("L15b", L15b);

      L15c = L15a - L15b;
      if (L15c > 0.0) {
        showline_wlabelmsg(
            "L15c", L15c,
            "Taxable Amount: include this amount on 2022 Form 1040 or 1040-SR, "
            "line 4b; or 2021 Form 1040-NR, line 16b");
      } else {
        showline_wlabel("L15c", L15c);
      }
    }
  }

  // Part II

  /* Are you completing Part II of this form? (Y/N) */
  get_parameter(infile, 's', word, "Complete_Part_Two?");
  get_param_single_line(infile, 'b', &complete_part_two, "Complete_Part_Two?");

  GetLine("L16", &L[16]);
  GetLine("L17", &L[17]);

  if (complete_part_two) {
    if (complete_part_one && dist_or_conv) {
      L[16] = L[8];
      L[17] = L[11];
    }

    showline(16);
    showline(17);
    L[18] = L[16] - L[17];
    showline(18);
  }

  // Part III

  /* Are you completing Part III of this form? (Y/N) */
  get_parameter(infile, 's', word, "Complete_Part_Three?");
  get_param_single_line(infile, 'b', &complete_part_three, "Complete_Part_Three?");
  GetLine("L19", &L[19]);
  GetLine("L20", &L[20]);
  GetLine("L22", &L[22]);
  GetLine("L24", &L[24]);
  GetLine("L25b", &L25b);

  if (complete_part_three) {
    showline(19);
    showline(20);

    L[21] = NotLessThanZero(L[19] - L[20]);
    showline(21);

    showline(22);

    if (L[21] > 0.0) {
      L[23] = NotLessThanZero(L[21] - L[22]);

      if (L[23] == 0) {
        showline(23);
      } else {
        showline_wmsg(
            23, "you may be subject to an additional tax (see instructions)");
      }
    }

    if (L[21] > 0 && L[23] > 0) {
      showline(24);

      L25a = NotLessThanZero(L[23] - L[24]);
      showline_wlabel("L25a", L25a);

      if (L25a > 0) {
        showline_wlabelmsg("L25b", L25b,
                           "Also, enter this amount on 2022 Form 8915-C, line "
                           "23, or 2022 Form 8915-D, line 14, as applicable");

        L25c = L25a - L25b;
        if (L25c > 0) {
          showline_wlabelmsg(
              "L25c", L25c,
              "Also include this amount on 2022 Form 1040 or 1040-SR, line 4b; "
              "or 2022 Form 1040-NR, line 16b.");

        } else {
          showline_wlabel("L25c", L25c);
        }
      }
    }
  }

  fprintf(outfile, "------------------------------\n");
  fprintf(outfile, "\n{ --------- Identity-Information:  --------- }\n");
  GetTextLineF("Name:");
  GetTextLineF("SocSec#:");
  GetTextLineF("Number&Street:");
  GetTextLineF("Apt#:");
  GetTextLineF("TownStateZip:");
  GetTextLineF("ForeignCountry:");
  GetTextLineF("ForeignState:");
  GetTextLineF("ForeignPostcode:");

  // End form-specific code

  fclose(infile);
  grab_any_pdf_markups(infname, outfile);
  fclose(outfile);

  printf("\nListing results from file: %s\n\n", outfname);
  Display_File(outfname);

  return 0;
}

#undef Yes
#undef No
}
namespace taxsolve_CA_540_2022 {
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
}
namespace taxsolve_CA_5805_2022 {
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
}
namespace taxsolve_NC_D400_2022 {
#define SINGLE                  1
#define MARRIED_FILING_JOINTLY  2
#define MARRIED_FILING_SEPARAT  3
#define HEAD_OF_HOUSEHOLD       4
#define WIDOW                   5
#define Yes 1
#define No  0
/************************************************************************/
/* TaxSolve_NC_D400_2022.c - North Carolina 2022 NC-DC400 State Taxes.	*/
/* Copyright (C) 2022 - S.Jenkins					*/
/* 									*/
/* Compile:   gcc taxsolve_NC_D400_2022.c -o taxsolve_NC_D400_2022	*/
/* Run:  ./taxsolve_NC_D400_2022		  			*/
/*  Uses log from TaxSolve Federal 1040.				*/
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
/* 1-2-2022	http://opentaxsolver.sourceforge.com/			*/
/*	Earlier versions - Lincoln Baxter (lab@lincolnbaxter.com)	*/
/*									*/
/************************************************************************/



float thisversion=20.00;


double flat_tax_rate = 0.0499;		/* Updated for 2022. */


struct FedReturnData
 {
  double fedline[MAX_LINES];
  int Itemized, Limited, Limited_L6;
  double Sched_A[MAX_LINES];
 };


void convert_slashes( char *fname )
{ /* Convert slashes in file name based on OS type. */
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


void ImportFederalReturnData( char *fedlogfile, struct FedReturnData *fed_data )
{
 FILE *infile;
 char fline[1000], word[1000];
 int linenum;

 convert_slashes( fedlogfile );
 infile = fopen(fedlogfile, "r");
 if (infile==0)
  {
   printf("Error: Could not open federal return '%s'\n", fedlogfile);
   fprintf(outfile,"\nError: Could not open federal return '%s'\n", fedlogfile);
   #ifdef __MINGW32__
    system("dir");
   #else
    system("pwd");
    system("ls -l");
   #endif
   exit(1);
  }
 printf(" Reading file: %s\n", fedlogfile );

 /* Set initial default values. */
 fed_data->Itemized = 1; 
 fed_data->Limited = 1; 
 fed_data->Limited_L6 = 1; 
 for (linenum=0; linenum<MAX_LINES; linenum++) fed_data->fedline[linenum] = 0.0;
 for (linenum=0; linenum<MAX_LINES; linenum++) fed_data->Sched_A[linenum] = 0.0;

 read_line(infile,fline);  linenum = 0;
 while (!feof(infile))
  {
   if ( verbose ) printf( "Read Line: %s" ,fline );
   if (strstr(fline,"Use standard deduction.")!=0) fed_data->Itemized = 0;
   if (strstr(fline,"Deductions not limited")!=0) 
    {
     fed_data->Limited = 0;
     if ( strstr(fline,"line 6") ) fed_data->Limited_L6 = 0;
    }
   next_word(fline, word, " \t=");
   if ((strstr(word,"A")==word) && (isdigit(word[1])) && (strstr(fline," = ")!=0))
    {
     if (sscanf(&word[1],"%d",&linenum)!=1) printf("Error: Reading fed sched-A line number '%s%s'\n",word,fline);
     next_word(fline, word, " 	=");
     if (sscanf(word,"%lf", &fed_data->Sched_A[linenum])!=1) printf("Error: Reading fed sched-A line %d '%s%s'\n",linenum,word,fline);
     if (verbose) printf("Sched_A[%d] = %2.2f\n", linenum, fed_data->Sched_A[linenum]);
    }
   if ((strstr(word,"L")==word) && (strstr(fline," = ")!=0))
    {
     if (sscanf(&word[1],"%d",&linenum)!=1) printf("Error: Reading fed line number '%s%s'\n",word,fline);
     next_word(fline, word, " 	=");
     if (sscanf(word,"%lf", &fed_data->fedline[linenum])!=1) printf("Error: Reading fed line %d '%s%s'\n",linenum,word,fline);
     if (verbose) printf("FedLin[%d] = %2.2f\n", linenum, fed_data->fedline[linenum]);
    }
   read_line(infile,fline);
  }
 fclose(infile);
}


/* ------------------------------------------------------------------------------------ */
/* ---				Main						    --- */
/* ------------------------------------------------------------------------------------ */
int main( int argc, char *argv[] )
{
 int j, jj, k, status;
 char word[1000], *infname=0, outfname[1000], *socsec, socsectmp[100];
 time_t now;
 struct FedReturnData fed_data;
 double stdded, min_payment=0.0, min2file, ChildDeduction=0.0;
 int L10a=0;
 double L12a=0.0, L20a=0.0, L20b=0.0, L21a=0.0, L21b=0.0, L21c=0.0, L21d=0.0;

 /*-----------------------------------------*/
 /* --- Decode any command line options. -- */
 /*-----------------------------------------*/
 printf("NC D400 2022 - v%3.2f\n", thisversion);
 jj = 1;  k=1;
 while (jj < argc)
 {
  if (strcmp(argv[jj],"-verbose")==0)  { verbose = 1; }
  else
  if (strcmp(argv[jj],"-round_to_whole_dollars")==0)  { round_to_whole_dollars = 1; }
  else
  if (k==1)
   {
    infname = strdup(argv[jj]);
    infile = fopen(argv[jj],"r");
    if (infile==0)
     {
	printf("ERROR: Parameter file '%s' could not be opened.\n", argv[jj]); 
	fprintf(outfile,"ERROR: Parameter file '%s' could not be opened.\n", argv[jj]); 
	exit(1);
     }
    k = 2;
    /* Base name of output file on input file. */
    strcpy(outfname,argv[jj]);
    j = strlen(outfname)-1;
    while ((j>=0) && (outfname[j]!='.')) j--;
    if (j<0) strcat(outfname,"_out.txt"); else strcpy(&(outfname[j]),"_out.txt");
    outfile = fopen(outfname,"w");
    if (outfile==0)
     {
	printf("ERROR: Output file '%s' could not be opened.\n", outfname); 
	fprintf(outfile,"ERROR: Output file '%s' could not be opened.\n", outfname); 
	exit(1);
     }
    printf("Writing results to file:  %s\n", outfname);
   }
  else
   {
	printf("Unknown command-line parameter '%s'\n", argv[jj]); 
	fprintf(outfile,"Unknown command-line parameter '%s'\n", argv[jj]); 
	exit(1);
   }
  jj++;
 }

 if (infile==0)
  {
	printf("Error: No input file on command line.\n"); 
	fprintf(outfile,"Error: No input file on command line.\n"); 
	exit(1);
  }
 

 /*--------------------------*/
 /* ---- Get Input Data ---- */
 /*--------------------------*/

 /* Pre-initialize all lines to zeros. */
 for (j = 0; j < MAX_LINES; j++) { L[j] = 0.0; }

 /* Accept parameters from input file. */
 /* Expect  D-400 lines, something like:
	Title:  NC-400 1999 Return
	L6	34900.0  {Wages}
 */

 /* Accept Form's "Title" line, and put out with date-stamp for your records. */
 read_line( infile, word );
 now = time(0);
 fprintf(outfile,"\n%s,	 v%2.2f, %s\n", word, thisversion, ctime( &now ));
 check_form_version( word, "Title:  NC State Tax Form 400 for 2022" );

 /* get_parameter(infile, kind, x, emssg ) */
 get_parameter( infile, 's', word, "FedReturn" );	/* File name of Federal Return log file. */
 if ( verbose ) printf( "word: %s\n", word );
 get_word( infile, word ); 
 ImportFederalReturnData( word, &fed_data);

 get_parameter( infile, 's', word, "Status"); /* 1=single, 2=married/joint, 3=married/separate, 4=house-head, 5=widow */
 get_parameter( infile, 'l', word, "Status ?");
 if ((word[0]>'0') && (word[0]<'6')) status = word[0]-48; else
 if (strncasecmp(word,"Single",4)==0) status = SINGLE; else
 if (strncasecmp(word,"Married/Joint",13)==0) status = MARRIED_FILING_JOINTLY; else
 if (strncasecmp(word,"Married/Sep",11)==0) status = MARRIED_FILING_SEPARAT; else
 if (strncasecmp(word,"Head_of_House",4)==0) status = HEAD_OF_HOUSEHOLD; else
 if (strncasecmp(word,"Widow",4)==0) status = WIDOW;
 else
  { 
   printf("Error: unrecognized status '%s'. Must be: Single, Married/joint, Married/sep, Head_of_house, Widow(er)\nExiting.\n", word); 
   fprintf(outfile,"Error: unrecognized status '%s'. Must be: Single, Married/joint, Married/sep, Head_of_house, Widow(er)\nExiting.\n", word); 
   exit(1); 
  }
 fprintf(outfile,"Status = %s (%d)\n", word, status);

 GetLine( "L7", &L[7] );	/* Additions to Fed AGI - From Sched-S Part A, Line 6. */  
 GetLine( "L9", &L[9] );	/* Deductions from Fed AGI - From Sched-S Part B, Line 14. */  

 GetInteger( "L10a", &L10a );	/* Number of Dependents. */

 GetLine( "L11", &L[11] );	/* Enter 0 to use Std Deduction; Otherwise Itemized Ded. from Sched-S part C, Line 23. */
 GetLine( "L13", &L[13] );	/* Enter 1.0 for full-year residents. Otherwise amount from Sched-S part D, Line 26. */  
 GetLine( "L16", &L[16] );	/* Tax credits. (D-400TC part 3 line 20) */  
 GetLine( "L18", &L[18] );	/* Consumer Use Tax. (pgs 9+10) */
 GetLine( "L20a", &L20a );	/* North Carolina Income Tax Withheld (yours) */
 GetLine( "L20b", &L20b );	/* North Carolina Income Tax Withheld (spouses) */
 L[20] = L20a + L20b;
 GetLine( "L21a", &L21a );	/* Other tax payments. 2022 Estimated Tax. */
 GetLine( "L21b", &L21b );	/* Other tax payments. Paid with Extension. */
 GetLine( "L21c", &L21c );	/* Other tax payments. Partnership. */
 GetLine( "L21d", &L21d );	/* Other tax payments. S Corporation. */



 /*-------------------------------*/
 /* ---- Do Tax Calculations ---- */
 /*-------------------------------*/

 L[6] = fed_data.fedline[11];		/* Taxable income from Fed1040 Line 11, AGI. */
 L[6] = Conditional_Round( L[6] );

 switch (status)
  {				/* Updated for 2022. */
   case SINGLE: 		 stdded   = 12750.0; 	/* NC std single deduction. */
				 min2file = 12750.0;
	break;
   case MARRIED_FILING_JOINTLY:  stdded   = 25500.0; 	/* NC std Married/joint deduction. */
				 min2file = 25500.0;
	break;
   case WIDOW:			 stdded   = 25500.0; 	/* NC std widow(er) deduction. */
				 min2file = 25500.0;
	break;
   case MARRIED_FILING_SEPARAT:  stdded   = 12750.0; 	/* NC std Married/sep deduction. */
				 min2file = 12750.0;
	break;
   case HEAD_OF_HOUSEHOLD:	 stdded   = 19125.0; 	/* NC std Head of house deduction. */
				 min2file = 19125.0;
	break;
   default:  
	stdded = 0;  printf("Unknown status\n");  fprintf(outfile,"Unknown status\n");
	exit(1); 
  }

 if (L[6] <= min2file)
  fprintf(outfile, "You may not need to file NC tax return, due to your income.\n");

 L[8] = L[6] + L[7];

 switch (status)
  {					/* Updated for 2022. */
   case MARRIED_FILING_JOINTLY: 
   case WIDOW: 
		if (L[6] <= 40000.0)	ChildDeduction = 3000.0;	else
		if (L[6] <= 60000.0)	ChildDeduction = 2500.0;	else
		if (L[6] <= 80000.0)	ChildDeduction = 2000.0;	else
		if (L[6] <= 100000.0)	ChildDeduction = 1500.0;	else
		if (L[6] <= 120000.0)	ChildDeduction = 1000.0;	else
		if (L[6] <= 140000.0)	ChildDeduction =  500.0;	else
		ChildDeduction = 0.0;
   	break;
   case HEAD_OF_HOUSEHOLD:
		if (L[6] <= 30000.0)	ChildDeduction = 3000.0;	else
		if (L[6] <= 45000.0)	ChildDeduction = 2500.0;	else
		if (L[6] <= 60000.0)	ChildDeduction = 2000.0;	else
		if (L[6] <= 75000.0)	ChildDeduction = 1500.0;	else
		if (L[6] <= 90000.0)	ChildDeduction = 1000.0;	else
		if (L[6] <= 105000.0)	ChildDeduction =  500.0;	else
		ChildDeduction = 0.0;
   	break;
   case SINGLE:	case MARRIED_FILING_SEPARAT:
		if (L[6] <= 20000.0)	ChildDeduction = 3000.0;	else
		if (L[6] <= 30000.0)	ChildDeduction = 2500.0;	else
		if (L[6] <= 40000.0)	ChildDeduction = 2000.0;	else
		if (L[6] <= 500000.0)	ChildDeduction = 1500.0;	else
		if (L[6] <= 60000.0)	ChildDeduction = 1000.0;	else
		if (L[6] <= 70000.0)	ChildDeduction =  500.0;	else
		ChildDeduction = 0.0;
   	break;
   default:	ChildDeduction = 0.0;
  } 
 L[10] = (double)L10a * ChildDeduction;

 if (L[11] < stdded)
  L[11] = stdded;

 L12a = L[9] + L[10] + L[11];

 L[12] = L[8] - L12a;

 L[14] = L[13] * L[12];		 /* NC Taxable Income. */

 L[15] = flat_tax_rate * L[14];	 /* NC Income Tax. */
 L[15] = Conditional_Round( L[15] );

 L[17] = L[15] - L[16];

 /* Calculate USE tax, if not entered on L18
  * based on Use Tax Worksheet on page 9.
  *  Estimate as:   0.000675 * L[14]
  * If you made purchases greater that $1000 that need
  * to be reported, or taxes paid to another state, then
  * fill out the Work Sheet in the instructions and enter
  * the amount on L18 in data file.
  */

 printf( "Assuming you have calculated your USE tax (%2.2f) according to instructions pg 9\n", L[18] );

 L[19] = L[17] + L[18];

 L[21] = L21a + L21b + L21c + L21d;

 L[23] = L[20] + L[21] + L[22];

 L[25] = L[23] - L[24];

 if (L[19] > L[25]) 
  {
   L[26] = L[19] - L[25];	/* You OWE */
   printf("         (Which is %2.1f%% of the total amount owed.)\n", 100.0 * L[26] / (L[19] + 1e-9) );

   /* Check for under payment see form D422 Part I */
   min_payment = 0.9 * L[19]; /* Estimate min required tax payments, form D422 Part I */
   if ((L[23] < min_payment) && (L[19] > 1000.00)) 
    {
     /* We would calculate penalty here... */
     printf("WARNING: Possible underpayment of est. taxes penalty. Calculation not performed.\n"); 
    }
   L[27] = L[26];  /* Assumes no penalties. */
  }
 else
  {
   L[28] = L[25] - L[19];
   L[33] = L[29] + L[30] + L[31] + L[32];
   L[34] = L[28] - L[33];	/* REFUND */
  }


 /*-------------------------*/
 /* ---- Print Results ---- */
 /*-------------------------*/

 showline(6);	/* Taxable fed income */
 showline(7);	/* Additions to fed income */
 showline(8);	
 showline(9);	/* Deductions */
 if (L10a > 0)
  fprintf(outfile, "L10a	%d\n", L10a );
 showline(10);
 showline(11);
 if (L[11] <= stdded)
  fprintf(outfile," Check_UsedStdDed: X\n");
 else
  fprintf(outfile," Check_ItemizedDed: X\n");
 showline_wlabel( "L12a", L12a );
 showline(12);
 if (L[13] < 1.0) showline(13);	 /* Part-yr */
 showline_wmsg(14, "North Carolina Taxable Income");
 showline_wmsg(15, "North Carolina Income Tax");
 showline(16);
 showline(17);
 if (L[18] == 0.0)
  fprintf(outfile,"Check_NoUseTax X\n");
 showline(18);
 showline(19);
 showline_wlabel( "L20a", L20a );
 showline_wlabel( "L20b", L20b );
 showline_wlabelmsg( "L20", L[20], "North Carolina Tax Withheld");
 showline_wlabel( "L21a", L21a );
 showline_wlabel( "L21b", L21b );
 showline_wlabel( "L21c", L21c );
 showline_wlabel( "L21d", L21d );
 showline(22);
 showline(23);
 showline(25);
 if (L[19] > L[25])
  {
   showline_wlabelmsg( "L26a", L[26], "TAX DUE" );
   showline_wmsg( 27, "Pay this amount" );
   fprintf(outfile,"         (Which is %2.1f%% of your total tax.)\n", 100.0 * L[26] / (L[19] + 1e-9) );
   if ((L[23] < min_payment) && (L[19] > 1000.00))
    {
     fprintf(outfile," You may owe underpayment interest and penalties.\n");
     fprintf(outfile," See page 6+7 instructions to calculate them according to your situation.\n");
    }
  }
 else
  {
   showline_wmsg(28, "OVERPAYMENT");
   showline(29);
   showline(30);
   showline(31);
   showline(32);
   showline(33);
   showline(34);
  }

 do_all_caps = 1;
 fprintf(outfile,"\n{ --------- }\n");
 GetTextLineF( "Your1stName:" );
 GetTextLineF( "YourInitial:" );
 GetTextLineF( "YourLastName:" );

 writeout_line = 0;
 socsec = GetTextLineF( "YourSocSec#:" );
 strcpy( socsectmp, socsec );   /* Copy to buffer, since formatting could add 2-chars. */
 format_socsec( socsectmp, 0 );
 fprintf(outfile,"YourSocSec#: %s\n", socsectmp );
 free( socsec );
 writeout_line = 1;

   GetTextLineF( "Spouse1stName:" );
   GetTextLineF( "SpouseInitial:" );
   GetTextLineF( "SpouseLastName:" );
   writeout_line = 0;
   socsec = GetTextLineF( "SpouseSocSec#:" );
   strcpy( socsectmp, socsec );   /* Copy to buffer, since formatting could add 2-chars. */
   format_socsec( socsectmp, 0 );
   fprintf(outfile,"SpouseSocSec#: %s\n", socsectmp );
   free( socsec );
   writeout_line = 1;

 GetTextLineF( "Number&Street:" );
 GetTextLineF( "Apt#:" );
 GetTextLineF( "Town:" );
 GetTextLineF( "State:" );
 GetTextLineF( "Zipcode:" );

 fclose(infile);
 grab_any_pdf_markups( infname, outfile );
 fclose(outfile);
 Display_File( outfname );
 printf("\nResults written to file '%s'\n", outfname);
 return 0;
}

#undef SINGLE
#undef MARRIED_FILING_JOINTLY
#undef MARRIED_FILING_SEPARAT
#undef HEAD_OF_HOUSEHOLD
#undef WIDOW
#undef Yes
#undef No
}
namespace taxsolve_NY_IT201_2022 {
#define SINGLE                  1
#define MARRIED_FILING_JOINTLY	2
#define MARRIED_FILING_SEPARAT 	3
#define HEAD_OF_HOUSEHOLD       4
#define WIDOW                   5
#define Yes 1
#define No  0
#define True  1
#define False 0
/************************************************************************/
/* TaxSolve_NY_IT-201_2022.c - NY State Tax form IT-201 for 2022.	*/
/* Copyright (C) 2003-2022 - Aston Roberts, Skeet Monker		*/
/* 									*/
/* Compile:   gcc taxsolve_NY_IT201_2022.c -o taxsolve_NY_IT201_2022	*/
/* Run:	      ./taxsolve_NY_IT201_2022  NY_IT201_2022.txt 		*/
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
/* Modified for NY 2005-2022 taxes - Skeet Monker			*/
/* Corrections 2021 taxes - Jason Striegel				*/
/************************************************************************/

float thisversion=20.00;


double A[10], S[10];


int 	status=0;

char 	statusnames[10][20]={"0","Single","Married/Joint","Married/Sep","Head_of_House","Widow"};
char 	*Your1stName="", *YourLastName="", *YourInitial="", 
	*Spouse1stName="", *SpouseLastName="", *SpouseInitial="";
char	*YourSocSec=0, *SpouseSocSec=0, *MailAddress=0, *AptNumber=0,
	Town[2048]="", StateName[1024]="", Zipcode[1024]="";

double L47a=0.0;                 /* NYC resident tax on line 47 */
double L69a=0.0;                 /* NYC school tax credit (rate reduction amount) */

struct FedReturnData
 {
  double fedline[MAX_LINES], schedA[MAX_LINES], schedD[MAX_LINES],
	 sched[8][MAX_LINES], fed_L4b, fed_L5b, fed_L6b,
	 schedA5a, schedA5b, schedA5c, schedA8a, schedA8b, schedA8c;
  int Exception, Itemized;
 } PrelimFedReturn;

struct dependent_info
 {
  char *Name1st, *NameLst, *Relat, *SocSec;
 } Dep_info[10];
int nDeps=0;


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


char *pull_initial( char *name )
{ /* Expect names like:  "John, D.", and pull initial out. */
  int j=0;
  char midinitial[10];
  while ((name[j] != '\0') && (name[j] != ','))
   j++;
  if (name[j] == ',')
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





int ImportFederalReturnData( char *fedlogfile, struct FedReturnData *fed_data )
{
 FILE *infile;
 char fline[4000], word[4000], tword[2000];
 int linenum, j;

 for (linenum=0; linenum<MAX_LINES; linenum++) 
  { 
   fed_data->fedline[linenum] = 0.0;
   fed_data->schedA[linenum] = 0.0;
   fed_data->schedD[linenum] = 0.0;
   for (j=0; j < 8; j++) fed_data->sched[j][linenum] = 0.0;
  }
 fed_data->fed_L4b = 0.0;
 fed_data->fed_L5b = 0.0;
 fed_data->fed_L6b = 0.0;
 fed_data->schedA5a = 0.0;
 fed_data->schedA5b = 0.0;
 fed_data->schedA5c = 0.0;
 fed_data->schedA8a = 0.0;
 fed_data->schedA8b = 0.0;
 fed_data->schedA8c = 0.0;
 convert_slashes( fedlogfile );
 infile = fopen(fedlogfile, "r");
 if (infile==0)
  {
   printf("Error: Could not open Federal return '%s'\n", fedlogfile);
   fprintf(outfile,"Error: Could not open Federal return '%s'\n", fedlogfile);
   return 0; 
  }
 fed_data->Itemized = 1; /* Set initial default values. */
 read_line(infile,fline);  linenum = 0;
 while (!feof(infile))
  {
   if (strstr(fline,"Use standard deduction.")!=0) fed_data->Itemized = 0;
   next_word(fline, word, " \t=");
   if ((strstr(word,"L")==word) && (strstr(fline," = ")!=0))
    {
     if (strcmp(word,"L9b") != 0)
      {
       if (sscanf(&word[1],"%d",&linenum)!=1)
	{
	 printf("Error: Reading Fed line number '%s%s'\n",word,fline);
	 fprintf(outfile,"Error: Reading Fed line number '%s%s'\n",word,fline);
	}
       next_word(fline, tword, " \t=");
       if (sscanf(tword,"%lf", &fed_data->fedline[linenum])!=1)
	{
	 printf("Error: Reading Fed line %d '%s%s'\n",linenum,tword,fline);
	 fprintf(outfile,"Error: Reading Fed line %d '%s%s'\n",linenum,tword,fline);
	}
       if (round_to_whole_dollars)
	fed_data->fedline[linenum] = Round( fed_data->fedline[linenum] );
       if (strcmp(word,"L4b") == 0)
	fed_data->fed_L4b = fed_data->fedline[linenum];
       if (strcmp(word,"L5b") == 0)
	fed_data->fed_L5b = fed_data->fedline[linenum];
       if (strcmp(word,"L6b") == 0)
	fed_data->fed_L6b = fed_data->fedline[linenum];

       if (verbose) printf("FedLin[%d] = %2.2f\n", linenum, fed_data->fedline[linenum]);
      }
    }
   else
   if ((strstr(word,"A")==word) && (strstr(word,"AMT")!=word) && (strstr(fline," = ")!=0))
    { /*Schedule-A*/
     if (strcmp( word, "A5a" ) == 0)
      {
	next_word(fline, word, " \t=");
	if (sscanf(word,"%lf", &fed_data->schedA5a) != 1) 
         {
          printf("Error: Reading Fed SchedA5a %d '%s%s'\n",linenum,word,fline);
	  fprintf(outfile, "Error: Reading Fed SchedA %d '%s%s'\n",linenum,word,fline);
         }
        if (round_to_whole_dollars)
         fed_data->schedA5a = Round( fed_data->schedA5a );
	if (verbose) printf("FedLinSchedA5a = %2.2f\n", fed_data->schedA5a );
      }
     else
     if (strcmp( word, "A5b" ) == 0)
      {
	next_word(fline, word, " \t=");
	if (sscanf(word,"%lf", &fed_data->schedA5b) != 1) 
         {
          printf("Error: Reading Fed SchedA5b %d '%s%s'\n",linenum,word,fline);
	  fprintf(outfile, "Error: Reading Fed SchedA %d '%s%s'\n",linenum,word,fline);
         }
        if (round_to_whole_dollars)
         fed_data->schedA5b = Round( fed_data->schedA5b );
	if (verbose) printf("FedLinSchedA5b = %2.2f\n", fed_data->schedA5b );
      }
     else
     if (strcmp( word, "A5c" ) == 0)
      {
	next_word(fline, word, " \t=");
	if (sscanf(word,"%lf", &fed_data->schedA5c) != 1) 
         {
          printf("Error: Reading Fed SchedA5c %d '%s%s'\n",linenum,word,fline);
	  fprintf(outfile, "Error: Reading Fed SchedA %d '%s%s'\n",linenum,word,fline);
         }
        if (round_to_whole_dollars)
         fed_data->schedA5c = Round( fed_data->schedA5c );
	if (verbose) printf("FedLinSchedA5c = %2.2f\n", fed_data->schedA5c );
      }
     else
     if (strcmp( word, "A8a" ) == 0)
      {
	next_word(fline, word, " \t=");
	if (sscanf(word,"%lf", &fed_data->schedA8a) != 1) 
         {
          printf("Error: Reading Fed SchedA8a %d '%s%s'\n",linenum,word,fline);
	  fprintf(outfile, "Error: Reading Fed SchedA %d '%s%s'\n",linenum,word,fline);
         }
        if (round_to_whole_dollars)
         fed_data->schedA8a = Round( fed_data->schedA8a );
	if (verbose) printf("FedLinSchedA8a = %2.2f\n", fed_data->schedA8a );
      }
     else
     if (strcmp( word, "A8b" ) == 0)
      {
	next_word(fline, word, " \t=");
	if (sscanf(word,"%lf", &fed_data->schedA8b) != 1) 
         {
          printf("Error: Reading Fed SchedA8b %d '%s%s'\n",linenum,word,fline);
	  fprintf(outfile, "Error: Reading Fed SchedA %d '%s%s'\n",linenum,word,fline);
         }
        if (round_to_whole_dollars)
         fed_data->schedA8b = Round( fed_data->schedA8b );
	if (verbose) printf("FedLinSchedA8b = %2.2f\n", fed_data->schedA8b );
      }
     else
     if (strcmp( word, "A8c" ) == 0)
      {
	next_word(fline, word, " \t=");
	if (sscanf(word,"%lf", &fed_data->schedA8c) != 1) 
         {
          printf("Error: Reading Fed SchedA8c %d '%s%s'\n",linenum,word,fline);
	  fprintf(outfile, "Error: Reading Fed SchedA %d '%s%s'\n",linenum,word,fline);
         }
        if (round_to_whole_dollars)
         fed_data->schedA8c = Round( fed_data->schedA8c );
	if (verbose) printf("FedLinSchedA8c = %2.2f\n", fed_data->schedA8c );
      }
     else
      { /*normal*/
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
       if (round_to_whole_dollars)
        fed_data->schedA[linenum] = Round( fed_data->schedA[linenum] );
       if (verbose) printf("FedLinSchedA[%d] = %2.2f\n", linenum, fed_data->schedA[linenum]);
      } /*normal*/
    } /*Schedule-A*/
   else
   if ((strstr(word,"D")==word) && (strstr(fline," = ")!=0) && (strstr(word,"Dep")!=word))
    { /*Schedule-D*/
     if (sscanf(&word[1],"%d",&linenum)!=1)
      {
	printf("Error: Reading Fed line number '%s%s'\n",word,fline);
	fprintf(outfile,"Error: Reading Fed line number '%s%s'\n",word,fline);
      }
     next_word(fline, word, " \t=");
     if (strcmp(word,"d") == 0)
      { /*Basis,Sale,Gain line. Capture last value.*/
	next_word(fline, word, " \teh=" );
	while (word[0] != '\0')
	 {
	  if (sscanf(word,"%lf", &fed_data->schedD[linenum]) != 1)
	    fprintf(outfile,"Error: Reading Fed schedD %d '%s %s'\n", linenum, word, fline);
	  if (round_to_whole_dollars)
	   fed_data->schedD[linenum] = Round( fed_data->schedD[linenum] );
	  next_word(fline, word, " \teh=" );
	 }
      } /*Schedule-D*/
     else
     if (sscanf(word,"%lf", &fed_data->schedD[linenum]) != 1) 
      {
       if (strncasecmp(word,"yes",1) == 0) fed_data->schedD[linenum] = 1;
       else
       if (strncasecmp(word,"no",1) == 0) fed_data->schedD[linenum] = 0;
       else
        {
         printf("Error: Reading fed schedD %d '%s%s'\n", linenum, word, fline);
	 fprintf(outfile,"Error: Reading Fed schedD %d '%s%s'\n", linenum, word, fline);
        }
      }
     else
     if (round_to_whole_dollars)
       fed_data->schedD[linenum] = Round( fed_data->schedD[linenum] );
     if (verbose) printf("FedLin[%d] = %2.2f\n", linenum, fed_data->schedD[linenum]);
    }
   else
   if (strcmp(word,"S1_2a") == 0)
    {
     next_word(fline, word, " \t=:");
     if (sscanf(word,"%lf", &fed_data->sched[1][2]) != 1) 
      {
       printf("Error: Reading Fed sched1 line 2 '%s'\n", word );
       fprintf(outfile,"Error: Reading Fed sched1 line 2 '%s'\n", word );
      }
     if (round_to_whole_dollars)
      fed_data->sched[1][2] = Round( fed_data->sched[1][2] );
    }
   else
   if (strcmp(word,"S1_2b:") == 0)
    {
	; // ignore this entry.
    }
   else
   if ((strncmp(word,"S1_",3) == 0) && (strstr( word, "_Type" ) == 0))
    {
     next_word( &(word[3]), tword, " \t=:");
     if (sscanf( tword, "%d", &linenum) != 1)
      {
       printf("Error: Reading Fed sched1 line-number '%s'\n", word );
       fprintf(outfile,"Error: Reading Fed sched1 line-number '%s'\n", word );
      }
     else
      {
	next_word(fline, word, " \t=:");
	if (sscanf(word,"%lf", &fed_data->sched[1][linenum]) != 1) 
         {
	  printf("Error: Reading Fed sched1 line %d '%s'\n", linenum, word );
	  fprintf(outfile,"Error: Reading Fed sched1 line %d '%s'\n", linenum, word );
	 }
	if (round_to_whole_dollars)
	 fed_data->sched[1][linenum] = Round( fed_data->sched[1][linenum] );
      }
    }
   else
   if ((strncmp(word,"S2_",3) == 0) && (strstr( word, "_Type" ) == 0))
    {
     next_word( &(word[3]), tword, " \t=:");
     if (sscanf( tword, "%d", &linenum) != 1)
      {
       printf("Error: Reading Fed sched2 line-number '%s'\n", word );
       fprintf(outfile,"Error: Reading Fed sched2 line-number '%s'\n", word );
      }
     else
      {
	next_word(fline, word, " \t=:");
	if (sscanf(word,"%lf", &fed_data->sched[2][linenum]) != 1) 
	 {
	  printf("Error: Reading Fed sched2 line %d '%s'\n", linenum, word );
	  fprintf(outfile,"Error: Reading Fed sched2 line %d '%s'\n", linenum, word );
	 }
	if (round_to_whole_dollars)
	 fed_data->sched[2][linenum] = Round( fed_data->sched[2][linenum] );
      }
    }
   else
   if (strncmp(word,"S3_",3) == 0)
    {
     next_word( &(word[3]), tword, " \t=:");
     if (sscanf( tword, "%d", &linenum) != 1)
      {
       printf("Error: Reading Fed sched3 line-number '%s'\n", word );
       fprintf(outfile,"Error: Reading Fed sched3 line-number '%s'\n", word );
      }
     else
     if ((linenum != 6) && (linenum != 13))
      {
	next_word(fline, word, " \t=:");
	if (sscanf(word,"%lf", &fed_data->sched[3][linenum]) != 1) 
	 {
	  printf("Error: Reading Fed sched3 line %d '%s'\n", linenum, word );
	  fprintf(outfile,"Error: Reading Fed sched3 line %d '%s'\n", linenum, word );
	 }
	if (round_to_whole_dollars)
	 fed_data->sched[3][linenum] = Round( fed_data->sched[3][linenum] );
      }
    }
   else
   if (strcmp(word,"Status") == 0)
    {
     next_word(fline, word, " \t=");
     fprintf(outfile," Status %s\n", word );
     if (strncasecmp(word,"Single",4)==0)
	status = SINGLE;
     else
     if (strncasecmp(word,"Married/Joint",13)==0)
	status = MARRIED_FILING_JOINTLY; 
     else
     if (strncasecmp(word,"Married/Sep",11)==0)
	status = MARRIED_FILING_SEPARAT;
     else
     if (strncasecmp(word,"Head_of_House",4)==0)
	status = HEAD_OF_HOUSEHOLD;
     else
     if (strncasecmp(word,"Widow",4)==0)
	status = WIDOW;
     else 
      { 
       printf("Error: unrecognized status '%s'. Exiting.\n", word); 
       fprintf(outfile,"Error: unrecognized status '%s'. Exiting.\n", word);
       return 0; 
      }
    }
   else
   if (strcmp(word,"Your1stName:") == 0)
    {
	Your1stName = strdup( fline );
	YourInitial = pull_initial( Your1stName );
    }
   else
   if (strcmp(word,"YourLastName:") == 0)
    {
	YourLastName = strdup( fline );
    }
   else
   if (strcmp(word,"YourSocSec#:") == 0)
    {
	YourSocSec = strdup( fline );
    }
   else
   if (strcmp(word,"Spouse1stName:") == 0)
    {
	Spouse1stName = strdup( fline );
	SpouseInitial = pull_initial( Spouse1stName );
    }
   else
   if (strcmp(word,"SpouseLastName:") == 0)
    {
	SpouseLastName = strdup( fline );
    }
   else
   if (strcmp(word,"SpouseSocSec#:") == 0)
    {
	SpouseSocSec = strdup( fline );
    }
   else
   if (strcmp(word,"Number&Street:") == 0)
    {
	MailAddress = strdup( fline );
    }
   else
   if (strcmp(word,"Apt#:") == 0)
    {
	AptNumber = strdup( fline );
    }
   else
   if (strcmp(word,"TownStateZip:") == 0)
    { /* Expect:  town name, NY, 10033	*/
     next_word( fline, Town, "," );
     next_word( fline, StateName, " \t," );
     next_word( fline, Zipcode, " \t," );
    }
   else
   if ((strncmp(word,"Dep",3) == 0) && (strstr(word,"_FirstName:") != 0))
    {
     if (strncmp(word,"Dep1_",5) == 0) j = 1; else
     if (strncmp(word,"Dep2_",5) == 0) j = 2; else
     if (strncmp(word,"Dep3_",5) == 0) j = 3; else
     if (strncmp(word,"Dep4_",5) == 0) j = 4; else
     if (strncmp(word,"Dep5_",5) == 0) j = 5; else j = -1;
     if (j > 0)
      {
	next_word( fline, word, " \t\n\r" );
	Dep_info[j].Name1st = strdup( word );
	read_line(infile,fline);
	next_word( fline, word, " \t\n\r" );
	if (strstr( word, "_LastName:" ) == 0)
	 { printf("Error: expected dependent %d last name, but found '%s'\n", j, word ); }
	next_word( fline, word, " \t\n\r" );
	Dep_info[j].NameLst = strdup( word );
	read_line(infile,fline);
        next_word( fline, word, " \t\n\r" );
        if (strstr( word, "_SocSec#:" ) == 0)
         { printf("Error: expected dependent %d SocSec#, but found '%s'\n", j, word ); }
        next_word( fline, word, " \t\n\r" );
        Dep_info[j].SocSec = strdup( word );
        read_line(infile,fline);
        next_word( fline, word, " \t\n\r" );
        if (strstr( word, "_Relation:" ) == 0)
         { printf("Error: expected dependent %d Relation, but found '%s'\n", j, word ); }
        next_word( fline, word, " \t\n\r" );
        Dep_info[j].Relat = strdup( word );
        nDeps = j;
      }
    }
   read_line(infile,fline);
  }
 fclose(infile);
 return 1;
}



double TaxRateFunction( double income, int status )
{
 double tax;
 switch (status)
  {
   case MARRIED_FILING_JOINTLY: case WIDOW:					/* Updated for 2022. */
	if (income <=    17150.0) tax =             0.04 * income; else		/* Data from pg 43. */
	if (income <=    23600.0) tax =     686.0 + 0.045  * (income - 17150.0); else
	if (income <=    27900.0) tax =     976.0 + 0.0525 * (income - 23600.0); else
	if (income <=   161550.0) tax =    1202.0 + 0.0585 * (income - 27900.0); else
	if (income <=   323200.0) tax =    9021.0 + 0.0625 * (income - 161550.0); else
	if (income <=  2155350.0) tax =   19124.0 + 0.0685 * (income - 323200.0); else
	if (income <=  5000000.0) tax =  144626.0 + 0.0965 * (income - 2155350.0); else
	if (income <= 25000000.0) tax =  419135.0 + 0.103 * (income - 5000000.0); else
				  tax = 2479135.0 + 0.109 * (income - 25000000.0);
      break;
   case SINGLE: case MARRIED_FILING_SEPARAT:
	if (income <=     8500.0) tax =  	    0.04   * income; else
	if (income <=    11700.0) tax =     340.0 + 0.045  * (income - 8500.0); else
	if (income <=    13900.0) tax =     484.0 + 0.0525 * (income - 11700.0); else
	if (income <=    80650.0) tax =     600.0 + 0.0585 * (income - 13900.0); else
	if (income <=   215400.0) tax =    4504.0 + 0.0625 * (income - 80650.0); else
	if (income <=  1077550.0) tax =   12926.0 + 0.0685 * (income - 215400.0); else
	if (income <=  5000000.0) tax =   71984.0 + 0.0965 * (income - 1077550.0); else
	if (income <= 25000000.0) tax =  450500.0 + 0.103 * (income - 5000000.0); else
			 	  tax = 2510500.0 + 0.109 * (income - 25000000.0);
      break;
   case HEAD_OF_HOUSEHOLD:
	if (income <=    12080.0) tax =             0.04 * income; else
	if (income <=    17650.0) tax =     512.0 + 0.045  * (income - 12800.0); else
	if (income <=    20900.0) tax =     730.0 + 0.0525 * (income - 17650.0); else
	if (income <=   107650.0) tax =     901.0 + 0.0585 * (income - 20900.0); else
	if (income <=   269300.0) tax =    5976.0 + 0.0625 * (income - 107650.0); else
	if (income <=  1616450.0) tax =   16079.0 + 0.0685 * (income - 269300.0); else
	if (income <=  5000000.0) tax =  108359.0 + 0.0965 * (income - 1616450.0); else
	if (income <= 25000000.0) tax =  434871.0 + 0.103 * (income - 5000000.0); else
				  tax = 2494871.0 + 0.109 * (income - 25000000.0);
      break;
   default: printf("Error: Unhandled status\n"); exit(0); break;
  }
 return tax;
}


void Report_bracket_info( double income, double tx, int status )
{
 double rate;
 switch (status)
  {
   case MARRIED_FILING_JOINTLY: case WIDOW:				/* Updated for 2022. */
	if (income <=    17150.0) rate = 0.04;  else
	if (income <=    23600.0) rate = 0.045;  else
	if (income <=    27900.0) rate = 0.0525;  else
	if (income <=   161550.0) rate = 0.0585;  else
	if (income <=   323200.0) rate = 0.0625;  else
	if (income <=  2155350.0) rate = 0.0685;  else  
	if (income <=  5000000.0) rate = 0.0965; else
	if (income <= 25000000.0) rate = 0.103; else rate = 0.109;
      break;
   case SINGLE: case MARRIED_FILING_SEPARAT:
	if (income <=     8500.0) rate = 0.04;  else
	if (income <=    11700.0) rate = 0.045;  else
	if (income <=    13900.0) rate = 0.0525;  else
	if (income <=    80650.0) rate = 0.0585;  else
	if (income <=   215400.0) rate = 0.0625;  else
	if (income <=  1077550.0) rate = 0.0685;  else
	if (income <=  5000000.0) rate = 0.0965;  else
	if (income <= 25000000.0) rate = 0.103;  else rate = 0.109;
      break;
   case HEAD_OF_HOUSEHOLD:
	if (income <=    12800.0) rate = 0.04;  else
	if (income <=    17650.0) rate = 0.045;  else
	if (income <=    20900.0) rate = 0.0525;  else
	if (income <=   107650.0) rate = 0.0585;  else
	if (income <=   269300.0) rate = 0.0625;  else
	if (income <=  1616450.0) rate = 0.0685;  else
	if (income <=  5000000.0) rate = 0.0965;  else
	if (income <= 25000000.0) rate = 0.103;  else rate = 0.109;
      break;
   default: printf("Error: Unhandled status\n"); exit(0); break;
  }
 printf("tx = %g, income = %g\n", tx, income );
 if (income == 0.0) income = 0.0001;	/* Prevent divide by zero. */
 printf(" You are in the %2.1f%% marginal tax bracket,\n and you are paying an effective %2.1f%% tax on your total income.\n",
	  100.0 * rate, 100.0 * tx / income );
 fprintf(outfile," You are in the %2.1f%% marginal tax bracket,\n and you are paying an effective %2.1f%% tax on your total income.\n",
	  100.0 * rate, 100.0 * tx / income );
}


double TaxRateLookup( double income, int status )
{
 double tax, dx;
 int m;

 if (income < 25.0)  dx = 12.5;  else
 if (income < 50.0)  dx = 25.0;  else  dx = 50.0;

 /* Round and truncate results from tax-function to approximate table lookup. */
 m = income / dx;             /* Round income to nearest $50. */
 income = (double)m * dx + 0.5 * dx;      /* Place into center of a $50 bracket. */
 tax = TaxRateFunction( income, status );

 return (int)(tax + 0.5);
}


double NYcityTaxRateFunction( double income, int status )	/* From page 51. */
{
 double tax, dx;
 int m;

 if (income < 25.0) dx = 12.5; else
 if (income < 50.0) dx = 25.0; else dx = 50.0;

 m = income / dx;             /* Round income to nearest $50. */
 if (income < 65000.0)
  income = m * dx + 0.5 * dx;      /* Place into center of a $50 bracket. */

 if ((status==MARRIED_FILING_JOINTLY) || (status==WIDOW))		/* Updated for 2022. */
  {
   if (income < 21600.0)  tax = income * 0.0308; else
   if (income < 45000.0)  tax = (income - 21600.00) * 0.0376 + 665.00; else
   if (income < 90000.0)  tax = (income - 45000.00) * 0.0382 + 1545.0; else
			  tax = (income - 90000.00) * 0.0388 + 3264.0;
  }
 else
 if ((status==SINGLE) || (status==MARRIED_FILING_SEPARAT))
  {
   if (income < 12000.0)  tax = income * 0.0308; else
   if (income < 25000.0)  tax = (income - 12000.00) * 0.0376 + 369.0;  else
   if (income < 50000.0)  tax = (income - 25000.00) * 0.0382 + 858.0;  else
			  tax = (income - 50000.00) * 0.0388 + 1813.00;
  }
 else
 if (status==HEAD_OF_HOUSEHOLD)
  {
   if (income < 14400.00) tax = income * 0.0308; else
   if (income < 30000.00) tax = (income - 14400.00) * 0.0376 +  443.0;  else
   if (income < 60000.00) tax = (income - 30000.00) * 0.0382 + 1030.0;  else
			  tax = (income - 60000.00) * 0.0388 + 2176.0;
 }
 else {printf("Status not covered.\n");  exit(1);}

 if (income < 65000.0) tax = (int)(tax + 0.5);   /* Round result to whole dollar. */
 return tax;
}


void worksheet1()	/*Tax Computation Worksheet 1 (pg 45) */		/* Updated for 2022. */
{ double ws[100];
  printf(" Doing Tax Computation Worksheet 1.\n");
  ws[1] = L[33];
  ws[2] = L[38];
  ws[3] = 0.0585 * ws[2];
  if (ws[1] >= 157650.0)
    ws[9] = ws[3];
  else
   {
    ws[4] = TaxRateFunction( ws[2], status );
    ws[5] = ws[3] - ws[4];
    ws[6] = ws[1] - 107650.0;
    /* Divide by 50k and round to forth decimal place. */
    ws[7] = 0.0001 * (double)Round( 10000.0 * (ws[6] / 50000.0) );
    ws[8] = ws[5] * ws[7];
    ws[9] = ws[4] + ws[8];
   }
  L[39] = ws[9];
}


void worksheet2()	/*Tax Computation Worksheet 2 (pg 46) */
{ double ws[100];
  printf(" Doing Tax Computation Worksheet 2.\n");
  ws[1] = L[33];
  ws[2] = L[38];
  ws[3] = TaxRateFunction( ws[2], status );
  ws[4] = 430.0;
  ws[5] = 646.0;
  ws[6] = ws[1] - 161550.0;
  ws[7] = smallerof( ws[6], 50000.0 );
  /* Divide by 50k and round to forth decimal place. */
  ws[8] = 0.0001 * (double)Round( 10000.0 * (ws[7] / 50000.0) );
  ws[9] = ws[5] * ws[8];
  ws[10] = ws[3] + ws[4] + ws[9];
  L[39] = ws[10];
}


void worksheet3()	/*Tax Computation Worksheet 3 (pg 46) */
{ double ws[100];
  printf(" Doing Tax Computation Worksheet 3.\n");
  ws[1] = L[33];
  ws[2] = L[38];
  ws[3] = TaxRateFunction( ws[2], status );
  ws[4] = 1076.0;
  ws[5] = 1940.0;
  ws[6] = ws[1] - 323200.0;
  ws[7] = smallerof( ws[6], 50000.0 );
  /* Divide by 50k and round to forth decimal place. */
  ws[8] = 0.0001 * (double)Round( 10000.0 * (ws[7] / 50000.0) );
  ws[9] = ws[5] * ws[8];
  ws[10] = ws[3] + ws[4] + ws[9];
  L[39] = ws[10];
}


void worksheet4()	/*Tax Computation Worksheet 4 (pg 46) */
{ double ws[100];
  printf(" Doing Tax Computation Worksheet 4.\n");
  ws[1] = L[33];
  ws[2] = L[38];
  ws[3] = TaxRateFunction( ws[2], status );
  ws[4] = 3016.0;
  ws[5] = 60349.0;
  ws[6] = ws[1] - 2155350.0;
  ws[7] = smallerof( ws[6], 50000.0 );
  /* Divide by 50k and round to forth decimal place. */
  ws[8] = 0.0001 * (double)Round( 10000.0 * (ws[7] / 50000.0) );
  ws[9] = ws[5] * ws[8];
  ws[10] = ws[3] + ws[4] + ws[9];
  L[39] = ws[10];
}


void worksheet5()	/*Tax Computation Worksheet 5 (pg 47) */
{ double ws[100];
  printf(" Doing Tax Computation Worksheet 5.\n");
  ws[1] = L[33];
  ws[2] = L[38];
  ws[3] = TaxRateFunction( ws[2], status );
  ws[4] = 63365.0;
  ws[5] = 32500.0;
  ws[6] = ws[1] - 5000000.0;
  ws[7] = smallerof( ws[6], 50000.0 );
  /* Divide by 50k and round to forth decimal place. */
  ws[8] = 0.0001 * (double)Round( 10000.0 * (ws[7] / 50000.0) );
  ws[9] = ws[5] * ws[8];
  ws[10] = ws[3] + ws[4] + ws[9];
  L[39] = ws[10];
}


void worksheet6()	/*Tax Computation Worksheet 6 (pg 47) */
{ double ws[100];
  printf(" Doing Tax Computation Worksheet 6.\n");
  ws[1] = L[38];
  ws[2] = 0.109 * ws[1];
  L[39] = ws[2];
}


void worksheet7()	/*Tax Computation Worksheet 7 (pg 47) */
{ double ws[100];
  printf(" Doing Tax Computation Worksheet 7.\n");
  ws[1] = L[33];
  ws[2] = L[38];
  ws[3] = 0.0625 * ws[2];
  if (ws[1] < 157650.0)
   {
    ws[4] = TaxRateFunction( ws[2], status );
    ws[5] = ws[3] - ws[4];
    ws[6] = ws[1] - 107650.0;
    /* Divide by 50k and round to forth decimal place. */
    ws[7] = 0.0001 * (double)Round( 10000.0 * (ws[6] / 50000.0) );
    ws[8] = ws[5] * ws[7];
    ws[9] = ws[4] + ws[8];
   }
  else
   ws[9] = ws[3];
  L[39] = ws[9];
}


void worksheet8()	/*Tax Computation Worksheet 8 (pg 48) */
{ double ws[100];
  printf(" Doing Tax Computation Worksheet 8.\n");
  ws[1] = L[33];
  ws[2] = L[38];
  ws[3] = TaxRateFunction( ws[2], status );
  ws[4] = 536.0;
  ws[5] = 1293.0;
  ws[6] = ws[1] - 215400.0;
  ws[7] = smallerof( ws[6], 50000.0 );
  /* Divide by 50k and round to forth decimal place. */
  ws[8] = 0.0001 * (double)Round( 10000.0 * (ws[7] / 50000.0) );
  ws[9] = ws[5] * ws[8];
  ws[10] = ws[3] + ws[4] + ws[9];
  L[39] = ws[10];
}


void worksheet9()	/*Tax Computation Worksheet 9 (pg 48) */
{ double ws[100];
  printf(" Doing Tax Computation Worksheet 9.\n");
  ws[1] = L[33];
  ws[2] = L[38];
  ws[3] = TaxRateFunction( ws[2], status );
  ws[4] = 1829.0;
  ws[5] = 30171.0;
  ws[6] = ws[1] - 1077550.0;
  ws[7] = smallerof( ws[6], 50000.0 );
  /* Divide by 50k and round to forth decimal place. */
  ws[8] = 0.0001 * (double)Round( 10000.0 * (ws[7] / 50000.0) );
  ws[9] = ws[5] * ws[8];
  ws[10] = ws[3] + ws[4] + ws[9];
  L[39] = ws[10];
}


void worksheet10()	/*Tax Computation Worksheet 10 (pg 48) */
{ double ws[100];
  printf(" Doing Tax Computation Worksheet 10.\n");
  ws[1] = L[33];
  ws[2] = L[38];
  ws[3] = TaxRateFunction( ws[2], status );
  ws[4] = 32000.0;
  ws[5] = 32500.0;
  ws[6] = ws[1] - 5000000.0;
  ws[7] = smallerof( ws[6], 50000.0 );
  /* Divide by 50k and round to forth decimal place. */
  ws[8] = 0.0001 * (double)Round( 10000.0 * (ws[7] / 50000.0) );
  ws[9] = ws[5] * ws[8];
  ws[10] = ws[3] + ws[4] + ws[9];
  L[39] = ws[10];
}


void worksheet11()	/*Tax Computation Worksheet 11 (pg 49) */
{ double ws[100];
  printf(" Doing Tax Computation Worksheet 11.\n");
  ws[1] = L[38];
  ws[2] = 0.0109 * ws[1];
  L[39] = ws[2];
}


void worksheet12()	/*Tax Computation Worksheet 12 (pg 49) */
{ double ws[100];
  printf(" Doing Tax Computation Worksheet 12.\n");
  ws[1] = L[33];
  ws[2] = L[38];
  ws[3] = 0.0625 * ws[2];
  if (ws[1] >= 157650.0)
   ws[9] = ws[3];
  else
   {
    ws[4] = TaxRateFunction( ws[2], status );
    ws[5] = ws[3] - ws[4];
    ws[6] = ws[1] - 107650.0;
    /* Divide by 50k and round to forth decimal place. */
    ws[7] = 0.0001 * (double)Round( 10000.0 * (ws[6] / 50000.0) );
    ws[8] = ws[5] * ws[7];
    ws[9] = ws[4] + ws[8];
    }
   L[39] = ws[9];
  }


void worksheet13()	/*Tax Computation Worksheet 13 (pg 49) */
{ double ws[100];
  printf(" Doing Tax Computation Worksheet 13.\n");
  ws[1] = L[33];
  ws[2] = L[38];
  ws[3] = TaxRateFunction( ws[2], status );
  ws[4] = 752.0;
  ws[5] = 1616.0;
  ws[6] = ws[1] - 269300.0;
  ws[7] = smallerof( ws[6], 50000.0 );
  /* Divide by 50k and round to forth decimal place. */
  ws[8] = 0.0001 * (double)Round( 10000.0 * (ws[7] / 50000.0) );
  ws[9] = ws[5] * ws[8];
  ws[10] = ws[3] + ws[4] + ws[9];
  L[39] = ws[10];
}


void worksheet14()	/*Tax Computation Worksheet 14 (pg 50) */
{ double ws[100];
  printf(" Doing Tax Computation Worksheet 14.\n");
  ws[1] = L[33];
  ws[2] = L[38];
  ws[3] = TaxRateFunction( ws[2], status );
  ws[4] = 2368.0;
  ws[5] = 45261.0;
  ws[6] = ws[1] - 1616450.0;
  ws[7] = smallerof( ws[6], 50000.0 );
  /* Divide by 50k and round to forth decimal place. */
  ws[8] = 0.0001 * (double)Round( 10000.0 * (ws[7] / 50000.0) );
  ws[9] = ws[5] * ws[8];
  ws[10] = ws[3] + ws[4] + ws[9];
  L[39] = ws[10];
}


void worksheet15()	/*Tax Computation Worksheet 15 (pg 50) */
{ double ws[100];
  printf(" Doing Tax Computation Worksheet 15.\n");
  ws[1] = L[33];
  ws[2] = L[38];
  ws[3] = TaxRateFunction( ws[2], status );
  ws[4] = 47629.0;
  ws[5] = 32500.0;
  ws[6] = ws[1] - 5000000.0;
  ws[7] = smallerof( ws[6], 50000.0 );
  /* Divide by 50k and round to forth decimal place. */
  ws[8] = 0.0001 * (double)Round( 10000.0 * (ws[7] / 50000.0) );
  ws[9] = ws[5] * ws[8];
  ws[10] = ws[3] + ws[4] + ws[9];
  L[39] = ws[10];
}


void worksheet16()	/*Tax Computation Worksheet 16 (pg 50) */
{ double ws[100];
  printf(" Doing Tax Computation Worksheet 16.\n");
  ws[1] = L[38];
  ws[2] = 0.109 * ws[1];
  L[39] = ws[2];
}


void tax_computation_worksheet( int status )	/* Called for Line-39 when Line-33 > $107,650. */
{ /* Worksheets from pages 45-50. Come here when AGI L[33] > $107,650. */
 switch (status)								/* Updated for 2022. */
  {
     case MARRIED_FILING_JOINTLY:  case WIDOW:			// 1-6
	if (L[33] <= 25000000.0)
	 {
	   if (L[38] <= 161550.0)
	    worksheet1();
	   else
	   if ((L[33] > 161550.0) && (L[38] <= 323200.0))
	    worksheet2();
	   else
	   if ((L[33] > 323200.0) && (L[38] > 323200.0) && (L[38] <= 2155350.0))
	    worksheet3();
	   else
	   if ((L[33] > 215535.0) && (L[38] > 2155350.0) && (L[38] <= 5000000.0))
	    worksheet4();
	   else
	   if ((L[33] > 5000000.0) && (L[38] > 5000000.0))
	    worksheet5();
	   else
	    {
		printf("AGI Case not handled.\n");
		fprintf(outfile,"AGI Case not handled. L33=%6.2f, L38=%6.2f\n", L[33], L[38] );
		exit(1);
	    }
	 }
	else
	 worksheet6();
	break;
     case SINGLE:  case MARRIED_FILING_SEPARAT:		// 7 - 11
	if (L[33] <= 25000000.0)
	 {
	   if (L[38] <= 215400.0)
	    worksheet7();
	   else
	   if ((L[33] > 215535.0) && (L[38] <= 1077550.0))
	    worksheet8();
	   else
	   if ((L[33] > 107750.0) && (L[38] <= 5000000.0))
	    worksheet9();
	   else
	   if  ((L[33] > 5000000.0) && (L[38] <= 5000000.0))
	    worksheet10();
	   else
	    {
		printf("AGI Case not handled.\n");
		fprintf(outfile,"AGI Case not handled. L33=%6.2f, L38=%6.2f\n", L[33], L[38] );
		exit(1);
	    }
	 }
	else
	 worksheet11();
	break;
     case HEAD_OF_HOUSEHOLD:					// 12-16

	if (L[33] <= 25000000.0)
	 {
	   if (L[38] <= 269300.0)
	    worksheet12();
	   else
	   if ((L[33] > 269300.0) && (L[38] <= 1616450.0))
	    worksheet13();
	   else
	   if ((L[33] > 1616450.0) && (L[38] <= 5000000.0))
	    worksheet14();
	   else
	   if  ((L[33] > 5000000.0) && (L[38] > 5000000.0))
	    worksheet15();
	   else
	    {
		printf("AGI Case not handled.\n");
		fprintf(outfile,"AGI Case not handled. L33=%6.2f, L38=%6.2f\n", L[33], L[38] );
		exit(1);
	    }
	 }
	else
	 worksheet16();
	break;
     default: printf("Case not handled.\n");  fprintf(outfile,"Case not handled.\n"); exit(1);
  }
}


/*----------------------------------------------------------------------------*/

int main( int argc, char *argv[] )
{
 int j, k, argk, day, month, yyyy, itemize=0, all_forms=0;
 char word[1000], *infname=0, outfname[1000], *answ;
 time_t now;
 int Dependent, Exemptions, nyc_resident, IT588=0, L36=0;
 double itemized_ded, std_ded=0.0, LTC=0, AddAdj=0.0, CollegeDed=0.0, L19a=0.0;
 double form_IT196[MAX_LINES], form_IT196_16a=0.0, dedthresh, IT588_L9=0.0, IT588_L18=0.0;
 char prelim_1040_outfilename[5000];
 char YourNames[2048]="";

 /* Intercept any command-line arguments. */
 printf("NY-IT201 - 2011 - v%3.1f\n", thisversion);
 argk = 1;  k=1;
 while (argk < argc)
 {
  if (strcmp(argv[argk],"-verbose")==0)  verbose = 1;
  else
  if (strcmp(argv[argk],"-round_to_whole_dollars")==0)  { round_to_whole_dollars = True; }
  else
  if (strcmp(argv[argk],"-allforms")==0)  { all_forms = True; }    /* Force printing of all form pages. */
  else
  if (k==1)
   {
    infname = strdup(argv[argk]);
    infile = fopen(argv[argk],"r");
    if (infile==0) {printf("ERROR: Parameter file '%s' could not be opened.\n", argv[argk]);  exit(1);}
    k = 2;
    /* Base name of output file on input file. */
    strcpy(outfname,argv[argk]);
    j = strlen(outfname)-1;
    while ((j>=0) && (outfname[j]!='.')) j--;
    if (j<0) strcat(outfname,"_out.txt"); else strcpy(&(outfname[j]),"_out.txt");
    outfile = fopen(outfname,"w");
    if (outfile==0) {printf("ERROR: Output file '%s' could not be opened.\n", outfname);  exit(1);}
    printf("Writing results to file:  %s\n", outfname);
   }
  else {printf("Unknown command-line parameter '%s'\n", argv[argk]);  exit(1);}
  argk = argk + 1;
 }

 if (infile==0) {printf("Error: No input file on command line.\n"); exit(1);}

 /* Pre-initialize all lines to zeros. */
 for (j=0; j<MAX_LINES; j++) 
  {
   L[j] = 0.0;
   form_IT196[j] = 0.0;
  }

 /* Accept parameters from input file. */
 /* Expect  NY IT200 form lines, something like:
	Title:  NY IT-200 1999 Return
	L1	;	{Wages}
	L2	;	{Interest}
	L3	;	{Dividends}
	...
*/

 /* Accept Form's "Title" line, and put out with date-stamp for records. */
 read_line( infile, word );
 now = time(0);
 fprintf(outfile,"\n%s,	 v%2.2f, %s\n", word, thisversion, ctime( &now ));
 check_form_version( word, "Title:  NY State 2022" );

 get_parameter( infile, 's', word, "FileName" );      /* Preliminary Return Output File-name. */
 get_word(infile, prelim_1040_outfilename );
 if (ImportFederalReturnData( prelim_1040_outfilename, &PrelimFedReturn ) == 0)
  {
   fclose(infile);
   fclose(outfile);
   Display_File( outfname );
   exit(1);
  }

 answ = GetTextLine( "YourDOB" );
 if (interpret_date( answ, &month, &day, &yyyy, "reading 'YourDOB'" ))
   fprintf(outfile,"YourDOB \"%s\"\n", format_mmddyyyy( month, day, yyyy ) );
 else
  fprintf(outfile,"YourDOB \"%s\"\n", answ );

 answ = GetTextLine( "SpouseDOB" );
 if (interpret_date( answ, &month, &day, &yyyy, "reading 'SpouseDOB'" ))
   fprintf(outfile,"SpouseDOB \"%s\"\n", format_mmddyyyy( month, day, yyyy ) );
 else
  fprintf(outfile,"SpouseDOB \"%s\"\n", answ );

 GetTextLineF( "County" );
 GetTextLineF( "SchooldDist" );
 GetTextLineF( "SchoolCode" );

 answ = GetTextLineF( "D1_ForeignAcct" );
 next_word( answ, word, " \t;" );
 if (toupper( word[0] ) == 'Y')
  fprintf(outfile,"CkD1y: X\n");
 else
  fprintf(outfile,"CkD1n: X\n");

 answ = GetTextLineF( "D2_1-YonkRelCred" );
 next_word( answ, word, " \t;" );
 if (toupper( word[0] ) == 'Y')
  fprintf(outfile,"CkD2_1y: X\n");
 else
 if ((mystrcasestr( word, "N/A" ) == 0) && (toupper( word[0] ) == 'N'))
  fprintf(outfile,"CkD2_1n: X\n");

 GetTextLineF( "D2_2-YRCamount" );
 
 answ = GetTextLineF( "D3-NonQualComp" );
 next_word( answ, word, " \t;" );
 if (toupper( word[0] ) == 'Y')
  fprintf(outfile,"CkD3y: X\n");
 else
  fprintf(outfile,"CkD3n: X\n");

 answ = GetTextLineF( "E1_LivedNYC" );
 next_word( answ, word, " \t;" );
 if (toupper( word[0] ) == 'Y')
  fprintf(outfile,"CkE1y: X\n");
 else
  fprintf(outfile,"CkE1n: X\n");

 GetTextLineF( "E2_DaysNYC" );
 GetTextLineF( "F1_MonthsYouNYC" );
 GetTextLineF( "F2_MonthsSpNYC" );
 GetTextLineF( "G_SpecCondCode" );

 get_parameter( infile, 's', word, "Dependent" );
 get_parameter( infile, 'b', &Dependent, "Dependent?"); 

 // GetLineF( "L1", &L[1] );	/* Wages. */
 L[1] = PrelimFedReturn.fedline[1];

 if (PrelimFedReturn.Itemized)
  {
   fprintf(outfile," Check box B = Yes\n");
   fprintf(outfile,"  Check_Itemized = X\n");
  }
 else
  {
   fprintf(outfile," Check box B = No\n");
   fprintf(outfile,"  Check_NoItemiz = X\n");
  }

 if (Dependent==1)
  {
   fprintf(outfile," Check box C = Yes\n");
   fprintf(outfile,"  Check_Depend = X\n");
  }
 else
  {
   fprintf(outfile," Check box C = No\n");
   fprintf(outfile,"  Check_NotDep = X\n");
  }

 showline(1);

 // GetLineF( "L2", &L[2] );	/* Taxable Interest. */
 L[2] = PrelimFedReturn.fedline[2];
 showline(2);

 // GetLineF( "L3", &L[3] );	/* Ordinary Dividends. */
 L[3] = PrelimFedReturn.fedline[3];
 showline(3);

 // GetLineF( "L4", &L[4] );	/* Taxable refunds, credits, offsets */
 L[4] = PrelimFedReturn.sched[1][1];
 showline(4);

 // GetLineF( "L5", &L[5] );	/* Alimony received */
 L[5] = PrelimFedReturn.sched[1][2];
 showline(5);

 // GetLineF( "L6", &L[6] );	/* Business income/loss (fed sched C) */
 L[6] = PrelimFedReturn.sched[1][3];
 showline(6);

 // GetLineF( "L7", &L[7] );	/* Capital gains/losses (fed sched D) */
 L[7] = PrelimFedReturn.schedD[16];
 showline(7);

 // GetLineF( "L8", &L[8] );	/* Other gains/losses (fed form 4794) */
 L[8] = PrelimFedReturn.sched[1][4];
 showline(8);

 // GetLine( "L9", &L[9] );	/* Taxable IRA distributions */
 L[9] = PrelimFedReturn.fed_L4b;
 showline(9);

 // GetLine( "L10", &L[10] );	/* Taxable pension/annuity amounts  */
 L[10] = PrelimFedReturn.fed_L5b;
 showline(10);

 // GetLineF( "L11", &L[11] );	/* Rental, royalties, partnership, S corp, (fed sched E) */
 L[11] = PrelimFedReturn.sched[1][5];
 showline(11);

 // GetLineF( "L13", &L[13] );	/* Farm income (fed sched F) */
 L[13] = PrelimFedReturn.sched[1][6];
 showline(13);

 // GetLineF( "L14", &L[14] );	/* Unemployment compensation */
 L[14] = PrelimFedReturn.sched[1][7];
 showline(14);

 // GetLineF( "L15", &L[15] );	/* Taxable Social Sec. benefits */
 L[15] = PrelimFedReturn.fed_L6b;
 showline(15);
 L[27] = L[15];

 // GetLineF( "L16", &L[16] );	/* Other income (pg. 14) */
 L[16] = PrelimFedReturn.sched[1][9];
 showline(16);

 for (j = 1; j <= 11; j++)
  L[17] = L[17] + L[j];
 for (j=13; j <= 16; j++)
  L[17] = L[17] + L[j];
 showline(17);
 if (absolutev( L[17] - PrelimFedReturn.fedline[9]) > 0.1)
  {
   printf(" Warning: L[17] = %6.2f, while Fed-line[9] = %6.2f\n", L[17], PrelimFedReturn.fedline[9] );
   fprintf(outfile," Warning: L[17] = %6.2f, while Fed-line[9] = %6.2f\n", L[17], PrelimFedReturn.fedline[9] );
  }

 // GetLineF( "L18", &L[18] );	/* Total federal adjustments to income (pg 14) */
 L[18] = PrelimFedReturn.sched[1][26];
 showline(18);

 L[19] = L[17] - L[18];
 showline_wmsg( 19, "Federal adjusted gross income" );
 if (absolutev(L[19] - PrelimFedReturn.fedline[11]) > 0.1)
  {
   printf(" Warning: L[19] = %6.2f, while Fed-line[11] = %6.2f\n", L[19], PrelimFedReturn.fedline[11] );
   fprintf(outfile," Warning: L[19] = %6.2f, while Fed-line[11] = %6.2f\n", L[19], PrelimFedReturn.fedline[11] );
  }
 // L19a recomputed federal agi. (IT201-instructions page 14, 19a worksheet)
 if (IT588)
  { double wrksht[10];
    wrksht[1] = L[19];
    wrksht[2] = IT588_L9;
    wrksht[3] = wrksht[1] + wrksht[2];
    wrksht[4] = IT588_L18;
    wrksht[5] = wrksht[3] - wrksht[4]; 
    L19a = wrksht[5];
  }
 else
  L19a = L[19];
 showline_wlabel( "L19a", L19a );

 GetLineF( "L20", &L[20] );	/* Interest income from non-NY state or local bonds */

 GetLineF( "L21", &L[21] );	/* Public employee retirement contributions (pg 15) */

 GetLineF( "L22", &L[22] );	/* College choice tuition saving distributions */

 GetLineF( "L23", &L[23] );	/* Other (pg 16) */

 for (j = 19; j <= 23; j++) L[24] = L[24] + L[j];
 showline(24);

 L[25] = L[4];			/* Taxable refunds, credits, offsets */
 showline(25);

 GetLineF( "L26", &L[26] );	/* Pensions of NYS and local governments and the federal government (see page 16)  */

 L[27] = L[15];			/* Taxable amount of social security benefits */
 showline(27);

 GetLineF( "L28", &L[28] );	/* Interest income on U.S. government bonds */

 GetLine( "L29", &L[29] );	/* Pension and annuity income exclusion  */
 if (L[29] > 20000.0)
  {
   L[29] = 20000.0;
   showline_wmsg( 29, "(Limited to 20,000.)" );
  }
 else
  showline(29);

 GetLine( "L30", &L[30] );	/* College choice tuition savings deduction / earnings distributions */
 if (status == MARRIED_FILING_JOINTLY)
  L[30] = smallerof( L[30], 10000.0 );
 else
  L[30] = smallerof( L[30], 5000.0 );
 showline(30);

 GetLineF( "L31", &L[31] );	/* Other (see page 21) */

 for (j=25; j <= 31; j++)
  L[32] = L[32] + L[j];
 showline(32);

 L[33] = L[24] - L[32];
 showline_wmsg(33,"New York adjusted gross income (AGI)");

 /* NYS Itemized Deductions - Form IT-196*/
 GetLine( "LTcare%", &LTC );
 GetLine( "AddAdj", &AddAdj );
 GetLine( "CollegeDed", &CollegeDed );

 form_IT196[1] = PrelimFedReturn.schedA[1];
 form_IT196[2] = L19a;
 form_IT196[3] = 0.10 * form_IT196[2];
 form_IT196[4] = NotLessThanZero( form_IT196[1] - form_IT196[3] );
 form_IT196[5] = PrelimFedReturn.schedA5a;
 form_IT196[6] = PrelimFedReturn.schedA5b;
 form_IT196[7] = PrelimFedReturn.schedA5c;
 form_IT196[8] = PrelimFedReturn.schedA[6];
 for (j=5; j <= 8; j++)
  form_IT196[9] = form_IT196[9] + form_IT196[j];
 form_IT196[10] = PrelimFedReturn.schedA8a;
 form_IT196[11] = PrelimFedReturn.schedA8b;
 form_IT196[12] = PrelimFedReturn.schedA8c;
 form_IT196[14] = PrelimFedReturn.schedA[9];
 for (j=10; j <= 14; j++)
  form_IT196[15] = form_IT196[15] + form_IT196[j];
 form_IT196_16a = PrelimFedReturn.schedA[11];
 form_IT196[16] = form_IT196_16a;
 form_IT196[17] = PrelimFedReturn.schedA[12];
 form_IT196[18] = PrelimFedReturn.schedA[13];
 for (j=16; j <= 18; j++)
  form_IT196[19] = form_IT196[19] + form_IT196[j];
 form_IT196[20] = PrelimFedReturn.schedA[15];
 form_IT196[39] = PrelimFedReturn.schedA[16];

 switch (status)	/* Determine the Deduction Threshold (IT196 pg 18). */
  {
   case WIDOW: 
   case MARRIED_FILING_JOINTLY: dedthresh = 338850.0;	break;
   case HEAD_OF_HOUSEHOLD: 	dedthresh = 310600.0;	break;
   case SINGLE: 		dedthresh = 282400.0;	break;
   case MARRIED_FILING_SEPARAT: 
   default:			dedthresh = 169400.0;	break;
  }

 if (L19a <= dedthresh)
  {
   form_IT196[40] = form_IT196[4] + form_IT196[9] + form_IT196[15] + form_IT196[19] + form_IT196[20] 
		 + form_IT196[28] + form_IT196[39];	
   fprintf(outfile,"Check_IT196_DedNotLimited = X\n");
  }
 else
  { double dedwksht[100];	/* Total itemized deductions worksheet. IT196instr pg 18. */
   fprintf(outfile,"Check_IT196_DedMaybeLimited = X\n");
   for (j=0; j< 100; j++) dedwksht[j] = 0.0;
   dedwksht[1] = form_IT196[4] + form_IT196[9] + form_IT196[15] + form_IT196[19] + form_IT196[20] +
		 form_IT196[28] + form_IT196[39];
   dedwksht[2] = form_IT196[4] + form_IT196[14] + form_IT196_16a + form_IT196[20] + form_IT196[29] +
		 form_IT196[30] + form_IT196[37];
   if (dedwksht[2] < dedwksht[1])
    {
     dedwksht[3] = dedwksht[1] - dedwksht[2];
     dedwksht[4] = 0.80 * dedwksht[3];
     dedwksht[5] = L19a;
     dedwksht[6] = dedthresh;
     if (dedwksht[6] < dedwksht[5])
      {
	dedwksht[7] = dedwksht[5] - dedwksht[6];
	dedwksht[8] = 0.03 * dedwksht[7];
	dedwksht[9] = smallerof( dedwksht[4], dedwksht[8] );
	dedwksht[10] = dedwksht[1] - dedwksht[9];
	form_IT196[40] = dedwksht[10];
      }
     else
      { /* Deduction not limited. */
       form_IT196[40] = dedwksht[1];
      }
    }
   else
    { /* Deduction not limited. */
     form_IT196[40] = dedwksht[1];
    }
   for (j=1; j <= 10; j++)
    showline_wrksht_nz( " DedWrksht_", j, dedwksht );
  }
 itemized_ded = form_IT196[40];		// Tentative only for now, finalized below.

 get_parameter( infile, 'l', word, "IT196_41 or L36" );
 if (strcmp( word, "IT196_41" ) == 0)
  {
   get_parameters( infile, 'f', &form_IT196[41], word );
   GetLine( "IT196_43", &form_IT196[43] );
   GetLine( "IT196_44", &form_IT196[44] );
   GetLine( "IT196_48", &form_IT196[48] );
   get_parameter( infile, 's', word, "L36" );	/* Number of Dependent Exemptions (Pg 76, line e) */
  }

 form_IT196[42] = form_IT196[40] - form_IT196[41];
 form_IT196[45] = form_IT196[42] + form_IT196[43] + form_IT196[44];
 if (L[33] > 100000.0)
  { double ws[20];
    for (j=0; j<20; j++) ws[j] = 0.0;
   if (L[33] < 475000.0)
    { /*wrksheet3 - IT196-Pg20*/
      ws[1] = L[33];
      switch (status)
	{
	 case SINGLE:
	 case MARRIED_FILING_SEPARAT:	ws[2] = 100000.0;	break;
	 case HEAD_OF_HOUSEHOLD:	ws[2] = 150000.0;	break;
	 case WIDOW:
	 case MARRIED_FILING_JOINTLY:	ws[2] = 200000.0;	break;
 	}
      ws[3] = ws[1] - ws[2];
      if (ws[3] >= 0.0)
       {
        ws[4] = smallerof( ws[3], 50000.0 );
        ws[5] = 0.0001 * (double)Round( 10000.0 * (ws[4] / 50000.0) );
        ws[6] = 0.25 * form_IT196[45];
        ws[7] = ws[5] * ws[6];
	form_IT196[46] = ws[7];
       }
    } /*wrksheet3 - IT196-Pg20*/
   else
   if (L[33] < 525000.0)
    { /*wrksheet4 - IT196-Pg20*/
      ws[1] = L[33] - 475000.0;
      ws[2] = 0.0001 * (double)Round( 10000.0 * (ws[1] / 50000.0) );
      ws[3] = 0.25 * form_IT196[45];
      ws[4] = ws[2] * ws[3];
      ws[5] = ws[3] + ws[4];
      form_IT196[46] = ws[5];
    } /*wrksheet4 - IT196-Pg20*/
   else
   if (L[33] < 1000000.0)
    { 
     form_IT196[46] = 0.50 * form_IT196[45];
    } 
   else
   if (L[33] < 10000000.0)
    { /*wrksheet5 - IT196-Pg20*/
      ws[1] = form_IT196[45];
      ws[2] = 0.50 * form_IT196[19];
      ws[3] = ws[1] - ws[2];
      form_IT196[46] = ws[3];
    } /*wrksheet5 - IT196-Pg20*/
   else
    { /*wrksheet6 - IT196-Pg20*/
      ws[1] = form_IT196[45];
      ws[2] = 0.25 * form_IT196[19];
      ws[3] = ws[1] - ws[2];
      form_IT196[46] = ws[3];
    } /*wrksheet6 - IT196-Pg20*/
  }
 form_IT196[47] = form_IT196[45] - form_IT196[46];
 form_IT196[49] = form_IT196[47] + form_IT196[48];
 itemized_ded = form_IT196[49];

 for (j=1; j <= 16; j++)	/* Display the IT196 lines. */
  showline_wrksht_nz( "IT196_", j, form_IT196 );
 fprintf(outfile,"IT196_16a = %6.2f\n", form_IT196_16a );
 fprintf(outfile," (Note: IT-196 Line-16 assumes Gifts are all qualified contributions.)\n");
 for (j=17; j <= 49; j++)	/* Display remaining IT196 lines. */
  showline_wrksht_nz( "IT196_", j, form_IT196 );



 switch (status)	/* Determine the Std. Deduction. Pg. 13. */
  {
   case SINGLE: if (Dependent)   std_ded = 3100.0; 
		else 		 std_ded = 8000.0;			/* Updated for 2022. */
	break;
   case MARRIED_FILING_JOINTLY:  std_ded = 16050.0; break;
   case MARRIED_FILING_SEPARAT:  std_ded =  8000.0; break;
   case HEAD_OF_HOUSEHOLD: 	 std_ded = 11200.0; break;
   case WIDOW: 			 std_ded = 16050.0; break;
  }

 if (std_ded > itemized_ded) 
  {
   L[34] = std_ded;
   fprintf(outfile,"Check_Std = X\n");
   showline_wmsg(34,"(Mark Std-deduction)");
   itemize = False;
  }
 else
  {
   L[34] = itemized_ded;
   fprintf(outfile,"Check_Item = X\n");
   showline_wmsg(34,"(Mark Itemized-deduction)");
   itemize = True;
  }

 if ((itemize) || (all_forms))
  {
   fprintf(outfile,"PDFpage: 5 5\n");
   fprintf(outfile,"EndPDFpage.\n");
   fprintf(outfile,"PDFpage: 6 6\n");
   fprintf(outfile,"EndPDFpage.\n");
   fprintf(outfile,"PDFpage: 7 7\n");
   fprintf(outfile,"EndPDFpage.\n");
  }

 L[35] = L[33] - L[34];
 if (L[35] < 0.0) L[35] = 0.0;
 else showline(35);

 // get_parameter( infile, 's', word, "L36" );	/* Number of Dependent Exemptions (Pg 76, line e) */
 get_parameters( infile, 'i', &L36, "L36" );
 L[36] = 1000.0 * (double)L36;
 showline(36);
 if (L36 > 0)
  fprintf(outfile, "L36_enter %d\n", L36 ); 

 L[37] = L[35] - L[36];
 if (L[37] < 0.0)
   L[37] = 0.0;
 showline_wmsg(37,"taxable income");
 L[38] = L[37];
 showline(38);

 if (L[33] <= 107650.0)
   L[39] = TaxRateLookup( L[38], status );
 else
   tax_computation_worksheet( status );
 showline(39);
 Report_bracket_info( L[38], L[39], status );

 /* Household credit. */
 get_parameter( infile, 's', word, "Exemptions" );	/* NY dependent exemptions, Pg 19. */
 get_parameter( infile, 'i', &Exemptions, "Exemptions" );
 if (Dependent)
  L[40] = 0.0;
 else	/* From tables starting on page 22. */
 if (status==SINGLE)
  {
   if (L[19] <  5000.0) L[40] = 75.0; else
   if (L[19] <  6000.0) L[40] = 60.0; else
   if (L[19] <  7000.0) L[40] = 50.0; else
   if (L[19] < 20000.0) L[40] = 45.0; else
   if (L[19] < 25000.0) L[40] = 40.0; else
   if (L[19] < 28000.0) L[40] = 20.0; else  L[40] = 0.0;
  }
 else	/* Status = MARRIED_FILING_JOINTLY, MARRIED_FILING_SEPARAT, Head_of_house, Widow */
  if (status!=MARRIED_FILING_SEPARAT)
   {
    if (L[19] <  5000.0) L[40] = 90.0 + 15.0 * (Exemptions-1); else
    if (L[19] <  6000.0) L[40] = 75.0 + 15.0 * (Exemptions-1); else
    if (L[19] <  7000.0) L[40] = 65.0 + 15.0 * (Exemptions-1); else
    if (L[19] < 20000.0) L[40] = 60.0 + 15.0 * (Exemptions-1); else
    if (L[19] < 22000.0) L[40] = 60.0 + 10.0 * (Exemptions-1); else
    if (L[19] < 25000.0) L[40] = 50.0 + 10.0 * (Exemptions-1); else
    if (L[19] < 28000.0) L[40] = 40.0 +  5.0 * (Exemptions-1); else  
    if (L[19] < 32000.0) L[40] = 20.0 +  5.0 * (Exemptions-1); else  L[40] = 0.0;
   }
  else
   {
    if (L[19] <  5000.0) L[40] = 45.0 + 8.0 * (Exemptions-1); else
    if (L[19] <  6000.0) L[40] = 37.5 + 8.0 * (Exemptions-1); else
    if (L[19] <  7000.0) L[40] = 32.5 + 8.0 * (Exemptions-1); else
    if (L[19] < 20000.0) L[40] = 30.0 + 8.0 * (Exemptions-1); else
    if (L[19] < 22000.0) L[40] = 30.0 + 5.0 * (Exemptions-1); else
    if (L[19] < 25000.0) L[40] = 25.0 + 5.0 * (Exemptions-1); else
    if (L[19] < 28000.0) L[40] = 20.0 + 3.0 * (Exemptions-1); else  
    if (L[19] < 32000.0) L[40] = 10.0 + 3.0 * (Exemptions-1); else  L[40] = 0.0;
   }
 showline_wmsg(40,"NY state household credit");	/* NY state household credit, (pg 29). */

 GetLineF( "L41", &L[41] );	/* Resident credit, Form IT-112-R or IT-112-C, pg 98 */

 GetLineF( "L42", &L[42] );	/* Other New York State nonrefundable credits  */

 L[43] = L[40] + L[41] + L[42];
 showline(43);

 L[44] = L[39] - L[43];
 if (L[44] < 0.0) L[44] = 0.0;
 else showline(44);

 GetLineF( "L45", &L[45] );	/* Net Other New York State taxes, Form IT-201-ATT, line 30 */

 L[46] = L[44] + L[45];
 showline_wmsg(46,"Total New York State taxes");

 get_parameter( infile, 's', word, "NYC_Resident" );
 get_parameters( infile, 'b', &nyc_resident, "NYC_Resident (yes/no) ?"); 

 GetLine( "L50", &L[50] );	/* Part-year New York City resident tax */
 GetLine( "L51", &L[51] );	/* Other New York City taxes */
 GetLine( "L53", &L[53] );	/* NY City nonrefundable credits */

 if (nyc_resident)
  { /*NYC*/

   /* TODO - see page 23. should adjust for itemized charitable deductions */
   L[47] = L[38];               /* NYC taxable income */
   showline(47);

   L47a = NYcityTaxRateFunction( L[47], status ); /* NYC resident tax */
   showline_wlabel( "L47a", L47a );

   /* NYC Household credit. */
   if (Dependent) L[48] = 0.0;
   else
   if (status==SINGLE)		/* From table 4, 5, or 6 on page 30. */
    {
     if (L[19] <  10000.0) L[48] = 15.0; else
     if (L[19] <  12500.0) L[48] = 10.0; else  L[48] = 0.0;
    }
   else	/* Status = 2, 4, or 5. */
   if (status!=MARRIED_FILING_SEPARAT)
    {
     if (L[19] <  15000.0) L[48] = 30.0 * Exemptions; else
     if (L[19] <  17500.0) L[48] = 25.0 * Exemptions; else
     if (L[19] <  20000.0) L[48] = 15.0 * Exemptions; else
     if (L[19] <  22500.0) L[48] = 10.0 * Exemptions; else  L[48] = 0.0;
    }
   else
    {
     if (L[19] <  15000.0) L[48] = 15.0 * Exemptions; else
     if (L[19] <  17500.0) L[48] = 13.0 * Exemptions; else
     if (L[19] <  20000.0) L[48] =  8.0 * Exemptions; else
     if (L[19] <  22500.0) L[48] =  5.0 * Exemptions; else  L[48] = 0.0;
    }
   showline_wmsg(48,"NY City household credit");	/* NY City household credit, (pg 34). */

   L[49] = L47a - L[48];
   if (L[49] > 0.0)
    showline(49);
   else
    L[49] = 0.0;

   showline(50);
   showline(51);

   L[52] = L[49] + L[50] + L[51];
   showline(52);
   showline(53);

   L[54] = L[52] - L[53];
   if (L[54] > 0.0) showline(54);  else  L[54] = 0.0;

   /* Yonkers case not added. */

   L[58] = L[54] + L[55] + L[56] + L[57];
   showline_wmsg(58,"NYC taxes");
  } /*NYC*/

 GetLineF( "L59", &L[59] );	/* Sales or use tax, see pg 29. */

 GetLineF( "L60", &L[60] );	/* Voluntary Gift contibutions (pgs 30). */

 L[61] = L[46] + L[58] + L[59] + L[60];
 showline(61);

 L[62] = L[61];
 showline(62);

 GetLineF( "L63", &L[63] );	/* Empire State child credit (attach new Form IT-213) */

 GetLineF( "L64", &L[64] );	/* NYS/NYC Child care/dependent credit. Form IT-216. */

 GetLineF( "L65", &L[65] );	/* NYS Earned income credit. Form IT-213. */

 GetLineF( "L66", &L[66] );	/* NYS State noncustodial parent EIC (attach new Form IT-209) */

 GetLineF( "L67", &L[67] );	/* Real property credit. Form IT-214. */

 GetLineF( "L68", &L[68] );	/* College tution credit. Form IT-272. */

 if (nyc_resident)		/* City of NY school taxcredit. (pg 26) */
  { /*NYC*/
    if (Dependent) L[69] = 0.0;
    else
    if (L[19] < 250000)
     {
      if ((status==SINGLE) || (status==MARRIED_FILING_SEPARAT) || (status==HEAD_OF_HOUSEHOLD))  L[69] = 63.0;  else
      if ((status==MARRIED_FILING_JOINTLY) || (status==WIDOW))  L[69] = 125.0;
     }
    else
     L[69] = 0.0;
    showline(69);

    if (Dependent)
     {
      L69a = 0.0;
     }
    else
    if (L[47] > 500000)
     {
      L69a = 0.0;
     }
    else
    if ((status==SINGLE) || (status==MARRIED_FILING_SEPARAT))
     {
      if (L[47] < 12000)
        L69a = L[47] * 0.00171;
      else
        L69a = (L[47] - 12000) * 0.00228 + 21;
     }
    else
    if (status==HEAD_OF_HOUSEHOLD)
     {
      if (L[47] < 14400)
        L69a = L[47] * 0.00171;
      else
        L69a = (L[47] - 14400) * 0.00228 + 25;
     }
    else
    if ((status==MARRIED_FILING_JOINTLY) || (status==WIDOW))
     {
      if (L[47] < 21600)
        L69a = L[47] * 0.00171;
      else
        L69a = (L[47] - 21600) * 0.00228 + 37;
     }
    else
     {
      L69a = 0.0;
     }

    showline_wlabel( "L69a", L69a );

    /* L[70] = earned_income_credit; */
    /* showline(70); */
  } /*NYC*/

 GetLineF( "L71", &L[71] );	/* Other refundable credits, IT-201-ATT line 18) */

 GetLineF( "L72", &L[72] );	/* Total NY State tax withheld. */

 GetLineF( "L73", &L[73] );	/* Total City of NY tax withheld. */
 GetLineF( "L74", &L[74] );	/* Yonkers tax withheld. */

 GetLineF( "L75", &L[75] );	/* Total estimated tax payments (from IT-370)*/

 for (j = 63; j <= 75; j++) L[76] = L[76] + L[j];
 L[76] += L69a;

 showline(76);

 if (L[76] > L[62])
  {
   L[77] = L[76] - L[62];
   fprintf(outfile,"L77 = %6.2f	REFUND !!!\n", L[77] );
   L[78] = L[77];
   showline(78);
  }
 else
  {
   L[80] = L[62] - L[76];
   fprintf(outfile,"L80 = %6.2f	DUE !!!\n", L[80] );
   fprintf(outfile,"         (Which is %2.1f%% of your total tax.)\n", 100.0 * L[80] / (L[62] + 1e-9) );
  }




 if (Your1stName)
  fprintf(outfile,"Your1stName: %s\n", Your1stName );
 if (YourInitial)
  fprintf(outfile,"YourInitial: %s\n", YourInitial );
 if (YourLastName)
  fprintf(outfile,"YourLastName: %s\n", YourLastName );
 if (YourSocSec)
  fprintf(outfile,"YourSocSec#: %s\n", YourSocSec );
 if (Spouse1stName)
  fprintf(outfile,"Spouse1stName: %s\n", Spouse1stName );
 if (SpouseInitial)
  fprintf(outfile,"SpouseInitial: %s\n", SpouseInitial );
 if (SpouseLastName)
  fprintf(outfile,"SpouseLastName: %s\n", SpouseLastName );
 if (SpouseSocSec)
  fprintf(outfile,"SpouseSocSec#: %s\n", SpouseSocSec );
 if (MailAddress)
  fprintf(outfile,"Number&Street: %s\n", MailAddress );
 if (AptNumber)
  fprintf(outfile,"Apt#: %s\n", AptNumber );
 if (Town[0] != '\0')
  fprintf(outfile,"Town: %s\n", Town );
 if (StateName[0] != '\0')
  fprintf(outfile,"StateName: %s\n", StateName );
 if (Zipcode[0] != '\0')
  fprintf(outfile,"Zipcode: %s\n", Zipcode );

 consume_leading_trailing_whitespace( YourLastName );
 if (strlen( YourLastName ) > 0)
  {
   strcpy( YourNames, YourLastName );
   strcat( YourNames, ", " );
   strcat( YourNames, Your1stName );
   consume_leading_trailing_whitespace( YourInitial );
   if (YourInitial[0] != '\0')
    {
     strcat( YourNames, ", " );
     strcat( YourNames, YourInitial );
    }
   consume_leading_trailing_whitespace( Spouse1stName );
   if (Spouse1stName[0] != '\0')
    {
     strcat( YourNames, ", " );
     if ((SpouseLastName[0] != '\0') && (strcmp( YourLastName, SpouseLastName ) != 0))
      {
       strcat( YourNames, SpouseLastName );
       strcat( YourNames, ", " );
      }
     strcat( YourNames, Spouse1stName );
     if (SpouseInitial[0] != '\0')
      {
       strcat( YourNames, ", " );
       strcat( YourNames, SpouseInitial );
      }
    }
   fprintf(outfile,"YourNames: %s\n", YourNames );
  }

 for (j=1; j <= nDeps; j++)
  {
    fprintf(outfile,"H%d_Name1st: %s\n", j, Dep_info[j].Name1st );
    fprintf(outfile,"H%d_NameLst: %s\n", j, Dep_info[j].NameLst );
    fprintf(outfile,"H%d_SocSec: %s\n", j, Dep_info[j].SocSec );
    fprintf(outfile,"H%d_Relat: %s\n", j, Dep_info[j].Relat );
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
#undef True
#undef False
}
namespace taxsolve_OH_IT1040_2022 {
#define SINGLE 		        1
#define MARRIED_FILING_JOINTLY  2
#define MARRIED_FILING_SEPARAT  3
#define HEAD_OF_HOUSEHOLD       1
/************************************************************************/
/* TaxSolve_OH_IT1040_2022.c - 						*/
/* Copyright (C) 2022 - Aston Roberts					*/
/* 									*/
/* Compile:   gcc taxsolve_OH_IT1040_2022.c -o taxsolve_OH_IT1040_2022	*/
/* Run:	      ./taxsolve_OH_IT1040_2022  OH_IT1040_2022.txt 		*/
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
/* Aston Roberts 1-2-2022	aston_roberts@yahoo.com			*/
/************************************************************************/



double thisversion=20.00;


double TaxRateFunction( double x, int status )
{							/* Updated for 2022. */
 if (x <= 26050.0) return    0.0; else
 if (x <  46100.0) return  360.69 + (x-26050.0)  * 0.02765; else
 if (x <  92150.0) return  915.07 + (x-46100.0)  * 0.03226; else
 if (x < 115300.0) return 2400.64 + (x-92150.0)  * 0.03688; else
 		   return 3254.41 + (x-115300.0) * 0.03990;
}


void Report_bracket_info( double income, double tx, int status )
{							/* Updated for 2022. */
 double rate;
 if (income <= 26050.0) rate = 0.0; else
 if (income <  46100.0) rate = 0.02765; else
 if (income <  92150.0) rate = 0.03226; else
 if (income < 115300.0) rate = 0.03688;
 else 		   	rate = 0.03990;
 printf(" You are in the %2.1f%% marginal tax bracket,\n and you are paying an effective %2.1f%% tax on your total income.\n",
	  100.0 * rate, 100.0 * tx / income );
 fprintf(outfile," You are in the %2.1f%% marginal tax bracket,\n and you are paying an effective %2.1f%% tax on your total income.\n",
	  100.0 * rate, 100.0 * tx / income );
}


char *pull_initial( char *name )
{ /* Expect names like:  "John, D.", and pull initial out. */
  int j=0;
  char midinitial[10];
  while ((name[j] != '\0') && (name[j] != ','))
   j++;
  if (name[j] == ',')
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


/*----------------------------------------------------------------------------*/

int main( int argc, char *argv[] )
{
 int j, k, mm;
 char word[4000], *infname=0, outfname[4000], label[90], *socsec, *pname, *MidInit;
 int status=0, exemptions=0, qualify_jfc=0;
 time_t now;
 double factorA;
 double L2a, L2b, L7a, L8a, L8b, L8c;
 double jfc=0.0, exemption_amnt;
 double SchedA[MAX_LINES], SchedC[MAX_LINES];
 char *DateBeganResidence, *DateEndResidence, *OtherState;

 /* Intercept any command-line arguments. */
 printf("OH IT1040 2022 - v%3.1f\n", thisversion);
 mm = 1;  k=1;
 while (mm < argc)
 {
  if (strcmp(argv[mm],"-verbose")==0)  verbose = 1;
  else
  if (k==1)
   {
    infname = strdup(argv[mm]);
    infile = fopen(argv[mm],"r");
    if (infile==0) {printf("ERROR: Parameter file '%s' could not be opened.\n", argv[mm]);  exit(1);}
    k = 2;
    /* Base name of output file on input file. */
    strcpy(outfname,argv[mm]);
    j = strlen(outfname)-1;
    while ((j>=0) && (outfname[j]!='.')) j--;
    if (j<0) strcat(outfname,"_out.txt"); else strcpy(&(outfname[j]),"_out.txt");
    outfile = fopen(outfname,"w");
    if (outfile==0) {printf("ERROR: Output file '%s' could not be opened.\n", outfname);  exit(1);}
    printf("Writing results to file:  %s\n", outfname);
   }
  else {printf("Unknown command-line parameter '%s'\n", argv[mm]); exit(1);}
  mm++;
 }

 if (infile==0) {printf("Error: No input file on command line.\n");  exit(1);}

 /* Pre-initialize all lines to zeros. */
 for (mm=0; mm<MAX_LINES; mm++) 
  {
   L[mm] = 0.0;
   SchedA[mm] = 0.0;
   SchedC[mm] = 0.0;
  }
 do_all_caps = 1;

 /* Accept parameters from input file. */
 /* Expect  OH IT1040 lines, something like:
	Title:  OH IT1040 1999 Return
	L1		{Wages}
*/

 /* Accept Form's "Title" line, and put out with date-stamp for records. */
 read_line( infile, word );
 now = time(0);
 fprintf(outfile,"\n%s,	 v%2.2f, %s\n", word, thisversion, ctime( &now ));
 check_form_version( word, "Title:  Ohio IT1040 State 2022" );

 /* get_parameter(infile, kind, x, emssg ) */
 get_parameter( infile, 's', word, "Status" );
 get_parameter( infile, 'l', word, "Status ?");
 if (strncasecmp(word,"Single",4)==0) status = SINGLE; else
 if (strncasecmp(word,"Married/Joint",11)==0) status = MARRIED_FILING_JOINTLY; else
 if (strncasecmp(word,"Married/Sep",11)==0) status = MARRIED_FILING_SEPARAT; else
 if (strncasecmp(word,"Head_of_House",4)==0) status = HEAD_OF_HOUSEHOLD;
 else
  { 
   printf("Error: unrecognized status '%s'. Must be: Single, Married/joint, Married/sep, Head_of_house.\nExiting.\n", word); 
   fprintf(outfile,"Error: unrecognized status '%s'. Must be: Single, Married/joint, Married/sep, Head_of_house.\nExiting.\n", word); 
   exit(1); 
  }
 fprintf(outfile,"Status = %s (%d)\n", word, status);

 get_parameter( infile, 's', word, "Exemptions" );	/* Exemptions, self/depend. */
 get_parameters( infile, 'i', &exemptions, "Exemptions"); 

 /* Answer YES only if Married Filing Jointly, and you and your spouse */
 /* each have qualifying Ohio adjusted gross income of at least $500. */
 get_parameter( infile, 's', word, "JointCredit" );
 get_parameter( infile, 'b', &qualify_jfc, "JointCredit ?"); 

 GetLine( "L1", &L[1] );	/* Federal Adjusted Gross Income */
 GetLine( "L4", &L[4] );	/* Personal and dependent exemption deduction, Sched-J */
 GetLine( "L6", &L[6] );	/* Taxable business income (Ohio Schedule IT BUS, line 13) */
 GetLine( "L8b", &L8b );	/* Business income tax liability (Schedule IT BUS, line 14) */
 GetLine( "L11", &L[11] );	/* Interest penalty on underpayment of estimated tax (IT/SD 2210) */
 GetLine( "L12", &L[12] );	/* Sales and use tax due */
 GetLine( "L14", &L[14] );	/* Ohio Tax Withheld (box 17 on your W-2) */
 GetLine( "L15", &L[15] );	/* Estimated and extension payments made */
 GetLine( "L17", &L[17] );	/* Amended return only - amount previously paid with original */
 GetLine( "L19", &L[19] );	/* Amended return only - overpayment previously received on original */

 GetLine( "SchedA_1", &SchedA[1] );	/* Non-Ohio state or local gov't interest and dividends */
 GetLine( "SchedA_2", &SchedA[2] );	/* Ohio pass-through entity and financial institutions taxes paid */
 GetLine( "SchedA_3", &SchedA[3] );	/* Reimbursed college tuit. fees deducted prev yrs. */
 GetLine( "SchedA_4", &SchedA[4] );	/* Losses from sale or disposition of Ohio public obligations */
 GetLine( "SchedA_5", &SchedA[5] );	/* Nonmedical withdrawals from medical savings account */
 GetLine( "SchedA_6", &SchedA[6] );	/* Reimbursement of expenses previously deducted for Ohio income tax ...*/
 GetLine( "SchedA_7", &SchedA[7] );	/* Adjustment for Internal Revenue Code sections 168(k) and 179 */
 GetLine( "SchedA_8", &SchedA[8] );	/* Federal interest and dividends subject to state taxation */
 GetLine( "SchedA_9", &SchedA[9] );	/* Miscellaneous federal income tax additions */

 GetLine( "SchedA_11", &SchedA[11] );	/* Business income deduction (Ohio Schedule IT BUS, line 11) */
 GetLine( "SchedA_12", &SchedA[12] );	/* Compensation earned in Ohio by residents of neighboring states */
 GetLine( "SchedA_13", &SchedA[13] );	/* State/municipal tax overpayments (IRS 1040, Sched 1, line 10) */
 GetLine( "SchedA_14", &SchedA[14] );	/* Taxable Social Security benefits */
 GetLine( "SchedA_15", &SchedA[15] );	/* Certain railroad retirement benefits */
 GetLine( "SchedA_16", &SchedA[16] );	/* Interest income from Ohio public obligations ... */
 GetLine( "SchedA_17", &SchedA[17] );	/* Amounts contributed to an individual development account */
 GetLine( "SchedA_18", &SchedA[18] );	/* Amounts contributed to an STABLE account */
 GetLine( "SchedA_19", &SchedA[19] );	/* Income from out-of-state business */

 GetLine( "SchedA_20", &SchedA[20] );	/* Federal interest and dividends exempt from state taxation */
 GetLine( "SchedA_21", &SchedA[21] );	/* Adjustment for Internal Revenue Code 168(k), 179 depreciation */
 GetLine( "SchedA_22", &SchedA[22] );	/* Refund or reimbursements shown on IRS form 1040, line 21 */
 GetLine( "SchedA_23", &SchedA[23] );	/* Repayment of income reported in a prior year */
 GetLine( "SchedA_24", &SchedA[24] );	/* Wage expense not deducted ... */
 GetLine( "SchedA_25", &SchedA[25] );	/* Miscellaneous federal income tax deductions */

 GetLine( "SchedA_26", &SchedA[26] );	/* Military pay for Ohio residents received while stationed outside Ohio */
 GetLine( "SchedA_27", &SchedA[27] );	/* Income earned by military nonresidents ... */
 GetLine( "SchedA_28", &SchedA[28] );	/* Uniformed services retirement income */
 GetLine( "SchedA_29", &SchedA[29] );	/* Military injury relief fund */
 GetLine( "SchedA_30", &SchedA[30] );	/* Certain Ohio National Guard reimbursements and benefits */

 GetLine( "SchedA_31", &SchedA[21] );	/* Ohio 529 contributions, tuition credit purchases */
 GetLine( "SchedA_32", &SchedA[32] );	/* Pell College Opportunity taxable grant amounts used for room and board */
 GetLine( "SchedA_33", &SchedA[33] );	/* Ohio educator expenses in excess of federal deduction */

 GetLine( "SchedA_34", &SchedA[34] );	/* Disability benefits */
 GetLine( "SchedA_35", &SchedA[35] );	/* Survivorship benefits */
 GetLine( "SchedA_36", &SchedA[36] );	/* Unreimbursed long-term care insurance premiums ... */
 GetLine( "SchedA_37", &SchedA[37] );	/* Funds deposited into, and earnings of, a medical savings account */
 GetLine( "SchedA_38", &SchedA[38] );	/* Qualified organ donor expenses */
 
 /* Schedule of Credits. */
 GetLine( "Credits_2", &SchedC[2] );	/* Retirement income credit */
 SchedC[2] = smallerof( SchedC[2], 200.0 );
 GetLine( "Credits_3", &SchedC[3] );	/* Lump sum retirement credit (Ohio LS WKS, line 6) */
 GetLine( "Credits_4", &SchedC[4] );	/* Senior citizen credit */
 SchedC[4] = smallerof( SchedC[4], 50.0 );
 GetLine( "Credits_5", &SchedC[5] );	/* Lump sum distribution credit */
 GetLine( "Credits_6", &SchedC[6] );	/* Child care and dependent care credit */
 GetLine( "Credits_7", &SchedC[7] );	/* Displaced worker training credit */
 SchedC[7] = smallerof( SchedC[7], 500.0 );
 GetLine( "Credits_8", &SchedC[8] );	/* Campaign contribution credit for Ohio General Assembly */
 if (status == MARRIED_FILING_JOINTLY)
   SchedC[8] = smallerof( SchedC[8], 100.0 );
 else
   SchedC[8] = smallerof( SchedC[8], 50.0 );
 GetLine( "Credits_13", &SchedC[13] );	/* Earned income credit */
 GetLine( "Credits_14", &SchedC[14] );	/* Home school expenses credit. */
 GetLine( "Credits_15", &SchedC[15] );	/* Scholarship donation credit. */
 GetLine( "Credits_16", &SchedC[16] );	/* Nonchartered, nonpublic school tuition credit. */
 GetLine( "Credits_17", &SchedC[17] );	/* Vocational job credit. */
 GetLine( "Credits_18", &SchedC[18] );	/* Ohio adoption credit */
 GetLine( "Credits_19", &SchedC[19] );	/* Job retention credit, nonrefundable portion */
 GetLine( "Credits_20", &SchedC[20] );	/* Credit for eligible new employees in an enterprise zone */
 GetLine( "Credits_21", &SchedC[21] );	/* Credit for purchases of grape production property */
 GetLine( "Credits_22", &SchedC[22] );	/* Invest Ohio credit */
 GetLine( "Credits_23", &SchedC[23] );	/* Lead abatement credit */
 GetLine( "Credits_24", &SchedC[24] );	/* Opportunity zone investment credit */
 GetLine( "Credits_25", &SchedC[25] );	/* Tech investment credit */
 GetLine( "Credits_26", &SchedC[26] );	/* Enterprise zone day care and training credits */
 GetLine( "Credits_27", &SchedC[27] );	/* Research and development credit */
 GetLine( "Credits_28", &SchedC[28] );	/* Nonrefundable Ohio historic preservation credit */

 DateBeganResidence = GetTextLine( "DateBeganResidence:" );
 DateEndResidence = GetTextLine( "DateEndResidence:" );
 OtherState = GetTextLine( "OtherState:" );
 GetLine( "Credits_31", &SchedC[31] );	/* Portion L3 was not earned in Ohio. */

 GetLine( "Credits_34", &SchedC[34] );	/* Resident credit - OH IT RC, line 7 */

 GetLine( "Credits_36", &SchedC[36] );	/* Refundable Historic preservation credit */
 GetLine( "Credits_37", &SchedC[37] );	/* Refundable Business jobs credit */
 GetLine( "Credits_38", &SchedC[38] );	/* Pass-through entity credit */
 GetLine( "Credits_39", &SchedC[39] );	/* Motion picture production credit */
 GetLine( "Credits_40", &SchedC[40] );	/* Venture capital credit */


 /* ---- Do Calculations. ---- */

 for (j=1; j <= 9; j++)
  SchedA[10] = SchedA[10] + SchedA[j];

 for (j=11; j <= 38; j++)
  SchedA[39] = SchedA[39] + SchedA[j];

 L2a = SchedA[10];
 L2b = SchedA[39];
 L[3] = L[1] + L2a - L2b;

 if (L[3] <= 40000.0)			/* Updated for 2022. */
  exemption_amnt = 2400.0;
 else
 if (L[3] <= 80000.0)
  exemption_amnt = 2150.0;
 else
  exemption_amnt = 1900.0;
 L[4] = exemption_amnt * exemptions;

 L[5] = NotLessThanZero( L[3] - L[4] );
 L[7] = NotLessThanZero( L[5] - L[6] );
 L7a = L[7];
 L8a = TaxRateFunction( L7a, status );
 L8c = L8a + L8b;
 SchedC[1] = L8c;

 if (L[5] < 30000.0)
  SchedC[9] = 20.0 * exemptions;	/* Exemption credtit. */
 
 for (j=2; j <= 9; j++)
  SchedC[10] = SchedC[10] + SchedC[j];		

 SchedC[11] = NotLessThanZero( SchedC[1] - SchedC[10] );

 if ((status == MARRIED_FILING_JOINTLY) && (qualify_jfc))
  { /*Joint_Filing_Credit*/
    if (L[5] < 25000) jfc = 0.20;
    else
    if (L[5] < 50000) jfc = 0.15;
    else
    if (L[5] < 75000) jfc = 0.10;
    else jfc = 0.05;
    SchedC[12] = smallerof( jfc * SchedC[11], 650.0 );
  } /*Joint_Filing_Credit*/

 for (j=12; j <= 28; j++)
  SchedC[29] = SchedC[29] + SchedC[j];          
 SchedC[30] = NotLessThanZero( SchedC[11] - SchedC[29] );

 SchedC[32] = L[3];
 j = 10000.0 * SchedC[31] / SchedC[32];
 factorA = (double)j / 10000.0;
 // printf(" %4g\n", factorA );
 if (factorA > 1.0)
  factorA = 1.0;
 SchedC[33] = SchedC[30] * factorA;

 SchedC[35] = SchedC[10] + SchedC[29] + SchedC[33] + SchedC[34];
 L[9] = SchedC[35];
 L[10] = NotLessThanZero( L8c - L[9] );

 for (j=36; j <= 40; j++)
  SchedC[41] = SchedC[41] + SchedC[j];          
 L[16] = SchedC[41];

 L[13] = L[10] + L[11] + L[12];			/* Total Ohio tax liability before withholding or estimated payments. */
 L[18] = L[14] + L[15] + L[16] + L[17];		/* Total Ohio tax payments */
 L[20] = L[18] - L[19];
 if (L[13] >= L[20])
  {
   L[21] = L[13] - L[20];
   L[23] = L[21] + L[22];			/* TOTAL AMOUNT DUE */
  }
 else
  {
   L[24] = L[20] - L[13];			/* Overpayment */
   L[27] = L[24];
  }

 if ((L[1] < 26050.0) && (L[3] < 0.0))		/* Min2File. */
  fprintf(outfile, "You do not need to file Ohio tax return (Fed AGI < minimum).\n");

 if ((L[1] < 26050.0) && (L[4] >= L[3]))
  fprintf(outfile, "You do not need to file Ohio tax return (L[4] >= L[3]).\n");


 /* Output the Results. */

 showline(1);
 showline_wlabel( "L2a", L2a );
 showline_wlabel( "L2b", L2b );
 showline(3);
 showline(4);
 fprintf(outfile," Exemptions = %d\n", exemptions );
 showline(5);
 showline(6);
 showline(7);
 showline_wlabel( "L7a", L[7] );
 showline_wlabel( "L8a", L8a );
 showline_wlabel( "L8b", L8b );
 showline_wlabel( "L8c", L8c );
 for (j = 9; j <= 12; j++)
  showline(j);
 showline_wmsg( 13, "Total Ohio tax liability" );
 Report_bracket_info( L[7], L[13], status );
 showline_wmsg( 14, "Ohio income tax withheld" );
 for (j = 15; j <= 17; j++)
  showline(j);
 showline_wmsg( 18, "Total Ohio tax payments" );
 for (j = 19; j <= 20; j++)
  showline(j);
 if (L[13] >= L[20])
  {
    showline(21);
    showline(22);
    showline_wmsg( 23, "TOTAL AMOUNT DUE !!!" );
    fprintf(outfile,"         (Which is %2.1f%% of your total tax.)\n", 100.0 * L[23] / (L[13] + 1e-9) );
  }
 else
  {
    showline_wmsg( 24, "Overpayment" );
    showline_wmsg( 27, "YOUR REFUND !!!" );
  }

 fprintf(outfile,"\n-- 2022 Ohio Schedule A --\n");
 for (j = 1; j <= 39; j++)
  {
   sprintf( label, "SchedA%d", j );
   showline_wlabel( label, SchedA[j] );
  }

 fprintf(outfile,"\n-- 2022 Ohio Schedule of Credits --\n");
 for (j = 1; j <= 11; j++)
  {
   sprintf( label, "Credits%d", j );
   showline_wlabel( label, SchedC[j] );
  }
 if (jfc > 0.0)
  fprintf(outfile,"JFC = %d\n", (int)(100.0 * jfc + 0.25) );
 for (j = 12; j <= 30; j++)
  {
   sprintf( label, "Credits%d", j );
   showline_wlabel( label, SchedC[j] );
  }

 if (DateBeganResidence[0] != 0)
  fprintf(outfile,"   DateBeganResidence: %s\n", DateBeganResidence );
 if (DateEndResidence[0] != 0)
  fprintf(outfile,"   DateEndResidence: %s\n", DateEndResidence );
 if (OtherState[0] != 0)
  fprintf(outfile,"   OtherState: %s\n", OtherState );

 sprintf(word,"%5.4f", factorA );
printf("factorA = %g, word = '%s'\n", factorA, word );
 fprintf(outfile,"   Credits32a \"%s\"\n", word );

 for (j = 31; j <= 41; j++)
  {
   sprintf( label, "Credits%d", j );
   showline_wlabel( label, SchedC[j] );
  }

 fprintf(outfile,"\n{ --------- }\n");
 pname = GetTextLine( "Your1stName:" );
 MidInit = pull_initial( pname );
 fprintf(outfile,"Your1stName: %s\n", pname );
 fprintf(outfile,"YourMidInit: %s\n", MidInit );
 GetTextLineF( "YourLastName:" );
 writeout_line = 0;
 socsec = GetTextLine( "YourSocSec#:" );
 format_socsec( socsec, 0 );
 fprintf(outfile,"YourSocSec#: %s\n", socsec );
 free( socsec );
 writeout_line = 1;
 pname = GetTextLine( "Spouse1stName:" );
 MidInit = pull_initial( pname );
 fprintf(outfile,"Spouse1stName: %s\n", pname );
 fprintf(outfile,"SpouseMidInit: %s\n", MidInit );
 GetTextLineF( "SpouseLastName:" );
 writeout_line = 0;
 socsec = GetTextLine( "SpouseSocSec#:" );
 format_socsec( socsec, 0 );
 if (status != MARRIED_FILING_SEPARAT)
  fprintf(outfile,"SpouseSocSec#: %s\n", socsec );
 else
  fprintf(outfile,"SpouseSocSec#Sep: %s\n", socsec );
 free( socsec );
 writeout_line = 1;
 GetTextLineF( "Number&Street:" );
 GetTextLineF( "Town:" );
 fprintf(outfile,"State: OH\n");
 GetTextLineF( "Zipcode:" );

 fprintf(outfile,"CkFYrRes: X\n");
 if (status == MARRIED_FILING_JOINTLY)
  fprintf(outfile,"CkFYrResSp: X\n");

 fclose(infile);
 grab_any_pdf_markups( infname, outfile );
 fclose(outfile);
 Display_File( outfname );
 printf("\nResults written to file:  %s\n", outfname);
 return 0;
}

#undef SINGLE
#undef MARRIED_FILING_JOINTLY
#undef MARRIED_FILING_SEPARAT
#undef HEAD_OF_HOUSEHOLD
}
namespace taxsolve_PA_40_2022 {
#define thisversion 20.00
#define SINGLE 		        1
#define MARRIED_FILING_JOINTLY  2
#define MARRIED_FILING_SEPARAT  3
#define WIDOW		        1
#define Yes 1
#define No  0
/************************************************************************/
/* TaxSolve_PA40_2022.c - Pennsylvania 2022 PA-40 State Tax Form.	*/
/* Copyright (C) 2022, - Aston Roberts					*/
/* 									*/
/* Compile:   gcc taxsolve_PA40_2022.c -o taxsolve_PA40_2022		*/
/* Run:	      ./taxsolve_PA40_2022  PA40_2022.txt 			*/
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
/* Aston Roberts 1-2-2022	aston_roberts@yahoo.com			*/
/************************************************************************/




double Tax_Rate = 0.0307;		/* Updated for 2022 tax-year. */


double pos( double x )
{ /* Return only positive amounts, otherwise return zero. */
 if (x > 0.0)
  return x;
 else
  return 0.0;
}


int main( int argc, char *argv[] )
{
 int i, j, k, status=0;
 char word[2500], *infname=0, outfname[2500];
 time_t now;
 double oneA, oneB;
 char *Your1stName=0, *YourLastName=0, *Spouse1stName=0, *SpouseLastName, *YourNames;

 /* Decode any command-line arguments. */
 printf("PA40 - 2022 - v%3.1f\n", thisversion);
 i = 1;  k=1;
 while (i < argc)
 {
  if (strcmp(argv[i],"-verbose")==0)  { verbose = 1; }
  else
  if (k==1)
   {
    infname = strdup(argv[i]);
    infile = fopen(argv[i],"r");
    if (infile==0) {printf("ERROR: Parameter file '%s' could not be opened.\n", argv[i]); exit(1);}
    k = 2;
    /* Base name of output file on input file. */
    strcpy(outfname,argv[i]);
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
 for (i=0; i<MAX_LINES; i++) L[i] = 0.0;

 /* Accept parameters from input file. */
 /* Expect  PA-40 lines, something like:
	Title:  PA 40 1999 Return
	L12	34900.0  {Wages}
 */

 /* Accept Form's "Title" line, and put out with date-stamp for your records. */
 read_line( infile, word );
 now = time(0);
 fprintf(outfile,"\n%s,	 v%2.2f, %s\n", word, thisversion, ctime( &now ));
 check_form_version( word, "Title:  PA State Tax Form PA-40 for 2022" );

 /* get_parameter(infile, kind, x, emssg ) */
 get_parameter( infile, 's', word, "Status" );	/* Single, Married/joint, Married/sep, Widow(er) */
 get_parameter( infile, 'l', word, "Status?");
 if (strncasecmp(word,"Single",4)==0) status = SINGLE; else
 if (strncasecmp(word,"Married/Joint",13)==0) status = MARRIED_FILING_JOINTLY; else
 if (strncasecmp(word,"Married/Sep",11)==0) status = MARRIED_FILING_SEPARAT; else
 if (strncasecmp(word,"Widow",4)==0) status = WIDOW;
 else
  { 
   printf("Error: unrecognized status '%s'. Exiting.\n", word); 
   fprintf(outfile,"Error: unrecognized status '%s'. Exiting.\n", word); 
   exit(1);
  }
 fprintf(outfile,"Status = %s (%d)\n", word, status);
 fprintf(outfile," Check_R_PennResident X\n");

 GetLineF( "L1a", &oneA );	/* Gross compensation. */

 GetLineF( "L1b", &oneB );	/* Unreimbursed employee business expenses. */

 L[1] = oneA - oneB;
 fprintf(outfile,"L1c = %2.2f\n", L[1] );		/* Net compensation. */
 
 GetLineF( "L2", &L[2] );	/* Interest Income. */

 GetLineF( "L3", &L[3] );	/* Dividend Income. */

 GetLine( "L4", &L[4] );	/* Income or loss for business operations. */
 fprintf(outfile,"L4 = %6.2f\n", absolutev( L[4] ) );
 if (L[4] < 0.0)
  fprintf(outfile," Check_4Loss X\n");

 GetLine( "L5", &L[5] );	/* Net gain or loss from disposition of property. */
 fprintf(outfile,"L5 = %6.2f\n", absolutev( L[5] ) );
 if (L[5] < 0.0)
  fprintf(outfile," Check_5Loss X\n");

 GetLine( "L6", &L[6] );	/* Net gain or loss rents, royalties, patents, or copyrights. */
 fprintf(outfile,"L6 = %6.2f\n", absolutev( L[6] ) );
 if (L[6] < 0.0)
  fprintf(outfile," Check_6Loss X\n");

 GetLineF( "L7", &L[7] );	/* Estate or Trust Income. */

 GetLineF( "L8", &L[8] );	/* Gambling or lottery winnings. */

 for (j=1; j<=8; j++) if (L[j] < 0.0) L[j] = 0.0;

 L[9] = pos(L[1]) + pos(L[2]) + pos(L[3]) + pos(L[4]) + pos(L[5]) + pos(L[6]) + pos(L[7]) + pos(L[8]);
 showline_wmsg(9,"Total PA Taxable Income");

 GetLineF( "L10", &L[10] );	/* Other Deductions. */

 L[11] = L[9] - L[10];
 showline_wmsg(11,"Adjusted PA Taxable Income"); /* Adjusted PA income. */

 L[12] = Tax_Rate * L[11];
 showline_wmsg(12,"PA Tax Liability");		/* PA Tax liability. */

 GetLine( "L13", &L[13] );	/* Total PA Tax withheld. */
 showline_wmsg(13,"Total PA tax withheld");

 GetLineF( "L14", &L[14] );	/* Credit from last year's PA income tax return. */

 GetLineF( "L15", &L[15] );	/* 2022 Estimated Installment payments. */

 GetLineF( "L16", &L[16] );	/* 2022 Extension payment. */

 GetLineF( "L17", &L[17] );	/* Non-resident tax withheld. */

 L[18] = L[14] + L[15] + L[16] + L[17];
 showline_wmsg(18,"Total Estimated Payments and Credits");

 GetLine( "L21", &L[21] );	/* Tax Forgiveness Credit from Part D, Line 16, PA Schedule SP. */
 showline_wmsg(21,"Tax Back/Tax Foregiveness Credit");

 GetLineF( "L22", &L[22] );	/* Resident credit (Scheds G/RK-1). */

 GetLineF( "L23", &L[23] );	/* Other credits (Sched OC). */

 L[24] = L[13] + L[18] + L[21] + L[22] + L[23];
 showline_wmsg(24,"Total Payments and Credits");

 GetLineF( "L25", &L[25] );	/* Use Tax. */
 GetLine( "L27", &L[27] );	/* Penalties and interest. */

 if (L[12] + L[25] > L[24])
  {
   L[26] = L[12] + L[25] - L[24];
   showline_wmsg(26,"TAX DUE");
   showline(27);
   L[28] = L[26] + L[27];
   if (L[28] > 0.0)
    {
     showline_wmsg( 28, "Total Payment Due" );
     fprintf(outfile,"         (Which is %2.1f%% of your total tax.)\n", 100.0 * L[28] / (L[12] + L[25] + 1e-9) );
    }
  }
 else
 if (L[24] > L[12] + L[25] + L[27])
  {
   showline(27);
   L[29] = L[24] - (L[12] + L[25] + L[27]);
   showline_wmsg(29,"OVERPAYMENT");
   L[30] = L[29];
   showline_wmsg(30,"REFUND");
  }
 
 fprintf(outfile,"\n{ --------- }\n");
 do_all_caps = 1;
 Your1stName = GetTextLineF( "Your1stName:" );
 GetTextLineF( "MidInitial:" );
 YourLastName = GetTextLineF( "YourLastName:" );
 GetTextLineF( "YourSocSec#:" );
 Spouse1stName = GetTextLineF( "Spouse1stName:" );
 GetTextLineF( "SpouseMidInit:" );
 SpouseLastName = GetTextLineF( "SpouseLastName:" );
 GetTextLineF( "SpouseSocSec#:" );
 GetTextLineF( "Number&Street:" );
 GetTextLineF( "Town:" );
 fprintf(outfile,"State: PA\n");
 GetTextLineF( "Zipcode:" );
 GetTextLineF( "Phone:" );
 GetTextLineF( "SchoolCode:" );
 GetTextLineF( "SchooldDist:" );
 GetTextLineF( "YourOccupation:" );
 GetTextLineF( "SpouseOccupat:" );
 if (YourLastName[0] != '\0')
  {
   if (status == MARRIED_FILING_JOINTLY)
    {
     YourNames = (char *)malloc( strlen(YourLastName) + strlen( Your1stName ) + 
				  strlen( SpouseLastName ) + strlen( Spouse1stName ) + 20 );
     strcpy( YourNames, Your1stName );
     if (strcmp( YourLastName, SpouseLastName ) == 0)
      { /* Common last name */
        strcat( YourNames, " & " );
	strcat( YourNames, Spouse1stName );
	strcat( YourNames, ", " );
	strcat( YourNames, YourLastName);
      }
     else
      {
        strcat( YourNames, " " );
	strcat( YourNames, YourLastName);
	strcat( YourNames, ", " );
	strcat( YourNames, Spouse1stName );
        strcat( YourNames, " " );
        strcat( YourNames, SpouseLastName);
      }
    }
   else
    {
     YourNames = (char *)malloc( strlen(YourLastName) + strlen( Your1stName ) + 10 );
     strcpy( YourNames, Your1stName );
     strcat( YourNames, ", " );
     strcat( YourNames, YourLastName );
    }
   fprintf(outfile,"YourNames: %s\n", YourNames );
  }
 fclose(infile);
 grab_any_pdf_markups( infname, outfile );
 fclose(outfile);
 printf("\nListing results from file: %s\n\n", outfname);
 Display_File( outfname );
 return 0;
}

#undef thisversion
#undef SINGLE
#undef MARRIED_FILING_JOINTLY
#undef MARRIED_FILING_SEPARAT
#undef WIDOW
#undef Yes
#undef No
}
namespace taxsolve_US_1040_2022 {
#define CAP_GAIN_ADJUSTMENT_CODES  "BTNHDQXRWLESCMOZY"   	/* Form 8949 Instructions */
#define MAXADJERRCNT 25     /* Max number of adj_code errors to print to terminal */ 
#define SINGLE 		        1
#define MARRIED_FILING_JOINTLY  2
#define MARRIED_FILING_SEPARAT  3
#define HEAD_OF_HOUSEHOLD       4
#define WIDOW		        5
#define Yes 1
#define No  0
/************************************************************************/
/* TaxSolve_usa_fed1040_2022.c - OpenTaxSolver for USFed1040 		*/
/* Copyright (C) 2022 - Aston Roberts					*/
/* 									*/
/* Tax Solver for US Fedral 1040 Income Tax return for 2022 Tax Year.	*/
/* 									*/
/* OTS Project Home Page and Updates:  					*/
/*		http://opentaxsolver.sourceforge.com/			*/
/* 									*/
/* Compile:   cc taxsolve_US_1040_2022.c -o taxsolve_US_1040_2022       */
/* Run:       ./taxsolve_US_1040_2022  Fed1040_2022.txt                 */
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

float thisversion=20.02;




double SchedA[MAX_LINES], SchedD[MAX_LINES], amtws[MAX_LINES];
double Sched1[MAX_LINES], Sched2[MAX_LINES], Sched3[MAX_LINES];
double Sched3_13a=0.0, Sched3_13b=0.0, Sched3_13d=0.0, Sched3_13e=0.0,
	Sched3_13f=0.0, Sched3_13h=0.0, Sched3_13z=0.0;
double L1a=0.0, L1b=0.0, L1c=0.0, L1d=0.0, L1e=0.0, L1f=0.0, L1g=0.0, L1h=0.0, L1i=0.0;
double L2a=0.0;			/* Tax-exempt interest (only for SocSec calculations). */
double L3a=0.0;			/* Qualified dividends. */
double L4a=0.0;			/* IRA distributions */
double L5a=0.0;			/* Pensions, and annuities. */
double L6a=0.0;			/* Social security benefits. */
double L25a=0.0, L25b=0.0, L25c=0.0;
double L27a=0.0, L27b=0.0, L27c=0.0;
double S4_60b=0.0;		/* First-time homebuyer credit repayment. Form 5405. */
double qcgws[MAX_LINES];	/* Support for AMT calculation. (qual.div+cap.gain wrksht vals.)*/
double amtws2c=0.0;		/* Investment interest expense (difference between regular tax and AMT) - AMT entry */
double amtws2g=0.0;		/* Specified private activity bond interest exempt from regular tax - AMT entry */
int Do_SchedD=No, Do_QDCGTW=No, Do_SDTW=No;
int status, under65=Yes, over65=No, dependent=No, force_print_all_pdf_forms=0;
int ForceItemize=0;
double localtax[10], loctaxlimit, homemort[10];
double  collectibles_gains=0.0, ws_sched_D[MAX_LINES];

char adj_code_err[MAXADJERRCNT][1024];
int adjerrcnt=0;

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


void Report_bracket_info( double income, double actual_tax, int status )  
{
  int  bracket=0;
  if (status == WIDOW) status = MARRIED_FILING_JOINTLY;  /* Handle case of widow(er). */
  status = status - 1;  /* Arrays start at zero; not one. */
  while (brkpt[status][bracket+1] < income) bracket++;
  printf(" You are in the %2.1f%% marginal tax bracket,\n and you are paying an effective %2.1f%% tax on your income.\n",
          100.0 * txrt[status][bracket], 100.0 * (actual_tax) / (income + 1e-9) );
  fprintf(outfile," You are in the %2.1f%% marginal tax bracket,\n and you are paying an effective %2.1f%% tax on your income.\n",
          100.0 * txrt[status][bracket], 100.0 * (actual_tax) / (income + 1e-9) );
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



void showschedA( int linenum )
{ if (SchedA[linenum] > 0.0) fprintf(outfile," A%d = %6.2f\n", linenum, SchedA[linenum] ); }

void showschedA_wMsg( int linenum, char *msg )
{ if (SchedA[linenum] > 0.0) fprintf(outfile," A%d = %6.2f	%s\n", linenum, SchedA[linenum], msg ); }

void print2( char *msg )
{
 printf("%s", msg );
 fprintf(outfile, "%s", msg );
}


/*-----------------------------------------------------------------*/
/* Qualified Dividends and Capital Gain Tax Worksheet for Line 16. */
/*  From page 36 of instructions.				   */
/*-----------------------------------------------------------------*/
void capgains_qualdividends_worksheets( int status )			/* Updated for 2022. */
{
 int j;

 qcgws[1] = L[15];
 qcgws[2] = L3a;
 if (Do_SchedD)
  qcgws[3] = NotLessThanZero(smallerof( SchedD[15], SchedD[16] ));
 else
  qcgws[3] = Sched1[13];
 qcgws[4] = qcgws[2] + qcgws[3];
 qcgws[5] = NotLessThanZero( qcgws[1] - qcgws[4] );
 switch (status)
  {
   case SINGLE: case MARRIED_FILING_SEPARAT: qcgws[6] = 41675.0; break;
   case MARRIED_FILING_JOINTLY: case WIDOW:  qcgws[6] = 83350.0; break;
   case HEAD_OF_HOUSEHOLD: 		      qcgws[6] = 55800.0; break;
  }
 qcgws[7] = smallerof( qcgws[1], qcgws[6] );
 qcgws[8]  = smallerof( qcgws[5], qcgws[7] );
 qcgws[9] = qcgws[7] - qcgws[8];				// This amount is taxed at 0%
 qcgws[10] = smallerof( qcgws[1], qcgws[4] );
 qcgws[11] = qcgws[9];
 qcgws[12] = qcgws[10] - qcgws[11];
 switch (status)
  {
   case SINGLE:  			      qcgws[13] = 459750.0;  break;
   case MARRIED_FILING_SEPARAT:	      	      qcgws[13] = 258600.0;  break;
   case MARRIED_FILING_JOINTLY: case WIDOW:   qcgws[13] = 517200.0;  break;
   case HEAD_OF_HOUSEHOLD: 		      qcgws[13] = 488500.0;  break;
  }
 qcgws[14] = smallerof( qcgws[1], qcgws[13] );
 qcgws[15] = qcgws[5] + qcgws[9];
 qcgws[16] = NotLessThanZero( qcgws[14] - qcgws[15] );
 qcgws[17] = smallerof( qcgws[12], qcgws[16] );
 qcgws[18] = 0.15 * qcgws[17];
 qcgws[19] = qcgws[9] + qcgws[17];
 qcgws[20] = qcgws[10] - qcgws[19];
 qcgws[21] = 0.20 * qcgws[20];
 qcgws[22] = TaxRateFunction( qcgws[5], status );
 qcgws[23] = qcgws[18] + qcgws[21] + qcgws[22];
 qcgws[24] = TaxRateFunction( qcgws[1], status );
 qcgws[25] = smallerof( qcgws[23], qcgws[24] );
 for (j = 1; j <= 25; j++)
  {
   printf("	Qual. Div & Gains WorkSheet %d:  %8.2f\n", j, qcgws[j] );
   if (j == 3) { if (Do_SchedD) fprintf(outfile,"\t\t3: Check Yes.\n"); else fprintf(outfile,"\t\t3: Check No.\n"); }
   fprintf(outfile,"	Qual. Div & Gains WorkSheet %d:  %8.2f\n", j, qcgws[j] );
  }
 L[16] = Conditional_Round( qcgws[25] );
}




/*----------------------------------------------------------------------------------------------*/
/* Form-6251 - Alternative Minimum Tax (AMT) form detailed calculations. 			*/
/* This routine establishes the framework for the 6251 form, for the limited few who need it. 	*/
/* Form 6251 asks many highly specialized questions, which are assumed zero for most filers. 	*/
/* Those who should make the additional entries will no-doubt know who they are, and can 	*/
/* simply add them to this section.  The balance of the routine will be helpful in either case. */
/* --- Anyone indicated to fill-out Form 6251 should review the 6251 instruction booklet. ---	*/ 
/*----------------------------------------------------------------------------------------------*/
double form6251_AlternativeMinimumTax( int itemized )						/* Updated for 2022. */
{
 double thresholdA=0.0, thresholdB=0.0, thresholdC=0.0, amtexmption=0.0;
 double offsetA=0.0;
 double amtws2a=0.0, amtws2b=0.0, amtws2e=0.0;
 int j, file_amt=1;

 printf("Review AMT form6251 routine for your situation.\n");
 fprintf(outfile,"Review AMT form6251 routine for your situation.\n");

 /* Part I - Alternative Minimum Taxable Income (AMTI) */
 if (L[15] > 0.0)  
  amtws[1] = L[15];
 else
  amtws[1] = L[11] - L[14];

 if (itemized)
  amtws2a = SchedA[7];
 else
  amtws2a = L[12];

 amtws2b = -(Sched1[1] + Sched1[8]);

	/* Following amounts assumed normally zero, but review and adjust if needed. */
 // amtws2c = 0.0;	/* Investment interest expense. (Diff between regular tax and AMT). */
 // amtws2d = 0.0;	/* Depletion (Diff between regular tax and AMT). */
 amtws2e = absolutev( Sched1[8] );
 // amtws2f = -0.0; 	/* Alternative tax net operating loss deduction, as negative amount. */
 // amtws2g = 0.0;	/* Interest from specified private activity bonds exempt from the regular tax */
 // amtws2h = 0.0;	/* Qualified small business stock (7% of gain excluded under section 1202) */
 // amtws2i = 0.0;	/* Exercise incentive stock options (excess of AMT income over reg tax income) */
 // amtws2j = 0.0;	/* Estates and trusts (amount from Schedule K-1 (Form 1041), box 12, code A) */
 // amtws2k = 0.0;	/* Disposition of property (difference between AMT and regular tax gain or loss) */
 // amtws2l = 0.0;	/* Deprec assets placed in service after 1986 (diff between regular tax and AMT) */
 // amtws2m = 0.0;	/* Passive activities (difference between AMT and regular tax income or loss) */
 // amtws2n = 0.0;	/* Loss limitations (difference between AMT and regular tax income or loss) */
 // amtws2o = 0.0;	/* Circulation costs (difference between regular tax and AMT) */
 // amtws2p = 0.0;	/* Long-term contracts (difference between AMT and regular tax income) */
 // amtws2q = 0.0;	/* Mining costs (difference between regular tax and AMT) */
 // amtws2r = 0.0;	/* Research and experimental costs (difference between regular tax and AMT) */
 // amtws2s = -0.0;	/* Income from certain installment sales before 1/1/87 (As negaitive amount.) */
 // amtws2t = 0.0;	/* Intangible drilling costs preference */

 amtws[2] = amtws2a + amtws2b + amtws2c + amtws2e + amtws2g;

 for (j = 1; j <= 3; j++)
  amtws[4] = amtws[4] + amtws[j];

 if ((status == MARRIED_FILING_SEPARAT) && (amtws[4] > 776100.0))
  {
   if (amtws[4] > 1012300.0)
    amtws[4] = amtws[4] + 59050.0;
   else
    amtws[4] = amtws[4] + 0.25 * (amtws[4] - 776100.0);
  }

 /* Part II */
 switch (status)
  {
     case SINGLE: case HEAD_OF_HOUSEHOLD:
	thresholdA = 539900.0;
	thresholdB = 843500.0;
	thresholdC = 206100;
	offsetA = 4122.0;
	amtexmption = 75900.0;
	break;
     case MARRIED_FILING_JOINTLY: case WIDOW: 
	thresholdA = 1079800.0;
	thresholdB = 1552200.0;
	thresholdC = 206100.0;
	offsetA = 4122.0;
	amtexmption = 118100.0;
	break;
     case MARRIED_FILING_SEPARAT: 
	thresholdA = 539900.0;
	thresholdB = 776100.0;
	thresholdC = 103050.0;
	offsetA = 2061.0;
        amtexmption = 59050.0;
	break;
     default:  printf("Status %d not handled.\n", status);  exit(1); 
  }

 if (amtws[4] > thresholdA)
  { /* Exemption Worksheet - page 9. */
    double ews[20];
   if (amtws[4] >= thresholdB)
    amtexmption = 0.0;
   else
    {
     ews[1] = amtexmption;
     ews[2] = amtws[4];
     ews[3] = thresholdA;
     ews[4] = NotLessThanZero( ews[2] - ews[3] );
     ews[5] = 0.25 * ews[4];
     ews[6] = NotLessThanZero( ews[1] - ews[5] );
     amtexmption = ews[6];
     /* Does not handle "Certain Children Under Age 24". */
    }
  }

 amtws[5] = amtexmption;
 amtws[6] = NotLessThanZero( amtws[4] - amtws[5] );
 if (amtws[6] > 0.0)
  { /* AMT Lines 7 through 9, */

    if ((L[7] != 0.0) || (L3a != 0.0) || ((SchedD[15] > 0.0) && (SchedD[16] > 0.0)))
     { /* Part III */
       amtws[12] = amtws[6];
       amtws[13] = largerof( qcgws[4], ws_sched_D[13] );
       amtws[14] = SchedD[19];
       if (Do_SDTW)
        amtws[15] = smallerof( amtws[13] + amtws[14], ws_sched_D[10] );
       else
        amtws[15] = amtws[13];
       amtws[16] = smallerof( amtws[12], amtws[15] );
       amtws[17] = amtws[12] - amtws[16];
       if (amtws[17] <= thresholdC)
        amtws[18] = 0.26 * amtws[17];
       else
        amtws[18] = 0.28 * amtws[17] - offsetA;
       switch (status)
        {
           case MARRIED_FILING_JOINTLY:  case WIDOW: 
	     amtws[19] = 83350.0;
	   break;
           case SINGLE:  case MARRIED_FILING_SEPARAT: 
   	     amtws[19] = 41675.0;
   	   break;
           case HEAD_OF_HOUSEHOLD:
   	     amtws[19] = 55800.0;
        }
       if (Do_QDCGTW)
        amtws[20] = NotLessThanZero( qcgws[5] );
       else
       if (Do_SDTW)
	amtws[20] = NotLessThanZero( ws_sched_D[14] );
       else
	amtws[20] = NotLessThanZero( L[15] );
       amtws[21] = NotLessThanZero( amtws[19] - amtws[20] );
       amtws[22] = smallerof( amtws[12], amtws[13] );
       amtws[23] = smallerof( amtws[21], amtws[22] );
       amtws[24] = amtws[22] - amtws[23];  
       switch (status)
	{
	   case SINGLE:  			      amtws[25] = 459750.0;  break;
	   case MARRIED_FILING_SEPARAT:	      	      amtws[25] = 258600.0;  break;
	   case MARRIED_FILING_JOINTLY: case WIDOW:   amtws[25] = 517200.0;  break;
	   case HEAD_OF_HOUSEHOLD: 		      amtws[25] = 488500.0;  break;
	   default:  printf("Status %d not handled.\n", status);  exit(1); 
	}
       amtws[26] = amtws[21];
       if (Do_QDCGTW)
	amtws[27] = NotLessThanZero( qcgws[5] );
       else
       if (Do_SDTW)
	amtws[27] = NotLessThanZero( ws_sched_D[21] );
       else
	amtws[27] = NotLessThanZero( L[15] );
       amtws[28] = amtws[26] + amtws[27];
       amtws[29] = NotLessThanZero( amtws[25] - amtws[28] );
       amtws[30] = smallerof( amtws[24], amtws[29] );
       amtws[31] = 0.15 * amtws[30];
       amtws[32] = amtws[23] + amtws[30];
       if (absolutev( amtws[12] - amtws[32] ) > 0.005)
	{ /*lines 33-37*/
	  amtws[33] = amtws[22] - amtws[32];
	  amtws[34] = 0.20 * amtws[33];
	  if (amtws[35] != 0.0)
	   { /*lines 35-37*/
	    amtws[35] = amtws[17] + amtws[32] + amtws[33];
	    amtws[36] = amtws[12] - amtws[35];
	    amtws[37] = 0.25 * amtws[36];
	   } /*lines 35-37*/
	} /*lines 33-37*/
       amtws[38] = amtws[18] + amtws[31] + amtws[34] + amtws[37];
       if (amtws[12] <= thresholdC)
        amtws[39] = 0.26 * amtws[12];
       else
        amtws[39] = 0.28 * amtws[12] - offsetA;
       amtws[40] = smallerof( amtws[38], amtws[39] );
       amtws[7] = amtws[40];
     } /* Part III */
    else
     {
      if (amtws[6] <= thresholdC)
       amtws[7] = 0.26 * amtws[6];
      else
       amtws[7] = 0.28 * amtws[6] - offsetA;
     }
    amtws[9] = amtws[7] - amtws[8];
  } 
 amtws[10] = L[16] + Sched2[2] - Sched3[1];
 if (amtws[6] > 0.0) 
  amtws[11] = NotLessThanZero( amtws[9] - amtws[10] );
 printf("	AMTws[11] = Abs( %6.2f - %6.2f ) = Abs( %6.2f )\n", amtws[9], amtws[10], amtws[9] - amtws[10] );
 // Sched2[1] = amtws[11];	/* Redundant.  Is assigned by return value below. */

 /* These rules are stated on Form-6251 Instructions page-1. */
 if (amtws[7] > amtws[10])
  {
   file_amt = Yes;
   fprintf(outfile,"You MUST file AMT form 6251. (%g > %g)\n", amtws[7], amtws[10] );
  }
 else
  {
   if (amtws2c + amtws2e + amtws2g + amtws[3] < 0.0)
    {
     file_amt = Yes;
     fprintf(outfile,"You may need to file AMT form 6251.  (sum(AMTws2c : AMTws3) = %g < 0.\n", 
			amtws2c + amtws2e + amtws2g + amtws[3] );
     fprintf(outfile," (See \"Who Must File\" on page-1 of Instructions for Form-6251.)\n");
    }
   else
    file_amt = No;
  }
 if (force_print_all_pdf_forms) 
  file_amt = 1;
 if (file_amt)
  fprintf(outfile,"PDFpage: 16 16\n");	/* Optional PDF Page. */
 for (j=0; j<100; j++) 
  {
   if (j == 2)
    {
     char tmplabel[1024];
     sprintf( tmplabel, " 		AMT_Form_6251_L2a");
     showline_wlabelnz( tmplabel, amtws2a );
     sprintf( tmplabel, " 		AMT_Form_6251_L2b");
     showline_wlabelnz( tmplabel, amtws2b );
     sprintf( tmplabel, " 		AMT_Form_6251_L2c");
     showline_wlabelnz( tmplabel, amtws2c );
     sprintf( tmplabel, " 		AMT_Form_6251_L2e");
     showline_wlabelnz( tmplabel, amtws2e );
     sprintf( tmplabel, " 		AMT_Form_6251_L2g");
     showline_wlabelnz( tmplabel, amtws2g );
    }
   if ((j == 11) || (amtws[j] != 0.0))
    {
     printf(" 		AMT Form 6251 L%d = %8.2f\n", j, amtws[j] );
     fprintf(outfile," 		AMT_Form_6251_L%d = %8.2f\n", j, amtws[j] );
    }
   if (file_amt && (j == 11))
    fprintf(outfile,"EndPDFpage.\nPDFpage: 17 17\n");
  }
 if (file_amt)
  fprintf(outfile,"EndPDFpage.\n");
 fprintf(outfile,"	AMTws[11] = OnlyIfMoreThanZero( %6.2f - %6.2f ) = %6.2f\n", amtws[9], amtws[10], amtws[11] );
 fprintf(outfile,"Your Alternative Minimum Tax = %8.2f\n", amtws[11] ); 
 printf("Your Alternative Minimum Tax = %8.2f\n", amtws[11] ); 
 return amtws[11];
}





struct FedReturnData
 {
  double fedline[MAX_LINES], schedD[MAX_LINES];
  int Exception, Itemized;
 } LastYearsReturn;


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


void ImportFederalReturnData( char *fedlogfile, struct FedReturnData *fed_data )
{
 FILE *infile;
 char fline[1000], word[1000];
 int linenum;

 for (linenum=0; linenum<MAX_LINES; linenum++) 
  { fed_data->fedline[linenum] = 0.0;  fed_data->schedD[linenum] = 0.0; }
 convert_slashes( fedlogfile );
 printf("Opening LastYearsFedReturn: '%s'\n", fedlogfile );
 infile = fopen(fedlogfile, "r");
 if (infile==0)
  {
   printf("Error: Could not open federal return '%s'\n", fedlogfile);
   fprintf(outfile,"Error: Could not open federal return '%s'\n", fedlogfile);
   exit(1);
  }
 printf("Importing Last Year's Federal Return Data from file '%s'\n", fedlogfile );
 fed_data->Itemized = 1; /* Set initial default values. */
 read_line(infile,fline);  linenum = 0;
 while (!feof(infile))
  {
   if (strstr(fline,"Use standard deduction.")!=0) fed_data->Itemized = 0;
   next_word(fline, word, " \t=");
   if ((strstr(word,"L")==word) && (strstr(fline," = ")!=0))
    {
     if (sscanf(&word[1],"%d",&linenum)!=1) printf("Error: Reading fed line number '%s%s'\n",word,fline);
     next_word(fline, word, " \t=");	remove_certain_chars( word, "," );
     if (sscanf(word,"%lf", &fed_data->fedline[linenum])!=1)
	printf("Error: Reading fed line %d '%s%s'\n",linenum,word,fline);
     if (verbose) printf("FedLin[%d] = %2.2f\n", linenum, fed_data->fedline[linenum]);
    }
   if ((strstr(word,"D") == word) && (strstr(fline," = ") != 0)) 
    {
     if (sscanf(&word[1],"%d",&linenum)!=1) printf("Error: Reading fed line number '%s%s'\n",word,fline);
     next_word(fline, word, " \t=");	remove_certain_chars( word, "," );
     if (sscanf(word,"%lf", &fed_data->schedD[linenum]) != 1) 
      {
       if (strcasecmp(word,"yes") == 0) fed_data->schedD[linenum] = 1;
       else
       if (strcasecmp(word,"no") == 0) fed_data->schedD[linenum] = 0;
       else
	printf("Error: Reading fed schedD %d '%s%s'\n",linenum,word,fline);
      }
     if (verbose) printf("FedLin[%d] = %2.2f\n", linenum, fed_data->schedD[linenum]);
    }
   read_line(infile,fline);
  }
 fclose(infile);
}


void CapitalLossCarryOverWorksheet( char *fedlogfile, struct FedReturnData *LastYearsReturn )	/* Updated for 2022. */
{ /* From instructions page D-11. */
 double ws[50];
 int k;

 for (k=0; k < 50; k++)		/* First, pre-initialize worksheet lines to zero. */
  ws[k] = 0.0;

 ImportFederalReturnData( fedlogfile, LastYearsReturn );
 if (LastYearsReturn->schedD[21] == 0.0) 
  {
   printf(" No carry-over loss.\n");
   fprintf(outfile," No carry-over loss.\n");
   return;  /* Use this worksheet only if last year's D[21] was a loss. */
  }
 if ((absolutev(LastYearsReturn->schedD[21]) >= absolutev(LastYearsReturn->schedD[16])) && (LastYearsReturn->fedline[15] >= 0.0))
  {
   printf(" No carry-over loss.\n");
   fprintf(outfile," No carry-over loss.\n");
   return;
  }

 for (k=0; k<50; k++) ws[k] = 0.0;
 ws[1] = LastYearsReturn->fedline[15];
 ws[2] = absolutev( LastYearsReturn->schedD[21] );	/* Loss from last year's Sched-D21 as positive amount. */
 ws[3] = NotLessThanZero( ws[1] + ws[2] );
 ws[4] = smallerof( ws[2], ws[3] );
 for (k=1; k<=4; k++)
  {
   printf("\tCarryOverWs%d = %2.2f\n", k, ws[k] );
   fprintf(outfile,"\tCarryOverWs%d = %2.2f\n", k, ws[k] );
  }
 if (LastYearsReturn->schedD[7] < 0.0)
  { /*lines5-8*/
    ws[5] = -LastYearsReturn->schedD[7];
    ws[6] = NotLessThanZero( LastYearsReturn->schedD[15] );
    ws[7] = ws[4] + ws[6];
    ws[8] = NotLessThanZero( ws[5] - ws[7] );
    if (ws[8] > 0.0)
     SchedD[6] = ws[8];
    for (k=5; k<=8; k++)
     {
	printf("\tCarryOverWs%d = %2.2f\n", k, ws[k] );
	fprintf(outfile,"\tCarryOverWs%d = %2.2f\n", k, ws[k] );
     }
  } /*lines5-8*/
 else
  printf("\t(Skip CarryOverWs lines 5-8.)\n");

 if (LastYearsReturn->schedD[15] < 0.0)
  { /*lines9-13*/
    ws[9] = absolutev( LastYearsReturn->schedD[15] );
    ws[10] = NotLessThanZero( LastYearsReturn->schedD[7] );
    ws[11] = NotLessThanZero( ws[4] - ws[5] );
    ws[12] = ws[10] + ws[11];
    ws[13] = NotLessThanZero( ws[9] - ws[12] );
    if (ws[13] > 0.0)
     SchedD[14] = ws[13];
    for (k=9; k<=13; k++)
     {
	printf("\tCarryOverWs%d = %2.2f\n", k, ws[k] );
	fprintf(outfile,"\tCarryOverWs%d = %2.2f\n", k, ws[k] );
     }
  } /*lines9-13*/
 else
  printf("\t(Skip CarryOverWorkSheet lines 9-13.)\n");
}



struct capgain_record
 {
  char *comment, *buy_date, *sell_date, *adj_code;
  double buy_amnt, sell_amnt, adj_amnt;
  struct capgain_record *nxt;
 } *short_trades=0, *long_trades=0;

double total_sales, total_costs=0.0, total_adjs;


void new_capgain( struct capgain_record **list, char *comment, double buy_amnt, 
		char *buy_date, double sell_amnt, char *sell_date, char *adj_code, double adj_amnt )
{ /* Add a new entry to a list. */
  struct capgain_record *new_item, *prev;

  new_item = (struct capgain_record *)malloc( sizeof(struct capgain_record) );
  new_item->comment = strdup( comment );	/* Make new list item and fill-in its fields. */
  if (strlen( new_item->comment ) > 31)
   new_item->comment[31] = '\0'; /* Limit comment length to avoid over-running column boundary. */
  new_item->buy_amnt = buy_amnt;
  new_item->buy_date = strdup( buy_date );
  new_item->sell_amnt = sell_amnt;
  new_item->sell_date = strdup( sell_date );
  new_item->adj_code = strdup( adj_code );
  new_item->adj_amnt = adj_amnt;
  new_item->nxt = 0;
  prev = *list;		/* Insert onto end of list. */
  if (prev == 0)
   *list = new_item;
  else
   {
    while (prev->nxt != 0) prev = prev->nxt;
    prev->nxt = new_item;
   }
}


void print_capgain_list( struct capgain_record *list, int section, char *message, char *pdfmsg )
{
 struct capgain_record *item;
 char word[4096], row='a';
 char pdf_adj_code[20];

 /* First write results in easily human-readable format. */
 total_sales = 0.0;
 total_costs = 0.0;
 total_adjs = 0.0;
 fprintf(outfile,"\n%s\n", message );
 fprintf(outfile," %d. (a Description)         (b Buy Date) (c Date Sold) (d Sold Price)   (e Cost)   (f Code)        (g Adj)       (h Gain)\n", section );
 fprintf(outfile," ------------------------------------------------------------------------------------------------------------------------\n");
 item = list;
 while (item != 0)
  {
   strcpy( word, item->comment );
   if (strlen( word ) > 27) word[30] = '\0';
   if ((strlen(word) > 0) && (word[ strlen(word) - 1 ] == '}')) word[ strlen(word) - 1 ] = '\0';
   while (strlen( word ) < 27) strcat( word, " " ); 	/* Fields become formatted right-justified. */
   fprintf(outfile," %s %10s %10s %14.2f %14.2f %10s %14.2f %14.2f\n", word, item->buy_date, item->sell_date, item->sell_amnt, 
	absolutev(item->buy_amnt), item->adj_code, item->adj_amnt, item->sell_amnt + item->buy_amnt + item->adj_amnt );
   total_sales = total_sales + item->sell_amnt;
   total_costs = total_costs + item->buy_amnt;
   total_adjs = total_adjs + item->adj_amnt;
   item = item->nxt;
  }
 fprintf(outfile," ------------------------------------------------------------------------------------------------------------------------\n");
 fprintf(outfile," %d. Totals:                                        %14.2f %14.2f            %14.2f %14.2f\n\n", 
	section + 1, total_sales, absolutev(total_costs), total_adjs, total_sales + total_costs + total_adjs );

 /* Now re-list them for update by the PDF-Convertor. */
 fprintf(outfile,"PDFpage: %s\n", pdfmsg );	/* Optional PDF page. */
 item = list;
 while (item != 0)
  {
   if (row > 'n')
    { /* All form-entries filled, go to new form-page. */
     fprintf(outfile," F8949_2d = ...\n");
     fprintf(outfile," F8949_2e = ...\n");
     fprintf(outfile," F8949_2h = ...\n");
     fprintf(outfile,"EndPDFpage.\nPDFpage:  %s\n", pdfmsg );	/* Overflow page. */
     row = 'a';
    }
   fprintf(outfile," F8949_1%ca: %s\n", row, item->comment );
   fprintf(outfile," F8949_1%cb: %s\n", row, item->buy_date );
   fprintf(outfile," F8949_1%cc: %s\n", row, item->sell_date );
   fprintf(outfile," F8949_1%cd = %14.2f\n", row, item->sell_amnt );
   fprintf(outfile," F8949_1%ce = %14.2f\n", row, absolutev(item->buy_amnt) );
   if (strcmp(item->adj_code, "~") == 0 )  /* if match */ 
    strcpy(pdf_adj_code, " ");             /* Convert "~"  to " " for PDF form */
   else
   {
    strcpy(pdf_adj_code, item->adj_code);
   }
   fprintf(outfile," F8949_1%cf = %3s\n", row, pdf_adj_code );
   if (item->adj_amnt != 0.0)
    fprintf(outfile," F8949_1%cg = %14.2f\n", row, item->adj_amnt );
   fprintf(outfile," F8949_1%ch = %14.2f\n", row, item->sell_amnt + item->buy_amnt + item->adj_amnt);
   row++;
   item = item->nxt;
  }
 fprintf(outfile," F8949_2d = %14.2f\n", total_sales );
 fprintf(outfile," F8949_2e = %14.2f\n", absolutev(total_costs) );
 fprintf(outfile," F8949_2g = %14.2f\n", total_adjs );
 fprintf(outfile," F8949_2h = %14.2f\n", total_sales + total_costs + total_adjs );
 fprintf(outfile,"EndPDFpage.\n\n");
}


void free_capgain_list( struct capgain_record **list )
{
 struct capgain_record *olditem;

 while (*list != 0)
  {
   olditem = *list;
   *list = (*list)->nxt;
   free( olditem->comment );
   free( olditem );
  }
}


int is_date1_beyond_date2 (struct date_rec date1, struct date_rec date2)
{
 if (  (date1.year > date2.year)
   || ((date1.year == date2.year) && (date1.month > date2.month))
   || ((date1.year == date2.year) && (date1.month == date2.month) && (date1.day > date2.day)) )
  return (1);   /* True - Date1 is beyond Date2 */
 else
  return (0);   /* False */
}

void adj_code_validity_check (char *adj_code, char *errmsg )
{
 char *okcodes = CAP_GAIN_ADJUSTMENT_CODES;
 char up_adj_code;
 char lwrcasemsg[1024];
 int j=0, k;   /* j is adj_code index,  k is okcodes index */
 int errindex;

 if (adjerrcnt < MAXADJERRCNT) /* Capture up to MAXADJERRCNT errors */
  {
   if (strcmp(adj_code, "~") != 0)  /* check for no-data place holder, leave as is */
   {
    while (adj_code[j] != '\0')
    {
     if ( ! isalpha(adj_code[j]))
     {
      adjerrcnt++;
      errindex = adjerrcnt - 1;
      strcpy(adj_code_err[errindex], errmsg);
     }
     else
     {             /* is alpha */
      k=0;
      up_adj_code = toupper(adj_code[j]);
      while ( (okcodes[k] != '\0') && (up_adj_code != okcodes[k]) ) k++; /* Look for uppercased match */
      if (okcodes[k] != '\0')  /* Have an uppercased match */
      {
        if ( adj_code[j] != up_adj_code)
        {
         adjerrcnt++;
         strcpy (lwrcasemsg, errmsg);
         strcat (lwrcasemsg, "    * Case Error - Should be Capitalized *");
         errindex = adjerrcnt - 1;
         strcpy(adj_code_err[errindex], lwrcasemsg);   
        } 
      }
      else
      {             /* No uppercased match */
       adjerrcnt++;
       errindex = adjerrcnt - 1;
       strcpy(adj_code_err[errindex], errmsg);   
      }
     } /* is alpha */
     j++;
    }  /* while adj_code */
   }  /* strcmp */
  } /* if adjerrcnt < MAXADJERRCNT */
}

void Display_adj_code_err()
{
 int i, index;
 
  printf("\n\n\n***** CAUTION:  Possible Invalid Form 8949  Adjustment Code  *****\n");
  printf("*****  Does NOT affect processing                            *****\n\n");
  printf("Code  Section      Description   BuyDate   DateSold\n"); 
  for ( i = 1; i <= adjerrcnt; i++)  /* adjerrcnt will never exceed MAXADJERRCNT */
  {
   index = i - 1;
   printf("\n %s  \n", adj_code_err[index] );
  } 
 }


/* Grab Spreadsheet for Capital Sales (Gain/Loss) of Form-8949 from a CSV or Tab-delimited file. */
void get_CSV_8949( char *spreadsheet_name )
{
 char sline[4096], word[4096], delim=',';
 char descrip[512], date_bought[512], date_sold[512], adj_code[512], adjcodeerrmsg[4096];
 double proceeds, cost, adj_amnt;
 struct date_rec buydate, selldate, annivdate;
 enum {none, short_term, long_term} term_flg=none;
 int err=0;
 FILE *sfile;	/* Spreadsheet-File. */

 remove_certain_chars( spreadsheet_name, "\"" );	/* Allow spaces in file-paths. */
 printf("Opening Form-849 Spreadsheet: '%s'\n", spreadsheet_name );
 sfile = fopen( spreadsheet_name, "r" );
 if (sfile == 0)
  {
   printf("ERROR: Could not open f8949 spreadsheet file '%s' for reading.\n", spreadsheet_name );
   fprintf(outfile,"ERROR: Could not open f8949 spreadsheet file '%s' for reading.\n", spreadsheet_name );
   return;
  }
 Do_SchedD = Yes;

 /* Expect f8949 spreadsheet file name extension to be ".csv", ".tsv", or ".txt". */
 strcpy( word, spreadsheet_name );
 capitalize( word );
 if (strstr( word, ".CSV" ))
  delim = ',';
 else
 if (strstr( word, ".TSV" ))
  delim = '\t';
 else
 if (strstr( word, ".TXT" ))
  { /* Try to determine if file is CSV or TSV. */
    read_line_safe( sfile, sline, 4096 );
    if (strstr( sline, "\t" ))
     delim = '\t';
    fclose( sfile );	/* Reload file. */
    sfile = fopen( spreadsheet_name, "r" );
  }
 else
  {
   printf("ERROR: f8949 Spreadsheet file '%s' is not '.csv', '.tsv', or '.txt'.\n", spreadsheet_name );
   fprintf(outfile,"ERROR: f8949 Spreadsheet file '%s' is not '.csv', '.tsv', or '.txt'.\n", spreadsheet_name );
   fclose( sfile );
   return;
  }
 if (delim == ',')
   fprintf(outfile," Reading Comma-delimited spreadsheet file.\n");
 else
   fprintf(outfile," Reading Tab-delimited spreadsheet file.\n");

 /* Expect 1st line of spreadsheet file to be:
       Description, Date_Acquired, Date_Sold, Proceeds, Cost, Code, Adjustment
 */
 read_line_safe( sfile, sline, 4096 );
 next_csv( sline, word, delim );	capitalize( word );
 if (!strstr( word, "DESCRIP" ))  err++;
 next_csv( sline, word, delim );	capitalize( word );
 if (!strstr( word, "ACQUIRED" )) err++;
 next_csv( sline, word, delim );	capitalize( word );
 if (!strstr( word, "SOLD" ))	  err++;
 next_csv( sline, word, delim );	capitalize( word );
 if (!strstr( word, "PROCEED" ))  err++;
 next_csv( sline, word, delim );	capitalize( word );
 if (!strstr( word, "COST" ))	  err++;
 if (err)
  {
   printf("ERROR: f8949 Spreadsheet file '%s' does not have expected header-line.\n", spreadsheet_name );
   fprintf(outfile,"ERROR: f8949 Spreadsheet file '%s' does not have expected header-line.\n", spreadsheet_name );
   fclose( sfile );
   return;
  }
  
 read_line_safe( sfile, sline, 4096 );
 while (!feof( sfile ))
  {
   consume_leading_trailing_whitespace( sline );
   if (strlen( sline ) > 1)
    { /*valid_line*/
      if (verbose) fprintf(outfile,"ReadLine: '%s'\n", sline );
      term_flg = none;  /* Initialize */
      next_csv( sline, descrip, delim );
      if (verbose) fprintf(outfile,"Descript = '%s', Line = '%s'\n", descrip, sline ); 
      next_csv( sline, date_bought, delim );
      if (verbose) fprintf(outfile,"BuyDate = '%s', Line = '%s'\n", date_bought, sline ); 
      if (mystrcasestr( date_bought, "various-short" ) != 0)
       term_flg = short_term;
      else
      if ((mystrcasestr( date_bought, "various-long" ) != 0) || (mystrcasestr( date_bought, "inherited" ) != 0))
       term_flg = long_term;
      else
       gen_date_rec( date_bought, descrip, &buydate );

      next_csv( sline, date_sold, delim );
      if (verbose) fprintf(outfile,"SoldDate = '%s', Line = '%s'\n", date_sold, sline ); 
      if (term_flg == none)		/* Executes if term_flg Not otherwise set in case: 1 */
       { /*term_flg*/
        gen_date_rec( date_sold, descrip, &selldate );
        if (is_date1_beyond_date2( buydate, selldate ))
         {
	  printf("DATA ERROR: Buy-date after sell-date in f8949 spreadsheet.   '%s'\n Buy-date '%s'  Sell-date '%s'\n", 
		descrip, date_bought, date_sold );
	  fprintf(outfile,"DATA ERROR: Buy-date after sell-date in f8949 spreadsheet.   '%s'\n Buy-date '%s'  Sell-date '%s'\n", 
		descrip, date_bought, date_sold );
	  exit(1);
         }
        /* "annivdate" will be the date of the one year holding period relative to the Buy-date */
        annivdate.year = buydate.year + 1;
        annivdate.month = buydate.month;
        annivdate.day = buydate.day;
        if ((annivdate.month == 2) && (annivdate.day == 28) && (isleapyear(annivdate.year)))
         annivdate.day=29;
        else
        if ((annivdate.month == 2) && (annivdate.day == 29) && !(isleapyear(annivdate.year)))
         annivdate.day=28;
        if (is_date1_beyond_date2(selldate, annivdate))
         term_flg = long_term;	/* Holding Period Test */
        else
         term_flg = short_term;
       } /*term_flg*/

      next_csv( sline, word, delim );
      if (verbose) fprintf(outfile,"Proceeds = '%s', Line = '%s'\n", word, sline ); 
      if (sscanf(word,"%lf",&proceeds)!=1)
       { printf("ERROR: Bad float '%s', in Proceeds column of f8949 Spreadsheet file '%s'.\n", word, spreadsheet_name ); 
         fprintf(outfile,"ERROR: Bad float '%s', in Proceeds column of f8949 Spreadsheet file '%s'.\n", word, spreadsheet_name );
         exit(1);
       }

      next_csv( sline, word, delim );
      if (verbose) fprintf(outfile,"Cost = '%s', Line = '%s'\n", word, sline ); 
      if (sscanf(word,"%lf",&cost)!=1)
       { printf("ERROR: Bad float '%s', in Cost column of f8949 Spreadsheet file '%s'.\n", word, spreadsheet_name ); 
         fprintf(outfile,"ERROR: Bad float '%s', in Cost column of f8949 Spreadsheet file '%s'.\n", word, spreadsheet_name );
         exit(1);
       }
      if (cost > 0.0) cost = -cost;   /* Cost/Buy amounts must be negative. (It is a cost.) */

      next_csv( sline, adj_code, delim );
      if (verbose) fprintf(outfile,"AdjCode = '%s', Line = '%s'\n", adj_code, sline ); 
      strcpy( adjcodeerrmsg, adj_code ); /* Assemble Error Msg for later use if needed */
      strcat( adjcodeerrmsg, "   " );
      strcat( adjcodeerrmsg, descrip );
      strcat( adjcodeerrmsg, "  " );
      strcat( adjcodeerrmsg, date_bought );
      strcat( adjcodeerrmsg, "  ");
      strcat( adjcodeerrmsg, date_sold );
      adj_code_validity_check( adj_code, adjcodeerrmsg );

      next_csv( sline, word, delim );
      if (verbose) fprintf(outfile,"AdjAmnt= '%s', Line = '%s'\n", word, sline );
      consume_leading_trailing_whitespace( word );
      if (word[0] == '\0')
	adj_amnt = 0.0;
      else
      if (sscanf(word,"%lf",&adj_amnt)!=1)
       { printf("ERROR: Bad float '%s', in Ajdustment column of f8949 Spreadsheet file '%s'.\n", word, spreadsheet_name ); 
         fprintf(outfile,"ERROR: Bad float '%s', in Proceeds column of f8949 Spreadsheet file '%s'.\n", word, spreadsheet_name );
         exit(1);
       }

      if (term_flg == long_term)
       { /*long-gain/loss*/
         new_capgain( &long_trades, descrip, cost, date_bought, proceeds, date_sold, adj_code, adj_amnt );
       } /*long-gain/loss*/
      else
       { /*short-gain/loss*/
         new_capgain( &short_trades, descrip, cost, date_bought, proceeds, date_sold, adj_code, adj_amnt );
       } /*short-gain/loss*/

    } /*valid_line*/
   read_line_safe( sfile, sline, 4096 );
  }
 fclose( sfile );
}


void get_gain_and_losses( char *label )
{
 char word[4096], date_str1[512], date_str2[512], adj_code[512]; 
 char comment[4096], comment2[2048], comment3[2048], labelcommentmsg[4096], adjcodeerrmsg[4096];
 double amnt1, amnt2, adj_amnt;
 int toggle=0 ;
 struct date_rec buydate, selldate, annivdate;
 enum {none, short_term, long_term} term_flg=none;

 get_parameter( infile, 'l', word, label );     /* CapGains or f8949_spreadsheet_file. */
 if (strstr( word, "f8949_spreadsheet"))
  {
   read_comment_filtered_line( infile, word, 4096 );
   if (strlen(word) > 1)
    get_CSV_8949( word );
   get_parameter( infile, 's', word, label );
  }

 get_word(infile, word);
 while (word[0]!=';')
 { /*while_not_end*/
  if (feof(infile))
   {printf("ERROR: Unexpected EOF on '%s'\n", label ); fprintf(outfile,"ERROR: Unexpected EOF on '%s'\n", label ); exit(1);}
  if (!Do_SchedD)
   { fprintf(outfile,"\nForm(s) 8949:\n");  Do_SchedD = Yes; }
  switch (toggle)
   { /*switch_toggle*/
    case 0:	toggle++;
         term_flg = none;  /* Initialize */
	 if (sscanf(word,"%lf",&amnt1)!=1)
	  {printf("ERROR: Bad float '%s', reading %s.\n", word, label ); fprintf(outfile,"ERROR: Bad float '%s', reading %s.\n", word, label ); exit(1); }
	 if (amnt1 > 0.0) amnt1 = -amnt1;  /* Buy amounts must be negative. (It is a cost.) */
	 break;
    case 1:	toggle++;
         /* Expect stock name in comment after first date (buy-date). */
         get_comment( infile, comment );  /* Get comment for use in DATA ERROR Messages */
         strcpy (labelcommentmsg, label);
         if (strlen(label) + strlen(comment) < 4092 )
	  {
           strcat(labelcommentmsg, ", ");
           strcat(labelcommentmsg, comment);
          }
	 strcpy( date_str1, word );
	 if (mystrcasestr( date_str1, "various-short" ) != 0)
	  term_flg = short_term;
	 else
	 if ((mystrcasestr( date_str1, "various-long" ) != 0) || (mystrcasestr( date_str1, "inherited" ) != 0))
	  term_flg = long_term;
	 else
	  gen_date_rec( word, labelcommentmsg, &buydate );
	 break;
    case 2:	toggle++;
	 if (sscanf(word,"%lf",&amnt2)!=1)
	  { printf("ERROR: Bad float '%s', reading %s.\n", word, label ); 
	    fprintf(outfile,"ERROR: Bad float '%s', reading %s.\n", word, label );
	    exit(1);
	  }
	 break;
    case 3:	toggle++;
	 strcpy( date_str2, word );
	 get_comment( infile, comment2 );	/* Check for and consume any additional comment. */
         strcat( comment, comment2 );
         if (term_flg == none)		/* Executes if term_flg Not otherwise set in case: 1 */
	  {
           gen_date_rec ( word, labelcommentmsg, &selldate );
           if (is_date1_beyond_date2 (buydate, selldate))
	    {
	     printf("DATA ERROR: Buy-date after sell-date.   '%s'\n Buy-date '%s'  Sell-date '%s'\n", labelcommentmsg, date_str1, date_str2);
	     fprintf(outfile,"DATA ERROR: Buy-date after sell-date.   '%s'\n Buy-date '%s'  Sell-date '%s'\n", labelcommentmsg, date_str1, date_str2);
	     exit(1);
            }
           /* "annivdate" will be the date of the one year holding period relative to the Buy-date */
           annivdate.year = buydate.year + 1;
           annivdate.month = buydate.month;
           annivdate.day = buydate.day;
           if ((annivdate.month == 2) && (annivdate.day == 28) && (isleapyear(annivdate.year)))
	    annivdate.day=29;
           else
           if ((annivdate.month == 2) && (annivdate.day == 29) && !(isleapyear(annivdate.year)))
	    annivdate.day=28;
           if (is_date1_beyond_date2(selldate, annivdate))
	    term_flg = long_term;	/* Holding Period Test */
           else
           term_flg = short_term;
	  }
	  break;
    case 4:	toggle++;
	  strcpy (adj_code, word);          
          strcpy (adjcodeerrmsg, adj_code); /* Assemble Error Msg for later use if needed */
          strcat (adjcodeerrmsg, "   ");
          strcat (adjcodeerrmsg, labelcommentmsg);
          strcat (adjcodeerrmsg, "  ");
          strcat (adjcodeerrmsg, date_str1);
          strcat (adjcodeerrmsg, "  ");
          strcat (adjcodeerrmsg, date_str2);
	  adj_code_validity_check (adj_code, adjcodeerrmsg);
	  break;
    case 5:	toggle = 0;
	  get_comment(infile, comment3);   /* Check for and consume any additional comment. */
          strcat( comment, comment3 );     /* For consistency with Case 3 */
	  if (strcmp(word, "~") == 0) adj_amnt = 0.00;
	  else
	  {
 	   if (sscanf(word, "%lf", &adj_amnt) != 1 )
	   {
 	    printf("ERROR: Adj-Amnt - Bad float '%s', reading %s.\n", word, labelcommentmsg);
	    fprintf(outfile,"ERROR: Adj-Amnt - Bad float '%s', reading %s.\n", word, labelcommentmsg);
	    exit(1);
	   }
	  }
	 if (term_flg == long_term)
	  { /*long-gain/loss*/
	    new_capgain( &long_trades, comment, amnt1, date_str1, amnt2, date_str2, adj_code, adj_amnt );
	  } /*long-gain/loss*/
	 else
	  { /*short-gain/loss*/
	    new_capgain( &short_trades, comment, amnt1, date_str1, amnt2, date_str2, adj_code, adj_amnt );
	  } /*short-gain/loss*/
	 break;
   } /*switch_toggle*/
  get_word(infile, word);
 } /*while_not_end*/
 if (toggle!=0)
  {
   printf("ERROR: Imbalanced cap-gains entry (toggle=%d).\n", toggle);
   fprintf(outfile,"ERROR: Imbalanced cap-gains entry (toggle=%d).\n", toggle);
   exit(1);
  }
}


/************************************************************************/
/* Get_Cap_Gains - Get and calculate gains.  Forms 8949 + Sched-D.	*/
/* Like "get_params", but must get transaction dates.			*/
/* Expect entries in triple pairs. 					*/
/*   buy_amnt   date 							*/
/*   sell_amnt  date 							*/
/*   adj_code, adj_amnt									*/
/*									*/
/************************************************************************/
void get_cap_gains()		/* This is Schedule-D. */			/* Updated for 2022. */
{
 char word[4092], *LastYearsOutFile=0, labelx[1024]="";
 int j, doline22=0, got_collectibles=0;
 double stcg=0.0, ltcg=0.0;      /* Variables for short and long term gains. */
 double SchedDd[20], SchedDe[20], SchedDg[20];

 for (j=0; j<20; j++)		/* Initialize. */
  { SchedDd[j] = 0.0;  SchedDe[j] = 0.0; SchedDg[j] = 0.0; }

 /* Form 8849 - Adjunct form to Schedule-D. */
 get_gain_and_losses( "CapGains-A/D" );	/* (A) Basis Reported to IRS. */
 if (short_trades)
  {
   print_capgain_list( short_trades, 1, "Form 8949 Part-I, Short-Term Cap Gains+Losses, CHECK (A) Basis Reported to IRS:", "14 14\n F8949_ckA X" );
   SchedDd[1] = total_sales;
   SchedDe[1] = total_costs;
   SchedDg[1] = total_adjs;
   SchedD[1] = SchedDd[1] + SchedDe[1] + SchedDg[1];
   free_capgain_list( &short_trades );
  }
 if (long_trades)
  {
   print_capgain_list( long_trades, 3, "Form 8949 Part-II, Long-Term Cap Gains+Losses, CHECK (D) Basis Reported to IRS:", "15 15\n F8949_ckD X" );
   SchedDd[8] = total_sales;
   SchedDe[8] = total_costs;
   SchedDg[8] = total_adjs;
   SchedD[8] = SchedDd[8] + SchedDe[8] + SchedDg[8];
   free_capgain_list( &long_trades );
  }

 get_gain_and_losses( "CapGains-B/E" );	/* (B) Basis NOT Reported to IRS. */
 if (short_trades)
  {
   print_capgain_list( short_trades, 1, "Form 8949 Part-I, Short-Term Cap Gains+Losses, CHECK (B) Basis NOT Reported to IRS:", "14 14\n F8949_ckB X" );
   SchedDd[2] = total_sales;
   SchedDe[2] = total_costs;
   SchedDg[2] = total_adjs;
   SchedD[2] = SchedDd[2] + SchedDe[2] +SchedDg[2] ;
   free_capgain_list( &short_trades );
  }
 if (long_trades)
  {
   print_capgain_list( long_trades, 3, "Form 8949 Part-II, Long-Term Cap Gains+Losses, CHECK (E) Basis NOT Reported to IRS:", "15 15\n F8949_ckE X"  );
   SchedDd[9] = total_sales;
   SchedDe[9] = total_costs;
   SchedDg[9] = total_adjs;
   SchedD[9] = SchedDd[9] + SchedDe[9] + SchedDg[9];
   free_capgain_list( &long_trades );
  }

 get_gain_and_losses( "CapGains-C/F" );	/* (C) Cannot check (A) or (B). */
 if (short_trades)
  {
   print_capgain_list( short_trades, 1, "Form 8949 Part-I, Short-Term Cap Gains+Losses, CHECK (C) Not reported on Form 1099-B.\n", "14 14\n F8949_ckC X" );
   SchedDd[3] = total_sales;
   SchedDe[3] = total_costs;
   SchedDg[3] = total_adjs;
   SchedD[3] = SchedDd[3] + SchedDe[3] + SchedDg[3];
   free_capgain_list( &short_trades );
  }
 if (long_trades)
  {
   print_capgain_list( long_trades, 3, "Form 8949 Part-II, Long-Term Cap Gains+Losses, CHECK (F) Not reported on Form 1099-B.\n", "15 15\n F8949_ckF X" );
   SchedDd[10] = total_sales;
   SchedDe[10] = total_costs;
   SchedDg[10] = total_adjs;
   SchedD[10] = SchedDd[10] + SchedDe[10] + SchedDg[10];
   free_capgain_list( &long_trades );
  }

 stcg = SchedD[1] + SchedD[2] + SchedD[3];
 ltcg = SchedD[8] + SchedD[9] + SchedD[10];

 GetLine( "D4", &SchedD[4] );       /* Short term gain from 6252 and short-term gain or loss from Forms 4684, 6781, 8824. */
 GetLine( "D5", &SchedD[5] );       /* Net short-term gain or loss from partnerships, S corps, estates, trusts from K-1. */

 get_parameter( infile, 's', word, "D6" );	/* Carryover short-term loss from last year.  Or, LastYear's Return Output File-name. */
 get_word(infile, word);
 if (strcmp(word,";") != 0)
  {
   if (sscanf(word,"%lf",&SchedD[6]) != 1) LastYearsOutFile = strdup(word);
   do
    { get_word(infile,word); 
      if ((strlen(word) > 0) && (strcmp(word,";") != 0))
       fprintf(outfile,"Warning: Unexpected multiple values on line D6.  '%s' ignored.\n If multi-part filename, then surround it in quotes (\").", word );
    } while (strcmp(word,";") != 0);
  }

 GetLine( "D11", &SchedD[11] );	    /* Gain from Form 4797. */
 GetLine( "D12", &SchedD[12] );	    /* Partnership net long-term gain or loss. */
 GetLine( "D13", &SchedD[13] );	    /* Cap Gains Distributions - 1099-DIV col. 2a. */
 GetLine( "D14", &SchedD[14] );     /* Carryover long-term loss from last year. Or, leave blank if last year's file entered in line D6. */

 while (!got_collectibles)
  {
   get_parameter( infile, 'l', labelx, "D19 or Collectibles" );
   if (strcmp( labelx, "D19" ) == 0)
     get_parameters( infile, 'f', &SchedD[19], labelx );
   else
   if (strcmp( labelx, "Collectibles" ) == 0)
    {
     get_parameters( infile, 'f', &collectibles_gains, labelx );
     got_collectibles = 1;
    }
   else
    {
     printf("ERROR1: Found '%s' when expecting 'D19 or Collectibles'\n", labelx ); 
     fprintf(outfile,"ERROR1: Found '%s' when expecting 'D19 or Collectibles'\n", labelx );
     exit(1);
    }
  }

 // GetLine( "Collectibles", &collectibles_gains );	/* Gains or Losses from Collectibles. (Usually zero.) */
 if (collectibles_gains != 0.0) fprintf(outfile, "Collectibles_Gains = %6.2f\n", collectibles_gains );

 if (LastYearsOutFile != 0)
  CapitalLossCarryOverWorksheet( LastYearsOutFile, &LastYearsReturn );

 if (SchedD[6] > 0.0)
  { 
   /* fprintf(outfile,"Warning: D6 Carryover Loss must be NEGATIVE.\n"); */
   SchedD[6] = -SchedD[6];
  }
 if (SchedD[14] > 0.0)
  { 
   /* fprintf(outfile,"Warning: D14 Carryover Loss must be NEGATIVE.\n"); */
   SchedD[14] = -SchedD[14];
  }

 if ((SchedD[4] != 0.0) || (SchedD[5] != 0.0) || (SchedD[6] != 0.0) || (SchedD[11] != 0.0) || 
     (SchedD[12] != 0.0) || (SchedD[13] != 0.0) || (SchedD[14] != 0.0))
  { Do_SchedD = Yes; }	/* Set Do_SchedD in case it was not already set by Cap-Gain/Loss in rows 1-3, or 8-10. */

 if (Do_SchedD)
  { /*Sched-D*/
   fprintf(outfile," Cap Gains/Losses Schedule-D\n");
   fprintf(outfile,"PDFpage: 12 12\n");
   // Do_QDCGTW = Yes;	/* Tentatively set to do: Qualified Dividends and Capital Gain tax Worksheet. */
   fprintf(outfile,"\tNet Forms-8949 Short-term Gains = %10.2f\n", stcg );
   fprintf(outfile,"\tNet Forms-8949 Long-term Gains  = %10.2f\n", ltcg);
   fprintf(outfile," D1bd = %10.2f\n   D1be = %10.2f\n    D1bg = %10.2f\n    D1bh = %10.2f\n", SchedDd[1], absolutev(SchedDe[1]), SchedDg[1], SchedD[1] );
   fprintf(outfile," D2d = %10.2f\n   D2e = %10.2f\n    D2g = %10.2f\n    D2h = %10.2f\n", SchedDd[2], absolutev(SchedDe[2]),  SchedDg[2],SchedD[2] );

   fprintf(outfile," D3d = %10.2f\n   D3e = %10.2f\n    D3g = %10.2f\n    D3h = %10.2f\n", SchedDd[3], absolutev(SchedDe[3]), SchedDg[3], SchedD[3] );

   fprintf(outfile," D4 = %6.2f\n", SchedD[4] );
   fprintf(outfile," D5 = %6.2f\n", SchedD[5] );
   fprintf(outfile," D6 = %6.2f		(Carry-over Loss)\n", SchedD[6] );
   SchedD[7] = SchedD[1] + SchedD[2] + SchedD[3] + SchedD[4] + SchedD[5] + SchedD[6];
   fprintf(outfile," D7 = %6.2f		{ Net short-term capital gain or loss }\n", SchedD[7] );

   fprintf(outfile," D8bd = %10.2f\n   D8be = %10.2f\n   D8bg = %10.2f\n   D8bh = %10.2f\n", SchedDd[8], absolutev(SchedDe[8]), SchedDg[8], SchedD[8] );

   fprintf(outfile," D9d = %10.2f\n   D9e = %10.2f\n   D9g = %10.2f\n   D9h = %10.2f\n", SchedDd[9], absolutev(SchedDe[9]), SchedDg[9], SchedD[9] );

   fprintf(outfile," D10d = %10.2f\n   D10e = %10.2f\n   D10g = %10.2f\n   D10h = %10.2f\n", SchedDd[10], 
			absolutev(SchedDe[10]),  SchedDg[10], SchedD[10] );

   fprintf(outfile," D11 = %6.2f\n", SchedD[11] );
   fprintf(outfile," D12 = %6.2f\n", SchedD[12] );
   fprintf(outfile," D13 = %6.2f\n", SchedD[13] );
   fprintf(outfile," D14 = %6.2f	(Carry-over Loss)\n", SchedD[14] );
   SchedD[15] = SchedD[8] + SchedD[9] + SchedD[10] + SchedD[11] + SchedD[12] + SchedD[13] + SchedD[14];
   fprintf(outfile," D15 = %6.2f		{ Net long-term capital gain or loss }\n", SchedD[15] );
   fprintf(outfile,"EndPDFpage.\nPDFpage: 13 13\n");

   /* Part ||| */
   SchedD[16] = SchedD[7] + SchedD[15];
   fprintf(outfile," D16 = %6.2f\n", SchedD[16]);
   if (SchedD[16] > 0.0) 
    { /*gain*/
     L[7] = SchedD[16];
     if ((SchedD[15] > 0.0) && (SchedD[16] > 0.0))
      { /* Lines 17-21 */
	double wsd[50];

	fprintf(outfile," D17 = yes\n CkD17y X\n");

	/* '28% Rate Gain Worksheet' on instructions page D-13. */
	wsd[1] = collectibles_gains;	/* Gain or losses from "Collectibles" only.  Usually zero. */
	wsd[2] = 0.0;	/* Any 1202 exclusions, usually 0.0. */
	wsd[3] = 0.0;	/* Total collectibles on forms 4684, 6245, 6781, 8824. Usually no. */
	wsd[4] = 0.0;	/* Total collectibles 1099-Div box 2d, 2439 box 1d, or K-1's. Usually no. */
	wsd[5] = SchedD[14];
	if (SchedD[7] < 0.0)  wsd[6] = SchedD[7];  else  wsd[6] = 0.0;
	wsd[7] = NotLessThanZero( wsd[1] + wsd[2] + wsd[3] + wsd[4] + wsd[5] + wsd[6] );
	SchedD[18] = wsd[7];
	fprintf(outfile," D18 = %6.2f\n", SchedD[18]);

	/* 'Unrecaptured Section 1250 Gain Worksheet' on page D14, usually 0. */
	fprintf(outfile," D19 = %6.2f\n", SchedD[19]);

        if ((SchedD[18] == 0.0) && (SchedD[19] == 0.0))
	 { /*yes*/
	  fprintf(outfile," D20 = Yes\n CkD20y X\n");
	  // printf("Complete 'Qualified Dividends and Capital Gain tax Worksheet', instructions page 43.\n");
	  Do_QDCGTW = Yes;
	 } /*yes*/
	else
	 { /*no*/
	  fprintf(outfile," D20 = No\n CkD20n X\n");
	  // printf("Complete 'Schedule D Tax Worksheet', instructions page D-15.\n");
	  Do_SDTW = Yes;
	  Do_QDCGTW = No;
	 } /*no*/
       doline22 = 0;
      } /* Lines 17-21 */
     else 
      {
       printf(" D17 = no\n CkD17n X\n");
       doline22 = Yes;
      }
    } /*gain*/  
   else
   if (SchedD[16] < 0.0) 
    { /*loss*/	/* Schedule-D line 21. Skip to here from line 16 if a loss. */
     double maxloss;

     if (status == MARRIED_FILING_SEPARAT) maxloss = -1500.0; else maxloss = -3000.0;
     if (SchedD[16] < maxloss) SchedD[21] = maxloss; else SchedD[21] = SchedD[16];
     fprintf(outfile," D21 = %6.2f\n", SchedD[21]);
     L[7] = SchedD[21];
     doline22 = Yes;
    }
   else
    { /*Zero gain/loss.*/
     L[7] = 0.0;
     doline22 = Yes;
    }

   if (doline22)
    {
     if (L3a > 0.0)
      { /*yes*/
       fprintf(outfile," D22 = Yes\n CkD22y X\n");
       // printf("Complete 'Qualified Dividends and Capital Gain tax Worksheet', instructions page 44.\n");
       Do_QDCGTW = Yes;	
      } /*yes*/
     else
      { /*no*/
       fprintf(outfile," D22 = No\n CkD22n X\n");
       // Do_QDCGTW = No;	
      } /*no*/
    }

    fprintf(outfile,"EndPDFpage.\n\n");
  } /*Sched-D*/
}


/*--------------------------------------------------------*/
/* 'Schedule D Tax Worksheet', instructions page D 16+17. */
/*--------------------------------------------------------*/
void sched_D_tax_worksheet( int status )			/* Updated for 2022. */
{
 double ws[100];
 int k;

 for (k = 0; k < 100; k++)	/* Initialize worksheet to zero's. */
  ws[k] = 0.0;
 ws[1] = L[15];
 ws[2] = L3a;
 ws[3] = 0.0;	/* Form 4952, line 4g. Usually 0.0. */
 ws[4] = 0.0;	/* Form 4952, line 4e. Usually 0.0. */
 ws[5] = NotLessThanZero( ws[3] - ws[4] );
 ws[6] = NotLessThanZero( ws[2] - ws[5] );
 ws[7] = smallerof( SchedD[15], SchedD[16] );
 ws[8] = smallerof( ws[3], ws[4] );
 ws[9] = NotLessThanZero( ws[7] - ws[8] );
 ws[10] = ws[6] + ws[9];
 ws[11] = SchedD[18] + SchedD[19];
 ws[12] = smallerof( ws[9], ws[11] );
 ws[13] = ws[10] - ws[12];
 ws[14] = NotLessThanZero( ws[1] - ws[13] );
 switch (status) 
  { case SINGLE: case MARRIED_FILING_SEPARAT: ws[15] = 41675.0; break;
    case MARRIED_FILING_JOINTLY: case WIDOW:  ws[15] = 83350.0; break;
    case HEAD_OF_HOUSEHOLD:      	       ws[15] = 55800.0; break;
  }
 ws[16] = smallerof( ws[1], ws[15] );
 ws[17] = smallerof( ws[14], ws[16] );
 ws[18] = NotLessThanZero( ws[1] - ws[10] );
 switch (status) 
  { case SINGLE: case MARRIED_FILING_SEPARAT: ws[19] = smallerof( ws[1], 170050.0 );  break;
    case MARRIED_FILING_JOINTLY: case WIDOW:  ws[19] = smallerof( ws[1], 340100.0 );  break;
    case HEAD_OF_HOUSEHOLD:      	       ws[19] = smallerof( ws[1], 170050.0 );  break;
  }
 ws[20] = smallerof( ws[14], ws[19] );
 ws[21] = largerof( ws[18], ws[20] );
 ws[22] = ws[16] - ws[17];	/* This amount is taxed at 0%. */
 if (ws[1] != ws[16])
  { /*lines23-43*/
   ws[23] = smallerof( ws[1], ws[13] );
   ws[24] = ws[22];
   ws[25] = NotLessThanZero( ws[23] - ws[24] );
   switch (status) 
    { case SINGLE: 			ws[26] = 459750.0;  break;
      case MARRIED_FILING_SEPARAT: 	ws[26] = 258600.0;  break;
      case MARRIED_FILING_JOINTLY: 
      case WIDOW:  			ws[26] = 517200.0;  break;
      case HEAD_OF_HOUSEHOLD:		ws[26] = 488500.0;  break;
    }
   ws[27] = smallerof( ws[1], ws[26] );
   ws[28] = ws[21] + ws[22];
   ws[29] = NotLessThanZero( ws[27] - ws[28] );
   ws[30] = smallerof( ws[25], ws[29] );
   ws[31] = 0.15 * ws[30];
   ws[32] = ws[24] + ws[30];
   if (absolutev( ws[1] - ws[32] ) < 0.01)
    { /*lines33-43*/
      ws[33] = ws[23] - ws[32];
      ws[34] = 0.20 * ws[33];
      if (SchedD[19] != 0.0)
       { /*lines35-40*/
	 ws[35] = smallerof( ws[9], SchedD[19] );
	 ws[36] = ws[10] + ws[21];
	 ws[37] = ws[1];
	 ws[38] = NotLessThanZero( ws[36] - ws[37] );
	 ws[39] = NotLessThanZero( ws[35] - ws[38] );
	 ws[40] = 0.25 * ws[39];
       } /*lines35-40*/
      if (SchedD[18] != 0.0)
       { /*lines41-43*/
	 ws[41] = ws[21] + ws[22] + ws[30] + ws[33] + ws[39];
	 ws[42] = ws[1] - ws[41];
	 ws[43] = 0.28 * ws[42];
       } /*lines41-43*/
    } /*lines33-43*/
  } /*lines23-43*/
 ws[44] = TaxRateFunction( ws[21], status );
 ws[45] = ws[31] + ws[34] + ws[40] + ws[43] + ws[44];
 ws[46] = TaxRateFunction( ws[1], status );
 ws[47] = smallerof( ws[45], ws[46] );
 L[16] = Conditional_Round( ws[47] );
 for (k = 0; k < 100; k++)
  {
   ws_sched_D[k] = ws[k];	/* Save worksheet values for AMT, if needed. */
   if (ws[k] != 0.0)
    fprintf(outfile,"  Sched-D tax Worksheet line %d = %6.2f\n", k, ws[k]);
  }
}



/*--------------------------------------------------------*/
/* Social Security Worksheet - From Instructions page 31. */
/*--------------------------------------------------------*/
void SocSec_Worksheet()							/* Updated for 2022. */
{	/* Depends on Sched1, lines 11-25, and 1040 lines 1-8. Sets L[6]. */
 double ws[100], negative_amount_sched1_7=0.0;
 int k;
 if (L6a == 0.0) return;
 for (k = 0; k < 100; k++)	/* Initialize worksheet to all zero's. */
  ws[k] = 0.0;
 ws[1] = L6a;
 ws[2] = 0.5 * ws[1];
 if (Sched1[7] < 0.0)	/* Do not include any Unemployment Compensation Exclusion (UCE) in SocSec calculations. */
  negative_amount_sched1_7 = Sched1[7];		/* Remove any UCE from L8 by subtracting it. */
 ws[3] = L[1] + L[2] + L[3] + L[4] + L[5] + L[7] + L[8] - negative_amount_sched1_7;
 ws[4] = L2a;
 ws[5] = ws[2] + ws[3] + ws[4];
 for (k = 11; k <= 20; k++)
  ws[6] = ws[6] + Sched1[k];
 ws[6] = ws[6] + Sched1[23] + Sched1[25];
 for (k = 0; k <= 6; k++)
  fprintf(outfile,"\tSocSecWorkSheet[%d] = %6.2f\n", k, ws[k] );
 if (ws[6] >= ws[5])
  {
   L[6] = 0.0;		/* Which is "L6b". */
   fprintf(outfile,"\tSocSecWorkSheet[7]: Check 'No'\n" );
   printf("None of your social security benefits are taxable.\n");
   fprintf(outfile,"None of your social security benefits are taxable.\n");
   return;
  }
 ws[7] = ws[5] - ws[6];
 fprintf(outfile,"\tSocSecWorkSheet[7] = %6.2f  (Check 'Yes')\n", ws[7] );
 if (status == MARRIED_FILING_JOINTLY)
  ws[8] = 32000.0;      						/* Updated for 2022. */
 else
  ws[8] = 25000.0;
 fprintf(outfile,"\tSocSecWorkSheet[8] = %6.2f\n", ws[8] );
 if (ws[8] >= ws[7])
  {
   L[6] = 0.0;
   fprintf(outfile,"\tSocSecWorkSheet[9]: Check 'No'\n" );
   printf("None of your social security benefits are taxable.\n");
   fprintf(outfile,"None of your social security benefits are taxable.\n");
   return;
  }
 ws[9] = ws[7] - ws[8];
 fprintf(outfile,"\tSocSecWorkSheet[9] = %6.2f  (Check 'Yes')\n", ws[9] );
 if (status == MARRIED_FILING_JOINTLY)
  ws[10] = 12000.0;      						/* Updated for 2022. */
 else
  ws[10] = 9000.0;
 ws[11] = NotLessThanZero( ws[9] - ws[10] );
 ws[12] = smallerof( ws[9], ws[10] );
 ws[13] = Conditional_Round( ws[12] / 2.0 );
 ws[14] = smallerof( ws[2], ws[13] );
 ws[15] = NotLessThanZero( Conditional_Round( 0.85 * ws[11] ));
 ws[16] = ws[14] + ws[15];
 ws[17] = Conditional_Round( 0.85 * ws[1]);
 ws[18] = smallerof( ws[16], ws[17] );
 for (k = 10; k <= 18; k++)
  fprintf(outfile,"\tSocSecWorkSheet[%d] = %6.2f\n", k, ws[k] );
 L[6] = ws[18];		/* Which is "L6b". */
}



void pull_comment( char *line, char *word )
{
 int j=0, k=0;
 while ((line[j] != '\0') && (line[j] != '{')) j++;
 if (line[j] != '\0')
  {
   j++;
   while ((line[j+k] != '\0') && (line[j+k] != '}'))
    {
     word[k] = line[j+k];  k++;
    }
  }
 word[k] = '\0';
}


void Grab_ScheduleB_Payer_Lines( char *infname, FILE *outfile )
{ /* Copy Schedule-B Line entries from input file, to output file -- only. Does not process data read. */
  /* Used for PDF form-FILING only.  Not used by tax-calculations. */
 int state=0, cnt=0, pg=0, ncnt=15, newentry=0;
 double value;
 double total=0.0;
 char line[2048], word1[1024], word2[1024], pgstr[10]="";
 FILE *infile;

 infile = fopen( infname, "rb" );
 if (infile == 0)
  {
   printf("Can no longer read '%s'.\n", infname );
   return;
  }
 fprintf(outfile,"\nSchedules Data:\n");
 fgets( line, 200, infile );
 while (!feof(infile))
  {
   next_word( line, word1, " \t\n\r" );
   switch (state)
    {
     case 0:
	if (strcmp( word1, "L2b" ) == 0) 
	 { 
	  state = 8;  ncnt = 15; 
	  pg = 0;  cnt = 0;  newentry = 1;
	  strcpy( pgstr, "B1_" );
	 }
	else
	if (strcmp( word1, "L3b" ) == 0)
	 { 
	  if (pg > 0)
	   {
	    fprintf(outfile,"EndPDFpage.\n");
	   }
	  state = 9;  ncnt = 16;  total = 0.0;
	  pg = 0;  cnt = 0;  newentry = 1;
	  strcpy( pgstr, "B5_" );
	 }
	break;
     case 8:
	if (word1[0] == ';')
	 {
	  state = 0;
	  if (pg > 0) 
           {
            fprintf(outfile,"Btotal = %8.2f\n", total );
            fprintf(outfile,"EndPDFpage.\n");
	    pg = 0;
           }
	 }
	else
	if ((word1[0] != '\0') && (word1[0] != '{'))
	 {
	  pull_comment( line, word2 );
	  cnt++;
	  if (cnt == ncnt)
	   {
	    if (pg > 0) 
	     {
		fprintf(outfile,"Btotal = %8.2f\n", total );
		fprintf(outfile,"EndPDFpage.\n");
	     }
	    fprintf(outfile,"PDFpage: 11 11\n");
	    fprintf(outfile,"SchedB_Additional_form:  Schedule B - Additional Interest Income\n");
	    strcpy( pgstr, "Baddi_" );
	    cnt = 1;	ncnt = 30;	total = 0.0;
	    pg++;
	   }
	  fprintf(outfile," %s%d_Text: %s\n", pgstr, cnt, word2 );
	  remove_certain_chars( word1, "," );
	  if (sscanf( word1, "%lf", &value ) != 1)
	   printf(" Error reading L2b value '%s'\n", word1 );
	  else
	   {
	    fprintf(outfile," %s%d %8.2f\n", pgstr, cnt, value );
	    total = total + value;
	   }
     	 }
	break;
     case 9:
	if (word1[0] == ';') 
	 {
	  state = 0;
	  if (pg > 0) 
           {
            fprintf(outfile,"Btotal = %8.2f\n", total );
            fprintf(outfile,"EndPDFpage.\n");
	    pg = 0;
           }
	 }
	else
	if ((word1[0] != '\0') && (word1[0] != '{'))
	 {
	  pull_comment( line, word2 );
	  cnt++;
	  if (cnt == ncnt)
	   {
	    if (pg > 0) 
	     {
		fprintf(outfile,"Btotal = %8.2f\n", total );
		fprintf(outfile,"EndPDFpage.\n");
	     }
	    fprintf(outfile,"PDFpage: 11 11\n");
	    fprintf(outfile,"SchedB_Additional_form:  Schedule B - Additional Dividend Income\n");
	    strcpy( pgstr, "Baddi_" );
	    cnt = 1;	ncnt = 30;	total = 0.0;
	    pg++;
	   }
	  fprintf(outfile," %s%d_Text: %s\n", pgstr, cnt, word2 );
	  remove_certain_chars( word1, "," );
	  if (sscanf( word1, "%lf", &value ) != 1)
	   printf(" Error reading L3b value '%s'\n", word1 );
	  else
	   {
	    fprintf(outfile," %s%d %8.2f\n", pgstr, cnt, value );
	    total = total + value;
	   }
     	 }
	break;
    }
   if (!newentry)
    fgets( line, 200, infile );
   else
    newentry = 0;
  }
 if (pg > 0) 
  {
   printf("Error: Missing ending ';' on L%d\n", state );
   fprintf(outfile,"Btotal = %6.2f\n", total );
   fprintf(outfile,"EndPDFpage.\n");
  }
 fclose(infile);
}


void Calc_StudentLoan_Sched1L21()		/* Instructions page 93 */
{ /* Depends on Sched1 lines 11-25, and 1040 L[9].  Sets Sched1 lines 21, which impacts Sched1 line 26 and 1040 line 10. */
 int j;
 if (Sched1[21] != 0.0)
  { /* Student loan interest calculation pg 93. */
   double ws[20], sum=0.0;
   ws[1] = smallerof( Sched1[21], 2500.0 );
   ws[2] = L[9];
   for (j=11; j <= 20; j++)
    sum = sum + Sched1[j];
   sum = sum + Sched1[23] + Sched1[25];
   ws[3] = sum;
   ws[4] = ws[2] - ws[3];
   if (status == MARRIED_FILING_JOINTLY) ws[5] = 145000.0; else ws[5] = 70000.0;	/* Updated for 2022. */
   if (ws[4] > ws[5])
    {
     ws[6] = ws[4] - ws[5];
     if (status == MARRIED_FILING_JOINTLY)
      ws[7] = ws[6] / 30000.0; 
     else
      ws[7] = ws[6] / 15000.0;
     if (ws[7] >= 1.0)
      ws[7] = 1.0;
     ws[8] = ws[1] * ws[7];
    }
   else
    ws[8] = 0.0;
   ws[9] = ws[1] - ws[8];
   Sched1[21] = ws[9];
  }
}




/*----------------------------------------------------------------------*/
/* Main									*/
/*----------------------------------------------------------------------*/
int main( int argc, char *argv[] )						/* Updated for 2022. */
{
 int argk, j, k, itemize=0;
 char word[2000], outfname[2000], *infname="", labelx[1024]="";
 time_t now;
 double exemption_threshold=0.0, tmpval=0.0;
 double S_STD_DEDUC, MFS_STD_DEDUC, MFJ_STD_DEDUC, HH_STD_DEDUC, std_deduc;
 char *Your1stName, *YourLastName, *Spouse1stName, *SpouseLastName, *socsec, socsectmp[100];
 double NumDependents=0.0;
 int StdDedChart_NumBoxesChecked=0, nnn;
 int SchedB7a=0, SchedB7aa=-1, SchedB8=0, gotB7a=0;
 char SchedB7b[1024]="";
 double ntcpe=0.0, pyei=0.0;
 double Sched2_17[50];
 char *S2_17a_Type, *S2_17z_Type, *S3_6z_Type, *S3_13z_Type;
 double charityCC=0.0, charityOT=0.0, charityCO=0.0;

 /* Decode any command-line arguments. */
 printf("US 1040 2022 - v%3.2f\n", thisversion);
 argk = 1;  k=1;
 while (argk < argc)
 {
  if (strcmp(argv[argk],"-verbose")==0)  { verbose = Yes; }
  else
  if (strcmp(argv[argk],"-allforms")==0)  { force_print_all_pdf_forms = 1; }
  else
  if (strcmp(argv[argk],"-round_to_whole_dollars")==0)  { round_to_whole_dollars = 1; }
  else
  if (k==1)
   {
    infname = strdup( argv[argk] );
    infile = fopen( infname,"r");
    if (infile==0) {printf("ERROR: Parameter file '%s' could not be opened.\n", infname ); exit(1);}
    k = 2;
    /* Base name of output file on input file. */
    strcpy(outfname, infname);
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

 if (infile==0) {printf("Error: No input file on command line.\n"); exit(1);}

 /* Pre-initialize all lines to zeros. */
 for (j=0; j<MAX_LINES; j++)
  { 
   L[j] = 0.0;
   SchedA[j] = 0.0; 
   SchedD[j] = 0.0;
   Sched1[j] = 0.0;
   Sched2[j] = 0.0;
   Sched3[j] = 0.0;
   ws_sched_D[j] = 0.0;
   amtws[j] = 0.0; 
   qcgws[j] = 0.0;
  }

 /* Accept parameters from input file. */
 /* Expect  US-Fed-1040 lines, something like:
	Title:  Federal 1040 2022 Return
	L1		{Wages}
	L2b		{Interest}
	L3b		{Dividends}
 */

 /* Accept Form's "Title" line, and put out with date-stamp for your records. */
 read_line( infile, word );
 now = time(0);
 fprintf(outfile,"\n%s,	 v%2.2f, %s\n", word, thisversion, ctime( &now ) );
 check_form_version( word, "Title:  US Federal 1040 Tax Form - 2022" );

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

 get_parameter( infile, 's', word, "You_65+Over?" );	/* Were you born before January 2, 1958 ? (Y/N) */
 get_parameter( infile, 'b', &j, "You_65+Over?" );
 StdDedChart_NumBoxesChecked = j;
 if (j == 0)
   under65 = 1;  
 else
  {
   under65 = 0;
   fprintf(outfile,"CkYouOver65 X\n");
  }

 get_parameter( infile, 's', word, "You_Blind?" );	/* Are you blind ? (Y/N) */
 get_parameter( infile, 'b', &j, "You_Blind?" );
 StdDedChart_NumBoxesChecked = StdDedChart_NumBoxesChecked + j;
 if (j)
  fprintf(outfile,"CkYouBlind X\n");

 get_parameter( infile, 's', word, "Spouse_65+Over?" );	/* Was Spouse born before January 2, 1958 ? (Y/N) */
 get_param_single_line( infile, 'b', &j, "Spouse_65+Over?" );
 StdDedChart_NumBoxesChecked = StdDedChart_NumBoxesChecked + j;
 if (j == 0)
  under65++;
 else
  fprintf(outfile,"CkSpouseOver65 X\n");

 get_parameter( infile, 's', word, "Spouse_Blind?" );	/* Is Spouse blind ? (Y/N) */
 get_param_single_line( infile, 'b', &j, "Spouse_Blind?" );
 StdDedChart_NumBoxesChecked = StdDedChart_NumBoxesChecked + j;
 if (j)
  fprintf(outfile,"CkSpouseBlind X\n");

 switch (status)
  {
   case SINGLE: fprintf(outfile,"CkSingle X\nCkYourself X\nL6ab = 1\n");  break;
   case MARRIED_FILING_JOINTLY: fprintf(outfile,"CkMFJ X\nCkYourself X\nCkSpouse X\nL6ab = 2\n");  break;
   case MARRIED_FILING_SEPARAT: fprintf(outfile,"CkMFS X\nCkYourself X\nL6ab = 1\n");  break;
   case HEAD_OF_HOUSEHOLD: fprintf(outfile,"CkHH X\nCkYourself X\nL6ab = 1\n");  break;
   case WIDOW: fprintf(outfile,"CkQW X\nCkYourself X\nL6ab = 1\n");  break;
   default: printf("Error: Unknown filing status %d.\n", status );
  }

 GetLine1( "Dependents", &NumDependents );

 get_parameter( infile, 's', word, "VirtCurr?" );	/* During the year, did you have Virtual Currency? (Y/N) */
 get_parameter( infile, 'b', &j, "VirtCurr?" );
 if (j == 0)
  fprintf(outfile,"ChkVirtNo X\n");
 else
  fprintf(outfile,"ChkVirtYes X\n");

 GetLineF( "L1a", &L1a );	/* Wages from W-2 forms Box-1. */
 GetLineF( "L1b", &L1b );	/* Household employee wages not reported on Form(s) W-2. */
 GetLineF( "L1c", &L1c );	/* Tip income not reported on line 1a. */
 GetLineF( "L1d", &L1d );	/* Medicaid waiver payments not reported on Form(s) W-2. */
 GetLineF( "L1e", &L1e );	/* Taxable dependent care benefits from Form 2441, line 26. */
 GetLineF( "L1f", &L1f );	/* Employer-provided adoption benefits from Form 8839, line 29.*/
 GetLineF( "L1g", &L1g );	/* Wages from Form 8919, line 6. Other earned income. */
 GetLineF( "L1h", &L1h );	/* Other earned income. */
 GetLineF( "L1i", &L1i );	/* Nontaxable combat pay election. */
 L[1] = L1a + L1b + L1c + L1d + L1e + L1f + L1g + L1h;
 showline_wlabel( "L1z", L[1] );

 GetLineFnz( "L2a", &L2a );	/* Tax-exempt interest. (only for SocialSecurity calculations) */
 GetLineF( "L2b", &L[2] );	/* Taxable interest. (Sched-B) */
 GetLineF( "L3a", &L3a );	/* Qualified Dividends. (Sched-B) */
 if (L3a > 0.0) Do_QDCGTW = Yes;	
 GetLineF( "L3b", &L[3] );	/* Ordinary Dividends. (Sched-B) */
 GetLineF( "L4a", &L4a );	/* IRAs distributions. */
 GetLineF( "L4b", &L[4] );	/* Taxable IRAs distributions. */
 GetLineF( "L5a", &L5a );	/* Pensions and annuities. */
 GetLineF( "L5b", &L[5] );	/* Taxable pensions, and annuities. */
 GetLineF( "L6a", &L6a );	/* Social Security benefits.  Forms SSA-1099 box-5. */

 GetLine( "L13", &L[13] );	/* Qualified business income deduction. */
 GetLine( "L19", &L[19] );	/* Child tax credit/credit for other dependents. */

 GetLine( "L25a", &L25a );	/* Federal income tax withheld, Forms W-2, 1099 */
 GetLine( "L25b", &L25b );	/* Federal income tax withheld, Forms W-2, 1099 */
 GetLine( "L25c", &L25c );	/* Federal income tax withheld, Forms W-2, 1099 */
 L[25] = L25a + L25b + L25c;
 GetLine( "L26", &L[26] );	/* Estimated tax payments for 2022. */ 
 GetLine( "L27", &L[27] );	/* Earned Income Credit (EIC). */
 GetLine( "L28", &L[28] );	/* Refundable credit: Sch. 8812 */
 GetLine( "L29", &L[29] );	/* American Opportunity Credit, Form 8863, line 8 */

 GetLine( "L38", &L[38] );	/* Estimated Tax Under-payment Penalty */

 get_cap_gains();	 /* Capital gains. (Schedule-D). This popuates "schedD[]" and L[7]. */
 

 /* Determine your Std. Deduction value. */
 fprintf(outfile, "StdDedChart_NumBoxesChecked = %d\n", StdDedChart_NumBoxesChecked ); 
 if (StdDedChart_NumBoxesChecked == 0)
  {
   S_STD_DEDUC   = 12950.0;						/* Updated for 2022. */
   MFJ_STD_DEDUC = 25900.0;
   MFS_STD_DEDUC = 12950.0;
   HH_STD_DEDUC  = 19400.0;
  }
 else
  { /* Std. Deduction chart for People who were Born Before January 2, 1958, or Were Blind, pg 34. */
    switch (StdDedChart_NumBoxesChecked)		/* Does not handle if someone claims you or joint-spouse as dependent. */
     {				/* (Qualifying Widow/er has same amounts as MFJ, so not broken into separate variable.) */
      case 1: 
	S_STD_DEDUC   = 14700.0;					/* Updated for 2022. */
	MFJ_STD_DEDUC = 27300.0;
	MFS_STD_DEDUC = 14350.0;
	HH_STD_DEDUC  = 21150.0;
	break;
      case 2: 
	S_STD_DEDUC   = 16450.0;
	MFJ_STD_DEDUC = 28700.0;
	MFS_STD_DEDUC = 15750.0;
	HH_STD_DEDUC  = 22900.0;
	break;
      case 3: 
	MFJ_STD_DEDUC = 30100.0;
	MFS_STD_DEDUC = 17150.0;
	S_STD_DEDUC   = 16450.0;	/* Cannot happen, but set to appease compiler. */
	HH_STD_DEDUC  = 22900.0;	/* .. */
	break;
      case 4: 
	MFJ_STD_DEDUC = 31500.0;
	MFS_STD_DEDUC = 18550.0;
	S_STD_DEDUC   = 16450.0;	/* Cannot happen, but set to appease compiler. */
	HH_STD_DEDUC  = 22900.0;	/* .. */
	break;
      default:  fprintf(outfile,"Error: StdDedChart_NumBoxesChecked (%d) not equal to 1, 2, 3, or 4.\n", StdDedChart_NumBoxesChecked );
		printf("Error: StdDedChart_NumBoxesChecked (%d) not equal to 1, 2, 3, or 4.\n", StdDedChart_NumBoxesChecked );
		exit(1); 
     }
    fprintf(outfile,"  (Assuming no one is claiming you, or your joint-spouse, as a dependent.)\n");
  }

 switch (status)
  {
   case SINGLE:
		std_deduc = S_STD_DEDUC;	break;
   case MARRIED_FILING_SEPARAT:  
		std_deduc = MFS_STD_DEDUC;	break;
   case WIDOW:
   case MARRIED_FILING_JOINTLY:
		std_deduc = MFJ_STD_DEDUC;	break;
   case HEAD_OF_HOUSEHOLD:
		std_deduc = HH_STD_DEDUC;	break;
   default:  printf("Case (Line 12) not handled.\n"); fprintf(outfile,"Case (Line 12) not handled.\n"); exit(1);
  }



 /* -- Schedule-1 -- Additional Income and Adjustments */

 GetLineF( "S1_1", &Sched1[1] );	/* Taxable refunds. */
 GetLineF( "S1_2a", &Sched1[2] );	/* Alimony received. */
 GetTextLineF( "S1_2b:" );		/* Date of divorce or separation. */

 GetLineF( "S1_3", &Sched1[3] );	/* Business income/loss. */
 showline_wlabel( "S1_3", Sched1[3] );   /* This line was set in get_cap_gains() above. */

 GetLineFnz( "S1_4", &Sched1[4] );	/* Other gains or losses. Form 4794. */

 GetLineFnz( "S1_5", &Sched1[5] );	/* Rent realestate, royalties, partnerships, S corp. (Sched E)*/

 GetLineFnz( "S1_6", &Sched1[6] );	/* Farm income/loss. (Sched F) */

 GetLineFnz( "S1_7", &Sched1[7] );	/* Unemployment compensation */

 GetLineFnz( "S1_8a", &Sched1[8] );	/* Net operating loss */
 Sched1[9] = -absolutev( Sched1[8] );
 GetLineFnz( "S1_8b", &Sched1[8] );	/* Gambling income */
 Sched1[9] = Sched1[9] + Sched1[8];
 GetLineFnz( "S1_8c", &Sched1[8] );	/* Cancellation of debt */
 Sched1[9] = Sched1[9] + Sched1[8];
 GetLineFnz( "S1_8d", &Sched1[8] );	/* Foreign earned income exclusion from Form 2555 */
 Sched1[9] = Sched1[9] - absolutev( Sched1[8] );
 GetLineFnz( "S1_8e", &Sched1[8] );	/* Income from Form 8853 */
 Sched1[9] = Sched1[9] + Sched1[8];
 GetLineFnz( "S1_8f", &Sched1[8] );	/* Income from Form 8889 */
 Sched1[9] = Sched1[9] + Sched1[8];
 GetLineFnz( "S1_8g", &Sched1[8] );	/* Alaska Permanent Fund dividends */
 Sched1[9] = Sched1[9] + Sched1[8];
 GetLineFnz( "S1_8h", &Sched1[8] );	/* Jury duty pay */
 Sched1[9] = Sched1[9] + Sched1[8];
 GetLineFnz( "S1_8i", &Sched1[8] );	/* Prizes and awards */
 Sched1[9] = Sched1[9] + Sched1[8];
 GetLineFnz( "S1_8j", &Sched1[8] );	/* Activity not engaged in for profit income */
 Sched1[9] = Sched1[9] + Sched1[8];
 GetLineFnz( "S1_8k", &Sched1[8] );	/* Stock options */
 Sched1[9] = Sched1[9] + Sched1[8];
 GetLineFnz( "S1_8l", &Sched1[8] );	/* Income from the rental of personal property if ... */
 Sched1[9] = Sched1[9] + Sched1[8];
 GetLineFnz( "S1_8m", &Sched1[8] );	/* Olympic and Paralympic medals + prizes */
 Sched1[9] = Sched1[9] + Sched1[8];
 GetLineFnz( "S1_8n", &Sched1[8] );	/* Section 951(a) inclusion */
 Sched1[9] = Sched1[9] + Sched1[8];
 GetLineFnz( "S1_8o", &Sched1[8] );	/* Section 951A(a) inclusion */
 Sched1[9] = Sched1[9] + Sched1[8];
 GetLineFnz( "S1_8p", &Sched1[8] );	/* Section 461(l) excess business loss adjustment */
 Sched1[9] = Sched1[9] + Sched1[8];
 GetLineFnz( "S1_8q", &Sched1[8] );	/* Taxable distributions from an ABLE account */
 Sched1[9] = Sched1[9] + Sched1[8];
 GetLineFnz( "S1_8r", &Sched1[8] );	/* Scholarship and fellowship grants not reported on Form W-2 */
 Sched1[9] = Sched1[9] + Sched1[8];
 GetLineFnz( "S1_8s", &Sched1[8] );	/* Nontaxable Medicaid waiver payments included on line 1a or 1d */
 Sched1[9] = Sched1[9] + -absolutev( Sched1[8] );
 GetLineFnz( "S1_8t", &Sched1[8] );	/* Pension from a nonqualifed deferred compensation plan or section 457 plan */
 Sched1[9] = Sched1[9] + Sched1[8];
 GetLineFnz( "S1_8u", &Sched1[8] );	/* Wages earned while incarcerated */
 Sched1[9] = Sched1[9] + Sched1[8];
 GetLineFnz( "S1_8z", &Sched1[8] );	/* Other income. */
 Sched1[9] = Sched1[9] + Sched1[8];
 GetTextLineF( "S1_8z_Type:" );

 showline_wlabel( "S1_9", Sched1[9] );

 for (j=1; j <= 7; j++)
  Sched1[10] = Sched1[10] + Sched1[j];
 Sched1[10] = Sched1[10] + Sched1[9];
 showline_wlabel( "S1_10", Sched1[10] );
 L[8] = Sched1[10];
 L[9] = L[1] + L[2] + L[3] + L[4] + L[5] + L[6] + L[7] + L[8];

 GetLineF( "S1_11", &Sched1[11] );	/* Educator expenses */
 GetLineF( "S1_12", &Sched1[12] );	/* Bus. exp.: reservists, artists, ... Attach Form 2106 */
 GetLineF( "S1_13", &Sched1[13] );	/* Health savings account deduction. Attach Form 8889 */
 GetLineF( "S1_14", &Sched1[14] );	/* Moving expenses. Attach Form 3903 */
 GetLineF( "S1_15", &Sched1[15] );	/* Deductable part of self-employment tax. Attach Schedule SE */
 GetLineF( "S1_16", &Sched1[16] );	/* Self-employed SEP, SIMPS1_E, and qualified plans */
 GetLineF( "S1_17", &Sched1[17] );	/* Self-employed health insurance deduction  */
 GetLineF( "S1_18", &Sched1[18] );	/* Penalty on early withdrawal of savings */
 GetLineF( "S1_19a", &Sched1[19] );	/* Alimony paid */

 GetTextLineF( "AlimRecipSSN:" );
 GetTextLineF( "DivorceDate:" );

 GetLineF( "S1_20", &Sched1[20] );	/* IRA deduction */
 GetLineF( "S1_21", &Sched1[21] );	/* Student loan interest deduction */
 GetLineF( "S1_23", &Sched1[23] );	/* Archer MSA deduction */

 GetLineF( "S1_24a", &Sched1[24] );	/* Jury duty pay */
 Sched1[25] = Sched1[24];
 GetLineF( "S1_24b", &Sched1[24] );	/* Deductible expenses of rental of personal property */
 Sched1[25] = Sched1[25] + Sched1[24];
 GetLineF( "S1_24c", &Sched1[24] );	/* Nontaxable Olympic and Paralympic medals & prizes */
 Sched1[25] = Sched1[25] + Sched1[24];
 GetLineF( "S1_24d", &Sched1[24] );	/* Reforestation amortization and expenses */
 Sched1[25] = Sched1[25] + Sched1[24];
 GetLineF( "S1_24e", &Sched1[24] );	/* Repayment of supplemental unemployment benefits */
 Sched1[25] = Sched1[25] + Sched1[24];
 GetLineF( "S1_24f", &Sched1[24] );	/* Contributions to section 501(c)(18)(D) pension plans */
 Sched1[25] = Sched1[25] + Sched1[24];
 GetLineF( "S1_24g", &Sched1[24] );	/* Contributions by certain chaplains to section 403(b) plans */
 Sched1[25] = Sched1[25] + Sched1[24];
 GetLineF( "S1_24h", &Sched1[24] );	/* Attorney fees and court costs for unlawful discrimination claims */
 Sched1[25] = Sched1[25] + Sched1[24];
 GetLineF( "S1_24i", &Sched1[24] );	/* Attorney fees and court costs for award from the IRS */
 Sched1[25] = Sched1[25] + Sched1[24];
 GetLineF( "S1_24j", &Sched1[24] );	/* Housing deduction from Form 2555 */
 Sched1[25] = Sched1[25] + Sched1[24];
 GetLineF( "S1_24k", &Sched1[24] );	/* Excess deductions of section 67(e) expenses from Schedule K-1 */
 Sched1[25] = Sched1[25] + Sched1[24];
 GetLineF( "S1_24z", &Sched1[24] );	/* Other adjustments. */
 Sched1[25] = Sched1[25] + Sched1[24];
 GetTextLineF( "S1_24z_Type:" );
 showline_wlabel( "S1_25", Sched1[25] );

 SocSec_Worksheet();	/* This calc. depends on line L6a and Sched1[11-25].  Calculates L6b, which is L[6]. */
 L[9] = L[1] + L[2] + L[3] + L[4] + L[5] + L[6] + L[7] + L[8];
 Calc_StudentLoan_Sched1L21();

 for (j=11; j <= 23; j++)
  Sched1[26] = Sched1[26] + Sched1[j];
 Sched1[26] = Sched1[26] + Sched1[25];
 showline_wlabel( "S1_26", Sched1[26] );
 L[10] = Sched1[26];

 /* -- End of Schedule-1 -- */


 /* -- Schedule A - Input -- */
 GetLine( "A1", &SchedA[1] );	/* Unreimbursed medical expenses. */
 for (j=0; j<10; j++)
   localtax[j] = 0.0;
 if (status != MARRIED_FILING_SEPARAT)
  loctaxlimit = 10000.0;
 else
  loctaxlimit = 5000.0;
 GetLine( "A5a", &localtax[1] );	/* State and local income taxes. Or sales taxes. */
 get_parameter( infile, 'l', labelx, "CheckBoxA5a or A5b" );
 if (strcmp( labelx, "CheckBoxA5a" ) == 0)
  {
   get_parameters( infile, 'b', &j, "CheckBoxA5a" );
   if (j)
    fprintf(outfile,"CheckBoxA5a X\n");
   GetLine( "A5b", &localtax[2] );
  }
 else
 if (strcmp( labelx, "A5b" ) == 0)
  {
   get_parameters( infile, 'f', &localtax[2], "A5b" );
  }
 else
  {
   printf("Error: Found '%s' when expecteding CheckBoxA5a or A5b,\n", labelx );
   fprintf(outfile,"Error: Found '%s' when expecteding CheckBoxA5a or A5b,\n", labelx );
   exit(1);
  }
 // GetLine( "A5b", &localtax[2] );	/* State and local real estate taxes. */	/* Optionally read-in just above. */
 GetLine( "A5c", &localtax[3] );	/* State and local personal property (eg. automobile) taxes. */
 GetLine( "A6", &SchedA[6] );		/* Other taxes. */
 GetLine( "A8a", &homemort[0] );	/* Home mortgage interest and points reported to you on Form 1098.*/
 GetLine( "A8b", &homemort[1] );	/* Home mortgage interest not reported to you on Form 1098.*/
 GetLine( "A8c", &homemort[2] );	/* Points not reported to you on Form 1098.*/
 // GetLine( "A8d", &homemort[3] );	/* Mortgage insurance premiums. */
 GetOptionalLine( "A8d or A9", labelx, &tmpval );
 if (strcmp( labelx, "A9" ) == 0)
  {
   homemort[3] = 0.0;
   SchedA[9] = tmpval;
  }
 else
 if (strcmp( labelx, "A8d" ) == 0)
  {
   homemort[3] = tmpval;
   GetLine( "A9", &SchedA[9] );
  }
 else
  {
   printf("Error: Found '%s' when expecteding A8d,\n", labelx );
   fprintf(outfile,"Error: Found '%s' when expecteding A8d,\n", labelx );
   exit(1);
  }
 // GetLine( "A9", &SchedA[9] );	/* Investment interest. Attach Form 4952*/	/* Optionally read-in just above. */

 GetLine( "A11", &charityCC );	/* Charity Contributions by Cash or Check. */
 SchedA[11] = charityCC;	/* Charity contributions by cash or check.*/

 GetLine( "A12", &charityOT );	/* Charity Contributions Other Than cash or check. */
 SchedA[12] = charityOT;	/* Contributions other than cash or check.*/

 GetLine( "A13", &charityCO );	/* Charity Contributions Carried Over from last year. */
 SchedA[13] = charityCO;	/* Carryover from prior year*/


 GetLine( "A15", &SchedA[15] );	/* Casualty or theft loss(es).*/
 GetLine( "A16", &SchedA[16] );	/* Other expenses*/

 /* Look for optional Force-Itemize line.  (Remove *optional* logic for 2022, once A18 is on ALL templates.) */
 get_parameter( infile, 'l', labelx, "A18 or B7a");
 if (strcmp( labelx, "A18" ) == 0)
  {
   // GetYesNo( "A18", &ForceItemize );
   get_parameters( infile, 'b', &ForceItemize, labelx );
  }
 else
 if (strcmp( labelx, "B7a" ) == 0)
  {
   // GetYesNo( "B7a", &SchedB7a );
   get_parameters( infile, 'b', &SchedB7a, labelx );
   gotB7a = 1;
  }
 else
  {
   printf("Error: Found '%s' when expecteding A18 or B7a.\n", labelx );
   fprintf(outfile,"Error: Found '%s' when expecteding A18 or B7a\n", labelx );
   exit(1);
  }

 L[11] = L[9] - L[10];

 /* -- Calculate Schedule A -- */
 SchedA[2] = L[11];
 SchedA[3] = Conditional_Round( 0.075 * SchedA[2] );
 SchedA[4] = NotLessThanZero( SchedA[1] - SchedA[3] );
 localtax[4] =  localtax[1] +  localtax[2] +  localtax[3];
 localtax[5] = smallerof( localtax[4], loctaxlimit );
 SchedA[7] = localtax[5] + SchedA[6];
 homemort[5] = homemort[0] + homemort[1] + homemort[2] + homemort[3];
 SchedA[10] = homemort[5] + SchedA[9];
 SchedA[14] = SchedA[11] + SchedA[12] + SchedA[13];
 SchedA[17] = SchedA[4] + SchedA[7] + SchedA[10] + SchedA[14] + SchedA[15] + SchedA[16];

 if ((std_deduc < SchedA[17]) || (ForceItemize))
  { /*Select_to_Itemize*/
   itemize = Yes;
   L[12] = SchedA[17];	/* Use itemized value. */
   if (ForceItemize)
    {
     printf(" You elected to itemize deductions, even though they may be less than your Standard Deduction.\n");
     fprintf(outfile," You elected to itemize deductions, even though they may be less than your Standard Deduction.\n");
     printf("  (Itemizations = %6.2f, Std-Deduction = %6.2f)\n", SchedA[17], std_deduc );
     fprintf(outfile,"  (Itemizations = %6.2f, Std-Deduction = %6.2f)\n", SchedA[17], std_deduc );
    }
   else
    {
     printf("  (Itemizations > Std-Deduction, %6.2f > %6.2f)\n", SchedA[17], std_deduc );
     fprintf(outfile,"	(Itemizations > Std-Deduction, %6.2f > %6.2f)\n", SchedA[17], std_deduc );
    }
   fprintf(outfile,"Itemizing.\n");
  } /*Select_to_Itemize*/
 else
  { /*Select_to_use_StdDeduction*/
   itemize = No;
   L[12] = std_deduc;		/* Take the Std.Deduction. */
   printf("  (Itemizations < Std-Deduction, %6.2f < %6.2f)\n", SchedA[17], std_deduc );
   fprintf(outfile,"  (Itemizations < Std-Deduction, %6.2f < %6.2f)\n", SchedA[17], std_deduc );
   fprintf(outfile,"Use standard deduction.\n");
  } /*Select_to_use_StdDeduction*/


 /* -- Display Schedule A -- */
  showschedA(1);
  showschedA(2);
  showschedA(3);
  showschedA(4);
  showline_wlabel( "A5a", localtax[1] );
  showline_wlabel( "A5b", localtax[2] );
  showline_wlabel( "A5c", localtax[3] );
  showline_wlabel( "A5d", localtax[4] );
  showline_wlabel( "A5e", localtax[5] );
  showschedA(6);
  showschedA(7);
  showline_wlabel( "A8a", homemort[0] );
  showline_wlabel( "A8b", homemort[1] );
  showline_wlabel( "A8c", homemort[2] );
  showline_wlabel( "A8d", homemort[3] );
  showline_wlabel( "A8e", homemort[5] );
  showschedA(9);
  showschedA(10);
  showschedA(11);
  showschedA(12);
  showschedA_wMsg(13, "Carryover from prior year" );
  showschedA(14);
  showschedA(15);
  showschedA(16);
  showschedA(17);
  if (ForceItemize)
   fprintf(outfile,"CheckBoxA18 = X\n");

 showline_wlabel( "L6b", L[6] ); 
 showline( 7 );
 showline( 8 );
 showline( 9 );
 showline_wlabel( "S1_20", Sched1[20] );
 showline_wlabelnz( "S1_21", Sched1[21] );
 showline_wlabel( "S1_22", Sched1[22] );
 showline( 10 );
 showline_wlabelmsg( "L11", L[11], "Adjusted Gross Income" );


 if ((L[2] != 0.0) || (L[3] != 0.0))
  {
   fprintf(outfile," Schedule-B:\n");
   fprintf(outfile,"  B2 = %6.2f\n", L[2] );
   fprintf(outfile,"  B4 = %6.2f\n", L[2] );
   fprintf(outfile,"  B6 = %6.2f\n", L[3] );
  }


 if (under65 == 0) over65 = 1; 
 switch (status)	/* Check for minimum income to file. (min2file) */		/* Updated for 2022. */
  {			/* Listed in Instructions page-9, in Chart A - For Most People. */
   case SINGLE:  		  if (under65) exemption_threshold = 12950.0;
				  else  exemption_threshold = 14700.0;
	break;
   case MARRIED_FILING_JOINTLY:  if (under65==2) exemption_threshold = 25900.0;
				  else 
				  if (under65==1) exemption_threshold = 27300.0;  
				  else  exemption_threshold = 28700.0;
				  if (under65 != 2) over65 = 1;
	break;
   case MARRIED_FILING_SEPARAT:  exemption_threshold = 5.0;
	break;
   case HEAD_OF_HOUSEHOLD: 	  if (under65) exemption_threshold = 19400.0;  
				  else  exemption_threshold = 21150.0;
	break;
   case WIDOW:  		  if (under65) exemption_threshold = 25900.0;  
				  else  exemption_threshold = 27300.0;
  }
 if (L[11] < exemption_threshold)
  {
   printf(" (L11 = %3.2f < Threshold = %3.2f)\n", L[11], exemption_threshold );
   printf("You may not need to file a return, due to your income level.\n\n"); 
   fprintf(outfile,"You may not need to file a return, due to your income level.\n\n");
  }

 showline( 12 );
 showline( 13 );		/* Qualified business income ded., read-in above. */

 L[14] = L[12] + L[13];
 showline( 14 ); 
 
 L[15] = NotLessThanZero( L[11] - L[14] );
 showline_wlabelmsg( "L15", L[15], "Taxable Income" );

 L[16] = TaxRateFunction( L[15], status );

 if (L[15] <= 0.0) 
  { /*exception*/	/* See rules on top of "Schedule D Tax Worksheet", pg 16 of Sched-D Instructions. */
    printf(" Exception (Sched-D Instructions page D-16) - Do not use QDCGT or Sched-D Tax Worksheets.\n");
  } /*exception*/
 else
  { /*no_exception*/
   if ((!Do_SDTW) && (!Do_QDCGTW) && ((L3a > 0.0) || (Sched1[13] > 0.0) || ((SchedD[15] > 0.0) && (SchedD[16] > 0.0)) ))
    Do_QDCGTW = Yes;
   if (Do_QDCGTW)
    {
     fprintf(outfile,"Doing 'Qualified Dividends and Capital Gain tax Worksheet', page 36.\n");
     capgains_qualdividends_worksheets( status );
    }
   else
   if (Do_SDTW)
   {
    fprintf(outfile,"Doing 'Schedule D Tax Worksheet', page D16.\n");
    sched_D_tax_worksheet( status );
   }
  } /*no_exception*/

 showline_wlabelmsg( "L16", L[16], "Tax" );

 if (!gotB7a)
  GetYesNo( "B7a", &SchedB7a );
 GetYesNo( "B7aa", &SchedB7aa );
 GetString( "B7b", SchedB7b );
 GetYesNo( "B8", &SchedB8 );

 if (SchedB7a)
  fprintf(outfile,"CkB7a_Y X\n");
 else
  fprintf(outfile,"CkB7a_N X\n");

 if (SchedB7aa == 1)
  fprintf(outfile,"CkB7aa_Y X\n");
 else
 if (SchedB7aa == 0)
  fprintf(outfile,"CkB7aa_N X\n");

 if (strlen( SchedB7b ) > 0)
  fprintf(outfile,"B7b = %s\n", SchedB7b );

 if (SchedB8)
  fprintf(outfile,"CkB8_Y X\n");
 else
  fprintf(outfile,"CkB8_N X\n");


 /* -- Alternative Minimum Tax (AMT) Entries (if needed) -- */
 GetLine( "AMTws2c", &amtws2c );
 GetLine( "AMTws2g", &amtws2g );
 GetLine( "AMTws3", &amtws[3] );
 GetLine( "AMTws8", &amtws[8] );


 /* -- Schedule 2 -- Additional Taxes */
 GetLine( "S2_2", &Sched2[2] );		/* Excess advance premium tax credit repayment. Form 8962. */
					/* (Needed by AMT form6251.) */
 GetLine( "S2_4", &Sched2[4] );		/* Self-employment tax. Sched SE. */
 GetLine( "S2_5", &Sched2[5] );		/* Social security and Medicare tax from Form 4137. */
 GetLine( "S2_6", &Sched2[6] );		/* Uncollected Social security and Medicare tax from Form 8919 */
 Sched2[7] = Sched2[5] + Sched2[6];
 GetLine( "S2_8", &Sched2[8] );		/* Additional tax on IRAs or other tax-favored accounts, Form 5329 */
 GetLine( "S2_9", &Sched2[9] );		/* Household employment taxes. Sched H */
 GetLine( "S2_10", &Sched2[10] );	/* First-time homebuyer credit repayment. Form 5405. */

 GetLine( "S2_11", &Sched2[11] );	/* Additional Medicare Tax. Attach Form 8959 */
 GetLine( "S2_12", &Sched2[12] );	/* Net investment income tax. Attach Form 8960 */
 GetLine( "S2_13", &Sched2[13] );	/* Uncollected social security ... from Form W-2, box 12 */
 GetLine( "S2_14", &Sched2[14] );	/* Interest on tax due on installment income */
 GetLine( "S2_15", &Sched2[15] );	/* Interest on the deferred tax on gain from certain installment sales */
 GetLine( "S2_16", &Sched2[16] );	/* Recapture of low-income housing credit. Attach Form 8611 */

 for (j=0; j < 50; j++)	  /* Clear temporary storage variables. */
  Sched2_17[j] = 0;

 j = 0;
 GetLine( "S2_17a", &Sched2_17[j] );	/* Recapture of other credits. */
 S2_17a_Type = GetTextLine( "S2_17a_Type:" );
 Sched2[18] = Sched2_17[j++];
 GetLine( "S2_17b", &Sched2_17[j] );	/* Recapture of federal mortgage subsidy. */
 Sched2[18] = Sched2[18] + Sched2_17[j++];
 GetLine( "S2_17c", &Sched2_17[j] );	/* Additional tax on HSA distributions. Attach Form 8889 */
 Sched2[18] = Sched2[18] + Sched2_17[j++];
 GetLine( "S2_17d", &Sched2_17[j] );	/* Additional tax on an HSA, Attach Form 8889 */
 Sched2[18] = Sched2[18] + Sched2_17[j++];
 GetLine( "S2_17e", &Sched2_17[j] );	/* Additional tax on Archer MSA distributions. */
 Sched2[18] = Sched2[18] + Sched2_17[j++];
 GetLine( "S2_17f", &Sched2_17[j] );	/* Additional tax on Medicare Advantage MSA distributions. Form 8853 */
 Sched2[18] = Sched2[18] + Sched2_17[j++];
 GetLine( "S2_17g", &Sched2_17[j] );	/* Recapture of a charitable contribution deduction */
 Sched2[18] = Sched2[18] + Sched2_17[j++];
 GetLine( "S2_17h", &Sched2_17[j] );	/* Income you received from a nonqualified deferred compensation */
 Sched2[18] = Sched2[18] + Sched2_17[j++];
 GetLine( "S2_17i", &Sched2_17[j] );	/* Compensation received from nonqualified deferred compensation plan */
 Sched2[18] = Sched2[18] + Sched2_17[j++];
 GetLine( "S2_17j", &Sched2_17[j] );	/*  Section 72(m)(5) excess benefits tax */
 Sched2[18] = Sched2[18] + Sched2_17[j++];
 GetLine( "S2_17k", &Sched2_17[j] );	/* Golden parachute payments */
 Sched2[18] = Sched2[18] + Sched2_17[j++];
 GetLine( "S2_17l", &Sched2_17[j] );	/*  Tax on accumulation distribution of trusts */
 Sched2[18] = Sched2[18] + Sched2_17[j++];
 GetLine( "S2_17m", &Sched2_17[j] );	/* Excise tax on insider stock compensation */
 Sched2[18] = Sched2[18] + Sched2_17[j++];
 GetLine( "S2_17n", &Sched2_17[j] );	/* Look-back interest under section 167(g) or 460(b) */
 Sched2[18] = Sched2[18] + Sched2_17[j++];
 GetLine( "S2_17o", &Sched2_17[j] );	/* Tax on non-effectively connected income */
 Sched2[18] = Sched2[18] + Sched2_17[j++];
 GetLine( "S2_17p", &Sched2_17[j] );	/* Any interest from Form 8621, line 16f */
 Sched2[18] = Sched2[18] + Sched2_17[j++];
 GetLine( "S2_17q", &Sched2_17[j] );	/* Any interest from Form 8621, line 24  */
 Sched2[18] = Sched2[18] + Sched2_17[j++];
 GetLine( "S2_17z", &Sched2_17[j] );	/* Any other taxes. */
 Sched2[18] = Sched2[18] + Sched2_17[j++];
 S2_17z_Type = GetTextLine( "S2_17z_Type:" );
 showline_wlabelnz( "S2_18", Sched2[18] );
 nnn = j - 1;
 GetLine( "S2_19", &Sched2[19] );	/* Additional tax from Schedule 8812. */
 GetLine( "S2_20", &Sched2[20] );	/* Section 965 net tax liability installment from Form 965-A. */


 GetLine( "S3_1", &Sched3[1] ); 	/*  Foreign tax credit. Form 1116. (Needed by AMT form6251.) */ 

 Sched2[1] = form6251_AlternativeMinimumTax( itemize );		/* (Depends on Sched2[2] & Sched3[1].) */
 if (Sched2[1] == 0.0)
  fprintf(outfile," (Not subject to Alternative Minimum Tax.)\n");
 else
  {
   fprintf(outfile," (You must pay Alternative Minimum Tax.)\n");
   showline_wlabelmsg( "S2_1", Sched2[1], "Alternative Minimum Tax" );
  }
 Sched2[3] = Sched2[1] + Sched2[2];

 for (j=2; j <= 16; j++)
  {
   sprintf(word,"S2_%d", j );
   showline_wlabelnz( word, Sched2[j] );
  }
 showline_wlabelnz( "S2_17a", Sched2_17[0] );
 printf("S2_17a_Type: %s\n", S2_17a_Type );
 fprintf(outfile,"S2_17a_Type: %s\n", S2_17a_Type );
 for (j=2; j <= nnn; j++)
  { 
   sprintf(word,"S2_17%c", 'a' + j - 1 );
   showline_wlabelnz( word, Sched2_17[j] );
  }
 fprintf(outfile,"S2_17z_Type: %s\n", S2_17z_Type );

 Sched2[21] = Sched2[4] + Sched2[18] + Sched2[19];
 for (j=7; j <= 16; j++)
  Sched2[21] = Sched2[21] + Sched2[j];
 for (j=18; j <= 21; j++)
  {
   sprintf(word,"S2_%d", j );
   showline_wlabelnz( word, Sched2[j] );
  }
 L[23] = Sched2[21];

 /* -- End of Schedule 2 */


 L[17] = Sched2[3];
 showline( 17 );

 L[18] = L[16] + L[17];
 showline( 18 );

 showline( 19 );

 
 /* -- Schedule 3 -- Part I - Nonrefundable Credits */
 showline_wlabel( "S3_1", Sched3[1] );

 GetLine( "S3_2", &Sched3[2] );		/* Child / dependent care expense credits. Form 2441. */
 showline_wlabel( "S3_2", Sched3[2] );

 GetLine( "S3_3", &Sched3[3] );		/*  Education credits. Form 8863. */
 showline_wlabel( "S3_3", Sched3[3] );

 GetLine( "S3_4", &Sched3[4] );		/*  Retirement savings contributions credit. Form 8880. */
 showline_wlabel( "S3_4", Sched3[4] );

 GetLine( "S3_5", &Sched3[5] );		/*  Residential energy credits. Form 5695. */
 showline_wlabel( "S3_5", Sched3[5] );

 GetLineFnz( "S3_6a", &Sched3[6] );	/* General business credit. Form 3800. */
 showline_wlabel( "S3_6a", Sched3[6] );
 Sched3[7] = Sched3[7] + Sched3[6];

 GetLineFnz( "S3_6b", &Sched3[6] );	/* Credit for prior year minimum tax. Form 8801. */
 showline_wlabel( "S3_6b", Sched3[6] );
 Sched3[7] = Sched3[7] + Sched3[6];

 GetLineFnz( "S3_6c", &Sched3[6] );	/* Adoption credit. Attach Form 8839 */
 showline_wlabel( "S3_6c", Sched3[6] );
 Sched3[7] = Sched3[7] + Sched3[6];

 GetLineFnz( "S3_6d", &Sched3[6] );	/* Credit for the elderly or disabled. Attach Schedule R */
 showline_wlabel( "S3_6d", Sched3[6] );
 Sched3[7] = Sched3[7] + Sched3[6];

 GetLineFnz( "S3_6e", &Sched3[6] );	/* Alternative motor vehicle credit. Attach Form 8910 */
 showline_wlabel( "S3_6e", Sched3[6] );
 Sched3[7] = Sched3[7] + Sched3[6];

 GetLineFnz( "S3_6f", &Sched3[6] );	/* Qualified plug-in motor vehicle credit. Attach Form 8936 */
 showline_wlabel( "S3_6e", Sched3[6] );
 Sched3[7] = Sched3[7] + Sched3[6];

 GetLineFnz( "S3_6g", &Sched3[6] );	/* Mortgage interest credit. Attach Form 8396 */
 showline_wlabel( "S3_6g", Sched3[6] );
 Sched3[7] = Sched3[7] + Sched3[6];

 GetLineFnz( "S3_6h", &Sched3[6] );	/* District of Columbia first-time homebuyer credit. Attach Form 8859 */
 showline_wlabel( "S3_6h", Sched3[6] );
 Sched3[7] = Sched3[7] + Sched3[6];

 GetLineFnz( "S3_6i", &Sched3[6] );	/* Qualified electric vehicle credit. Attach Form 8834 */
 showline_wlabel( "S3_6i", Sched3[6] );
 Sched3[7] = Sched3[7] + Sched3[6];

 GetLineFnz( "S3_6j", &Sched3[6] );	/* Alternative fuel vehicle refueling property credit. Attach Form 8911 */
 showline_wlabel( "S3_6j", Sched3[6] );
 Sched3[7] = Sched3[7] + Sched3[6];

 GetLineFnz( "S3_6k", &Sched3[6] );	/* Credit to holders of tax credit bonds. Attach Form 8912 */
 showline_wlabel( "S3_6j", Sched3[6] );
 Sched3[7] = Sched3[7] + Sched3[6];

 GetLineFnz( "S3_6l", &Sched3[6] );	/* Amount on Form 8978, line 14. */
 showline_wlabel( "S3_6l", Sched3[6] );
 Sched3[7] = Sched3[7] + Sched3[6];

 GetLineFnz( "S3_6z", &Sched3[6] );	/* Other nonrefundable credits. */
 showline_wlabel( "S3_6z", Sched3[6] );
 Sched3[7] = Sched3[7] + Sched3[6];

 S3_6z_Type = GetTextLine( "S3_6z_Type:" );
 printf("S3_6z_Type: %s\n", S3_6z_Type );
 fprintf(outfile,"S3_6z_Type: %s\n", S3_6z_Type );

 showline_wlabel( "S3_7", Sched3[7] );

 for (j=1; j <= 5; j++)
  Sched3[8] = Sched3[8] + Sched3[j];
 Sched3[8] = Sched3[8] + Sched3[7];
 showline_wlabelnz( "S3_8", Sched3[8] );
 L[20] = Sched3[8];
 showline( 20 );

 GetLine( "S3_9", &Sched3[9] );		/* Net premium tax credit. Form 8962. */
 showline_wlabelnz( "S3_9", Sched3[9] );

 GetLine( "S3_10", &Sched3[10] );	/* Amnt paid in filing extension req. */
 showline_wlabelnz( "S3_10", Sched3[10] );

 GetLine( "S3_11", &Sched3[11] );	/* Excess Soc. Sec. + tier 1 RRTA tax withheld */
 showline_wlabelnz( "S3_11", Sched3[11] );

 GetLine( "S3_12", &Sched3[12] );	/* Credits for federal tax on fuels. Attach form 4136. */
 showline_wlabelnz( "S3_12", Sched3[12] );

 GetLine( "S3_13a", &Sched3_13a );	/* Credits from Form 2439 */
 showline_wlabelnz( "S3_13a", Sched3_13a );

 GetLine( "S3_13b", &Sched3_13b );	/* Credits from Form 7202 */
 showline_wlabelnz( "S3_13b", Sched3_13b );

 GetLine( "S3_13d", &Sched3_13d );	/* Credit for repayment of amounts included in income from prior years. */
 showline_wlabelnz( "S3_13d", Sched3_13d );

 GetLine( "S3_13f", &Sched3_13f );	/* Deferred amount of net 965 tax liability */
 showline_wlabelnz( "S3_13f", Sched3_13f );

 GetLine( "S3_13h", &Sched3_13h );	/* Qualified sick and family leave credits from Schedule(s) H andForm(s) 7202 */
 showline_wlabelnz( "S3_13h", Sched3_13h );

 GetLine( "S3_13z", &Sched3_13z );	/* Credits from Other */
 showline_wlabelnz( "S3_13z", Sched3_13z );

 S3_13z_Type = GetTextLine( "S3_13z_Type:" );
 printf("S3_13z_Type: %s\n", S3_13z_Type );
 fprintf(outfile,"S3_13z_Type: %s\n", S3_13z_Type );

 Sched3[14] = Sched3_13a + Sched3_13b + Sched3_13d + Sched3_13f + Sched3_13h;
 showline_wlabelnz( "S3_14", Sched3[14] );

 for (j = 9; j <= 12; j++)
  Sched3[15] = Sched3[15] + Sched3[j];
 Sched3[15] = Sched3[15] + Sched3[14];
 showline_wlabel( "S3_15", Sched3[15] );
 L[31] = Sched3[15];

 /* -- End of Schedule 3 -- */



 L[21] = L[19] + L[20];
 showline( 21 );

 L[22] = NotLessThanZero( L[18] - L[21] );
 showline( 22 );

 L[23] = Sched2[21];		/* Also set above. */
 showline( 23 );

 L[24] = L[22] + L[23];
 showline_wmsg( 24, "Total Tax" );

 Report_bracket_info( L[15], L[24], status );
 
 showline_wlabelnz( "L25a", L25a );
 showline_wlabelnz( "L25b", L25b );
 showline_wlabelnz( "L25c", L25c );
 showline_wlabelnz( "L25d", L[25] );
 showline( 26 );
 showline_wlabelnz( "L27a", L[27] );
 showline_wlabelnz( "L27b", ntcpe );
 showline_wlabelnz( "L27c", pyei );
 showline( 28 );
 showline( 29 );
 showline( 30 );
 showline( 31 );
 for (j=27; j<=31; j++)
  L[32] = L[32] + L[j];
 showline_wmsg( 32, "Total other payments and refundable credits" );
 L[33] = L[25] + L[26] + L[32];
 showline_wmsg( 33, "Total Payments" );

  /* Refund or Owe sections. */
 if (L[33] > L[24])
  { /* Refund */
   L[34] = L[33] - L[24];
   fprintf(outfile,"L34 = %6.2f  Amount you Overpaid!!!\n", L[34] );
   L[35] = L[34];
   fprintf(outfile,"L35a = %6.2f \n", L[35] );
  }
 else 
  { /* Tax-Due */
   L[37] = L[24] - L[33];
   fprintf(outfile,"L37 = %6.2f  DUE !!!\n", L[37] );
   fprintf(outfile,"         (Which is %2.1f%% of your Total Federal Tax.)\n", 100.0 * L[37] / (L[16] + 1e-9) );
  }
 ShowLineNonZero( 38 );
 fprintf(outfile,"------------------------------\n");

 
 fprintf(outfile,"\n{ --------- Identity-Information:  --------- }\n");
 Your1stName    = GetTextLineF( "Your1stName:" );
 YourLastName   = GetTextLineF( "YourLastName:" );
 writeout_line = 0;
 socsec = GetTextLineF( "YourSocSec#:" );
 strcpy( socsectmp, socsec );	/* Copy to buffer, since formatting could add 2-chars. */
 format_socsec( socsectmp, 0 );
 fprintf(outfile,"YourSocSec#: %s\n", socsectmp );
 free( socsec );
 writeout_line = 1;
 Spouse1stName  = GetTextLineF( "Spouse1stName:" );
 SpouseLastName = GetTextLineF( "SpouseLastName:" );
 writeout_line = 0;
 socsec = GetTextLineF( "SpouseSocSec#:" );
 strcpy( socsectmp, socsec );	/* Copy to buffer, since formatting could add 2-chars. */
 format_socsec( socsectmp, 0 );
 fprintf(outfile,"SpouseSocSec#: %s\n", socsectmp );
 free( socsec );
 writeout_line = 1;
 if (strlen( YourLastName ) > 0)
  {
   if (strcmp( YourLastName, SpouseLastName ) == 0)
    fprintf(outfile,"YourNames: %s & %s, %s\n", Your1stName, Spouse1stName, YourLastName );
   else
   if (strlen( SpouseLastName ) > 0)
    fprintf(outfile,"YourNames: %s %s & %s %s\n", Your1stName, YourLastName, Spouse1stName, SpouseLastName );
   else
    fprintf(outfile,"YourNames: %s %s\n", Your1stName, YourLastName );
  }
 GetTextLineF( "Number&Street:" );
 GetTextLineF( "Apt#:" );

 // GetTextLineF( "TownStateZip:" );
 GetTextLineF( "Town/City:" );
 GetTextLineF( "State:" );
 GetTextLineF( "ZipCode:" );

 GetTextLineF( "YourOccupat:" );
 GetTextLineF( "SpouseOccupat:" );

 get_word(infile, labelx );	/* Look for optional Dependent fields. */
 while (!feof(infile))
  { /*OptionalLine*/
   read_comment_filtered_line( infile, word, 512 );
   // printf("\nLine '%s' = '%s'\n", labelx, word );
   if (word[0] != '\0')
    { /*valid_entry*/
     if (strncmp( labelx,"Dep", 3 ) == 0)
      {
	if (strstr( labelx,"SocSec" ) != 0)
	  format_socsec( word, 1 );
	fprintf(outfile, "%s \"%s\"\n", labelx, word );
      }
     else
     if (strncmp( labelx,"CkDep", 5 ) == 0)
      {
	if (toupper(word[0]) == 'Y')
	 fprintf(outfile, "%s X\n", labelx );
      }
    } /*valid_entry*/
   get_word(infile, labelx );
  } /*OptionalLine*/

 fclose(infile);
 Grab_ScheduleB_Payer_Lines( infname, outfile );
 grab_any_pdf_markups( infname, outfile );
 fclose(outfile);

 printf("\nListing results from file: %s\n\n", outfname);
 Display_File( outfname );

 if (adjerrcnt != 0 ) Display_adj_code_err();

 return 0;
}

#undef CAP_GAIN_ADJUSTMENT_CODES
#undef MAXADJERRCNT
#undef SINGLE
#undef MARRIED_FILING_JOINTLY
#undef MARRIED_FILING_SEPARAT
#undef HEAD_OF_HOUSEHOLD
#undef WIDOW
#undef Yes
#undef No
}
namespace taxsolve_US_1040_Sched_C_2022 {
#define Yes 1
#define No  0
/************************************************************************/
/* TaxSolve_US1040_Sched_C.c -	 					*/
/* Copyright (C)  2022 - S.Jenkins					*/
/* 									*/
/* Compile:								*/
/*  cc taxsolve_US1040_Sched_C.c -o taxsolve_US1040_Sched_C		*/
/*									*/
/* Documentation & Updates:						*/
/*        http://opentaxsolver.sourceforge.net/				*/
/*									*/
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
/* Not updated for 2022 tax year:						*/
/*  S.Jenkins 12-20-2022   						*/
/* Earlier Updates	Robert Heller  heller@deepsoft.com		*/
/************************************************************************/

float thisversion=20.00;



/*----------------------------------------------------------------------------*/

int main( int argc, char *argv[] )
{
 int i, j, k;
 char word[4000], outfname[4000], *EIN=0, *answ, *infname=0;
 time_t now;
 double L16b=0.0, L20b=0.0, L24b=0.0, Mileage_1stHalf=0.0, Mileage_2ndHalf=0.0, 
	L44a_1stHalf=0.0, L44a_2ndHalf=0.0, L44a=0.0, L44b=0.0, L44c=0.0;
 int L32=0;
 char veh_mm[1024]="", veh_dd[1024]="", veh_yy[1024]="";
 char Ck45[1024]="", L46answ[1024]="", L47a_answ[1024]="", L47b_answ[1024]="";
 char *L48a_descr="", *L48b_descr="", *L48c_descr="", *L48d_descr="", *L48e_descr="",
      *L48f_descr="", *L48g_descr="", *L48h_descr="", *L48i_descr="";
 double L48a_amnt=0.0, L48b_amnt=0.0, L48c_amnt=0.0, L48d_amnt=0.0, L48e_amnt=0.0,
        L48f_amnt=0.0, L48g_amnt=0.0, L48h_amnt=0.0, L48i_amnt=0.0;

 printf("US 1040 Schedule C, 2022 - v%3.2f\n", thisversion);

 /* Decode any command-line arguments. */
 i = 1;  k=1;
 while (i < argc)
 {
  if (strcmp(argv[i],"-verbose")==0)  { verbose = 1; }
  else
  if (strcmp(argv[i],"-round_to_whole_dollars")==0)  { round_to_whole_dollars = 1; }
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

 /* Accept parameters from input file. */
 /* Expect  Sched C lines, something like:
	Title:  Sched C 1999 Return
	L1		{Gross Receipts}
	L2		{Returns and Allowances}
	. . .
 */

 /* Accept Form's "Title" line, and put out with date-stamp for your records. */
 read_line( infile, word );
 now = time(0);
 fprintf(outfile,"\n%s,	 v%2.2f, %s\n", word, thisversion, ctime( &now ));
 check_form_version( word, "Title:  1040 Schedule C" );

 GetTextLineF( "YourName:" );
 GetTextLineF( "YourSocSec#:" );
 GetTextLineF( "PrincipalBus:" );
 GetTextLineF( "BusinessName:" );
 GetTextLineF( "Number&Street:" );
 GetTextLineF( "TownStateZip:" );

 GetTextLineF( "ActivityCode:" );
 EIN = GetTextLine( "BusinessEIN:" );
 format_socsec( EIN, 1 );
 fprintf(outfile,"BusinessEIN: %s\n", EIN );

 answ = GetTextLine( "Fmethod:" );
 next_word( answ, word, " \t;" );
 if (strcasecmp( word, "Cash" ) == 0)
  fprintf(outfile,"CkFcash: X\n");
 else
  if (strcasecmp( word, "Accrual" ) == 0)
  fprintf(outfile,"CkFsccrual: X\n");
 else
  if (strcasecmp( word, "Other" ) == 0)
  fprintf(outfile,"CkFother: X\n");
 
 answ = GetTextLine( "GPartic:" );
 next_word( answ, word, " \t;" );
 if (toupper( word[0] ) == 'Y')
  fprintf(outfile,"CkParticipate: X\n");
 else
 if ((mystrcasestr( word, "N/A" ) == 0) && (toupper( word[0] ) == 'N'))
  fprintf(outfile,"CkNotParticipate: X\n");
 
 answ = GetTextLine( "Hacquired:" );
 next_word( answ, word, " \t;" );
 if (toupper( word[0] ) == 'Y')
  fprintf(outfile,"CkAcquired: X\n");

 answ = GetTextLine( "Ireq1099s:" );
 next_word( answ, word, " \t;" );
 if (toupper( word[0] ) == 'Y')
  fprintf(outfile,"CkReq1099: X\n");
 else
 if ((mystrcasestr( word, "N/A" ) == 0) && (toupper( word[0] ) == 'N'))
  fprintf(outfile,"CkNotReq1099: X\n");
 
 answ = GetTextLine( "Jfile1099s:" );
 next_word( answ, word, " \t;" );
 if (toupper( word[0] ) == 'Y')
  fprintf(outfile,"CkWillFile1099: X\n");
 else
 if ((mystrcasestr( word, "N/A" ) == 0) && (toupper( word[0] ) == 'N'))
  fprintf(outfile,"CkNotFile1099: X\n");
 

 GetLine( "L1", &L[1] );	/* Gross Receipts */

 GetLine( "L2", &L[2] );	/* Returns and Allowances */	

 // GetLine( "L4", &L[4] );	/* Cost of Goods Sold */

 GetLine( "L6", &L[6] );	/* Other income, including fed & state fuel credit */

 GetLine( "L8", &L[8] );	/* Advertising */

 GetLine( "L9", &L[9] );	/* Car & truck expenses */

 GetLine( "L10", &L[10] );	/* Commissions & fees */

 GetLine( "L11", &L[11] );	/* Contract labor */

 GetLine( "L12", &L[12] );	/* Depletion */

 GetLine( "L13", &L[13] );	/* Depreciation & Sec 179 exp ded */

 GetLine( "L14", &L[14] );	/* Employee benfit programs (other than line 19) */

 GetLine( "L15", &L[15] );	/* Insurance (other than health) */

 GetLine( "L16a", &L[16] );	/* Interest (mortgage paid to banks) */

 GetLine( "L16b", &L16b );	/* Interest (Other) */

 GetLine( "L17", &L[17] );	/* Legal & professional services */

 GetLine( "L18", &L[18] );	/* Office expense */

 GetLine( "L19", &L[19] );	/* Pension & profit sharing plans */

 GetLine( "L20a", &L[20] );	/* Vehicles and equiment Rent or Lease */

 GetLine( "L20b", &L20b );	/* Rent or lease Other business property */

 GetLine( "L21", &L[21] );	/* Repairs & maintenance */

 GetLine( "L22", &L[22] );	/* Supplies (not in Part III) */

 GetLine( "L23", &L[23] );	/* Taxes & licenses */

 GetLine( "L24a", &L[24] );	/* Travel */

 GetLine( "L24b", &L24b );	/* Deductable Meals & entertainment */

 GetLine( "L25", &L[25] );	/* Utilities */

 GetLine( "L26", &L[26] );	/* Wages (less employment credits) */

 // GetLine( "L27a", &L[27] );	/* Other expenses here from line 48 pg 2 */

 GetLine( "L30", &L[30] );	/* Expenses for business use of home (form 8829) */

 answ = GetTextLine( "L32a" );  /* Yes or No, All investment is at risk */
 next_word( answ, word, " \t;" );
 if (toupper( word[0] ) == 'Y')
   L32 = Yes;
 else
 if (toupper( word[0] ) == 'N')
  L32 = No;
 else
 if (word[0] != '\0')
  {
   printf("Error: Unexpected answer for Line-32a (Y/N) = '%s'.  Exiting.\n", word );
   fprintf(outfile,"Error: Unexpected answer for Line-32a (Y/N) = '%s'.  Exiting.\n", word );
   exit(1);
  }
 else
  {
   printf(" No answer for Line-32a (Y/N).\n");
   fprintf(outfile," No answer for Line-32a (Y/N).\n");
  }


 /* Part III */

 answ = GetTextLine( "L33:" );
 next_word( answ, word, " \t;" );
 if (strcasecmp( word, "Cost" ) == 0)
  fprintf(outfile,"Ck33aCost: X\n");
 else
 if (strcasecmp( word, "Market" ) == 0)
  fprintf(outfile,"Ck33bMarket: X\n");
 else
 if (strcasecmp( word, "Other" ) == 0)
  fprintf(outfile,"Ck33cOther: X\n");
 else
 if (word[0] != '\0')
  printf("Warning: Unexpected answer for L33: '%s'\n", word ); 

 answ = GetTextLine( "L34:" );
 next_word( answ, word, " \t;" );
 if (toupper( word[0] ) == 'Y')
  fprintf(outfile,"Ck34Yes: X\n");
 else
 if (toupper( word[0] ) == 'N')
  fprintf(outfile,"Ck34No: X\n");
 
 GetLine( "L35", &L[35] );	/* Inventory at beginning of year */

 GetLine( "L36", &L[36] );	/* Purchases minus cost of personel items */

 GetLine( "L37", &L[37] );	/* Cost of labor (not paid to yourself) */

 GetLine( "L38", &L[38] );	/* Materials & supplies */

 GetLine( "L39", &L[39] );	/* Other costs */

 L[40] = L[35] + L[36] + L[37] + L[38] + L[39];

 GetLine( "L41", &L[41] );	/* Inventory at year end */

 L[42] = L[40] - L[41];


 answ = GetTextLine( "L43:" );
 next_word( answ, veh_mm, " \t-/.,;" );
 next_word( answ, veh_dd, " \t-/.,;" );
 next_word( answ, veh_yy, " \t-/.,;" );
 
 GetLine( "L44a_1stHalf", &L44a_1stHalf );		/* Vehicle Miles 1/1/2022 - 6/30/2022 */
 GetLine( "L44a_2ndHalf", &L44a_2ndHalf );		/* Vehicle Miles 7/1/2022 - 12/31/2022 */
 L44a = L44a_1stHalf + L44a_2ndHalf;
 GetLine( "L44b", &L44b );
 GetLine( "L44c", &L44c );

 answ = GetTextLine( "L45:" );
 next_word( answ, Ck45, " \t;" );

 answ = GetTextLine( "L46:" );
 next_word( answ, L46answ, " \t;" );

 answ = GetTextLine( "L47a:" );
 next_word( answ, L47a_answ, " \t;" );

 answ = GetTextLine( "L47b:" );
 next_word( answ, L47b_answ, " \t;" );

 L48a_descr = GetTextLine( "L48a_descr:" );
 GetLine( "L48a_amnt", &L48a_amnt );
 L[48] = L48a_amnt;
 L48b_descr = GetTextLine( "L48b_descr:" );
 GetLine( "L48b_amnt", &L48b_amnt );
 L[48] = L[48] + L48b_amnt;
 L48c_descr = GetTextLine( "L48c_descr:" );
 GetLine( "L48c_amnt", &L48c_amnt );
 L[48] = L[48] + L48c_amnt;
 L48d_descr = GetTextLine( "L48d_descr:" );
 GetLine( "L48d_amnt", &L48d_amnt );
 L[48] = L[48] + L48d_amnt;
 L48e_descr = GetTextLine( "L48e_descr:" );
 GetLine( "L48e_amnt", &L48e_amnt );
 L[48] = L[48] + L48e_amnt;
 L48f_descr = GetTextLine( "L48f_descr:" );
 GetLine( "L48f_amnt", &L48f_amnt );
 L[48] = L[48] + L48f_amnt;
 L48g_descr = GetTextLine( "L48g_descr:" );
 GetLine( "L48g_amnt", &L48g_amnt );
 L[48] = L[48] + L48g_amnt;
 L48h_descr = GetTextLine( "L48h_descr:" );
 GetLine( "L48h_amnt", &L48h_amnt );
 L[48] = L[48] + L48h_amnt;
 L48i_descr = GetTextLine( "L48i_descr:" );
 GetLine( "L48i_amnt", &L48i_amnt );
 L[48] = L[48] + L48i_amnt;
 L[27] = L[48];

 /* -- Compute the tax form -- */

 showline(1);
 showline(2);
 L[3] = L[1] - L[2];
 showline(3);
 L[4] = L[42];
 showline(4);
 L[5] = L[3] - L[4];
 showline_wmsg(5, "Gross profit");
 showline(6);
 L[7] = L[5] + L[6];
 showline_wmsg(7, "Gross income");
 showline(8);
 Mileage_1stHalf = L44a_1stHalf;		/* Allowable "Business Mileage". */
 Mileage_2ndHalf = L44a_2ndHalf;		/* Allowable "Business Mileage". */
 L[9] = L[9] + 0.585 * Mileage_1stHalf + 0.625 * Mileage_2ndHalf;				/* Not updated for 2022. */
 showline(9);
 showline(10);
 showline(11);
 showline(12);
 showline(13);
 showline(14);
 showline(15);
 fprintf(outfile,"L16a = %6.2f\n", L[16]);
 fprintf(outfile,"L16b = %6.2f\n", L16b);
 showline(17);
 showline(18);
 showline(19);
 fprintf(outfile,"L20a = %6.2f\n", L[20]);
 fprintf(outfile,"L20b = %6.2f\n", L20b);
 showline(21);
 showline(22);
 showline(23);
 fprintf(outfile,"L24a = %6.2f\n", L[24]);
 fprintf(outfile,"L24b = %6.2f\n", L24b);
 showline(25);
 showline(26);
 showline_wlabel( "L27a", L[27] );
 L[28] = L[8] + L[9] + L[10] + L[11] + L[12] + L[13] + L[14] + L[15] + L[16] + L16b + L[17] + L[18] 
 	 + L[19] + L[20] + L20b + L[21] + L[22] + L[23] + L[24] + L24b + L[25] + L[26] + L[27];
 showline_wmsg(28,"Total expenses");
 L[29] = L[7] - L[28];
 showline(29);
 showline(30);
 L[31] = L[29] - L[30];
 showline_wmsg(31,"Net Profit (loss)");
 if (L[31] > 0.0)
  fprintf(outfile,"Enter %2.2f on Form 1040 line S1_12. Sched-SE line 2. Estates/trusts on Form 1041 line 3.\n", L[31]);
 else
 if (L[31] < 0.0)
  {
   // fprintf(outfile,"Mark box 32a accordingly\n");
   if (L32 == Yes)
    {
     fprintf(outfile,"If you checked 32a, enter %2.2f on Form 1040 line S1_12.\n", L[31]);
     fprintf(outfile,"        Estates and trusts, enter on Form 1041, line 3.\n");
     fprintf(outfile,"Ck32a: x\n");
    }
   else
    {
     fprintf(outfile,"If you checked 32b, you must attach Form 6198. Your loss may be limited.\n");
     fprintf(outfile,"Ck32b: x\n");
    }
  }

 showline(35);
 showline(36);
 showline(37);
 showline(38);
 showline(39);
 showline(40);
 showline(41);
 showline_wmsg(42,"Cost of goods sold");


 fprintf(outfile,"L43mm: %s\n", veh_mm );
 fprintf(outfile,"L43dd: %s\n", veh_dd );
 fprintf(outfile,"L43yy: %s\n", veh_yy );

 showline_wlabelnz( "L44a", L44a );
 showline_wlabelnz( "L44b", L44b );
 showline_wlabelnz( "L44c", L44c );

 if (toupper( Ck45[0] ) == 'Y')
  fprintf(outfile,"Ck45Yes: X\n");
 else
 if (strcasecmp( Ck45, "No") == 0)
  fprintf(outfile,"Ck45No: X\n");

 if (toupper( L46answ[0] ) == 'Y')
  fprintf(outfile,"Ck46Yes: X\n");
 else
 if (strcasecmp( L46answ, "No") == 0)
  fprintf(outfile,"Ck46No: X\n");

 if (toupper( L47a_answ[0] ) == 'Y')
  fprintf(outfile,"Ck47aYes: X\n");
 else
 if (strcasecmp( L47a_answ, "No") == 0)
  fprintf(outfile,"Ck47aNo: X\n");

 if (toupper( L47b_answ[0] ) == 'Y')
  fprintf(outfile,"Ck47bYes: X\n");
 else
 if (strcasecmp( L47b_answ, "No") == 0)
  fprintf(outfile,"Ck47bNo: X\n");

 if (strlen( L48a_descr ) > 0)
  fprintf(outfile,"L48a_descr: %s\n", L48a_descr );
 showline_wlabelnz( "L48a_amnt", L48a_amnt );
 if (strlen( L48b_descr ) > 0)
  fprintf(outfile,"L48b_descr: %s\n", L48b_descr );
 showline_wlabelnz( "L48b_amnt", L48b_amnt );
 if (strlen( L48c_descr ) > 0)
  fprintf(outfile,"L48c_descr: %s\n", L48c_descr );
 showline_wlabelnz( "L48c_amnt", L48c_amnt );
 if (strlen( L48d_descr ) > 0)
  fprintf(outfile,"L48d_descr: %s\n", L48d_descr );
 showline_wlabelnz( "L48d_amnt", L48d_amnt );
 if (strlen( L48e_descr ) > 0)
  fprintf(outfile,"L48e_descr: %s\n", L48e_descr );
 showline_wlabelnz( "L48e_amnt", L48e_amnt );
 if (strlen( L48f_descr ) > 0)
  fprintf(outfile,"L48f_descr: %s\n", L48f_descr );
 showline_wlabelnz( "L48f_amnt", L48f_amnt );
 if (strlen( L48g_descr ) > 0)
  fprintf(outfile,"L48g_descr: %s\n", L48g_descr );
 showline_wlabelnz( "L48g_amnt", L48g_amnt );
 if (strlen( L48h_descr ) > 0)
  fprintf(outfile,"L48h_descr: %s\n", L48h_descr );
 showline_wlabelnz( "L48ha_amnt", L48h_amnt );
 if (strlen( L48i_descr ) > 0)
  fprintf(outfile,"L48i_descr: %s\n", L48i_descr );
 showline_wlabelnz( "L48i_amnt", L48i_amnt );

 showline_wmsg( 48, "Total other expenses" );

 fclose(infile);
 grab_any_pdf_markups( infname, outfile );
 fclose(outfile);

 printf("\nListing results from file: %s\n\n", outfname);
 Display_File( outfname );

 return 0;
}

#undef Yes
#undef No
}
namespace taxsolve_US_1040_Sched_SE_2022 {
#define Yes 1
#define No  0
/************************************************************************
  TaxSolve_Form_US_1040_Sched_SE.c - Self Employment Tax Form

  NOTE: Only Part-I is implemented.
	Parts II and III are NOT presently implemented.

 Documentation & Updates:
        http://opentaxsolver.sourceforge.net/

 GNU Public License - GPL:
 This program is free software; you can redistribute it and/or
 modify it under the terms of the GNU General Public License as
 published by the Free Software Foundation; either version 2 of the
 License, or (at your option) any later version.
 
 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 General Public License for more details.
 
 You should have received a copy of the GNU General Public License
 along with this program; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
 02111-1307 USA
 
 Provided by Steve Wiswell, March 16, 2021.

 ************************************************************************/

float thisversion=3.00;



double L4a=0.0, L4c=0.0;
double L5a=0.0, L5b=0.0; 			/* Church employee income */
double L8a=0.0, L8b=0.0, L8c=0.0, L8d=0.0; 	/* Wages & Tips */

/*----------------------------------------------------------------------------*/

int main( int argc, char *argv[] )
{
 int i, j, k;
 char word[8000], outfname[8000], *infname=0;
 time_t now;

 printf("US 1040 Schedule SE, 2022 - v%1.0f\n", thisversion);

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

 /* Accept parameters from input file. */
 /* Expect lines, something like:
        Title:  1040 Schedule SE - 2022 Return
        L2              {Net Profit/Loss}
        . . .
 */


 /* Accept Form's "Title" line, and put out with date-stamp for your records. */
 read_line( infile, word );
 now = time(0);
 fprintf(outfile,"\n%s,  v%2.2f, %s\n", word, thisversion, ctime( &now ));
 check_form_version( word, "Title:  1040 Schedule SE - 2022" );


 /* ----- Accept form data and process the numbers.         ------ */
 /* ----- Place all your form-specific code below here .... ------ */

 GetTextLineF( "YourName:" );
 GetTextLineF( "YourSocSec#:" );

 GetLine( "L2", &L[2] );	/* Net Profit/Loss */	
 GetLine( "L5a", &L5a );	/* Church employee income from Form W-2 */	
 L[7] = 147000.0;           	/* Fixed value for tax year 2022 */			/* Updated for 2022. */
 GetLine( "L8a", &L8a );	/* Wages & Tips */
 GetLine( "L8b", &L8b );	/* Unreported tips from Form 4137, line 10 */
 GetLine( "L8c", &L8c );	/* Wages from Form 8919, line 10 */

 /* -- Compute the tax form  -- */
 showline(2);
 L4a = L[2] * 0.9235;									/* Updated for 2022. */
 showline_wlabel( "L4a", L4a );
 L4c = NotLessThanZero( L4a );
 showline_wlabel( "L4c", L4c );
 showline_wlabel( "L5a", L5a );
 L5b = NotLessThanZero( L5a * 0.9235 );
 showline_wlabel( "L5b", L5b );
 L[6] = L4c + L5b;
 showline(6);
 showline_wlabel("L8a", L8a);
 showline_wlabel("L8b", L8b);
 showline_wlabel("L8c", L8c);
 L8d = L8a + L8b + L8c;
 showline_wlabel("L8d", L8d);
 L[9] = NotLessThanZero( L[7] - L8d );
 showline(9);
 L[10] = 0.124 * SmallerOf( L[6], L[9]);
 showline(10);
 L[11] = L[6] * 0.029;									/* Updated for 2022. */
 showline(11);
 L[12] = L[10] + L[11];
 showline_wmsg( 12, "Also enter this number on Schedule-2, line 4." );
 L[13] = L[12] * 0.5;
 showline_wmsg( 13, "Also enter this number on Schedule-1, line 15." );

 L[14] = 6040.0;									/* Updated for 2022. */

 /* ----- .... Until here.  ----- */


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

#undef Yes
#undef No
}
namespace taxsolve_VA_760_2022 {
#define SINGLE 		        1
#define MARRIED_FILING_JOINTLY  2
#define MARRIED_FILING_SEPARAT  3
#define HEAD_OF_HOUSEHOLD       4
#define WIDOW		        5
/************************************************************************/
/* TaxSolve_VA760_2022.c - 						*/
/* Copyright (C) 2022 - Aston Roberts					*/
/* 									*/
/* Compile:   gcc taxsolve_VA760_2022.c -o taxsolve_VA760_2022		*/
/* Run:	      ./taxsolve_VA760_2022  VA_760_2022.txt 			*/
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
/* Aston Roberts 3-20-2023	aston_roberts@yahoo.com			*/
/************************************************************************/



float thisversion=20.01;


double TaxRateFunction( double income, int status )
{
 if (income < 3000.0) return income * 0.02; else
 if (income < 5000.0) return  60.0 + (income - 3000.0) * 0.03; else
 if (income < 17000.0) return 120.0 + (income - 5000.0) * 0.05; else
 return 720.0 + (income - 17000.0) * 0.0575; 
}


void Report_bracket_info( double income, double tx, int status )
{
 double rate;
 if (income < 3000.0) rate = 0.02;  else
 if (income < 5000.0) rate = 0.03;  else
 if (income < 17000.0) rate = 0.05; else  rate = 0.0575;
 printf(" You are in the %2.1f%% marginal tax bracket,\n and you are paying an effective %2.1f%% tax on your total income.\n",
	  100.0 * rate, 100.0 * tx / income );
 fprintf(outfile," You are in the %2.1f%% marginal tax bracket,\n and you are paying an effective %2.1f%% tax on your total income.\n",
	  100.0 * rate, 100.0 * tx / income );
}


struct date_record yourDOB, spouseDOB, DL;


/*----------------------------------------------------------------------------*/

int main( int argc, char *argv[] )
{
 int i, j, k;
 char word[1000], outfname[4000], *lnameptr, lastname[1024], *socsec, *datestr, *twrd, *infname=0;
 int status=0, exemptionsA=0, exemptionsB=0, youBlind=0, spouseBlind=0;
 time_t now;
 double L19b=0.0, std_ded=0.0, min2file;

 /* Intercept any command-line arguments. */
 printf("VA-760 2022 - v%3.1f\n", thisversion);
 i = 1;  k=1;
 while (i < argc)
 {
  if (strcmp(argv[i],"-verbose")==0)  verbose = 1;
  else
  if (k==1)
   {
    infname = strdup(argv[i]);
    infile = fopen(argv[i],"r");
    if (infile==0) {printf("ERROR: Parameter file '%s' could not be opened.\n", argv[i]); exit(1);}
    k = 2;
    /* Base name of output file on input file. */
    strcpy(outfname,argv[i]);
    j = strlen(outfname)-1;
    while ((j>=0) && (outfname[j]!='.')) j--;
    if (j<0) strcat(outfname,"_out.txt"); else strcpy(&(outfname[j]),"_out.txt");
    outfile = fopen(outfname,"w");
    if (outfile==0) {printf("ERROR: Output file '%s' could not be opened.\n", outfname); exit(1);}
    printf("Writing results to file:  %s\n", outfname);
   }
  else {printf("Unknown command-line parameter '%s'\n", argv[i]); exit(1);}
  i = i + 1;
 }

 if (infile==0) {printf("Error: No input file on command line.\n"); exit(1);}

 /* Pre-initialize all lines to zeros. */
 for (i=0; i<MAX_LINES; i++) 
  {
   L[i] = 0.0;
  }

 /* Accept parameters from input file. */
 /* Expect  VA-760 lines, something like:
	Title:  VA-760 1999 Return
	L1		{Wages}
*/


 /* Accept Form's "Title" line, and put out with date-stamp for records. */
 read_line( infile, word );
 now = time(0);
 fprintf(outfile,"\n%s,	 v%2.2f, %s\n", word, thisversion, ctime( &now ));
 check_form_version( word, "Title:  VA-760 State 2022" );

 /* get_parameter(infile, kind, x, emssg ) */
 get_parameter( infile, 's', word, "Status" );
 get_parameter( infile, 'l', word, "Status ?");
 if (strncasecmp(word,"Single",4)==0) status = SINGLE; else
 if (strncasecmp(word,"Married/Joint",13)==0) status = MARRIED_FILING_JOINTLY; else
 if (strncasecmp(word,"Married/Sep",11)==0) status = MARRIED_FILING_SEPARAT; else
 if (strncasecmp(word,"Head_of_House",4)==0) status = HEAD_OF_HOUSEHOLD;
 else
  { 
   printf("Error: unrecognized status '%s'. Must be: Single, Married/joint, Married/sep, Head_of_house, Widow(er)\nExiting.\n", word); 
   fprintf(outfile,"Error: unrecognized status '%s'. Must be: Single, Married/joint, Married/sep, Head_of_house, Widow(er)\nExiting.\n", word); 
   exit(1); 
  }
 fprintf(outfile,"Status = %s\n", word );
 fprintf(outfile," FilingStatus: %d\n", status );

 GetTextLineF( "Your1stName:" );
 GetTextLineF( "YourMI:" );
 lnameptr = GetTextLineF( "YourLastName:" );
 strcpy( lastname, lnameptr );
 lastname[4] = '\0';
 fprintf(outfile,"Y1st4: %s\n", lastname );
 writeout_line = 0;
 socsec = GetTextLineF( "YourSocSec#:" );
 format_socsec( socsec, 1 );
 fprintf(outfile,"YourSocSec#: %s\n", socsec );
 datestr = GetTextLineF( "YourDOB:" );
 if (datestr[0] == '\0')
  {
   show_errmsg("\nMissing 'YourDOB' -- needed to check age exemptions.\n");
   datestr = strdup( "1 / 1 / 2000");	/* Pressume under 65. */
  }
 if (interpret_date( datestr, &(yourDOB.month), &(yourDOB.day), &(yourDOB.year), "Bad YourDOB" ) != 1)
   exit(1);
 twrd = format_mmddyyyy( yourDOB.month, yourDOB.day, yourDOB.year );
 fprintf(outfile,"YourDOB: %s\n", twrd );
 writeout_line = 1;
 GetTextLineF( "YourDrivLic:" );
 writeout_line = 0;
 datestr = GetTextLineF( "YourDLdate:" );
 if ((datestr[0] != '\0') && (interpret_date( datestr, &(DL.month), &(DL.day), &(DL.year), "Bad YourDL" )))
  {
   twrd = format_mmddyyyy( DL.month, DL.day, DL.year );
   fprintf(outfile,"YourDLdate: %s\n", twrd );
  }
 writeout_line = 1;

 GetTextLineF( "Spouse1stName:" );
 GetTextLineF( "SpouseMI:" );
 lnameptr = GetTextLineF( "SpouseLastName:" );
 strcpy( lastname, lnameptr );
 lastname[4] = '\0';
 fprintf(outfile,"S1st4: %s\n", lastname );
 writeout_line = 0;
 socsec = GetTextLineF( "SpouseSocSec#:" );
 format_socsec( socsec, 1 );
 fprintf(outfile,"SpouseSocSec#: %s\n", socsec );
 datestr = GetTextLineF( "SpouseDOB:" );
 if (status == MARRIED_FILING_JOINTLY)
  {
   if (datestr[0] == '\0')
    {
     show_errmsg("\nMissing 'SpouseDOB' -- needed to check age exemptions.\n");
     datestr = strdup( "1 / 1 / 2000");   /* Pressume under 65. */
    }
   if (interpret_date( datestr, &(spouseDOB.month), &(spouseDOB.day), &(spouseDOB.year), "Bad SpouseDOB" ) != 1)
     exit(1);
  twrd = format_mmddyyyy( spouseDOB.month, spouseDOB.day, spouseDOB.year );
  fprintf(outfile,"SpouseDOB: %s\n", twrd );
 }
 writeout_line = 1;
 GetTextLineF( "SpouseDrivLic:" );
 writeout_line = 0;
 datestr = GetTextLineF( "SpouseDLdate:" );
 if ((datestr[0] != '\0') && (interpret_date( datestr, &(DL.month), &(DL.day), &(DL.year), "Bad YourDL" )))
  {
   twrd = format_mmddyyyy( DL.month, DL.day, DL.year );
   fprintf(outfile,"SpouseDLdate: %s\n", twrd );
  }
 writeout_line = 1;

 GetTextLineF( "Number&Street:" );
 GetTextLineF( "Town:" );
 fprintf(outfile,"State: VA\n");
 GetTextLineF( "Zipcode:" );
 fprintf(outfile,"\n{ --------- }\n");

 get_parameter( infile, 's', word, "OtherDependents" );	/* Dependents not including you or spouse. */
 get_parameters( infile, 'i', &exemptionsA, "OtherDependents");
 if (exemptionsA > 0) 
  fprintf(outfile,"ExmpDeps: %d\n", exemptionsA );
 if (status == MARRIED_FILING_JOINTLY)
  exemptionsA = 2 + exemptionsA;
 else
  exemptionsA = 1 + exemptionsA; 
 fprintf(outfile,"NExemptionsA = %d\n", exemptionsA );
 fprintf(outfile,"ExemptionsA = %d\n", 930 * exemptionsA );

 if (yourDOB.year < 1958)			/* Updated for 2022. */
  {
   fprintf(outfile,"YouOver65 = 1\n" );		/* You are 65 or over. */
   exemptionsB = 1;
  }
 else
  exemptionsB = 0;

 /* Handle Exemptions for:  YouOver65:      SpOver65:       YouBlind:     SpBlind:       */

 get_parameter( infile, 's', word, "YouBlind" );   
 get_parameter( infile, 'b', &youBlind, "YouBlind"); 
 if (youBlind != 0)
  {
   fprintf(outfile,"YouBlind = 1\n");
   exemptionsB++;
  }

 get_parameter( infile, 's', word, "SpouseBlind" );
 get_param_single_line( infile, 'b', &spouseBlind, "SpouseBlind"); 
 if (status == MARRIED_FILING_JOINTLY)
  {
   if (spouseDOB.year < 1958)			/* Updated for 2022. */
    {
     fprintf(outfile,"SpouseOver65 = 1\n" );	/* Spouse is 65 or over. */
     exemptionsB++;
    }
   if (spouseBlind != 0)
    {
     fprintf(outfile,"SpouseBlind = 1\n");
     exemptionsB++;
    }
  }

 fprintf(outfile,"NExemptionsB = %d\n", exemptionsB );
 fprintf(outfile,"ExemptionsB = %d\n", 800 * exemptionsB );

 if (status == MARRIED_FILING_JOINTLY)
  {
   fprintf(outfile,"ExmpSpouse:  1\n");
   if (exemptionsA > 2)
    fprintf(outfile,"ExmpDeps: %d\n", exemptionsA - 2 );
  }
 else
 if (exemptionsA > 1)
  fprintf(outfile,"ExmpDeps: %d\n", exemptionsA - 1 );

 GetLineF( "L1", &L[1] );	/* Federal Adjusted Gross Income */

 GetLineF( "L2", &L[2] );	/* Additions from attached Schedule ADJ, line 3 */

 L[3] = L[1] + L[2];
 showline(3);

 GetLineF( "L4", &L[4] );	/* Deduction for age on Jan 1, 2022. */

 GetLineF( "L5", &L[5] );	/* Social Security Act, Tier 1 Railroad Retirement Act benef. */

 GetLineF( "L6", &L[6] );	/* State Income Tax refund or overpayment credit */

 GetLineF( "L7", &L[7] );	/* Subtractions from Schedule ADJ, line 7 */

 L[8] = L[4] + L[5] + L[6] + L[7];
 showline(8);

 L[9] = L[3] - L[8]; 
 showline(9);			/* Virginia Adjusted Gross Income (VAGI) */

 GetLineF( "L10", &L[10] );	/* Deductions - Std or Itemized minus income taxes */

 switch (status)
  {							/* Updated for 2022. */
   case SINGLE:  		  std_ded = 8000.0;  min2file = 11950.0;  break;
   case MARRIED_FILING_JOINTLY:  std_ded = 16000.0;  min2file = 23900.0;  break;
   case MARRIED_FILING_SEPARAT:  std_ded = 8000.0;  min2file = 11950.0;  break;
   default:  printf("Unexpected status.\n");
	     fprintf(outfile,"Unexpected status.\n");
	     exit(1);  
	break;
  }

 if (L[10] == 0.0)
   L[11] = std_ded;
 showline(11);

 L[12] = 930.0 * exemptionsA + 800.0 * exemptionsB;
 showline(12);
  
 GetLineF( "L13", &L[13] );	/* Deductions from Virginia Adjusted Gross Income Schedule ADJ, Line 9. */

 L[14] = L[10] + L[11] + L[12];
 showline(14);

 L[15] = L[9] - L[14];
 showline_wmsg( 15, "Virginia Taxable Income" );

 L[16] = TaxRateFunction( L[15], status );
 showline(16);
 Report_bracket_info( L[15], L[16], status );

 GetLine( "L17", &L[17] );	/* Spouse Tax Adjustment. */
 showline(17);

 L[18] = L[16] - L[17];
 showline_wmsg( 18, "Net Amount of Tax" );	

 GetLineF( "L19a", &L[19] );	/* Virginia tax withheld for 2022. */
 GetLineF( "L19b", &L19b );	/* Spouse's Virginia tax withheld. */

 GetLineF( "L20", &L[20] );	/* Estimated tax paid for 2022. (form 760ES) */

 GetLineF( "L21", &L[21] );	/* Amount of last year's overpayment applied toward 2022 estimated tax. */

 GetLineF( "L22", &L[22] );	/* Extension payments (form 760E). */

 GetLine( "L23", &L[23] );	/* Tax Credit, Low Income Individuals (Sch. ADJ, line 17) */

 if (L[23] > L[18])
  L[23] = L[18];	/* Low-Income Credit cannot exceed tax liability. */

 if ((L[23] > 0.0) && (exemptionsB > 0.0))
  {
   fprintf(outfile," Cannot claim both Low-Income Credit and Age or Blind Exemptions.\n");
   L[23] = 0.0;	/* Cannot claim both low-income credit and exemptions. */
  }
 showline(23);

 GetLineF( "L24", &L[24] );	/* Credit, Tax Paid to other State (Sched OSC, line 21 ...) */
 GetLineF( "L25", &L[25] );	/* Credits from enclosed Schedule CR, Section 5, Part 1, Line 1A */

 L[26] = L[19] + L19b + L[20] + L[21] + L[22] + L[23] + L[24] + L[25];
 showline(26);

 if (L[26] < L[18])
  {
   L[27] = L[18] - L[26];
   showline_wmsg( 27, "Tax You Owe" );
  }
 else
  {
   L[28] = L[26] - L[18];
   showline_wmsg( 28, "Your Tax OverPayment" );
  }

 GetLineF( "L29", &L[29] );	/* Amount of overpayment you want credited to next year's estimated tax. */
 GetLineF( "L30", &L[30] );	/* Virginia College Savings Plan Contributions from Schedule VAC, Section I, Line 6. */
 GetLineF( "L31", &L[31] );	/* Other voluntary contribitions. */
 GetLineF( "L32", &L[32] );	/* Addition to Tax, Penalty and Interest from attached Schedule ADJ, Line 21 */
 GetLineF( "L33", &L[33] );	/* Consumer's Use Tax. */

 for (j=29; j < 33; j++)
   L[34] = L[34] + L[j];
 showline(34);

 if (L[27] > 0.0)
  {
   L[35] = L[27] + L[34];
   showline_wmsg( 35, "AMOUNT DUE" );
   fprintf(outfile,"         (Which is %2.1f%% of your total tax.)\n", 100.0 * L[35] / (L[18] + 1e-9) );
  }
 else
 if (L[28] < L[34])
  {
   L[35] = L[34] - L[28];
   showline_wmsg( 35, "AMOUNT DUE" );
   fprintf(outfile,"         (Which is %2.1f%% of your total tax.)\n", 100.0 * L[35] / (L[19] + 1e-9) );
  }
 else
 if (L[28] > L[34])
  {
   L[36] = L[28] - L[34];
   showline_wmsg( 36, "YOUR REFUND" );
  }

 if (L[9] < min2file)
  {
   fprintf(outfile,"\nYour VAGI is less than the minimum required to file a return.\n");
   if (L[19] + L19b + L[20] > 0.0)
    fprintf(outfile," But you need to file return to receive refund of withheld taxes.\n");
   else
    fprintf(outfile,"You do not need to file return.  Your VA Tax is zero.\n");
  }

 fclose(infile);
 grab_any_pdf_markups( infname, outfile );
 fclose(outfile);
 Display_File( outfname );
 printf("\nResults written to file:  %s\n", outfname);
 return 0;
}

#undef SINGLE
#undef MARRIED_FILING_JOINTLY
#undef MARRIED_FILING_SEPARAT
#undef HEAD_OF_HOUSEHOLD
#undef WIDOW
}
namespace taxsolve_f2210_2022 {
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
}
namespace taxsolve_f8829_2022 {
#define FIRST_DIRECT_INDIRECT_LINE  9
#define LAST_DIRECT_INDIRECT_LINE  23
/************************************************************************/
/* taxsolve_f8829.c -                                                   */
/* Contributed by Rylan Luke, 1/2023					*/
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

float thisversion=1.00;




/*----------------------------------------------------------------------------*/

//====== Schedule C Import ======
typedef struct FED_SCH_C_IMP_F8995_T {
        double L29;
        char *YourName;
        char *YourSocSec;
} FED_SCH_C_IMP_F8995_T, *P_FED_SCH_C_IMP_F8995;

FED_SCH_C_IMP_F8995_T f_sch_c;

// Mapping from label name to address of local data struct element; either double or char*
static FORM_IMPORT_DEF f_sch_c_imp_defs[] = {
        { "L29", &f_sch_c.L29, NULL },
        { "YourName:", NULL, &f_sch_c.YourName },
        { "YourSocSec#:", NULL, &f_sch_c.YourSocSec }
};

int f_sch_c_imp_defs_size = sizeof(f_sch_c_imp_defs)/sizeof(FORM_IMPORT_DEF);

int main( int argc, char *argv[] )
{
 int i, j, k;
 char word[6000], outfname[6000], *infname=0;
 time_t now;
 // Local line number variables for direct/indirect lines
 double La[LAST_DIRECT_INDIRECT_LINE+2]={0};
 double Lb[LAST_DIRECT_INDIRECT_LINE+2]={0};

 printf("Form 8829, 2022 - v%3.2f\n", thisversion);

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
 check_form_version( word, "Title: 2022 Form 8829" );

 /* ----- Accept form data and process the numbers.         ------ */
 /* ----- Place all your form-specific code below here .... ------ */

 // Example:
 //  GetLineF( "L2", &L[2] );
 //  GetLineF( "L3", &L[3] );
 //  L[4] = L[2] - L[3];
 //  showline_wlabel( "L4", L[4] );

 char *f_sch_c_filename = GetTextLine( "FileNameSchC") ;
 printf("f_sch_c_filename: '%s'\n", f_sch_c_filename);

 int autocalc = 0;

 if (strlen(f_sch_c_filename) != 0) {
     ImportReturnData( f_sch_c_filename, f_sch_c_imp_defs, f_sch_c_imp_defs_size);
     fprintf( outfile, "INFO: --- Imported Sch C Data from file '%s' ---\n", f_sch_c_filename);
     fprintf( outfile, "INFO: Sch C L29 --  %6.2f\n", f_sch_c.L29);
     fprintf( outfile, "INFO: Sch C YourName: -- %s\n", f_sch_c.YourName);
     fprintf( outfile, "INFO: Sch C YourSocSec#: -- %s\n", f_sch_c.YourSocSec);
     autocalc = 1;
 } else {
     fprintf( outfile, "INFO: --- No Imported Schedule C Form Data : no filename provided ---\n");
     autocalc = 0;
 }


 char *info;
 info = GetTextLine( "YourName:" );
 if ((strlen(info) == 0) && autocalc) {
    info = f_sch_c.YourName;
 }
 fprintf( outfile, "YourName: %s\n", info);

 // If blank, fill in from schedule C
 info = GetTextLine( "YourSocSec#:" );
 if ((strlen(info) == 0) && autocalc) {
    info = f_sch_c.YourSocSec;
 }
 fprintf( outfile, "YourSocSec#: %s\n", info);


 GetLineFnz( "L1", &L[1] );
 GetLineFnz( "L2", &L[2] );

 if (L[2] != 0.0)
    L[3] = L[1]/L[2] * 100.0;
 else
    L[3] = 0.0;
 ShowLineNonZero( 3 );

 GetLineFnz( "L4", &L[4] );
 GetLineFnz( "L5", &L[5] );
 if (L[5] != 0.0)
    L[6] = L[4]/L[5];
 else
    L[6] = 0.0;
 ShowLineNonZero( 6 );

 // L[4] is being non-zero is used to indicate that this is a
 // daycare business with non-exclusive use of the space.
 // For all other businesses, or for daycare with exclusive
 // use of the space, leave L[4] at 0.0
 L[7] = (L[4] != 0.0) ? (L[6] * L[3]) : L[3];

 
 ShowLineNonZero( 7 );

 GetLine( "L8", &L[8] );
 // If L8 == 0, and autocalc is set, fill in value directly
 // from Schedule C.
 if (L[8] == 0.0 && autocalc) {
    L[8] = f_sch_c.L29;
 }
 showline( 8 );

 // NOTE: "La" and "Lb" are local line arrays, used for the
 // direct and indirect expense categories in lines 9 through 23.
 // These arrays are dimensioned so that the actual line numbers are
 // used as indexes, so indexes 0 through 8 are not used.

 // Get the direct expenses from lines 9-12, and then lines 16-23
 GetLine("L9a",  &La[9]);
 GetLine("L10a", &La[10]);
 GetLine("L11a", &La[11]);
 GetLine("L16a", &La[16]);
 GetLine("L17a", &La[17]);
 GetLine("L18a", &La[18]);
 GetLine("L19a", &La[19]);
 GetLine("L20a", &La[20]);
 GetLine("L21a", &La[21]);
 GetLine("L22a", &La[22]);

 // Get the indirect expenses from lines 9-12, and then lines 16-23
 GetLine("L9b",  &Lb[9]);
 GetLine("L10b", &Lb[10]);
 GetLine("L11b", &Lb[11]);
 GetLine("L16b", &Lb[16]);
 GetLine("L17b", &Lb[17]);
 GetLine("L18b", &Lb[18]);
 GetLine("L19b", &Lb[19]);
 GetLine("L20b", &Lb[20]);
 GetLine("L21b", &Lb[21]);
 GetLine("L22b", &Lb[22]);
 // Get carryover from prior year
 GetLine("L25", &L[25]);

 // Sum the first block of direct expenses
 La[12] = La[9] + La[10] + La[11];

 // Sum the first block of indirect expenses
 Lb[12] = Lb[9] + Lb[10] + Lb[11];

 // Scale indirect expenses by business percentage
 L[13] = Lb[12] * (L[7] / 100.0);

 // Sum first block of direct and indirect expenses
 L[14] = La[12] + L[13];

 // Subtract first block sum from business income
 L[15] = L[8] - L[14];
 if (L[15] < 0.0)
    L[15] = 0.0;

 // Output all first block numbers
 showline_wlabelnz("L9a",  La[9]);
 showline_wlabelnz("L10a", La[10]);
 showline_wlabelnz("L11a", La[11]);
 showline_wlabelnz("L12a", La[12]);

 showline_wlabelnz("L9b",  Lb[9]);
 showline_wlabelnz("L10b", Lb[10]);
 showline_wlabelnz("L11b", Lb[11]);
 showline_wlabelnz("L12b", Lb[12]);
 showline_wlabelnz("L13", L[13]);

 showline( 14 );
 showline( 15 );

 // Sum the second block of direct expenses
 La[23] = La[16] + La[17] + La[18] + La[19] + La[20] + La[21] + La[22];
 
 // Sum the second block of indirect expenses
 Lb[23] = Lb[16] + Lb[17] + Lb[18] + Lb[19] + Lb[20] + Lb[21] + Lb[22];

 // Scale indirect expenses by business percentage
 L[24] = Lb[23] * (L[7] / 100.0);

 L[26] = La[23] + L[24] + L[25];
 L[27] = SmallerOf( L[15], L[26] );
 L[28] = L[15] - L[27];

 // Output all second block numbers
 // Direct expenses
 showline_wlabelnz("L16a", La[16]);
 showline_wlabelnz("L17a", La[17]);
 showline_wlabelnz("L18a", La[18]);
 showline_wlabelnz("L19a", La[19]);
 showline_wlabelnz("L20a", La[20]);
 showline_wlabelnz("L21a", La[21]);
 showline_wlabelnz("L22a", La[22]);
 showline_wlabelnz("L23a", La[23]);

 // Indirect expenses
 showline_wlabelnz("L16b", Lb[16]);
 showline_wlabelnz("L17b", Lb[17]);
 showline_wlabelnz("L18b", Lb[18]);
 showline_wlabelnz("L19b", Lb[19]);
 showline_wlabelnz("L20b", Lb[20]);
 showline_wlabelnz("L21b", Lb[21]);
 showline_wlabelnz("L22b", Lb[22]);
 showline_wlabelnz("L23b", Lb[23]);

 showline_wlabelnz("L24", L[24]);
 showline_wlabelnz("L25", L[25]);

 ShowLineNonZero( 26 );
 showline( 27 );
 showline( 28 );

 // Excess casualty loss
 GetLineFnz( "L29", &L[29] );

 // Need to get the rest of the form's data, as L[30] is a copy of L[42]
 GetLine( "L31", &L[31] );
 GetLine( "L35", &L[35] );
 GetLine( "L37", &L[37] );
 GetLine( "L38", &L[38] );
 GetLine( "L41", &L[41] );

 // Calculate depreciation in Part III
 L[39] = L[37] - L[38];
 L[40] = (L[7] / 100.0) * L[39];
 L[42] = L[40] * (L[41] / 100.0);

 // Copy depreciation allowable back to line 30 in Part II
 L[30] = L[42];

 // Now, continue where we left off, on Line 32 in part II
 L[32] = L[29] + L[30] + L[31];
 L[33] = SmallerOf(L[28], L[32] );
 L[34] = L[14] + L[27] + L[33];
 L[36] = L[34] - L[35];

 // Part IV
 // Operating expenses to next year
 L[43] = NotLessThanZero(L[26] - L[27]);
 // Carryover of excess casualty losses and depreciation to next year
 L[44] = NotLessThanZero(L[32] - L[33]);

 ShowLineNonZero( 30 );
 ShowLineNonZero( 31 );
 ShowLineNonZero( 32 );
 showline       ( 33 );
 ShowLineNonZero( 34 );
 ShowLineNonZero( 35 );
 ShowLineNonZero( 36 );
 ShowLineNonZero( 37 );
 ShowLineNonZero( 38 );
 ShowLineNonZero( 39 );
 ShowLineNonZero( 40 );
 ShowLineNonZero( 41 );
 showline       ( 42 );
 showline       ( 43 );
 showline       ( 44 );

 /* ----- .... Until here.  ----- */


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


#undef FIRST_DIRECT_INDIRECT_LINE
#undef LAST_DIRECT_INDIRECT_LINE
}
namespace taxsolve_f8995_2022 {
/************************************************************************/
/* taxsolve_f8995.c -                                                   */
/* Contributed by Rylan Luke, 1/2023					*/
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

float thisversion=1.00;



//====== Form 1040 Import ======
// Imported form data; add elements as needed, using the same variable name as in the imported form label for clarity.
// Also update 'f1040_imp_defs' to add mapping.
typedef struct FED_1040_IMP_F8995_T {
        double L11;
        double L12;
        double S1_3;
        double S1_15;
        double S1_16;
        double S1_17;
        char *Your1stName;
        char *YourLastName;
        char *YourSocSec;
} FED_1040_IMP_F8995, P_FED_1040_IMP_F8995;

FED_1040_IMP_F8995 f1040i;

// Mapping from label name to address of local data struct element; either double or char*
static FORM_IMPORT_DEF f1040_imp_defs[] = {
        { "L11", &f1040i.L11, NULL },
        { "L12", &f1040i.L12, NULL },
        { "S1_3", &f1040i.S1_3, NULL },
        { "S1_15", &f1040i.S1_15, NULL },
        { "S1_16", &f1040i.S1_16, NULL },
        { "S1_17", &f1040i.S1_17, NULL },
        { "Your1stName:", NULL, &f1040i.Your1stName },
        { "YourLastName:", NULL, &f1040i.YourLastName },
        { "YourSocSec#:", NULL, &f1040i.YourSocSec }
};

int f1040_imp_defs_size = sizeof(f1040_imp_defs)/sizeof(FORM_IMPORT_DEF);

//====== Schedule C Import ======
typedef struct FED_SCH_C_IMP_F8995_T {
        double L7;
        double L31;
} FED_SCH_C_IMP_F8995_T, *P_FED_SCH_C_IMP_F8995;

FED_SCH_C_IMP_F8995_T f_sch_c;

// Mapping from label name to address of local data struct element; either double or char*
static FORM_IMPORT_DEF f_sch_c_imp_defs[] = {
        { "L7", &f_sch_c.L7, NULL },
        { "L31", &f_sch_c.L31, NULL },
};

int f_sch_c_imp_defs_size = sizeof(f_sch_c_imp_defs)/sizeof(FORM_IMPORT_DEF);

/*----------------------------------------------------------------------------*/

int main( int argc, char *argv[] )
{
 int i, j, k;
 char word[6000], outfname[6000], *infname=0;
 time_t now;
 // Local line number variables for direct/indirect lines

 printf("Form 8995, 2022 - v%3.2f\n", thisversion);

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
 check_form_version( word, "Title: 2022 Form 8995" );

 /* ----- Accept form data and process the numbers.         ------ */
 /* ----- Place all your form-specific code below here .... ------ */


 char *f1040_filename = GetTextLine( "FileName1040") ;

 if (strlen(f1040_filename) != 0) {
     ImportReturnData( f1040_filename, f1040_imp_defs, f1040_imp_defs_size);
     fprintf( outfile, "INFO: --- Imported 1040 Data from file '%s' ---\n", f1040_filename);
     fprintf( outfile, "INFO: f1040i.L11   -- %6.2f\n", f1040i.L11);
     fprintf( outfile, "INFO: f1040i.L12  -- %6.2f\n", f1040i.L12);
     fprintf( outfile, "INFO: f1040i.S1_15   -- %6.2f\n", f1040i.S1_15);
     fprintf( outfile, "INFO: f1040i.S1_16   -- %6.2f\n", f1040i.S1_16);
     fprintf( outfile, "INFO: f1040i.S1_17   -- %6.2f\n", f1040i.S1_17);
     fprintf( outfile, "INFO: f1040i.S1_3  -- %6.2f\n", f1040i.S1_3);
     fprintf( outfile, "INFO: f1040i.Your1stName: -- %s\n", f1040i.Your1stName);
     fprintf( outfile, "INFO: f1040i.YourLastName: -- %s\n", f1040i.YourLastName);
     fprintf( outfile, "INFO: f1040i.YourSocSec#: -- %s\n", f1040i.YourSocSec);
 } else {
     fprintf( outfile, "ERROR: --- No Imported 1040 Form Data : no filename provided ---\n");
     exit(1);
 }

 char *f_sch_c_filename = GetTextLine( "FileNameSchC") ;
 printf("f_sch_c_filename: '%s'\n", f_sch_c_filename);

 if (strlen(f_sch_c_filename) != 0) {
     ImportReturnData( f_sch_c_filename, f_sch_c_imp_defs, f_sch_c_imp_defs_size);
     fprintf( outfile, "INFO: --- Imported Schedule C Data from file '%s' ---\n", f_sch_c_filename);
     fprintf( outfile, "INFO: f_sch_c.L7  --  %6.2f\n", f_sch_c.L7);
     fprintf( outfile, "INFO: f_sch_c.L31 --  %6.2f\n", f_sch_c.L31);
 } else {
     fprintf( outfile, "INFO: --- No Imported Schedule C Form Data : no filename provided ---\n");
 }

 // Set autocalculate mode only if both filenames are provided.
 int auto_calc = (strlen(f1040_filename) != 0) && (strlen(f_sch_c_filename) != 0);



 // Example:
 //  GetLineF( "L2", &L[2] );
 //  GetLineF( "L3", &L[3] );
 //  L[4] = L[2] - L[3];
 //  showline_wlabel( "L4", L[4] );

 // GetTextLine( "YourName:" );
 // GetTextLine( "YourSocSec#:" );
 fprintf(outfile, "YourName: %s%s\n", f1040i.Your1stName, f1040i.YourLastName);
 fprintf(outfile, "YourSocSec#: %s\n", f1040i.YourSocSec);

 // showline_wlabel( "INFO: Net QBI Income (this form L1-<row>-c)", f1040i.S1_3 - f1040i.S1_26);
 // showline_wlabel( "INFO: Form 1040 Line 11", f1040i.L11);
 // showline_wlabel( "INFO: Form 1040 Line 12c", f1040i.L12);
 // showline_wlabel( "INFO: Taxable Income Before QBI Deduction (this form L11)", f1040i.L11 - f1040i.L12);

 char * L1_row_names[] = { "i", "ii", "iii", "iv", "v" };
 char * L1_col_names[] = { "a", "b", "c" };

 double L1[sizeof(L1_row_names)/sizeof(char *)][sizeof(L1_col_names)/sizeof(char *)];
 char   L1_name[sizeof(L1_row_names)/sizeof(char *)][sizeof(L1_col_names)/sizeof(char *)][100];
 int row, col;

 // Set total income value to 0
 L[2] = 0.0;
 for (row = 0; row < sizeof(L1_row_names)/sizeof(char *); ++row) {
    double row_val;
    char *row_name="";
    for (col = 0; col < sizeof(L1_col_names)/sizeof(char *); ++col) {
        sprintf(L1_name[row][col], "L1_%s_%s%s", L1_row_names[row], L1_col_names[col], (col == 2) ? "" : ":");
        if (col == 0) { // Name business
            row_name = GetTextLineF(L1_name[row][col]) ;
        } else if (col == 1) {
            GetTextLineF(L1_name[row][col]) ;
        } else if (col == 2) {  // qualified business income (loss)
            GetLine(L1_name[row][col], &row_val );
            // If zero value was entered, and this row has a non-blank name, 
            // and both a 1040 and schedule C filename have been provided (auto_calc), calculate the total
            if ((row_val == 0.0) && (strlen(row_name) > 0) && auto_calc) {
                fprintf(outfile, "INFO: Auto calculating QBI profit/loss for L1_%s_%s\n", L1_row_names[row], L1_col_names[col]);
                L1[row][2] =  f_sch_c.L31 - (f1040i.S1_15 + f1040i.S1_16 + f1040i.S1_17);
                fprintf(outfile, "INFO: L1_%s_%s = %6.2f = f_sch_c.L31 - (f1040i.S1_15 + f1040i.S1_16 + f1040i.S1_17) = "
                    "%6.2f - (%6.2f + %6.2f + %6.2f)\n",  
                    L1_row_names[row], L1_col_names[col], 
                    L1[row][2], f_sch_c.L31, f1040i.S1_15, f1040i.S1_16, f1040i.S1_17);
            } else {
                L1[row][2] =  row_val;
            }
            showline_wlabelnz( L1_name[row][col], L1[row][2] );
        }
    }
    // Add to total income
    L[2] += L1[row][2];
 }

 GetLine( "L3", &L[3] );
 GetLine( "L6", &L[6] );
 GetLine( "L7", &L[7] );


 // double L11_prelim;
 // GetLine( "L11", &L11_prelim );
 // 
 // if ((L11_prelim == 0.0) && auto_calc) {
 //     fprintf(outfile, "INFO: Auto calculating QBI L11\n");
 //     L[11] = f1040i.L11 - f1040i.L12;
 //     fprintf(outfile, "INFO: %6.2f = f1040i.L11 - f1040i.L12 = %6.2f - %6.2f\n",  L[11], f1040i.L11, f1040i.L12);
 // } else {
 //    L[11] = L11_prelim;
 // }

 // Calculate line 11 from 1040 line 11 and line 12 values
 L[11] = f1040i.L11 - f1040i.L12;
 fprintf(outfile, "INFO: Line 11 = %6.2f = f1040i.L11 - f1040i.L12 = %6.2f - %6.2f\n",  L[11], f1040i.L11, f1040i.L12);

 GetLine( "L12", &L[12] );


 double qbi_percentage = 0.20;

 // Total qualified business income
 L[4] = NotLessThanZero(L[2] + L[3]);
 // Qualified business income component; mult by 20%
 L[5] = L[4] * qbi_percentage;
 // Total qualified REIT dividends and PTP income
 L[8] = NotLessThanZero(L[6] + L[7]);
 // REIT and PTP component
 L[9] = L[8] * qbi_percentage;

 L[10] = L[5] + L[9];
 L[13] = NotLessThanZero( L[11] - L[12] );
 L[14] = L[13] * qbi_percentage;
 L[15] = SmallerOf( L[10], L[14] );

 L[16] = L[2] + L[3];
 if (L[16] > 0.0)
    L[16] = 0.0;

 L[17] = L[6] + L[7];
 if (L[17] > 0.0)
    L[17] = 0.0;

 showline( 2 );
 ShowLineNonZero( 3 );
 showline( 4 );
 showline( 5 );

 ShowLineNonZero( 6 );
 ShowLineNonZero( 7 );
 showline       ( 8 );
 showline       ( 9 );

 showline( 10 );
 showline( 11 );
 ShowLineNonZero( 12 );
 showline( 13 );
 showline( 14 );
 showline( 15 );
 ShowLineNonZero( 16 );
 ShowLineNonZero( 17 );



 /* ----- .... Until here.  ----- */


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


}
#undef MAX_LINES
}
#undef system(...)
#undef printf(...)
