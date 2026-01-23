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
namespace OpenTaxSolver2019 {
namespace taxsolve_US_1040_2019 {

#define MAX_LINES 1000
/************************************************************************/
/* TaxSolve_Routines.c - General purpose reusable routines for making	*/
/*  tax programs.  These routines are not specific to any particular	*/
/*  tax form or country.  This file is usually compiled-with, linked-	*/
/*  with, or included-in a form-specific program.			*/
/* 									*/
/* Copyright (C) 2003, 2004 - Aston Roberts				*/
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
/* Aston Roberts 1-1-2004	aston_roberts@yahoo.com			*/
/************************************************************************/



double L[MAX_LINES];	/* Declare the Line entry variables. */
char errmsg[10000];

FILE *infile=0,	 /* Main input file to be used for reading tax input data. */
     *outfile=0; /* Main output file. */
int verbose=0;	 /* Declare and set the "verbosity" flag. */
int notappvalue=0;
int single_line_entry=0;
int whole_line_entry=0;

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
  word[j]=getc(infile);
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
        if (word[j]=='{') do word[j] = getc(infile); while ((!feof(infile)) && (word[j]!='}'));
	if (word[j]==',') word[j] = getc(infile);
      } 
   while ((!feof(infile)) && ((word[j]!=spc) && (word[j]!='\t') && (word[j]!='\n') && (word[j]!=';')));
   if (word[j]==';') ungetc(word[j],infile);
  }
 word[j] = '\0';	/* Add termination character. */
 if (verbose) printf("Read: '%s'\n", word);
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


/*------------------------------------------------------------------------------*/
/* Get Parameter - Get a single value.						*/
/*   Expect value kinds:  'i'=integer, 'f'=float, 's'=string, 'b'=boolean.	*/
/*------------------------------------------------------------------------------*/
void get_parameter( FILE *infile, char kind, void *x, char *emssg )
{
 char word[2048], *owrd;
 int i, *ii;
 double y, *yy;

 if (kind=='w') 
  { single_line_entry = 1;  whole_line_entry = 1; }

 get_word(infile, word);

 if (feof(infile))
  {
   printf("ERROR: Unexpected EOF on '%s'\n",emssg);
   if (outfile) fprintf(outfile,"ERROR: Unexpected EOF on '%s'\n",emssg);
   exit(1);
  }
 if (kind=='i')
  {
   if (sscanf(word,"%d",&i)!=1)
    {printf("ERROR: Bad integer '%s', reading %s.\n", word, emssg); fprintf(outfile,"ERROR: Bad integer '%s', reading %s.\n", word, emssg); exit(1); }
   ii = (int *)x;
   *ii = i;
  }
 else
 if (kind=='f')
  {
   if (sscanf(word,"%lf",&y)!=1) 
    {printf("ERROR: Bad float '%s', reading %s.\n", word, emssg); fprintf(outfile,"ERROR: Bad float '%s', reading %s.\n", word, emssg); exit(1); }
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
   else if ((strcasecmp(word,"TRUE")==0) || (strcasecmp(word,"YES")==0) || (strcmp(word,"1")==0)) i = 1;
   else if ((strcasecmp(word,"FALSE")==0) || (strcasecmp(word,"NO")==0) || (strcmp(word,"0")==0)) i = 0;
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

 get_word(infile,word);
 while (word[0]!=';')
 {
 if (feof(infile))
  {printf("ERROR: Unexpected EOF on '%s'\n",emssg); fprintf(outfile,"ERROR: Unexpected EOF on '%s'\n",emssg); exit(1);}
 if (kind=='i')
  {
   if (sscanf(word,"%d",&j)!=1)
    {printf("ERROR: Bad integer '%s', reading %s.\n", word, emssg); fprintf(outfile,"ERROR: Bad integer '%s', reading %s.\n", word, emssg); exit(1); }
   ii = (int *)x;
   *ii = j;
  }
 else
 if (kind=='f')
  {
   if (sscanf(word,"%lf",&y)!=1) 
    {printf("ERROR: Bad float '%s', reading %s.\n", word, emssg); fprintf(outfile,"ERROR: Bad float '%s', reading %s.\n", word, emssg); exit(1); }
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
   if ((strcasecmp(word,"TRUE")==0) || (strcasecmp(word,"YES")==0) || (strcmp(word,"Y")==0) || (strcmp(word,"1")==0)) 
	j = 1;
   else
   if ((strcasecmp(word,"FALSE")==0) || (strcasecmp(word,"NO")==0) || (strcmp(word,"N")==0) || (strcmp(word,"0")==0))
	j = 0;
   else
   if (strcasecmp(word,"n/a")==0)
     {
	get_word(infile,word);
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
  .   pre_trash - white-space or delimiters skipped before word.	.
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
  /* Expect month-day-year as in: 3-3-01, Feb 3, 2019, or 3/3/2008, etc. */
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
       line[j] = getc(infile);
     line[j] = ' ';
    }
   j++;
  }
 while ((!feof(infile)) && (line[j-1] != '\n') && (j < maxlen-2));
 line[j-1] = '\0';
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


int Round( double x )
{ int y; if (x<0.0) y = x - 0.5; else y = x + 0.5;  return y; }


/* Get a line value. */
void GetLine( char *linename, double *value )
{
 char word[1024];
 get_parameter( infile, 's', word, linename);
 get_parameters( infile, 'f', value, linename);
}

/* Get a single line value. */
void GetLine1( char *linename, double *value )
{
 char word[1024];
 get_parameter( infile, 's', word, linename);
 get_parameter( infile, 'f', value, linename);
}

/* Get a line value, and print it to file. */
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
 get_parameter( infile, 'l', label, linename);
 get_parameters( infile, 'f', value, linename);
}




double smallerof( double a, double b ) { if (a<b) return a; else return b; }
double largerof( double a, double b )  { if (a>b) return a; else return b; }
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
  {
   k = 0;
   while (line[k] != '\0')
    {
     line[k] = toupper( line[k] );
     k++;
    }
  }
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


/* --- PDF Markup Support --- */
/* This object supports the ability to intercept "MarkupPDF" commands in a Tax Input File,
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
  int page;
  float xpos, ypos;
  struct pdf_markup_record *next;
 } *pdf_markup_list=0;

void add_pdf_markup( char *tagname, int page, float xpos, float ypos, char *value )
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
}

void process_pdf_markup_command( char *line )
{
 char word[4096], tagname[4096], value[4096];
 int pgnum=-1;
 float xpos=0.0, ypos=0.0;
 if (mystrcasestr( line, "MarkupPDF" ) == 0) return;
 if (mystrcasestr( line, "MarkupPDF(" ) != 0)
  {
   next_word( line, word, " \t(" );
   next_word( line, word, " \t(," );
   if (sscanf( word, "%d", &pgnum ) != 1)
    { printf("Error reading MarkupPDF page-num '%s'\n", word );
      fprintf(outfile,"Error reading MarkupPDF page-num '%s'\n", word );
      return;
    }
   next_word( line, word, " \t," );
   if (sscanf( word, "%f", &xpos ) != 1)
    { printf("Error reading MarkupPDF Xposition '%s'\n", word );
      fprintf(outfile,"Error reading MarkupPDF Xposition '%s'\n", word );
      return;
    }
   next_word( line, word, " \t,)" );
   if (sscanf( word, "%f", &ypos ) != 1)
    { printf("Error reading MarkupPDF Yposition '%s'\n", word );
      fprintf(outfile,"Error reading MarkupPDF Yposition '%s'\n", word );
      return;
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
 add_pdf_markup( tagname, pgnum, xpos, ypos, value );
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
     fprintf(outfile,"NewPDFMarkup( %d, %g, %g ) %s\n", pdf_markup_list->page, 
		pdf_markup_list->xpos, pdf_markup_list->ypos, pdf_markup_list->tagname );
    fprintf(outfile,"%s = %s\n", pdf_markup_list->tagname, pdf_markup_list->value );
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
#define MARRIED_FILLING_JOINTLY 2
#define MARRIED_FILLING_SEPARAT 3
#define HEAD_OF_HOUSEHOLD       4
#define WIDOW		        5
#define Yes 1
#define No  0
/************************************************************************/
/* TaxSolve_usa_fed1040_2019.c - OpenTaxSolver for USFed1040 		*/
/* Copyright (C) 2020 - Aston Roberts					*/
/* 									*/
/* Tax Solver for US Fedral 1040 Income Tax return for 2019 Tax Year.	*/
/* 									*/
/* OTS Project Home Page and Updates:  					*/
/*		http://opentaxsolver.sourceforge.com/			*/
/* 									*/
/* Compile:   cc taxsolve_US_1040_2019.c -o taxsolve_US_1040_2019       */
/* Run:       ./taxsolve_US_1040_2019  Fed1040_2019.txt                 */
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
/* Aston Roberts 1-2-2020	aston_roberts@yahoo.com			*/
/************************************************************************/

float thisversion=17.01;



double SchedA[MAX_LINES], SchedD[MAX_LINES], amtws[MAX_LINES];
double Sched1[MAX_LINES], Sched2[MAX_LINES], Sched3[MAX_LINES];
double L2a=0.0;			/* Tax-exempt interest (only for SocSec calculations). */
double L3a=0.0;			/* Qualified dividends. */
double L4a=0.0;			/* IRA distributions */
double L4b=0.0;			/* Taxable IRA distributions */
double L4c=0.0;			/* Pensions, and annuities. */
double L4d=0.0;			/* Taxable pensions, and annuities. */
double L5a=0.0;			/* Social security benefits. */
double L11a=0.0;
double L12a=0.0;		/* Tax before Sched-2. */
double L13a=0.0;		/* Child tax credit */
double S4_60b=0.0;		/* First-time homebuyer credit repayment. Form 5405. */
double qcgws6=0.0, qcgws7=0.0;	/* Support for AMT calculation. (qual.div+cap.gain wrksht vals.)*/
double amtws2c=0.0;		/* Investment interest expense (difference between regular tax and AMT) - AMT entry */
double amtws2g=0.0;		/* Specified private activity bond interest exempt from regular tax - AMT entry */
int Do_SchedD=No, Do_QDCGTW=No, Do_SDTW=No;
int status, under65=Yes, over65=No, dependent=No, force_print_all_pdf_forms=0;
double  collectibles_gains=0.0, ws_sched_D[MAX_LINES], L18a=0.0, L18b=0.0, L18c=0.0, L18d=0.0;

				/* Following values taken from 1040 Instructions. */	/* Updated for 2019. */
double brkpt[4][9]={
		{ 0.0,   9700.0,  39475.0,  84200.0, 160725.0, 204100.0, 510300.0, 9e9 },  /* Single */
		{ 0.0,  19400.0,  78950.0, 168400.0, 321450.0, 408200.0, 612350.0, 9e9 },  /* Married, filing jointly. */
		{ 0.0,   9700.0,  39475.0,  84200.0, 160725.0, 204100.0, 306175.0, 9e9 },  /* Married, filing separate. */
		{ 0.0,  13850.0,  52850.0,  84200.0, 160700.0, 204100.0, 510300.0, 9e9 },  /* Head of Household. */
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
  if (status == WIDOW) status = MARRIED_FILLING_JOINTLY;  /* Handle case of widow(er). */
  status = status - 1;  /* Arrays start at zero; not one. */
  while (brkpt[status][bracket+1] < x)
   {
    sum = sum + (brkpt[status][bracket+1] - brkpt[status][bracket]) * txrt[status][bracket];
    bracket = bracket + 1;
   }
  return (x - brkpt[status][bracket]) * txrt[status][bracket] + sum;
}


void Report_bracket_info( double income, double addedtx, int status )  
{
  double tx;
  int  bracket=0;
  tx = TaxRateFormula( income, status );  
  if (status == WIDOW) status = MARRIED_FILLING_JOINTLY;  /* Handle case of widow(er). */
  status = status - 1;  /* Arrays start at zero; not one. */
  while (brkpt[status][bracket+1] < income) bracket++;
  printf(" You are in the %2.1f%% marginal tax bracket,\n and you are paying an effective %2.1f%% tax on your income.\n",
          100.0 * txrt[status][bracket], 100.0 * (tx + addedtx) / (income + 1e-9) );
  fprintf(outfile," You are in the %2.1f%% marginal tax bracket,\n and you are paying an effective %2.1f%% tax on your income.\n",
          100.0 * txrt[status][bracket], 100.0 * (tx + addedtx) / (income + 1e-9) );
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
/* Qualified Dividends and Capital Gain Tax Worksheet for Line 12a. */
/*  From page 33 of instructions.				   */
/*-----------------------------------------------------------------*/
void capgains_qualdividends_worksheets( int status )			/* Updated for 2019. */
{
 double ws[50];
 int j;

 for (j=0; j<50; j++) ws[j] = 0.0;
 ws[1] = L[11];
 ws[2] = L3a;
 if (Do_SchedD)
  ws[3] = NotLessThanZero(smallerof( SchedD[15], SchedD[16] ));
 else
  ws[3] = Sched1[13];
 ws[4] = ws[2] + ws[3];
 ws[5] = 0.0;  /* Investment interest expense, form 4952, usually 0. */
 ws[6] = NotLessThanZero( ws[4] - ws[5] );  	qcgws6 = ws[6];
 ws[7] = NotLessThanZero( ws[1] - ws[6] );	qcgws7 = ws[7];
 switch (status)
  {
   case SINGLE: case MARRIED_FILLING_SEPARAT: ws[8] = 39375.0; break;
   case MARRIED_FILLING_JOINTLY: case WIDOW:  ws[8] = 78750.0; break;
   case HEAD_OF_HOUSEHOLD: 		      ws[8] = 52750.0; break;
  }
 ws[9]  = smallerof( ws[1], ws[8] );
 ws[10] = smallerof( ws[7], ws[9] );
 ws[11] = ws[9] - ws[10];		/* This amount is taxed at 0%. */
 ws[12] = smallerof( ws[1], ws[6] );
 ws[13] = ws[11];
 ws[14] = ws[12] - ws[13];
 switch (status)
  {
   case SINGLE:  			      ws[15] = 434550.0;  break;
   case MARRIED_FILLING_SEPARAT:	      ws[15] = 244425.0;  break;
   case MARRIED_FILLING_JOINTLY: case WIDOW:  ws[15] = 488850.0;  break;
   case HEAD_OF_HOUSEHOLD: 		      ws[15] = 461700.0;  break;
  }
 ws[16] = smallerof( ws[1], ws[15] );
 ws[17] = ws[7] + ws[11];
 ws[18] = NotLessThanZero( ws[16] - ws[17] );
 ws[19] = smallerof( ws[14], ws[18] );
 ws[20] = 0.15 * ws[19];
 ws[21] = ws[11] + ws[19];
 ws[22] = ws[12] - ws[21];
 ws[23] = 0.20 * ws[22];
 ws[24] = TaxRateFunction( ws[7], status );
 ws[25] = ws[20] + ws[23] + ws[24];
 ws[26] = TaxRateFunction( ws[1], status );
 ws[27] = smallerof( ws[25], ws[26] );
 for (j = 1; j <= 27; j++)
  {
   printf("	Qual. Div & Gains WorkSheet %d:  %8.2f\n", j, ws[j] );
   if (j == 3) { if (Do_SchedD) fprintf(outfile,"\t\t3: Check Yes.\n"); else fprintf(outfile,"\t\t3: Check No.\n"); }
   fprintf(outfile,"	Qual. Div & Gains WorkSheet %d:  %8.2f\n", j, ws[j] );
  }
 L12a = ws[27];
}




/*----------------------------------------------------------------------------------------------*/
/* Form-6251 - Alternative Minimum Tax (AMT) form detailed calculations. 			*/
/* This routine establishes the framework for the 6251 form, for the limited few who need it. 	*/
/* Form 6251 asks many highly specialized questions, which are assumed zero for most filers. 	*/
/* Those who should make the additional entries will no-doubt know who they are, and can 	*/
/* simply add them to this section.  The balance of the routine will be helpful in either case. */
/* --- Anyone indicated to fill-out Form 6251 should review the 6251 instruction booklet. ---	*/ 
/*----------------------------------------------------------------------------------------------*/
double form6251_AlternativeMinimumTax( int itemized )						/* Updated for 2019. */
{
 double thresholdA=0, thresholdB=0, thresholdC=0.0, amtexmption;
 double offsetA=0.0;
 double amtws2a, amtws2b, amtws2e;
 int j, file_amt=1;

 printf("Review AMT form6251 routine for your situation.\n");
 fprintf(outfile,"Review AMT form6251 routine for your situation.\n");

 /* Part I - Alternative Minimum Taxable Income (AMTI) */
 if (L[11] > 0.0)  
  amtws[1] = L[11];
 else
  amtws[1] = L[8] - L[9] - L[10];

 if (itemized)
  amtws2a = SchedA[7];
 else
  amtws2a = L[9];

 amtws2b = -(Sched1[1] + Sched1[8]);

	/* Following amounts assumed normally zero, but review and adjust if needed. */
 // amtws2c = 0.0;	/* Investment interest expense. (Diff between regular tax and AMT). */
 // amtws2d = 0.0;	/* Depletion (Diff between regular tax and AMT). */
 amtws2e = absolutev( Sched1[1] );
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

 // amtws[3] = 0.0;	/* Other adjustments, including income-based related adjustments */

 for (j = 1; j <= 3; j++)
  amtws[4] = amtws[4] + amtws[j];

 if ((status == MARRIED_FILLING_SEPARAT) && (amtws[4] > 733700.0))
  {
   if (amtws[4] > 957100.0)
    amtws[4] = amtws[4] + 55850.0;
   else
    amtws[4] = amtws[4] + 0.25 * (amtws[4] - 733700.0);
  }

 /* Part II */
 switch (status)
  {
     case SINGLE: case HEAD_OF_HOUSEHOLD:
	thresholdA = 510300.0;
	thresholdB = 797100.0;
	thresholdC = 194800.0;
	offsetA = 3896.0;
	amtexmption = 71700.0;
	break;
     case MARRIED_FILLING_JOINTLY: case WIDOW: 
	thresholdA = 1020600.0;
	thresholdB = 1467400.0;
	thresholdC = 194800.0;
	offsetA = 3896.0;
	amtexmption = 111700.0;
	break;
     case MARRIED_FILLING_SEPARAT: 
	thresholdA = 510300.0;
	thresholdB = 733700.0;
	thresholdC = 97400.0;
	offsetA = 1948.0;
        amtexmption = 55850.0;
	break;
     default:  printf("Status %d not handled.\n", status);  exit(1); 
  }

 if (amtws[4] > thresholdA)
  { /* Exemption Worksheet - page 5. */
    double ews[20];
   if (amtws[4] > thresholdB)
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

    if ((L[6] != 0.0) || (L3a != 0.0) || ((SchedD[15] > 0.0) && (SchedD[16] > 0.0)))
     { /* Part III */
       amtws[12] = amtws[6];
       amtws[13] = largerof( qcgws6, ws_sched_D[13] );
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
           case MARRIED_FILLING_JOINTLY:  case WIDOW: 
	     amtws[19] = 78750.0;
	   break;
           case SINGLE:  case MARRIED_FILLING_SEPARAT: 
   	     amtws[19] = 39375.0;
   	   break;
           case HEAD_OF_HOUSEHOLD:
   	     amtws[19] = 52750.0;
        }
       if (Do_QDCGTW)
        amtws[20] = NotLessThanZero( qcgws7 );
       else
       if (Do_SDTW)
	amtws[20] = NotLessThanZero( ws_sched_D[14] );
       else
	amtws[20] = NotLessThanZero( L[11] );
       amtws[21] = NotLessThanZero( amtws[19] - amtws[20] );
       amtws[22] = smallerof( amtws[12], amtws[13] );
       amtws[23] = smallerof( amtws[21], amtws[22] );
       amtws[24] = amtws[22] - amtws[23];  
       switch (status)
	{
	   case SINGLE:  			      amtws[25] = 434550.0;  break;
	   case MARRIED_FILLING_SEPARAT:	      amtws[25] = 244425.0;  break;
	   case MARRIED_FILLING_JOINTLY: case WIDOW:  amtws[25] = 488850.0;  break;
	   case HEAD_OF_HOUSEHOLD: 		      amtws[25] = 461700.0;  break;
	   default:  printf("Status %d not handled.\n", status);  exit(1); 
	}
       amtws[26] = amtws[21];
       if (Do_QDCGTW)
	amtws[27] = NotLessThanZero( qcgws7 );
       else
       if (Do_SDTW)
	amtws[27] = NotLessThanZero( ws_sched_D[19] );
       else
	amtws[27] = NotLessThanZero( L[10] );
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
 amtws[10] = L12a + Sched2[2] - Sched3[1];
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
   if (amtws2e + amtws[3] < 0.0)
    {
     file_amt = Yes;
     fprintf(outfile,"You may need to file AMT form 6251.  (AMTws[31]=%g which is NOT more than AMTws[34]=%g)\n", amtws[31], amtws[34] );
     fprintf(outfile," (See \"Who Must File\" on page-1 of Instructions for Form-6251.)\n");
    }
   else
    file_amt = No;
  }
 if (force_print_all_pdf_forms) 
  file_amt = 1;
 if (file_amt)
  fprintf(outfile,"PDFpage: 13 13\n");	/* Optional PDF Page. */
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
    fprintf(outfile,"EndPDFpage.\nPDFpage: 14 14\n");
  }
 if (file_amt)
  fprintf(outfile,"EndPDFpage.\n");
 fprintf(outfile,"	AMTws[11] = Abs( %6.2f - %6.2f ) = Abs( %6.2f )\n", amtws[9], amtws[10], amtws[9] - amtws[10] );
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


void CapitalLossCarryOverWorksheet( char *fedlogfile, struct FedReturnData *LastYearsReturn )	/* Updated for 2019. */
{ /* From instructions page D-11. */
 double ws[50];
 int k;

 ImportFederalReturnData( fedlogfile, LastYearsReturn );
 if (LastYearsReturn->schedD[21] == 0.0) 
  {
   printf(" No carry-over loss.\n");
   fprintf(outfile," No carry-over loss.\n");
   return;  /* Use this worksheet only if last year's D[21] was a loss. */
  }
 if ((absolutev(LastYearsReturn->schedD[21]) >= absolutev(LastYearsReturn->schedD[16])) && (LastYearsReturn->fedline[41] >= 0.0))
  {
   printf(" No carry-over loss.\n");
   fprintf(outfile," No carry-over loss.\n");
   return;
  }

 for (k=0; k<50; k++) ws[k] = 0.0;
 ws[1] = LastYearsReturn->fedline[10];
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
    if (ws[8] > 0.0) SchedD[6] = ws[8];
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
    if (ws[13] > 0.0) SchedD[14] = ws[13];
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
  char *comment, *buy_date, *sell_date;
  double buy_amnt, sell_amnt;
  struct capgain_record *nxt;
 } *short_trades=0, *long_trades=0;

double total_sales, total_costs=0.0;


void new_capgain( struct capgain_record **list, char *comment, double buy_amnt, 
					char *buy_date, double sell_amnt, char *sell_date )
{ /* Add a new entry to a list. */
  struct capgain_record *new_item, *prev;

  new_item = (struct capgain_record *)malloc( sizeof(struct capgain_record) );
  new_item->comment = strdup( comment );	/* Make new list item and fill-in its fields. */
  new_item->buy_amnt = buy_amnt;
  new_item->buy_date = strdup( buy_date );
  new_item->sell_amnt = sell_amnt;
  new_item->sell_date = strdup( sell_date );
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

 /* First write results in easily human-readable format. */
 total_sales = 0.0;
 total_costs = 0.0;
 fprintf(outfile,"\n%s\n", message );
 fprintf(outfile," %d. (a Description)         (b Buy Date) (c Date Sold) (d Sold Price) (e Cost) (h Gain)\n", section );
 fprintf(outfile," ---------------------------------------------------------------------------------------\n");
 item = list;
 while (item != 0)
  {
   strcpy( word, item->comment );
   if (strlen( word ) > 27) word[30] = '\0';
   if ((strlen(word) > 0) && (word[ strlen(word) - 1 ] == '}')) word[ strlen(word) - 1 ] = '\0';
   while (strlen( word ) < 27) strcat( word, " " ); 	/* Fields become formatted right-justified. */
   fprintf(outfile," %s %10s %10s %14.2f %14.2f %14.2f\n", word, item->buy_date, item->sell_date, item->sell_amnt, 
	absolutev(item->buy_amnt), item->sell_amnt + item->buy_amnt );
   total_sales = total_sales + item->sell_amnt;
   total_costs = total_costs + item->buy_amnt;
   item = item->nxt;
  }
 fprintf(outfile," ---------------------------------------------------------------------------------------\n");
 fprintf(outfile," %d. Totals:                                        %14.2f %14.2f %14.2f\n\n", 
	section + 1, total_sales, absolutev(total_costs), total_sales + total_costs );

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
   fprintf(outfile," F8949_1%ch = %14.2f\n", row, item->sell_amnt + item->buy_amnt );
   row++;
   item = item->nxt;
  }
 fprintf(outfile," F8949_2d = %14.2f\n", total_sales );
 fprintf(outfile," F8949_2e = %14.2f\n", absolutev(total_costs) );
 fprintf(outfile," F8949_2h = %14.2f\n", total_sales + total_costs );
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


void get_gain_and_losses( char *label )
{
 char comment[4096], comment2[2048], date_str1[512], date_str2[512], word[4096], daterrmsg[4096];
 double amnt1, amnt2;
 int toggle=0 ;
 struct date_rec buydate, selldate, annivdate;
 enum {none, short_term, long_term} term_flg=none;

 get_parameter( infile, 's', word, label );     /* Capital gains. */
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
         strcpy (daterrmsg, label);
         if (strlen(label) + strlen(comment) < 4092 )
	  {
           strcat(daterrmsg, ", ");
           strcat(daterrmsg, comment);
          }
	 strcpy( date_str1, word );
	 if (mystrcasestr( date_str1, "various-short" ) != 0)
	  term_flg = short_term;
	 else
	 if (mystrcasestr( date_str1, "various-long" ) != 0)
	  term_flg = long_term;
	 else
	  gen_date_rec ( word, daterrmsg, &buydate );
	 break;
    case 2:	toggle++;
	 if (sscanf(word,"%lf",&amnt2)!=1)
	  { printf("ERROR: Bad float '%s', reading %s.\n", word, label ); 
	    fprintf(outfile,"ERROR: Bad float '%s', reading %s.\n", word, label );
	    exit(1);
	  }
	 break;
    case 3:	toggle = 0;
	 strcpy( date_str2, word );
	 get_comment( infile, comment2 );	/* Check for and consume any additional comment. */
         strcat( comment, comment2 );
         if (term_flg == none)		/* Executes if term_flg Not otherwise set in case: 1 */
	  {
           gen_date_rec ( word, daterrmsg, &selldate );
           if (is_date1_beyond_date2 (buydate, selldate))
	    {
	     printf("DATA ERROR: Buy-date after sell-date.   '%s'\n Buy-date '%s'  Sell-date '%s'\n", daterrmsg, date_str1, date_str2);
	     fprintf(outfile,"DATA ERROR: Buy-date after sell-date.   '%s'\n Buy-date '%s'  Sell-date '%s'\n", daterrmsg, date_str1, date_str2);
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
	 if (term_flg == long_term)
	  { /*long-gain/loss*/
	    new_capgain( &long_trades, comment, amnt1, date_str1, amnt2, date_str2 );
	  } /*long-gain/loss*/
	 else
	  { /*short-gain/loss*/
	    new_capgain( &short_trades, comment, amnt1, date_str1, amnt2, date_str2 );
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
/* Expect entries in double pairs. 					*/
/*   buy_amnt   date 							*/
/*   sell_amnt  date 							*/
/*									*/
/************************************************************************/
void get_cap_gains()		/* This is Schedule-D. */			/* Updated for 2019. */
{
 char word[4092], *LastYearsOutFile=0;
 int j, doline22=0;
 double stcg=0.0, ltcg=0.0;      /* Variables for short and long term gains. */
 double SchedDd[20], SchedDe[20];

 for (j=0; j<20; j++) { SchedDd[j] = 0.0;  SchedDe[j] = 0.0; }
 /* Form 8849 - Adjunct form to Schedule-D. */
 get_gain_and_losses( "CapGains-A/D" );	/* (A) Basis Reported to IRS. */
 if (short_trades)
  {
   print_capgain_list( short_trades, 1, "Form 8949 Part-I, Short-Term Cap Gains+Losses, CHECK (A) Basis Reported to IRS:", "11 11\n F8949_ckA X" );
   SchedDd[1] = total_sales;
   SchedDe[1] = total_costs;
   SchedD[1] = SchedDd[1] + SchedDe[1];
   free_capgain_list( &short_trades );
  }
 if (long_trades)
  {
   print_capgain_list( long_trades, 3, "Form 8949 Part-II, Long-Term Cap Gains+Losses, CHECK (D) Basis Reported to IRS:", "12 12\n F8949_ckD X" );
   SchedDd[8] = total_sales;
   SchedDe[8] = total_costs;
   SchedD[8] = SchedDd[8] + SchedDe[8];
   free_capgain_list( &long_trades );
  }

 get_gain_and_losses( "CapGains-B/E" );	/* (B) Basis NOT Reported to IRS. */
 if (short_trades)
  {
   print_capgain_list( short_trades, 1, "Form 8949 Part-I, Short-Term Cap Gains+Losses, CHECK (B) Basis NOT Reported to IRS:", "11 11\n F8949_ckB X" );
   SchedDd[2] = total_sales;
   SchedDe[2] = total_costs;
   SchedD[2] = SchedDd[2] + SchedDe[2];
   free_capgain_list( &short_trades );
  }
 if (long_trades)
  {
   print_capgain_list( long_trades, 3, "Form 8949 Part-II, Long-Term Cap Gains+Losses, CHECK (E) Basis NOT Reported to IRS:", "12 12\n F8949_ckE X"  );
   SchedDd[9] = total_sales;
   SchedDe[9] = total_costs;
   SchedD[9] = SchedDd[9] + SchedDe[9];
   free_capgain_list( &long_trades );
  }

 get_gain_and_losses( "CapGains-C/F" );	/* (C) Cannot check (A) or (B). */
 if (short_trades)
  {
   print_capgain_list( short_trades, 1, "Form 8949 Part-I, Short-Term Cap Gains+Losses, CHECK (C) Not reported on Form 1099-B.\n", "11 11\n F8949_ckC X" );
   SchedDd[3] = total_sales;
   SchedDe[3] = total_costs;
   SchedD[3] = SchedDd[3] + SchedDe[3];
   free_capgain_list( &short_trades );
  }
 if (long_trades)
  {
   print_capgain_list( long_trades, 3, "Form 8949 Part-II, Long-Term Cap Gains+Losses, CHECK (F) Not reported on Form 1099-B.\n", "12 12\n F8949_ckF X" );
   SchedDd[10] = total_sales;
   SchedDe[10] = total_costs;
   SchedD[10] = SchedDd[10] + SchedDe[10];
   free_capgain_list( &long_trades );
  }

 stcg = SchedD[1] + SchedD[2] + SchedD[3];
 ltcg = SchedD[8] + SchedD[9] + SchedD[10];

 GetLine( "D4", &SchedD[4] );       /* Short term gain from 6252 and short-term gain or loss from Forms 4684, 6781, 8824. */
 GetLine( "D5", &SchedD[5] );       /* Net short-term gain or loss from partnerships, S corps, estates, trusts from K-1. */

 get_parameter( infile, 's', word, "D6" );	/* Carryover short-term loss from last year.  Or, LastYear's Return Output File-name. */
 get_word(infile,word);
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

 GetLine( "Collectibles", &collectibles_gains );	/* Gains or Losses from Collectibles. (Usually zero.) */
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
   fprintf(outfile,"PDFpage: 9 9\n");
   // Do_QDCGTW = Yes;	/* Tentatively set to do: Qualified Dividends and Capital Gain tax Worksheet. */
   fprintf(outfile,"\tNet Forms-8949 Short-term Gains = %10.2f\n", stcg );
   fprintf(outfile,"\tNet Forms-8949 Long-term Gains  = %10.2f\n", ltcg);
   fprintf(outfile," D1bd = %10.2f\n   D1be = %10.2f\n    D1bh = %10.2f\n", SchedDd[1], absolutev(SchedDe[1]), SchedD[1] );
   fprintf(outfile," D2d = %10.2f\n   D2e = %10.2f\n    D2h = %10.2f\n", SchedDd[2], absolutev(SchedDe[2]), SchedD[2] );
   fprintf(outfile," D3d = %10.2f\n   D3e = %10.2f\n    D3h = %10.2f\n", SchedDd[3], absolutev(SchedDe[3]), SchedD[3] );
   fprintf(outfile," D4 = %6.2f\n", SchedD[4] );
   fprintf(outfile," D5 = %6.2f\n", SchedD[5] );
   fprintf(outfile," D6 = %6.2f		(Carry-over Loss)\n", SchedD[6] );
   SchedD[7] = SchedD[1] + SchedD[2] + SchedD[3] + SchedD[4] + SchedD[5] + SchedD[6];
   fprintf(outfile," D7 = %6.2f		{ Net short-term capital gain or loss }\n", SchedD[7] );
   fprintf(outfile," D8bd = %10.2f\n   D8be = %10.2f\n   D8bh = %10.2f\n", SchedDd[8], absolutev(SchedDe[8]), SchedD[8] );
   fprintf(outfile," D9d = %10.2f\n   D9e = %10.2f\n   D9h = %10.2f\n", SchedDd[9], absolutev(SchedDe[9]), SchedD[9] );
   fprintf(outfile," D10d = %10.2f\n   D10e = %10.2f\n   D10h = %10.2f\n", SchedDd[10], absolutev(SchedDe[10]), SchedD[10] );
   fprintf(outfile," D11 = %6.2f\n", SchedD[11] );
   fprintf(outfile," D12 = %6.2f\n", SchedD[12] );
   fprintf(outfile," D13 = %6.2f\n", SchedD[13] );
   fprintf(outfile," D14 = %6.2f	(Carry-over Loss)\n", SchedD[14] );
   SchedD[15] = SchedD[8] + SchedD[9] + SchedD[10] + SchedD[11] + SchedD[12] + SchedD[13] + SchedD[14];
   fprintf(outfile," D15 = %6.2f		{ Net long-term capital gain or loss }\n", SchedD[15] );
   fprintf(outfile,"EndPDFpage.\nPDFpage: 10 10\n");

   /* Part ||| */
   SchedD[16] = SchedD[7] + SchedD[15];
   fprintf(outfile," D16 = %6.2f\n", SchedD[16]);
   if (SchedD[16] > 0.0) 
    { /*gain*/
     L[6] = SchedD[16];
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

     if (status == MARRIED_FILLING_SEPARAT) maxloss = -1500.0; else maxloss = -3000.0;
     if (SchedD[16] < maxloss) SchedD[21] = maxloss; else SchedD[21] = SchedD[16];
     fprintf(outfile," D21 = %6.2f\n", SchedD[21]);
     L[6] = SchedD[21];
     doline22 = Yes;
    }
   else
    { /*Zero gain/loss.*/
     L[6] = 0.0;
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


/*------------------------------------------------------*/
/* 'Schedule D Tax Worksheet', instructions page D-15.	*/
/*------------------------------------------------------*/
void sched_D_tax_worksheet( int status )			/* Updated for 2019. */
{
 double ws[100];
 int k;

 for (k = 0; k < 100; k++) ws[k] = 0.0;
 ws[1] = L[1];
 ws[2] = L3a;
 ws[3] = 0.0;	/* Form 4952, line 4g. Usually 0.0. */
 ws[4] = 0.0;	/* Form 4952, line 4e. Usually 0.0. */
 ws[5] = NotLessThanZero( ws[3] - ws[4] );
 ws[6] = NotLessThanZero( ws[2] - ws[5] );
 ws[7] = smallerof( SchedD[15], SchedD[16] );
 ws[8] = smallerof( ws[3], ws[4] );
 ws[9] = NotLessThanZero( ws[7] - ws[8] );
 ws[10] = ws[6] + ws[9];
 fprintf(outfile,"  Sched-D tax Worksheet line 10 = %6.2f\n", ws[10]);
 ws[11] = SchedD[18] + SchedD[19];
 ws[12] = smallerof( ws[9], ws[11] );
 ws[13] = ws[10] - ws[12];
 ws[14] = NotLessThanZero( ws[1] - ws[13] );
 fprintf(outfile,"  Sched-D tax Worksheet line 13 = %6.2f\n", ws[13]);
 fprintf(outfile,"  Sched-D tax Worksheet line 14 = %6.2f\n", ws[14]);
 switch (status) 
  { case SINGLE: case MARRIED_FILLING_SEPARAT: ws[15] = 39375.0; break;
    case MARRIED_FILLING_JOINTLY: case WIDOW:  ws[15] = 78750.0; break;
    case HEAD_OF_HOUSEHOLD:      	       ws[15] = 52750.0; break;
  }
 ws[16] = smallerof( ws[1], ws[15] );
 ws[17] = smallerof( ws[14], ws[16] );
 ws[18] = NotLessThanZero( ws[1] - ws[10] );
 switch (status) 
  { case SINGLE: case MARRIED_FILLING_SEPARAT: ws[19] = smallerof( ws[1], 160725.0 );  break;
    case MARRIED_FILLING_JOINTLY: case WIDOW:  ws[19] = smallerof( ws[1], 321450.0 );  break;
    case HEAD_OF_HOUSEHOLD:      	       ws[19] = smallerof( ws[1], 160700.0 );  break;
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
    { case SINGLE: 			ws[24] = 434550.0;  break;
      case MARRIED_FILLING_SEPARAT: 	ws[24] = 244425.0;  break;
      case MARRIED_FILLING_JOINTLY: 
      case WIDOW:  			ws[24] = 488850.0;  break;
      case HEAD_OF_HOUSEHOLD:		ws[24] = 461700.0;  break;
    }
   ws[25] = smallerof( ws[1], ws[24] );
   ws[26] = ws[19] + ws[20];
   ws[27] = NotLessThanZero( ws[25] - ws[26] );
   ws[28] = smallerof( ws[23], ws[27] );
   ws[29] = 0.15 * ws[28];
   ws[30] = ws[22] + ws[28];
   if (ws[1] != ws[30])
    { /*lines31-41*/
      ws[31] = ws[21] - ws[30];
      ws[32] = 0.20 * ws[31];
      if (SchedD[19] != 0.0)
       { /*lines33-38*/
	 ws[33] = smallerof( ws[9], SchedD[19] );
	 ws[34] = ws[10] + ws[19];
	 ws[35] = ws[1];
	 ws[36] = NotLessThanZero( ws[34] - ws[35] );
	 ws[37] = NotLessThanZero( ws[33] - ws[36] );
	 ws[38] = 0.25 * ws[37];
       } /*lines33-38*/
      if (SchedD[18] != 0.0)
       { /*lines39-41*/
	 ws[39] = ws[19] + ws[20] + ws[28] + ws[31] + ws[37];
	 ws[40] = ws[1] - ws[39];
	 ws[41] = 0.28 * ws[40];
       } /*lines39-41*/
    } /*lines31-41*/
  } /*lines23-43*/
 ws[44] = TaxRateFunction( ws[21], status );
 ws[45] = ws[31] + ws[34] + ws[40] + ws[43] + ws[44];
 ws[46] = TaxRateFunction( ws[1], status );
 ws[47] = smallerof( ws[45], ws[46] );
 L12a = ws[47];
 for (k = 0; k < 100; k++) ws_sched_D[k] = ws[k];	/* Save worksheet values for AMT, if needed. */
}



/*-------------------------------------------------------*/
/* Social Security Worksheet - From Instructions page 28. */
/*-------------------------------------------------------*/
void SocSec_Worksheet()							/* Updated for 2019. */
{
 double ws[100];
 int k;
 if (L5a == 0.0) return;
 for (k = 0; k < 100; k++) ws[k] = 0.0;
 ws[1] = L5a;
 ws[2] = 0.5 * ws[1];
 ws[3] = L[1] + L[2] + L[3] + L[4] + L[6] + Sched1[9];
 ws[4] = L2a;
 ws[5] = ws[2] + ws[3] + ws[4];
 for (k = 10; k <= 19; k++)
  ws[6] = ws[6] + Sched1[k];
 for (k = 0; k <= 6; k++)
  fprintf(outfile,"\tSocSecWorkSheet[%d] = %6.2f\n", k, ws[k] );
 if (ws[6] >= ws[5])
  {
   L[5] = 0.0;		/* Which is "L5b". */
   fprintf(outfile,"\tSocSecWorkSheet[7]: Check 'No'\n" );
   printf("None of your social security benefits are taxable.\n");
   fprintf(outfile,"None of your social security benefits are taxable.\n");
   return;
  }
 ws[7] = ws[5] - ws[6];
 fprintf(outfile,"\tSocSecWorkSheet[7] = %6.2f  (Check 'Yes')\n", ws[7] );
 if (status == MARRIED_FILLING_JOINTLY)
  ws[8] = 32000.0;      						/* Updated for 2019. */
 else
  ws[8] = 25000.0;
 fprintf(outfile,"\tSocSecWorkSheet[8] = %6.2f\n", ws[8] );
 if (ws[8] >= ws[7])
  {
   L[5] = 0.0;
   fprintf(outfile,"\tSocSecWorkSheet[9]: Check 'No'\n" );
   printf("None of your social security benefits are taxable.\n");
   fprintf(outfile,"None of your social security benefits are taxable.\n");
   return;
  }
 ws[9] = ws[7] - ws[8];
 fprintf(outfile,"\tSocSecWorkSheet[9] = %6.2f  (Check 'Yes')\n", ws[9] );
 if (status == MARRIED_FILLING_JOINTLY)
  ws[10] = 12000.0;      						/* Updated for 2019. */
 else
  ws[10] = 9000.0;
 ws[11] = NotLessThanZero( ws[9] - ws[10] );
 ws[12] = smallerof( ws[9], ws[10] );
 ws[13] = ws[12] / 2.0;
 ws[14] = smallerof( ws[2], ws[13] );
 ws[15] = 0.85 * ws[11];
 ws[16] = ws[14] + ws[15];
 ws[17] = 0.85 * ws[1];
 ws[18] = smallerof( ws[16], ws[17] );
 for (k = 10; k <= 18; k++)
  fprintf(outfile,"\tSocSecWorkSheet[%d] = %6.2f\n", k, ws[k] );
 L[5] = ws[18];		/* Which is "L5b". */
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
  /* Used for PDF form-filling only.  Not used by tax-calculations. */
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
	  state = 9;  ncnt = 17;  total = 0.0;
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
	    fprintf(outfile,"PDFpage: 8 8\n");
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
	    fprintf(outfile,"PDFpage: 8 8\n");
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



/*----------------------------------------------------------------------*/
/* Main									*/
/*----------------------------------------------------------------------*/
int main( int argc, char *argv[] )						/* Updated for 2019. */
{
 int argk, j, k, itemize=0;
 char word[2000], outfname[2000], *infname="", labelx[1024]="";
 time_t now;
 double exemption_threshold=0.0, tmpval=0.0;
 double S_STD_DEDUC, MFS_STD_DEDUC, MFJ_STD_DEDUC, HH_STD_DEDUC, std_deduc;
 char *Your1stName, *YourLastName, *Spouse1stName, *SpouseLastName, *socsec, socsectmp[100];
 double NumDependents=0.0;
 double localtax[10], loctaxlimit, homemort[10];
 int StdDedChart_NumBoxesChecked=0, gotS2_2=0;
 int SchedB7a=0, SchedB7aa=-1, SchedB8=0;
 char SchedB7b[0124]="";
 double S2_7b=0.0;

 /* Decode any command-line arguments. */
 printf("US 1040 2019 - v%3.2f\n", thisversion);
 argk = 1;  k=1;
 while (argk < argc)
 {
  if (strcmp(argv[argk],"-verbose")==0)  { verbose = Yes; }
  else
  if (strcmp(argv[argk],"-allforms")==0)  { force_print_all_pdf_forms = 1; }
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
  }

 /* Accept parameters from input file. */
 /* Expect  US-Fed-1040 lines, something like:
	Title:  Federal 1040 2019 Return
	L1		{Wages}
	L2b		{Interest}
	L3b		{Dividends}
 */

 /* Accept Form's "Title" line, and put out with date-stamp for your records. */
 read_line( infile, word );
 now = time(0);
 fprintf(outfile,"\n%s,	 v%2.2f, %s\n", word, thisversion, ctime( &now ) );

 get_parameter( infile, 's', word, "Status" );	/* Single, Married/joint, Married/sep, Head house, Widow(er) */
 get_parameter( infile, 'l', word, "Status?");
 if (strncasecmp(word,"Single",4)==0) status = SINGLE; else
 if (strncasecmp(word,"Married/Joint",13)==0) status = MARRIED_FILLING_JOINTLY; else
 if (strncasecmp(word,"Married/Sep",11)==0) status = MARRIED_FILLING_SEPARAT; else
 if (strncasecmp(word,"Head_of_House",4)==0) status = HEAD_OF_HOUSEHOLD; else
 if (strncasecmp(word,"Widow",4)==0) status = WIDOW;
 else
  { 
   printf("Error: unrecognized status '%s'. Exiting.\n", word); 
   fprintf(outfile,"Error: unrecognized status '%s'. Exiting.\n", word); 
   exit(1);
  }
 fprintf(outfile,"Status = %s (%d)\n", word, status);

 get_parameter( infile, 's', word, "You_65+Over?" );	/* Were you born before January 2, 1954 ? (Y/N) */
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

 get_parameter( infile, 's', word, "Spouse_65+Over?" );	/* Was Spouse born before January 2, 1954 ? (Y/N) */
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
   case MARRIED_FILLING_JOINTLY: fprintf(outfile,"CkMFJ X\nCkYourself X\nCkSpouse X\nL6ab = 2\n");  break;
   case MARRIED_FILLING_SEPARAT: fprintf(outfile,"CkMFS X\nCkYourself X\nL6ab = 1\n");  break;
   case HEAD_OF_HOUSEHOLD: fprintf(outfile,"CkHH X\nCkYourself X\nL6ab = 1\n");  break;
   case WIDOW: fprintf(outfile,"CkQW X\nCkYourself X\nL6ab = 1\n");  break;
   default: printf("Error: Unknown filing status %d.\n", status );
  }

 GetLine1( "Dependents", &NumDependents );

 GetLineF( "L1", &L[1] );	/* Wages, salery, tips (W-2). */
 GetLineFnz( "L2a", &L2a );	/* Tax-exempt interest. (only for SocialSecurity calculations) */
 GetLineF( "L2b", &L[2] );	/* Taxable interest. (Sched-B) */
 GetLineF( "L3a", &L3a );	/* Qualified Dividends. (Sched-B) */
 if (L3a > 0.0) Do_QDCGTW = Yes;	
 GetLineF( "L3b", &L[3] );	/* Ordinary Dividends. (Sched-B) */
 GetLineF( "L4a", &L4a );	/* IRAs distributions. */
 GetLineF( "L4b", &L4b );	/* Taxable IRAs distributions. */
 GetLineF( "L4c", &L4c );	/* Pensions and annuities. */
 GetLineF( "L4d", &L4d );	/* Taxable pensions, and annuities. */
 L[4] = L4b + L4d;
 GetLineF( "L5a", &L5a );	/* Social Security benefits.  Forms SSA-1099 box-5. */

 GetLine( "L10", &L[10] );	/* Qualified business income deduction. */
 GetLine( "L13a", &L13a );	/* Child tax credit/credit for other dependents. */
 GetLine( "L17", &L[17] );	/* Federal income tax withheld, Forms W-2, 1099 */

 GetLine( "L18a", &L18a );	/* Refundable credit: EIC */
 GetLine( "L18b", &L18b );	/* Refundable credit: Sch. 8812 */
 GetLine( "L18c", &L18c );	/* Refundable credit: Form 8863 */
 GetLine( "L24", &L[24] );	/* Estimated Tax Under-payment Penalty */

 get_cap_gains();	 /* Capital gains. (Schedule-D). This popuates "schedD[]" and L[6]. */


 /* -- Schedule-1 -- Additional Income and Adjustments */

 get_parameter( infile, 's', word, "VirtCurr?" );	/* During the year, did you have Virtual Currency? (Y/N) */
 get_parameter( infile, 'b', &j, "VirtCurr?" );
 if (j == 0)
  fprintf(outfile,"ChkVirtNo X\n");
 else
  fprintf(outfile,"ChkVirtYes X\n");
 GetLineF( "S1_1", &Sched1[1] );	/* Taxable refunds. */
 GetLineF( "S1_2a", &Sched1[2] );	/* Alimony received. */
 GetTextLineF( "S1_2b:" );

 GetLineF( "S1_3", &Sched1[3] );	/* Business income/loss. */
 showline_wlabel( "S1_3", Sched1[3] );   /* This line was set in get_cap_gains() above. */

 GetLineFnz( "S1_4", &Sched1[4] );	/* Other gains or losses. Form 4794. */

 GetLineFnz( "S1_5", &Sched1[5] );	/* Rent realestate, royalties, partnerships, S corp. (Sched E)*/

 GetLineFnz( "S1_6", &Sched1[6] );	/* Farm income/loss. (Sched F) */

 GetLineFnz( "S1_7", &Sched1[7] );	/* Unemployment compensation */

 GetLineFnz( "S1_8", &Sched1[8] );	/* Other income. (pg 28) */
 GetTextLineF( "S1_8_Type:" );

 for (j=1; j <= 8; j++)
  Sched1[9] = Sched1[9] + Sched1[j];
 showline_wlabel( "S1_9", Sched1[9] );
 showline_wlabel( "L7a", Sched1[9] );

 /* Adjusted Gross Income section. */
 GetLineFnz( "S1_10", &Sched1[10] );	/* Educator expenses */
 GetLineFnz( "S1_11", &Sched1[11] );	/* Bus. exp.: reservists, artists, ... Attach Form 2106 */
 GetLineFnz( "S1_12", &Sched1[12] );	/* Health savings account deduction. Attach Form 8889 */
 GetLineFnz( "S1_13", &Sched1[13] );	/* Moving expenses. Attach Form 3903*/
 GetLineFnz( "S1_14", &Sched1[14] );	/* One-half of self-employment tax. Attach Schedule SE*/
 GetLineFnz( "S1_15", &Sched1[15] );	/* Self-employed SEP, SIMPLE, and qualified plans */
 GetLineFnz( "S1_16", &Sched1[16] );	/* Self-employed health insurance deduction */
 GetLineFnz( "S1_17", &Sched1[17] );	/* Penalty on early withdrawal of savings*/
 GetLineFnz( "S1_18a", &Sched1[18] );	/* Alimony paid*/
 GetTextLineF( "AlimRecipSSN:" );
 GetTextLineF( "AlimRecipName:" );

 GetLineFnz( "S1_19", &Sched1[19] );	/* IRA deduction (Done above) */

 SocSec_Worksheet();		/* This calc. depends on line L5a and Sched1[9-19].  Calculates L5b, which is L[5]. */
 showline_wlabel( "L5b", L[5] ); 

 showline( 6 );
 L[7] = L[1] + L[2] + L[3] + L[4] + L[5] + L[6] + Sched1[9];
 showline_wlabel( "L7b", L[7] );

 GetLine( "S1_20", &Sched1[20] );	/* Student loan interest deduction - page 90 */
 if (Sched1[20] != 0.0)
  { /* Student loan interest calculation pg 90. */
   double ws[20], sum=0.0;
   ws[1] = smallerof( Sched1[20], 2500.0 );
   ws[2] = L[7];
   for (j=10; j <= 19; j++)
    sum = sum + Sched1[j];
   ws[3] = sum;
   ws[4] = ws[2] - ws[3];
   if (status == MARRIED_FILLING_JOINTLY) ws[5] = 140000.0; else ws[5] = 70000.0;	/* Updated for2019. */
   if (ws[4] > ws[5])
    {
     ws[6] = ws[4] - ws[5];
     if (status == MARRIED_FILLING_JOINTLY)
      ws[7] = ws[6] / 30000.0; 
     else
      ws[7] = ws[6] / 15000.0;
     if (ws[7] >= 1.0)
      ws[7] = 1.0;
     ws[8] = ws[1] * ws[7];
    }
   else ws[8] = 0.0;
   ws[9] = ws[1] - ws[8];
   Sched1[20] = ws[9];
  }
 showline_wlabel( "S1_20", Sched1[20] );

 GetLineFnz( "S1_21", &Sched1[21] );	/* Tuition and fees. */

 for (j=10; j <= 21; j++)
  Sched1[22] = Sched1[22] + Sched1[j];
 showline_wlabel( "S1_22", Sched1[22] );

 /* -- End of Schedule-1 -- */

 showline_wlabel( "L8a", Sched1[22] );
 L[8] = L[7] - Sched1[22];
 showline_wlabelmsg( "L8b", L[8], "Adjusted Gross Income" );

 if (under65 == 0) over65 = 1; 
 switch (status)	/* Check for minimum income to file. */				/* Updated for 2019. */
  {
   case SINGLE:  		  if (under65) exemption_threshold = 12200.0;
				  else  exemption_threshold = 13850.0;
	break;
   case MARRIED_FILLING_JOINTLY:  if (under65==2) exemption_threshold = 24400.0;
				  else 
				  if (under65==1) exemption_threshold = 25700.0;  
				  else  exemption_threshold = 27000.0;
				  if (under65 != 2) over65 = 1;
	break;
   case MARRIED_FILLING_SEPARAT:  exemption_threshold = 5.0;
	break;
   case HEAD_OF_HOUSEHOLD: 	  if (under65) exemption_threshold = 18350.0;  
				  else  exemption_threshold = 20000.0;
	break;
   case WIDOW:  		  if (under65) exemption_threshold = 24400.0;  
				  else  exemption_threshold = 25700.0;
  }
 if (L[8] < exemption_threshold)
  {
   printf(" (L8 = %3.2f < Threshold = %3.2f)\n", L[8], exemption_threshold );
   printf("You may not need to file a return, due to your income level.\n"); 
   fprintf(outfile,"You may not need to file a return, due to your income level.\n");
  }


 /* Schedule A */
 GetLine( "A1", &SchedA[1] );	/* Unreimbursed medical expenses. */
  showschedA(1);
 SchedA[2] = L[8];
  showschedA(2);
 SchedA[3] = 0.1 * SchedA[2];
  showschedA(3);
 SchedA[4] = NotLessThanZero( SchedA[1] - SchedA[3] );
  showschedA(4);
 GetLine( "A5a", &localtax[1] );	/* State and local income taxes. Or sales taxes. */
  showline_wlabel( "A5a", localtax[1] );
 GetLine( "A5b", &localtax[2] );	/* State and local real estate taxes. */
  showline_wlabel( "A5b", localtax[2] );
 GetLine( "A5c", &localtax[3] );	/* State and local personal property (eg. automobile) taxes. */
  showline_wlabel( "A5c", localtax[3] );
 localtax[4] =  localtax[1] +  localtax[2] +  localtax[3];
  showline_wlabel( "A5d", localtax[4] );
 if (status != MARRIED_FILLING_SEPARAT)
  loctaxlimit = 10000.0;
 else
  loctaxlimit = 5000.0;
 localtax[5] = smallerof( localtax[4], loctaxlimit );
  showline_wlabel( "A5e", localtax[5] );
 GetLine( "A6", &SchedA[6] );	/* Other taxes. */
  showschedA(6);
 SchedA[7] = localtax[5] + SchedA[6];
  showschedA(7);

 GetLine( "A8a", &homemort[1] );	/* Home mortgage interest and points reported to you on Form 1098.*/
  showline_wlabel( "A8a", homemort[1] );
 GetLine( "A8b", &homemort[2] );	/* Home mortgage interest not reported to you on Form 1098.*/
  showline_wlabel( "A8b", homemort[2] );
 GetLine( "A8c", &homemort[3] );	/* Points not reported to you on Form 1098.*/
  showline_wlabel( "A8b", homemort[3] );
 homemort[5] = homemort[1] + homemort[2] + homemort[3];
  showline_wlabel( "A8e", homemort[5] );
 GetLine( "A9", &SchedA[9] );	/* Investment interest. Attach Form 4952*/
  showschedA(9);
 SchedA[10] = homemort[5] + SchedA[9];
  showschedA(10);

 GetLine( "A11", &SchedA[11] );	/* Charity contributions by cash or check.*/
  showschedA(11);
 GetLine( "A12", &SchedA[12] );	/* Contributions other than cash or check.*/
  showschedA(12);
 GetLine( "A13", &SchedA[13] );	/* Carryover from prior year*/
  showschedA_wMsg(13, "Carryover from prior year" );
 SchedA[14] = SchedA[11] + SchedA[12] + SchedA[13];
  showschedA(14);
 GetLine( "A15", &SchedA[15] );	/* Casualty or theft loss(es).*/
  showschedA(15);
 GetLine( "A16", &SchedA[16] );	/* Other expenses*/
  showschedA(16);
 SchedA[17] = SchedA[4] + SchedA[7] + SchedA[10] + SchedA[14] + SchedA[15] + SchedA[16];
  showschedA(17);
 L[9] = SchedA[17];	/* Tentative setting. */
 if (L[9] > 0.0)  itemize = Yes;  else  itemize = No;

 if ((L[2] != 0.0) || (L[3] != 0.0))
  {
   fprintf(outfile," Schedule-B:\n");
   fprintf(outfile,"  B2 = %6.2f\n", L[2] );
   fprintf(outfile,"  B4 = %6.2f\n", L[2] );
   fprintf(outfile,"  B6 = %6.2f\n", L[3] );
  }

 fprintf(outfile, "StdDedChart_NumBoxesChecked = %d\n", StdDedChart_NumBoxesChecked ); 
 if (StdDedChart_NumBoxesChecked == 0)
  {
   S_STD_DEDUC   = 12200.0;						/* Updated for 2019. */
   MFJ_STD_DEDUC = 24400.0;
   MFS_STD_DEDUC = 12200.0;
   HH_STD_DEDUC  = 18350.0;
  }
 else
  { /* Std. Deduction chart for People who were Born Before January 2, 1955, or Were Blind, pg 30. */
    switch (StdDedChart_NumBoxesChecked)		/* Does not handle if someone claims you or joint-spouse as dependent. */
     {				/* (Qualifying Widow/er has same amounts as MFJ, so not broken into separate variable.) */
      case 1: 
	S_STD_DEDUC   = 13850.0;					/* Updated for 2019. */
	MFJ_STD_DEDUC = 25700.0;
	MFS_STD_DEDUC = 13500.0;
	HH_STD_DEDUC  = 20000.0;
	break;
      case 2: 
	S_STD_DEDUC   = 15500.0;
	MFJ_STD_DEDUC = 27000.0;
	MFS_STD_DEDUC = 14800.0;
	HH_STD_DEDUC  = 21650.0;
	break;
      case 3: 
	MFJ_STD_DEDUC = 28300.0;
	MFS_STD_DEDUC = 16100.0;
	S_STD_DEDUC   = 15500.0;	/* Cannot happen, but set to appease compiler. */
	HH_STD_DEDUC  = 21650.0;	/* .. */
	break;
      case 4: 
	MFJ_STD_DEDUC = 29600.0;
	MFS_STD_DEDUC = 17400.0;
	S_STD_DEDUC   = 15500.0;	/* Cannot happen, but set to appease compiler. */
	HH_STD_DEDUC  = 21650.0;	/* .. */
	break;
      default:  fprintf(outfile,"Error: StdDedChart_NumBoxesChecked (%d) not equal to 1, 2, 3, or 4.\n", StdDedChart_NumBoxesChecked );
		printf("Error: StdDedChart_NumBoxesChecked (%d) not equal to 1, 2, 3, or 4.\n", StdDedChart_NumBoxesChecked );
		exit(1); 
     }
    fprintf(outfile,"(Assuming no one is claiming your or your joint-spouse as a dependent.)\n");
  }

 switch (status)
  {
   case SINGLE:
		std_deduc = S_STD_DEDUC;	break;
   case MARRIED_FILLING_SEPARAT:  
		std_deduc = MFS_STD_DEDUC;	break;
   case WIDOW:
   case MARRIED_FILLING_JOINTLY:
		std_deduc = MFJ_STD_DEDUC;	break;
   case HEAD_OF_HOUSEHOLD:
		std_deduc = HH_STD_DEDUC;	break;
   default:  printf("Case (Line 8) not handled.\n"); fprintf(outfile,"Case (Line 8) not handled.\n"); exit(1);
  }

 if (L[9] <= std_deduc)
  {
   printf("	(Itemizations < Std-Deduction, %6.2f < %6.2f)\n", L[9], std_deduc );
   fprintf(outfile,"	(Itemizations < Std-Deduction, %6.2f < %6.2f)\n", L[9], std_deduc );
   L[9] = std_deduc;
   fprintf(outfile,"Use standard deduction.\n");
   itemize = 0;
  }
 else
  {
   printf("	(Itemizations > Std-Deduction, %6.2f > %6.2f)\n", L[9], std_deduc );
   fprintf(outfile,"	(Itemizations > Std-Deduction, %6.2f > %6.2f)\n", L[9], std_deduc );
   fprintf(outfile,"Itemizing.\n");
  }
 showline(9);
 showline(10);

 L11a = L[9] + L[10];
 showline_wlabel( "L11a", L11a );

 L[11] = NotLessThanZero( L[8] - L11a );
 showline_wmsg( 11, "Taxable Income" );

 L12a = TaxRateFunction( L[11], status );

 if (L[11] <= 0.0) 
  { /*exception*/
    printf(" Exception (Sched-D Instructions page D-16) - Do not use QDCGT or Sched-D Tax Worksheets.\n");
  } /*exception*/
 else
  { /*no_exception*/
   if ((!Do_SDTW) && (!Do_QDCGTW) && ((L3a > 0.0) || (Sched1[13] > 0.0) || ((SchedD[15] > 0.0) && (SchedD[16] > 0.0)) ))
    Do_QDCGTW = Yes;
   if (Do_QDCGTW)
    {
     fprintf(outfile,"Doing 'Qualified Dividends and Capital Gain tax Worksheet', page 33.\n");
     capgains_qualdividends_worksheets( status );
    }
   else
   if (Do_SDTW)
   {
    fprintf(outfile,"Doing 'Schedule D Tax Worksheet', page D9.\n");
    sched_D_tax_worksheet( status );
   }
  } /*no_exception*/

 showline_wlabel( "L12a", L12a );


 /* -- Schedule 2 -- Additional Taxes */
 while (!gotS2_2)	/* Get any optional AMTws lines, or the next normal line S2_2. */
  {
   get_parameter( infile, 'l', labelx, "S2_2 or AMTwsXX or B7a");
   if (strcmp( labelx, "S2_2" ) == 0)
    {
     get_parameters( infile, 'f', &tmpval, labelx );
     Sched2[2] = tmpval;
     gotS2_2 = 1;
    }
   else
   if (strcasecmp( labelx, "AMTws2c" ) == 0)
    {
     get_parameters( infile, 'f', &amtws2c, labelx );
    }
   else
   if (strcasecmp( labelx, "AMTws2g" ) == 0)
    {
     get_parameters( infile, 'f', &amtws2g, labelx );
    }
   else
   if (strstr( labelx, "AMTws" ) != 0)
    { 
     get_parameters( infile, 'f', &tmpval, labelx );
     if ((sscanf( &(labelx[5]), "%d", &j) == 1) && (j >= 3) && (j < 3))
      amtws[j] = tmpval;
     else
      {
        printf("ERROR reading '%s'.\n", labelx ); 
        fprintf(outfile,"ERROR reading '%s'.\n", labelx ); 
      }
    }
   else
   if (strcmp( labelx, "B7a" ) == 0)
    {
     get_parameters( infile, 'b', &SchedB7a, labelx );
    }
   else
   if (strcmp( labelx, "B7aa" ) == 0)
    {
     get_parameters( infile, 'b', &SchedB7aa, labelx );
    }
   else
   if (strcmp( labelx, "B7b" ) == 0)
    {
     get_parameters( infile, 'w', &SchedB7b, labelx );
    }
   else
   if (strcmp( labelx, "B8" ) == 0)
    {
     get_parameters( infile, 'b', &SchedB8, labelx );
    }
   else
    {
     printf("ERROR1: Found '%s' when expecting 'S2_2 or AMTwsXX or B7a'\n", labelx ); 
     fprintf(outfile,"ERROR1: Found '%s' when expecting 'S2_2 or AMTwsXX'\n", labelx );
     exit(1);
    }
  }

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

 // GetLine( "S2_2", &Sched2[2] );	/* Excess advance premium tax credit repayment. Form 8962. */
					/* (Needed by AMT form6251.) */

 GetLine( "S2_4", &Sched2[4] );		/* Self-employment tax. Sched SE. */
 GetLine( "S2_5", &Sched2[5] );		/* Unreported social security and Medicare tax from Forms 4137, 8919 */
 GetLine( "S2_6", &Sched2[6] );		/* Additional tax on IRAs, other qualified retirement plan, Form 5329 */
 GetLine( "S2_7a", &Sched2[7] );	/* Household employment taxes. Sched H */
 GetLine( "S2_7b", &S2_7b );		/* First-time homebuyer credit repayment. Form 5405. */
 GetLine( "S2_8", &Sched2[8] );		/* Taxes from Forms 8959, 8960, others. */
 GetLine( "S2_9", &Sched2[9] );		/* Section 965 net tax liability installment from Form965-A. */

 GetLine( "S3_1", &Sched3[1] ); 	/*  Foreign tax credit. Form 1116. (Needed by AMT form6251.) */ 

 Sched2[1] = form6251_AlternativeMinimumTax( itemize );	/* (Depends on L11 and prior lines.) */
 if (Sched2[1] == 0.0)
  fprintf(outfile," (Not subject to Alternative Minimum Tax.)\n");
 else
  {
   fprintf(outfile," (You must pay Alternative Minimum Tax.)\n");
   showline_wlabelmsg( "S2_1", Sched2[1], "Alternative Minimum Tax" );
  }

 Sched2[3] = Sched2[1] + Sched2[2];
 showline_wlabelnz( "S2_3", Sched2[3] );
 showline_wlabelnz( "S2_4", Sched2[4] );
 showline_wlabelnz( "S2_5", Sched2[5] );
 showline_wlabelnz( "S2_6", Sched2[6] );
 showline_wlabelnz( "S2_7a", Sched2[7] );
 showline_wlabelnz( "S2_7b", S2_7b );
 Sched2[7] = Sched2[7] + S2_7b;
 showline_wlabelnz( "S2_8", Sched2[8] );
 showline_wlabelnz( "S2_9", Sched2[9] );
 for (j = 4; j <= 8; j++)
   Sched2[10] = Sched2[10] + Sched2[j];
 showline_wlabel( "S2_10", Sched2[10] );
 /* -- End of Schedule 2 */

 L[12] = Sched2[3] + L12a;
 showline( 12 );
 Report_bracket_info( L[11], Sched2[3], status );
 
 L[15] = Sched2[10];
 

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

 GetLine( "S3_6", &Sched3[6] );	/*  Other credits. Forms 3800, 8801, ect. */
 showline_wlabel( "S3_6", Sched3[6] );

 for (j = 1; j <= 6; j++)
  Sched3[7] = Sched3[7] + Sched3[j];
 showline_wlabel( "S3_7", Sched3[7] );
 L[13] = L13a + Sched3[7];

 GetLine( "S3_8", &Sched3[8] );		/* 2019 estimated payments + amnt applied from last year. */
 showline_wlabelnz( "S3_8", Sched3[8] );

 GetLine( "S3_9", &Sched3[9] );	/* Net premium tax credit. Form 8962. */
 showline_wlabelnz( "S3_9", Sched3[9] );

 GetLine( "S3_10", &Sched3[10] );	/* Amnt paid in filing extension req. */
 showline_wlabelnz( "S3_10", Sched3[10] );

 GetLine( "S3_11", &Sched3[11] );	/* Excess Soc. Sec. + tier 1 RRTA tax withheld */
 showline_wlabelnz( "S3_11", Sched3[11] );

 GetLine( "S3_12", &Sched3[12] );	/* Credits for federal tax on fuels. Attach form 4136. */
 showline_wlabelnz( "S3_12", Sched3[12] );

 GetLine( "S3_13", &Sched3[13] );	/* Credits from Form 2439, 4136, 6801, 8885 */
 showline_wlabelnz( "S3_13", Sched3[13] );

 for (j = 8; j <= 13; j++)
  Sched3[14] = Sched3[14] + Sched3[j];
 showline_wlabelnz( "S3_14", Sched3[14] );
 L18d = Sched3[14];

 /* -- End of Schedule 3 -- */

 showline(13);

 L[14] = NotLessThanZero( L[12] - L[13] );
 showline(14);
 showline(15);

 L[16] = L[14] + L[15];
 showline_wmsg( 16, "Total Tax" );

 showline( 17 );

 showline_wlabelnz( "L18a", L18a );
 showline_wlabelnz( "L18b", L18b );
 showline_wlabelnz( "L18c", L18c );
 showline_wlabelnz( "L18d", L18d );

 L[18] = L18a + L18b + L18c + L18d;
 showline_wlabelnz( "L18e", L[18] );
 
 L[19] = L[17] + L[18];
 showline_wmsg( 19, "Total Payments" );


 /* Refund or Owe sections. */
 if (L[19] > L[16])
  { /* Refund */
   L[20] = L[19] - L[16];
   fprintf(outfile,"L20 = %6.2f  Amount you Overpaid!!!\n", L[20] );
   fprintf(outfile,"L21a = %6.2f \n", L[20] );
  }
 else 
  { /* Tax-Due */
   L[23] = L[16] - L[19];
   fprintf(outfile,"L23 = %6.2f  DUE !!!\n", L[23] );
   fprintf(outfile,"         (Which is %2.1f%% of your Total Federal Tax.)\n", 100.0 * L[23] / (L[16] + 1e-9) );
  }
 ShowLineNonZero( 24 );
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
 GetTextLineF( "TownStateZip:" );

 fclose(infile);
 Grab_ScheduleB_Payer_Lines( infname, outfile );
 grab_any_pdf_markups( infname, outfile );
 fclose(outfile);

 printf("\nListing results from file: %s\n\n", outfname);
 Display_File( outfname );
 return 0;
}

#undef SINGLE
#undef MARRIED_FILLING_JOINTLY
#undef MARRIED_FILLING_SEPARAT
#undef HEAD_OF_HOUSEHOLD
#undef WIDOW
#undef Yes
#undef No

#undef MAX_LINES

} // namespace taxsolve_US_1040_2019
} // namespace OpenTaxSolver2019

#undef printf
#undef system
#ifdef _MSC_VER
#undef strcasecmp
#undef strncasecmp
#endif


