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

namespace taxsolve_MA_1_2019 {
#define SINGLE 		        1
#define MARRIED_FILLING_JOINTLY 2
#define MARRIED_FILLING_SEPARAT 3
#define HEAD_OF_HOUSEHOLD       4
#define WIDOW		        5
#define Yes 1
#define No  0
/************************************************************************/
/* taxsolve_ma_1_2019.c - OpenTaxSolver for Mass Form 1 		*/
/* Copyright (C) 2020 							*/
/* 									*/
/* OTS Project Home Page and Updates:  					*/
/*		http://opentaxsolver.sourceforge.com/			*/
/* 									*/
/* Compile:   cc taxsolve_ma_1_2019.c -o taxsolve_ma_1_2019		*/
/* Run:       ./taxsolve_ma_1_2019  Mass1_2019.txt			*/
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
/* Updated 1-2-2020	Aston Roberts 					*/
/************************************************************************/


float thisversion=17.00;




double Sum( double *v, int start_slot, int end_slot )
{
 int j;
 double result = 0.0;
 for (j=start_slot; j <= end_slot; j++) result += v[j];
 return result;
}


double ComputeTax(double taxableIncome)
{ double taxrate=0.0505;					/* Updated for 2019. */
 if (taxableIncome < 24000.0)
  return (int)(taxrate * (taxableIncome + 25.0) + 0.5);
 else
  return taxableIncome * taxrate;
}



/*----------------------------------------------------------------------------*/
/* ---				Main					  --- */
/*----------------------------------------------------------------------------*/
int main( int argc, char *argv[] )
{
 int i, j, k, status=0, i65, iblind, ndep, dep_deduct;
 int flag, notaxstatus=0;
 char word[4000], *infname=0, outfname[4000], *answ;
 time_t now;
 double Exemptions[10];
 double MassBankInterest, Iexempt, AGI;
 double Unemployment, Lottery;
 double MassRetirement[2];
 double L23a=0.0, L33[6], L35a=0.0, L35b=0.0;
 double L43a=0.0;
 
 printf("Massachusetts Form-1 2019 - v%3.2f\n", thisversion);
 
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

 /* Get status as:  Single, Married/joint, Head house, Married/sep. */
 get_parameter( infile, 's', word, "Status" );
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

 for (i=0; i<10; i++) {Exemptions[i] = 0.0;}

 switch (status)
  {
   case SINGLE: 
	Exemptions[0] = 4400.0;
	fprintf(outfile," Check_single x\n");
   	break;
   case MARRIED_FILLING_SEPARAT:
	Exemptions[0] = 4400.0;
	fprintf(outfile," Check_sep x\n");
   	break;
   case HEAD_OF_HOUSEHOLD:
	Exemptions[0] = 6800.0;
	fprintf(outfile," Check_hh x\n");
	break;
   case MARRIED_FILLING_JOINTLY: 
	Exemptions[0] = 8800.0;
	fprintf(outfile," Check_mfj x\n");
	break;
  }   

 GetLineF( "L1a", &(L[0]) );	/* Only for PDF form. */
 GetLineF( "L1b", &(L[0]) );	/* Only for PDF form. */
 
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
 if (status == MARRIED_FILLING_JOINTLY)
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
 showline_wmsg( 10, "TOTAL 5.1% INCOME" );
 
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

 GetLine( "L12", &L[12] );	/* Child under 13... */
 ShowLineNonZero(12);

 get_parameter( infile, 's', word, "L13"); /* Dependent under 12. */
 get_parameter( infile, 'i', &dep_deduct, "L13");
 if (dep_deduct > 2) dep_deduct = 2;
 if ((L[12] == 0) && ((status == MARRIED_FILLING_JOINTLY) || (status == HEAD_OF_HOUSEHOLD))
     && (dep_deduct > 0))
  {
   L[13] = dep_deduct * 3600.0;
   sprintf(word,"a. %d x 3,600 ", dep_deduct);
   showline_wmsg(13, word);
  }

 GetLine( "L14a", &L[14] );	/* Rental Paid */
 showline_wlabel( "14a", L[14] );
 L[14] = L[14] / 2.0;
 if (status == MARRIED_FILLING_SEPARAT)
  L[14] = smallerof( L[14] , 1500.0 );
 else
  L[14] = smallerof( L[14] , 3000.0 );
 ShowLineNonZero(14);

 GetLine( "L15", &L[15] );	/* Other Deductions (sched Y, L17) */
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
 showline_wmsg(21, "Total 5.1% Taxable Income");

 L[22] = ComputeTax( L[21] );
 showline_wmsg(22,"5.05% Tax");

 GetLine( "L23a", &L23a ); 	/* 12% income */
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

 if ((status == SINGLE) || (status == HEAD_OF_HOUSEHOLD) || (status == MARRIED_FILLING_JOINTLY))
 { /* AGI Worksheet pg 14. */
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
   if (status != MARRIED_FILLING_SEPARAT)
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
       case MARRIED_FILLING_JOINTLY: 
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
 showline_wmsg(28, "Total Tax");

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

 GetLine( "L38", &L[38] );	/* Mass income tax withheld, Forms W-2, 1099 */
 ShowLineNonZero(38);

 GetLine( "L39", &L[39] );	/* Last year's overpayment you want applied to 2019 estimated tax */
 ShowLineNonZero(39);

 GetLine( "L40", &L[40] );	/* 2019 estimated tax payments */
 ShowLineNonZero(40);

 GetLine( "L41", &L[41] );	/* Payments made with extension */
 ShowLineNonZero(41);

 GetLine( "L42", &L[42] );	/* Payments w/original return. Use only if amending return. */
 ShowLineNonZero(42);

 GetLine( "L43a", &L43a );	/* Earned income credit (EIC) */
 if (L43a != 0.0) fprintf(outfile, " L43a = %6.2f  x 0.23 = .....  ", L43a );
 L[43] = L43a * 0.23;
 ShowLineNonZero(43);

 GetLine( "L44", &L[44] );	/* Senior Circuit Breaker Credit, sched CB */
 ShowLineNonZero(44);

 GetLine( "L45", &L[45] );	/* Refundable credits, Sched RF, line 4. */
 ShowLineNonZero(45);

 GetLine( "L46", &L[46] );	/* Excess Paid Family Leave withholding. */
 ShowLineNonZero(46);

 L[47] = Sum( L, 38, 46 );
 showline_wmsg(47,"total payments");

 GetLine( "L49", &L[49] );	/* Overpayment to be applied to next year's estimated tax */

 /* Refund or Owe section. */
 if (L[37] < L[47]) 
  {
   L[48] = L[47] - L[37];
   fprintf(outfile,"L48 = %6.2f  Overpayment!\n", L[48] );
   if (L[49] > L[48])
    L[49] = L[48];
   showline_wmsg(49,"Overpayment to be applied to next year's estimated tax");
   L[50] = L[48] - L[49];
   fprintf(outfile,"L50 = %6.2f  THIS IS YOUR REFUND\n", L[50] );
  }
 else 
  {
   L[51] = L[37] - L[47];
   fprintf(outfile,"L51 = %6.2f  TAX DUE !!!\n", L[51] );
   fprintf(outfile,"         (Which is %2.1f%% of your total tax.)\n", 100.0 * L[51] / (L[37] + 1e-9) );
   if ((L[51] > 400.0) && (L[47] < 0.80 * L[37]))
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

 fclose(infile);
 grab_any_pdf_markups( infname, outfile );
 fclose(outfile);

 printf("\nListing results from file: %s\n\n", outfname);
 Display_File( outfname );
 printf("\nResults writen to file: %s\n", outfname);

 return 0;
}

#undef SINGLE
#undef MARRIED_FILLING_JOINTLY
#undef MARRIED_FILLING_SEPARAT
#undef HEAD_OF_HOUSEHOLD
#undef WIDOW
#undef Yes
#undef No
}
namespace taxsolve_NY_IT201_2019 {
#define SINGLE                  1
#define MARRIED_FILLING_JOINTLY 2
#define MARRIED_FILLING_SEPARAT 3
#define HEAD_OF_HOUSEHOLD       4
#define WIDOW                   5
#define Yes 1
#define No  0
/************************************************************************/
/* TaxSolve_NY_IT-201_2019.c - NY State Tax form IT-201 for 2019.	*/
/* Copyright (C) 2003-2020 - Aston Roberts, Skeet Monker		*/
/* 									*/
/* Compile:   gcc taxsolve_NY_IT201_2019.c -o taxsolve_NY_IT201_2019	*/
/* Run:	      ./taxsolve_NY_IT201_2019  NY_IT201_2019.txt 		*/
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
/* Modified for NY 2005-2019 taxes - Skeet Monker			*/
/************************************************************************/

float thisversion=17.00;


double A[10], S[10];


int 	status=0;

char 	statusnames[10][20]={"0","Single","Married/Joint","Married/Sep","Head_of_House","Widow"};
char 	*Your1stName="", *YourLastName="", *YourInitial="", 
	*Spouse1stName="", *SpouseLastName="", *SpouseInitial="";
char	*YourSocSec=0, *SpouseSocSec=0, *MailAddress=0, *AptNumber=0,
	Town[2048]="", StateName[1024]="", Zipcode[1024]="";

struct FedReturnData
 {
  double fedline[MAX_LINES], schedA[MAX_LINES], schedD[MAX_LINES],
	 sched[8][MAX_LINES], fed_L4b, fed_L4d, fed_L5b;
  int Exception, Itemized;
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
 fed_data->fed_L4d = 0.0;
 fed_data->fed_L5b = 0.0;
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
       if (strcmp(word,"L4b") == 0)
	fed_data->fed_L4b = fed_data->fedline[linenum];
       if (strcmp(word,"L4d") == 0)
	fed_data->fed_L4d = fed_data->fedline[linenum];
       if (strcmp(word,"L5b") == 0)
	fed_data->fed_L5b = fed_data->fedline[linenum];

       if (verbose) printf("FedLin[%d] = %2.2f\n", linenum, fed_data->fedline[linenum]);
      }
    }
   else
   if ((strstr(word,"A")==word) && (strstr(word,"AMT")!=word) && (strstr(fline," = ")!=0))
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
     if (verbose) printf("FedLin[%d] = %2.2f\n", linenum, fed_data->schedA[linenum]);
    }
   else
   if ((strstr(word,"D")==word) && (strstr(fline," = ")!=0))
    {
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
	  next_word(fline, word, " \teh=" );
	 }
      }
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
     if (verbose) printf("FedLin[%d] = %2.2f\n", linenum, fed_data->schedD[linenum]);
    }
   else
   if (strncmp(word,"S1_",3) == 0)
    {
     next_word( &(word[3]), tword, " \t=:");
     if (sscanf( tword, "%d", &linenum) != 1)
      printf("Error: Reading Fed sched1 line-number '%s'\n", word );
     else
      {
	next_word(fline, word, " \t=:");
	if (sscanf(word,"%lf", &fed_data->sched[1][linenum]) != 1) 
	 printf("Error: Reading Fed sched1 line '%s'\n", word );
      }
    }
   else
   if (strncmp(word,"S2_",3) == 0)
    {
     next_word( &(word[3]), tword, " \t=:");
     if (sscanf( tword, "%d", &linenum) != 1)
      printf("Error: Reading Fed sched2 line-number '%s'\n", word );
     else
      {
	next_word(fline, word, " \t=:");
	if (sscanf(word,"%lf", &fed_data->sched[2][linenum]) != 1) 
	 printf("Error: Reading Fed sched2 line '%s'\n", word );
      }
    }
   else
   if (strncmp(word,"S3_",3) == 0)
    {
     next_word( &(word[3]), tword, " \t=:");
     if (sscanf( tword, "%d", &linenum) != 1)
      printf("Error: Reading Fed sched3 line-number '%s'\n", word );
     else
      {
	next_word(fline, word, " \t=:");
	if (sscanf(word,"%lf", &fed_data->sched[3][linenum]) != 1) 
	 printf("Error: Reading Fed sched3 line '%s'\n", word );
      }
    }
   else
   if (strncmp(word,"S4_",3) == 0)
    {
     next_word( &(word[3]), tword, " \t=:");
     if (sscanf( tword, "%d", &linenum) != 1)
      printf("Error: Reading Fed sched4 line-number '%s'\n", word );
     else
      {
	next_word(fline, word, " \t=:");
	if (sscanf(word,"%lf", &fed_data->sched[4][linenum]) != 1) 
	 printf("Error: Reading Fed sched4 line '%s'\n", word );
      }
    }
   else
   if (strncmp(word,"S5_",3) == 0)
    {
     next_word( &(word[3]), tword, " \t=:");
     if (sscanf( tword, "%d", &linenum) != 1)
      printf("Error: Reading Fed sched5 line-number '%s'\n", word );
     else
      {
	next_word(fline, word, " \t=:");
	if (sscanf(word,"%lf", &fed_data->sched[5][linenum]) != 1) 
	 printf("Error: Reading Fed sched5 line '%s'\n", word );
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
	status = MARRIED_FILLING_JOINTLY; 
     else
     if (strncasecmp(word,"Married/Sep",11)==0)
	status = MARRIED_FILLING_SEPARAT;
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
   case MARRIED_FILLING_JOINTLY: case WIDOW:					/* Updated for 2019. */
	if (income <=   17150.0) tax = 	         0.04 * income; else		/* Data from pg 57. */
	if (income <=   23600.0) tax =   686.0 + 0.045  * (income - 17150.0); else
	if (income <=   27900.0) tax =   976.0 + 0.0525 * (income - 23600.0); else
	if (income <=   43000.0) tax =  1202.0 + 0.059  * (income - 27900.0); else
	if (income <=  161550.0) tax =  2093.0 + 0.0621 * (income - 43000.0); else
	if (income <=  323200.0) tax =  9455.0 + 0.0649 * (income - 161550.0); else
	if (income <= 2155350.0) tax = 19946.0 + 0.0685 * (income - 323200.0); 
	else			tax = 145448.0 + 0.0882 * (income - 2155350.0);
      break;
   case SINGLE: case MARRIED_FILLING_SEPARAT:
	if (income <=    8500.0) tax =  	  0.04   * income; else
	if (income <=   11700.0) tax =    340.0 + 0.045  * (income - 8500.0); else
	if (income <=   13900.0) tax =    484.0 + 0.0525 * (income - 11700.0); else
	if (income <=   21400.0) tax =    600.0 + 0.059  * (income - 13900.0); else
	if (income <=   80650.0) tax =   1042.0 + 0.0621 * (income - 21400.0); else
	if (income <=  215400.0) tax =   4721.0 + 0.0649 * (income - 80650.0); else
	if (income <= 1077550.0) tax =  13467.0 + 0.0685 * (income - 215400.0); 
	else 		 	 tax =  72524.0 + 0.0882 * (income - 1077550.0);
      break;
   case HEAD_OF_HOUSEHOLD:
	if (income <=   12080.0) tax = 	         0.04 * income; else
	if (income <=   17650.0) tax =   512.0 + 0.045  * (income - 12800.0); else
	if (income <=   20900.0) tax =   730.0 + 0.0525 * (income - 17650.0); else
	if (income <=   32200.0) tax =   901.0 + 0.059  * (income - 20900.0); else
	if (income <=  107650.0) tax =  1568.0 + 0.0621 * (income - 32200.0); else
	if (income <=  269300.0) tax =  6253.0 + 0.0649 * (income - 107650.0); else
	if (income <= 1616450.0) tax = 16744.0 + 0.0685 * (income - 269300.0);
	else			tax = 109024.0 + 0.0882 * (income - 1616450.0);
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
   case MARRIED_FILLING_JOINTLY: case WIDOW:				/* Updated for 2019. */
	if (income <=   17150.0) rate = 0.04;  else
	if (income <=   23600.0) rate = 0.045;  else
	if (income <=   27900.0) rate = 0.0525;  else
	if (income <=   43000.0) rate = 0.059;  else
	if (income <=  161550.0) rate = 0.0621;  else
	if (income <=  323200.0) rate = 0.0649;  else
	if (income <= 2155350.0) rate = 0.0685;  else  rate = 0.0882;
      break;
   case SINGLE: case MARRIED_FILLING_SEPARAT:
	if (income <=    8500.0) rate = 0.04;  else
	if (income <=   11700.0) rate = 0.045;  else
	if (income <=   13900.0) rate = 0.0525;  else
	if (income <=   21400.0) rate = 0.059;  else
	if (income <=   80650.0) rate = 0.0621;  else
	if (income <=  215400.0) rate = 0.0649;  else
	if (income <= 1077550.0) rate = 0.0685;  else  rate = 0.0882;
      break;
   case HEAD_OF_HOUSEHOLD:
	if (income <=   12800.0) rate = 0.04;  else
	if (income <=   17650.0) rate = 0.045;  else
	if (income <=   20900.0) rate = 0.0525;  else
	if (income <=   32200.0) rate = 0.059;  else
	if (income <=  107650.0) rate = 0.0621;  else
	if (income <=  269300.0) rate = 0.0649;  else
	if (income <= 1616450.0) rate = 0.0685;  else  rate = 0.0882;
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


double NYcityTaxRateFunction( double income, int status )	/* From page 69. */
{
 double tax, dx;
 int m;

 if (income < 25.0) dx = 12.5; else
 if (income < 50.0) dx = 25.0; else dx = 50.0;

 m = income / dx;             /* Round income to nearest $50. */
 if (income < 65000.0)
  income = m * dx + 0.5 * dx;      /* Place into center of a $50 bracket. */

 if ((status==MARRIED_FILLING_JOINTLY) || (status==WIDOW))		/* Updated for 2019. */
  {
   if (income < 21600.0)  tax = income * 0.03078; else
   if (income < 45000.0)  tax = (income - 21600.00) * 0.03762 + 665.00; else
   if (income < 90000.0)  tax = (income - 45000.00) * 0.03819 + 1545.0; else
			  tax = (income - 90000.00) * 0.03876 + 3264.0;
  }
 else
 if ((status==SINGLE) || (status==MARRIED_FILLING_SEPARAT))
  {
   if (income < 12000.0)  tax = income * 0.03078; else
   if (income < 25000.0)  tax = (income - 12000.00) * 0.03762 + 369.0;  else
   if (income < 50000.0)  tax = (income - 25000.00) * 0.03819 + 858.0;  else
			  tax = (income - 50000.00) * 0.03876 + 1813.00;
  }
 else
 if (status==HEAD_OF_HOUSEHOLD) 
  {
   if (income < 14400.00) tax = income * 0.03078; else
   if (income < 30000.00) tax = (income - 14400.00) * 0.03762 +  443.0;  else
   if (income < 60000.00) tax = (income - 30000.00) * 0.03819 + 1030.0;  else
			  tax = (income - 60000.00) * 0.03876 + 2176.0;
 }
 else {printf("Status not covered.\n");  exit(1);}

 if (income < 65000.0) tax = (int)(tax + 0.5);   /* Round result to whole dollar. */
 return tax;
}


void worksheet1()	/*Tax Computation Worksheet 1 (pg 58) */		/* Updated for 2019. */
{ double ws[100];
  ws[1] = L[33];
  ws[2] = L[38];
  ws[3] = 0.0621 * ws[2];
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


void worksheet2()	/*Tax Computation Worksheet 2 (pg 58) */
{ double ws[100];
  ws[1] = L[33];
  ws[2] = L[38];
  ws[3] = 0.0649 * ws[2];
  if (ws[1] >= 211550.0)
    ws[11] = ws[3];
  else
   {
    ws[4] = TaxRateFunction( ws[2], status );
    ws[5] = ws[3] - ws[4];
    ws[6] = 577.0;
    ws[7] = ws[5] - ws[6];
    ws[8] = ws[1] - 161550.0;
    /* Divide by 50k and round to forth decimal place. */
    ws[9] = 0.0001 * (double)Round( 10000.0 * (ws[8] / 50000.0) );
    ws[10] = ws[7] * ws[9];
    ws[11] = ws[4] + ws[6] + ws[10];
   }
  L[39] = ws[11];
}


void worksheet3()	/*Tax Computation Worksheet 3 (pg 58) */
{ double ws[100];
  ws[1] = L[33];
  ws[2] = L[38];
  ws[3] = 0.0685 * ws[2];
  if (ws[1] >= 373200.0)
   ws[11] = ws[3];
  else
   {
    ws[4] = TaxRateFunction( ws[2], status );
    ws[5] = ws[3] - ws[4];
    ws[6] = 1030.0;
    ws[7] = ws[5] - ws[6];
    ws[8] = ws[1] - 323200.0;
    /* Divide by 50k and round to forth decimal place. */
    ws[9] = 0.0001 * (double)Round( 10000.0 * (ws[8] / 50000.0) );
    ws[10] = ws[7] * ws[9];
    ws[11] = ws[4] + ws[6] + ws[10];
    }
   L[39] = ws[11];
  }


void worksheet4()	/*Tax Computation Worksheet 4 (pg 58) */
{ double ws[100];
  ws[1] = L[33];
  ws[2] = L[38];
  ws[3] = 0.0882 * ws[2];
  if (ws[1] >= 2205350.0)
   ws[11] = ws[3];
  else
   {
    ws[4] = TaxRateFunction( ws[2], status );
    ws[5] = ws[3] - ws[4];
    if (ws[2] <= 161550.0)
     ws[6] = 577.0;
    else
    if (ws[2] <= 323200.0)
     ws[6] = 1030.0;
    else
     ws[6] = 2193.0;
    ws[7] = ws[5] - ws[6];
    ws[8] = ws[1] - 2155350.0;
    /* Divide by 50k and round to forth decimal place. */
    ws[9] = 0.0001 * (double)Round( 10000.0 * (ws[8] / 50000.0) );
    ws[10] = ws[7] * ws[9];
    ws[11] = ws[4] + ws[6] + ws[10];
    }
   L[39] = ws[11];
  }


void worksheet5()	/*Tax Computation Worksheet 5 (pg 59) */
{ double ws[100];
  ws[1] = L[33];
  ws[2] = L[38];
  ws[3] = 0.0649 * ws[2];
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


void worksheet6()	/*Tax Computation Worksheet 6 (pg 59) */
{ double ws[100];
  ws[1] = L[33];
  ws[2] = L[38];
  ws[3] = 0.0685 * ws[2];
  if (ws[1] >= 265400.0)
    ws[11] = ws[3];
  else
   {
    ws[4] = TaxRateFunction( ws[2], status );
    ws[5] = ws[3] - ws[4];
    ws[6] = 513.0;
    ws[7] = ws[5] - ws[6];
    ws[8] = ws[1] - 215400.0;
    /* Divide by 50k and round to forth decimal place. */
    ws[9] = 0.0001 * (double)Round( 10000.0 * (ws[8] / 50000.0) );
    ws[10] = ws[7] * ws[9];
    ws[11] = ws[4] + ws[6] + ws[10];
   }
  L[39] = ws[11];
}


void worksheet7()	/*Tax Computation Worksheet 7 (pg 59) */
{ double ws[100];
  ws[1] = L[33];
  ws[2] = L[38];
  ws[3] = 0.0882 * ws[2];
  if (ws[1] >= 1127550.0)
   ws[11] = ws[3];
  else
   {
    ws[4] = TaxRateFunction( ws[2], status );
    ws[5] = ws[3] - ws[4];
    if (ws[2] <= 215400.0)
     ws[6] = 513.0;
    else
     ws[6] = 1288.0;
    ws[7] = ws[5] - ws[6];
    ws[8] = ws[1] - 1077550.0;
    /* Divide by 50k and round to forth decimal place. */
    ws[9] = 0.0001 * (double)Round( 10000.0 * (ws[8] / 50000.0) );
    ws[10] = ws[7] * ws[9];
    ws[11] = ws[4] + ws[6] + ws[10];
    }
   L[39] = ws[11];
  }


void worksheet8()	/*Tax Computation Worksheet 8 (pg 60) */
{ double ws[100];
  ws[1] = L[33];
  ws[2] = L[38];
  ws[3] = 0.0649 * ws[2];
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


void worksheet9()	/*Tax Computation Worksheet 9 (pg 60) */
{ double ws[100];
  ws[1] = L[33];
  ws[2] = L[38];
  ws[3] = 0.0685 * ws[2];
  if (ws[1] >= 319300.0)
    ws[11] = ws[3];
  else
   {
    ws[4] = TaxRateFunction( ws[2], status );
    ws[5] = ws[3] - ws[4];
    ws[6] = 733.0;
    ws[7] = ws[5] - ws[6];
    ws[8] = ws[1] - 269300.0;
    /* Divide by 50k and round to forth decimal place. */
    ws[9] = 0.0001 * (double)Round( 10000.0 * (ws[8] / 50000.0) );
    ws[10] = ws[7] * ws[9];
    ws[11] = ws[4] + ws[6] + ws[10];
   }
  L[39] = ws[11];
}


void worksheet10()	/*Tax Computation Worksheet 10 (pg 60) */
{ double ws[100];
  ws[1] = L[33];
  ws[2] = L[38];
  ws[3] = 0.0882 * ws[2];
  if (ws[1] >= 1666450.0)
   ws[11] = ws[3];
  else
   {
    ws[4] = TaxRateFunction( ws[2], status );
    ws[5] = ws[3] - ws[4];
    if (ws[2] <= 269300.0)
     ws[6] = 733.0;
    else
     ws[6] = 1703.0;
    ws[7] = ws[5] - ws[6];
    ws[8] = ws[1] - 1616450.0;
    /* Divide by 50k and round to forth decimal place. */
    ws[9] = 0.0001 * (double)Round( 10000.0 * (ws[8] / 50000.0) );
    ws[10] = ws[7] * ws[9];
    ws[11] = ws[4] + ws[6] + ws[10];
    }
   L[39] = ws[11];
  }


void tax_computation_worksheet( int status )
{ /* Worksheets from pages 58-60. Come here when AGI L[33] > $107,650. */
 switch (status)								/* Updated for 2019. */
  {
     case MARRIED_FILLING_JOINTLY:  case WIDOW:
	if (L[33] <= 2155350.0)
	 {
	   if (L[38] <= 161550.0)
	    worksheet1();
	   else
	   if ((L[33] > 161550.0) && (L[38] <= 323200.0))
	    worksheet2();
	   else
	   if ((L[33] > 323200.0) && (L[38] > 323200.0))
	    worksheet3();
	   else
	    worksheet4();
	 }
	else
	 worksheet4();
	break;
     case SINGLE:  case MARRIED_FILLING_SEPARAT:
	if (L[33] <= 1077550.0)
	 {
	   if (L[38] <= 215400.0)
	    worksheet5();
	   else
	    worksheet6();
	 }
	else
	 worksheet7();
	break;
     case HEAD_OF_HOUSEHOLD:
	if (L[33] <= 1616450.0)
	 {
	   if (L[38] <= 269300.0)
	    worksheet8();
	   else
	    worksheet9();
	 }
	else
	 worksheet10();
	break;
     default: printf("Case not handled.\n");  fprintf(outfile,"Case not handled.\n"); exit(1);
  }
}


/*----------------------------------------------------------------------------*/

int main( int argc, char *argv[] )
{
 int j, k, argk, day, month, yyyy;
 char word[1000], *infname=0, outfname[1000], *answ;
 time_t now;
 int Dependent, Exemptions, nyc_resident;
 double itemized_ded, std_ded=0.0, LTC=0, AddAdj=0.0, CollegeDed=0.0;
 double ded_sched[MAX_LINES];
 char prelim_1040_outfilename[5000];
 char YourNames[2048]="";

 /* Intercept any command-line arguments. */
 printf("NY-IT201 - 2011 - v%3.1f\n", thisversion);
 argk = 1;  k=1;
 while (argk < argc)
 {
  if (strcmp(argv[argk],"-verbose")==0)  verbose = 1;
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
   ded_sched[j] = 0.0;
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
 if (interpret_date( answ, &day, &month, &yyyy, "reading 'YourDOB'" ))
   fprintf(outfile,"YourDOB %s\n", format_mmddyyyy( month, day, yyyy ) );
 else
  fprintf(outfile,"YourDOB %s\n", answ );

 answ = GetTextLine( "SpouseDOB" );
 if (interpret_date( answ, &day, &month, &yyyy, "reading 'SpouseDOB'" ))
   fprintf(outfile,"SpouseDOB %s\n", format_mmddyyyy( month, day, yyyy ) );
 else
  fprintf(outfile,"SpouseDOB %s\n", answ );

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
 L[10] = PrelimFedReturn.fed_L4d;
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
 L[15] = PrelimFedReturn.fed_L5b;
 showline(15);
 L[27] = L[15];

 // GetLineF( "L16", &L[16] );	/* Other income (pg. 14) */
 L[16] = PrelimFedReturn.sched[1][8];
 showline(16);

 for (j = 1; j <= 11; j++)
  L[17] = L[17] + L[j];
 for (j=13; j <= 16; j++)
  L[17] = L[17] + L[j];
 showline(17);
 if (absolutev( L[17] - PrelimFedReturn.fedline[7]) > 0.1)
  {
   printf(" Warning: L[17] = %6.2f, while Fed-line[7b] = %6.2f\n", L[17], PrelimFedReturn.fedline[7] );
   fprintf(outfile," Warning: L[17] = %6.2f, while Fed-line[7b] = %6.2f\n", L[17], PrelimFedReturn.fedline[7] );
  }

 // GetLineF( "L18", &L[18] );	/* Total federal adjustments to income (pg 14) */
 L[18] = PrelimFedReturn.sched[1][22];
 showline(18);

 L[19] = L[17] - L[18];
 showline_wmsg( 19, "Federal adjusted gross income" );
 if (absolutev(L[19] - PrelimFedReturn.fedline[8]) > 0.1)
  {
   printf(" Warning: L[19] = %6.2f, while Fed-line[8b] = %6.2f\n", L[19], PrelimFedReturn.fedline[8] );
   fprintf(outfile," Warning: L[19] = %6.2f, while Fed-line[8b] = %6.2f\n", L[19], PrelimFedReturn.fedline[8] );
  } 

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
 if (status == MARRIED_FILLING_JOINTLY)
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

 ded_sched[1] = PrelimFedReturn.schedA[1];
 ded_sched[2] = L[19];
 ded_sched[3] = 0.10 * ded_sched[2];
 ded_sched[4] = ded_sched[1] - ded_sched[3];
 ded_sched[9] = PrelimFedReturn.schedA[7];
 ded_sched[15] = PrelimFedReturn.schedA[10];
 ded_sched[19] = PrelimFedReturn.schedA[14];
 ded_sched[20] = PrelimFedReturn.schedA[15];
 ded_sched[39] = PrelimFedReturn.schedA[16];
 ded_sched[40] = ded_sched[4] + ded_sched[9] + ded_sched[15] + ded_sched[19] + ded_sched[20] 
		 + ded_sched[28] + ded_sched[39];	
 itemized_ded = ded_sched[40];

 switch (status)	/* Determine the Std. Deduction. Pg. 21. */
  {
   case SINGLE: if (Dependent)   std_ded = 3100.0; 
		else 		 std_ded = 8000.0;			/* Updated for 2019. */
	break;
   case MARRIED_FILLING_JOINTLY: std_ded = 16050.0; break;
   case MARRIED_FILLING_SEPARAT: std_ded =  8000.0; break;
   case HEAD_OF_HOUSEHOLD: 	 std_ded = 11200.0; break;
   case WIDOW: 			 std_ded = 16050.0; break;
  }

 if (std_ded > itemized_ded) 
  {
   L[34] = std_ded;
   fprintf(outfile,"Check_Std = X\n");
   showline_wmsg(34,"(Mark Std-deduction)");
  }
 else
  {
   L[34] = itemized_ded;
   fprintf(outfile,"Check_Item = X\n");
   showline_wmsg(34,"(Mark Itemized-deduction)");
  }

 L[35] = L[33] - L[34];
 if (L[35] < 0.0) L[35] = 0.0;
 else showline(35);

 get_parameter( infile, 's', word, "L36" );	/* Number of Dependent Exemptions (Pg 76, line e) */
 get_parameters( infile, 'i', &k, "L36" );
 L[36] = 1000.0 * (double)k;
 showline(36);
 if (k > 0)
  fprintf(outfile, "L36_enter %d\n", k ); 

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
 else	/* Status = MARRIED_FILLING_JOINTLY, MARRIED_FILLING_SEPARAT, Head_of_house, Widow */
  if (status!=MARRIED_FILLING_SEPARAT)
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
   L[47] = NYcityTaxRateFunction( L[38], status ); 
   showline(47);

   /* NYC Household credit. */
   if (Dependent) L[48] = 0.0;
   else
   if (status==SINGLE)		/* From table 4, 5, or 6 on page 30. */
    {
     if (L[19] <  10000.0) L[48] = 15.0; else
     if (L[19] <  12500.0) L[48] = 10.0; else  L[48] = 0.0;
    }
   else	/* Status = 2, 4, or 5. */
   if (status!=MARRIED_FILLING_SEPARAT)
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

   L[49] = L[47] - L[48];
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
    if (L[37] < 250000)
     {
      if ((status==SINGLE) || (status==MARRIED_FILLING_SEPARAT) || (status==HEAD_OF_HOUSEHOLD))  L[69] = 63.0;  else
      if ((status==MARRIED_FILLING_JOINTLY) || (status==WIDOW))  L[69] = 125.0;
     }
    else
     L[69] = 0.0;
    showline(69);
    /* L[70] = earned_income_credit; */
    /* showline(70); */
  } /*NYC*/

 GetLineF( "L71", &L[71] );	/* Other refundable credits, IT-201-ATT line 18) */

 GetLineF( "L72", &L[72] );	/* Total NY State tax withheld. */

 GetLineF( "L73", &L[73] );	/* Total City of NY tax withheld. */
 GetLineF( "L74", &L[74] );	/* Yonkers tax withheld. */
 
 GetLineF( "L75", &L[75] );	/* Total estimated tax payments (from IT-370)*/

 for (j = 63; j <= 75; j++) L[76] = L[76] + L[j];
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

 fclose(infile);
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
}
namespace taxsolve_US_1040_Sched_C_2019 {
#define Yes 1
#define No  0
/************************************************************************/
/* TaxSolve_US1040_Sched_C.c -	 					*/
/* Copyright (C)  2019 - S.Jenkins					*/
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
/* Updated for 2019 tax year:						*/
/*  S.Jenkins 12-20-2019   						*/
/* Earlier Updates	Robert Heller  heller@deepsoft.com		*/
/************************************************************************/

float thisversion=17.00;



/*----------------------------------------------------------------------------*/

int main( int argc, char *argv[] )
{
 int i, j, k;
 char word[4000], outfname[4000], *EIN=0, *answ, *infname=0;
 time_t now;
 double L16b=0.0, L20b=0.0, L24b=0.0, Mileage=0.0;
 int L32;

 printf("US 1040 Schedule C, 2019 - v%3.2f\n", thisversion);

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
	Title:  Sched C 1999 Return
	L1		{Gross Receipts}
	L2		{Returns and Allowances}
	. . .
 */

 /* Accept Form's "Title" line, and put out with date-stamp for your records. */
 read_line( infile, word );
 now = time(0);
 fprintf(outfile,"\n%s,	 v%2.2f, %s\n", word, thisversion, ctime( &now ));

 GetTextLineF( "YourName:" );
 GetTextLineF( "YourSocSec#:" );
 GetTextLineF( "PrincipalBus:" );
 GetTextLineF( "BusinessName:" );
 GetTextLineF( "Number&Street:" );
 GetTextLineF( "TownStateZip:" );

 GetTextLineF( "ActivityCode:" );
 writeout_line = 0;	/* Suppress GetLineF's from immediately writing to outfile. */
 EIN = GetTextLineF( "BusinessEIN:" );
 format_socsec( EIN, 1 );
 fprintf(outfile,"BusinessEIN: %s\n", EIN );

 answ = GetTextLineF( "Fmethod:" );
 next_word( answ, word, " \t;" );
 if (strcasecmp( word, "Cash" ) == 0)
  fprintf(outfile,"CkFcash: X\n");
 else
  if (strcasecmp( word, "Accrual" ) == 0)
  fprintf(outfile,"CkFsccrual: X\n");
 else
  if (strcasecmp( word, "Other" ) == 0)
  fprintf(outfile,"CkFother: X\n");
 
 answ = GetTextLineF( "GPartic:" );
 next_word( answ, word, " \t;" );
 if (toupper( word[0] ) == 'Y')
  fprintf(outfile,"CkParticipate: X\n");
 else
 if ((mystrcasestr( word, "N/A" ) == 0) && (toupper( word[0] ) == 'N'))
  fprintf(outfile,"CkNotParticipate: X\n");
 
 answ = GetTextLineF( "Hacquired:" );
 next_word( answ, word, " \t;" );
 if (toupper( word[0] ) == 'Y')
  fprintf(outfile,"CkAcquired: X\n");

 answ = GetTextLineF( "Ireq1099s:" );
 next_word( answ, word, " \t;" );
 if (toupper( word[0] ) == 'Y')
  fprintf(outfile,"CkReq1099: X\n");
 else
 if ((mystrcasestr( word, "N/A" ) == 0) && (toupper( word[0] ) == 'N'))
  fprintf(outfile,"CkNotReq1099: X\n");
 
 answ = GetTextLineF( "Jfile1099s:" );
 next_word( answ, word, " \t;" );
 if (toupper( word[0] ) == 'Y')
  fprintf(outfile,"CkWillFile1099: X\n");
 else
 if ((mystrcasestr( word, "N/A" ) == 0) && (toupper( word[0] ) == 'N'))
  fprintf(outfile,"CkNotFile1099: X\n");
 
 writeout_line = 1;

 GetLine( "L1", &L[1] );	/* Gross Receipts */

 GetLine( "L2", &L[2] );	/* Returns and Allowances */	

 // GetLine( "L4", &L[4] );	/* Cost of Goods Sold */

 GetLine( "L6", &L[6] );	/* Other income, including fed & state fuel credit */

 GetLine( "L8", &L[8] );	/* Advertising */

 GetLine( "L9", &L[9] );	/* Car & truck expenses */

 GetLine( "Miles", &Mileage );	/* Miles for Line 9, not already included in Line 9. */
 L[9] = L[9] + 0.58 * Mileage;					/* Updated for 2019. */

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

 GetLine( "L27a", &L[27] );	/* Other expenses here from line 48 pg 2 */

 GetLine( "L30", &L[30] );	/* Expenses for business use of home (form 8829) */

 get_parameter( infile, 's', word, "L32a" );  /* Yes or No, All investment is at risk */
 get_parameter( infile, 'b', &L32, "L32a");

 /* Part III */

 writeout_line = 0;	/* Suppress GetLineF's from immediately writing to outfile. */
 answ = GetTextLineF( "L33:" );
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
  printf("Warning: Unexpted answer for L33: '%s'\n", word ); 

 answ = GetTextLineF( "L34:" );
 next_word( answ, word, " \t;" );
 if (toupper( word[0] ) == 'Y')
  fprintf(outfile,"Ck34Yes: X\n");
 else
 if (toupper( word[0] ) == 'N')
  fprintf(outfile,"Ck34No: X\n");
 writeout_line = 1;
 
 GetLine( "L35", &L[35] );	/* Inventory at beginning of year */

 GetLine( "L36", &L[36] );	/* Purchases minus cost of personel items */

 GetLine( "L37", &L[37] );	/* Cost of labor (not paid to yourself) */

 GetLine( "L38", &L[38] );	/* Materials & supplies */

 GetLine( "L39", &L[39] );	/* Other costs */

 L[40] = L[35] + L[36] + L[37] + L[38] + L[39];

 GetLine( "L41", &L[41] );	/* Inventory at year end */

 L[42] = L[40] - L[41];


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

 writeout_line = 0;
 answ = GetTextLineF( "L43:" );
 next_word( answ, word, " \t-/.,;" );
 fprintf(outfile,"L43mm: %s\n", word);
 next_word( answ, word, " \t-/.,;" );
 fprintf(outfile,"L43dd: %s\n", word);
 next_word( answ, word, " \t-/.,;" );
 fprintf(outfile,"L43yy: %s\n", word);
 writeout_line = 1;
 GetTextLineF( "L44a" );
 GetTextLineF( "L44b" );
 GetTextLineF( "L44c" );

 writeout_line = 0;     /* Suppress GetLineF's from immediately writing to outfile. */

 answ = GetTextLineF( "L45:" );
 next_word( answ, word, " \t;" );
 if (toupper( word[0] ) == 'Y')
  fprintf(outfile,"Ck45Yes: X\n");
 else
 if (strcasecmp( word, "No") == 0)
  fprintf(outfile,"Ck45No: X\n");

 answ = GetTextLineF( "L46:" );
 next_word( answ, word, " \t;" );
 if (toupper( word[0] ) == 'Y')
  fprintf(outfile,"Ck46Yes: X\n");
 else
 if (strcasecmp( word, "No") == 0)
  fprintf(outfile,"Ck46No: X\n");

 answ = GetTextLineF( "L47a:" );
 next_word( answ, word, " \t;" );
 if (toupper( word[0] ) == 'Y')
  fprintf(outfile,"Ck47aYes: X\n");
 else
 if (strcasecmp( word, "No") == 0)
  fprintf(outfile,"Ck47aNo: X\n");

 answ = GetTextLineF( "L47b:" );
 next_word( answ, word, " \t;" );
 if (toupper( word[0] ) == 'Y')
  fprintf(outfile,"Ck47bYes: X\n");
 else
 if (strcasecmp( word, "No") == 0)
  fprintf(outfile,"Ck47bNo: X\n");

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
namespace taxsolve_VA_760_2019 {
#define SINGLE 		        1
#define MARRIED_FILLING_JOINTLY 2
#define MARRIED_FILLING_SEPARAT 3
#define HEAD_OF_HOUSEHOLD       4
#define WIDOW		        5
/************************************************************************/
/* TaxSolve_VA760_2019.c - 						*/
/* Copyright (C) 2020 - Aston Roberts					*/
/* 									*/
/* Compile:   gcc taxsolve_VA760_2019.c -o taxsolve_VA760_2019		*/
/* Run:	      ./taxsolve_VA760_2019  VA_760_2019.txt 			*/
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
 printf("VA-760 2019 - v%3.1f\n", thisversion);
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

 /* get_parameter(infile, kind, x, emssg ) */
 get_parameter( infile, 's', word, "Status" );
 get_parameter( infile, 'l', word, "Status ?");
 if (strncasecmp(word,"Single",4)==0) status = SINGLE; else
 if (strncasecmp(word,"Married/Joint",13)==0) status = MARRIED_FILLING_JOINTLY; else
 if (strncasecmp(word,"Married/Sep",11)==0) status = MARRIED_FILLING_SEPARAT; else
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
 if (status == MARRIED_FILLING_JOINTLY)
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
 if (status == MARRIED_FILLING_JOINTLY)
  exemptionsA = 2 + exemptionsA;
 else
  exemptionsA = 1 + exemptionsA; 
 fprintf(outfile,"NExemptionsA = %d\n", exemptionsA );
 fprintf(outfile,"ExemptionsA = %d\n", 930 * exemptionsA );

 if (yourDOB.year < 1954)
  {
   fprintf(outfile,"YouOver65 = 1\n" );
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
 if (status == MARRIED_FILLING_JOINTLY)
  {
   if (spouseDOB.year < 1954)
    {
     fprintf(outfile,"SpouseOver65 = 1\n" );
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

 if (status == MARRIED_FILLING_JOINTLY)
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

 GetLineF( "L4", &L[4] );	/* Deduction for age on Jan 1, 2019. */

 GetLineF( "L5", &L[5] );	/* Social Security Act, Tier 1 Railroad Retirement Act benef. */

 GetLineF( "L6", &L[6] );	/* State Income Tax refund or overpayment credit */

 GetLineF( "L7", &L[7] );	/* Subtractions from Schedule ADJ, line 7 */

 L[8] = L[4] + L[5] + L[6] + L[7];
 showline(8);

 L[9] = L[3] - L[8]; 
 showline(9);			/* Virginia Adjusted Gross Income (VAGI) */

 GetLineF( "L10", &L[10] );	/* Deductions - Std or Itemized minus income taxes */

 switch (status)
  {
   case SINGLE:  		  std_ded = 4500.0;  min2file = 11950.0;  break;
   case MARRIED_FILLING_JOINTLY:  std_ded = 9000.0;  min2file = 23900.0;  break;
   case MARRIED_FILLING_SEPARAT:  std_ded = 4500.0;  min2file = 11950.0;  break;
   default:  printf("Unexpected status.\n");
	     fprintf(outfile,"Unexpected status.\n");
	     exit(1);  
	break;
  }

 if (L[10] != 0.0)
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

 GetLineF( "L19a", &L[19] );	/* Virginia tax withheld for 2019. */
 GetLineF( "L19b", &L19b );	/* Spouse's Virginia tax withheld. */

 GetLineF( "L20", &L[20] );	/* Estimated tax paid for 2019. (form 760ES) */

 GetLineF( "L21", &L[21] );	/* Amount of last year's overpayment applied toward 2019 estimated tax. */

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
#undef MARRIED_FILLING_JOINTLY
#undef MARRIED_FILLING_SEPARAT
#undef HEAD_OF_HOUSEHOLD
#undef WIDOW
}
namespace taxsolve_OH_IT1040_2019 {
#define SINGLE 		        1
#define MARRIED_FILLING_JOINTLY 2
#define MARRIED_FILLING_SEPARAT 3
#define HEAD_OF_HOUSEHOLD       1
/************************************************************************/
/* TaxSolve_OH_IT1040_2019.c - 						*/
/* Copyright (C) 2020 - Aston Roberts					*/
/* 									*/
/* Compile:   gcc taxsolve_OH_IT1040_2019.c -o taxsolve_OH_IT1040_2019	*/
/* Run:	      ./taxsolve_OH_IT1040_2019  OH_IT1040_2019.txt 		*/
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



double thisversion=17.00;


double TaxRateFunction( double x, int status )
{							/* Updated for 2019. */
 if (x <= 21750.0) return    0.0; else
 if (x <  43450.0) return  310.47 + (x-21750.0)  * 0.02850; else
 if (x <  86900.0) return  928.92 + (x-43450.0)  * 0.03326; else
 if (x < 108700.0) return 2374.07 + (x-86900.0)  * 0.03802; else
 if (x < 217400.0) return 3202.91 + (x-108700.0) * 0.04413;
 else 		   return 7999.84 + (x-217400.0) * 0.04797;
}


void Report_bracket_info( double income, double tx, int status )
{							/* Updated for 2019. */
 double rate;
 if (income <= 21750.0) rate = 0.0; else
 if (income <  43450.0) rate = 0.02850; else
 if (income <  86900.0) rate = 0.03326; else
 if (income < 108700.0) rate = 0.03802; else
 if (income < 217400.0) rate = 0.04413;
 else 		   	rate = 0.04797;
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
 double factorA, factorB;
 double L2a, L2b, L7a, L8a, L8b, L8c;
 double jfc, exemption_amnt;
 double SchedA[MAX_LINES], SchedC[MAX_LINES];

 /* Intercept any command-line arguments. */
 printf("OH IT1040 2019 - v%3.1f\n", thisversion);
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

 /* get_parameter(infile, kind, x, emssg ) */
 get_parameter( infile, 's', word, "Status" );
 get_parameter( infile, 'l', word, "Status ?");
 if (strncasecmp(word,"Single",4)==0) status = SINGLE; else
 if (strncasecmp(word,"Married/Joint",11)==0) status = MARRIED_FILLING_JOINTLY; else
 if (strncasecmp(word,"Married/Sep",11)==0) status = MARRIED_FILLING_SEPARAT; else
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

 GetLine( "SchedA_33", &SchedA[33] );	/* Disability benefits */
 GetLine( "SchedA_34", &SchedA[34] );	/* Survivorship benefits */
 GetLine( "SchedA_35", &SchedA[35] );	/* Unreimbursed long-term care insurance premiums ... */
 GetLine( "SchedA_36", &SchedA[36] );	/* Funds deposited into, and earnings of, a medical savings account */
 GetLine( "SchedA_37", &SchedA[37] );	/* Qualified organ donor expenses */
 
 /* Schedule of Credits. */
 GetLine( "Credits_2", &SchedC[2] );	/* Retirement income credit */
 SchedC[2] = smallerof( SchedC[2], 200.0 );
 GetLine( "Credits_3", &SchedC[3] );	/* Lump sum retirement credit (Ohio LS WKS, line 6) */
 GetLine( "Credits_4", &SchedC[4] );	/* Senior citizen credit */
 SchedC[4] = smallerof( SchedC[4], 50.0 );
 GetLine( "Credits_5", &SchedC[5] );	/* Lump sum distribution credit */
 GetLine( "Credits_6", &SchedC[6] );	/* Child care and dependent care credit */
 GetLine( "Credits_7", &SchedC[7] );	/* Displaced worker training credit */
 GetLine( "Credits_8", &SchedC[8] );	/* Ohio political contributions credit */
 if (status == MARRIED_FILLING_JOINTLY)
  {
   SchedC[7] = smallerof( SchedC[7], 1000.0 );
   SchedC[8] = smallerof( SchedC[8], 100.0 );
  }
 else
  {
   SchedC[7] = smallerof( SchedC[7], 500.0 );
   SchedC[8] = smallerof( SchedC[8], 50.0 );
  }

 GetLine( "Credits_13", &SchedC[13] );	/* Earned income credit */
 GetLine( "Credits_14", &SchedC[14] );	/* Ohio adoption credit */
 GetLine( "Credits_15", &SchedC[15] );	/* Job retention credit, nonrefundable portion */
 GetLine( "Credits_16", &SchedC[16] );	/* Credit for eligible new employees in an enterprise zone */
 GetLine( "Credits_17", &SchedC[17] );	/* Credit for purchases of grape production property */
 GetLine( "Credits_18", &SchedC[18] );	/* Invest Ohio */
 GetLine( "Credits_19", &SchedC[19] );	/* Opportunity zone investment credit */
 GetLine( "Credits_20", &SchedC[20] );	/* Tech investment credit */
 GetLine( "Credits_21", &SchedC[21] );	/* Enterprise zone day care and training credits */
 GetLine( "Credits_22", &SchedC[22] );	/* Research and development credit */
 GetLine( "Credits_23", &SchedC[23] );	/* Ohio historic preservation credit, nonrefundable carryforward portion */

 GetLine( "Credits_26", &SchedC[26] );	/*  Portion L3 was not earned in Ohio. */

 GetLine( "Credits_29", &SchedC[29] );	/* Portion L3 taxed by other states */
 GetLine( "Credits_32", &SchedC[32] );	/* Income Tax paid to Other States */

 GetLine( "Credits_35", &SchedC[35] );	/* Historic preservation credit */
 GetLine( "Credits_36", &SchedC[36] );	/* Business jobs credit */
 GetLine( "Credits_37", &SchedC[37] );	/* Pass-through entity credit */
 GetLine( "Credits_38", &SchedC[38] );	/* Motion picture production credit */
 GetLine( "Credits_39", &SchedC[39] );	/* Financial Institutions Tax (FIT) credit */
 GetLine( "Credits_40", &SchedC[40] );	/* Venture capital credit */


 /* ---- Do Calculations. ---- */

 for (j=1; j <= 9; j++)
  SchedA[10] = SchedA[10] + SchedA[j];

 for (j=11; j <= 37; j++)
  SchedA[38] = SchedA[38] + SchedA[j];

 L2a = SchedA[10];
 L2b = SchedA[38];
 L[3] = L[1] + L2a - L2b;

 if (L[3] <= 40000.0)			/* Updated for 2019. */
  exemption_amnt = 2350.0;
 else
 if (L[3] <= 80000.0)
  exemption_amnt = 2100.0;
 else
  exemption_amnt = 1850.0;
 L[4] = exemption_amnt * exemptions;

 L[5] = NotLessThanZero( L[3] - L[4] );
 L[7] = NotLessThanZero( L[5] - L[6] );
 L7a = L[7];
 L8a = TaxRateFunction( L7a, status );
 L8c = L8a + L8b;
 SchedC[1] = L8c;

 if (L[5] < 30000.0)
  SchedC[9] = 20.0 * exemptions;
 
 for (j=2; j <= 9; j++)
  SchedC[10] = SchedC[10] + SchedC[j];		

 SchedC[11] = NotLessThanZero( SchedC[1] - SchedC[10] );

 if ((status == MARRIED_FILLING_JOINTLY) && (qualify_jfc))
  { /*Joint_Filing_Credit*/
    if (L[5] < 25000) jfc = 0.20;
    else
    if (L[5] < 50000) jfc = 0.15;
    else
    if (L[5] < 75000) jfc = 0.10;
    else jfc = 0.05;
    SchedC[12] = smallerof( jfc * L[11], 650.0 );
  } /*Joint_Filing_Credit*/

 for (j=12; j <= 23; j++)
  SchedC[24] = SchedC[24] + SchedC[j];          
 SchedC[25] = NotLessThanZero( SchedC[11] - SchedC[24] );

 SchedC[27] = L[3];
 j = 10000.0 * SchedC[26] / SchedC[27];
 factorA = (double)j / 10000.0;
 // printf(" %4g\n", factorA );
 SchedC[28] = SchedC[25] * factorA;

 SchedC[29] = L[3];
 j = 10000.0 * SchedC[28] / SchedC[29];
 factorB = (double)j / 10000.0;
 // printf(" %4g\n", factorB );			
 SchedC[30] = SchedC[24] * factorB;
 // SchedC[31] = L[13];
 SchedC[32] = smallerof( SchedC[30], SchedC[31] );

 SchedC[34] = SchedC[10] + SchedC[24] + SchedC[28] + SchedC[33];
 L[9] = SchedC[34];

 for (j=35; j <= 40; j++)
  SchedC[41] = SchedC[41] + SchedC[j];          
 L[16] = SchedC[41];

 L[10] = NotLessThanZero( L8c - L[9] );
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

 if ((L[1] < 21750.0) && (L[3] < 0.0))		/* Min2File. */
  fprintf(outfile, "You do not need to file Ohio tax return (Fed AGI < minimum).\n");

 if ((L[1] < 21750.0) && (L[4] >= L[3]))
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

 fprintf(outfile,"\n-- 2019 Ohio Schedule A --\n");
 for (j = 1; j <= 38; j++)
  {
   sprintf( label, "SchedA%d", j );
   showline_wlabel( label, SchedA[j] );
  }

 fprintf(outfile,"\n-- 2019 Ohio Schedule of Credits --\n");
 for (j = 1; j <= 27; j++)
  {
   sprintf( label, "Credits%d", j );
   showline_wlabel( label, SchedC[j] );
  }
 sprintf(word,"%5.4f", factorA);
printf("factorA = %g, word = '%s'\n", factorA, word );
 fprintf(outfile,"   Credits28_Factor %s\n", &(word[2]) );
 showline_wlabel( "Credits28", SchedC[28] );
 showline_wlabel( "Credits29", SchedC[29] );
 showline_wlabel( "Credits30", SchedC[30] );
 sprintf(word,"%5.4f", factorB );
printf("factorB = %g, word = '%s'\n", factorB, word );
 fprintf(outfile,"   Credits31_Factor %s\n", &(word[2]) );

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
 socsec = GetTextLineF( "YourSocSec#:" );
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
 socsec = GetTextLineF( "SpouseSocSec#:" );
 format_socsec( socsec, 0 );
 if (status != MARRIED_FILLING_SEPARAT)
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
 if (status == MARRIED_FILLING_JOINTLY)
  fprintf(outfile,"CkFYrResSp: X\n");

 fclose(infile);
 grab_any_pdf_markups( infname, outfile );
 fclose(outfile);
 Display_File( outfname );
 printf("\nResults written to file:  %s\n", outfname);
 return 0;
}

#undef SINGLE
#undef MARRIED_FILLING_JOINTLY
#undef MARRIED_FILLING_SEPARAT
#undef HEAD_OF_HOUSEHOLD
}
namespace taxsolve_PA_40_2019 {
#define thisversion 17.00
#define SINGLE 		        1
#define MARRIED_FILLING_JOINTLY 2
#define MARRIED_FILLING_SEPARAT 3
#define WIDOW		        1
#define Yes 1
#define No  0
/************************************************************************/
/* TaxSolve_PA40_2019.c - Pennsylvania 2019 PA-40 State Tax Form.	*/
/* Copyright (C) 2020, - Aston Roberts					*/
/* 									*/
/* Compile:   gcc taxsolve_PA40_2019.c -o taxsolve_PA40_2019		*/
/* Run:	      ./taxsolve_PA40_2019  PA40_2019.txt 			*/
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




double Tax_Rate = 0.0307;		/* Updated for 2019 tax-year. */


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
 char word[2000], *infname=0, outfname[1500];
 time_t now;
 double oneA, oneB;
 char *Your1stName=0, *YourLastName=0, *Spouse1stName=0, *SpouseLastName, *YourNames;

 /* Decode any command-line arguments. */
 printf("PA40 - 2019 - v%3.1f\n", thisversion);
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

 /* get_parameter(infile, kind, x, emssg ) */
 get_parameter( infile, 's', word, "Status" );	/* Single, Married/joint, Married/sep, Widow(er) */
 get_parameter( infile, 'l', word, "Status?");
 if (strncasecmp(word,"Single",4)==0) status = SINGLE; else
 if (strncasecmp(word,"Married/Joint",13)==0) status = MARRIED_FILLING_JOINTLY; else
 if (strncasecmp(word,"Married/Sep",11)==0) status = MARRIED_FILLING_SEPARAT; else
 if (strncasecmp(word,"Widow",4)==0) status = WIDOW;
 else
  { 
   printf("Error: unrecognized status '%s'. Exiting.\n", word); 
   fprintf(outfile,"Error: unrecognized status '%s'. Exiting.\n", word); 
   exit(1);
  }
 fprintf(outfile,"Status = %s (%d)\n", word, status);

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

 GetLineF( "L15", &L[15] );	/* 2019 Estimated Installment payments. */

 GetLineF( "L16", &L[16] );	/* 2019 Extension payment. */

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
   if (status == MARRIED_FILLING_JOINTLY)
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
#undef MARRIED_FILLING_JOINTLY
#undef MARRIED_FILLING_SEPARAT
#undef WIDOW
#undef Yes
#undef No
}
namespace taxsolve_US_1040_2019 {
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
}
namespace taxsolve_NJ_1040_2019 {
#define SINGLE 		        1
#define MARRIED_FILLING_JOINTLY 2
#define MARRIED_FILLING_SEPARAT 3
#define HEAD_OF_HOUSEHOLD       4
#define WIDOW		        5
/************************************************************************/
/* TaxSolve_NJ_1040_2019.c - 						*/
/* Copyright (C) 2020 - Aston Roberts					*/
/* 									*/
/* Compile:   gcc taxsolve_NJ_1040_2019.c -o taxsolve_NJ_1040_2019	*/
/* Run:	      ./taxsolve_NJ_1040_2019  NJ_1040_2019.txt 		*/
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

float thisversion=17.00;



double COJ[MAX_LINES], S[MAX_LINES], F[MAX_LINES];



double TaxRateFormula( double x, int status )
{
 if ((status==SINGLE) || (status==MARRIED_FILLING_SEPARAT))	/* Single, Married/sep */
  {
   if (x < 20000.0)   return x * 0.014;               else
   if (x < 35000.0)   return x * 0.0175  -     70.0;  else
   if (x < 40000.0)   return x * 0.035   -    682.5;  else
   if (x < 75000.0)   return x * 0.05525 -   1492.5;  else
   if (x < 500000.0)  return x * 0.0637  -   2126.25; else
   if (x < 5000000.0) return x * 0.0897  -  15126.25;
   else		      return x * 0.1075  - 104126.25;
  }
 else
 if ((status==MARRIED_FILLING_JOINTLY) || (status==HEAD_OF_HOUSEHOLD) || (status==WIDOW))
  {								/* Married/Joint, HouseHead, widower. */
   if (x < 20000.0)   return x * 0.014;              else
   if (x < 50000.0)   return x * 0.0175  -     70.0; else
   if (x < 70000.0)   return x * 0.0245  -    420.0; else
   if (x < 80000.0)   return x * 0.035   -   1154.5; else
   if (x < 150000.0)  return x * 0.05525 -   2775.0; else
   if (x < 500000.0)  return x * 0.0637  -   4042.5; else
   if (x < 5000000.0) return x * 0.0897  -  17042.5;
   else		      return x * 0.1075  - 106042.50;
  }
 else { printf("Status not covered.\n"); exit(1); }
}


void Report_bracket_info( double x, int status )
{
 double tx, rate;
 tx = TaxRateFormula( x, status );
 if ((status==SINGLE) || (status==MARRIED_FILLING_SEPARAT))	/* Single, Married/sep */
  {
   if (x < 20000.0)   rate = 0.014;	else
   if (x < 35000.0)   rate = 0.0175;	else
   if (x < 40000.0)   rate = 0.035;	else
   if (x < 75000.0)   rate = 0.05525;	else
   if (x < 500000.0)  rate = 0.0637;    else
   if (x < 5000000.0) rate = 0.0897;
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
   if (x < 500000.0)  rate = 0.0897;
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



/*----------------------------------------------------------------------------*/

int main( int argc, char *argv[] )
{
 int i, j, k;
 char word[1000], *infname=0, outfname[4000];
 int status=0;
 time_t now;
 double L16b=0.0, L20b=0.0, L28a=0.0, L28b=0.0;
 double COJ_b[10], COJ_9a=0.0, proptxcredit;
 double H[10], Hb[10];	/* Worksheet H, added by BWB. */
 double I[10], Ib[10];	/* Worksheet I. */
 char *Your1stName="", *YourLastName="", *YourInitial="", *Spouse1stName="", *SpouseLastName="", *SpouseInitial="";
 char YourNames[2048]="";

 /* Intercept any command-line arguments. */
 printf("NJ 1040 2019 - v%3.1f\n", thisversion);
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

 /* get_parameter(infile, kind, x, mesage ) */
 get_parameter( infile, 's', word, "Status" );
 get_parameter( infile, 'l', word, "Status ?");
 if (strncasecmp(word,"Single",4)==0) status = SINGLE; else
 if (strncasecmp(word,"Married/Joint",13)==0) status = MARRIED_FILLING_JOINTLY; else
 if (strncasecmp(word,"Married/Sep",11)==0) status = MARRIED_FILLING_SEPARAT; else
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
  case MARRIED_FILLING_JOINTLY: fprintf(outfile,"Status = Married/Joint (%d)\n", status); 
				fprintf(outfile," Check_Spouse = X\n"); 
				L[6] = 2;
				break;
  case MARRIED_FILLING_SEPARAT: fprintf(outfile,"Status = Married/Sep (%d)\n", status);
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

 get_parameter( infile, 's', word, "YouOver65" );	/* Exemptions, Over 65. */
 get_parameter( infile, 'b', &j, "YouOver65"); 
 L[7] = j;
 if (j) fprintf(outfile," Check_Over65 = X\n");

 get_parameter( infile, 's', word, "SpouseOver65" );	/* Exemptions, Spouse Over 65. */
 get_param_single_line( infile, 'b', &j, "SpouseOver65"); 
 if (status == MARRIED_FILLING_JOINTLY)
  {
   L[7] = L[7] + j;
   if (j) fprintf(outfile," Check_SpOver65 = X\n");
  }
 fprintf(outfile, "L7a = %d\n", (int)(L[7]) );
 L[7] = 1000.0 * L[7];
 shownum(7); 

 get_parameter( infile, 's', word, "YouBlindDisa" );	/* Exemptions, Blind/disabled. */
 get_parameter( infile, 'b', &j, "YouBlindDisa"); 
 L[8] = j;
 if (j) fprintf(outfile," Check_Blind = X\n");

 get_parameter( infile, 's', word, "SpouseBlindDisa" );    /* Exemptions, Spouse Blind/disabled. */
 get_param_single_line( infile, 'b', &j, "SpouseBlindDisa"); 
 if (status == MARRIED_FILLING_JOINTLY)
  {
   L[8] = L[8] + j;
   if (j) fprintf(outfile," Check_SpBlind = X\n");
  }
 fprintf(outfile, "L8a = %d\n", (int)(L[8]) );
 L[8] = 1000.0 * L[8];
 shownum(8); 

 get_parameter( infile, 's', word, "YouVeteran" );	/* Exemptions, Veteran */
 get_parameter( infile, 'b', &j, "YouVeteran"); 
 L[9] = j;
 if (j) fprintf(outfile," Check_Vet = X\n");

 get_parameter( infile, 's', word, "SpouseVeteran" );    /* Exemptions, Spouse Veteran */
 get_param_single_line( infile, 'b', &j, "SpouseVeteran"); 
 if (status == MARRIED_FILLING_JOINTLY)
  {
   L[8] = L[8] + j;
   if (j) fprintf(outfile," Check_SpVet = X\n");
  }
 fprintf(outfile, "L9a = %d\n", (int)(L[9]) );
 L[9] = 6000.0 * L[9];
 shownum(9); 

 get_parameter( infile, 's', word, "L10" );	/* Exemptions, children. */
 get_parameter( infile, 'i', &j, "L10"); 
 fprintf(outfile, "L10a = %d\n", j );
 L[10] = 1500.0 * j;
 shownum(10); 

 get_parameter( infile, 's', word, "L11" );	/* Exemptions, other dependents. */
 get_parameter( infile, 'i', &j, "L11"); 
 fprintf(outfile, "L11a = %d\n", j );
 L[11] = 1500.0 * j;
 shownum(11); 

 get_parameter( infile, 's', word, "L12" );	/* Exemptions, college kids. */
 get_parameter( infile, 'i', &j, "L12"); 
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

 if ((status == SINGLE) || (status == MARRIED_FILLING_SEPARAT))		/* Min2File */
  { if (L[29] < 10000.0)
     fprintf(outfile," --- You do not need to file, (except to get refund).  Income < $10,000. ---\n");
  }
 else
  { if (L[29] < 20000.0) 
     fprintf(outfile," --- You do not need to file, (except to get refund).  Income < $20,000. ---\n");
  }

 L[30] = L[13];
 showline(30);

 fprintf(outfile,"\n");
 GetLine( "F1", &F[1] );	/* Worksheet F Medical Expenses (See pg 27). */
 showline_wrksht('F',1,F);
 F[2] = 0.02 * L[28];
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
 L[10] = F[6];
 if (L[31] != 0.0)
  showline_wmsg(31," Medical Expenses Worksheet F (See pg 27)");
 /* end of Worksheet F */

 GetLineF( "L32", &L[32] );	/* Alimony and maintenance payments PAYED. */

 GetLineF( "L33", &L[33] );	/* Qualified Conservation Contribution. */

 GetLineF( "L34", &L[34] );	/* Health Enterprise Zone Deduction. */

 GetLineF( "L35", &L[35] );	/* Alternative Business Calc Adj (Sched NJ-BUS-2, Line 11). */

 for (j=30; j <= 35; j++)
  L[36] = L[36] + L[j];
 showline_wmsg(36,"Total Exemptions and Deductions");

 /* Taxable income. */
 L[37] = L[29] - L[36];
 if (L[37] > 0.0)
  showline_wmsg(37, "(Taxable Income)");

 GetLineF( "L38a", &L[38] );	/* Property Tax Paid. */

 GetLine( "COJ1", &COJ[1] );	/* Income taxed by other jurisdictions, if any. */
 GetLine( "COJ9a",&COJ_9a );	/* Tax paid to other jurisdictions on that income, if any. */

 fprintf(outfile,"\n");  /* Property Tax Deduction Worksheet H (pg 31). */
 H[1] = L[38];
 showline_wrksht('H',1,H);
 if (status != MARRIED_FILLING_SEPARAT)
  H[2] = smallerof( H[1], 15000.0 );
 else
  H[2] = smallerof( H[1],  7500.0 );
 showline_wrksht('H',2,H);

 if (status != MARRIED_FILLING_SEPARAT)
  proptxcredit = 50.0;
 else
  proptxcredit = 25.0;

 if (COJ_9a == 0.0)
  { /*Worksheet-H*/
    H[3] = L[37];	 Hb[3] = L[37];
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
    if (H[7] >= proptxcredit)
     { /*yes*/
       fprintf(outfile," H8. Yes. (Take Property Tax Deduction.)\n");
       L[39] = H[4];
       L[40] = H[5];
       L[41] = H[6];
       L[55] = 0.0;
     } /*yes*/
    else
     { /*no*/
       fprintf(outfile," H8. No. (Take Property Tax Credit.)\n");
       L[39] = 0.0;
       L[40] = Hb[5];
       L[41] = Hb[6];
       L[55] = proptxcredit;
     } /*no*/
  } /*Worksheet-H*/
 else
  { /*Sched  COJ +Worksheet-I*/
    fprintf(outfile,"\nSchedule COJ Credit for Income or Wage Taxes Paid to Other Jurisdiction (Previously Sched A):\n");
    showline_wlabel("COJ_1", COJ[1]); 
    COJ[2] = L[29];
    showline_wlabel("COJ_2", COJ[2]);
    COJ[3] = smallerof( 1.0, (COJ[1] / COJ[2]) );
    fprintf(outfile," COJ_3 = %6.2f %%\n", 100.0 * COJ[3] );
    COJ[4] = L[37];
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

    if (Ib[4] >= proptxcredit)
     {
      fprintf(outfile," Sched-I, Yes:  Take PropTax Deduction\n\n");
      L[39] = COJ[5];	// fprintf(outfile,"L36c = %6.2f\n", L[36]);
      L[40] = COJ[6];
      L[41] = COJ[7];
      L[42] = I[2];
      L[55] = 0.0;
     }
    else
     {
      fprintf(outfile," Sched-I, No:  Take PropTax Credit\n\n");
      L[39] = 0.0;
      L[40] = COJ_b[6];
      L[41] = COJ_b[7];
      L[42] = Ib[2];
      L[55] = proptxcredit;
     }
  } /*SchedA+Worksheet-I*/


 /* If no property tax was paid, ensure prop.tax credit is set to 0 */
 if ( L[38] == 0.0 ) L[55] = 0.0;

 if (L[38] > 0.0)
  fprintf(outfile, "L38a = %6.2f\n", L[38]);

 showline(39);

 fprintf(outfile,"\n");  /* NJ Taxable Income.*/
 // L[40] = L[37] - L[39];  /* Handled above in Sched-1. */
 if (L[40] > 0.0)
  showline_wmsg( 40, "NJ Taxable Income" );

 // L[41] = TaxRateFunction( L[40], status );  /* Handled above in Schedules+Worksheets, A, G, H, I. */
 showline_wmsg(41, "TAX");
 Report_bracket_info( L[40], status );

 if (COJ[1] > 0.0)
  showline_wmsg(42, "( Credit for Taxes paid to other jurisdictions. )\n");

 L[43] = L[41] - L[42];
 showline_wmsg(43, "( Balance of Tax )");

 GetLineF( "L44", &L[44] );	/* Child and Dependent Care Credit (Worksheet J). */
 L[45] = L[43] - L[45];
 showline(45);
 GetLineF( "L46", &L[46] );	/* Sheltered Workshop Tax Credit. */
 L[47] = L[45] - L[46];
 showline(47);

 GetLineF( "L48", &L[48] );	/* Gold Start Family Counseling Credit. */
 L[49] = NotLessThanZero( L[47] - L[48] );
 showline_wmsg( 49, "Balance of Tax after Credits." );

 GetLineF( "L50", &L[50] );	/* Use Tax Due on Out-of-State Purchases (pg 37). */
 GetLineF( "L51", &L[51] );	/* Penalty for underpayment of estimated tax. */
 GetLineF( "L52", &L[52] );	/* Shared Responsibility (Med. Insurance) Payment. */

 L[53] = L[49] + L[50] + L[51] + L[52];
 showline_wmsg( 53, "Total Tax Due" );			/* Total Tax + Penalty. */

 GetLine( "L54", &L[54] );	/* Withheld amount. */
 showline_wmsg( 54, "Total NJ Income Tax Withheld" );

 showline_wmsg( 55, "Property tax Credit" );

 GetLineF( "L56", &L[56] );	/* NJ Estimated Tax Payments/Credit from last year's return. */

 GetLineF( "L57", &L[57] );	/* NJ Earned Income Tax Credit. (See Sched pg 38.) */

 GetLineF( "L58", &L[58] );	/* EXCESS NJ UI/HC/WD Withheld, (See pg 38.) */

 GetLineF( "L59", &L[59] );	/* EXCESS NJ Disability Insurance Withheld, (See pg 38.) */

 GetLineF( "L60", &L[60] );	/* EXCESS NJ Family Leave Insurance Withheld, (See pg 38.) */

 GetLineF( "L61", &L[61] );	/* Wounded Warrior Caregivers Credit */

 for (j=54; j <= 61; j++)
  L[62] = L[62] + L[j];
 showline_wmsg( 62, "Total Withholding Payments & Credits" );

 for (j=65; j <= 73; j++)
  L[74] = L[74] + L[j];
 
 if (L[62] < L[53])
  {
   L[63] = L[53] - L[62];
   fprintf(outfile, "L63 = %6.2f	DUE !!!\n", L[63] );
   fprintf(outfile,"         (Which is %2.1f%% of your total tax.)\n", 100.0 * L[63] / (L[43] + 1e-9) );
   showline_wmsg( 74, "( Total Adjustments to tax due )");
   L[75] = L[63] + L[74];
   showline_wmsg( 75, "Balance Due" );
  }
 else
  {
   L[64] = L[62] - L[53];
   fprintf(outfile, "L64 = %6.2f	Overpayment\n", L[64] );

   showline_wmsg( 74, "( Total Adjustments to overpayment )");
   L[76] = L[64] - L[74];
   showline_wmsg( 76, "Refund !!!" );
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
#undef MARRIED_FILLING_JOINTLY
#undef MARRIED_FILLING_SEPARAT
#undef HEAD_OF_HOUSEHOLD
#undef WIDOW
}
namespace taxsolve_NC_D400_2019 {
#define SINGLE                  1
#define MARRIED_FILLING_JOINTLY 2
#define MARRIED_FILLING_SEPARAT 3
#define HEAD_OF_HOUSEHOLD       4
#define WIDOW                   5
#define Yes 1
#define No  0
/************************************************************************/
/* TaxSolve_NC_D400_2019.c - North Carolina 2019 NC-DC400 State Taxes.	*/
/* Copyright (C) 2020 - S.Jenkins					*/
/* 									*/
/* Compile:   gcc taxsolve_NC_D400_2019.c -o taxsolve_NC_D400_2019	*/
/* Run:  ./taxsolve_NC_D400_2019		  			*/
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
/* 1-2-2020	http://opentaxsolver.sourceforge.com/			*/
/*	Earlier versions - Lincoln Baxter (lab@lincolnbaxter.com)	*/
/*									*/
/************************************************************************/



float thisversion=17.00;


double flat_tax_rate = 0.0525;		/* Updated for 2019. */


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
 double stdded, min_payment=0.0, min2file;
 double L20a=0.0, L20b=0.0, L21a=0.0, L21b=0.0, L21c=0.0, L21d=0.0;

 /*-----------------------------------------*/
 /* --- Decode any command line options. -- */
 /*-----------------------------------------*/
 printf("NC D400 2019 - v%3.2f\n", thisversion);
 jj = 1;  k=1;
 while (jj < argc)
 {
  if (strcmp(argv[jj],"-verbose")==0)  { verbose = 1; }
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


 /* get_parameter(infile, kind, x, emssg ) */
 get_parameter( infile, 's', word, "FedReturn" );	/* File name of Federal Return log file. */
 if ( verbose ) printf( "word: %s\n", word );
 get_word( infile, word ); 
 ImportFederalReturnData( word, &fed_data);

 get_parameter( infile, 's', word, "Status"); /* 1=single, 2=married/joint, 3=married/separate, 4=house-head, 5=widow */
 get_parameter( infile, 'l', word, "Status ?");
 if ((word[0]>'0') && (word[0]<'6')) status = word[0]-48; else
 if (strncasecmp(word,"Single",4)==0) status = SINGLE; else
 if (strncasecmp(word,"Married/Joint",13)==0) status = MARRIED_FILLING_JOINTLY; else
 if (strncasecmp(word,"Married/Sep",11)==0) status = MARRIED_FILLING_SEPARAT; else
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
 GetLine( "L11", &L[11] );	/* Enter 0 to use Std Deduction; Otherwise Itemized Ded. from Sched-S part C, Line 23. */
 GetLine( "L13", &L[13] );	/* Enter 1.0 for full-year residents. Otherwise amount from Sched-S part D, Line 26. */  
 GetLine( "L16", &L[16] );	/* Tax credits. (D-400TC part 3 line 20) */  
 GetLine( "L18", &L[18] );	/* Consumer Use Tax. (pgs 9+10) */
 GetLine( "L20a", &L20a );	/* North Carolina Income Tax Withheld (yours) */
 GetLine( "L20b", &L20b );	/* North Carolina Income Tax Withheld (spouses) */
 L[20] = L20a + L20b;
 GetLine( "L21a", &L21a );	/* Other tax payments. 2019 Estimated Tax. */
 GetLine( "L21b", &L21b );	/* Other tax payments. Paid with Extension. */
 GetLine( "L21c", &L21c );	/* Other tax payments. Partnership. */
 GetLine( "L21d", &L21d );	/* Other tax payments. S Corporation. */



 /*-------------------------------*/
 /* ---- Do Tax Calculations ---- */
 /*-------------------------------*/

 L[6] = fed_data.fedline[8];		/* Taxable income from Fed1040 Line 8, AGI. */

 switch (status)
  {
   case SINGLE: 		 stdded   = 10000.0; 	/* NC std single deduction. */
				 min2file = 10000.0;
	break;
   case MARRIED_FILLING_JOINTLY: stdded   = 20000.0; 	/* NC std Married/joint deduction. */
				 min2file = 20000.0;
	break;
   case WIDOW:			 stdded   = 20000.0; 	/* NC std widow(er) deduction. */
				 min2file = 20000.0;
	break;
   case MARRIED_FILLING_SEPARAT: stdded   = 10000.0; 	/* NC std Married/sep deduction. */
				 min2file = 10000.0;
	break;
   case HEAD_OF_HOUSEHOLD:	 stdded   = 15000.0; 	/* NC std Head of house deduction. */
				 min2file = 15000.0;
	break;
   default:  
	stdded = 0;  printf("Unknown status\n");  fprintf(outfile,"Unknown status\n");
	exit(1); 
  }

 if (L[6] <= min2file)
  fprintf(outfile, "You may not need to file NC tax return, due to your income.\n");

 L[8] = L[6] + L[7];

 L[10] = L[8] - L[9];

 if (L[11] < stdded)
  L[11] = stdded;

 L[12] = L[10] - L[11];

 L[14] = L[13] * L[12];		 /* NC Taxable Income. */

 L[15] = flat_tax_rate * L[14];	 /* NC Income Tax. */

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
   L[32] = L[29] + L[30] + L[31];
   L[33] = L[28] - L[32];	/* REFUND */
  }


 /*-------------------------*/
 /* ---- Print Results ---- */
 /*-------------------------*/

 showline(6);	/* Taxable fed income */
 showline(7);	/* Additions to fed income */
 showline(8);	
 showline(9);	/* Deductions */
 showline(10);
 showline(11);
 if (L[11] < stdded)
  fprintf(outfile," Check_UsedStdDed: X\n");
 else
  fprintf(outfile," Check_ItemizedDed: X\n");
 showline(12);
 if (L[13] < 1.0) showline(13);	 /* Part-yr */
 showline_wmsg(14, "North Carolina Taxable Income");
 showline_wmsg(15, "North Carolina Income Tax");
 showline(16);
 showline(17);
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
   showline(32);
   showline(33);
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
#undef MARRIED_FILLING_JOINTLY
#undef MARRIED_FILLING_SEPARAT
#undef HEAD_OF_HOUSEHOLD
#undef WIDOW
#undef Yes
#undef No
}
namespace taxsolve_CA_540_2019 {
#define SINGLE 		        1
#define MARRIED_FILLING_JOINTLY 2
#define MARRIED_FILLING_SEPARAT 3
#define HEAD_OF_HOUSEHOLD       4
#define WIDOW		        5
/************************************************************************/
/* TaxSolve_CA_540_2019.c - California state 540 tax form.		*/
/* Copyright (C) 2020 - Aston Roberts					*/
/* 									*/
/* Compile:   gcc taxsolve_CA_540_2019.c -o taxsolve_CA_540_2019	*/
/* Run:	      ./taxsolve_CA_540_2019  CA_540_2019.txt 			*/
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

float thisversion=17.02;




int status=0;	/* Value for filing status. */
double 	sched540part2[MAX_LINES], sched540part2_sub[MAX_LINES], sched540part2_add[MAX_LINES],
	sched540part2_5a=0.0, sched540part2_5b=0.0, sched540part2_5c=0.0, sched540part2_5d=0.0,
	sched540part2_8a=0.0, sched540part2_8b=0.0, sched540part2_8c=0.0,
	sched540part2_add8a=0.0, sched540part2_add8b=0.0, sched540part2_add8c=0.0;


double TaxRateFormula( double income, int status )
{											/* Updated for 2019. */
 double tax;
 if ((status==SINGLE) || (status==MARRIED_FILLING_SEPARAT))
  {
   if (income <   8809.00)  tax =             0.01 * income;                else
   if (income <  20883.00)  tax =    88.09 +  0.02 * (income -   8809.00);  else
   if (income <  32960.00)  tax =   329.57 +  0.04 * (income -  20833.00);  else
   if (income <  45753.00)  tax =   812.65 +  0.06 * (income -  32960.00);  else
   if (income <  57824.00)  tax =  1580.23 +  0.08 * (income -  45753.00);  else
   if (income < 295373.00)  tax =  2545.91 + 0.093 * (income -  57824.00);  else
   if (income < 354445.00)  tax = 24637.97 + 0.103 * (income - 295373.00);  else
   if (income < 590742.00)  tax = 30722.39 + 0.113 * (income - 354445.00);
   else                     tax = 57423.95 + 0.123 * (income - 590742.00);
  }
 else
 if ((status==MARRIED_FILLING_JOINTLY) || (status==WIDOW))
  {
   if (income <   17618.00)  tax =              0.01 * income;                 else
   if (income <   41766.00)  tax =    176.18 +  0.02 * (income -   17618.00);  else
   if (income <   65920.00)  tax =    659.14 +  0.04 * (income -   41766.00);  else
   if (income <   91506.00)  tax =   1625.30 +  0.06 * (income -   65920.00);  else
   if (income <  115648.00)  tax =   3160.46 +  0.08 * (income -   91506.00);  else
   if (income <  590746.00)  tax =   5091.82 + 0.093 * (income -  115648.00);  else
   if (income <  708890.00)  tax =  49275.93 + 0.103 * (income -  590746.00);  else
   if (income < 1181484.00)  tax =  61444.76 + 0.113 * (income -  708890.00);
   else                      tax = 114847.88 + 0.123 * (income - 1181484.00);
  }
 else
  {
   if (income <  17629.00)  tax =             0.01 * income;                else
   if (income <  41768.00)  tax =   176.29 +  0.02 * (income -  17629.00);  else
   if (income <  53848.00)  tax =   659.07 +  0.04 * (income -  41768.00);  else
   if (income <  66636.00)  tax =  1142.07 +  0.06 * (income -  53848.00);  else
   if (income <  78710.00)  tax =  1909.65 +  0.08 * (income -  66636.00);  else
   if (income < 401705.00)  tax =  2875.57 + 0.093 * (income -  78710.00);  else
   if (income < 482047.00)  tax = 32914.11 + 0.103 * (income - 401705.00);  else
   if (income < 803410.00)  tax = 41189.34 + 0.113 * (income - 482047.00); 
   else                     tax = 77503.36 + 0.123 * (income - 803410.00);
  }
 return (int)(tax+0.5);
}


void Report_bracket_info( double income, int status )
{
 double tx, rate;
 tx = TaxRateFormula( income, status );
 if ((status==SINGLE) || (status==MARRIED_FILLING_SEPARAT))
  {
   if (income <    8809.00)  rate = 0.01;  else
   if (income <   20883.00)  rate = 0.02;  else
   if (income <   32960.00)  rate = 0.04;  else
   if (income <   45753.00)  rate = 0.06;  else
   if (income <   57824.00)  rate = 0.08;  else
   if (income <  295373.00)  rate = 0.093;  else
   if (income <  354445.00)  rate = 0.103;  else
   if (income <  590742.00)  rate = 0.113;  else  rate = 0.123;
  }
 else
 if ((status==MARRIED_FILLING_JOINTLY) || (status==WIDOW))
  {
   if (income <   17618.00)  rate = 0.01;  else
   if (income <   41766.00)  rate = 0.02;  else
   if (income <   65920.00)  rate = 0.04;  else
   if (income <   91506.00)  rate = 0.06;  else
   if (income <  115648.00)  rate = 0.08;  else
   if (income <  590746.00)  rate = 0.093;  else
   if (income <  708890.00)  rate = 0.103;  else
   if (income < 1181484.00)  rate = 0.113;  else  rate = 0.123;
  }
 else
  {
   if (income <  17629.00)  rate = 0.01;  else
   if (income <  41768.00)  rate = 0.02;  else
   if (income <  53848.00)  rate = 0.04;  else
   if (income <  66636.00)  rate = 0.06;  else
   if (income <  78710.00)  rate = 0.08;  else
   if (income < 401705.00)  rate = 0.093;  else
   if (income < 482047.00)  rate = 0.103;  else
   if (income < 803410.00)  rate = 0.113;  else  rate = 0.123;
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
		TaxRateFunction( income, MARRIED_FILLING_JOINTLY ), 
		TaxRateFunction( income, HEAD_OF_HOUSEHOLD ) );
 exit(0);
}


/*----------------------------------------------------------------------------*/


struct FedReturnData
 {
  double fedline[MAX_LINES], schedA[MAX_LINES], fed_L2a, fed_L3a,
	fed_L4a, fed_L4b, fed_L4c, fed_L4d, fed_L5a, fed_L5b,
	schedA5a, schedA5b, schedA5c,
	schedA8a, schedA8b, schedA8c,
	sched1[MAX_LINES],
	fedl8b, fedl9b, fedl15a, fedl16a, fedl20a;
  int Exception, Itemized;
  char AlimRecipSSN[512], AlimRecipName[2048];
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
{
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


int ImportFederalReturnData( char *fedlogfile, struct FedReturnData *fed_data )
{
 FILE *infile;
 char fline[2000], word[2000], tword[2000];
 int linenum;

 for (linenum=0; linenum<MAX_LINES; linenum++) 
  { 
   fed_data->fedline[linenum] = 0.0;
   fed_data->schedA[linenum] = 0.0;
   fed_data->sched1[linenum] = 0.0;
  }
 fed_data->fed_L2a = 0;
 fed_data->fed_L3a = 0;
 fed_data->fed_L4a = 0;
 fed_data->fed_L4b = 0;
 fed_data->fed_L4c = 0;
 fed_data->fed_L4d = 0;
 fed_data->fed_L5a = 0;
 fed_data->fed_L5b = 0;
 fed_data->schedA5a = 0.0;
 fed_data->schedA5b = 0.0;
 fed_data->schedA5c = 0.0;
 fed_data->schedA8a = 0.0;
 fed_data->schedA8b = 0.0;
 fed_data->schedA8c = 0.0;

 fed_data->fedl8b = 0.0;
 fed_data->fedl9b = 0.0;
 fed_data->fedl15a = 0.0;
 fed_data->fedl16a = 0.0;
 fed_data->fedl20a = 0.0;
 strcpy( fed_data->AlimRecipSSN, "" );
 strcpy( fed_data->AlimRecipName, "" );
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
     if (strcmp(word,"L4c") == 0)
      grab_line_value( word, fline, &(fed_data->fed_L4c) );
     else
     if (strcmp(word,"L4d") == 0)
      grab_line_value( word, fline, &(fed_data->fed_L4d) );
     else
     if (strcmp(word,"L5a") == 0)
      grab_line_value( word, fline, &(fed_data->fed_L5a) );
     else
     if (strcmp(word,"L5b") == 0)
      grab_line_value( word, fline, &(fed_data->fed_L5b) );
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
   if (strncmp(word, "AlimRecipSSN", 12) == 0)
    grab_line_string( fline, fed_data->AlimRecipSSN );
   else
   if (strncmp(word, "AlimRecipName", 13) == 0)
    grab_line_string( fline, fed_data->AlimRecipName );
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
   else
   if (strcmp(word,"Status") == 0)
    {
     next_word(fline, word, " \t=");
     if (strncasecmp(word,"Single",4)==0) status = SINGLE; else
     if (strncasecmp(word,"Married/Joint",13)==0) status = MARRIED_FILLING_JOINTLY; else
     if (strncasecmp(word,"Married/Sep",11)==0) status = MARRIED_FILLING_SEPARAT; else
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
 int argk, j, k, iline7, iline8, iline9, iline10;
 double min2file=0.0, sched540A[MAX_LINES], sched540B[MAX_LINES], sched540C[MAX_LINES],
	sched540Ab[MAX_LINES], sched540Ac[MAX_LINES],
	sched540Bb[MAX_LINES], sched540Bc[MAX_LINES],
	sched540Cb[MAX_LINES], sched540Cc[MAX_LINES],
	threshA=0, std_ded=0;
 char word[4000], *infname=0, outfname[4000], prelim_1040_outfilename[5000];
 char 	*Your1stName="", *YourLastName="", YourName[2048]="", YourNames[2048]="", 
	*YourMidInitial="", *SpouseMidInitial="",
	*Spouse1stName="", *SpouseLastName="", *socsec;
 double  sched540A4d=0.0, sched540Ab4d=0.0, sched540Ac4d=0.0,
	 sched540Bb8a=0.0, sched540Bb8b=0.0, sched540Bc8c=0.0, sched540Bb8d=0.0,
	 sched540Bb8e=0.0, sched540Bb8f=0.0, sched540Bc8f=0.0, sched540Bb8g=0.0;
 time_t now;

 /* Decode any command-line arguments. */
 argk = 1;  k=1;
 while (argk < argc)
 {
  if (strcmp(argv[argk],"-verbose")==0)  { verbose = 1; }
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

 printf("CA-540 2019 - v%3.2f\n", thisversion);

 /* Accept Form's "Title" line, and put out with date-stamp for your records. */
 read_line( infile, word );
 now = time(0);
 fprintf(outfile,"\n%s,	 v%2.2f, %s\n", word, thisversion, ctime( &now ));

 get_parameter( infile, 's', word, "FileName" );      /* Preliminary Fed Return Output File-name. */
 get_word(infile, prelim_1040_outfilename );
 ImportFederalReturnData( prelim_1040_outfilename, &PrelimFedReturn );

 /* Filing Status. */
 switch (status)
 {
  case SINGLE: 			fprintf(outfile,"Status = Single (%d)\nCkSingle: X\nL7a = 1\n", status); break;
  case MARRIED_FILLING_JOINTLY: fprintf(outfile,"Status = Married/Joint (%d)\nCkMFJ: X\nL7a = 2\n", status); break;
  case MARRIED_FILLING_SEPARAT: fprintf(outfile,"Status = Married/Sep (%d)\nCkMFS: X\nL7a = 1\n", status); break;
  case HEAD_OF_HOUSEHOLD: 	fprintf(outfile,"Status = Head_of_Household (%d)\nCkHH: X\nL7a = 1\n", status); break;
  case WIDOW: 		  	fprintf(outfile,"Status = Widow(er) (%d)\nCkQW: X\nL7a = 1\n", status); break;
 }
 fprintf(outfile,"\nStep-2 fill-in box %d\n", status );

 /* Exemptions. */
 get_parameter( infile, 's', word, "L6" );	/* Are you a dependent? (yes/No). */
 get_parameter( infile, 'b', &j, "L6");
 L[6] = j;
 if (L[6] == 0)
  fprintf(outfile," L6 = no\n");
 else
  fprintf(outfile," L6 = yes, (check box on line 6).\n  CkDep: X\n");

 if ((status==SINGLE) || (status==MARRIED_FILLING_SEPARAT) || (status==HEAD_OF_HOUSEHOLD))
  iline7 = 1;  else  iline7 = 2;
 if (L[6] != 0.0) iline7 = 0; /* <-- Possible exceptions here. */
 L[7] = 122.0 * iline7;
 showline(7);

 get_parameter( infile, 's', word, "L8" );	/* Blind?, 1 if you or spouse, 2 if both. */
 get_parameter( infile, 'i', &iline8, "L8" );
 L[8] = iline8 * 122.0;
 showline(8);
 if (iline8 > 0) fprintf(outfile,"  L8a = %d\n", iline8 );

 get_parameter( infile, 's', word, "L9" );	/* Senior?, 1 if you or spouse, 2 if both. */
 get_parameter( infile, 'i', &iline9, "L9" );
 L[9] = iline9 * 122.0;
 showline(9);
 if (iline9 > 0) fprintf(outfile,"  L9a = %d\n", iline9 );

 get_parameter( infile, 's', word, "L10" );  /* Number of Dependents. */
 get_parameter( infile, 'i', &iline10, "L10"); 
 L[10] = iline10 * 378.0;
 showline(10);
 if (iline10 > 0) fprintf(outfile,"  L10a = %d\n", iline10 );

 L[11] = L[7] + L[8] + L[9] + L[10];
 showline_wmsg(11, "Exemption amount");

 /* Taxable Income. */
 GetLineF( "L12", &L[12] );		/* State Wages (W2 box 16). */

 L[13] = PrelimFedReturn.fedl8b;	/* Fed Wages (Fed 1040 line 8b). */
 showline(13);


 /* -- Sched540 Part I -- */

  GetLine("CA540_Subtr_A1", &(sched540Ab[1]) );
  GetLine("CA540_Addit_A1", &(sched540Ac[1]) );
  GetLine("CA540_Subtr_A2", &(sched540Ab[2]) );
  GetLine("CA540_Addit_A2", &(sched540Ac[2]) );
  GetLine("CA540_Subtr_A3", &(sched540Ab[3]) );
  GetLine("CA540_Addit_A3", &(sched540Ac[3]) );
  GetLine("CA540_Subtr_A4b", &(sched540Ab[4]) );
  GetLine("CA540_Addit_A4b", &(sched540Ac[4]) );
  GetLine("CA540_Subtr_A4d", &sched540Ab4d );
  GetLine("CA540_Addit_A4d", &sched540Ac4d );
  // GetLine("CA540_Subtr_A5", &(sched540Ab[5]) );	/* Soc Sec subtraction handled below. */
  GetLine("CA540_Subtr_A6", &(sched540Ab[6]) );
  GetLine("CA540_Addit_A6", &(sched540Ac[6]) );

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

  GetLine("CA540_Subtr_B8a", &sched540Bb8a );
  GetLine("CA540_Subtr_B8b", &sched540Bb8b );
  GetLine("CA540_Addit_B8c", &sched540Bc8c );
  GetLine("CA540_Subtr_B8d", &sched540Bb8d );
  GetLine("CA540_Subtr_B8e", &sched540Bb8e );
  GetLine("CA540_Subtr_B8f", &sched540Bb8f );
  GetLine("CA540_Addit_B8f", &sched540Bc8f );
  GetLine("CA540_Subtr_B8g", &sched540Bb8g );

  GetLine("CA540_Subtr_C10", &(sched540Cb[10]) );
  GetLine("CA540_Subtr_C11", &(sched540Cb[11]) );
  GetLine("CA540_Addit_C11", &(sched540Cc[11]) );
  GetLine("CA540_Subtr_C12", &(sched540Cb[12]) );
  GetLine("CA540_Addit_C13", &(sched540Cc[13]) );
  GetLine("CA540_Addit_C18", &(sched540Cc[18]) );
  GetLine("CA540_Addit_C20", &(sched540Cc[20]) );
  GetLine("CA540_Subtr_C21", &(sched540Cb[21]) );

  sched540A[1] = PrelimFedReturn.fedline[1];
  sched540B[9] = sched540B[9] + sched540A[1];
  if (sched540A[1] != 0.0)
   fprintf(outfile," SchedCA540_A1 = %6.2f\n", sched540A[1] );

  sched540Bb[9] = sched540Bb[9] + sched540Ab[1];
  if (sched540Ab[1] != 0.0)
   fprintf(outfile," SchedCA540_A1b = %6.2f\n", sched540Ab[1] );

  sched540Bc[9] = sched540Bc[9] + sched540Ac[1];
  if (sched540Ac[1] != 0.0)
   fprintf(outfile," SchedCA540_A1c = %6.2f\n", sched540Ac[1] );


  if (PrelimFedReturn.fed_L2a != 0.0)
   fprintf(outfile," SchedCA540_A2a = %6.2f\n", PrelimFedReturn.fed_L2a );

  sched540A[2] = PrelimFedReturn.fedline[2];
  sched540B[9] = sched540B[9] + sched540A[2];
  if (sched540A[2] != 0.0)
   fprintf(outfile," SchedCA540_A2 = %6.2f\n", sched540A[2] );

  sched540Bb[9] = sched540Bb[9] + sched540Ab[2];
  if (sched540Ab[2] != 0.0)
   fprintf(outfile," SchedCA540_A2b = %6.2f\n", sched540Ab[2] );

  sched540Bc[9] = sched540Bc[9] + sched540Ac[2];
  if (sched540Ac[2] != 0.0)
   fprintf(outfile," SchedCA540_A2c = %6.2f\n", sched540Ac[2] );


  if (PrelimFedReturn.fed_L3a != 0.0)
   fprintf(outfile," SchedCA540_A3a = %6.2f\n", PrelimFedReturn.fed_L3a );

  sched540A[3] = PrelimFedReturn.fedline[3];
  sched540B[9] = sched540B[9] + sched540A[3];
  if (sched540A[3] != 0.0)
   fprintf(outfile," SchedCA540_A3 = %6.2f\n", sched540A[3] );

  sched540Bb[9] = sched540Bb[9] + sched540Ab[3];
  if (sched540Ab[3] != 0.0)
   fprintf(outfile," SchedCA540_A3b = %6.2f\n", sched540Ab[3] );

  sched540Bc[9] = sched540Bc[9] + sched540Ac[3];
  if (sched540Ac[3] != 0.0)
   fprintf(outfile," SchedCA540_A3c = %6.2f\n", sched540Ac[3] );


  if (PrelimFedReturn.fed_L4a != 0.0)
   fprintf(outfile," SchedCA540_A4a = %6.2f\n", PrelimFedReturn.fed_L4a );

  sched540A[4] = PrelimFedReturn.fed_L4b;
  sched540B[9] = sched540B[9] + sched540A[4];
  if (sched540A[4] != 0.0)
   fprintf(outfile," SchedCA540_A4 = %6.2f\n", sched540A[4] );

  sched540Bb[9] = sched540Bb[9] + sched540Ab[4];
  if (sched540Ab[4] != 0.0)
   fprintf(outfile," SchedCA540_A4b = %6.2f\n", sched540Ab[4] );

  sched540Bc[9] = sched540Bc[9] + sched540Ac[4];
  if (sched540Ac[4] != 0.0)
   fprintf(outfile," SchedCA540_A4c = %6.2f\n", sched540Ac[4] );


  if (PrelimFedReturn.fed_L4c != 0.0)
   fprintf(outfile," SchedCA540_A4d_ = %6.2f\n", PrelimFedReturn.fed_L4c );

  sched540A4d = PrelimFedReturn.fed_L4d;
  sched540B[9] = sched540B[9] + sched540A4d;
  if (sched540A4d != 0.0)
   fprintf(outfile," SchedCA540_A4d = %6.2f\n", sched540A4d );

  sched540Bb[9] = sched540Bb[9] + sched540Ab4d;
  if (sched540Ab4d != 0.0)
   fprintf(outfile," SchedCA540_A4db = %6.2f\n", sched540Ab4d );

  sched540Bc[9] = sched540Bc[9] + sched540Ac4d;
  if (sched540Ac4d != 0.0)
   fprintf(outfile," SchedCA540_A4dc = %6.2f\n", sched540Ac4d );

  if (PrelimFedReturn.fed_L5a != 0.0)
   fprintf(outfile," SchedCA540_A5a = %6.2f\n", PrelimFedReturn.fed_L5a );

  sched540A[5] = PrelimFedReturn.fed_L5b;
  sched540B[9] = sched540B[9] + sched540A[5];
  if (sched540A[5] != 0.0)
   fprintf(outfile," SchedCA540_A5 = %6.2f\n", sched540A[5] );

  sched540Ab[5] = sched540A[5];			/* Subtract SocSec payments from AGI in CA. */
  sched540Bb[9] = sched540Bb[9] + sched540Ab[5];
  if (sched540Ab[5] != 0.0)
   fprintf(outfile," SchedCA540_A5b = %6.2f\n", sched540Ab[5] );

  sched540A[6] = PrelimFedReturn.fedline[6];
  sched540B[9] = sched540B[9] + sched540A[6];
  if (sched540A[6] != 0.0)
   fprintf(outfile," SchedCA540_A6 = %6.2f\n", sched540A[6] );

  sched540Bb[9] = sched540Bb[9] + sched540Ab[6];
  if (sched540Ab[6] != 0.0)
   fprintf(outfile," SchedCA540_A6b = %6.2f\n", sched540Ab[6] );

  sched540Bc[9] = sched540Bc[9] + sched540Ac[6];
  if (sched540Ac[6] != 0.0)
   fprintf(outfile," SchedCA540_A6c = %6.2f\n", sched540Ac[6] );


 for (j=1; j <= 7; j++)
  {
   sched540B[j] = PrelimFedReturn.sched1[j];
   sched540B[9] = sched540B[9] + sched540B[j];
   if (sched540B[j] != 0.0)
    fprintf(outfile," SchedCA540_B%d = %6.2f\n", j, sched540B[j] );

   sched540Bb[9] = sched540Bb[9] + sched540Bb[j];
   if (sched540Bb[j] != 0.0)
    fprintf(outfile," SchedCA540_B%db = %6.2f\n", j, sched540Bb[j] );

   sched540Bc[9] = sched540Bc[9] + sched540Bc[j];
   if (sched540Bc[j] != 0.0)
    fprintf(outfile," SchedCA540_B%dc = %6.2f\n", j, sched540Bc[j] );
  }

  sched540B[8] = PrelimFedReturn.sched1[8];
  sched540B[9] = sched540B[9] + sched540B[8];
  if (sched540B[8] != 0.0)
   fprintf(outfile," SchedCA540_B8 = %6.2f\n", sched540B[8] );


  sched540Bb[9] = sched540Bb[9] + sched540Bb8a;
  if (sched540Bb8a != 0.0)
   fprintf(outfile," SchedCA540_B8ba = %6.2f\n", sched540Bb8a );

  sched540Bb[9] = sched540Bb[9] + sched540Bb8b;
  if (sched540Bb8b != 0.0)
   fprintf(outfile," SchedCA540_B8bb = %6.2f\n", sched540Bb8b );

  sched540Bb[9] = sched540Bb[9] + sched540Bb8d;
  if (sched540Bb8d != 0.0)
   fprintf(outfile," SchedCA540_B8bd = %6.2f\n", sched540Bb8d );

  sched540Bb[9] = sched540Bb[9] + sched540Bb8e;
  if (sched540Bb8e != 0.0)
   fprintf(outfile," SchedCA540_B8be = %6.2f\n", sched540Bb8e );

  sched540Bb[9] = sched540Bb[9] + sched540Bb8f;
  if (sched540Bb8f != 0.0)
   fprintf(outfile," SchedCA540_B8bf = %6.2f\n", sched540Bb8f );

  sched540Bb[9] = sched540Bb[9] + sched540Bb8g;
  if (sched540Bb8g != 0.0)
   fprintf(outfile," SchedCA540_B8bg = %6.2f\n", sched540Bb8g );

  sched540Bc[9] = sched540Bc[9] + sched540Bc8c;
  if (sched540Bc8c != 0.0)
   fprintf(outfile," SchedCA540_B8cc = %6.2f\n", sched540Bc8c );

  sched540Bc[9] = sched540Bc[9] + sched540Bc8f;
  if (sched540Bc8f != 0.0)
   fprintf(outfile," SchedCA540_B8cf = %6.2f\n", sched540Bc8f );


  if (sched540B[9] != 0.0)
   fprintf(outfile," SchedCA540_B9 = %6.2f\n", sched540B[9] );

  if (sched540Bb[9] != 0.0)
   fprintf(outfile," SchedCA540_B9b = %6.2f\n", sched540Bb[9] );

  if (sched540Bc[9] != 0.0)
   fprintf(outfile," SchedCA540_B9c = %6.2f\n", sched540Bc[9] );


  for (j=10; j <= 21; j++)
   {
    sched540C[j] = PrelimFedReturn.sched1[j];
    sched540C[22] = sched540C[22] + sched540C[j];
    if (sched540C[j] != 0.0)
     fprintf(outfile," SchedCA540_C%d = %6.2f\n", j, sched540C[j] );

    sched540Cb[22] = sched540Cb[22] + sched540Cb[j];
    if (sched540Cb[j] != 0.0)
     fprintf(outfile," SchedCA540_C%db = %6.2f\n", j, sched540Cb[j] );

    sched540Cc[22] = sched540Cc[22] + sched540Cc[j];
    if (sched540Cc[j] != 0.0)
     fprintf(outfile," SchedCA540_C%dc = %6.2f\n", j, sched540Cc[j] );
   }

  if (PrelimFedReturn.AlimRecipSSN[0] != '\0')
   fprintf(outfile," AlimRecipSSN: %s\n", PrelimFedReturn.AlimRecipSSN );
  if (PrelimFedReturn.AlimRecipName[0] != '\0')
   fprintf(outfile," AlimRecipName: %s\n", PrelimFedReturn.AlimRecipName );

  if (sched540C[22] != 0.0)
   fprintf(outfile," SchedCA540_C22 = %6.2f\n", sched540C[22] );

  if (sched540Cb[22] != 0.0)
   fprintf(outfile," SchedCA540_C22b = %6.2f\n", sched540Cb[22] );

  if (sched540Cc[22] != 0.0)
   fprintf(outfile," SchedCA540_C22c = %6.2f\n", sched540Cc[22] );


  sched540C[23] = sched540B[9] - sched540C[22];
  if (sched540C[23] != 0.0)
   fprintf(outfile," SchedCA540_C23 = %6.2f\n", sched540C[23] );

  sched540Cb[23] = sched540Bb[9] - sched540Cb[22];
  if (sched540Cb[23] != 0.0)
   fprintf(outfile," SchedCA540_C23b = %6.2f\n", sched540Cb[23] );

  sched540Cc[23] = sched540Bc[9] - sched540Cc[22];
  if (sched540Cc[23] != 0.0)
   fprintf(outfile," SchedCA540_C23c = %6.2f\n", sched540Cc[23] );


 /* -- Sched540 Part II -- */

 GetLine("CA540_P2_1", &(sched540part2[1]) );	/* Medical and dental expenses */
 sched540part2[2] = PrelimFedReturn.fedline[7];
 sched540part2[3] = 0.075 * sched540part2[2];
 sched540part2[4] = NotLessThanZero( sched540part2[1] - sched540part2[3] );
 sched540part2_5a = PrelimFedReturn.schedA5a;
 sched540part2_5b = PrelimFedReturn.schedA5b;
 sched540part2_5c = PrelimFedReturn.schedA5c;
 sched540part2_5d = sched540part2_5a + sched540part2_5b + sched540part2_5c;
 if (status != MARRIED_FILLING_SEPARAT)
  sched540part2[5] = smallerof( sched540part2_5d, 10000.0 );	/* Will be Line 5e. */
 else
  sched540part2[5] = smallerof( sched540part2_5d, 5000.0 );
 GetLine("CA540_P2_Sub_5a", &(sched540part2_sub[5]) );
 sched540part2_add[5] = sched540part2_5d - sched540part2[5];
 sched540part2[6] = PrelimFedReturn.schedA[6];
 GetLine("CA540_P2_Sub_6", &(sched540part2_sub[6]) );
 sched540part2[7] = sched540part2[5] + sched540part2[6];
 sched540part2_sub[7] = sched540part2_sub[5] + sched540part2_sub[6];
 sched540part2_add[7] = sched540part2_add[5];
 sched540part2_8a = PrelimFedReturn.schedA8a;
 GetLine("CA540_P2_Add_8a", &sched540part2_add8a );
 sched540part2_8b = PrelimFedReturn.schedA8b;
 GetLine("CA540_P2_Add_8b", &sched540part2_add8b );
 sched540part2_8c = PrelimFedReturn.schedA8c;
 GetLine("CA540_P2_Add_8c", &sched540part2_add8c );
 sched540part2[8] = sched540part2_8a + sched540part2_8b + sched540part2_8c;
 sched540part2_add[8] = sched540part2_add8a + sched540part2_add8b + sched540part2_add8c;
 sched540part2[9] = PrelimFedReturn.schedA[9];
 GetLine("CA540_P2_Sub_9", &(sched540part2_sub[9]) );
 GetLine("CA540_P2_Add_9", &(sched540part2_add[9]) );
 sched540part2[10] = sched540part2[8] + sched540part2[9];
 sched540part2_sub[10] = sched540part2_sub[9];
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
 sched540part2[16] = PrelimFedReturn.schedA[13];
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
 sched540part2[23] = PrelimFedReturn.fedline[7];
 sched540part2[24] = NotLessThanZero( 0.02 * sched540part2[23] );
 sched540part2[25] = NotLessThanZero( sched540part2[22] - sched540part2[24] );
 sched540part2[26] = sched540part2[18] + sched540part2[25];
 // GetLine( "Adj", &sched540part2[27] ); 	/* Now read above. */
 sched540part2[28] = sched540part2[26] + sched540part2[27];
 switch (status)
  {
   case SINGLE:
   case MARRIED_FILLING_SEPARAT:  threshA = 200534.0;	std_ded = 4537.0;  break;	/* Updated for 2019. */
   case MARRIED_FILLING_JOINTLY:
   case WIDOW:                    threshA = 401072.0;	std_ded = 9074.0;  break;
   case HEAD_OF_HOUSEHOLD:        threshA = 300805.0;	std_ded = 9074.0;  break;
  }
 if (L[13] > threshA)
  { /*Itemized Deductions Worksheet*/
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
 if (sched540part2[8] != 0.0)
  fprintf(outfile," SchedCA540_Part2_8ea = %6.2f\n", sched540part2[8] );
 if (sched540part2_add[8] != 0.0)
  fprintf(outfile," SchedCA540_Part2_8ec = %6.2f\n", sched540part2_add[8] );
 
 for (j=9; j <= 17; j++)
   display_part2( j );
 display_part2column( 18, 0 );

 L[18] = sched540part2[30];

 for (j=19; j <= 30; j++)
   display_part2column( j, 0 );

 /* -- End Sched540 Part II -- */


 L[14] = sched540Cb[23];	/* CA Adjustments, Schedule CA 540 line 23 column B. */
 showline(14);

 L[15] = L[13] - L[14];
 if (L[15] < 0.0) fprintf(outfile,"L15 = (%f6.2)\n", -L[15] );
 else showline(15);

 L[16] = sched540Cc[23];	/* CA Adjustments, Schedule CA 540 line 37 column C. */
 showline(16);

 L[17] = L[15] + L[16];		/* CA Adjusted Gross Income (AGI). */
 showline(17);

 switch (status)
  {		/* Minimum AGI (Line 17) required to file. */		/* Updated for 2019. */
    case SINGLE:
    case HEAD_OF_HOUSEHOLD:
		if (iline9 == 0)		/*Under65*/
		   switch (iline10)		  /*Dependents*/
		    {
		     case 0:  min2file = 18241.0;	break;
		     case 1:  min2file = 30841.0;	break;
		     default: min2file = 40291.0;	break;
		    }
		else			 	/*Over65*/
		   switch (iline10)		  /*Dependents*/
		    {
		     case 0:  min2file = 24341.0;	break;
		     case 1:  min2file = 33791.0;	break;
		     default: min2file = 41351.0;	break;
		    }
	  break;
   case MARRIED_FILLING_JOINTLY: 
		if (iline9 == 0)		 /*Both Under65*/
		   switch (iline10)
		    {
		     case 0:  min2file = 36485.0;	break;
		     case 1:  min2file = 49085.0;	break;
		     default: min2file = 58535.0;	break;
		    }
		else
		if (iline9 == 1)		 /*One Over65*/
		   switch (iline10)
		    {
		     case 0:  min2file = 42585.0;	break;
		     case 1:  min2file = 52035.0;	break;
		     default: min2file = 59595.0;	break;
		    }
		else
		   switch (iline10)		 /*Both Over65*/
		    {
		     case 0:  min2file = 48685.0;	break;
		     case 1:  min2file = 58135.0;	break;
		     default: min2file = 65695.0;	break;
		    }
	  break;
   case WIDOW:
		if (iline9 == 0)		/*Under65*/
		   switch (iline10)		  /*Dependents*/
		    {
		     case 0:  min2file = 0.0;		break;	/* N/A */
		     case 1:  min2file = 30841.0;	break;
		     default: min2file = 40291.0;	break;
		    }
		else			 	/*Over65*/
		   switch (iline10)		  /*Dependents*/
		    {
		     case 0:  min2file = 0.0;		break;	/* N/A */
		     case 1:  min2file = 33791.0;	break;
		     default: min2file = 41351.0;	break;
		    }
	  break;
  }
 if (L[17] <= min2file)
  fprintf(outfile,"You may not need to file CA Taxes, due to your California Adjusted Gross Income (%6.2f <= %6.2f).\n", 
	L[17], min2file );

 showline(18);

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
    if (status != MARRIED_FILLING_SEPARAT)
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
 showline_wmsg(64,"Total Tax");	/* Total tax. */

 /* Payments. */
 GetLineF( "L71", &L[71] ); 	/* CA income tax withheld. */
 
 GetLineF( "L72", &L[72] ); 	/* Estimated tax paid. */
 
 GetLineF( "L73", &L[73] ); 	/* Realestate withholding. */
 
 GetLineF( "L74", &L[74] ); 	/* Excess SDI. */

 GetLineF( "L75", &L[75] ); 	/* Earned Income Tax Credit (EITC). */

 GetLineF( "L76", &L[76] ); 	/* Young Child Tax Credit (YCTC). */
 
 L[77] = L[71] + L[72] + L[73] + L[74] + L[75] + L[76];
 showline_wmsg(77,"Total Payments");

 GetLineF( "L91", &L[91] );	/* Use Tax. */

 if (L[77] > L[91])
  {
   L[92] = L[77] - L[91];
   showline(92);
  }
 else
 if (L[91] > L[77])
  {
   L[93] = L[91] - L[77];
   showline(93);
  }
 
 GetLine( "L112", &L[112] );	/* Interest, late penalties. */
 GetLine( "L113", &L[113] );	/* Underpayment of estimated tax penalty. (FTB 5805) */

 /* Refund / Tax-Due. */
 if (L[92] > L[64])
  {
   L[94] = L[92] - L[64];
   fprintf(outfile,"L94 = %6.2f  REBATE!!!\n", L[94] );
   showline(95);
   L[96] = L[94]  - L[95];
   showline(96);
   showline(112);
   showline(113);
   L[115] = L[96] - (L[110] + L[112] + L[113]);
   showline(115);
  }
 else
  {
   L[97] = L[64] - L[92];
   fprintf(outfile,"L97 = %6.2f  DUE !!!\n", L[97] );
   fprintf(outfile,"         (Which is %2.1f%% of your total tax.)\n", 100.0 * L[97] / (L[64] + 1e-9) );
   L[111] = L[93] + L[97] + L[110];
   showline(111);
   showline(112);
   showline(113);
   L[114] = L[111] + L[112] + L[113];
   showline(114);
  }
 
 fprintf(outfile,"\nSelect any charity contributions and complete\n form accordingly.\n");

 fprintf(outfile,"\n{ --------- }\n");
 writeout_line = 0;
 Your1stName = GetTextLineF( "Your1stName:" );
 YourMidInitial = pull_initial( Your1stName );
 Your1stName[11] = '\0';
 fprintf(outfile,"Your1stName: %s\n", Your1stName );
 fprintf(outfile,"YourMidInit: %s\n", YourMidInitial );
 YourLastName   = GetTextLineF( "YourLastName:" );
 YourLastName[15] = '\0';
 fprintf(outfile,"YourLastName: %s\n", YourLastName );
 socsec = GetTextLineF( "YourSocSec#:" );
 format_socsec( socsec, 1 );
 fprintf(outfile,"YourSocSec#: %s\n", socsec );
 free( socsec );

 Spouse1stName = GetTextLineF( "Spouse1stName:" );
 SpouseMidInitial = pull_initial( Spouse1stName );
 Spouse1stName[11] = '\0';
 fprintf(outfile,"Spouse1stName: %s\n", Spouse1stName );
 fprintf(outfile,"SpouseMidInit: %s\n", SpouseMidInitial );
 SpouseLastName = GetTextLineF( "SpouseLastName:" );
 SpouseLastName[15] = '\0';
 fprintf(outfile,"SpouseLastName: %s\n", SpouseLastName );
 socsec = GetTextLineF( "SpouseSocSec#:" );
 format_socsec( socsec, 1 );
 fprintf(outfile,"SpouseSocSec#: %s\n", socsec );
 free( socsec );
 writeout_line = 1;

 if (strlen( YourLastName ) > 0)
  {
   strcpy( YourName, Your1stName );
   strcat( YourName, " " );
   strcat( YourName, YourLastName );
   YourName[15] = '\0';		/* Limit name to 15 characters. */
   fprintf(outfile,"YourName: %s\n", YourName );

   if (strcmp( YourLastName, SpouseLastName ) == 0)
    sprintf(YourNames,"%s & %s, %s", Your1stName, Spouse1stName, YourLastName );
   else
   if (strlen( SpouseLastName ) > 0)
    sprintf(YourNames,"%s %s & %s %s", Your1stName, YourLastName, Spouse1stName, SpouseLastName );
   else
    sprintf(YourNames,"%s %s", Your1stName, YourLastName );
   YourNames[33] = '\0';
   fprintf(outfile,"YourNames: %s\n", YourNames );
  }
 GetTextLineF( "Number&Street:" );
 GetTextLineF( "Apt#:" );
 GetTextLineF( "Town:" );
 fprintf(outfile,"State: CA\n");
 GetTextLineF( "Zipcode:" );
 GetTextLineF( "YourDOB:" );
 GetTextLineF( "SpouseDOB:" );
 fclose(infile);
 grab_any_pdf_markups( infname, outfile );
 fclose(outfile);
 Display_File( outfname );
 return 0;
}

#undef SINGLE
#undef MARRIED_FILLING_JOINTLY
#undef MARRIED_FILLING_SEPARAT
#undef HEAD_OF_HOUSEHOLD
#undef WIDOW
}
#undef MAX_LINES
}
#undef system(...)
#undef printf(...)
#ifdef _MSC_VER
#undef strcasecmp
#undef strncasecmp
#endif

