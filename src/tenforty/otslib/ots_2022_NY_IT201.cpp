#include "ots_2022_routines.h"
namespace OpenTaxSolver2022 {
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

} // namespace taxsolve_NY_IT201_2022
} // namespace OpenTaxSolver2022
