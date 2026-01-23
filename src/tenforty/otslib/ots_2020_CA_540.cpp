#include "ots_2020_routines.h"
namespace OpenTaxSolver2020 {
namespace taxsolve_CA_540_2020 {

#define SINGLE 		        1
#define MARRIED_FILLING_JOINTLY 2
#define MARRIED_FILLING_SEPARAT 3
#define HEAD_OF_HOUSEHOLD       4
#define WIDOW		        5
/************************************************************************/
/* TaxSolve_CA_540_2020.c - California state 540 tax form.		*/
/* Copyright (C) 2020 - Aston Roberts					*/
/* 									*/
/* Compile:   gcc taxsolve_CA_540_2020.c -o taxsolve_CA_540_2020	*/
/* Run:	      ./taxsolve_CA_540_2020  CA_540_2020.txt 			*/
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

float thisversion=18.03;




int status=0;	/* Value for filing status. */
double 	sched540part2[MAX_LINES], sched540part2_sub[MAX_LINES], sched540part2_add[MAX_LINES],
	sched540part2_5a=0.0, sched540part2_5b=0.0, sched540part2_5c=0.0, sched540part2_5d=0.0,
	sched540part2_8a=0.0, sched540part2_8b=0.0, sched540part2_8c=0.0, sched540part2_8d=0.0,
	sched540part2_add8a=0.0, sched540part2_add8b=0.0, sched540part2_add8c=0.0, sched540part2_sub8d=0.0;


double TaxRateFormula( double income, int status )
{											/* Updated for 2020. */
 double tax;
 if ((status==SINGLE) || (status==MARRIED_FILLING_SEPARAT))
  {
   if (income <   8932.00)  tax =             0.01 * income;                else
   if (income <  21175.00)  tax =    89.32 +  0.02 * (income -   8832.00);  else
   if (income <  33421.00)  tax =   334.18 +  0.04 * (income -  21175.00);  else
   if (income <  46394.00)  tax =   824.02 +  0.06 * (income -  33421.00);  else
   if (income <  58634.00)  tax =  1602.40 +  0.08 * (income -  46394.00);  else
   if (income < 299508.00)  tax =  2581.60 + 0.093 * (income -  58634.00);  else
   if (income < 359407.00)  tax = 24982.88 + 0.103 * (income - 299508.00);  else
   if (income < 599012.00)  tax = 31152.48 + 0.113 * (income - 359407.00);
   else                     tax = 58227.85 + 0.123 * (income - 599012.00);
  }
 else
 if ((status==MARRIED_FILLING_JOINTLY) || (status==WIDOW))
  {
   if (income <   17864.00)  tax =              0.01 * income;                 else
   if (income <   42350.00)  tax =    178.64 +  0.02 * (income -   17864.00);  else
   if (income <   66842.00)  tax =    668.36 +  0.04 * (income -   42350.00);  else
   if (income <   92788.00)  tax =   1648.04 +  0.06 * (income -   66842.00);  else
   if (income <  117268.00)  tax =   3204.80 +  0.08 * (income -   92788.00);  else
   if (income <  599016.00)  tax =   5163.20 + 0.093 * (income -  117268.00);  else
   if (income <  718814.00)  tax =  49965.76 + 0.103 * (income -  599016.00);  else
   if (income < 1198024.00)  tax =  62304.95 + 0.113 * (income -  718814.00);
   else                      tax = 116455.68 + 0.123 * (income - 1198024.00);
  }
 else
  {
   if (income <  17876.00)  tax =             0.01 * income;                else
   if (income <  42353.00)  tax =   178.76 +  0.02 * (income -  17876.00);  else
   if (income <  54597.00)  tax =   668.30 +  0.04 * (income -  42353.00);  else
   if (income <  67569.00)  tax =  1158.06 +  0.06 * (income -  54597.00);  else
   if (income <  79812.00)  tax =  1936.38 +  0.08 * (income -  67569.00);  else
   if (income < 407329.00)  tax =  2915.82 + 0.093 * (income -  79812.00);  else
   if (income < 488796.00)  tax = 33374.90 + 0.103 * (income - 407329.00);  else
   if (income < 814658.00)  tax = 41766.00 + 0.113 * (income - 488796.00); 
   else                     tax = 78588.41 + 0.123 * (income - 814658.00);
  }
 return (int)(tax+0.5);
}


void Report_bracket_info( double income, int status )
{
 double tx, rate;
 tx = TaxRateFormula( income, status );
 if ((status==SINGLE) || (status==MARRIED_FILLING_SEPARAT))
  {
   if (income <    8832.00)  rate = 0.01;  else
   if (income <   21175.00)  rate = 0.02;  else
   if (income <   33421.00)  rate = 0.04;  else
   if (income <   46394.00)  rate = 0.06;  else
   if (income <   58634.00)  rate = 0.08;  else
   if (income <  299508.00)  rate = 0.093;  else
   if (income <  359407.00)  rate = 0.103;  else
   if (income <  599012.00)  rate = 0.113;  else  rate = 0.123;
  }
 else
 if ((status==MARRIED_FILLING_JOINTLY) || (status==WIDOW))
  {
   if (income <   17864.00)  rate = 0.01;  else
   if (income <   42350.00)  rate = 0.02;  else
   if (income <   66842.00)  rate = 0.04;  else
   if (income <   92788.00)  rate = 0.06;  else
   if (income <  117268.00)  rate = 0.08;  else
   if (income <  599016.00)  rate = 0.093;  else
   if (income <  718814.00)  rate = 0.103;  else
   if (income < 1198024.00)  rate = 0.113;  else  rate = 0.123;
  }
 else
  {
   if (income <  17876.00)  rate = 0.01;  else
   if (income <  42353.00)  rate = 0.02;  else
   if (income <  54597.00)  rate = 0.04;  else
   if (income <  67569.00)  rate = 0.06;  else
   if (income <  79812.00)  rate = 0.08;  else
   if (income < 407329.00)  rate = 0.093;  else
   if (income < 488796.00)  rate = 0.103;  else
   if (income < 804658.00)  rate = 0.113;  else  rate = 0.123;
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
	fed_L4a, fed_L4b, fed_L5a, fed_L5b, fed_L6a, fed_L6b,
	schedA5a, schedA5b, schedA5c, schedA5,
	schedA8a, schedA8b, schedA8c, schedA8d,
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
 double  sched540Bb8a=0.0, sched540Bb8b=0.0, sched540Bc8c=0.0, sched540Bb8d=0.0,
	 sched540Bb8e=0.0, sched540Bb8f=0.0, sched540Bc8f=0.0, sched540Bb8g=0.0;
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

 printf("CA-540 2020 - v%3.2f\n", thisversion);

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
 L[7] = 124.0 * iline7;							/* Updated for 2020. */
 showline(7);

 get_parameter( infile, 's', word, "L8" );	/* Blind?, 1 if you or spouse, 2 if both. */
 get_parameter( infile, 'i', &iline8, "L8" );
 L[8] = iline8 * 124.0;							/* Updated for 2020. */
 showline(8);
 if (iline8 > 0) fprintf(outfile,"  L8a = %d\n", iline8 );

 get_parameter( infile, 's', word, "L9" );	/* Senior?, 1 if you or spouse, 2 if both. */
 get_parameter( infile, 'i', &iline9, "L9" );
 L[9] = iline9 * 124.0;							/* Updated for 2020. */
 showline(9);
 if (iline9 > 0) fprintf(outfile,"  L9a = %d\n", iline9 );

 get_parameter( infile, 's', word, "L10" );  /* Number of Dependents. */
 get_parameter( infile, 'i', &iline10, "L10"); 
 L[10] = iline10 * 383.0;						/* Updated for 2020. */
 showline(10);
 if (iline10 > 0) fprintf(outfile,"  L10a = %d\n", iline10 );

 L[11] = L[7] + L[8] + L[9] + L[10];
 showline_wmsg(11, "Exemption amount");

 /* Taxable Income. */
 GetLineF( "L12", &L[12] );		/* State Wages (W2 box 16). */

 L[13] = PrelimFedReturn.fedline[11];	/* Fed Wages (Fed 1040 line 11). */
 showline(13);


 /* -- Sched540 Part I -- */

  GetLine("CA540_Subtr_A1", &(sched540Ab[1]) );
  GetLine("CA540_Addit_A1", &(sched540Ac[1]) );
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
  GetLine("CA540_Subtr_C14", &(sched540Cb[14]) );
  GetLine("CA540_Subtr_C16", &(sched540Cb[16]) );
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


  if (PrelimFedReturn.fed_L5a != 0.0)
   fprintf(outfile," SchedCA540_A5a = %6.2f\n", PrelimFedReturn.fed_L5a );

  sched540A[5] = PrelimFedReturn.fed_L5b;
  sched540B[9] = sched540B[9] + sched540A[5];
  if (sched540A[5] != 0.0)
   fprintf(outfile," SchedCA540_A5 = %6.2f\n", sched540A[5] );

  sched540Bb[9] = sched540Bb[9] + sched540Ab[5];
  if (sched540Ab[5] != 0.0)
   fprintf(outfile," SchedCA540_A5b = %6.2f\n", sched540Ab[5] );

  sched540Bc[9] = sched540Bc[9] + sched540Ac[5];
  if (sched540Ac[5] != 0.0)
   fprintf(outfile," SchedCA540_A5c = %6.2f\n", sched540Ac[5] );

  if (PrelimFedReturn.fed_L6a != 0.0)
   fprintf(outfile," SchedCA540_A6a = %6.2f\n", PrelimFedReturn.fed_L6a );

  sched540A[6] = PrelimFedReturn.fed_L6b;
  sched540B[9] = sched540B[9] + sched540A[6];
  if (sched540A[6] != 0.0)
   fprintf(outfile," SchedCA540_A6 = %6.2f\n", sched540A[6] );

  sched540Ab[6] = sched540A[6];			/* Subtract SocSec payments from AGI in CA. */
  sched540Bb[9] = sched540Bb[9] + sched540Ab[6];
  if (sched540Ab[6] != 0.0)
   fprintf(outfile," SchedCA540_A6b = %6.2f\n", sched540Ab[6] );

  sched540A[7] = PrelimFedReturn.fedline[7];
  sched540B[9] = sched540B[9] + sched540A[7];
  if (sched540A[7] != 0.0)
   fprintf(outfile," SchedCA540_A7 = %6.2f\n", sched540A[7] );

  sched540Bb[9] = sched540Bb[9] + sched540Ab[7];
  if (sched540Ab[7] != 0.0)
   fprintf(outfile," SchedCA540_A7b = %6.2f\n", sched540Ab[7] );

  sched540Bc[9] = sched540Bc[9] + sched540Ac[7];
  if (sched540Ac[7] != 0.0)
   fprintf(outfile," SchedCA540_A7c = %6.2f\n", sched540Ac[7] );

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

 // GetLine("CA540_P2_1", &(sched540part2[1]) );	/* Medical and dental expenses */
 sched540part2[1] = PrelimFedReturn.schedA[1];
 sched540part2[2] = PrelimFedReturn.fedline[11];
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
 sched540part2_sub[5] = sched540part2_5a;
 sched540part2_add[5] = sched540part2_5d - sched540part2[5];

 sched540part2[6] = PrelimFedReturn.schedA[6];
 GetLine("CA540_P2_Sub_6", &(sched540part2_sub[6]) );
 GetLine("CA540_P2_Add_6", &(sched540part2_add[6]) );

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
   case MARRIED_FILLING_SEPARAT:  threshA = 203341.0;	std_ded = 4601.0;  break;	/* Updated for 2020. */
   case MARRIED_FILLING_JOINTLY:
   case WIDOW:                    threshA = 406687.0;	std_ded = 9202.0;  break;
   case HEAD_OF_HOUSEHOLD:        threshA = 305016.0;	std_ded = 9202.0;  break;
  }
 if (L[13] > threshA)
  { /*Itemized Deductions Worksheet*/	/* Page 47. */
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
  {		/* Minimum AGI (Line 17) required to file. */		/* Updated for 2020. */
    case SINGLE:
    case HEAD_OF_HOUSEHOLD:
		if (iline9 == 0)		/*Under65*/
		   switch (iline10)		  /*Dependents*/
		    {
		     case 0:  min2file = 18496.0;	break;
		     case 1:  min2file = 31263.0;	break;
		     default: min2file = 40838.0;	break;
		    }
		else			 	/*Over65*/
		   switch (iline10)		  /*Dependents*/
		    {
		     case 0:  min2file = 24696.0;	break;
		     case 1:  min2file = 34271.0;	break;
		     default: min2file = 41931.0;	break;
		    }
	  break;
   case MARRIED_FILLING_JOINTLY: 
		if (iline9 == 0)		 /*Both Under65*/
		   switch (iline10)
		    {
		     case 0:  min2file = 36996.0;	break;
		     case 1:  min2file = 49763.0;	break;
		     default: min2file = 59338.0;	break;
		    }
		else
		if (iline9 == 1)		 /*One Over65*/
		   switch (iline10)
		    {
		     case 0:  min2file = 43196.0;	break;
		     case 1:  min2file = 52771.0;	break;
		     default: min2file = 60631.0;	break;
		    }
		else
		   switch (iline10)		 /*Both Over65*/
		    {
		     case 0:  min2file = 49396.0;	break;
		     case 1:  min2file = 58971.0;	break;
		     default: min2file = 66631.0;	break;
		    }
	  break;
   case WIDOW:
		if (iline9 == 0)		/*Under65*/
		   switch (iline10)		  /*Dependents*/
		    {
		     case 0:  min2file = 0.0;		break;	/* N/A */
		     case 1:  min2file = 31263.0;	break;
		     default: min2file = 40838.0;	break;
		    }
		else			 	/*Over65*/
		   switch (iline10)		  /*Dependents*/
		    {
		     case 0:  min2file = 0.0;		break;	/* N/A */
		     case 1:  min2file = 34271.0;	break;
		     default: min2file = 41931.0;	break;
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

 GetLineF( "L64", &L[64] );	/* Excess Advance Premium Assistance Subsidy (APAS) repayment. */

 L[65] = L[48] + L[61] + L[62] + L[63] + L[64];
 showline_wmsg( 65, "Total Tax" );	/* Total tax. */

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
 GetLineF( "L92", &L[92] );	/* Individual Shared Responsibility (ISR) Penalty. */

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

 
 GetLine( "L112", &L[112] );	/* Interest, late penalties. */
 GetLine( "L113", &L[113] );	/* Underpayment of estimated tax penalty. (FTB 5805) */

 /* Refund / Tax-Due. */
 if (L[95] > L[65])
  {
   L[97] = L[95] - L[65];
   fprintf(outfile,"L97 = %6.2f  REFUND!!!\n", L[97] );
   showline(97);
   L[99] = L[97]  - L[98];
   showline(99);
   showline(112);
   showline(113);
   L[115] = L[99] - (L[110] + L[112] + L[113]);
   showline(115);
  }
 else
  {
   L[100] = L[65] - L[95];
   fprintf(outfile,"L100 = %6.2f  DUE !!!\n", L[100] );
   fprintf(outfile,"         (Which is %2.1f%% of your total tax.)\n", 100.0 * L[100] / (L[65] + 1e-9) );
   L[111] = L[96] + L[100] + L[110];
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

} // namespace taxsolve_CA_540_2020
} // namespace OpenTaxSolver2020

