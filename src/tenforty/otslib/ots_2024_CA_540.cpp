#include "ots_2024_routines.h"
namespace OpenTaxSolver2024 {
namespace taxsolve_CA_540_2024 {

#define SINGLE 		        1
#define MARRIED_FILING_JOINTLY  2
#define MARRIED_FILING_SEPARAT  3
#define HEAD_OF_HOUSEHOLD       4
#define WIDOW		        5
/************************************************************************/
/* TaxSolve_CA_540_2024.c - California state 540 tax form.		*/
/* Copyright (C) 2025 - Aston Roberts					*/
/* 									*/
/* Compile:   gcc taxsolve_CA_540_2024.c -lm -o taxsolve_CA_540_2024	*/
/* Run:	      ./taxsolve_CA_540_2024  CA_540_2024.txt 			*/
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
/* Aston Roberts 1-2-2025	aston_roberts@yahoo.com			*/
/************************************************************************/

float thisversion=22.03;




int status=0;	/* Value for filing status. */
double 	sched540part2[MAX_LINES], sched540part2_sub[MAX_LINES], sched540part2_add[MAX_LINES],
	sched540part2_5a=0.0, sched540part2_5b=0.0, sched540part2_5c=0.0, sched540part2_5d=0.0,
	sched540part2_8a=0.0, sched540part2_8b=0.0, sched540part2_8c=0.0, 
	sched540part2_add8a=0.0, sched540part2_add8b=0.0, sched540part2_add8c=0.0, sched540part2_sub8d=0.0;
 char 	*Your1stName="", *YourLastName="", *your_socsec="", 
	*Spouse1stName="", *SpouseLastName="", *spouse_socsec="",
	*street_address="", *apartment="", *town="", *zipcode="";


double TaxRateFormula( double income, int status )
{									/* Updated for 2024. */
 double tax;
 if ((status==SINGLE) || (status==MARRIED_FILING_SEPARAT))
  {
   if (income <  10756.00)  tax =             0.01 * income;                else
   if (income <  25499.00)  tax =   107.56 +  0.02 * (income -  10756.00);  else
   if (income <  40245.00)  tax =   402.42 +  0.04 * (income -  25499.00);  else
   if (income <  55866.00)  tax =   992.26 +  0.06 * (income -  40245.00);  else
   if (income <  70606.00)  tax =  1929.52 +  0.08 * (income -  55866.00);  else
   if (income < 360659.00)  tax =  3108.72 + 0.093 * (income -  70606.00);  else
   if (income < 432787.00)  tax = 30083.65 + 0.103 * (income - 360659.00);  else
   if (income < 721314.00)  tax = 37512.83 + 0.113 * (income - 432787.00);
   else                     tax = 70116.38 + 0.123 * (income - 721314.00);
  }
 else
 if ((status==MARRIED_FILING_JOINTLY) || (status==WIDOW))
  {
   if (income <   21512.00)  tax =              0.01 * income;                 else
   if (income <   50998.00)  tax =    215.12 +  0.02 * (income -   21512.00);  else
   if (income <   80490.00)  tax =    804.84 +  0.04 * (income -   50998.00);  else
   if (income <  111732.00)  tax =   1984.52 +  0.06 * (income -   80490.00);  else
   if (income <  141212.00)  tax =   3859.04 +  0.08 * (income -  111732.00);  else
   if (income <  721318.00)  tax =   6217.44 + 0.093 * (income -  141212.00);  else
   if (income <  865574.00)  tax =  60167.30 + 0.103 * (income -  721318.00);  else
   if (income < 1442628.00)  tax =  75025.67 + 0.113 * (income -  865574.00);
   else                      tax = 140232.77 + 0.123 * (income - 1442628.00);
  }
 else
  { /* Head of Household. */
   if (income <  21527.00)  tax =             0.01 * income;                else
   if (income <  51000.00)  tax =   215.27 +  0.02 * (income -  21527.00);  else
   if (income <  65744.00)  tax =   804.73 +  0.04 * (income -  51000.00);  else
   if (income <  81364.00)  tax =  1394.49 +  0.06 * (income -  65744.00);  else
   if (income <  96107.00)  tax =  2331.69 +  0.08 * (income -  81364.00);  else
   if (income < 490493.00)  tax =  3511.13 + 0.093 * (income -  96107.00);  else
   if (income < 588593.00)  tax = 40189.03 + 0.103 * (income - 490493.00);  else
   if (income < 980987.00)  tax = 50293.33 + 0.113 * (income - 588593.00); 
   else                     tax = 94633.85 + 0.123 * (income - 980987.00);
  }
 return (int)(tax+0.5);
}


void Report_bracket_info( double income, int status )
{
 double tx, rate;
 tx = TaxRateFormula( income, status );
 if ((status==SINGLE) || (status==MARRIED_FILING_SEPARAT))
  {
   if (income <   10756.00)  rate = 0.01;  else
   if (income <   25499.00)  rate = 0.02;  else
   if (income <   40245.00)  rate = 0.04;  else
   if (income <   55866.00)  rate = 0.06;  else
   if (income <   70606.00)  rate = 0.08;  else
   if (income <  360659.00)  rate = 0.093;  else
   if (income <  432787.00)  rate = 0.103;  else
   if (income <  721314.00)  rate = 0.113;  else  rate = 0.123;
  }
 else
 if ((status==MARRIED_FILING_JOINTLY) || (status==WIDOW))
  {
   if (income <   21512.00)  rate = 0.01;  else
   if (income <   50998.00)  rate = 0.02;  else
   if (income <   80490.00)  rate = 0.04;  else
   if (income <  111732.00)  rate = 0.06;  else
   if (income <  141212.00)  rate = 0.08;  else
   if (income <  721318.00)  rate = 0.093;  else
   if (income <  865574.00)  rate = 0.103;  else
   if (income < 1442628.00)  rate = 0.113;  else  rate = 0.123;
  }
 else
  {
   if (income <  21527.00)  rate = 0.01;  else
   if (income <  51000.00)  rate = 0.02;  else
   if (income <  65744.00)  rate = 0.04;  else
   if (income <  81364.00)  rate = 0.06;  else
   if (income <  96107.00)  rate = 0.08;  else
   if (income < 490493.00)  rate = 0.093;  else
   if (income < 588593.00)  rate = 0.103;  else
   if (income < 980987.00)  rate = 0.113;  else  rate = 0.123;
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
	 sched540Bb8o=0.0, sched540Bb8p=0.0, sched540Bc8p=0.0, sched540Bb8v=0.0, sched540Bc8v=0.0, 
	 sched540Bb8z=0.0, sched540Bc8z=0.0,
	 sched540Cb24b=0.0, sched540Cc24b=0.0, sched540Cb24c=0.0, sched540Cb24d=0.0, 
	 sched540Cb24f=0.0, sched540Cc24f=0.0, sched540Cb24g=0.0,  sched540Cc24g=0.0, 
	 sched540Cb24i=0.0, sched540Cb24j=0.0, sched540Cb24k=0.0, sched540Cb24z=0.0,
	 sched540Cc24z=0.0;
 int CkPayedUseTaxCDTFA=0;
 char labelx[1024]="";
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

 printf("CA-540 2024 - v%3.2f\n", thisversion);

 #if (0)
   add_pdf_markup( "NotReady", 1, 240, 40, 17, 1, 1.0, 0, 0, "\"This program is NOT ready for 2024.\"" );
  #ifdef microsoft
   system( "start bin\\notify_popup -delay 3 -expire 10 \"Warning: This program is NOT ready for 2024.\"" );
  #else
   system( "bin/notify_popup -delay 3 -expire 10 \"Warning: This program is NOT ready for 2024.\" &" );
  #endif
#endif


 /* Accept Form's "Title" line, and put out with date-stamp for your records. */
 read_line( infile, word );
 now = time(0);
 fprintf(outfile,"\n%s,	 v%2.2f, %s\n", word, thisversion, ctime( &now ));
 check_form_version( word, "Title:  CA State Tax Form 540 - 2024" );

 get_parameter( infile, 's', word, "FileName" );      /* Preliminary Fed Return Output File-name. */
 get_word(infile, prelim_1040_outfilename );
 ImportFederalReturnData( prelim_1040_outfilename, &PrelimFedReturn );


 /* Only for 2022, handle next line(s) optionally, due to change in template. */
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
 L[7] = 149.0 * iline7;							/* Updated for 2024. */
 showline(7);

 fprintf(outfile,"L7a = %d\n", L7a );

 get_parameter( infile, 's', word, "L8" );	/* Blind?, 1 if you or spouse, 2 if both. */
 get_parameter( infile, 'i', &iline8, "L8" );
 L[8] = iline8 * 149.0;							/* Updated for 2024. */
 showline(8);
 if (iline8 > 0) fprintf(outfile,"  L8a = %d\n", iline8 );

 get_parameter( infile, 's', word, "L9" );	/* Senior?, 1 if you or spouse, 2 if both. */
 get_parameter( infile, 'i', &iline9, "L9" );
 L[9] = iline9 * 149.0;							/* Updated for 2024. */
 showline(9);
 if (iline9 > 0) fprintf(outfile,"  L9a = %d\n", iline9 );

 get_parameter( infile, 's', word, "L10" );  /* Number of Dependents. */
 get_parameter( infile, 'i', &iline10, "L10"); 
 L[10] = iline10 * 461.0;						/* Updated for 2024. */
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
  GetLine("CA540_Subtr_B8v", &sched540Bb8v );
  GetLine("CA540_Addit_B8v", &sched540Bc8v );
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

  sched540Bb[9] = sched540Bb[9] + sched540Bb8v;
  showline_wlabelnz( " SchedCA540_B8Vb", sched540Bb8v );

  sched540Bc[9] = sched540Bc[9] + sched540Bc8v;
  showline_wlabelnz( " SchedCA540_B8Vc", sched540Bc8v );

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
 sched540part2[8] = sched540part2_8a + sched540part2_8b + sched540part2_8c;
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
   case MARRIED_FILING_SEPARAT:  threshA = 244857.0;	std_ded = 5540.0;   break;	/* Updated for 2024. */
   case HEAD_OF_HOUSEHOLD:       threshA = 367291.0;	std_ded = 11080.0;  break;
   case MARRIED_FILING_JOINTLY:
   case WIDOW:                   threshA = 489719.0;	std_ded = 11080.0;  break;
  }
 if (L[13] > threshA)
  { /*Itemized Deductions Worksheet*/	/* Page 67. */
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
  {		/* Minimum AGI (Line 17) required to file. */		/* Updated for 2024. */
    case SINGLE:
    case HEAD_OF_HOUSEHOLD:
		if (iline9 == 0)		
		  min2file = 17818.0;		/*Under65*/
		else
		  min2file = 25268.0;		/*65over*/
	  break;
   case MARRIED_FILING_JOINTLY: 
		if (iline9 == 0)		
		  min2file = 35642.0;		/*BothUnder65*/
		else
		if (iline9 == 1)		
		  min2file = 43092.0;		/*OneUnder65*/
		else
		  min2file = 50542.0;		/*Both65over*/
	  break;
   case WIDOW:
		if (iline9 == 0)		
		  min2file = 33185.0;		/*Under65*/
		else
		  min2file = 36793.0;		/*65over*/
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
	ws_d = ceil(ws_c / 2500.0);  
    else 
	ws_d = ceil(ws_c / 1250.0);
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
 
 // GetLineF( "L74", &L[74] ); 	/* Excess SDI. */

 /* Allow un-needed L74 for now. (Remove *optional* logic for 2024, once reoved from on ALL templates.) */
 get_parameter( infile, 'l', labelx, "L74 or L75");
 if (strcmp( labelx, "L74" ) == 0)
  {
   get_parameters( infile, 'f', &L[74], labelx );
   GetLineF( "L75", &L[75] ); 	/* Earned Income Tax Credit (EITC). */
  }
 else
 if (strcmp( labelx, "L75" ) == 0)
  {
   get_parameters( infile, 'f', &L[75], labelx );
   showline(75);
  }
 else
  {
   printf("Error: Found '%s' when expecteding A18 or B7a.\n", labelx );
   fprintf(outfile,"Error: Found '%s' when expecteding A18 or B7a\n", labelx );
   exit(1);
  }


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

 get_word(infile, labelx );     /* Look for optional fields. */
 while (!feof(infile))
  { /*OptionalLine*/
   read_comment_filtered_line( infile, word, 512 );
   // printf("\nLine '%s' = '%s'\n", labelx, word );
   if (word[0] != '\0')
    { /*valid_entry*/
      if (strstr( labelx,"WantHealthInfo" ) != 0)
       {
        if (toupper(word[0]) == 'Y')
         fprintf(outfile, "CkHCinfoYes X\n"); 
	else
         fprintf(outfile, "CkHCinfoNo X\n"); 
       }
      else
      if (strstr( labelx,"Discuss" ) != 0)
       {
        if (toupper(word[0]) == 'Y')
         fprintf(outfile, "CkDiscussYes X\n"); 
	else
         fprintf(outfile, "CkDiscussNo X\n"); 
       }
    } /*valid_entry*/
   get_word(infile, labelx );
  } /*OptionalLine*/

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

} // namespace taxsolve_CA_540_2024
} // namespace OpenTaxSolver2024

