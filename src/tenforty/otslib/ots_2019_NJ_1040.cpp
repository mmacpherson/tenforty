#include "ots_2019_routines.h"
namespace OpenTaxSolver2019 {
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

} // namespace taxsolve_NJ_1040_2019
} // namespace OpenTaxSolver2019

