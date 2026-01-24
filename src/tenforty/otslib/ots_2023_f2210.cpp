#include "ots_2023_routines.h"
namespace OpenTaxSolver2023 {
namespace taxsolve_f2210_2023 {

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
/* TaxSolve_Form_2210.c - 2023                                          */
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

/*-----------Tax Routines Copied From taxsolve_US_1040_2023.c----------------*/

			/* Following values taken from 1040-Instructions pg 110. */	/* Updated for 2023. */
double brkpt[4][9]={
		{ 0.0,  11000.0,  44725.0,  95375.0, 182100.0, 231250.0, 578125.0, 9e19 },  /* Single */
		{ 0.0,  22000.0,  89450.0, 190750.0, 364200.0, 462500.0, 693750.0, 9e19 },  /* Married, filing jointly. */
		{ 0.0,  11000.0,  44725.0,  95375.0, 182100.0, 231250.0, 346875.0, 9e19 },  /* Married, filing separate. */
		{ 0.0,  15700.0,  59850.0,  95350.0, 182100.0, 231250.0, 578100.0, 9e19 },  /* Head of Household. */
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

/* 2023 Tax Rate Schedule for Estates/Trusts - From 2023 Form 1041 Instructions, page 30 */

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

 printf("Form 2210, 2023 - v%3.2f\n", thisversion);

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
 check_form_version( word, "Title:  Form 2210 for Tax Year 2023" );

 // fprintf(outfile,"\n--- THIS IS PRELIMINARY USER-CONTRIBUTED FORM ---\n");
 // MarkupPDF( 1, 240, 40, 17, 1.0, 0, 0 ) NotReady "This program is NOT updated for 2023."
 // add_pdf_markup( "NotReady", 1, 240, 40, 17, 1, 1.0, 0, 0, "\"This program is NOT ready for 2023.\"" );


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

		a[29] = 4050;			/* Updated for 2023 */
		b[29] = 66750;
		c[29] = 106800;
		d[29] = 160200;

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

} // namespace taxsolve_f2210_2023
} // namespace OpenTaxSolver2023
