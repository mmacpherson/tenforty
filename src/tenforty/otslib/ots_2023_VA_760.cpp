#include "ots_2023_routines.h"
namespace OpenTaxSolver2023 {
namespace taxsolve_VA_760_2023 {

#define SINGLE 		        1
#define MARRIED_FILING_JOINTLY  2
#define MARRIED_FILING_SEPARAT  3
#define HEAD_OF_HOUSEHOLD       4
#define WIDOW		        5
/************************************************************************/
/* TaxSolve_VA760_2023.c - 						*/
/* Copyright (C) 2023 - Aston Roberts					*/
/* 									*/
/* Compile:   gcc taxsolve_VA760_2023.c -o taxsolve_VA760_2023		*/
/* Run:	      ./taxsolve_VA760_2023  VA_760_2023.txt 			*/
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
/* Aston Roberts 1-2-2024	aston_roberts@yahoo.com			*/
/************************************************************************/



float thisversion=21.01;


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




/*----------------------------------------------------------------------------*/

int main( int argc, char *argv[] )
{
 int i, j, k;
 char word[1000], outfname[4000], *lnameptr, lastname[1024], *socsec, *datestr, *twrd, *infname=0;
 int status=0, exemptionsA=0, exemptionsB=0, youBlind=0, spouseBlind=0;
 time_t now;
 double L19b=0.0, std_ded=0.0, min2file, STA_VAGI=0.0;

 /* Intercept any command-line arguments. */
 printf("VA-760 2023 - v%3.1f\n", thisversion);
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
 check_form_version( word, "Title:  VA-760 State 2023" );

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

 if (yourDOB.year < 1959)			/* Updated for 2023. */
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
   if (spouseDOB.year < 1959)			/* Updated for 2023. */
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

 GetLineF( "L4", &L[4] );	/* Deduction for age on Jan 1, 2023. */

 GetLineF( "L5", &L[5] );	/* Social Security Act, Tier 1 Railroad Retirement Act benef. */

 GetLineF( "L6", &L[6] );	/* State Income Tax refund or overpayment credit */

 GetLineF( "L7", &L[7] );	/* Subtractions from Schedule ADJ, line 7 */

 L[8] = L[4] + L[5] + L[6] + L[7];
 showline(8);

 L[9] = L[3] - L[8]; 
 showline(9);			/* Virginia Adjusted Gross Income (VAGI) */

 GetLineF( "L10", &L[10] );	/* Deductions - Std or Itemized minus income taxes */

 switch (status)
  {							/* Updated for 2023. */
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

 get_parameter( infile, 'l', word, "L17" );
 if (strcmp( word, "STA_VAGI" ) == 0)
  {
   get_parameters( infile, 'f', &STA_VAGI, "STA_VAGI" );
   showline_wlabelnz( "STA_VAGI", STA_VAGI );
   get_parameter( infile, 's', word, "L17" );
  }
 get_parameters( infile, 'f', &L[17], "L17" );
 // GetLine( "L17", &L[17] );	/* Spouse Tax Adjustment. */		// Update in 2024.
 showline(17);

 L[18] = L[16] - L[17];
 showline_wmsg( 18, "Net Amount of Tax" );	

 GetLineF( "L19a", &L[19] );	/* Virginia tax withheld for 2023. */
 GetLineF( "L19b", &L19b );	/* Spouse's Virginia tax withheld. */

 GetLineF( "L20", &L[20] );	/* Estimated tax paid for 2023. (form 760ES) */

 GetLineF( "L21", &L[21] );	/* Amount of last year's overpayment applied toward 2023 estimated tax. */

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

} // namespace taxsolve_VA_760_2023
} // namespace OpenTaxSolver2023

