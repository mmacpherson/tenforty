#include "ots_2022_routines.h"
namespace OpenTaxSolver2022 {
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

} // namespace taxsolve_MA_1_2022
} // namespace OpenTaxSolver2022
