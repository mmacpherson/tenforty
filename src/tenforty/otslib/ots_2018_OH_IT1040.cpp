#include "ots_2018_routines.h"
namespace OpenTaxSolver2018 {
namespace taxsolve_OH_IT1040_2018 {

#define SINGLE 		        1
#define MARRIED_FILLING_JOINTLY 2
#define MARRIED_FILLING_SEPARAT 3
#define HEAD_OF_HOUSEHOLD       1
/************************************************************************/
/* TaxSolve_OH_IT1040_2018.c - 						*/
/* Copyright (C) 2019 - Aston Roberts					*/
/* 									*/
/* Compile:   gcc taxsolve_OH_IT1040_2018.c -o taxsolve_OH_IT1040_2018	*/
/* Run:	      ./taxsolve_OH_IT1040_2018  OH_IT1040_2018.txt 		*/
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
/* Aston Roberts 2-14-2019	aston_roberts@yahoo.com			*/
/************************************************************************/



double thisversion=16.01;


double TaxRateFunction( double x, int status )
{							/* Updated for 2018. */
 if (x <= 10850.0) return     0.0; else
 if (x < 16300.0) return    80.56 + (x-10850.0)  * 0.01980; else
 if (x < 21750.0) return   188.47 + (x-16300.0)  * 0.02476; else
 if (x < 43450.0) return   323.41 + (x-21750.0)  * 0.02969; else
 if (x < 86900.0) return   967.68 + (x-43450.0)  * 0.03465; else
 if (x < 108700.0) return 2473.22 + (x-86900.0)  * 0.03960; else
 if (x < 217400.0) return 3336.50 + (x-108700.0) * 0.04597;
 else 		   return 8333.44 + (x-217400.0) * 0.04997;
}


void Report_bracket_info( double income, double tx, int status )
{							/* Updated for 2018. */
 double rate;
 if (income <= 10850.0) rate = 0.0; else
 if (income < 16300.0)  rate = 0.01980; else
 if (income < 21750.0)  rate = 0.02476; else
 if (income < 43450.0)  rate = 0.02969; else
 if (income < 86900.0)  rate = 0.03465; else
 if (income < 108700.0) rate = 0.03960; else
 if (income < 217400.0) rate = 0.04597;
 else 		   	rate = 0.04997;
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
 printf("OH IT1040 2018 - v%3.1f\n", thisversion);
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

 GetLine( "SchedA_33", &SchedA[33] );	/* Disability and survivorship benefi ts */
 GetLine( "SchedA_34", &SchedA[34] );	/* Unreimbursed long-term care insurance premiums ... */
 GetLine( "SchedA_35", &SchedA[35] );	/* Funds deposited into, and earnings of, a medical savings account */
 GetLine( "SchedA_36", &SchedA[36] );	/* Qualified organ donor expenses */

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
 GetLine( "Credits_19", &SchedC[19] );	/* Tech investment credit */
 GetLine( "Credits_20", &SchedC[20] );	/* Enterprise zone day care and training credits */
 GetLine( "Credits_21", &SchedC[21] );	/* Research and development credit */
 GetLine( "Credits_22", &SchedC[22] );	/* Ohio historic preservation credit, nonrefundable carryforward portion */

 GetLine( "Credits_25", &SchedC[25] );	/*  Portion L3 was not earned in Ohio. */

 GetLine( "Credits_28", &SchedC[28] );	/* Portion L3 taxed by other states */
 GetLine( "Credits_31", &SchedC[31] );	/* Income Tax paid to Other States */

 GetLine( "Credits_34", &SchedC[34] );	/* Historic preservation credit */
 GetLine( "Credits_35", &SchedC[35] );	/* Business jobs credit */
 GetLine( "Credits_36", &SchedC[36] );	/* Pass-through entity credit */
 GetLine( "Credits_37", &SchedC[37] );	/* Motion picture production credit */
 GetLine( "Credits_38", &SchedC[38] );	/* Financial Institutions Tax (FIT) credit */
 GetLine( "Credits_39", &SchedC[39] );	/* Venture capital credit */


 /* ---- Do Calculations. ---- */

 for (j=1; j <= 9; j++)
  SchedA[10] = SchedA[10] + SchedA[j];

 for (j=11; j <= 36; j++)
  SchedA[37] = SchedA[37] + SchedA[j];

 L2a = SchedA[10];
 L2b = SchedA[37];
 L[3] = L[1] + L2a - L2b;

 if (L[3] <= 40000.0)			/* Updated for 2018. */
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

 for (j=12; j <= 22; j++)
  SchedC[23] = SchedC[23] + SchedC[j];
 SchedC[24] = NotLessThanZero( SchedC[11] - SchedC[23] );

 SchedC[26] = L[3];
 j = 10000.0 * SchedC[25] / SchedC[26];
 factorA = (double)j / 10000.0;
 // printf(" %4g\n", factorA );
 SchedC[27] = SchedC[24] * factorA;

 SchedC[29] = L[3];
 j = 10000.0 * SchedC[28] / SchedC[29];
 factorB = (double)j / 10000.0;
 // printf(" %4g\n", factorB );
 SchedC[30] = SchedC[24] * factorB;
 // SchedC[31] = L[13];
 SchedC[32] = smallerof( SchedC[30], SchedC[31] );

 SchedC[33] = SchedC[10] + SchedC[23] + SchedC[27] + SchedC[32];
 L[9] = SchedC[33];

 for (j=34; j <= 39; j++)
  SchedC[40] = SchedC[40] + SchedC[j];
 L[16] = SchedC[40];

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

 if ((L[1] < 12950.0) && (L[3] < 0.0))
  fprintf(outfile, "You do not need to file Ohio tax return (Fed AGI < minimum).\n");

 if ((L[1] < 12950.0) && (L[4] >= L[3]))
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

 fprintf(outfile,"\n-- 2018 Ohio Schedule A --\n");
 for (j = 1; j <= 37; j++)
  {
   sprintf( label, "SchedA%d", j );
   showline_wlabel( label, SchedA[j] );
  }

 fprintf(outfile,"\n-- 2018 Ohio Schedule of Credits --\n");
 for (j = 1; j <= 27; j++)
  {
   sprintf( label, "Credits%d", j );
   showline_wlabel( label, SchedC[j] );
  }
 sprintf(word,"%5.4f", factorA);
printf("factorA = %g, word = '%s'\n", factorA, word );
 fprintf(outfile,"   Credits27_Factor %s\n", &(word[2]) );
 showline_wlabel( "Credits28", SchedC[28] );
 showline_wlabel( "Credits29", SchedC[29] );
 showline_wlabel( "Credits30", SchedC[30] );
 sprintf(word,"%5.4f", factorB );
printf("factorB = %g, word = '%s'\n", factorB, word );
 fprintf(outfile,"   Credits30_Factor %s\n", &(word[2]) );

 for (j = 31; j <= 40; j++)
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
 fprintf(outfile,"SpouseSocSec#: %s\n", socsec );
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

} // namespace taxsolve_OH_IT1040_2018
} // namespace OpenTaxSolver2018
