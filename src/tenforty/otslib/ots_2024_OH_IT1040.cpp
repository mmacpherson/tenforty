#include "ots_2024_routines.h"
namespace OpenTaxSolver2024 {
namespace taxsolve_OH_IT1040_2024 {

#define SINGLE 		        1
#define MARRIED_FILING_JOINTLY  2
#define MARRIED_FILING_SEPARAT  3
#define HEAD_OF_HOUSEHOLD       1
/************************************************************************/
/* TaxSolve_OH_IT1040_2024.c - 						*/
/* Copyright (C) 2025 - Aston Roberts					*/
/* 									*/
/* Compile:   gcc taxsolve_OH_IT1040_2024.c -o taxsolve_OH_IT1040_2024	*/
/* Run:	      ./taxsolve_OH_IT1040_2024  OH_IT1040_2024.txt 		*/
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



double thisversion=22.01;


double TaxRateFunction( double x, int status )
{							/* Updated for 2024. */
 if (x <=  26050.0) return    0.0; else
 if (x <= 100000.0) return  360.69 + (x-26050.0)  * 0.0275;
 else		    return 2394.32 + (x-100000.0) * 0.035; 
}


void Report_bracket_info( double income, double tx, int status )
{							/* Updated for 2024. */
 double rate;
 if (income <= 26050.0) rate = 0.0; else
 if (income < 100000.0) rate = 0.0275;
 else 		   	rate = 0.0350;
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
 double L2a, L2b, L7a, L8a, L8b, L8c;
 double jfc=0.0, exemption_amnt;
 double SchedA[MAX_LINES], SchedC[MAX_LINES];

 /* Intercept any command-line arguments. */
 printf("OH IT1040 2024 - v%3.1f\n", thisversion);
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
 check_form_version( word, "Title:  Ohio IT1040 State 2024" );

 /* get_parameter(infile, kind, x, emssg ) */
 get_parameter( infile, 's', word, "Status" );
 get_parameter( infile, 'l', word, "Status ?");
 if (strncasecmp(word,"Single",4)==0) status = SINGLE; else
 if (strncasecmp(word,"Married/Joint",11)==0) status = MARRIED_FILING_JOINTLY; else
 if (strncasecmp(word,"Married/Sep",11)==0) status = MARRIED_FILING_SEPARAT; else
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
 GetLine( "SchedA_3", &SchedA[3] );
 GetLine( "SchedA_4", &SchedA[4] );
 GetLine( "SchedA_5", &SchedA[5] );
 GetLine( "SchedA_6", &SchedA[6] );
 GetLine( "SchedA_7", &SchedA[7] );
 GetLine( "SchedA_8", &SchedA[8] );
 GetLine( "SchedA_9", &SchedA[9] );
 GetLine( "SchedA_10", &SchedA[10] );
 GetLine( "SchedA_11", &SchedA[11] );

 GetLine( "SchedA_13", &SchedA[13] );	/* Business income deduction (Ohio Schedule IT BUS, line 11) */
 GetLine( "SchedA_14", &SchedA[14] );	/* Compensation earned in Ohio by residents of neighboring states */
 GetLine( "SchedA_15", &SchedA[15] );
 GetLine( "SchedA_16", &SchedA[16] );
 GetLine( "SchedA_17", &SchedA[17] );
 GetLine( "SchedA_18", &SchedA[18] );
 GetLine( "SchedA_19", &SchedA[19] );
 GetLine( "SchedA_20", &SchedA[20] );
 GetLine( "SchedA_21", &SchedA[21] );
 GetLine( "SchedA_22", &SchedA[22] );

 GetLine( "SchedA_23", &SchedA[23] );
 GetLine( "SchedA_24", &SchedA[24] );
 GetLine( "SchedA_25", &SchedA[25] );
 GetLine( "SchedA_26", &SchedA[26] );
 GetLine( "SchedA_27", &SchedA[27] );
 GetLine( "SchedA_28", &SchedA[28] );

 GetLine( "SchedA_29", &SchedA[29] );
 GetLine( "SchedA_30", &SchedA[30] );
 GetLine( "SchedA_31", &SchedA[31] );
 GetLine( "SchedA_32", &SchedA[32] );
 GetLine( "SchedA_33", &SchedA[33] );

 GetLine( "SchedA_34", &SchedA[34] );
 GetLine( "SchedA_35", &SchedA[35] );

 GetLine( "SchedA_36", &SchedA[36] );
 GetLine( "SchedA_37", &SchedA[36] );
 GetLine( "SchedA_38", &SchedA[36] );
 GetLine( "SchedA_39", &SchedA[39] );
 GetLine( "SchedA_40", &SchedA[40] );
 GetLine( "SchedA_41", &SchedA[41] );
 GetLine( "SchedA_42", &SchedA[42] );
 GetLine( "SchedA_43", &SchedA[43] );
 GetLine( "SchedA_44", &SchedA[44] );
 GetLine( "SchedA_45", &SchedA[45] );
 
 /* Schedule of Credits. */
 GetLine( "Credits_2", &SchedC[2] );	/* Retirement income credit */
 SchedC[2] = smallerof( SchedC[2], 200.0 );
 GetLine( "Credits_3", &SchedC[3] );	/* Lump sum retirement credit (Ohio LS WKS, line 6) */
 GetLine( "Credits_4", &SchedC[4] );	/* Senior citizen credit */
 SchedC[4] = smallerof( SchedC[4], 50.0 );
 GetLine( "Credits_5", &SchedC[5] );	/* Lump sum distribution credit */
 GetLine( "Credits_6", &SchedC[6] );	/* Child care and dependent care credit */
 GetLine( "Credits_7", &SchedC[7] );	/* Displaced worker training credit */
 SchedC[7] = smallerof( SchedC[7], 500.0 );
 GetLine( "Credits_8", &SchedC[8] );	/* Campaign contribution credit for Ohio General Assembly */
 if (status == MARRIED_FILING_JOINTLY)
   SchedC[8] = smallerof( SchedC[8], 100.0 );
 else
   SchedC[8] = smallerof( SchedC[8], 50.0 );

 GetLine( "Credits_13", &SchedC[13] );	/* Earned income credit */
 GetLine( "Credits_14", &SchedC[14] );	/* Home school expenses credit. */
 GetLine( "Credits_15", &SchedC[15] );	/* Scholarship donation credit. */
 GetLine( "Credits_16", &SchedC[16] );	/* Nonchartered, nonpublic school tuition credit. */
 GetLine( "Credits_17", &SchedC[17] );	/* Vocational job credit. */
 GetLine( "Credits_18", &SchedC[18] );	/* Ohio adoption credit */
 GetLine( "Credits_19", &SchedC[19] );	/* Job retention credit, nonrefundable portion */
 GetLine( "Credits_20", &SchedC[20] );	/* Credit for eligible new employees in an enterprise zone */
 GetLine( "Credits_21", &SchedC[21] );	/* Credit for purchases of grape production property */
 GetLine( "Credits_22", &SchedC[22] );
 GetLine( "Credits_23", &SchedC[23] );
 GetLine( "Credits_24", &SchedC[24] );
 GetLine( "Credits_25", &SchedC[25] );
 GetLine( "Credits_26", &SchedC[26] );
 GetLine( "Credits_27", &SchedC[27] );
 GetLine( "Credits_28", &SchedC[28] );
 GetLine( "Credits_29", &SchedC[29] );
 GetLine( "Credits_30", &SchedC[30] );
 GetLine( "Credits_31", &SchedC[31] );
 GetLine( "Credits_32", &SchedC[32] );
 GetLine( "Credits_33", &SchedC[33] );
 GetLine( "Credits_34", &SchedC[35] );


 GetLine( "Credits_37", &SchedC[37] );	/* Non-Resident credit */
 GetLine( "Credits_38", &SchedC[38] );	/* Resident credit - OH IT RC, line 7 */

 GetLine( "Credits_40", &SchedC[40] );	/* Refundable Historic preservation credit */
 GetLine( "Credits_41", &SchedC[41] );	/* Refundable Business jobs credit */
 GetLine( "Credits_42", &SchedC[42] );	/* Pass-through entity credit */
 GetLine( "Credits_43", &SchedC[43] );	/* Motion picture production credit */
 GetLine( "Credits_44", &SchedC[44] );
 GetLine( "Credits_45", &SchedC[45] );	/* Venture capital credit */


 /* ---- Do Calculations. ---- */

 for (j=1; j <= 11; j++)
  SchedA[12] = SchedA[12] + SchedA[j];

 for (j=13; j <= 45; j++)
  SchedA[46] = SchedA[46] + SchedA[j];

 L2a = SchedA[12];
 L2b = SchedA[46];
 L[3] = L[1] + L2a - L2b;

 if (L[3] <= 40000.0)			/* Updated for 2024. */
  exemption_amnt = 2400.0;
 else
 if (L[3] <= 80000.0)
  exemption_amnt = 2150.0;
 else
  exemption_amnt = 1900.0;
 L[4] = exemption_amnt * exemptions;

 L[5] = NotLessThanZero( L[3] - L[4] );
 L[7] = NotLessThanZero( L[5] - L[6] );
 L7a = L[7];
 L8a = TaxRateFunction( L7a, status );
 L8c = L8a + L8b;
 SchedC[1] = L8c;

 if (L[5] < 30000.0)
  SchedC[9] = 20.0 * exemptions;	/* Exemption credtit. */
 
 for (j=2; j <= 9; j++)
  SchedC[10] = SchedC[10] + SchedC[j];		

 SchedC[11] = NotLessThanZero( SchedC[1] - SchedC[10] );

 if ((status == MARRIED_FILING_JOINTLY) && (qualify_jfc))
  { /*Joint_Filing_Credit*/
    if (L[5] < 25000) jfc = 0.20;
    else
    if (L[5] < 50000) jfc = 0.15;
    else
    if (L[5] < 75000) jfc = 0.10;
    else jfc = 0.05;
    SchedC[12] = smallerof( jfc * SchedC[11], 650.0 );
  } /*Joint_Filing_Credit*/

 for (j=12; j <= 34; j++)
  SchedC[35] = SchedC[35] + SchedC[j];          
 SchedC[36] = NotLessThanZero( SchedC[11] - SchedC[34] );

 SchedC[39] = SchedC[10] + SchedC[35] + SchedC[37] + SchedC[38];
 L[9] = SchedC[39];
 L[10] = NotLessThanZero( L8c - L[9] );

 for (j=40; j <= 45; j++)
  SchedC[46] = SchedC[46] + SchedC[j];          
 L[16] = SchedC[46];

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

 if ((L[1] < 26050.0) && (L[3] < 0.0))		/* Min2File. */
  fprintf(outfile, "You do not need to file Ohio tax return (Fed AGI < minimum).\n");

 if ((L[1] < 26050.0) && (L[4] >= L[3]))
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

 fprintf(outfile,"\n-- 2024 Ohio Schedule A --\n");
 for (j = 1; j <= 46; j++)
  {
   sprintf( label, "SchedA%d", j );
   showline_wlabel( label, SchedA[j] );
  }

 fprintf(outfile,"\n-- 2024 Ohio Schedule of Credits --\n");
 for (j = 1; j <= 11; j++)
  {
   sprintf( label, "Credits%d", j );
   showline_wlabel( label, SchedC[j] );
  }
 if (jfc > 0.0)
  fprintf(outfile,"JFC = %d\n", (int)(100.0 * jfc + 0.25) );
 for (j = 12; j <= 46; j++)
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
 socsec = GetTextLine( "YourSocSec#:" );
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
 socsec = GetTextLine( "SpouseSocSec#:" );
 format_socsec( socsec, 0 );
 if (status != MARRIED_FILING_SEPARAT)
  fprintf(outfile,"SpouseSocSec#: %s\n", socsec );
 else
  fprintf(outfile,"SpouseSocSec#Sep: %s\n", socsec );
 free( socsec );
 writeout_line = 1;
 GetTextLineF( "Number&Street:" );
 GetTextLineF( "Town:" );
 fprintf(outfile,"State: OH\n");
 GetTextLineF( "Zipcode:" );
 GetTextLineF( "AddressLine2:" );
 GetTextLineF( "OhioCounty:" );

 fprintf(outfile,"CkFYrRes: X\n");
 if (status == MARRIED_FILING_JOINTLY)
  fprintf(outfile,"CkFYrResSp: X\n");

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

} // namespace taxsolve_OH_IT1040_2024
} // namespace OpenTaxSolver2024

