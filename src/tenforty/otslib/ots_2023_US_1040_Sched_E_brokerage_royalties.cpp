#include "ots_2023_routines.h"
namespace OpenTaxSolver2023 {
namespace taxsolve_US_1040_Sched_E_brokerage_royalties_2023 {

#define Yes 1
#define No  0
/************************************************************************
* TaxSolve_US_1040_Sched_E_brokerage_royalties_2023.c -
*
* E. Strnod 9/12/2023
*************************************************************************/
/*
 * This version handles Schedule E for reporting brokerage royalty income
 * only.  It may be used as a starting point for the full Schedule E
 * (which would be used for real estate rental income, etc.)  For ease
 * of use for brokerage royalty income only, a full version should be
 * implemented in an alternative, separate program.  This program makes
 * assumptions about some responses in the form when handling brokerage
 * royalty income.
 */
float thisversion=2.00;



/*----------------------------------------------------------------------------*/

int main( int argc, char *argv[] )
{
 int i, j, k;
 char word[4000], outfname[4000], *infname=0;
 time_t now;

 printf("Schedule E, 2023 - v%3.2f\n", thisversion);


add_pdf_markup( "NotReady", 1, 240, 40, 17, 1, 1.0, 0, 0, "\"This program is NOT ready for 2023.\"" );
#ifdef microsoft
 system( "start bin\\notify_popup -delay 3 -expire 10 \"Warning: This program is NOT ready for 2023.\"" );
#else
 system( "bin/notify_popup -delay 3 -expire 10 \"Warning: This program is NOT ready for 2023.\" &" );
#endif



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
 /* Expect lines, something like:
        Title:  Form XXXX Return
        L2              {Returns and Allowances}
        . . .
 */


 /* Accept Form's "Title" line, and put out with date-stamp for your records. */
 read_line( infile, word );
 now = time(0);
 fprintf(outfile,"\n%s,  v%2.2f, %s\n", word, thisversion, ctime( &now ));


 /* ----- Accept form data and process the numbers.         ------ */
 /* ----- Place all your form-specific code below here .... ------ */

 // property income & expense lines and values array.  For referential convenience,
 // the line desc #defines are 1-based to align with the form (so will technically
 // be off by one but the array is just for capture.) Each define applies to lines
 // for the 3 properties' income and expenses.
 // NOTE: very little of this is needed for royalties but looking ahead with the impl
 // to prevent having to totally rework it later if expanded to include rents.
 int    propertyIndex = 0;   // only 0 in case of royalties
 double propIncAndExp[3][23] = {{0}};
 #define RENTS_RECD 3
 #define ROYALTIES_RECD 4
 #define ADVERTISING_EXP 5
 #define AUTO_AND_TRAVEL_EXP 6
 #define CLEANING_AND_MAINT_EXP 7
 #define COMMISSIONS_EXP 8
 #define INSURANCE_EXP 9
 #define LEGAL_AND_PROF_FEES_EXP 10
 #define MGMT_FEES_EXP 11
 #define MORTGAGE_INT_PAID_EXP 12
 #define OTHER_INT_EXP 13
 #define REPAIRS_EXP 14
 #define SUPPLIES_EXP 15
 #define TAXES_EXP 16
 #define UTILITIES_EXP 17
 #define DEPRECIATION_OR_DEPLETION_EXP 18
 #define OTHER_EXP 19
 #define TOTAL_EXP 20
 #define INC_MINUS_EXP 21
 #define DED_RE_LOSS_AFTER_LIMIT 22

 double totRentRecd = 0.0, totRoyaltiesRecd = 0.0, totMortgageIntExp = 0.0, totDepreciationDepletionExp = 0.0,
	totExpenses = 0.0, totIncome = 0.0, totLosses = 0.0, totIncomeOrLosses = 0.0;

 GetTextLineF( "YourName:" );
 GetTextLineF( "YourSocSec#:" );

 // Data collection for Schedule E is abbreviated to accommodate brokerage investment royalties only
 //get_parameter( infile, 's', word, "Ireq1099s:" );	/* Did you make payments requiring 1099's ?  */
 //get_parameter( infile, 'b', &j, "Ireq1099s:" );

 //if (j == 0)
  fprintf(outfile,"ChkIreq1099sNo X\n");
 //else {
 // fprintf(outfile,"ChkIreq1099sYes X\n");

  // validation warning applicable to only-royalties implementation
  //fprintf(outfile, "WARNING: 'Y' on this question does not apply to brokerage investment royalties!\n");
 //}

 // (1a of form collects 3 physical addresses; this will be blank for only-royalties application,
 // so ignoring for now.)

 // 1b : Type of property (3 columns; for only-royalties, only concerned with 1b_A and it'd better be "6")
 //GetTextLineF( "1b_A:" );
 fprintf(outfile, "1b_A: 6\n");
 //validation warning applicable to only-royalties implementation

 //fprintf(outfile, "WARNING:  anything other than '6' on question 1b_A \n   does not apply to brokerage investment royalties!\n");

 // Income: (collecting just 1 col, doing royalties only)
 GetLineF("4_A", &propIncAndExp[propertyIndex][ROYALTIES_RECD]);

 // Expenses: (collecting just 1 col, doing royalties only)
 GetLineF("18_A", &propIncAndExp[propertyIndex][DEPRECIATION_OR_DEPLETION_EXP]);
 GetTextLineF( "19_text:" );
 GetLineF("19_A", &propIncAndExp[propertyIndex][OTHER_EXP]);

 // Properties calculations (includes all properties/incomes/expenses (some not collected), for future expansion):
 for (propertyIndex = 0; propertyIndex < 3; propertyIndex++) {
	 for (int i = ADVERTISING_EXP; i < TOTAL_EXP; i++) {
		 propIncAndExp[propertyIndex][TOTAL_EXP] += propIncAndExp[propertyIndex][i];
	 }
	 double propTotInc = propIncAndExp[propertyIndex][RENTS_RECD] + propIncAndExp[propertyIndex][ROYALTIES_RECD];
	 propIncAndExp[propertyIndex][INC_MINUS_EXP] += (propTotInc - propIncAndExp[propertyIndex][TOTAL_EXP]);
 }

 // total expenses for each property (only printing A for now, for royalties-only)
 fprintf(outfile, "20_A: %2.2f\n", propIncAndExp[0][TOTAL_EXP]);
 // total income minus expenses for each property (only printing A for now, for royalties-only)
 fprintf(outfile, "21_A: %2.2f\n", propIncAndExp[0][INC_MINUS_EXP]);

 // Totals calculations across all properties
 for (propertyIndex = 0; propertyIndex < 3; propertyIndex++) {
	 totRentRecd += propIncAndExp[propertyIndex][RENTS_RECD];
 }
 for (propertyIndex = 0; propertyIndex < 3; propertyIndex++) {
	 totRoyaltiesRecd += propIncAndExp[propertyIndex][ROYALTIES_RECD];
 }
 for (propertyIndex = 0; propertyIndex < 3; propertyIndex++) {
	 totMortgageIntExp += propIncAndExp[propertyIndex][MORTGAGE_INT_PAID_EXP];
 }
 for (propertyIndex = 0; propertyIndex < 3; propertyIndex++) {
	 totDepreciationDepletionExp += propIncAndExp[propertyIndex][DEPRECIATION_OR_DEPLETION_EXP];
 }
 for (propertyIndex = 0; propertyIndex < 3; propertyIndex++) {
	 totExpenses += propIncAndExp[propertyIndex][TOTAL_EXP];
 }
 for (propertyIndex = 0; propertyIndex < 3; propertyIndex++) {
	 // add positive amounts only here
	 if (propIncAndExp[propertyIndex][INC_MINUS_EXP] > 0)
		 totIncome += propIncAndExp[propertyIndex][INC_MINUS_EXP];
 }

 // DED_RE_LOSS_AFTER_LIMIT (line 22) values are shown in parentheses, indicating negative, so they will
 // NOT be negative in the variable.  Subtract them rather than adding.
 for (propertyIndex = 0; propertyIndex < 3; propertyIndex++) {
	 // add negative amounts only here
	 if (propIncAndExp[propertyIndex][INC_MINUS_EXP] < 0)
		 totLosses += propIncAndExp[propertyIndex][INC_MINUS_EXP];
 }

 for (propertyIndex = 0; propertyIndex < 3; propertyIndex++) {
	 totLosses -= propIncAndExp[propertyIndex][DED_RE_LOSS_AFTER_LIMIT];
 }
 totIncomeOrLosses = totIncome + totLosses;

 // total royalties across all properties
 fprintf(outfile, "23b: %2.2f\n", totRoyaltiesRecd);

 // total depreciation expense or depletion across all properties
 fprintf(outfile, "23d: %2.2f\n", totDepreciationDepletionExp);

 // total expenses across all properties
 fprintf(outfile, "23e: %2.2f\n", totExpenses);

 // total income across all properties
 fprintf(outfile, "24: %2.2f\n", totIncome);

 // total losses across all properties
 fprintf(outfile, "25: %2.2f\n", totLosses * -1);

 // total rental real estate and royalty income or (loss) across all properties
 fprintf(outfile, "26: %2.2f\n", totIncomeOrLosses);

 /* ----- .... Until here.  ----- */


 /***
    Summary of useful functions:
	GetLine( "label", &variable )	- Looks for "label" in input file, and places the corresponding sum of
					  values following that label (until ";") into variable.
	GetLineF( "label", &variable )	- Like GetLine() above, but also writes the result to the output file.
	GetLineFnz(( "label", &variable ) - Like GetLine(), but only writes non-zero values to the output file.
	GetLine1( "label", &variable )  - Like GetLine() above, but expects single value (no sum, no ";" in input file).

	c = SmallerOf( a, b );		- Selects smaller of two values.
	c = LargerOf( a, b );		- Selects larger of two values.
	c = NotLessThanZero( a );	- Selects positive value or zero. Prevents negative values.

	showline( j )			- Writes currency value of L[j] to output file with label in nice format.
	shownum( j )			- Writes integer value of L[j] to output file with label in nice format.
	showline_wmsg( j, "msg" )	- Like showline, but adds the provided message to the output line.
	ShowLineNonZero( j )		- Like showline, but only writes non-zero values.
	ShowLineNonZero_wMsg( j, "msg" ) - Like showline_wmsg, but only writes non-zero values.
	showline_wlabel( "label", value ) - For custom line names and variables not in the default L[] array.
	showline_wlabelnz( "label", value ) - Like showline_wlabel, but only writes non-zero values.
	showline_wlabelmsg( "label", value, "msg" ) - Like showline_wlabel,but adds the provided message to the output line.

  ***/

 fclose(infile);
 grab_any_pdf_markups( infname, outfile );
 fclose(outfile);

 printf("\nListing results from file: %s\n\n", outfname);
 Display_File( outfname );

 return 0;
}


#undef Yes
#undef No

} // namespace taxsolve_US_1040_Sched_E_brokerage_royalties_2023
} // namespace OpenTaxSolver2023
