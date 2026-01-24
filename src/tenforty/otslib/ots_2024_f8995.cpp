#include "ots_2024_routines.h"
namespace OpenTaxSolver2024 {
namespace taxsolve_f8995_2024 {


/************************************************************************/
/* taxsolve_f8995.c -                                                   */
/* Contributed by Rylan Luke, 1/2024					*/
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

float thisversion=2.00;



//====== Form 1040 Import ======
// Imported form data; add elements as needed, using the same variable name as in the imported form label for clarity.
// Also update 'f1040_imp_defs' to add mapping.
typedef struct FED_1040_IMP_F8995_T {
        double L11;
        double L12;
        double S1_3;
        double S1_15;
        double S1_16;
        double S1_17;
        char *Your1stName;
        char *YourLastName;
        char *YourSocSec;
} FED_1040_IMP_F8995, P_FED_1040_IMP_F8995;

FED_1040_IMP_F8995 f1040i;

// Mapping from label name to address of local data struct element; either double or char*
static FORM_IMPORT_DEF f1040_imp_defs[] = {
        { "L11", &f1040i.L11, NULL },
        { "L12", &f1040i.L12, NULL },
        { "S1_3", &f1040i.S1_3, NULL },
        { "S1_15", &f1040i.S1_15, NULL },
        { "S1_16", &f1040i.S1_16, NULL },
        { "S1_17", &f1040i.S1_17, NULL },
        { "Your1stName:", NULL, &f1040i.Your1stName },
        { "YourLastName:", NULL, &f1040i.YourLastName },
        { "YourSocSec#:", NULL, &f1040i.YourSocSec }
};

int f1040_imp_defs_size = sizeof(f1040_imp_defs)/sizeof(FORM_IMPORT_DEF);

//====== Schedule C Import ======
typedef struct FED_SCH_C_IMP_F8995_T {
        double L7;
        double L31;
} FED_SCH_C_IMP_F8995_T, *P_FED_SCH_C_IMP_F8995;

FED_SCH_C_IMP_F8995_T f_sch_c;

// Mapping from label name to address of local data struct element; either double or char*
static FORM_IMPORT_DEF f_sch_c_imp_defs[] = {
        { "L7", &f_sch_c.L7, NULL },
        { "L31", &f_sch_c.L31, NULL },
};

int f_sch_c_imp_defs_size = sizeof(f_sch_c_imp_defs)/sizeof(FORM_IMPORT_DEF);

/*----------------------------------------------------------------------------*/

int main( int argc, char *argv[] )
{
 int i, j, k;
 char word[6000], outfname[6000], *infname=0;
 time_t now;
 // Local line number variables for direct/indirect lines

 printf("Form 8995, 2024 - v%3.2f\n", thisversion);

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
 check_form_version( word, "Title: 2024 Form 8995" );

 /* ----- Accept form data and process the numbers.         ------ */
 /* ----- Place all your form-specific code below here .... ------ */


 char *f1040_filename = GetTextLine( "FileName1040") ;

 if (strlen(f1040_filename) != 0) {
     ImportReturnData( f1040_filename, f1040_imp_defs, f1040_imp_defs_size);
     fprintf( outfile, "INFO: --- Imported 1040 Data from file '%s' ---\n", f1040_filename);
     fprintf( outfile, "INFO: f1040i.L11   -- %6.2f\n", f1040i.L11);
     fprintf( outfile, "INFO: f1040i.L12  -- %6.2f\n", f1040i.L12);
     fprintf( outfile, "INFO: f1040i.S1_15   -- %6.2f\n", f1040i.S1_15);
     fprintf( outfile, "INFO: f1040i.S1_16   -- %6.2f\n", f1040i.S1_16);
     fprintf( outfile, "INFO: f1040i.S1_17   -- %6.2f\n", f1040i.S1_17);
     fprintf( outfile, "INFO: f1040i.S1_3  -- %6.2f\n", f1040i.S1_3);
     fprintf( outfile, "INFO: f1040i.Your1stName: -- %s\n", f1040i.Your1stName);
     fprintf( outfile, "INFO: f1040i.YourLastName: -- %s\n", f1040i.YourLastName);
     fprintf( outfile, "INFO: f1040i.YourSocSec#: -- %s\n", f1040i.YourSocSec);
 } else {
     fprintf( outfile, "ERROR: --- No Imported 1040 Form Data : no filename provided ---\n");
     exit(1);
 }

 char *f_sch_c_filename = GetTextLine( "FileNameSchC") ;
 printf("f_sch_c_filename: '%s'\n", f_sch_c_filename);

 if (strlen(f_sch_c_filename) != 0) {
     ImportReturnData( f_sch_c_filename, f_sch_c_imp_defs, f_sch_c_imp_defs_size);
     fprintf( outfile, "INFO: --- Imported Schedule C Data from file '%s' ---\n", f_sch_c_filename);
     fprintf( outfile, "INFO: f_sch_c.L7  --  %6.2f\n", f_sch_c.L7);
     fprintf( outfile, "INFO: f_sch_c.L31 --  %6.2f\n", f_sch_c.L31);
 } else {
     fprintf( outfile, "INFO: --- No Imported Schedule C Form Data : no filename provided ---\n");
 }

 // Set autocalculate mode only if both filenames are provided.
 int auto_calc = (strlen(f1040_filename) != 0) && (strlen(f_sch_c_filename) != 0);



 // Example:
 //  GetLineF( "L2", &L[2] );
 //  GetLineF( "L3", &L[3] );
 //  L[4] = L[2] - L[3];
 //  showline_wlabel( "L4", L[4] );

 // GetTextLine( "YourName:" );
 // GetTextLine( "YourSocSec#:" );
 fprintf(outfile, "YourName: %s %s\n", f1040i.Your1stName, f1040i.YourLastName);
 fprintf(outfile, "YourSocSec#: %s\n", f1040i.YourSocSec);

 // showline_wlabel( "INFO: Net QBI Income (this form L1-<row>-c)", f1040i.S1_3 - f1040i.S1_26);
 // showline_wlabel( "INFO: Form 1040 Line 11", f1040i.L11);
 // showline_wlabel( "INFO: Form 1040 Line 12c", f1040i.L12);
 // showline_wlabel( "INFO: Taxable Income Before QBI Deduction (this form L11)", f1040i.L11 - f1040i.L12);

 char * L1_row_names[] = { "i", "ii", "iii", "iv", "v" };
 char * L1_col_names[] = { "a", "b", "c" };

 double L1[sizeof(L1_row_names)/sizeof(char *)][sizeof(L1_col_names)/sizeof(char *)];
 char   L1_name[sizeof(L1_row_names)/sizeof(char *)][sizeof(L1_col_names)/sizeof(char *)][100];
 int row, col;

 // Set total income value to 0
 L[2] = 0.0;
 for (row = 0; row < sizeof(L1_row_names)/sizeof(char *); ++row) {
    double row_val;
    char *row_name="";
    for (col = 0; col < sizeof(L1_col_names)/sizeof(char *); ++col) {
        sprintf(L1_name[row][col], "L1_%s_%s%s", L1_row_names[row], L1_col_names[col], (col == 2) ? "" : ":");
        if (col == 0) { // Name business
            row_name = GetTextLineF(L1_name[row][col]) ;
        } else if (col == 1) {
            GetTextLineF(L1_name[row][col]) ;
        } else if (col == 2) {  // qualified business income (loss)
            GetLine(L1_name[row][col], &row_val );
            // If zero value was entered, and this row has a non-blank name, 
            // and both a 1040 and schedule C filename have been provided (auto_calc), calculate the total
            if ((row_val == 0.0) && (strlen(row_name) > 0) && auto_calc) {
                fprintf(outfile, "INFO: Auto calculating QBI profit/loss for L1_%s_%s\n", L1_row_names[row], L1_col_names[col]);
                L1[row][2] =  f_sch_c.L31 - (f1040i.S1_15 + f1040i.S1_16 + f1040i.S1_17);
                fprintf(outfile, "INFO: L1_%s_%s = %6.2f = f_sch_c.L31 - (f1040i.S1_15 + f1040i.S1_16 + f1040i.S1_17) = "
                    "%6.2f - (%6.2f + %6.2f + %6.2f)\n",  
                    L1_row_names[row], L1_col_names[col], 
                    L1[row][2], f_sch_c.L31, f1040i.S1_15, f1040i.S1_16, f1040i.S1_17);
            } else {
                L1[row][2] =  row_val;
            }
            showline_wlabelnz( L1_name[row][col], L1[row][2] );
        }
    }
    // Add to total income
    L[2] += L1[row][2];
 }

 GetLine( "L3", &L[3] );
 GetLine( "L6", &L[6] );
 GetLine( "L7", &L[7] );


 // double L11_prelim;
 // GetLine( "L11", &L11_prelim );
 // 
 // if ((L11_prelim == 0.0) && auto_calc) {
 //     fprintf(outfile, "INFO: Auto calculating QBI L11\n");
 //     L[11] = f1040i.L11 - f1040i.L12;
 //     fprintf(outfile, "INFO: %6.2f = f1040i.L11 - f1040i.L12 = %6.2f - %6.2f\n",  L[11], f1040i.L11, f1040i.L12);
 // } else {
 //    L[11] = L11_prelim;
 // }

 // Calculate line 11 from 1040 line 11 and line 12 values
 L[11] = f1040i.L11 - f1040i.L12;
 fprintf(outfile, "INFO: Line 11 = %6.2f = f1040i.L11 - f1040i.L12 = %6.2f - %6.2f\n",  L[11], f1040i.L11, f1040i.L12);

 GetLine( "L12", &L[12] );


 double qbi_percentage = 0.20;	// 20%		/* Updated/checked for tax-year 2024. */

 // Total qualified business income
 L[4] = NotLessThanZero(L[2] + L[3]);
 // Qualified business income component; mult by 20%
 L[5] = L[4] * qbi_percentage;
 // Total qualified REIT dividends and PTP income
 L[8] = NotLessThanZero(L[6] + L[7]);
 // REIT and PTP component
 L[9] = L[8] * qbi_percentage;

 L[10] = L[5] + L[9];
 L[13] = NotLessThanZero( L[11] - L[12] );
 L[14] = L[13] * qbi_percentage;
 L[15] = SmallerOf( L[10], L[14] );

 L[16] = L[2] + L[3];
 if (L[16] > 0.0)
    L[16] = 0.0;

 L[17] = L[6] + L[7];
 if (L[17] > 0.0)
    L[17] = 0.0;

 showline( 2 );
 ShowLineNonZero( 3 );
 showline( 4 );
 showline( 5 );

 ShowLineNonZero( 6 );
 ShowLineNonZero( 7 );
 showline       ( 8 );
 showline       ( 9 );

 showline( 10 );
 showline( 11 );
 ShowLineNonZero( 12 );
 showline( 13 );
 showline( 14 );
 showline( 15 );
 ShowLineNonZero( 16 );
 ShowLineNonZero( 17 );



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



} // namespace taxsolve_f8995_2024
} // namespace OpenTaxSolver2024

