#include "ots_2023_routines.h"
namespace OpenTaxSolver2023 {
namespace taxsolve_f8829_2023 {

#define FIRST_DIRECT_INDIRECT_LINE  9
#define LAST_DIRECT_INDIRECT_LINE  23
/************************************************************************/
/* taxsolve_f8829.c -                                                   */
/* Contributed by Rylan Luke, 1/2023					*/
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




/*----------------------------------------------------------------------------*/

//====== Schedule C Import ======
typedef struct FED_SCH_C_IMP_F8995_T {
        double L29;
        char *YourName;
        char *YourSocSec;
} FED_SCH_C_IMP_F8995_T, *P_FED_SCH_C_IMP_F8995;

FED_SCH_C_IMP_F8995_T f_sch_c;

// Mapping from label name to address of local data struct element; either double or char*
static FORM_IMPORT_DEF f_sch_c_imp_defs[] = {
        { "L29", &f_sch_c.L29, NULL },
        { "YourName:", NULL, &f_sch_c.YourName },
        { "YourSocSec#:", NULL, &f_sch_c.YourSocSec }
};

int f_sch_c_imp_defs_size = sizeof(f_sch_c_imp_defs)/sizeof(FORM_IMPORT_DEF);

int main( int argc, char *argv[] )
{
 int i, j, k;
 char word[6000], outfname[6000], *infname=0;
 time_t now;
 // Local line number variables for direct/indirect lines
 double La[LAST_DIRECT_INDIRECT_LINE+2]={0};
 double Lb[LAST_DIRECT_INDIRECT_LINE+2]={0};

 printf("Form 8829, 2023 - v%3.2f\n", thisversion);

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
 check_form_version( word, "Title: 2023 Form 8829" );

 /* ----- Accept form data and process the numbers.         ------ */
 /* ----- Place all your form-specific code below here .... ------ */

 // Example:
 //  GetLineF( "L2", &L[2] );
 //  GetLineF( "L3", &L[3] );
 //  L[4] = L[2] - L[3];
 //  showline_wlabel( "L4", L[4] );

 char *f_sch_c_filename = GetTextLine( "FileNameSchC") ;
 printf("f_sch_c_filename: '%s'\n", f_sch_c_filename);

 int autocalc = 0;

 if (strlen(f_sch_c_filename) != 0) {
     ImportReturnData( f_sch_c_filename, f_sch_c_imp_defs, f_sch_c_imp_defs_size);
     fprintf( outfile, "INFO: --- Imported Sch C Data from file '%s' ---\n", f_sch_c_filename);
     fprintf( outfile, "INFO: Sch C L29 --  %6.2f\n", f_sch_c.L29);
     fprintf( outfile, "INFO: Sch C YourName: -- %s\n", f_sch_c.YourName);
     fprintf( outfile, "INFO: Sch C YourSocSec#: -- %s\n", f_sch_c.YourSocSec);
     autocalc = 1;
 } else {
     fprintf( outfile, "INFO: --- No Imported Schedule C Form Data : no filename provided ---\n");
     autocalc = 0;
 }


 char *info;
 info = GetTextLine( "YourName:" );
 if ((strlen(info) == 0) && autocalc) {
    info = f_sch_c.YourName;
 }
 fprintf( outfile, "YourName: %s\n", info);

 // If blank, fill in from schedule C
 info = GetTextLine( "YourSocSec#:" );
 if ((strlen(info) == 0) && autocalc) {
    info = f_sch_c.YourSocSec;
 }
 fprintf( outfile, "YourSocSec#: %s\n", info);


 GetLineFnz( "L1", &L[1] );
 GetLineFnz( "L2", &L[2] );

 if (L[2] != 0.0)
    L[3] = L[1]/L[2] * 100.0;
 else
    L[3] = 0.0;
 ShowLineNonZero( 3 );

 GetLineFnz( "L4", &L[4] );
 GetLineFnz( "L5", &L[5] );
 if (L[5] != 0.0)
    L[6] = L[4]/L[5];
 else
    L[6] = 0.0;
 ShowLineNonZero( 6 );

 // L[4] is being non-zero is used to indicate that this is a
 // daycare business with non-exclusive use of the space.
 // For all other businesses, or for daycare with exclusive
 // use of the space, leave L[4] at 0.0
 L[7] = (L[4] != 0.0) ? (L[6] * L[3]) : L[3];

 
 ShowLineNonZero( 7 );

 GetLine( "L8", &L[8] );
 // If L8 == 0, and autocalc is set, fill in value directly
 // from Schedule C.
 if (L[8] == 0.0 && autocalc) {
    L[8] = f_sch_c.L29;
 }
 showline( 8 );

 // NOTE: "La" and "Lb" are local line arrays, used for the
 // direct and indirect expense categories in lines 9 through 23.
 // These arrays are dimensioned so that the actual line numbers are
 // used as indexes, so indexes 0 through 8 are not used.

 // Get the direct expenses from lines 9-12, and then lines 16-23
 GetLine("L9a",  &La[9]);
 GetLine("L10a", &La[10]);
 GetLine("L11a", &La[11]);
 GetLine("L16a", &La[16]);
 GetLine("L17a", &La[17]);
 GetLine("L18a", &La[18]);
 GetLine("L19a", &La[19]);
 GetLine("L20a", &La[20]);
 GetLine("L21a", &La[21]);
 GetLine("L22a", &La[22]);

 // Get the indirect expenses from lines 9-12, and then lines 16-23
 GetLine("L9b",  &Lb[9]);
 GetLine("L10b", &Lb[10]);
 GetLine("L11b", &Lb[11]);
 GetLine("L16b", &Lb[16]);
 GetLine("L17b", &Lb[17]);
 GetLine("L18b", &Lb[18]);
 GetLine("L19b", &Lb[19]);
 GetLine("L20b", &Lb[20]);
 GetLine("L21b", &Lb[21]);
 GetLine("L22b", &Lb[22]);
 // Get carryover from prior year
 GetLine("L25", &L[25]);

 // Sum the first block of direct expenses
 La[12] = La[9] + La[10] + La[11];

 // Sum the first block of indirect expenses
 Lb[12] = Lb[9] + Lb[10] + Lb[11];

 // Scale indirect expenses by business percentage
 L[13] = Lb[12] * (L[7] / 100.0);

 // Sum first block of direct and indirect expenses
 L[14] = La[12] + L[13];

 // Subtract first block sum from business income
 L[15] = L[8] - L[14];
 if (L[15] < 0.0)
    L[15] = 0.0;

 // Output all first block numbers
 showline_wlabelnz("L9a",  La[9]);
 showline_wlabelnz("L10a", La[10]);
 showline_wlabelnz("L11a", La[11]);
 showline_wlabelnz("L12a", La[12]);

 showline_wlabelnz("L9b",  Lb[9]);
 showline_wlabelnz("L10b", Lb[10]);
 showline_wlabelnz("L11b", Lb[11]);
 showline_wlabelnz("L12b", Lb[12]);
 showline_wlabelnz("L13", L[13]);

 showline( 14 );
 showline( 15 );

 // Sum the second block of direct expenses
 La[23] = La[16] + La[17] + La[18] + La[19] + La[20] + La[21] + La[22];
 
 // Sum the second block of indirect expenses
 Lb[23] = Lb[16] + Lb[17] + Lb[18] + Lb[19] + Lb[20] + Lb[21] + Lb[22];

 // Scale indirect expenses by business percentage
 L[24] = Lb[23] * (L[7] / 100.0);

 L[26] = La[23] + L[24] + L[25];
 L[27] = SmallerOf( L[15], L[26] );
 L[28] = L[15] - L[27];

 // Output all second block numbers
 // Direct expenses
 showline_wlabelnz("L16a", La[16]);
 showline_wlabelnz("L17a", La[17]);
 showline_wlabelnz("L18a", La[18]);
 showline_wlabelnz("L19a", La[19]);
 showline_wlabelnz("L20a", La[20]);
 showline_wlabelnz("L21a", La[21]);
 showline_wlabelnz("L22a", La[22]);
 showline_wlabelnz("L23a", La[23]);

 // Indirect expenses
 showline_wlabelnz("L16b", Lb[16]);
 showline_wlabelnz("L17b", Lb[17]);
 showline_wlabelnz("L18b", Lb[18]);
 showline_wlabelnz("L19b", Lb[19]);
 showline_wlabelnz("L20b", Lb[20]);
 showline_wlabelnz("L21b", Lb[21]);
 showline_wlabelnz("L22b", Lb[22]);
 showline_wlabelnz("L23b", Lb[23]);

 showline_wlabelnz("L24", L[24]);
 showline_wlabelnz("L25", L[25]);

 ShowLineNonZero( 26 );
 showline( 27 );
 showline( 28 );

 // Excess casualty loss
 GetLineFnz( "L29", &L[29] );

 // Need to get the rest of the form's data, as L[30] is a copy of L[42]
 GetLine( "L31", &L[31] );
 GetLine( "L35", &L[35] );
 GetLine( "L37", &L[37] );
 GetLine( "L38", &L[38] );
 GetLine( "L41", &L[41] );

 // Calculate depreciation in Part III
 L[39] = L[37] - L[38];
 L[40] = (L[7] / 100.0) * L[39];
 L[42] = L[40] * (L[41] / 100.0);

 // Copy depreciation allowable back to line 30 in Part II
 L[30] = L[42];

 // Now, continue where we left off, on Line 32 in part II
 L[32] = L[29] + L[30] + L[31];
 L[33] = SmallerOf(L[28], L[32] );
 L[34] = L[14] + L[27] + L[33];
 L[36] = L[34] - L[35];

 // Part IV
 // Operating expenses to next year
 L[43] = NotLessThanZero(L[26] - L[27]);
 // Carryover of excess casualty losses and depreciation to next year
 L[44] = NotLessThanZero(L[32] - L[33]);

 ShowLineNonZero( 30 );
 ShowLineNonZero( 31 );
 ShowLineNonZero( 32 );
 showline       ( 33 );
 ShowLineNonZero( 34 );
 ShowLineNonZero( 35 );
 ShowLineNonZero( 36 );
 ShowLineNonZero( 37 );
 ShowLineNonZero( 38 );
 ShowLineNonZero( 39 );
 ShowLineNonZero( 40 );
 ShowLineNonZero( 41 );
 showline       ( 42 );
 showline       ( 43 );
 showline       ( 44 );

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

#undef FIRST_DIRECT_INDIRECT_LINE
#undef LAST_DIRECT_INDIRECT_LINE

} // namespace taxsolve_f8829_2023
} // namespace OpenTaxSolver2023

