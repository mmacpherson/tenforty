#include "ots_2021_routines.h"
namespace OpenTaxSolver2021 {
namespace taxsolve_US_1040_Sched_SE_2021 {

#define Yes 1
#define No  0
/************************************************************************
  TaxSolve_Form_US_1040_Sched_SE.c - Self Employment Tax Form

  NOTE: Only Part-I is implemented.
	Parts II and III are NOT presently implemented.

 Documentation & Updates:
        http://opentaxsolver.sourceforge.net/

 GNU Public License - GPL:
 This program is free software; you can redistribute it and/or
 modify it under the terms of the GNU General Public License as
 published by the Free Software Foundation; either version 2 of the
 License, or (at your option) any later version.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
 02111-1307 USA

 Provided by Steve Wiswell, March 16, 2021.

 ************************************************************************/

float thisversion=2.01;



double L4a=0.0, L4c=0.0;
double L5a=0.0, L5b=0.0; 			/* Church employee income */
double L8a=0.0, L8b=0.0, L8c=0.0, L8d=0.0; 	/* Wages & Tips */

/*----------------------------------------------------------------------------*/

int main( int argc, char *argv[] )
{
 int i, j, k;
 char word[8000], outfname[8000], *infname=0;
 time_t now;

 printf("US 1040 Schedule SE, 2021 - v%1.0f\n", thisversion);

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
        Title:  1040 Schedule SE - 2021 Return
        L2              {Net Profit/Loss}
        . . .
 */


 /* Accept Form's "Title" line, and put out with date-stamp for your records. */
 read_line( infile, word );
 now = time(0);
 fprintf(outfile,"\n%s,  v%2.2f, %s\n", word, thisversion, ctime( &now ));
 check_form_version( word, "Title:  1040 Schedule SE - 2021" );


 /* ----- Accept form data and process the numbers.         ------ */
 /* ----- Place all your form-specific code below here .... ------ */

 GetTextLineF( "YourName:" );
 GetTextLineF( "YourSocSec#:" );

 GetLine( "L2", &L[2] );	/* Net Profit/Loss */
 GetLine( "L5a", &L5a );	/* Church employee income from Form W-2 */
 L[7] = 142800.0;           	/* Fixed value for tax year 2021 */			/* Updated for 2021. */
 GetLine( "L8a", &L8a );	/* Wages & Tips */
 GetLine( "L8b", &L8b );	/* Unreported tips from Form 4137, line 10 */
 GetLine( "L8c", &L8c );	/* Wages from Form 8919, line 10 */

 /* -- Compute the tax form  -- */
 showline(2);
 L4a = L[2] * 0.9235;									/* Updated for 2021. */
 showline_wlabel( "L4a", L4a );
 L4c = NotLessThanZero( L4a );
 showline_wlabel( "L4c", L4c );
 showline_wlabel( "L5a", L5a );
 L5b = NotLessThanZero( L5a * 0.9235 );
 showline_wlabel( "L5b", L5b );
 L[6] = L4c + L5b;
 showline(6);
 showline_wlabel("L8a", L8a);
 showline_wlabel("L8b", L8b);
 showline_wlabel("L8c", L8c);
 L8d = L8a + L8b + L8c;
 showline_wlabel("L8d", L8d);
 L[9] = NotLessThanZero( L[7] - L8d );
 showline(9);
 L[10] = 0.124 * SmallerOf( L[6], L[9]);
 showline(10);
 L[11] = L[6] * 0.029;									/* Updated for 2021. */
 showline(11);
 L[12] = L[10] + L[11];
 showline_wmsg( 12, "Also enter this number on Schedule-2, line 4." );
 L[13] = L[12] * 0.5;
 showline_wmsg( 13, "Also enter this number on Schedule-1, line 15." );

 L[14] = 5880.0;									/* Updated for 2021. */

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

} // namespace taxsolve_US_1040_Sched_SE_2021
} // namespace OpenTaxSolver2021
