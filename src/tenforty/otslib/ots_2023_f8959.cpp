#include "ots_2023_routines.h"
namespace OpenTaxSolver2023 {
namespace taxsolve_f8959_2023 {

#define SINGLE 		        1
#define MARRIED_FILING_JOINTLY  2
#define MARRIED_FILING_SEPARAT  3
#define HEAD_OF_HOUSEHOLD       4
#define WIDOW		        5
#define Yes 1
#define No  0
/************************************************************************/
/* TaxSolve_Form_8959.c - 2023						*/
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

float thisversion=3.00;



int status;

/*----------------------------------------------------------------------------*/

int main( int argc, char *argv[] )
{
 int i, j, k;
 char word[6000], outfname[6000], *infname=0;
 time_t now;
/*
 double L[25];
*/
 printf("Form 8959, 2023 - v%3.2f\n", thisversion);

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
 check_form_version( word, "Title: 2023 Form 8959" );

 /* ----- Accept form data and process the numbers.         ------ */
 /* ----- Place all your form-specific code below here .... ------ */

 // fprintf(outfile,"\n--- THIS IS PRELIMINARY USER-CONTRIBUTED FORM ---\n");
 // fprintf(outfile,"--- NOT YET FULLY UPDATED FOR 2023. ---\n\n");

 // MarkupPDF( 1, 240, 40, 17, 1.0, 0, 0 ) NotReady "This program is NOT updated for 2023."
 // add_pdf_markup( "NotReady", 1, 240, 40, 17, 1, 1.0, 0, 0, "\"This program is NOT ready for 2023.\"" );


 // Example:
 //  GetLineF( "L2", &L[2] );
 //  GetLineF( "L3", &L[3] );
 //  L[4] = L[2] - L[3];
 //  showline_wlabel( "L4", L[4] );

 GetTextLineF( "YourName:" );
 GetTextLineF( "YourSocSec#:" );

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
 L[4] = L[1] + L[2] + L[3];
 showline( 4 );
 if(status == MARRIED_FILING_JOINTLY)
	L[5] = 250000.00;
 else if(status == MARRIED_FILING_SEPARAT)
	L[5] = 125000.00;
 else if((status == SINGLE) || (status == HEAD_OF_HOUSEHOLD) || (status == WIDOW))
	L[5] = 200000.00;
 showline( 5 );
 L[6] = NotLessThanZero( L[4] - L[5]);
 showline( 6 );
 L[7] = L[6] * 0.009;
 showline( 7 );
 GetLineF( "L8", &L[8] );
 if(status == MARRIED_FILING_JOINTLY)
	L[9] = 250000.00;
 else if(status == MARRIED_FILING_SEPARAT)
	L[9] = 125000.00;
 else if((status == SINGLE) || (status == HEAD_OF_HOUSEHOLD) || (status == WIDOW))
	L[9] = 200000.00;
 showline( 9 );
 L[10] = L[4];
 showline( 10 );
 L[11] = NotLessThanZero( L[9] - L[10]);
 showline( 11 );
 L[12] = NotLessThanZero( L[8] - L[11]);
 showline( 12 );
 L[13] = L[12] * 0.009;
 showline( 13 );
 GetLineF( "L14", &L[14] );
 if(status == MARRIED_FILING_JOINTLY)
	L[15] = 250000.00;
 else if(status == MARRIED_FILING_SEPARAT)
	L[15] = 125000.00;
 else if((status == SINGLE) || (status == HEAD_OF_HOUSEHOLD) || (status == WIDOW))
	L[15] = 200000.00;
 showline( 15 );
 L[16] = NotLessThanZero( L[14] - L[15]);
 showline( 16 );
 L[17] = L[16] * 0.009;
 showline( 17 );
 L[18] = L[7] + L[13] + L[17];
 showline_wmsg( 18, "include this amount on Schedule 2 (Form 1040), line 11 \
(Form 1040-PR or 1040-SS filers, see instructions)" );
 GetLineF( "L19", &L[19] );
 L[20] = L[1];
 showline( 20 );
 L[21] = L[20] * 0.0145;
 showline_wmsg( 21, "This is your regular Medicare tax \
withholding on Medicare wages" );
 L[22] = NotLessThanZero( L[19] - L[21]);
 showline_wmsg( 22, "This is your Additional Medicare Tax \
withholding on Medicare wages" );
 GetLineF( "L23", &L[23] );
 L[24] = L[22] + L[23];
 showline_wmsg( 24, "include this amount with \
federal income tax withholding on Form 1040, 1040-SR, or 1040-NR, line 25c (Form 1040-PR or \
1040-SS filers, see instructions)" );  

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

#undef SINGLE
#undef MARRIED_FILING_JOINTLY
#undef MARRIED_FILING_SEPARAT
#undef HEAD_OF_HOUSEHOLD
#undef WIDOW
#undef Yes
#undef No

} // namespace taxsolve_f8959_2023
} // namespace OpenTaxSolver2023

