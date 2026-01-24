#include "ots_2021_routines.h"
namespace OpenTaxSolver2021 {
namespace taxsolve_f8960_2021 {

#define SINGLE 		        1
#define MARRIED_FILING_JOINTLY 2
#define MARRIED_FILING_SEPARAT 3
#define HEAD_OF_HOUSEHOLD       4
#define WIDOW		        5
#define Yes 1
#define No  0
/************************************************************************/
/* TaxSolve_Form_8960.c - 2021						*/
/*  User contributed.							*/
/************************************************************************/

float thisversion=2.01;



/*----------------------------------------------------------------------------*/

int main( int argc, char *argv[] )
{
 int i, j, k;
 char word[6000], outfname[4000], *infname=0;
 time_t now;

 double L4a = 0.0,  L4b = 0.0, L4c = 0.0;
 double L5a = 0.0, L5b = 0.0, L5c = 0.0, L5d = 0.0;
 double L9a = 0.0, L9b = 0.0, L9c = 0.0, L9d = 0.0;
 double L18a = 0.0,  L18b = 0.0, L18c = 0.0;
 double L19a = 0.0,  L19b = 0.0, L19c = 0.0;

 int status, individual = No;

 printf("Form 8960, 2021 - v%3.2f\n", thisversion);

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
 check_form_version( word, "Title: 2021 Form 8960" );


 /* ----- Accept form data and process the numbers.         ------ */
 /* ----- Place all your form-specific code below here .... ------ */

 // fprintf(outfile,"\n--- THIS IS PRELIMINARY USER-CONTRIBUTED FORM ---\n");
 // fprintf(outfile,"--- NOT YET FULLY UPDATED FOR 2021. ---\n\n");
 // MarkupPDF( 1, 240, 40, 17, 1.0, 0, 0 ) NotReady "This program is NOT updated for 2021."
 // add_pdf_markup( "NotReady", 1, 240, 40, 17, 1, 1.0, 0, 0, "\"This program is NOT ready for 2021.\"" );


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

 get_parameter( infile, 's', word, "Entity" );
 get_parameter( infile, 'l', word, "Entity?");
 if (strncasecmp(word,"Individual",3)==0) individual = Yes;
 fprintf(outfile,"Entity = %s (%d)\n", word, individual);
     
 get_parameter( infile, 's', word, "Sec6013g" );
 get_parameter( infile, 'l', word, "Sec6013g?");
 if (strncasecmp(word,"Yes",1)==0)
   fprintf(outfile, "CkSec6013g X\n");
  
 get_parameter( infile, 's', word, "Sec6013h" );
 get_parameter( infile, 'l', word, "Sec6013h?");
 if (strncasecmp(word,"Yes",1)==0)
   fprintf(outfile, "CkSec6013h X\n");
  
 get_parameter( infile, 's', word, "Sec1141_10g" );
 get_parameter( infile, 'l', word, "Sec1141_10g?");
 if (strncasecmp(word,"Yes",1)==0)
   fprintf(outfile, "CkSec1141_10g X\n");  

 GetLineF( "L1", &L[1] );
 GetLineF( "L2", &L[2] );
 GetLineF( "L3", &L[3] );
 GetLineF( "L4a", &L4a );
 GetLineF( "L4b", &L4b );
 L4c = L4a + L4b;
 showline_wlabel( "L4c", L4c );
 GetLineF( "L5a", &L5a );
 GetLineF( "L5b", &L5b );
 GetLineF( "L5c", &L5c );
 L5d = L5a + L5b + L5c;
 showline_wlabel( "L5d", L5d );
 GetLineF( "L6", &L[6] );
 GetLineF( "L7", &L[7] );
 L[8] = L[1] + L[2] + L[3] + L4c + L5d + L[6] + L[7];
 showline( 8 );
 GetLineF( "L9a", &L9a );
 GetLineF( "L9b", &L9b );
 GetLineF( "L9c", &L9c );
 L9d = L9a + L9b + L9c;
 showline_wlabel( "L9d", L9d );
 GetLineF( "L10", &L[10] );
 L[11] = L9d + L[10];
 showline( 11 );
 L[12] = NotLessThanZero(L[8] - L[11]);
 showline( 12 );

 if(individual == Yes){
   GetLineF( "L13", &L[13] );
   if( status == MARRIED_FILING_JOINTLY)
	  L[14] = 250000.00;
   else if(status == WIDOW)
	  L[14] = 250000.00;
   else if(status == MARRIED_FILING_SEPARAT)
	  L[14] = 125000.00;
   else if((status == SINGLE) || (status == HEAD_OF_HOUSEHOLD))
	  L[14] = 200000.00;
   showline( 14 );
   L[15] = NotLessThanZero(L[13] - L[14]);
   showline( 15 );
   L[16] = SmallerOf(L[12], L[15]);
   showline( 16 );
   L[17] = L[16] * 0.038;
   showline_wmsg( 17, "Include on your tax return see instructions)" );
 }
 else{
   GetLine( "L13", &L[13] );	/* consume unneeded line from input file */
   L18a = L[12];
   showline_wlabel( "L18a", L18a );
   GetLineF( "L18b", &L18b );
   L18c = NotLessThanZero( L18a - L18b );
   showline_wlabel( "L18c", L18c );
   GetLineF( "L19a", &L19a );
   GetLineF( "L19b", &L19b );
   L19c = NotLessThanZero( L19a - L19b );
   showline_wlabel( "L19c", L19c );
   L[20] = SmallerOf( L18c, L19c );
   L[21] = L[20] * 0.038;
   showline_wmsg( 21, "Include on your tax return see instructions)" );
 }  

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

} // namespace taxsolve_f8960_2021
} // namespace OpenTaxSolver2021

