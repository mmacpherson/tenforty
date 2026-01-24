#include "ots_2024_routines.h"
namespace OpenTaxSolver2024 {
namespace taxsolve_f8812_2024 {

#define SINGLE                  1
#define MARRIED_FILING_JOINTLY  2
#define MARRIED_FILING_SEPARAT  3
#define HEAD_OF_HOUSEHOLD       4
#define WIDOW                   5
#define Yes 1
#define No  0
/************************************************************************/
/* taxsolve_f8812.c -                                                   */
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



int status;


/*----------------------------------------------------------------------------*/

int main( int argc, char *argv[] )
{
 int i, j, k;
 char word[6000], outfname[6000], *infname=0;
 time_t now;
 char *name=0, *socsec;
 double L2a=0.0, L2b=0.0, L2c=0.0, L2d=0.0, L16a=0.0, L16b=0.0, L18a=0.0, L18b=0.0;

 printf("Form 8812, 2024 - v%3.2f\n", thisversion );

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
 for (j=0; j<MAX_LINES; j++) { L[j] = 0.0; }

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
 check_form_version( word, "Title: Form 8812 - 2024" );

 /* ----- Accept form data and process the numbers.         ------ */
 /* ----- Place all your form-specific code below here .... ------ */

 // Example:
 //  GetLineF( "L2", &L[2] );
 //  GetLineF( "L3", &L[3] );
 //  L[4] = L[2] - L[3];
 //  showline_wlabel( "L4", L[4] );

 get_parameter( infile, 's', word, "Status" );  /* Single, Married/joint, Married/sep, Head house, Widow(er) */
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


 GetLineF( "L1", &L[1] );	// Amount from line 11 of your Form 1040
 GetLineF( "L2a", &L2a );	// Income from Puerto Rico that you excluded
 GetLineF( "L2b", &L2b );	// Amounts from lines 45 and 50 of your Form 2555
 GetLineF( "L2c", &L2c );	// Amount from line 15 of your Form 4563
 L2d = L2a + L2b + L2c;
 showline_wlabel( "L2d", L2d );

 L[3]  = L[1] + L2d;
 showline( 3 );

 GetLine( "L4", &L[4] ); 	// Number of qualifying children under age 17, who have Soc.Sec. numbers.
 printf("L4 = %d\n", (int)L[4] );
 fprintf(outfile,"L4 = %d\n", (int)L[4] );

 L[5] = L[4] * 2000.0;		// Checked for tax-year 2024.
 showline( 5 );

 GetLine( "L6", &L[6] );	// Number of other dependents, including any qualifying children
 printf("L6 = %d\n", (int)L[6] );
 fprintf(outfile,"L6 = %d\n", (int)L[6] );

 L[7] = L[6] * 500.0;		// Checked for tax-year 2024.
 showline( 7 );

 L[8] = L[5] + L[7];
 showline( 8 );

 switch (status)
  {				// Checked for tax-year 2024.
   case MARRIED_FILING_JOINTLY: 	L[9] = 400000.0;  break;
   default:  				L[9] = 200000.0;
  }
 showline( 9 );

 L[10] = NotLessThanZero( L[3] - L[9] );
 fprintf(outfile,"L[10] = %g\n", L[10] );

 if (L[10] > 0.0)
   L[10] = (double)((int)((L[10]-0.01) / 1000.0) + 1) * 1000.0;
 showline( 10 );

 L[11] = L[10] * 0.05;		// Checked for tax-year 2024.
 showline( 11 );

 GetLine( "L13", &L[13] );	// Amount from Credit Limit Worksheet A

 GetLine( "Amnt19", &L[19] );	// Amount on Form 1040, line 19.

 GetLine( "L18a", &L18a );	// Earned income (see instructions) }
 GetLine( "L18b", &L18b ); 	// Nontaxable combat pay (see instructions) }

 GetLine( "L21", &L[21] );	// Withheld Soc.Sec., Medicare, & Additional Medicare taxes
 GetLine( "L22", &L[22] );	// Total of amounts from Sched-1 (Form 1040), line 15; Sched-2  lines 5+6, 13+22

 GetLine( "L24", &L[24] );	// Total of amounts from Form 1040, line 27, and Sched-3 line 11. }


 if (L[8] <= L[11])
  {
   fprintf(outfile," Since, L8 is not more than L11     (%6.2f < %6.2f)\n", L[8], L[11] );
   fprintf(outfile," You cannot take the child tax credit, credit for other dependents, or additional child tax credit.\n");
   // Skip to L28
  }
 else
  { /*A*/
   L[12] = L[8] - L[11];
   showline( 12 );
   showline( 13 );

   L[14] = SmallerOf( L[12], L[13] );
   showline( 14 );

   if (L[12] > L[14])
    {
     fprintf(outfile,"Since L12 > L14,\n");
     fprintf(outfile,"You may be able to take the additional child tax credit on Form 1040, , line 28.\n");
    }

   // Part II-A Additional Child Tax Credit for All Filers
   // Caution: If you file Form 2555, you cannot claim the additional child tax credit.

   L16a  = L[12] - L[14];
   showline_wlabel( "L16a", L16a );

   if ((L16a <= 0.0) || (L[4] == 0.0))
    { /*B*/
     fprintf(outfile,"Since L16a is less-than or equal 0, or L4 is zero,\n");
     fprintf(outfile,"You cannot take the additional child tax credit.\n");
     L[27] = 0.0;
    } /*B*/
   else
    { /*C*/
     L16b = 1700.0 * L[4];		// Checked/updated for tax-year 2024.
     showline_wlabel( "L16b", L16b );

     if (L16b > 0.0)
      { /*D*/
       L[17] = SmallerOf( L16a, L16b );
       showline( 17 );

       showline_wlabel( "L18a", L18a );
       showline_wlabel( "L18b", L18b );

       if (L18a > 2500.0)
        { /*E*/
	 L[19] = L18a - 2500.0;		// Checked/updated for tax-year 2024.
	 showline( 19 );
	 L[20] = L[19] * 0.15;
	 showline( 20 );

	 if (L16b <= 5100.0)		// Checked/updated for tax-year 2024.
	  { /*F*/
	   L[27] = SmallerOf( L[17], L[20] );
	  } /*F*/
	 else
	  { /*G*/
	   if (L[20] >= L[17])
	    { /*H*/
	      L[27] = L[17];
	    } /*H*/
	   else
	    { /*i*/
	     showline( 21 );
	     showline( 22 );
	     L[23] = L[21] + L[22];
	     showline( 23 );
	     showline( 24 );
	     L[25] = NotLessThanZero( L[23] - L[24] );
	     showline( 25 );
	     L[26] = LargerOf( L[20], L[25] );
	     showline( 26 );
	     L[27] = SmallerOf( L[17], L[26] );
	   } /*i*/
	  } /*G*/
        } /*E*/
       else
	{
	 fprintf(outfile,"Note that L18a <= 2,500/\n");
	 L[20] = 0.0;
	 showline( 20 );
	}
      } /*D*/
    } /*C*/
  } /*A*/


 // Part II-C Additional Child Tax Credit
 if (L[27] > 0.0)								// Checked for tax-year 2024.
  showline_wmsg( 27, "This is your additional child tax credit. Enter this amount on Form 1040, line 28." );


 name = GetTextLine( "YourName:" );
 if (name != 0)
  fprintf( outfile, "YourName: %s\n", name );

 socsec = GetTextLine( "SocSec:" );
 if (socsec!= 0)
  fprintf( outfile, "SocSec#: %s\n", socsec );


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

#undef SINGLE
#undef MARRIED_FILING_JOINTLY
#undef MARRIED_FILING_SEPARAT
#undef HEAD_OF_HOUSEHOLD
#undef WIDOW
#undef Yes
#undef No

} // namespace taxsolve_f8812_2024
} // namespace OpenTaxSolver2024
