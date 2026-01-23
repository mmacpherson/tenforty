#include "ots_2022_routines.h"
namespace OpenTaxSolver2022 {
namespace taxsolve_HSA_f8889 {

#define Yes 1
#define No  0
/************************************************************************/
/* TaxSolve_HSA_f8889.c - 						*/
/* Copyright (C)  2022 - A. Roberts					*/
/* 									*/
/* Compile:								*/
/*  cc taxsolve_HSA_f8889.c -o ../bin/taxsolve_HSA_f8889		*/
/*									*/
/* Documentation & Updates:						*/
/*        http://opentaxsolver.sourceforge.net/				*/
/*									*/
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
/* Updated for 2022 tax year:						*/
/************************************************************************/

float thisversion=20.00;



/*----------------------------------------------------------------------------*/

int main( int argc, char *argv[] )
{
 int i, j, k;
 char word[4000], outfname[4000], *answ, *infname=0;
 time_t now;
 double L14a=0.0, L14b=0.0, L14c=0.0, L17b=0.0;

 printf("Form 8889 HSA, 2022 - v%3.2f\n", thisversion );

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
 /* Expect  Sched C lines, something like:
	Title:  f8889 Return
	L2		{Returns and Allowances}
	. . .
 */

 /* Accept Form's "Title" line, and put out with date-stamp for your records. */
 read_line( infile, word );
 now = time(0);
 fprintf(outfile,"\n%s,	 v%2.2f, %s\n", word, thisversion, ctime( &now ));
 check_form_version( word, "Title: 8889 HSA Form - 2022" );

 GetTextLineF( "YourName:" );
 GetTextLineF( "YourSocSec#:" );
 
 writeout_line = 0;     /* Suppress GetLineF's from immediately writing to outfile. */
 answ = GetTextLineF( "L1:" );
 next_word( answ, word, " \t;" );
 if (strcasecmp( word, "Self-Only" ) == 0)
  fprintf(outfile,"CkSelf-Only: X\n");
 else
  fprintf(outfile,"CkFamily: X\n");
 writeout_line = 1;

 GetLineF( "L2", &L[2] );
 GetLineF( "L3", &L[3] );
 GetLineF( "L4", &L[4] );
 L[5] = NotLessThanZero( L[3] - L[4] );
 showline( 5 );

 GetLine( "L6", &L[6] );
 if (!value_was_detected)
  L[6] = L[5];			/* If user did not supply a value for L6, then L6 must be set to L5. */
 showline( 6 );

 GetLineF( "L7", &L[7] );
 L[8] = L[6] + L[7];
 showline( 8 );
 GetLineF( "L9", &L[9] );
 GetLineF( "L10", &L[10] );
 L[11] = L[9] + L[10];
 showline( 11 );
 L[12] = NotLessThanZero( L[8] - L[11] );
 showline( 12 );
 L[13] = SmallerOf( L[2], L[12] );
 showline_wmsg( 13, "HSA Deduction.  Enter this on Sched-1 Part II, Line 13 on your 1040 Form." );
 if (L[2] > L[13])
  fprintf(outfile,"Caution: Since L2 > L13, you may have to pay additional tax. See instructions.\n\n");

 GetLineF( "L14a", &L14a );
 GetLineF( "L14b", &L14b );
 L14c = L14a - L14b;
 showline_wlabel( "L14c", L14c ); 
 GetLineF( "L15", &L[15] );
 L[16] = NotLessThanZero( L14c - L[15] );
 showline_wmsg( 16, "Taxable HSA distributions. Include this on Sched-1 Line 8e on your 1040 Form." );

 writeout_line = 0;     /* Suppress GetLineF's from immediately writing to outfile. */
 answ = GetTextLineF( "L17a:" );
 next_word( answ, word, " \t;" );
 if (toupper( word[0] ) == 'Y')
  fprintf(outfile,"Ck17a: X\n");
 writeout_line = 1;

 if (toupper( word[0] ) != 'Y')
  {
   L17b = 0.20 * L[16];
   showline_wlabel( "L17b", L17b );
  }

 GetLineF( "L18", &L[18] );
 GetLineF( "L19", &L[19] );
 L[20] = L[18] + L[19];
 showline_wmsg( 20, "Total income. Include this on Sched-1 Line 8z on your 1040 Form." );

 L[21] = 0.10 * L[20];
 showline_wmsg( 21, "Additional tax. Include this on Sched-2 Line 17d on your 1040 Form." );

 fclose(infile);
 grab_any_pdf_markups( infname, outfile );
 fclose(outfile);

 printf("\nListing results from file: %s\n\n", outfname);
 Display_File( outfname );

 return 0;
}

#undef Yes
#undef No

} // namespace taxsolve_HSA_f8889
} // namespace OpenTaxSolver2022

