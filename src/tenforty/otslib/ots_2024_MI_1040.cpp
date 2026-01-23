#include "ots_2024_routines.h"
namespace OpenTaxSolver2024 {
namespace taxsolve_MI_1040_2024 {

#define SINGLE                  1
#define MARRIED_FILING_JOINTLY  2
#define MARRIED_FILING_SEPARAT  3
#define Yes 1
#define No  0
/************************************************************************
  TaxSolve_MI_1040_2024.c - Michigan 2024 MI-1040 State Taxes.
   Copyright (C) 2025 - C. Kindman

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
  02111-1307 USA.

  2-4-2025	http://opentaxsolver.sourceforge.com/

 ************************************************************************/



float thisversion=1.01;


double flat_tax_rate = 0.0425;		/* Updated for 2024. */


/* ------------------- Main -------------------------------------- */
int main( int argc, char *argv[] )
{
 int j, jj, k, status;
 char word[1000], *infname=0, outfname[1000], *socsec, socsectmp[500], labelx[1000];
 time_t now;
 int residency=0, L9a=0, L9b=0, L9c=0, L9d=0, L9e=0;
 double L9aa=0.0, L9bb=0.0, L9cc=0.0, L9dd=0.0, L9ee=0.0, L18a=0.0, L19a=0.0, L27a=0.0;
 double interest=0.0, penalty=0.0;

 /*-----------------------------------------*/
 /* --- Decode any command line options. -- */
 /*-----------------------------------------*/
 printf("MI-1040 2024 - v%3.2f\n", thisversion);
 jj = 1;  k=1;
 while (jj < argc)
 {
  if (strcmp(argv[jj],"-verbose")==0)  { verbose = 1; }
  else
  if (strcmp(argv[jj],"-round_to_whole_dollars")==0)  { round_to_whole_dollars = 1; }
  else
  if (k==1)
   {
    infname = strdup(argv[jj]);
    infile = fopen(argv[jj],"r");
    if (infile==0)
     {
	printf("ERROR: Parameter file '%s' could not be opened.\n", argv[jj]); 
	fprintf(outfile,"ERROR: Parameter file '%s' could not be opened.\n", argv[jj]); 
	exit(1);
     }
    k = 2;
    /* Base name of output file on input file. */
    strcpy(outfname,argv[jj]);
    j = strlen(outfname)-1;
    while ((j>=0) && (outfname[j]!='.')) j--;
    if (j<0) strcat(outfname,"_out.txt"); else strcpy(&(outfname[j]),"_out.txt");
    outfile = fopen(outfname,"w");
    if (outfile==0)
     {
	printf("ERROR: Output file '%s' could not be opened.\n", outfname); 
	fprintf(outfile,"ERROR: Output file '%s' could not be opened.\n", outfname); 
	exit(1);
     }
    printf("Writing results to file:  %s\n", outfname);
   }
  else
   {
	printf("Unknown command-line parameter '%s'\n", argv[jj]); 
	fprintf(outfile,"Unknown command-line parameter '%s'\n", argv[jj]); 
	exit(1);
   }
  jj++;
 }

 if (infile==0)
  {
	printf("Error: No input file on command line.\n"); 
	fprintf(outfile,"Error: No input file on command line.\n"); 
	exit(1);
  }
 

 /*--------------------------*/
 /* ---- Get Input Data ---- */
 /*--------------------------*/

 /* Pre-initialize all lines to zeros. */
 for (j = 0; j < MAX_LINES; j++) { L[j] = 0.0; }

 /* Accept parameters from input file. */
 /* Expect  D-400 lines, something like:
	Title:  MI-1040 1999 Return
	L6	34900.0  {Wages}
 */

 /* Accept Form's "Title" line, and put out with date-stamp for your records. */
 read_line( infile, word );
 now = time(0);
 fprintf(outfile,"\n%s,	 v%2.2f, %s\n", word, thisversion, ctime( &now ));
 check_form_version( word, "Title:  MI-1040" );

 get_parameter( infile, 's', word, "Status"); /* 1=single, 2=married/joint, 3=married/separate/ */
 get_parameter( infile, 'l', word, "Status ?");
 if ((word[0]>'0') && (word[0]<'6')) status = word[0]-48; else
 if (strncasecmp(word,"Single",4)==0) status = SINGLE; else
 if (strncasecmp(word,"Married/Joint",13)==0) status = MARRIED_FILING_JOINTLY; else
 if (strncasecmp(word,"Married/Sep",11)==0) status = MARRIED_FILING_SEPARAT;
 else
  { 
   printf("Error: unrecognized status '%s'. Must be: Single, Married/joint, Married/sep/\nExiting.\n", word); 
   fprintf(outfile,"Error: unrecognized status '%s'. Must be: Single, Married/joint, Married/sep.\nExiting.\n", word); 
   exit(1); 
  }
 fprintf(outfile,"Status = %s (%d)\n", word, status);


 check_if_yes( "CkFarmFish" );

 // ResidencyStatus Resident { (answer: Resident, Nonresident, PartYear ) }
 GetLineString( "ResidencyStatus", word );
 if (strcasecmp( word, "Resident" ) == 0)
  residency = 1;
 else
 if (strcasecmp( word, "Nonresident" ) == 0)
  residency = 2;
 else
 if (strcasecmp( word, "PartYear" ) == 0)
  residency = 3;

printf("Residency = %d, word = '%s'\n", residency, word );

 GetInteger( "L9a", &L9a );	/* Number of exemptions. */
printf("L9a = %d\n", L9a );
 GetInteger( "L9b", &L9b );	/* */
printf("L9b = %d\n", L9b );
 GetInteger( "L9c", &L9c );	/* */
printf("L9c = %d\n", L9c );
 GetInteger( "L9d", &L9d );	/* */
 GetInteger( "L9e", &L9e );	/* Claimed as dependent */
 
 GetLine( "L10", &L[10] );	/* Adjusted Gross Income from your U.S. Form 1040 */  
 GetLine( "L11", &L[11] );	/* Additions from Schedule 1, line 9. */  
 GetLine( "L13", &L[13] );	/* Subtractions from Schedule 1, line 31.*/  
 GetLine( "L15", &L[15] );	/* Exemption allowance. Enter amount from line 9f or Schedule NR, line 19. */

 GetLine( "L18a", &L18a );
 GetLine( "L18b", &L[18] );
 GetLine( "L19a", &L19a );
 GetLine( "L19b", &L[19] );

 GetLine( "L21", &L[21] );
 GetLine( "L22", &L[22] );
 GetLine( "L23", &L[23] );

 GetLine( "L25", &L[25] );
 GetLine( "L26", &L[26] );
 GetLine( "L27a", &L27a );

 GetLine( "L28", &L[28] );
 GetLine( "L29", &L[29] );
 GetLine( "L30", &L[30] );
 GetLine( "L31", &L[31] );

 check_if_yes( "Ck32a" );
 check_if_yes( "Ck32b" );
 GetLine( "L32c", &L[32] );

 GetLine( "Interest", &interest );
 GetLine( "Penalty", &penalty );
 GetLine( "L36", &L[36] );


 /*-------------------------------*/
 /* ---- Do Tax Calculations ---- */
 /*-------------------------------*/

 L9aa = 5600.0 * L9a;
 L9bb = 3300.0 * L9b;
 L9cc = 500.0 * L9c;
 L9dd = 5600.0 * L9d;
 L9ee = 1500.0 * L9e;
 L[9] = L9aa + L9bb + L9cc + L9dd + L9ee;
 L[12] = L[10] + L[11];
 L[14] = NotLessThanZero( L[12] - L[13] );
 L[16] = NotLessThanZero( L[14] - L[15] );
 L[17] = flat_tax_rate * L[16];
 L[20] = NotLessThanZero( L[17] - L[18] - L[19] );
 L[24] = L[20] +  L[21] +  L[22] +  L[23];
 L[27] = 0.30 * L27a;

 for (j=25; j <= 32; j++)
  L[33] = L[33] + L[j];

 if (L[33] < L[24])
  { /*Owe*/
    L[34] = L[24] - L[33] + interest + penalty;

  } /*Owe*/
 else
  { /*Refund*/
    L[35] = L[33] - L[24];
    L[37] = L[35] - L[36];

  } /*Refund*/


 /*-------------------------*/
 /* ---- Print Results ---- */
 /*-------------------------*/

 showline_wlabelnz( "L9a", L9a ); 
 showline_wlabelnz( "L9aa", L9aa ); 

 showline_wlabelnz( "L9b", L9b ); 
 showline_wlabelnz( "L9bb", L9bb ); 

 showline_wlabelnz( "L9c", L9c ); 
 showline_wlabelnz( "L9cc", L9cc ); 

 showline_wlabelnz( "L9d", L9d ); 
 showline_wlabelnz( "L9dd", L9dd ); 

 showline_wlabelnz( "L9e", L9e ); 
 showline_wlabelnz( "L9ee", L9ee ); 

 showline(9);	/* */
 showline(10);	/* */
 showline(11);	/* */
 showline(12);	/* */
 showline(13);	/* */
 showline_wmsg(14, "Income subject to tax." );	/* Income subject to tax. */
 showline(15);	/* */
 showline_wmsg(16, "Taxable income." );		/* */
 showline_wmsg(17, "Tax." );			/* */

 showline_wlabelnz( "L18a", L18a );
 showline_wlabelnz( "L18b", L[18] );

 showline_wlabelnz( "L19a", L19a );
 showline_wlabelnz( "L19b", L[19] );

 for (j=20; j <= 26; j++)
  showline(j);

 showline_wlabelnz( "L27a", L27a );
 
 for (j=27; j <= 33; j++)
  showline(j);

 showline_wlabelnz( "interest", interest );
 showline_wlabelnz( "penalty", penalty );
 ShowLineNonZero_wMsg( 34, "You OWE." );
 showline(35); 
 showline(36); 
 ShowLineNonZero_wMsg( 37, "REFUND." );



 fprintf(outfile,"\n{ --------- }\n");
 GetTextLineF( "Your1stName:" );
 GetTextLineF( "YourInitial:" );
 GetTextLineF( "YourLastName:" );

 writeout_line = 0;
 socsec = GetTextLineF( "YourSocSec#:" );
 strcpy( socsectmp, socsec );   /* Copy to buffer, since formatting could add 2-chars. */
 format_socsec( socsectmp, 0 );
 fprintf(outfile,"YourSocSec#: %s\n", socsectmp );
 free( socsec );
 writeout_line = 1;

   GetTextLineF( "Spouse1stName:" );
   GetTextLineF( "SpouseInitial:" );
   GetTextLineF( "SpouseLastName:" );
   writeout_line = 0;
   socsec = GetTextLineF( "SpouseSocSec#:" );
   strcpy( socsectmp, socsec );   /* Copy to buffer, since formatting could add 2-chars. */
   format_socsec( socsectmp, 0 );
   fprintf(outfile,"SpouseSocSec#: %s\n", socsectmp );
   free( socsec );
   writeout_line = 1;

 GetTextLineF( "Number&Street:" );
 GetTextLineF( "Town:" );
 GetTextLineF( "State:" );
 GetTextLineF( "Zipcode:" );

 GetTextLineF( "SchoolDist:" );

 switch (residency)
  {
   case 1: fprintf(outfile,"CkResident X\n");	break;
   case 2: fprintf(outfile,"CkNonRes X\n");	break;
   case 3: fprintf(outfile,"CkPartyear X\n");   break;
  }

 get_word(infile, labelx );     /* Look for optional fields. */
 read_comment_filtered_line( infile, word, 512 );
 fprintf(outfile,"%s	\"%s\"\n", labelx, word );
 get_word(infile, labelx );     /* Look for optional fields. */
 read_comment_filtered_line( infile, word, 512 );
 fprintf(outfile,"%s	\"%s\"\n", labelx, word );
 check_if_yes( "CkChecking" );
 check_if_yes( "CkSavings" );

 fclose(infile);
 grab_any_pdf_markups( infname, outfile );
 fclose(outfile);
 Display_File( outfname );
 printf("\nResults written to file '%s'\n", outfname);
 return 0;
}




/***	 Summary of useful functions:

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

#undef SINGLE
#undef MARRIED_FILING_JOINTLY
#undef MARRIED_FILING_SEPARAT
#undef Yes
#undef No

} // namespace taxsolve_MI_1040_2024
} // namespace OpenTaxSolver2024

