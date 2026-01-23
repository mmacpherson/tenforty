#include "ots_2024_routines.h"
namespace OpenTaxSolver2024 {
namespace taxsolve_US_1040_Sched_C_2024 {

#define Yes 1
#define No  0
/************************************************************************/
/* TaxSolve_US1040_Sched_C.c -	 					*/
/* Copyright (C)  2025 - S.Jenkins					*/
/* 									*/
/* Compile:								*/
/*  cc taxsolve_US1040_Sched_C.c -o taxsolve_US1040_Sched_C		*/
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
/* Updated for 2024 tax year:						*/
/*  S.Jenkins 1-2-2025   						*/
/* Earlier Updates	Robert Heller  heller@deepsoft.com		*/
/************************************************************************/

float thisversion=22.02;



/*----------------------------------------------------------------------------*/

int main( int argc, char *argv[] )
{
 int i, j, k;
 char word[4000], outfname[4000], *EIN=0, *answ, *infname=0;
 time_t now;
 double L16b=0.0, L20b=0.0, L24b=0.0, L44a=0.0, L44b=0.0, L44c=0.0;
 int L32=0;
 char veh_mm[1024]="", veh_dd[1024]="", veh_yy[1024]="";
 char Ck45[1024]="", L46answ[1024]="", L47a_answ[1024]="", L47b_answ[1024]="";
 char *L48a_descr="", *L48b_descr="", *L48c_descr="", *L48d_descr="", *L48e_descr="",
      *L48f_descr="", *L48g_descr="", *L48h_descr="", *L48i_descr="";
 double L48a_amnt=0.0, L48b_amnt=0.0, L48c_amnt=0.0, L48d_amnt=0.0, L48e_amnt=0.0,
        L48f_amnt=0.0, L48g_amnt=0.0, L48h_amnt=0.0, L48i_amnt=0.0;

 printf("US 1040 Schedule C, 2024 - v%3.2f\n", thisversion);

 /* Decode any command-line arguments. */
 i = 1;  k=1;
 while (i < argc)
 {
  if (strcmp(argv[i],"-verbose")==0)  { verbose = 1; }
  else
  if (strcmp(argv[i],"-round_to_whole_dollars")==0)  { round_to_whole_dollars = 1; }
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
	Title:  Sched C 1999 Return
	L1		{Gross Receipts}
	L2		{Returns and Allowances}
	. . .
 */

 /* Accept Form's "Title" line, and put out with date-stamp for your records. */
 read_line( infile, word );
 now = time(0);
 fprintf(outfile,"\n%s,	 v%2.2f, %s\n", word, thisversion, ctime( &now ));
 check_form_version( word, "Title:  1040 Schedule C" );

 GetTextLineF( "YourName:" );
 GetTextLineF( "YourSocSec#:" );
 GetTextLineF( "PrincipalBus:" );
 GetTextLineF( "BusinessName:" );
 GetTextLineF( "Number&Street:" );
 GetTextLineF( "TownStateZip:" );

 GetTextLineF( "ActivityCode:" );
 EIN = GetTextLine( "BusinessEIN:" );
 format_socsec( EIN, 1 );
 fprintf(outfile,"BusinessEIN: %s\n", EIN );

 answ = GetTextLine( "Fmethod:" );
 next_word( answ, word, " \t;" );
 if (strcasecmp( word, "Cash" ) == 0)
  fprintf(outfile,"CkFcash: X\n");
 else
  if (strcasecmp( word, "Accrual" ) == 0)
  fprintf(outfile,"CkFsccrual: X\n");
 else
  if (strcasecmp( word, "Other" ) == 0)
  fprintf(outfile,"CkFother: X\n");

 answ = GetTextLine( "GPartic:" );
 next_word( answ, word, " \t;" );
 if (toupper( word[0] ) == 'Y')
  fprintf(outfile,"CkParticipate: X\n");
 else
 if ((mystrcasestr( word, "N/A" ) == 0) && (toupper( word[0] ) == 'N'))
  fprintf(outfile,"CkNotParticipate: X\n");

 answ = GetTextLine( "Hacquired:" );
 next_word( answ, word, " \t;" );
 if (toupper( word[0] ) == 'Y')
  fprintf(outfile,"CkAcquired: X\n");

 answ = GetTextLine( "Ireq1099s:" );
 next_word( answ, word, " \t;" );
 if (toupper( word[0] ) == 'Y')
  fprintf(outfile,"CkReq1099: X\n");
 else
 if ((mystrcasestr( word, "N/A" ) == 0) && (toupper( word[0] ) == 'N'))
  fprintf(outfile,"CkNotReq1099: X\n");

 answ = GetTextLine( "Jfile1099s:" );
 next_word( answ, word, " \t;" );
 if (toupper( word[0] ) == 'Y')
  fprintf(outfile,"CkWillFile1099: X\n");
 else
 if ((mystrcasestr( word, "N/A" ) == 0) && (toupper( word[0] ) == 'N'))
  fprintf(outfile,"CkNotFile1099: X\n");


 GetLine( "L1", &L[1] );	/* Gross Receipts */

 GetLine( "L2", &L[2] );	/* Returns and Allowances */

 // GetLine( "L4", &L[4] );	/* Cost of Goods Sold */

 GetLine( "L6", &L[6] );	/* Other income, including fed & state fuel credit */

 GetLine( "L8", &L[8] );	/* Advertising */

 GetLine( "L9", &L[9] );	/* Car & truck expenses */

 GetLine( "L10", &L[10] );	/* Commissions & fees */

 GetLine( "L11", &L[11] );	/* Contract labor */

 GetLine( "L12", &L[12] );	/* Depletion */

 GetLine( "L13", &L[13] );	/* Depreciation & Sec 179 exp ded */

 GetLine( "L14", &L[14] );	/* Employee benfit programs (other than line 19) */

 GetLine( "L15", &L[15] );	/* Insurance (other than health) */

 GetLine( "L16a", &L[16] );	/* Interest (mortgage paid to banks) */

 GetLine( "L16b", &L16b );	/* Interest (Other) */

 GetLine( "L17", &L[17] );	/* Legal & professional services */

 GetLine( "L18", &L[18] );	/* Office expense */

 GetLine( "L19", &L[19] );	/* Pension & profit sharing plans */

 GetLine( "L20a", &L[20] );	/* Vehicles and equiment Rent or Lease */

 GetLine( "L20b", &L20b );	/* Rent or lease Other business property */

 GetLine( "L21", &L[21] );	/* Repairs & maintenance */

 GetLine( "L22", &L[22] );	/* Supplies (not in Part III) */

 GetLine( "L23", &L[23] );	/* Taxes & licenses */

 GetLine( "L24a", &L[24] );	/* Travel */

 GetLine( "L24b", &L24b );	/* Deductable Meals & entertainment */

 GetLine( "L25", &L[25] );	/* Utilities */

 GetLine( "L26", &L[26] );	/* Wages (less employment credits) */

 // GetLine( "L27a", &L[27] );	/* Other expenses here from line 48 pg 2 */

 GetLine( "L30", &L[30] );	/* Expenses for business use of home (form 8829) */

 answ = GetTextLine( "L32a" );  /* Yes or No, All investment is at risk */
 next_word( answ, word, " \t;" );
 if (toupper( word[0] ) == 'Y')
   L32 = Yes;
 else
 if (toupper( word[0] ) == 'N')
  L32 = No;
 else
 if (word[0] != '\0')
  {
   printf("Error: Unexpected answer for Line-32a (Y/N) = '%s'.  Exiting.\n", word );
   fprintf(outfile,"Error: Unexpected answer for Line-32a (Y/N) = '%s'.  Exiting.\n", word );
   exit(1);
  }
 else
  {
   printf(" No answer for Line-32a (Y/N).\n");
   fprintf(outfile," No answer for Line-32a (Y/N).\n");
  }


 /* Part III */

 answ = GetTextLine( "L33:" );
 next_word( answ, word, " \t;" );
 if (strcasecmp( word, "Cost" ) == 0)
  fprintf(outfile,"Ck33aCost: X\n");
 else
 if (strcasecmp( word, "Market" ) == 0)
  fprintf(outfile,"Ck33bMarket: X\n");
 else
 if (strcasecmp( word, "Other" ) == 0)
  fprintf(outfile,"Ck33cOther: X\n");
 else
 if (word[0] != '\0')
  printf("Warning: Unexpected answer for L33: '%s'\n", word );

 answ = GetTextLine( "L34:" );
 next_word( answ, word, " \t;" );
 if (toupper( word[0] ) == 'Y')
  fprintf(outfile,"Ck34Yes: X\n");
 else
 if (toupper( word[0] ) == 'N')
  fprintf(outfile,"Ck34No: X\n");

 GetLine( "L35", &L[35] );	/* Inventory at beginning of year */

 GetLine( "L36", &L[36] );	/* Purchases minus cost of personel items */

 GetLine( "L37", &L[37] );	/* Cost of labor (not paid to yourself) */

 GetLine( "L38", &L[38] );	/* Materials & supplies */

 GetLine( "L39", &L[39] );	/* Other costs */

 L[40] = L[35] + L[36] + L[37] + L[38] + L[39];

 GetLine( "L41", &L[41] );	/* Inventory at year end */

 L[42] = L[40] - L[41];


 answ = GetTextLine( "L43:" );
 // TODO this may not always be a date; could be a string like "See Additional Vehicle Information"
 next_word( answ, veh_mm, " \t-/.,;" );
 next_word( answ, veh_dd, " \t-/.,;" );
 next_word( answ, veh_yy, " \t-/.,;" );

 GetLine( "L44a", &L44a );		/* Vehicle Miles. */
 GetLine( "L44b", &L44b );
 GetLine( "L44c", &L44c );

 answ = GetTextLine( "L45:" );
 next_word( answ, Ck45, " \t;" );

 answ = GetTextLine( "L46:" );
 next_word( answ, L46answ, " \t;" );

 answ = GetTextLine( "L47a:" );
 next_word( answ, L47a_answ, " \t;" );

 answ = GetTextLine( "L47b:" );
 next_word( answ, L47b_answ, " \t;" );

 L48a_descr = GetTextLine( "L48a_descr:" );
 GetLine( "L48a_amnt", &L48a_amnt );
 L[48] = L48a_amnt;
 L48b_descr = GetTextLine( "L48b_descr:" );
 GetLine( "L48b_amnt", &L48b_amnt );
 L[48] = L[48] + L48b_amnt;
 L48c_descr = GetTextLine( "L48c_descr:" );
 GetLine( "L48c_amnt", &L48c_amnt );
 L[48] = L[48] + L48c_amnt;
 L48d_descr = GetTextLine( "L48d_descr:" );
 GetLine( "L48d_amnt", &L48d_amnt );
 L[48] = L[48] + L48d_amnt;
 L48e_descr = GetTextLine( "L48e_descr:" );
 GetLine( "L48e_amnt", &L48e_amnt );
 L[48] = L[48] + L48e_amnt;
 L48f_descr = GetTextLine( "L48f_descr:" );
 GetLine( "L48f_amnt", &L48f_amnt );
 L[48] = L[48] + L48f_amnt;
 L48g_descr = GetTextLine( "L48g_descr:" );
 GetLine( "L48g_amnt", &L48g_amnt );
 L[48] = L[48] + L48g_amnt;
 L48h_descr = GetTextLine( "L48h_descr:" );
 GetLine( "L48h_amnt", &L48h_amnt );
 L[48] = L[48] + L48h_amnt;
 L48i_descr = GetTextLine( "L48i_descr:" );
 GetLine( "L48i_amnt", &L48i_amnt );
 L[48] = L[48] + L48i_amnt;
 L[27] = L[48];

 /* -- Compute the tax form -- */

 showline(1);
 showline(2);
 L[3] = L[1] - L[2];
 showline(3);
 L[4] = L[42];
 showline(4);
 L[5] = L[3] - L[4];
 showline_wmsg(5, "Gross profit");
 showline(6);
 L[7] = L[5] + L[6];
 showline_wmsg(7, "Gross income");
 showline(8);
 L[9] = L[9] + 0.67 * L44a;				/* Updated for 2024. */
 showline(9);
 showline(10);
 showline(11);
 showline(12);
 showline(13);
 showline(14);
 showline(15);
 fprintf(outfile,"L16a = %6.2f\n", L[16]);
 fprintf(outfile,"L16b = %6.2f\n", L16b);
 showline(17);
 showline(18);
 showline(19);
 fprintf(outfile,"L20a = %6.2f\n", L[20]);
 fprintf(outfile,"L20b = %6.2f\n", L20b);
 showline(21);
 showline(22);
 showline(23);
 fprintf(outfile,"L24a = %6.2f\n", L[24]);
 fprintf(outfile,"L24b = %6.2f\n", L24b);
 showline(25);
 showline(26);
 showline_wlabel( "L27a", L[27] );
 L[28] = L[8] + L[9] + L[10] + L[11] + L[12] + L[13] + L[14] + L[15] + L[16] + L16b + L[17] + L[18]
 	 + L[19] + L[20] + L20b + L[21] + L[22] + L[23] + L[24] + L24b + L[25] + L[26] + L[27];
 showline_wmsg(28,"Total expenses");
 L[29] = L[7] - L[28];
 showline(29);
 showline(30);
 L[31] = L[29] - L[30];
 showline_wmsg(31,"Net Profit (loss)");

 if (L[31] < 0.0) // Loss
  {
   if (L32 == Yes)
    {
     fprintf(outfile,"Ck32a: x\n");
    }
   else
    {
     fprintf(outfile,"Ck32b: x\n");
     fprintf(outfile,"You must attach Form 6198. Your loss may be limited.\n");
    }
  }

 // For either profit, or loss when "All investment is at risk", message is the same
 if ((L[31] >= 0.0) || L32 == Yes)
  {
    {
     fprintf(outfile,"Enter %2.2f on Form 1040 line S1_3.   Sched-SE line 2.\n", L[31] );
     fprintf(outfile,"        Estates and trusts, enter on Form 1041, line 3.\n");
    }
  }

 showline(35);
 showline(36);
 showline(37);
 showline(38);
 showline(39);
 showline(40);
 showline(41);
 showline_wmsg(42,"Cost of goods sold");


 fprintf(outfile,"L43mm: %s\n", veh_mm );
 fprintf(outfile,"L43dd: %s\n", veh_dd );
 fprintf(outfile,"L43yy: %s\n", veh_yy );

 showline_wlabelnz( "L44a", L44a );
 showline_wlabelnz( "L44b", L44b );
 showline_wlabelnz( "L44c", L44c );

 if (toupper( Ck45[0] ) == 'Y')
  fprintf(outfile,"Ck45Yes: X\n");
 else
 if (strcasecmp( Ck45, "No") == 0)
  fprintf(outfile,"Ck45No: X\n");

 if (toupper( L46answ[0] ) == 'Y')
  fprintf(outfile,"Ck46Yes: X\n");
 else
 if (strcasecmp( L46answ, "No") == 0)
  fprintf(outfile,"Ck46No: X\n");

 if (toupper( L47a_answ[0] ) == 'Y')
  fprintf(outfile,"Ck47aYes: X\n");
 else
 if (strcasecmp( L47a_answ, "No") == 0)
  fprintf(outfile,"Ck47aNo: X\n");

 if (toupper( L47b_answ[0] ) == 'Y')
  fprintf(outfile,"Ck47bYes: X\n");
 else
 if (strcasecmp( L47b_answ, "No") == 0)
  fprintf(outfile,"Ck47bNo: X\n");

 if (strlen( L48a_descr ) > 0)
  fprintf(outfile,"L48a_descr: %s\n", L48a_descr );
 showline_wlabelnz( "L48a_amnt", L48a_amnt );
 if (strlen( L48b_descr ) > 0)
  fprintf(outfile,"L48b_descr: %s\n", L48b_descr );
 showline_wlabelnz( "L48b_amnt", L48b_amnt );
 if (strlen( L48c_descr ) > 0)
  fprintf(outfile,"L48c_descr: %s\n", L48c_descr );
 showline_wlabelnz( "L48c_amnt", L48c_amnt );
 if (strlen( L48d_descr ) > 0)
  fprintf(outfile,"L48d_descr: %s\n", L48d_descr );
 showline_wlabelnz( "L48d_amnt", L48d_amnt );
 if (strlen( L48e_descr ) > 0)
  fprintf(outfile,"L48e_descr: %s\n", L48e_descr );
 showline_wlabelnz( "L48e_amnt", L48e_amnt );
 if (strlen( L48f_descr ) > 0)
  fprintf(outfile,"L48f_descr: %s\n", L48f_descr );
 showline_wlabelnz( "L48f_amnt", L48f_amnt );
 if (strlen( L48g_descr ) > 0)
  fprintf(outfile,"L48g_descr: %s\n", L48g_descr );
 showline_wlabelnz( "L48g_amnt", L48g_amnt );
 if (strlen( L48h_descr ) > 0)
  fprintf(outfile,"L48h_descr: %s\n", L48h_descr );
 showline_wlabelnz( "L48h_amnt", L48h_amnt );
 if (strlen( L48i_descr ) > 0)
  fprintf(outfile,"L48i_descr: %s\n", L48i_descr );
 showline_wlabelnz( "L48i_amnt", L48i_amnt );

 showline_wmsg( 48, "Total other expenses" );

 fclose(infile);
 grab_any_pdf_markups( infname, outfile );
 fclose(outfile);

 printf("\nListing results from file: %s\n\n", outfname);
 Display_File( outfname );

 return 0;
}

#undef Yes
#undef No

} // namespace taxsolve_US_1040_Sched_C_2024
} // namespace OpenTaxSolver2024
