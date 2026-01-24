#include "ots_2021_routines.h"
namespace OpenTaxSolver2021 {
namespace taxsolve_CA_5805_2021 {

#define SINGLE 		        1
#define MARRIED_FILING_JOINTLY 2
#define MARRIED_FILING_SEPARAT 3
#define HEAD_OF_HOUSEHOLD       4
#define WIDOW		        5
#define INDIVIDUAL	1
#define ESTATE	2
#define Yes 1
#define No  0
#define NotApplicable 3
#define Short 1
#define Annualized 2
/************************************************************************/
/* TaxSolve_CA_5805-2021.c						*/
/*  User contributed.							*/
/************************************************************************/

float thisversion=3.02;




/* The following two tax functions copied from taxsolve_CA_540_2020.c. */
/* 2020 tax rates are used in California for calculating estimated taxes */

double TaxRateFormula( double income, int status )
{									/* Updated for 2021 to the 2020 CA-tax-table. */
 double tax;
 if ((status==SINGLE) || (status==MARRIED_FILING_SEPARAT))
  {
   if (income <   8932.00)  tax =             0.01 * income;                else
   if (income <  21175.00)  tax =    89.32 +  0.02 * (income -   8832.00);  else
   if (income <  33421.00)  tax =   334.18 +  0.04 * (income -  21175.00);  else
   if (income <  46394.00)  tax =   824.02 +  0.06 * (income -  33421.00);  else
   if (income <  58634.00)  tax =  1602.40 +  0.08 * (income -  46394.00);  else
   if (income < 299508.00)  tax =  2581.60 + 0.093 * (income -  58634.00);  else
   if (income < 359407.00)  tax = 24982.88 + 0.103 * (income - 299508.00);  else
   if (income < 599012.00)  tax = 31152.48 + 0.113 * (income - 359407.00);
   else                     tax = 58227.85 + 0.123 * (income - 599012.00);
  }
 else
 if ((status==MARRIED_FILING_JOINTLY) || (status==WIDOW))
  {
   if (income <   17864.00)  tax =              0.01 * income;                 else
   if (income <   42350.00)  tax =    178.64 +  0.02 * (income -   17864.00);  else
   if (income <   66842.00)  tax =    668.36 +  0.04 * (income -   42350.00);  else
   if (income <   92788.00)  tax =   1648.04 +  0.06 * (income -   66842.00);  else
   if (income <  117268.00)  tax =   3204.80 +  0.08 * (income -   92788.00);  else
   if (income <  599016.00)  tax =   5163.20 + 0.093 * (income -  117268.00);  else
   if (income <  718814.00)  tax =  49965.76 + 0.103 * (income -  599016.00);  else
   if (income < 1198024.00)  tax =  62304.95 + 0.113 * (income -  718814.00);
   else                      tax = 116455.68 + 0.123 * (income - 1198024.00);
  }
 else
  {
   if (income <  17876.00)  tax =             0.01 * income;                else
   if (income <  42353.00)  tax =   178.76 +  0.02 * (income -  17876.00);  else
   if (income <  54597.00)  tax =   668.30 +  0.04 * (income -  42353.00);  else
   if (income <  67569.00)  tax =  1158.06 +  0.06 * (income -  54597.00);  else
   if (income <  79812.00)  tax =  1936.38 +  0.08 * (income -  67569.00);  else
   if (income < 407329.00)  tax =  2915.82 + 0.093 * (income -  79812.00);  else
   if (income < 488796.00)  tax = 33374.90 + 0.103 * (income - 407329.00);  else
   if (income < 814658.00)  tax = 41766.00 + 0.113 * (income - 488796.00); 
   else                     tax = 78588.41 + 0.123 * (income - 814658.00);
  }
 return (int)(tax+0.5);
}

double TaxRateFunction( double income, int status )     /* Emulates table lookup or function appropriately. */
{
 double x, tx;
 int k;

 if (income < 100000.0)   /* Quantize to match tax-table exactly. */
  {
   if (income < 99951.0) 
    {
     k = (income + 49) / 100;
     x = 100 * (double)k;
    }
   else x = 99975.0;
   tx = (int)(TaxRateFormula( x, status ));
  }
 else
  tx = TaxRateFormula( income, status );
 return tx;
}

double L6WS(int column, double IIIL4, double ScdA, double IIIL5, double FAIWSL3, int status){

	double L[14];
	int i;

	L[1] = IIIL4;
	L[2] = ScdA;
	L[3] = L[1] - L[2];
	L[4] = IIIL5;
	L[5] = L[1] * L[4];
	if(L[3] == 0){
		fprintf(outfile, " Line 6 Worksheet - Column (%c),\n", column);
		for(i = 1; i <= 5; i++)
			fprintf(outfile, "L6WS_%d%c %0.2lf\n", i, column, L[i]);
		return(L[5]);
	}
	L[6] = L[3] * L[4];
	L[7] = Round(L[6] * 0.80);
	L[8] = FAIWSL3;
	if((status == MARRIED_FILING_JOINTLY) || (status == WIDOW))
		L[9] = 424581;
	else if((status == SINGLE) || (status == MARRIED_FILING_SEPARAT)) 
		L[9] = 212288;
	else if(status == HEAD_OF_HOUSEHOLD)
		L[9] = 318437;
	L[10] = L[8] - L[9];
	L[11] = Round(L[10] * 0.06);
	L[12] = SmallerOf(L[7], L[11]);
	L[13] = Round(L[5] - L[12]);
	
	fprintf(outfile, " Line 6 Worksheet - Column (%c),\n", column);
	for(i = 1; i <= 13; i++)
		fprintf(outfile, "L6WS_%d%c %0.2lf\n", i, column, L[i]);
	return(L[13]);
}

/*----------------------------------------------------------------------------*/

int main( int argc, char *argv[] )
{
  int i, j, k, status, entity = INDIVIDUAL, Quest4 = Yes;
 char word[4000], outfname[4000], *infname=0;
 time_t now;

 int Quest2, Quest3 = 0, Num_Days = -1;		/* negative Num_Days used as a flag below */
 double Wthd_Per_1, Wthd_Per_2, Wthd_Per_3, Wthd_Per_4, CA_AGI;
 double a10add, b10add, c10add, d10add;
 

 /* line entry variables L[n] are declared in taxsolve_routines.c */

 double a[24], b[24], c[24], d[24];	/* cells in grid of Part III Annualizd Income Installment Method Schedule */
					/* comprised of lines 1-13 and 15-23; e.g., cell 1(a) will be in variable a[1] */
					/*  lines 14a-14e declared individually */

 double L14aa = 0, L14ab = 0, L14ac = 0, L14ad = 0;
 double L14ba = 0, L14bb = 0, L14bc = 0, L14bd = 0;
 double L14ca = 0, L14cb = 0, L14cc = 0, L14cd = 0;
 double L14da = 0, L14db = 0, L14dc = 0, L14dd = 0;
 double L14ea = 0, L14eb = 0, L14ec = 0, L14ed = 0;

 double FAIWS_a[4], FAIWS_b[4], FAIWS_c[4], FAIWS_d[4];		/* cells in grid for Federal Annualized Income Worksheet */

 double L6WS_a[14], L6WS_b[14], L6WS_c[14], L6WS_d[14]; 	/* cells in grid for Line 6 Worksheet */

 double A[15], B[15], C[15], D[15];	/* cells in grid of Worksheet II */
					/* e.g., cell 1(a) will be in variable A[1] */

  printf("Form 5805, 2021 - v%3.2f\n", thisversion);

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

 for(i = 0; i <= 14; i++){
	A[i] = 0.0;
	B[i] = 0.0;
	C[i] = 0.0;
	D[i] = 0.0;
 }

 for(i = 0; i <= 23; i++){
	a[i] = 0.0;
	b[i] = 0.0;
	c[i] = 0.0;
	d[i] = 0.0;
 }

 for(i = 0; i <= 3; i++){
	FAIWS_a[i] = 0.0;
	FAIWS_b[i] = 0.0;
	FAIWS_c[i] = 0.0;
	FAIWS_d[i] = 0.0;
 }

for(i = 0; i <= 13; i++){
	L6WS_a[i] = 0.0;
	L6WS_b[i] = 0.0;
	L6WS_c[i] = 0.0;
	L6WS_d[i] = 0.0;
 }

 Wthd_Per_1 = 0.0;
 Wthd_Per_2 = 0.0;
 Wthd_Per_3 = 0.0;
 Wthd_Per_4 = 0.0;

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
 check_form_version( word, "Title:  Form 5805 for Tax Year 2021" );

 // add_pdf_markup( "NotReady", 1, 240, 40, 17, 1, 1.0, 0, 0, "\"This program is NOT ready for 20xx.\"" );

 fprintf(outfile, "%s\n", "==================================================");
 fprintf(outfile, "%s\n", "                                                            CAUTION\nThis program fills out Form 5805 to determine WHETHER OR NOT you owe a penalty\nfor underpayment of estimated tax.  It calculates the AMOUNT of any penalty you\nmay owe for the MOST LIKELY CASE in which up to four estimated tax payments have\n been made.  You should carefully review the instructions for Form 5805 to see if the\ncalculations are correct for your particular tax situation.  DO NOT INTERPRET a\ndefault zero value for the penalty on the filled PDF to indicate that you do not owe\na penalty, especially if you have not input all required information, including the\nactual dates on which you made your payments. Scroll down to the end of this\nresults file to see if you had an underpayment for any period.  If so, you may owe\na penalty.\n\nItemized deductions are limited for high-income taxpayers.  When you are\nchecking calculations, values on line 6 of Part III may appear to be in error due to\nthese limitations.  See this results file and the last page of the output PDF for the\nlimitation calculations.  Also note that the annualization factor values on lines 4\nof the last page of the output PDF round by default to the nearest integer.  Use\nthe values in this results file for lines L6WS_4a, L6WS_4b, L6WS_4c, and L6WS_4d,\n which for most individual taxpayers will be 4.0, 2.4, 1.5, and 1.0, respectively.\n\nThis program does not calculate the phase-out of exemption credits for high-income\ntaxpayers as collection of the necessary information for each period would\nunnecessarily complicate this program for most users and the impact of exemption\nlimitations on the tax estimates would be negligible.  See instructions for line 11.");
 fprintf(outfile, "%s\n\n", "==================================================");
 
 /* ----- Place all your form-specific code below here .... ------ */

 // Exam
 //  GetLineF( "L2", &L[2] );
 //  GetLineF( "L3", &L[3] );
 //  L[4] = L[2] - L[3];
 //  showline_wlabel( "L4", L[4] );

 GetTextLineF( "YourName:" );
 GetTextLineF( "YourSocSec#:" );

 get_parameter( infile, 's', word, "Entity" );
 get_parameter( infile, 'w', word, "Entity?");
 if (strncasecmp(word,"Individual",3)==0){
 	entity = INDIVIDUAL;
 }
 else if(strncasecmp(word,"Estate/Trust",3)==0){
	entity = ESTATE;
 }
 fprintf(outfile,"Entity = %s (%d)\n", word, entity);


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

 if((entity == ESTATE) && (status != SINGLE)){

	#ifdef microsoft
	 system( "start bin\\notify_popup -delay 3 -expire 60 \"Setting Status to Single as required for estates/trusts.\"" );
	#else
	 system( "bin/notify_popup -delay 3 -expire 60 \"Setting Status to Single as required for estates/trusts.\" &" );
	#endif

	strcpy(word, "Single");
	status = SINGLE;
	fprintf(outfile, "Setting Status to Single as required for estates/trusts\n");
 }
 fprintf(outfile,"Status = %s (%d)\n", word, status);

 get_parameter( infile, 's', word, "Quest1" );
 get_parameter( infile, 'w', word, "Quest1?");
 if (strncasecmp(word,"Yes",1)==0){
	// Quest1 = Yes;
	fprintf(outfile,"CkQuest1Yes X\n");
}
else {
		fprintf(outfile,"CkQuest1No X\n");
}

 get_parameter( infile, 's', word, "Quest2" );
 get_parameter( infile, 'w', word, "Quest2?");
 if (strncasecmp(word,"Yes",1)==0){
	 Quest2 = Yes;
	fprintf(outfile,"CkQuest2Yes X\n");
}
else {
	Quest2 = No;
	fprintf(outfile,"CkQuest2No X\n");
}

 get_parameter( infile, 's', word, "Quest3" );
 get_parameter( infile, 'w', word, "Quest3?");
 if (strncasecmp(word,"Yes",1)==0){
	Quest3 = Yes;
	fprintf(outfile,"CkQuest3Yes X\n");
}
else if (strncasecmp(word,"No",2)==0){
	Quest3 = No;
	fprintf(outfile,"CkQuest3No X\n");
}
else {
	// Quest3 = NotApplicable;
	fprintf(outfile,"CkQuest3NA X\n");
}

 GetLineF( "Wthd_Per_1", &Wthd_Per_1 );
 GetLineF( "Wthd_Per_2", &Wthd_Per_2 );
 GetLineF( "Wthd_Per_3", &Wthd_Per_3 );
 GetLineF( "Wthd_Per_4", &Wthd_Per_4 );

 get_parameter( infile, 's', word, "Quest4" );
 get_parameter( infile, 'w', word, "Quest4?");
 if (strncasecmp(word,"Yes",1)==0){
	 Quest4 = Yes;
	fprintf(outfile,"CkQuest4Yes X\n");
}
else if (strncasecmp(word,"No",2)==0){
	Quest4 = No;
	fprintf(outfile,"CkQuest4No X\n");
}

 if((entity == ESTATE) && (Quest4 == Yes)){

	fprintf(outfile, "Estates and grantor trusts, which receive the residue of the decedent's estate,\nare required to make estimated income tax payments for any year ending two or\nmore years after the date of the decedent's death. If you answer \"Yes\" to\nPart I, Question 4, complete Part I only and attach form FTB 5805 to the\nback of your tax return.\n");

  exit(1);
 
 }


/* Part II - Required Annual Payment */

 GetLineF( "L1", &L[1] );
  L[2] = L[1] * 0.90;
  showline( 2 );
 GetLine( "L3", &L[3] );
	if(Quest3 == Yes){
		L[3] = Wthd_Per_1 + Wthd_Per_2 + Wthd_Per_3 + Wthd_Per_4;
	}
 showline( 3 );
 L[4] = L[1]  -  L[3];
 showline( 4 );
 if((L[4] < 250.00) && (status == MARRIED_FILING_SEPARAT)){
		fprintf(outfile, "Status is \"Married Filing Separately\" and line 4 is less than $250.  Stop here.\nYou do not owe the penalty. Do not file form FTB 5805.\n");
		exit(0);
 }
 else if((L[4] < 500.00) && (status != MARRIED_FILING_SEPARAT)){
		fprintf(outfile, "Status is not \"Married Filing Separately\" and line 4 is less than $500.  Stop here.\nYou do not owe the penalty. Do not file form FTB 5805.\n");
		exit(0);
 }
  GetLineF( "L5", &L[5] );

 GetLineF( "CA_AGI", &CA_AGI );

 if((CA_AGI >= 1000000.00) || (status == MARRIED_FILING_SEPARAT && CA_AGI >= 500000.00))
	L[6] = L[2];
else
	L[6] = SmallerOf(L[2], L[5]);
showline( 6 );

/* Short Method */

/* Need to read these Short Method inputs even if Short Method is not used; */
/* otherwise, error message re: unexpected input item is thrown */

  GetLineF( "L8", &L[8] );
 
  GetInteger( "Num_Days", &Num_Days);

	if(Quest2 == No){			/* Not using Annualized Income Installment Method */

		L[7] = L[3];
		L[9] = L[7 ]+ L[8];
		L[10] = L[6] - L[9];

		for(i = 7; i <= 10; i++)
			showline( i );

		if(L[10] <= 0){
			fprintf(outfile, "Line 10 is zero or less.  Stop here.  You do not owe the penalty.\nDo not file form FTB 5805.\n");
			exit(0);
		}

		L[11] = L[10] * 0.02121370;

		if(Num_Days > -1){

			L[12] = L[10] * Num_Days * 0.00008;
			L[13] = L[11] - L[12];
	
			showline( 11 );
			showline( 12 );
		
			showline_wmsg(13, "PENALTY.  Enter this amount on Form 540, line 113; Form 540NR, line 123; or Form 541, line 44.\n Also, check the box for FTB 5805.\n");
		}
	}
	else{					/* Using the Annualized Income Installment Method */

	/* Annualized Income Installment Method */

	/* Inputs must be read even if Annualized Income Installment Method is not used; */
	/* otherwise, error message re: unexpected input is thrown */
	
	   GetLine( "SchdAI_1a", &a[1] );
	   GetLine( "SchdAI_1b", &b[1] );
	   GetLine( "SchdAI_1c", &c[1] );
	   GetLine( "SchdAI_1d", &d[1] );

	   if(entity == ESTATE){

		a[2] = 6.0;
		b[2] = 3.0;
		c[2] = 1.71429;
		d[2] = 1.09091;

	   }
	   else{
		a[2] = 4.0;
		b[2] = 2.4;
		c[2] = 1.5;
		d[2] = 1.0;
	   }

//	   GetLine1( "SchdAI_2a", &a[2] );
//	   GetLine1( "SchdAI_2b", &b[2] );
//	   GetLine1( "SchdAI_2c", &c[2] );
//	   GetLine1( "SchdAI_2d", &d[2] );
 	
	   GetLine( "SchdAI_4a", &a[4] );
	   GetLine( "SchdAI_4b", &b[4] );
	   GetLine( "SchdAI_4c", &c[4] );
	   GetLine( "SchdAI_4d", &d[4] );

//	   GetLine( "SchdAI_6a", &a[6] );
//	   GetLine( "SchdAI_6b", &b[6] );
//	   GetLine( "SchdAI_6c", &c[6] );
//	   GetLine( "SchdAI_6d", &d[6] );	   
	
	   GetLine( "SchdAI_7a", &a[7] );
	   b[7] = a[7];
	   c[7] = a[7];
	   d[7] = a[7];
	
	   GetLine( "SchdAI_10a_add", &a10add );
	   GetLine( "SchdAI_10b_add", &b10add );
	   GetLine( "SchdAI_10c_add", &c10add );
	   GetLine( "SchdAI_10d_add", &d10add ); 

	   GetLine( "SchdAI_11a", &a[11] );
	   GetLine( "SchdAI_11b", &b[11] );
	   GetLine( "SchdAI_11c", &c[11] );
	   GetLine( "SchdAI_11d", &d[11] );

	   GetLine( "SchdAI_13a", &a[13] );
	   GetLine( "SchdAI_13b", &b[13] );
	   GetLine( "SchdAI_13c", &c[13] );
	   GetLine( "SchdAI_13d", &d[13] ); 	   
	
	   GetLine( "SchdAI_14ba", &L14ba );
	   GetLine( "SchdAI_14bb", &L14bb );
	   GetLine( "SchdAI_14bc", &L14bc );
	   GetLine( "SchdAI_14bd", &L14bd );

	   GetLine( "SchdAI_14da", &L14da );
	   GetLine( "SchdAI_14db", &L14db );
	   GetLine( "SchdAI_14dc", &L14dc );
	   GetLine( "SchdAI_14dd", &L14dd );
	
	   GetLine( "FAIWS_1a", &FAIWS_a[1]);
	   GetLine( "FAIWS_1b", &FAIWS_b[1]);
	   GetLine( "FAIWS_1c", &FAIWS_c[1]);
	   GetLine( "FAIWS_1d", &FAIWS_d[1]);

	   GetLine( "L6WS_2a", &L6WS_a[2]);
	   GetLine( "L6WS_2b", &L6WS_b[2]);
	   GetLine( "L6WS_2c", &L6WS_c[2]);
	   GetLine( "L6WS_2d", &L6WS_d[2]);	   

        GetLine( "WSII_2a", &A[2]);
	GetLine( "WSII_2b", &B[2]);
	GetLine( "WSII_2c", &C[2]);
	GetLine( "WSII_2d", &D[2]);

	GetLine( "WSII_10a", &A[10]);
	GetLine( "WSII_10b", &B[10]);
	GetLine( "WSII_10c", &C[10]);
	GetLine( "WSII_10d", &D[10]);	   

	a[3] = a[1] * a[2];
	b[3] = b[1] * b[2];
	c[3] = c[1] * c[2];
	d[3] = d[1] * d[2];

	a[5] = a[2];		
	b[5] = b[2];
	c[5] = c[2];
	d[5] = d[2];

	a[6] = a[4] * a[5];		/* calculation for lower income filers */		
	b[6] = b[4] * b[5];		/* these values will be overwritten if higher income thresholds are exceeded */
	c[6] = c[4] * c[5];
	d[6] = d[4] * d[5];

	FAIWS_a[3] = FAIWS_a[1] * a[5];
	fprintf(outfile, "FAIWS_1a\t%0.2lf\n", FAIWS_a[1]);
	fprintf(outfile, "FAIWS_2a\t%0.2lf\n", a[5]);
	fprintf(outfile, "FAIWS_3a\t%0.2lf\n", FAIWS_a[3]);

	if(a[4] > 0){
		if((((status == MARRIED_FILING_JOINTLY) || (status == WIDOW)) && (FAIWS_a[3] > 424581.00)) || \
	(((status == SINGLE) || (status == MARRIED_FILING_SEPARAT)) && (FAIWS_a[3] > 212288.00)) || \
	((status == HEAD_OF_HOUSEHOLD) && (FAIWS_a[3] > 318437.00))){
			a[6] = L6WS('a', a[4], L6WS_a[2], a[5], FAIWS_a[3], status);
		}
	}
	
	FAIWS_b[3] = FAIWS_b[1] * b[5];
	fprintf(outfile, "FAIWS_1b\t%0.2lf\n", FAIWS_b[1]);
	fprintf(outfile, "FAIWS_2b\t%0.2lf\n", b[5]);
	fprintf(outfile, "FAIWS_3b\t%0.2lf\n", FAIWS_b[3]);

	if(b[4] > 0){
		if((((status == MARRIED_FILING_JOINTLY) || (status == WIDOW)) && (FAIWS_b[3] > 424581.00)) || \
	(((status == SINGLE) || (status == MARRIED_FILING_SEPARAT)) && (FAIWS_b[3] > 212288.00)) || \
	((status == HEAD_OF_HOUSEHOLD) && (FAIWS_b[3] > 318437.00))){
			b[6] = L6WS('b', b[4], L6WS_b[2], b[5], FAIWS_b[3], status);
		}	
	}

	FAIWS_c[3] = FAIWS_c[1] * c[5];
	fprintf(outfile, "FAIWS_1c\t%0.2lf\n", FAIWS_c[1]);
	fprintf(outfile, "FAIWS_2c\t%0.2lf\n", c[5]);
	fprintf(outfile, "FAIWS_3c\t%0.2lf\n", FAIWS_c[3]);

	if(c[4] > 0){
		if((((status == MARRIED_FILING_JOINTLY) || (status == WIDOW)) && (FAIWS_c[3] > 424581.00)) || \
	(((status == SINGLE) || (status == MARRIED_FILING_SEPARAT)) && (FAIWS_c[3] > 212288.00)) || \
	((status == HEAD_OF_HOUSEHOLD) && (FAIWS_c[3] > 318437.00))){
			c[6] = L6WS('c', c[4], L6WS_c[2], c[5], FAIWS_c[3], status);
		}
	}

	FAIWS_d[3] = FAIWS_d[1] * d[5];
 	fprintf(outfile, "FAIWS_1d\t%0.2lf\n", FAIWS_d[1]);
	fprintf(outfile, "FAIWS_2d\t%0.2lf\n", d[5]);
	fprintf(outfile, "FAIWS_3d\t%0.2lf\n", FAIWS_d[3]);

	if(d[4] > 0){
		if((((status == MARRIED_FILING_JOINTLY) || (status == WIDOW)) && (FAIWS_d[3] > 424581.00)) || \
	(((status == SINGLE) || (status == MARRIED_FILING_SEPARAT)) && (FAIWS_d[3] > 212288.00)) || \
	((status == HEAD_OF_HOUSEHOLD) && (FAIWS_d[3] > 318437.00))){
			d[6] = L6WS('d', d[4], L6WS_d[2], d[5], FAIWS_d[3], status);
		}
	}

	a[8] = LargerOf(a[6], a[7]);
	b[8] = LargerOf(b[6], b[7]);
	c[8] = LargerOf(c[6], c[7]); 
	d[8] = LargerOf(d[6], d[7]);

	a[9] = a[3] - a[8];
	b[9] = b[3] - b[8];
	c[9] = c[3] - c[8];
	d[9] = d[3] - d[8];

	a[10] = TaxRateFunction( a[9], status);
	b[10] = TaxRateFunction( b[9], status);	
	c[10] = TaxRateFunction( c[9], status);
	d[10] = TaxRateFunction( d[9], status);

	a[10] += a10add;
	b[10] += b10add;	
	c[10] += c10add;
	d[10] += d10add;

	a[12] = a[10] - a[11];
	b[12] = b[10] - b[11];
	c[12] = c[10] - c[11];
	d[12] = d[10] - d[11];

	L14aa = NotLessThanZero(a[12] - a[13]);
	L14ab = NotLessThanZero(b[12] - b[13]);
	L14ac = NotLessThanZero(c[12] - c[13]);
	L14ad = NotLessThanZero(d[12] - d[13]);

	L14ca = L14aa + L14ba;		/* first lower case letter is row; second lower case letter is column */
	L14cb = L14ab + L14bb;
	L14cc = L14ac + L14bc;
	L14cd = L14ad + L14bd;

	L14ea = L14ca - L14da;
	L14eb = L14cb - L14db;
	L14ec = L14cc - L14dc;
	L14ed = L14cd - L14dd;

	a[16] = Round(L14ea * 0.27);
	b[16] = Round(L14eb * 0.63);
	c[16] = Round(L14ec * 0.63);
	d[16] = Round(L14ed * 0.90);

	a[18] = NotLessThanZero(a[16] - a[17]);
	a[19] = Round(L[6] * 0.30);
	a[21] = a[19];
	a[22] = NotLessThanZero(a[21] - a[18]);
	a[23] = SmallerOf(a[18], a[21]);
	A[1] = a[23];

	b[17] = a[23];
	b[18] = NotLessThanZero(b[16] - b[17]);
	b[19] = Round(L[6] * 0.40);
	b[20] = a[22];
	b[21] = b[19] + b[20];
	b[22] = NotLessThanZero(b[21] - b[18]);
	b[23] = SmallerOf(b[18], b[21]);
	B[1] = b[23];
	
	c[17] = a[23] + b[23];
	c[18] = NotLessThanZero(c[16] - c[17]);
	c[19] = 0;
	c[20] = b[22];	
	c[21] = c[19] + c[20];
	c[22] = NotLessThanZero(c[21] - c[18]);
	c[23] = SmallerOf(c[18], c[21]);
	C[1] = c[23];
	
	d[17] = a[23] + b[23] + c[23];
	d[18] = NotLessThanZero(d[16] - d[17]);
	d[19] = Round(L[6] * 0.30);
	d[20] = c[22];
	d[21] = d[19] + d[20];
	d[22] = NotLessThanZero(d[21] - d[18]);
	d[23] = SmallerOf(d[18], d[21]);
	D[1] = d[23];

	}	
		/* WORKSHEET II */

	/* A[2], B[2], C[2], D[2] contain estimated taxes paid each period.  Add uneven withholding in period */
	/* or total of evenly withheld amounts allocated to each period based on number of days in period */
	/* Withholding is either uneven or even (in which case Wthd_Per_1, Wthd_Per_2, Wthd_Per_3, and */
	/* Wthd_Per_4 should all be blank or zero. */

	if(Quest3 == Yes){
		A[2] = A[2] + Wthd_Per_1;
		B[2] = B[2] + Wthd_Per_2;
		C[2] = C[2] + Wthd_Per_3;
		D[2] = D[2] + Wthd_Per_4;
	}
	else{
		A[2] = A[2] + L[3] * (90.0 / 365.0);
		B[2] = B[2] + L[3] * (61.0 / 365.0);
		C[2] = C[2] + L[3] * (92.0 / 365.0);
		D[2] = D[2] + L[3] * (122.0 / 365.0);
	}
	
	A[6] = A[2];

	if(A[1] >= A[6])
		A[8] = A[1] - A[6];
	else
		A[9] = A[6] - A[1];

	A[11] = A[8] * A[10]/365 * 0.03;
		
	B[3] = A[9];
	B[4] = B[2] + B[3];
	B[5] = A[7] + A[8];
	B[6] = NotLessThanZero(B[4] - B[5]);
	if(B[6] == 0)
		B[7] = B[5] - B[4];
	else
		B[7] = 0;
	if(B[1] >= B[6])
		B[8] = B[1] - B[6];
	else
		B[9] = B[6] - B[1];

	B[11] = B[8] * B[10]/365 * 0.03;

	C[3] = B[9];
	C[4] = C[2] + C[3];
	C[5] = B[7] + B[8];
	C[6] = NotLessThanZero(C[4] - C[5]);
	if(C[6] == 0)
		C[7] = C[5] - C[4];
	else
		C[7] = 0;
	if(C[1] >= C[6])
		C[8] = C[1] - C[6];
	else
		C[9] = C[6] - C[1];

	C[11] = C[8] * C[10]/365 * 0.03;

	D[3] = C[9];
	D[4] = D[2] + D[3];
	D[5] = C[7] + C[8];
	D[6] = NotLessThanZero(D[4] - D[5]);
	
	if(D[1] >= D[6])
		D[8] = D[1] - D[6];
	else
		D[9] = D[6] - D[1];

	D[11] = D[8] * D[10]/365 * 0.03;

	A[0] = A[11] + B[11] + C[11] + D[11];	/* line 12 of WSII */

	if(Quest2 == Yes){
	
		for(i = 18; i <= 14; i++){
			fprintf(outfile, "SecA_%d%s %0.2lf\n", i, "a", A[i]);
			fprintf(outfile, "SecA_%d%s %0.2lf\n", i, "b", B[i]);
			fprintf(outfile, "SecA_%d%s %0.2lf\n", i, "c", C[i]);
			fprintf(outfile, "SecA_%d%s %0.2lf\n", i, "d", D[i]);
		}

		i = 1;
			fprintf(outfile, "SchdAI_%d%s %0.2lf\n", i, "a", a[i]);
			fprintf(outfile, "SchdAI_%d%s %0.2lf\n", i, "b", b[i]);
			fprintf(outfile, "SchdAI_%d%s %0.2lf\n", i, "c", c[i]);
			fprintf(outfile, "SchdAI_%d%s %0.2lf\n", i, "d", d[i]);

		i = 2;
			fprintf(outfile, "SchdAI_%d%s %0.1lf\n", i, "a", a[i]);
			fprintf(outfile, "SchdAI_%d%s %0.1lf\n", i, "b", b[i]);
			fprintf(outfile, "SchdAI_%d%s %0.5lf\n", i, "c", c[i]);
			fprintf(outfile, "SchdAI_%d%s %0.5lf\n", i, "d", d[i]);

		for(i = 3; i <= 9; i++){
			fprintf(outfile, "SchdAI_%d%s %0.2lf\n", i, "a", a[i]);
			fprintf(outfile, "SchdAI_%d%s %0.2lf\n", i, "b", b[i]);
			fprintf(outfile, "SchdAI_%d%s %0.2lf\n", i, "c", c[i]);
			fprintf(outfile, "SchdAI_%d%s %0.2lf\n", i, "d", d[i]);
		}

		i = 10;
			fprintf(outfile, "SchdAI_%d%s %0.2lf     = calculated: %0.2lf + additions: %0.2lf\n", i, "a", a[i], a[10] - a10add, a10add);
			fprintf(outfile, "SchdAI_%d%s %0.2lf     = calculated: %0.2lf + additions: %0.2lf\n", i, "b", b[i], b[10] - b10add, b10add);
			fprintf(outfile, "SchdAI_%d%s %0.2lf     = calculated: %0.2lf + additions: %0.2lf\n", i, "c", c[i], c[10] - c10add, c10add);
			fprintf(outfile, "SchdAI_%d%s %0.2lf     = calculated: %0.2lf + additions: %0.2lf\n", i, "d", d[i], d[10] - d10add, d10add);

		for(i = 11; i <= 13; i++){
			fprintf(outfile, "SchdAI_%d%s %0.2lf\n", i, "a", a[i]);
			fprintf(outfile, "SchdAI_%d%s %0.2lf\n", i, "b", b[i]);
			fprintf(outfile, "SchdAI_%d%s %0.2lf\n", i, "c", c[i]);
			fprintf(outfile, "SchdAI_%d%s %0.2lf\n", i, "d", d[i]);
		}
		
		fprintf(outfile, "%s %0.2lf\n", "L14aa", L14aa);
		fprintf(outfile, "%s %0.2lf\n", "L14ab", L14ab);
		fprintf(outfile, "%s %0.2lf\n", "L14ac", L14ac);
		fprintf(outfile, "%s %0.2lf\n", "L14ad", L14ad);
		fprintf(outfile, "%s %0.2lf\n", "L14ba", L14ba);
		fprintf(outfile, "%s %0.2lf\n", "L14bb", L14bb);
		fprintf(outfile, "%s %0.2lf\n", "L14bc", L14bc);
		fprintf(outfile, "%s %0.2lf\n", "L14bd", L14bd);
		fprintf(outfile, "%s %0.2lf\n", "L14ca", L14ca);
		fprintf(outfile, "%s %0.2lf\n", "L14cb", L14cb);
		fprintf(outfile, "%s %0.2lf\n", "L14cc", L14cc);
		fprintf(outfile, "%s %0.2lf\n", "L14cd", L14cd);
		fprintf(outfile, "%s %0.2lf\n", "L14da", L14da);
		fprintf(outfile, "%s %0.2lf\n", "L14db", L14db);
		fprintf(outfile, "%s %0.2lf\n", "L14dc", L14dc);
		fprintf(outfile, "%s %0.2lf\n", "L14dd", L14dd);
		fprintf(outfile, "%s %0.2lf\n", "L14ea", L14ea);
		fprintf(outfile, "%s %0.2lf\n", "L14eb", L14eb);
		fprintf(outfile, "%s %0.2lf\n", "L14ec", L14ec);
		fprintf(outfile, "%s %0.2lf\n", "L14ed", L14ed);

		for(i = 15; i <= 23; i++){
			fprintf(outfile, "SchdAI_%d%s %0.2lf\n", i, "a", a[i]);
			fprintf(outfile, "SchdAI_%d%s %0.2lf\n", i, "b", b[i]);
			fprintf(outfile, "SchdAI_%d%s %0.2lf\n", i, "c", c[i]);
			fprintf(outfile, "SchdAI_%d%s %0.2lf\n", i, "d", d[i]);
		}

		for(i = 1; i <= 11; i++){
			if((i != 3) && (i != 4) && (i != 5) && (i != 7))
				fprintf(outfile, "WSII_%d%s %0.2lf\n", i, "a", A[i]);
			fprintf(outfile, "WSII_%d%s %0.2lf\n", i, "b", B[i]);
			fprintf(outfile, "WSII_%d%s %0.2lf\n", i, "c", C[i]);
			if((i != 7) && (i != 9))
				fprintf(outfile, "WSII_%d%s %0.2lf\n", i, "d", D[i]);
		}
		fprintf(outfile, "WSII_%d %0.2lf     %s\n", 12, A[0], "PENALTY");
	
		if((A[8] == 0) && (B[8] == 0) && (C[8] == 0) && (D[8] == 0))
			fprintf(outfile, "As line 8 on WSII is zero for all payment periods, you don't owe a penalty.\n");
		else
			fprintf(outfile, "%s\n%s\n%s\n", "There is an underpayment for one or more periods.  See the  instructions and if", "you have not already done so, enter the number of days any payment was late", "into the GUI so this program can calculate the penalty.");
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
#undef INDIVIDUAL
#undef ESTATE
#undef Yes
#undef No
#undef NotApplicable
#undef Short
#undef Annualized

} // namespace taxsolve_CA_5805_2021
} // namespace OpenTaxSolver2021

