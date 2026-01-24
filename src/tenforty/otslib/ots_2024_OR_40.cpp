#include "ots_2024_routines.h"
namespace OpenTaxSolver2024 {
namespace taxsolve_OR_40_2024 {

#define MIN(i, j) (((i) < (j)) ? (i) : (j))
#define MAX(i, j) (((i) > (j)) ? (i) : (j))
/************************************************************************/
/* taxsolve_OR_40.c -                                                   */
/* Contributed by Rylan Luke, 3/2025                    */
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

// NOTE: Search for TAXYEAR_DEPENDENT to find likely entries
// which need to be updated for each tax year.

float thisversion=1.00;




// Arrays of TAX_TABLE entries are terminated with start = -1.
// Incrementing start values in array are required.
typedef struct TAX_TABLE {
    double start;   // Start of range definition; end is defined by next 'start' in array.
    double rate;    // Marginal percentage rate for this interval
} TAX_TABLE, *P_TAX_TABLE;

// TAXYEAR_DEPENDENT

// Tax breakpoint table for single or married filing separately.
TAX_TABLE or_40_single_tax_table[] = {
    { 0.00,      0.0475 },
    { 4300.00,   0.0675 },
    { 10750.00,  0.0875 },
    { 125000.00, 0.0990 },
    { -1.00,     0.00 },
};

// Tax breakpoint table for married/joint, head of household, surviving spouse
TAX_TABLE or_40_joint_tax_table[] = {
    { 0.00,      0.0475 },
    { 8600.00,   0.0675 },
    { 21500.00,  0.0875 },
    { 250000.00, 0.0990 },
    { -1.00,     0.00 },
};

double TaxFunction(double val, P_TAX_TABLE p_tax_table) {
    double retval;
    retval = 0.0;
    while (p_tax_table->start != -1.00) {
        // Find row corresponding to the input value

        // Get next start value; -1.00 means end so inhibit upper bound comparison
        double next_start = (p_tax_table + 1)->start;
        
        // At the same time, calculate a cumulative value. Round to nearest whole dollar each time
        // Only include from 'start' to the lower of the current value, or the next start
        double incr;
        double eval;
        if (next_start == -1.00) {
            eval = val;
        } else {
            eval = MIN(val, next_start);
        }
        incr = (eval - p_tax_table->start) * p_tax_table->rate;

        // First round the increment to the nearest cent, and then to the nearest dollar,
        // to match method of calculation used in the Oregon tax table generation.
        incr = round(100.0 * incr) / 100.0; // to nearest cent
        incr = round(incr);                 // to nearest dollar

        retval += incr;
        // printf("TaxFunction: val: %6.2f, incr: %6.2f, retval: %6.4f, next_start: %6.2f, start: %6.2f, rate: %6.2f\n", 
        //  val, incr, retval, next_start, p_tax_table->start, p_tax_table->rate);

        // Check for proper range, and halt if found. If the next_start is -1, don't do the upper bound comparison
        if (val >= p_tax_table->start && ((next_start == -1.0) || val < next_start))
            break;

        // Go to next line in tax table
        ++p_tax_table;
    }
    return retval;
}

// Tax table only spans up to this limit.
const double TAX_TABLE_LIMIT = 50000.00;

// Primary tax function, which looks up tax from the tax table if appropriate,
// or uses the generalized tax function if over the tax table limit. The table
// quantizes a range of values into a single, midpoint value, and the tax for
// the range is calculated based on the midpoint value. The first three ranges
// are 0 to 20 (mid = 10), 20 to 50 (mid = 35), and 50 to 100 (mid = 75).
// Subsequent values are every 100, with a midpoint of 50 in each span.

double TaxLookup(double val, P_TAX_TABLE p_tax_table) {

    double table_input_val;
    if (val < 20) {
        table_input_val = 10;
    } else if (val < 50) {
        table_input_val = 35;
    } else if (val < 100) {
        table_input_val = 75;
    } else if (val < TAX_TABLE_LIMIT) {
        table_input_val = (((int)val / 100) * 100) + 50;
    } else {
        table_input_val = val;
    }
    
    double tax;
    tax = TaxFunction(table_input_val, p_tax_table);
    // printf("TaxLookup: val: %6.2f, table_input_val: %6.2f, tax: %6.2f\n", val, table_input_val, tax );
    return tax;
}
// Return the marginal tax rate
double TaxRate(double val, P_TAX_TABLE p_tax_table) {
    while (p_tax_table->start != -1.00) {

        // Get next start value; -1.00 means end so inhibit upper bound comparison
        double next_start = (p_tax_table + 1)->start;
        
        if (val >= p_tax_table->start && ((next_start == -1.0) || val < next_start))
            return p_tax_table->rate;

        // Go to next line in tax table
        ++p_tax_table;
    }
    // Shouldn't get here, but return something
    return 0.0;
}

typedef struct LIMIT_TABLE {
    double start;   // This is the start of the range for the associated limit. End is defined by next 'start' value in array.
    double limit;   // This sets a maximum limit for some output value
} LIMIT_TABLE, *P_LIMIT_TABLE;

// TAXYEAR_DEPENDENT

// Single limit for fed tax subtraction
LIMIT_TABLE or_40_single_fed_sub_table[] = {
    { 0,      8250 },
    { 125000, 6600 },
    { 130000, 4950 },
    { 135000, 3300 },
    { 140000, 1650 },
    { 145000, 0    },
    { -1,     0    },
};

// Married/sep limit for fed tax subtraction
LIMIT_TABLE or_40_m_sep_fed_sub_table[] = {
    { 0,      4125 },
    { 125000, 3300 },
    { 130000, 2475 },
    { 135000, 1650 },
    { 140000, 825  },
    { 145000, 0    },
    { -1,     0    },
};

// Used for all other cases except single, and married filing separately:
// married/joint, head of household, and widow
LIMIT_TABLE or_40_other_fed_sub_table[] = {
    { 0,      8250 },
    { 250000, 6600 },
    { 260000, 4950 },
    { 270000, 3300 },
    { 280000, 1650 },
    { 290000, 0    },
    { -1,     0    },
};



// Returns a limit value based on an input variable. How the limit is used is up to the caller.
double GetLimit(double val, P_LIMIT_TABLE p_lim_table) {

    // Find row corresponding to the input val
    while (p_lim_table->start != -1.00) {
        // Get next start value; -1.00 means end so inhibit upper bound comparison
        double next_start = (p_lim_table + 1)->start;

        // printf("GetLimit: val: %6.2f, start: %6.2f, next_start: %6.2f, limit: %6.2f\n", 
        //  val, p_lim_table->start, next_start, p_lim_table->limit);
        
        // Check for proper range, and halt if found. If the next_start is -1, don't do the upper bound comparison
        if (val >= p_lim_table->start && ((next_start == -1.0) || val < next_start))
            break;

        // Go to next line in limit table
        ++p_lim_table;
    }

    return p_lim_table->limit;
}

// Associative structure for filing status and various limits and tax tables
typedef struct FILING_STATUS_CFG {
    char *status_match_string;
    char *status_label;
    char *checkbox_name;
    P_TAX_TABLE p_tax_table;
    P_LIMIT_TABLE p_fed_liability_limit;
    double std_deduction;
    double extra_std_deduction;
    double exemption_credit_limit;
    double income_and_property_tax_deduction_limit;
} FILING_STATUS_CFG, *P_FILING_STATUS_CFG;

// TAXYEAR_DEPENDENT
FILING_STATUS_CFG status_cfg[] = {
   { "SINGLE",        "Single",            "CkSingle", or_40_single_tax_table, or_40_single_fed_sub_table, 2745.00, 1200.00, 100000.00, 10000.0 },
   { "MARRIED/JOINT", "Married/Joint",     "CkMFJ",    or_40_joint_tax_table,  or_40_other_fed_sub_table,  5495.00, 1000.00, 200000.00, 10000.0 },
   { "MARRIED/SEP",   "Married/Separate",  "CkMFS",    or_40_single_tax_table, or_40_m_sep_fed_sub_table,  2745.00, 1000.00, 100000.00,  5000.0 },
   { "HEAD_OF_HOUSE", "Head of Household", "CkHH",     or_40_joint_tax_table,  or_40_other_fed_sub_table,  4420.00, 1200.00, 200000.00, 10000.0 },
   { "WIDOW",         "Surviving Spouse",  "CkQW",     or_40_joint_tax_table,  or_40_other_fed_sub_table,  5495.00, 1000.00, 200000.00, 10000.0 },
   { "",              NULL,                "",         NULL,                   NULL,                       0.00,    0.00,    0.0,       0.0 },
};

// These constants do not vary with filing status
const double EXEMPTION_CREDIT_DISABILITY_LIMIT = 100000.00;
const double EXEMPTION_CREDIT = 249.0;

//Get status configuration based on filing status match string, as capitalized from the f1040 import, and return it.
P_FILING_STATUS_CFG get_status_cfg(char *status) {
    P_FILING_STATUS_CFG p_cfg;
    p_cfg = status_cfg;
    while (strlen(p_cfg->status_match_string) > 0) {
        if (strstr(status, p_cfg->status_match_string) != NULL)
            return p_cfg;
        ++p_cfg;
    }
    return NULL;
}


//====== Form 1040 Import ======
// Imported form data; add elements as needed, using the same variable name as in the imported form label for clarity.
// Also update 'f1040_imp_defs' to add mapping.
typedef struct FED_1040_IMP_OR_40 {
        double L6b;     // Taxable social security benefits
        double L11;     // AGI (Adjusted gross income)
        double L22;     // Total tax before "other taxes, including self-employment tax"
        double S2_1a;   // Excess advance premium tax credit repayment
        double S2_8;    // Additional tax on IRAs or other tax-favored accounts
        double S2_16;   // Recapture of low-income housing credit
        double S2_10;   // Repayment of first-time homebuyer credit
        double L29;     // American Opportunity credit
        char *Status;
        char *Your1stName;
        char *YourLastName;
        char *YourSocSec;
        char *Spouse1stName;
        char *SpouseLastName;
        char *SpouseSocSec;
        char *NumberStreet;
        char *Apt;
        char *City;
        char *State;
        char *ZipCode;
        char *Dep1FirstName;
        char *Dep1LastName;
        char *Dep1SocSec;
        char *Dep2FirstName;
        char *Dep2LastName;
        char *Dep2SocSec;
        char *Dep3FirstName;
        char *Dep3LastName;
        char *Dep3SocSec;

} FED_1040_IMP_OR_40, P_FED_1040_IMP_OR_40;

FED_1040_IMP_OR_40 f1040i;

// Mapping from label name to address of local data struct element; either double or char*
static FORM_IMPORT_DEF f1040_imp_defs[] = {
        { "L6b",    &f1040i.L6b, NULL },
        { "L11",    &f1040i.L11, NULL },
        { "L22",    &f1040i.L22, NULL },
        { "L29",    &f1040i.L29, NULL },
        { "S2_1a",  &f1040i.S2_1a, NULL },
        { "S2_8",   &f1040i.S2_8, NULL },
        { "S2_10",  &f1040i.S2_10, NULL },
        { "S2_16",  &f1040i.S2_16, NULL },
        { "Status", NULL, &f1040i.Status },
        { "Your1stName:", NULL, &f1040i.Your1stName },
        { "YourLastName:", NULL, &f1040i.YourLastName },
        { "YourSocSec#:", NULL, &f1040i.YourSocSec },
        { "Spouse1stName:", NULL, &f1040i.Spouse1stName },
        { "SpouseLastName:", NULL, &f1040i.SpouseLastName },
        { "SpouseSocSec#:", NULL, &f1040i.SpouseSocSec },
        { "Number&Street:", NULL, &f1040i.NumberStreet },
        { "Apt#:", NULL, &f1040i.Apt },
        { "Town/City:", NULL, &f1040i.City },
        { "State:", NULL, &f1040i.State },
        { "ZipCode:", NULL, &f1040i.ZipCode },
        { "Dep1_FirstName:", NULL, &f1040i.Dep1FirstName },
        { "Dep1_LastName:", NULL, &f1040i.Dep1LastName },
        { "Dep1_SocSec#:", NULL, &f1040i.Dep1SocSec },
        { "Dep2_FirstName:", NULL, &f1040i.Dep2FirstName },
        { "Dep2_LastName:", NULL, &f1040i.Dep2LastName },
        { "Dep2_SocSec#:", NULL, &f1040i.Dep2SocSec },
        { "Dep3_FirstName:", NULL, &f1040i.Dep3FirstName },
        { "Dep3_LastName:", NULL, &f1040i.Dep3LastName },
        { "Dep3_SocSec#:", NULL, &f1040i.Dep3SocSec },
};

int f1040_imp_defs_size = sizeof(f1040_imp_defs)/sizeof(FORM_IMPORT_DEF);


/*----------------------------------------------------------------------------*/

// For the special formatting reqirements of OR-40, an integer value with
// commas is required.
void showline_wlabel_or_40(char *label, double value) {
    char buf[100];
    int cpos;
    int c;
    int val_int = (int)(value + 0.5); 
    fprintf(outfile, "%s = ", label);
    sprintf(buf, "%d", val_int);

    int buf_len = strlen(buf);
    // The starting position is adjusted to allow the space for the minus
    // sign if the number is negative.
    c = 2 - ((buf_len - ((val_int < 0) ? 1 : 0)) % 3);

    for (cpos = 0; cpos < buf_len; ++cpos) {
        fputc(buf[cpos], outfile);
        // Only process commas if the current character is not a minus sign.
        if (buf[cpos] != '-') {
            if (c == 1 && (cpos < buf_len - 1)) {
                fputc(',', outfile);
            }
            c = (c + 1) % 3;
        }
    }
    fputc('\n', outfile);
}

// Only show number if non-zero
void showline_wlabel_or_40_nz(char *label, double value) {
    if (value == 0) {
        return;
    } else {
        showline_wlabel_or_40(label, value);
    }
}

// Show number from 'L' array
void shownum_or_40(int j) {
    char buf[100];
    sprintf(buf, "L%d", j);
    showline_wlabel_or_40(buf, L[j]);
}

// Show number from 'L' array, if non-zero
void shownum_or_40_nz(int j) {
    if (L[j] == 0.0)
        return;
    char buf[100];
    sprintf(buf, "L%d", j);
    showline_wlabel_or_40(buf, L[j]);
}


// Separate the combined first name and initial into separate fields, and print them
// to the output file, using the field labels provided.
void show_fname_init_or_40(char *combined_name, char *first_name_label, char *initial_label) {

    // If combined_name ends in a space followed by a single character, treat it as initial
    char *first_name;
    char *initial;

    int comb_len = strlen(combined_name);

    first_name = strdup(combined_name);
    initial = strdup(combined_name);

    // Look for second to last character is a space, and last character is not a space;
    if ((*(combined_name + comb_len - 2) == ' ') && (*(combined_name + comb_len - 1) != ' ')) {
        *initial = *(combined_name + comb_len - 1);
        *(initial + 1) = '\0';
        // printf("show_fname_init_or_40: '%s'\n", initial);
        strncpy(first_name, combined_name, comb_len - 2);
        *(first_name + comb_len - 1) = '\0';
    } else {
        // No initial found; return full first name, and empty initial
        strcpy(first_name, combined_name);
        *initial = '\0';
    }

    fprintf(outfile, "%s %s\n", first_name_label, first_name);
    if (strlen(initial) > 0)
        fprintf(outfile, "%s %s\n", initial_label, initial);
}

// Format and print a social security number to the output file, inserting
// spaces, and ignoring any input characters (e.g. ' ' or '-') which are not
// numeric digits.
void show_ssn_or_40(char *label, char *ssn) {
    fprintf(outfile, "%s ", label);
    int cpos;
    int digit_pos = 0;  // Position counting digits only in input number
    for (cpos = 0; cpos < strlen(ssn); ++cpos) {
        // Skip over any non-digit characters, and don't count them as
        // digit_pos
        if (isdigit(*(ssn + cpos))) {
            // If we have reached the third or fifth digit (zero-based),
            // insert a space beforehand.
            if ((digit_pos == 3) || (digit_pos == 5))
                fputc(' ', outfile);
            fputc(*(ssn + cpos), outfile);
            ++digit_pos;
        }
    }
    fputc('\n', outfile);
}


int main( int argc, char *argv[] )
{
 int i, j, k;
 char word[6000], outfname[6000], *infname=0;
 time_t now;
 // Local line number variables for direct/indirect lines

 printf("Oregon Form OR-40 - 2024 - v%3.2f\n", thisversion);

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
 check_form_version( word, "Title: Oregon Form OR-40" );

 /* ----- Accept form data and process the numbers.         ------ */
 /* ----- Place all your form-specific code below here .... ------ */

#undef TAX_TABLE_TEST
#ifdef TAX_TABLE_TEST
    printf("Tax Table\n");
    printf("Val\tSingleTax\tJointTax\n");
    double v;
    for (v = 0.0; v <= 50000; v+= 10.0) {
        if ((v == 0.0) || (v == 20.0) || (v == 50.0) || (((int)v % 100) == 0)) 
            printf("%6.2f\t%6.2f\t%6.2f\n", v, TaxLookup(v, or_40_single_tax_table), TaxLookup(v, or_40_joint_tax_table));
    }

    for (v = 49000; v <= 65000; v+= 10.0) {
        printf("%6.2f\t%6.2f\t%6.2f\n", v, TaxLookup(v, or_40_single_tax_table), TaxLookup(v, or_40_joint_tax_table));
    }
#endif

    char *f1040_filename = GetTextLine( "FileName1040:") ;

    if (strlen(f1040_filename) != 0) {
        ImportReturnData( f1040_filename, f1040_imp_defs, f1040_imp_defs_size);

        int d;
        P_FORM_IMPORT_DEF pdef;
        for (d = 0; d < f1040_imp_defs_size; ++d) {
            pdef = &f1040_imp_defs[d];
            if ( pdef->p_field_string != NULL )
                 // After import, convert strings to upper case, in place.
                capitalize( *pdef->p_field_string );
        }

        fprintf( outfile, "INFO: --- Imported 1040 Data from file '%s' ---\n", f1040_filename);
        fprintf( outfile, "INFO: --- All strings converted to upper case ---\n");
        fprintf( outfile, "INFO: f1040i.L6b   -- %6.2f\n", f1040i.L6b);
        fprintf( outfile, "INFO: f1040i.L11   -- %6.2f\n", f1040i.L11);
        fprintf( outfile, "INFO: f1040i.L22   -- %6.2f\n", f1040i.L22);
        fprintf( outfile, "INFO: f1040i.S2_1a   -- %6.2f\n", f1040i.S2_1a);
        fprintf( outfile, "INFO: f1040i.S2_8   -- %6.2f\n", f1040i.S2_8);
        fprintf( outfile, "INFO: f1040i.S2_10   -- %6.2f\n", f1040i.S2_10);
        fprintf( outfile, "INFO: f1040i.S2_16   -- %6.2f\n", f1040i.S2_16);
        fprintf( outfile, "INFO: f1040i.L29   -- %6.2f\n", f1040i.L29);
        fprintf( outfile, "INFO: f1040i.Status: -- %s\n", f1040i.Status);
        fprintf( outfile, "INFO: f1040i.Your1stName: -- %s\n", f1040i.Your1stName);
        fprintf( outfile, "INFO: f1040i.YourLastName: -- %s\n", f1040i.YourLastName);
        fprintf( outfile, "INFO: f1040i.YourSocSec#: -- %s\n", f1040i.YourSocSec);
        fprintf( outfile, "INFO: f1040i.Spouse1stName: -- %s\n", f1040i.Spouse1stName);
        fprintf( outfile, "INFO: f1040i.SpouseLastName: -- %s\n", f1040i.SpouseLastName);
        fprintf( outfile, "INFO: f1040i.SpouseSocSec#: -- %s\n", f1040i.SpouseSocSec);
        fprintf( outfile, "INFO: f1040i.Number&Street: -- %s\n", f1040i.NumberStreet);
        fprintf( outfile, "INFO: f1040i.Apt#: -- %s\n", f1040i.Apt);
        fprintf( outfile, "INFO: f1040i.Town/City: -- %s\n", f1040i.City);
        fprintf( outfile, "INFO: f1040i.State: -- %s\n", f1040i.State);
        fprintf( outfile, "INFO: f1040i.ZipCode: -- %s\n", f1040i.ZipCode);
        fprintf( outfile, "INFO: f1040i.Dep1FirstName: -- %s\n", f1040i.Dep1FirstName);
        fprintf( outfile, "INFO: f1040i.Dep1LastName: -- %s\n", f1040i.Dep1LastName);
        fprintf( outfile, "INFO: f1040i.Dep1SocSec: -- %s\n", f1040i.Dep1SocSec);
        fprintf( outfile, "INFO: f1040i.Dep2FirstName: -- %s\n", f1040i.Dep2FirstName);
        fprintf( outfile, "INFO: f1040i.Dep2LastName: -- %s\n", f1040i.Dep2LastName);
        fprintf( outfile, "INFO: f1040i.Dep2SocSec: -- %s\n", f1040i.Dep2SocSec);
        fprintf( outfile, "INFO: f1040i.Dep3FirstName: -- %s\n", f1040i.Dep3FirstName);
        fprintf( outfile, "INFO: f1040i.Dep3LastName: -- %s\n", f1040i.Dep3LastName);
        fprintf( outfile, "INFO: f1040i.Dep3SocSec: -- %s\n", f1040i.Dep3SocSec);
    } else {
        fprintf( outfile, "ERROR: --- No Imported 1040 Form Data : no filename provided ---\n");
        exit(1);
    }

    //====== Page 1 ======

    //=== Fiscal Year, Amended Return, Special Status ===

    GetTextLineF( "FiscalYearEnd:") ;

    check_if_yes("CkAmended");

    GetTextLineF( "NOLTaxYear:") ;

    check_if_yes("CkCalcAsIfFed");
    check_if_yes("CkShortYear");
    check_if_yes("CkExtFiled");
    check_if_yes("CkFormOR24");
    check_if_yes("CkFormOR243");
    check_if_yes("CkFedForm8379");
    check_if_yes("CkFedForm8886");
    check_if_yes("CkDisasterRelief");

    //=== Identity-Information ===

    fprintf(outfile, "YourLName: %s\n", f1040i.YourLastName);
    show_ssn_or_40("YourSSN:", f1040i.YourSocSec);


    show_fname_init_or_40(f1040i.Your1stName, "YourFName:", "YourInit:");

    show_fname_init_or_40(f1040i.Spouse1stName, "SpouseFName:", "SpouseInit:");
    fprintf(outfile, "SpouseLName: %s\n", f1040i.SpouseLastName);
    show_ssn_or_40("SpouseSSN:", f1040i.SpouseSocSec);

    GetTextLineF( "YourDOB:") ;

    check_if_yes("CkYFirstSSN");
    check_if_yes("CkYAppITIN");
    check_if_yes("CkYDeceased");

    GetTextLineF( "SpouseDOB:") ;

    check_if_yes("CkSFirstSSN");
    check_if_yes("CkSAppITIN");
    check_if_yes("CkSDeceased");

    char *apt_prefix = "";
    if (strlen(f1040i.Apt) > 0)
        apt_prefix = " #";
    fprintf(outfile, "Address: %s%s%s\n", f1040i.NumberStreet, apt_prefix, f1040i.Apt);

    fprintf(outfile, "City: %s\n", f1040i.City);
    fprintf(outfile, "State: %s\n", f1040i.State);
    fprintf(outfile, "Zip: %s\n", f1040i.ZipCode);

    GetTextLineF( "Country:");
    GetTextLineF( "Phone:");

    //=== Filing Status ===
    // Filing status configuration pointer.
    P_FILING_STATUS_CFG p_cfg = get_status_cfg(f1040i.Status);

    if (p_cfg == NULL) {
        fprintf(outfile,"Error: unrecognized filing status '%s'. Check form1040 imported info. Exiting.\n", f1040i.Status); 
        exit(1);
    }

    fprintf(outfile, "INFO: filing_status: '%s'\n", p_cfg->status_label);
    
    // Set the filing status checkbox
    fprintf(outfile, "%s X\n", p_cfg->checkbox_name);

    //====== Page 2 ======

    //=== Exemptions ===

    // Your exemptions
    int b6aRegular;
    int b6aDisabled;
    int b6aDependent;

    GetYesNoSL("CkL6aRegular", &b6aRegular);
    GetYesNoSL("CkL6aDisabled", &b6aDisabled);
    GetYesNoSL("CkL6aDep", &b6aDependent);

    double L6a;
    L6a = b6aRegular + b6aDisabled + b6aDependent;
    showline_wlabel_or_40("L6a", L6a);

    if (b6aRegular)
        fprintf(outfile, "CkL6aRegular X\n");
    if (b6aDisabled)
        fprintf(outfile, "CkL6aDisabled X\n");
    if (b6aDependent)
        fprintf(outfile, "CkL6aDep X\n");

    if (b6aRegular && b6aDependent) {
        fprintf( outfile, "ERROR: --- Only one of L6a 'Regular' and 'Dependent' checkboxes may be selected ---\n");
        exit(1);
    }


    // Spouse exemptions
    int b6bRegular;
    int b6bDisabled;
    int b6bDependent;

    GetYesNoSL("CkL6bRegular", &b6bRegular);
    GetYesNoSL("CkL6bDisabled", &b6bDisabled);
    GetYesNoSL("CkL6bDep", &b6bDependent);

    double L6b;
    L6b = b6bRegular + b6bDisabled + b6bDependent;
    showline_wlabel_or_40_nz("L6b", L6b);

    if (b6bRegular)
        fprintf(outfile, "CkL6bRegular X\n");
    if (b6bDisabled)
        fprintf(outfile, "CkL6bDisabled X\n");
    if (b6bDependent)
        fprintf(outfile, "CkL6bDep X\n");

    if (b6bRegular && b6bDependent) {
        fprintf( outfile, "ERROR: --- Only one of L6b 'Regular' and 'Dependent' checkboxes may be selected ---\n");
        exit(1);
    }

    //=== Dependents ===

    show_fname_init_or_40(f1040i.Dep1FirstName, "Dep1FirstName:", "Dep1Initial:");
    fprintf(outfile, "Dep1LastName: %s\n", f1040i.Dep1LastName);
    show_ssn_or_40("Dep1SSN:", f1040i.Dep1SocSec);
    GetTextLineF("Dep1DOB:");
    GetTextLineF("Dep1Code:");
    check_if_yes("CkDep1Dis");

    show_fname_init_or_40(f1040i.Dep2FirstName, "Dep2FirstName:", "Dep2Initial:");
    fprintf(outfile, "Dep2LastName: %s\n", f1040i.Dep2LastName);
    show_ssn_or_40("Dep2SSN:", f1040i.Dep2SocSec);
    GetTextLineF("Dep2DOB:");
    GetTextLineF("Dep2Code:");
    check_if_yes("CkDep2Dis");

    show_fname_init_or_40(f1040i.Dep3FirstName, "Dep3FirstName:", "Dep3Initial:");
    fprintf(outfile, "Dep3LastName: %s\n", f1040i.Dep3LastName);
    show_ssn_or_40("Dep3SSN:", f1040i.Dep3SocSec);
    GetTextLineF("Dep3DOB:");
    GetTextLineF("Dep3Code:");
    check_if_yes("CkDep3Dis");

    // Total number of dependents
    double L6c;
    GetLine("L6c", &L6c);
    showline_wlabel_or_40("L6c", L6c);

    // Total number of dependent children with a qualifying disability
    double L6d;
    GetLine("L6d", &L6d);
    showline_wlabel_or_40("L6d", L6d);

    // Total number of exemptions
    double L6e;
    L6e = L6a + L6b + L6c + L6d;
    showline_wlabel_or_40("L6e", L6e);

    // From this point forward, calculations must be deferred until all
    // supporting schedule data is available. See template file for ordering
    // and field descriptions.
    //====== Page 3 ======

    double L10_worksheet_L4;
    double L10_worksheet_L7;
    GetLine("L10_worksheet_L4", &L10_worksheet_L4);
    GetLine("L10_worksheet_L7", &L10_worksheet_L7);


    GetLine("L12", &L[12]);

    int bStdDedOverride;
    double StdDedOverride;

    GetYesNoSL("CkStdDedOverride", &bStdDedOverride);
    GetLine("StdDedOverride", &StdDedOverride);

    int bL17a;
    int bL17b;
    int bL17c;
    int bL17d;
    
    GetYesNoSL("CkL17a", &bL17a);
    GetYesNoSL("CkL17b", &bL17b);
    GetYesNoSL("CkL17c", &bL17c);
    GetYesNoSL("CkL17d", &bL17d);

    //====== Page 4 ======
    
    int b20a;
    int b20b;
    int b20c;
    
    GetYesNoSL("CkL20a", &b20a);
    GetYesNoSL("CkL20b", &b20b);
    GetYesNoSL("CkL20c", &b20c);

    double AltMethodTaxAmt;
    GetLine("AltMethodTaxAmt", &AltMethodTaxAmt);

    GetLine("L21", &L[21]);

    GetLine("L26", &L[26]);

    //====== Page 5 ======

    GetLine("L32", &L[32]);
    GetLine("L33", &L[33]);
    GetLine("L34", &L[34]);
    GetLine("L35", &L[35]);
    GetLine("L36", &L[36]);
    GetLine("L37", &L[37]);

    GetLine("L43", &L[43]);
    GetLine("L44", &L[44]);

    double L44a;
    GetLine("L44a", &L44a);

    int bL44b;
    GetYesNoSL("CkL44b", &bL44b);

    //====== Page 6 ======

    GetLine("L48", &L[48]);
    GetLine("L49", &L[49]);
    GetLine("L50", &L[50]);

    char *L50a;
    char *L50b;
    L50a = GetTextLine("L50a:");
    L50b = GetTextLine("L50b:");

    GetLine("L51", &L[51]);

    int bL54DepOutUS;
    GetYesNoSL("CkL54DepOutUS", &bL54DepOutUS);

    int bAcctChecking;
    GetYesNoSL("CkAcctChecking", &bAcctChecking);

    int bAcctSavings;
    GetYesNoSL("CkAcctSavings", &bAcctSavings);

    char *AcctRoutingNumber;
    char *AcctNumber;
    AcctRoutingNumber = GetTextLine("AcctRoutingNumber:");
    AcctNumber = GetTextLine("AcctNumber:");
    // There may be letters in account number, so convert to upper case.
    capitalize(AcctNumber);

    //====== Oregon Schedule OR-A Data ======

    //====== OR-A Page 1 ======

    #define SCH_A_LINES 23
    double SchA_L[SCH_A_LINES + 1]; // Allow 1 extra for 1-based counting
    char *SchA_L10_Type;
    char *SchA_L22_Type;

    GetLine("SchA_L1", &SchA_L[1]);
    GetLine("SchA_L5", &SchA_L[5]);
    GetLine("SchA_L6", &SchA_L[6]);
    GetLine("SchA_L7", &SchA_L[7]);
    GetLine("SchA_L10", &SchA_L[10]);
    SchA_L10_Type = GetTextLine("SchA_L10_Type:");
    capitalize(SchA_L10_Type);

    //====== OR-A Page 2 ======

    GetLine("SchA_L12", &SchA_L[12]);
    GetLine("SchA_L13", &SchA_L[13]);
    GetLine("SchA_L14", &SchA_L[14]);
    GetLine("SchA_L16", &SchA_L[16]);
    GetLine("SchA_L18", &SchA_L[18]);
    GetLine("SchA_L19", &SchA_L[19]);
    GetLine("SchA_L20", &SchA_L[20]);
    GetLine("SchA_L22", &SchA_L[22]);
    SchA_L22_Type = GetTextLine("SchA_L22_Type:");
    capitalize(SchA_L22_Type);

    //====== Oregon Schedule OR-ASC Data ======

    //====== OR-ASC Page 1 ======

    // A section
    #define SCH_ASC_A_FIELDS 5
    double SchASC_A[SCH_ASC_A_FIELDS + 1];
    char *SchASC_A1;
    char *SchASC_A3;

    SchASC_A1 = GetTextLine("SchASC_A1:");
    capitalize(SchASC_A1);
    GetLine("SchASC_A2", &SchASC_A[2]);
    SchASC_A3 = GetTextLine("SchASC_A3:");
    capitalize(SchASC_A3);
    GetLine("SchASC_A4", &SchASC_A[4]);

    // B section
    #define SCH_ASC_B_FIELDS 7
    double SchASC_B[SCH_ASC_B_FIELDS + 1];
    char *SchASC_B1;
    char *SchASC_B3;
    char *SchASC_B5;

    SchASC_B1 = GetTextLine("SchASC_B1:");
    capitalize(SchASC_B1);
    GetLine("SchASC_B2", &SchASC_B[2]);
    SchASC_B3 = GetTextLine("SchASC_B3:");
    capitalize(SchASC_B3);
    GetLine("SchASC_B4", &SchASC_B[4]);
    SchASC_B5 = GetTextLine("SchASC_B5:");
    capitalize(SchASC_B5);
    GetLine("SchASC_B6", &SchASC_B[6]);

    // C section
    #define SCH_ASC_C_FIELDS 5
    double SchASC_C[SCH_ASC_C_FIELDS + 1];
    char *SchASC_C1;
    char *SchASC_C3;

    SchASC_C1 = GetTextLine("SchASC_C1:");
    capitalize(SchASC_C1);
    GetLine("SchASC_C2", &SchASC_C[2]);
    SchASC_C3 = GetTextLine("SchASC_C3:");
    capitalize(SchASC_C3);
    GetLine("SchASC_C4", &SchASC_C[4]);

    //====== OR-ASC Page 2 ======

    // D section
    #define SCH_ASC_D_FIELDS 16 
    double SchASC_D[SCH_ASC_D_FIELDS + 1];
    char *SchASC_D1;
    char *SchASC_D2;
    char *SchASC_D4;
    char *SchASC_D5;
    char *SchASC_D7;
    char *SchASC_D8;
    char *SchASC_D10;
    char *SchASC_D11;
    char *SchASC_D13;
    char *SchASC_D14;

    SchASC_D1 = GetTextLine("SchASC_D1:");
    capitalize(SchASC_D1);
    SchASC_D2 = GetTextLine("SchASC_D2:");
    capitalize(SchASC_D2);
    GetLine("SchASC_D3", &SchASC_D[3]);
    SchASC_D4 = GetTextLine("SchASC_D4:");
    capitalize(SchASC_D4);
    SchASC_D5 = GetTextLine("SchASC_D5:");
    capitalize(SchASC_D5);
    GetLine("SchASC_D6", &SchASC_D[6]);
    SchASC_D7 = GetTextLine("SchASC_D7:");
    capitalize(SchASC_D7);
    SchASC_D8 = GetTextLine("SchASC_D8:");
    capitalize(SchASC_D8);
    GetLine("SchASC_D9", &SchASC_D[9]);
    SchASC_D10 = GetTextLine("SchASC_D10:");
    capitalize(SchASC_D10);
    SchASC_D11 = GetTextLine("SchASC_D11:");
    capitalize(SchASC_D11);
    GetLine("SchASC_D12", &SchASC_D[12]);
    SchASC_D13 = GetTextLine("SchASC_D13:");
    capitalize(SchASC_D13);
    SchASC_D14 = GetTextLine("SchASC_D14:");
    capitalize(SchASC_D14);
    GetLine("SchASC_D15", &SchASC_D[15]);

    // E section
    #define SCH_ASC_E_FIELDS 9
    double SchASC_E[SCH_ASC_E_FIELDS + 1];
    char *SchASC_E1;
    char *SchASC_E5;

    SchASC_E1 = GetTextLine("SchASC_E1:");
    capitalize(SchASC_E1);
    GetLine("SchASC_E2", &SchASC_E[2]);
    GetLine("SchASC_E3", &SchASC_E[3]);
    GetLine("SchASC_E4", &SchASC_E[4]);
    SchASC_E5 = GetTextLine("SchASC_E5:");
    capitalize(SchASC_E5);
    GetLine("SchASC_E6", &SchASC_E[6]);
    GetLine("SchASC_E7", &SchASC_E[7]);
    GetLine("SchASC_E8", &SchASC_E[8]);

    //====== OR-ASC Page 3 ======

    // F section
    #define SCH_ASC_F_FIELDS 7
    double SchASC_F[SCH_ASC_F_FIELDS + 1];
    char *SchASC_F1;
    char *SchASC_F3;
    char *SchASC_F5;

    SchASC_F1 = GetTextLine("SchASC_F1:");
    capitalize(SchASC_F1);
    GetLine("SchASC_F2", &SchASC_F[2]);
    SchASC_F3 = GetTextLine("SchASC_F3:");
    capitalize(SchASC_F3);
    GetLine("SchASC_F4", &SchASC_F[4]);
    SchASC_F5 = GetTextLine("SchASC_F5:");
    capitalize(SchASC_F5);
    GetLine("SchASC_F6", &SchASC_F[6]);

    //====== End of input file data reads ======

    // Now that all data has been accumulated, calculations can be begin on OR-40, and schedules.
    // First calculated and display OR-A and OR-ASC, as they create values used in the main OR-40 form
    // Get fed AGI into OR-40 form array before calculating OR-A
    L[7] = round(f1040i.L11);
    double fed_agi = L[7];
    

    //====== OR-A Calculations and output ======

    showline_wlabel_or_40_nz("SchA_L1", SchA_L[1]);
    SchA_L[2] = fed_agi;
    showline_wlabel_or_40_nz("SchA_L2", SchA_L[2]);
    SchA_L[3] = round(SchA_L[2] * 0.075);
    showline_wlabel_or_40_nz("SchA_L3", SchA_L[3]);
    SchA_L[4] = NotLessThanZero(SchA_L[1]- SchA_L[3]);
    showline_wlabel_or_40("SchA_L4", SchA_L[4]);
    showline_wlabel_or_40_nz("SchA_L5", SchA_L[5]);
    showline_wlabel_or_40_nz("SchA_L6", SchA_L[6]);
    showline_wlabel_or_40_nz("SchA_L7", SchA_L[7]);

    // Sum of income and property tax deduction.
    double SchA_L9_Sum; 
    SchA_L9_Sum = SchA_L[5] + SchA_L[6] + SchA_L[7] + SchA_L[8]; 
    SchA_L[9] = SmallerOf(SchA_L9_Sum, p_cfg->income_and_property_tax_deduction_limit);
    showline_wlabel_or_40_nz("SchA_L9", SchA_L[9]);

    // If limited, show info message.
    if (SchA_L9_Sum > p_cfg->income_and_property_tax_deduction_limit) {
        fprintf(outfile, "INFO: Total income and property taxes on Schedule A = %8.2f has been limited to %8.2f\n", 
            SchA_L9_Sum, SchA_L[9]);
    }

    showline_wlabel_or_40_nz("SchA_L10", SchA_L[10]);
    fprintf(outfile, "SchA_L10_Type: %s\n", SchA_L10_Type);

    SchA_L[11] = SchA_L[9] + SchA_L[10];
    showline_wlabel_or_40_nz("SchA_L11", SchA_L[11]);

    showline_wlabel_or_40_nz("SchA_L12", SchA_L[12]);
    showline_wlabel_or_40_nz("SchA_L13", SchA_L[13]);
    showline_wlabel_or_40_nz("SchA_L14", SchA_L[14]);
    showline_wlabel_or_40_nz("SchA_L16", SchA_L[16]);

    SchA_L[17] = SchA_L[12] + SchA_L[13] + SchA_L[14] + SchA_L[16];
    showline_wlabel_or_40_nz("SchA_L17", SchA_L[17]);

    showline_wlabel_or_40_nz("SchA_L18", SchA_L[18]);
    showline_wlabel_or_40_nz("SchA_L19", SchA_L[19]);
    showline_wlabel_or_40_nz("SchA_L20", SchA_L[20]);

    SchA_L[21] = SchA_L[18] + SchA_L[19] + SchA_L[20];
    showline_wlabel_or_40_nz("SchA_L21", SchA_L[21]);

    showline_wlabel_or_40_nz("SchA_L22", SchA_L[22]);
    fprintf(outfile, "SchA_L22_Type: %s\n", SchA_L22_Type);

    // Finally, the sum of all deductions
    SchA_L[23] = SchA_L[4] + SchA_L[11] + SchA_L[17] + SchA_L[21] + SchA_L[22];
    showline_wlabel_or_40_nz("SchA_L23", SchA_L[23]);

    //====== OR-ASC Calculations and output ======
    // Section A
    fprintf(outfile, "SchASC_A1: %s\n", SchASC_A1);
    showline_wlabel_or_40_nz("SchASC_A2", SchASC_A[2]);

    fprintf(outfile, "SchASC_A3: %s\n", SchASC_A3);
    showline_wlabel_or_40_nz("SchASC_A4", SchASC_A[4]);

    SchASC_A[5] = SchASC_A[2] + SchASC_A[4];
    showline_wlabel_or_40_nz("SchASC_A5", SchASC_A[5]);

    // Section B
    fprintf(outfile, "SchASC_B1: %s\n", SchASC_B1);
    showline_wlabel_or_40_nz("SchASC_B2", SchASC_B[2]);

    fprintf(outfile, "SchASC_B3: %s\n", SchASC_B3);
    showline_wlabel_or_40_nz("SchASC_B4", SchASC_B[4]);

    fprintf(outfile, "SchASC_B5: %s\n", SchASC_B5);
    showline_wlabel_or_40_nz("SchASC_B6", SchASC_B[6]);

    SchASC_B[7] = SchASC_B[2] + SchASC_B[4] + SchASC_B[6];
    showline_wlabel_or_40_nz("SchASC_B7", SchASC_B[7]);

    // Section C
    fprintf(outfile, "SchASC_C1: %s\n", SchASC_C1);
    showline_wlabel_or_40_nz("SchASC_C2", SchASC_C[2]);

    fprintf(outfile, "SchASC_C3: %s\n", SchASC_C3);
    showline_wlabel_or_40_nz("SchASC_C4", SchASC_C[4]);

    SchASC_C[5] = SchASC_C[2] + SchASC_C[4];
    showline_wlabel_or_40_nz("SchASC_C5", SchASC_C[5]);

    // Section D
    fprintf(outfile, "SchASC_D1: %s\n", SchASC_D1);
    fprintf(outfile, "SchASC_D2: %s\n", SchASC_D2);
    showline_wlabel_or_40_nz("SchASC_D3", SchASC_D[3]);

    fprintf(outfile, "SchASC_D4: %s\n", SchASC_D4);
    fprintf(outfile, "SchASC_D5: %s\n", SchASC_D5);
    showline_wlabel_or_40_nz("SchASC_D6", SchASC_D[6]);

    fprintf(outfile, "SchASC_D7: %s\n", SchASC_D7);
    fprintf(outfile, "SchASC_D8: %s\n", SchASC_D8);
    showline_wlabel_or_40_nz("SchASC_D9", SchASC_D[9]);

    fprintf(outfile, "SchASC_D10: %s\n", SchASC_D10);
    fprintf(outfile, "SchASC_D11: %s\n", SchASC_D11);
    showline_wlabel_or_40_nz("SchASC_D12", SchASC_D[12]);

    fprintf(outfile, "SchASC_D13: %s\n", SchASC_D13);
    fprintf(outfile, "SchASC_D14: %s\n", SchASC_D14);
    showline_wlabel_or_40_nz("SchASC_D15", SchASC_D[15]);

    SchASC_D[16] = SchASC_D[3] + SchASC_D[6] + SchASC_D[9] + SchASC_D[12] + SchASC_D[15];
    showline_wlabel_or_40_nz("SchASC_D16", SchASC_D[16]);

    // Section E
    fprintf(outfile, "SchASC_E1: %s\n", SchASC_E1);
    showline_wlabel_or_40_nz("SchASC_E2", SchASC_E[2]);
    showline_wlabel_or_40_nz("SchASC_E3", SchASC_E[3]);
    // Limit the 'Total used this year' to be not more than the sum of prior and this year values
    double SchASC_E1_prior_this_sum;
    SchASC_E1_prior_this_sum =  SchASC_E[2] +  SchASC_E[3];
    if (SchASC_E[4] > SchASC_E1_prior_this_sum) {
        fprintf(outfile, "Schedule ASC Section E line E4 has been limited to the sum of E2 and E3: %8.2f\n", SchASC_E1_prior_this_sum);
    }
    SchASC_E[4] = SmallerOf(SchASC_E[4], SchASC_E1_prior_this_sum);
    showline_wlabel_or_40_nz("SchASC_E4", SchASC_E[4]);

    fprintf(outfile, "SchASC_E5: %s\n", SchASC_E5);
    showline_wlabel_or_40_nz("SchASC_E6", SchASC_E[6]);
    showline_wlabel_or_40_nz("SchASC_E7", SchASC_E[7]);
    // Limit the 'Total used this year' to be not more than the sum of prior and this year values
    double SchASC_E5_prior_this_sum;
    SchASC_E5_prior_this_sum =  SchASC_E[6] +  SchASC_E[7];

    if (SchASC_E[8] > SchASC_E5_prior_this_sum) {
        fprintf(outfile, "Schedule ASC Section E line E8 has been limited to the sum of E6 and E7: %8.2f\n", SchASC_E5_prior_this_sum);
    }
    SchASC_E[8] = SmallerOf(SchASC_E[8], SchASC_E5_prior_this_sum);
    showline_wlabel_or_40_nz("SchASC_E8", SchASC_E[8]);

    SchASC_E[9] = SchASC_E[4] + SchASC_E[8];
    showline_wlabel_or_40_nz("SchASC_E9", SchASC_E[9]);

    // Section F
    fprintf(outfile, "SchASC_F1: %s\n", SchASC_F1);
    showline_wlabel_or_40_nz("SchASC_F2", SchASC_F[2]);

    fprintf(outfile, "SchASC_F3: %s\n", SchASC_F3);
    showline_wlabel_or_40_nz("SchASC_F4", SchASC_F[4]);

    fprintf(outfile, "SchASC_F5: %s\n", SchASC_F5);
    showline_wlabel_or_40_nz("SchASC_F6", SchASC_F[6]);

    SchASC_F[7] = SchASC_F[2] + SchASC_F[4] + SchASC_F[6];
    showline_wlabel_or_40_nz("SchASC_F7", SchASC_F[7]);

    //=== End of supporting schedules ===

    //====== Finish OR-40 form calculations and display ======
    //=== Taxable Income ===

    shownum_or_40(7);

    // Additions from OR-ASC, line A5
    // GetLine("L8", &L[8]);
    L[8] = SchASC_A[5];
    shownum_or_40_nz(8);

    L[9] = L[7] + L[8];
    shownum_or_40(9);

    //=== Subtractions from Taxable Income ===

    // Federal Tax Worksheet
    #define FTW_NUM_LINES 11
    double ftw_L[FTW_NUM_LINES + 1];    // Add one extra to allow 1-based; [0] entry is ignored.

    char *ftw_line_description[FTW_NUM_LINES + 1] = {
        "", // Line[0] not used
        "Federal tax liability (Form 1040 line 22)",
        "Excess advance premium tax credit (Form 1040, Schedule 2, line 1a)",
        "Line 1 minus line 2. (If less than 0, enter 0)",
        "Other taxes (see instructions)",
        "Line 3 plus line 4",
        "American Opportunity credit (form 1040, line 29)",
        "Premium tax credit (Form 8962, line 24)",
        "Line 6 plus line 7",
        "Line 5 minus line 8. (If less than 0, enter 0)",
        "Maximum subtraction amount from Table 4",
        "Smaller of line 9 or line 10.  This is your federal tax liability subtraction"
    };

    ftw_L[1] = round(f1040i.L22);
    ftw_L[2] = round(f1040i.S2_1a);
    ftw_L[3] = NotLessThanZero(ftw_L[1] - ftw_L[2]);
    ftw_L[4] = L10_worksheet_L4;
    ftw_L[5] = ftw_L[3] + ftw_L[4];
    ftw_L[6] = round(f1040i.L29);
    ftw_L[7] = L10_worksheet_L7;
    ftw_L[8] = ftw_L[6] + ftw_L[7];
    ftw_L[9] = NotLessThanZero(ftw_L[5] - ftw_L[8]);
    ftw_L[10] = GetLimit(fed_agi, p_cfg->p_fed_liability_limit);
    ftw_L[11] = SmallerOf(ftw_L[9], ftw_L[10]);

    int line;
    fprintf( outfile, "INFO: Federal tax liability subtraction worksheet\n" );
    for (line = 1; line <= FTW_NUM_LINES; ++line) {
        fprintf( outfile, "INFO: --- Line %2d: %9.2f : '%s'\n", line, ftw_L[line], ftw_line_description[line]);
    }
    
    // Federal tax liability allowed for subtraction, from line 11 of worksheet.
    L[10] = ftw_L[11];
    shownum_or_40(10);

    // Taxable social security amount
    L[11] = round(f1040i.L6b);
    shownum_or_40_nz(11);

    // Oregon income tax refund
    shownum_or_40_nz(12);

    // Subtractions from OR-ASC, line B7 
    L[13] = SchASC_B[7];
    shownum_or_40_nz(13);

    // Total subtractions
    L[14] = L[10] + L[11] + L[12] + L[13];
    shownum_or_40(14);

    // Income after subtractions
    L[15] = L[9] - L[14];
    shownum_or_40(15);

    // Itemized deductions
    L[16] = SchA_L[23];
    shownum_or_40(16);



    if (bL17a)
        fprintf(outfile, "CkL17a X\n");
    if (bL17b)
        fprintf(outfile, "CkL17b X\n");
    if (bL17c)
        fprintf(outfile, "CkL17c X\n");
    if (bL17d)
        fprintf(outfile, "CkL17d X\n");


    double std_deduction = 0.0;
    // Calculate standard deduction based on filing status and extra checkboxes
    if (bStdDedOverride) {
        std_deduction = StdDedOverride;
    } else {
        std_deduction = p_cfg->std_deduction;

        fprintf( outfile, "INFO: Base standard deduction: %6.2f\n", std_deduction);

        // Adjust for extra checkboxes on line 17
        double std_deduction_extra_amt;
        std_deduction_extra_amt = p_cfg->extra_std_deduction;


        double std_deduction_extra;
        std_deduction_extra = (bL17a + bL17b + bL17c + bL17d) * std_deduction_extra_amt;
        fprintf( outfile, "INFO: Standard deduction extra amount: %6.2f\n", std_deduction_extra);

        std_deduction += std_deduction_extra;
    }
    fprintf( outfile, "INFO: Total standard deduction: %6.2f\n", std_deduction);

    // Standard deduction
    L[17] = std_deduction;
    shownum_or_40(17);

    L[18] = LargerOf(L[16], L[17]);
    shownum_or_40(18);

    // Oregon taxable income
    L[19] = NotLessThanZero(L[15] - L[18]);
    shownum_or_40(19);


    if (b20a)
        fprintf(outfile, "CkL20a X\n");
    if (b20b)
        fprintf(outfile, "CkL20b X\n");
    if (b20c)
        fprintf(outfile, "CkL20c X\n");


    double oregon_tax;

    if (b20a || b20b || b20c)
        oregon_tax = AltMethodTaxAmt;
    else
        oregon_tax = TaxLookup(L[19], p_cfg->p_tax_table);

    double marginal_rate;
    marginal_rate = TaxRate(L[19], p_cfg->p_tax_table);

    fprintf(outfile,"INFO: You are in the %2.1f%% marginal tax bracket,\n"
                    "INFO: and you are paying an effective %2.1f%% tax on your income.\n",
          100.0 * marginal_rate, 100.0 * oregon_tax / (L[19] + 1e-9) );

    L[20] = oregon_tax;
    shownum_or_40(20);

    shownum_or_40_nz(21);

    L[22] = SchASC_C[5];
    shownum_or_40_nz(22);

    L[23] = L[21] + L[22];
    shownum_or_40_nz(23);

    L[24] = L[20] + L[23];
    shownum_or_40(24);

    // Exemption credit worksheet
    #define ECW_NUM_LINES 5

    char *ecw_line_description[ECW_NUM_LINES + 1] = {
        "", // Line[0] not used
        "Regular exemptions, limited by AGI",
        "Disability exemptions, limited by AGI",
        "Children with disability exemptions, limited by AGI",
        "Sum of exemptions",
        "Total exemption credit"
    };
    
    double ecw_L[ECW_NUM_LINES + 1];    // Add one extra to allow 1-based; [0] entry is ignored.

    ecw_L[1] = (fed_agi > p_cfg->exemption_credit_limit) ? 0.0 : (b6aRegular + b6bRegular + L6c);
    ecw_L[2] = (fed_agi > EXEMPTION_CREDIT_DISABILITY_LIMIT) ? 0.0 : (b6aDisabled + b6bDisabled);
    ecw_L[3] = (fed_agi > EXEMPTION_CREDIT_DISABILITY_LIMIT) ? 0.0 : L6d;
    ecw_L[4] = ecw_L[1] + ecw_L[2] + ecw_L[3];
    ecw_L[5] = ecw_L[4] * EXEMPTION_CREDIT;

    fprintf( outfile, "INFO: Exemption credit worksheet\n" );
    fprintf( outfile, "INFO: Fed AGI Regular Exemption Limit: %9.2f, Disabled Exemption Limit: %9.2f\n", p_cfg->exemption_credit_limit, EXEMPTION_CREDIT_DISABILITY_LIMIT);
    for (line = 1; line <= ECW_NUM_LINES; ++line) {
        fprintf( outfile, "INFO: --- Line %2d: %9.2f : '%s'\n", line, ecw_L[line], ecw_line_description[line]);
    }
    
    L[25] = ecw_L[5];
    shownum_or_40(25);
    shownum_or_40_nz(26);

    L[27] = SchASC_D[16];
    shownum_or_40_nz(27);

    L[28] = L[25] + L[26] + L[27];
    shownum_or_40_nz(28);

    L[29] = NotLessThanZero(L[24] - L[28]);
    shownum_or_40(29);

    L[30] = SmallerOf(SchASC_E[9], L[29]);
    shownum_or_40_nz(30);

    L[31] = NotLessThanZero(L[29] - L[30]);
    shownum_or_40(31);

    shownum_or_40_nz(32);
    shownum_or_40_nz(33);
    shownum_or_40_nz(34);
    shownum_or_40_nz(35);
    shownum_or_40_nz(36);
    shownum_or_40_nz(37);

    L[39] = SchASC_F[7];
    shownum_or_40_nz(39);
    
    L[40] = L[32] + L[33] + L[34] + L[35] + L[36] + L[37] + L[39];
    shownum_or_40(40);

    // Overpayment
    L[41] = NotLessThanZero(L[40] - L[31]);
    shownum_or_40_nz(41);

    // Net tax owed
    L[42] = NotLessThanZero(L[31] - L[40]);
    shownum_or_40_nz(42);

    // Penalties and interest
    shownum_or_40_nz(43);

    // Interest on underpayment of tax
    shownum_or_40_nz(44);
    showline_wlabel_or_40_nz("L44a", L44a);
    if (bL44b)
        fprintf(outfile, "CkL44b X\n");

    // Total penalties and interest
    L[45] = L[43] + L[44];
    shownum_or_40_nz(45);

    // Amount you owe
    // If there is an overpayment, and the overpayment is less than
    // the total penalties and interest, use the difference as the amount owed.
    // still owed, else use tax owed plus penalties and interest
    if (L[41] > 0 && (L[41] < L[45])) {
        L[46] = L[45] - L[41]; 
    } else {
        L[46] = L[42] + L[45];
    }
    shownum_or_40_nz(46);

    // Refund
    // From the form OR-40 instructions:
    // "If the total penalty and interest is more than your
    // overpayment, see the instructions for line 46." Err, what?
    // Paraphrasing: "If L[45] > L[41], see the blah,blah,blah".
    // Meaning there will be money owed, so the refund will be zero.
    // So we simply need to make sure that the output is zero when
    // L[45] > L[41], which is handled by the NotLessThanZero operator without
    // any other conditions.
    L[47] = NotLessThanZero(L[41] - L[45]);
    shownum_or_40_nz(47);

    shownum_or_40_nz(48);
    shownum_or_40_nz(49);
    shownum_or_40_nz(50);

    fprintf(outfile, "L50a: %s\n", L50a);
    fprintf(outfile, "L50b: %s\n", L50b);

    shownum_or_40_nz(51);

    // Total refund checkoff amounts
    double L52_pre_sum = L[48] + L[49] + L[50] + L[51];
    L[52] = SmallerOf(L52_pre_sum, L[47]);
    if (L52_pre_sum > L[47]) {
        fprintf(outfile, "INFO: Refund checkoff amount request in L52 of %8.2f has been limited to refund amount: %8.2f\n", 
            L52_pre_sum, L[47]);
    }

    shownum_or_40_nz(52);

    // Net refund
    L[53] = NotLessThanZero(L[47] - L[52]);
    shownum_or_40_nz(53);

    if (bL54DepOutUS)
        fprintf(outfile, "CkL54DepOutUS X\n");

    if (bAcctChecking)
        fprintf(outfile, "CkAcctChecking X\n");

    if (bAcctSavings)
        fprintf(outfile, "CkAcctSavings X\n");

    fprintf(outfile, "AcctRoutingNumber: %s\n", AcctRoutingNumber);
    fprintf(outfile, "AcctNumber: %s\n", AcctNumber);

#ifdef SHOWNUM_OR_40_TEST
    L[7] = -567891234.0;
    shownum_or_40(7);
    L[7] = -67891234.0;
    shownum_or_40(7);
    L[7] = -7891234.0;
    shownum_or_40(7);
    L[7] = -891234.0;
    shownum_or_40(7);
    L[7] = -91234.0;
    shownum_or_40(7);
    L[7] = -1234.0;
    shownum_or_40(7);
    L[7] = -234.0;
    shownum_or_40(7);
    L[7] = -34.0;
    shownum_or_40(7);
    L[7] = -3.0;
    shownum_or_40(7);
    L[7] = 4.0;
    shownum_or_40(7);
    L[7] = 34.0;
    shownum_or_40(7);
    L[7] = 1234.0;
    shownum_or_40(7);
    L[7] = 91234.0;
    shownum_or_40(7);
    L[7] = 891234.0;
    shownum_or_40(7);
    L[7] = 7891234.0;
    shownum_or_40(7);
    L[7] = 67891234.0;
    shownum_or_40(7);
    L[7] = 567891234.0;
    shownum_or_40(7);
#endif // SHOWNUM_OR_40_TEST



 /* ----- .... Until here.  ----- */


 /***
    Summary of useful functions:
    GetLine( "label", &variable )   - Looks for "label" in input file, and places the corresponding sum of 
                      values following that label (until ";") into variable.
    GetLineF( "label", &variable )  - Like GetLine() above, but also writes the result to the output file.
    GetLineFnz(( "label", &variable ) - Like GetLine(), but only writes non-zero values to the output file.
    GetLine1( "label", &variable )  - Like GetLine() above, but expects single value (no sum, no ";" in input file).

    c = SmallerOf( a, b );      - Selects smaller of two values.
    c = LargerOf( a, b );       - Selects larger of two values.
    c = NotLessThanZero( a );   - Selects positive value or zero. Prevents negative values.

    showline( j )           - Writes currency value of L[j] to output file with label in nice format.
    shownum( j )            - Writes integer value of L[j] to output file with label in nice format.
    showline_wmsg( j, "msg" )   - Like showline, but adds the provided message to the output line.
    ShowLineNonZero( j )        - Like showline, but only writes non-zero values.
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

#undef MIN(i,
#undef MAX(i,

} // namespace taxsolve_OR_40_2024
} // namespace OpenTaxSolver2024

