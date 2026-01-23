#include "ots_2020_routines.h"
namespace OpenTaxSolver2020 {
namespace taxsolve_f8606 {

#define Yes 1
#define No 0
/************************************************************************/
/* TaxSolve_Form_8606.c -                                               */
/*									*/
/* Provided by Fred Robinson - 2020-7-8.				*/	
/************************************************************************/

float thisversion = 1.00;



/*----------------------------------------------------------------------------*/

int main(int argc, char *argv[])
{
  int i, j, k;
  int complete_part_one, complete_part_two, complete_part_three, dist_or_conv;
  char word[4000], outfname[4000], *infname = 0;
  time_t now;
  double L15a = 0.0, L15b = 0.0, L15c = 0.0;
  double L25a = 0.0, L25b = 0.0, L25c = 0.0;

  printf("Form 8606, 2020 - v%3.2f\n", thisversion);

  /* Decode any command-line arguments. */
  i = 1;
  k = 1;
  while (i < argc) {
    if (strcmp(argv[i], "-verbose") == 0) {
      verbose = 1;
    } else if (k == 1) {
      infname = strdup(argv[i]);
      infile = fopen(infname, "r");
      if (infile == 0) {
        printf("ERROR: Parameter file '%s' could not be opened.\n", infname);
        exit(1);
      }
      k = 2;
      /* Base name of output file on input file. */
      strcpy(outfname, infname);
      j = strlen(outfname) - 1;
      while ((j >= 0) && (outfname[j] != '.'))
        j--;
      if (j < 0)
        strcat(outfname, "_out.txt");
      else
        strcpy(&(outfname[j]), "_out.txt");
      outfile = fopen(outfname, "w");
      if (outfile == 0) {
        printf("ERROR: Output file '%s' could not be opened.\n", outfname);
        exit(1);
      }
      printf("Writing results to file:  %s\n", outfname);
    } else {
      printf("Unknown command-line parameter '%s'\n", argv[i]);
      exit(1);
    }
    i = i + 1;
  }
  if (infile == 0) {
    printf("Error: No input file on command line.\n");
    exit(1);
  }

  /* Pre-initialize all lines to zeros. */
  for (i = 0; i < MAX_LINES; i++) {
    L[i] = 0.0;
  }

  /* Accept parameters from input file. */
  /* Expect lines, something like:
         Title:  Form XXXX Return
         L2              {Returns and Allowances}
         . . .
  */

  /* Accept Form's "Title" line, and put out with date-stamp for your records.
   */
  read_line(infile, word);
  now = time(0);
  fprintf(outfile, "\n%s,  v%2.2f, %s\n", word, thisversion, ctime(&now));

  // Begin form-specific code

  // Part I

  /* Are you completing Part I of this form? (Y/N) */
  get_parameter(infile, 's', word, "Complete_Part_One?");
  get_param_single_line(infile, 'b', &complete_part_one, "Complete_Part_One?");

  GetLine("L1", &L[1]);
  GetLine("L2", &L[2]);

  /* Did you take a distribution or make a conversion? (Y/N) */
  get_parameter(infile, 's', word, "Dist_or_Conv?");
  get_param_single_line(infile, 'b', &dist_or_conv, "Dist_or_Conv?");

  GetLine("L4", &L[4]);
  GetLine("L6", &L[6]);
  GetLine("L7", &L[7]);
  GetLine("L8", &L[8]);
  GetLine("L15b", &L15b);

  // All inputs are read, now produce output for Part I
  if (complete_part_one) {
    showline(1);
    showline(2);
    L[3] = L[1] + L[2];
    showline(3);

    if (!dist_or_conv) {
      L[14] = L[3];
      showline(14);
    } else {

      showline(4);
      L[5] = L[3] - L[4];
      showline(5);
      showline(6);
      showline(7);
      showline(8);
      L[9] = L[6] + L[7] + L[8];
      showline(9);

      L[10] = L[5] / L[9];
      if (L[10] > 1.00) {
        L[10] = 1.0;
      }

      double integral;
      double fractional = modf(L[10], &integral);
      int integral_int, fractional_int;
      integral_int = integral;
      fractional_int = abs( (int)(1000.0 * fractional) );
      fprintf(outfile, "L10intpart %d\n", integral_int);
      fprintf(outfile, "L10rest %03d\n", fractional_int);

      L[11] = L[8] * L[10];
      showline(11);

      L[12] = L[7] * L[10];
      showline(12);

      L[13] = L[11] + L[12];
      showline(13);

      L[14] = L[3] - L[13];
      showline(14);

      L15a = L[7] - L[12];
      showline_wlabel("L15a", L15a);

      showline_wlabel("L15b", L15b);

      L15c = L15a - L15b;
      if (L15c > 0.0) {
        showline_wlabelmsg(
            "L15c", L15c,
            "Taxable Amount: include this amount on 2020 Form 1040 or 1040-SR, "
            "line 4b; or 2019 Form 1040-NR, line 16b");
      } else {
        showline_wlabel("L15c", L15c);
      }
    }
  }

  // Part II

  /* Are you completing Part II of this form? (Y/N) */
  get_parameter(infile, 's', word, "Complete_Part_Two?");
  get_param_single_line(infile, 'b', &complete_part_two, "Complete_Part_Two?");

  GetLine("L16", &L[16]);
  GetLine("L17", &L[17]);

  if (complete_part_two) {
    if (complete_part_one && dist_or_conv) {
      L[16] = L[8];
      L[17] = L[11];
    }

    showline(16);
    showline(17);
    L[18] = L[16] - L[17];
    showline(18);
  }

  // Part III

  /* Are you completing Part III of this form? (Y/N) */
  get_parameter(infile, 's', word, "Complete_Part_Three?");
  get_param_single_line(infile, 'b', &complete_part_three, "Complete_Part_Three?");
  GetLine("L19", &L[19]);
  GetLine("L20", &L[20]);
  GetLine("L22", &L[22]);
  GetLine("L24", &L[24]);
  GetLine("L25b", &L25b);

  if (complete_part_three) {
    showline(19);
    showline(20);

    L[21] = NotLessThanZero(L[19] - L[20]);
    showline(21);

    showline(22);

    if (L[21] > 0.0) {
      L[23] = NotLessThanZero(L[21] - L[22]);

      if (L[23] == 0) {
        showline(23);
      } else {
        showline_wmsg(
            23, "you may be subject to an additional tax (see instructions)");
      }
    }

    if (L[21] > 0 && L[23] > 0) {
      showline(24);

      L25a = NotLessThanZero(L[23] - L[24]);
      showline_wlabel("L25a", L25a);

      if (L25a > 0) {
        showline_wlabelmsg("L25b", L25b,
                           "Also, enter this amount on 2020 Form 8915-C, line "
                           "23, or 2020 Form 8915-D, line 14, as applicable");

        L25c = L25a - L25b;
        if (L25c > 0) {
          showline_wlabelmsg(
              "L25c", L25c,
              "Also include this amount on 2020 Form 1040 or 1040-SR, line 4b; "
              "or 2020 Form 1040-NR, line 16b.");

        } else {
          showline_wlabel("L25c", L25c);
        }
      }
    }
  }

  fprintf(outfile, "------------------------------\n");
  fprintf(outfile, "\n{ --------- Identity-Information:  --------- }\n");
  GetTextLineF("Name:");
  GetTextLineF("SocSec#:");
  GetTextLineF("Number&Street:");
  GetTextLineF("Apt#:");
  GetTextLineF("TownStateZip:");
  GetTextLineF("ForeignCountry:");
  GetTextLineF("ForeignState:");
  GetTextLineF("ForeignPostcode:");

  // End form-specific code

  fclose(infile);
  grab_any_pdf_markups(infname, outfile);
  fclose(outfile);

  printf("\nListing results from file: %s\n\n", outfname);
  Display_File(outfname);

  return 0;
}

#undef Yes
#undef No

} // namespace taxsolve_f8606
} // namespace OpenTaxSolver2020

