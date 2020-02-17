#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <time.h>
/* START of taxsolve_routines.c */
double L[1000];
char errmsg[10000];
FILE *infile = 0;
FILE *outfile = 0;
int verbose = 0;
int notappvalue = 0;
void show_errmsg(char *emsg)
{
;  if (outfile != 0)
    fprintf(outfile, "%s\n", emsg);

}

void get_word(FILE *infile, char *word)
{
  int j = 0;
  do
  {
    word[j] = getc(infile);
    if (word[j] == '{')
    {
      do
        word[j] = getc(infile);
      while ((word[j] != '}') && (!feof(infile)));
      word[j] = getc(infile);
    }

  }
  while ((!feof(infile)) && ((((word[j] == ' ') || (word[j] == '\t')) || (word[j] == '\n')) || (word[j] == '\r')));
  if (word[j] == '$')
    word[j] = getc(infile);

  if (word[j] == ';')
    j++;
  else
    if (word[j] == '"')
  {
    j = 0;
    do
      word[j++] = getc(infile);
    while ((word[j - 1] != '"') && (!feof(infile)));
    if (word[j - 1] == '"')
      j--;

  }
  else
  {
    do
    {
      j++;
      word[j] = getc(infile);
      if (word[j] == '{')
        do
        word[j] = getc(infile);
      while ((!feof(infile)) && (word[j] != '}'));

      if (word[j] == ',')
        word[j] = getc(infile);

    }
    while ((!feof(infile)) && ((((word[j] != ' ') && (word[j] != '\t')) && (word[j] != '\n')) && (word[j] != ';')));
    if (word[j] == ';')
      ungetc(word[j], infile);

  }


  word[j] = '\0';
  if (verbose)
;
}

char *mystrcasestr(char *haystack, char *needle)
{
  int j = 0;
  char *hs;
  char *ndl;
  char *pt;
  hs = strdup(haystack);
  while (hs[j] != '\0')
  {
    hs[j] = toupper(hs[j]);
    j++;
  }

  ndl = strdup(needle);
  j = 0;
  while (ndl[j] != '\0')
  {
    ndl[j] = toupper(ndl[j]);
    j++;
  }

  pt = strstr(hs, ndl);
  if (pt != 0)
  {
    j = 0;
    while (pt != (&hs[j]))
      j++;

    pt = &haystack[j];
  }

  free(ndl);
  free(hs);
  return pt;
}

void get_parameter(FILE *infile, char kind, void *x, char *emssg)
{
  char word[1024];
  char *owrd;
  int i;
  int *ii;
  double y;
  double *yy;
  get_word(infile, word);
  if (feof(infile))
  {
;    if (outfile)
      fprintf(outfile, "ERROR: Unexpected EOF on '%s'\n", emssg);

    exit(1);
  }

  if (kind == 'i')
  {
    if (sscanf(word, "%d", &i) != 1)
    {
;      fprintf(outfile, "ERROR: Bad integer '%s', reading %s.\n", word, emssg);
      exit(1);
    }

    ii = (int *) x;
    *ii = i;
  }
  else
    if (kind == 'f')
  {
    if (sscanf(word, "%lf", &y) != 1)
    {
;      fprintf(outfile, "ERROR: Bad float '%s', reading %s.\n", word, emssg);
      exit(1);
    }

    yy = (double *) x;
    *yy = y;
  }
  else
    if (kind == 's')
  {
    owrd = (char *) x;
    strcpy(owrd, word);
    if (emssg[0] != '\0')
    {
      if (strcmp(word, emssg) != 0)
      {
;        fprintf(outfile, "ERROR1: Found '%s' when expecting '%s'\n", word, emssg);
        exit(1);
      }

    }

  }
  else
    if (kind == 'l')
  {
    owrd = (char *) x;
    strcpy(owrd, word);
  }
  else
    if (kind == 'b')
  {
    if (strcasecmp(word, "y") == 0)
      i = 1;
    else
      if (strcasecmp(word, "n") == 0)
      i = 0;
    else
      if (((strcasecmp(word, "TRUE") == 0) || (strcasecmp(word, "YES") == 0)) || (strcmp(word, "1") == 0))
      i = 1;
    else
      if (((strcasecmp(word, "FALSE") == 0) || (strcasecmp(word, "NO") == 0)) || (strcmp(word, "0") == 0))
      i = 0;
    else
      if (strcasecmp(word, "N/A") == 0)
      i = notappvalue;
    else
    {
;      fprintf(outfile, "ERROR: Bad boolean '%s', reading %s.\n", word, emssg);
      exit(1);
    }





    ii = (int *) x;
    *ii = i;
  }
  else
  {
;    fprintf(outfile, "ERROR: Unknown type '%c'\n", kind);
    exit(1);
  }





}

void get_parameters(FILE *infile, char kind, void *x, char *emssg)
{
  char word[1024];
  char *owrd;
  int j;
  int *ii;
  double y;
  double *yy;
  if (kind == 'f')
  {
    yy = (double *) x;
    *yy = 0.0;
  }

  get_word(infile, word);
  while (word[0] != ';')
  {
    if (feof(infile))
    {
;      fprintf(outfile, "ERROR: Unexpected EOF on '%s'\n", emssg);
      exit(1);
    }

    if (kind == 'i')
    {
      if (sscanf(word, "%d", &j) != 1)
      {
;        fprintf(outfile, "ERROR: Bad integer '%s', reading %s.\n", word, emssg);
        exit(1);
      }

      ii = (int *) x;
      *ii = j;
    }
    else
      if (kind == 'f')
    {
      if (sscanf(word, "%lf", &y) != 1)
      {
;        fprintf(outfile, "ERROR: Bad float '%s', reading %s.\n", word, emssg);
        exit(1);
      }

      yy = (double *) x;
      *yy = (*yy) + y;
    }
    else
      if (kind == 's')
    {
      owrd = (char *) x;
      strcpy(owrd, word);
      if (emssg[0] != '\0')
      {
        if (strcmp(word, emssg) != 0)
        {
;          fprintf(outfile, "ERROR2: Found '%s' when expecting '%s'\n", word, emssg);
          exit(1);
        }

      }

    }
    else
      if (kind == 'b')
    {
      if ((((strcasecmp(word, "TRUE") == 0) || (strcasecmp(word, "YES") == 0)) || (strcmp(word, "Y") == 0)) || (strcmp(word, "1") == 0))
        j = 1;
      else
        if ((((strcasecmp(word, "FALSE") == 0) || (strcasecmp(word, "NO") == 0)) || (strcmp(word, "N") == 0)) || (strcmp(word, "0") == 0))
        j = 0;
      else
      {
;        fprintf(outfile, "ERROR: Bad boolean '%s', reading %s.\n", word, emssg);
        exit(1);
      }


      ii = (int *) x;
      *ii = j;
    }
    else
    {
;      fprintf(outfile, "ERROR: Unknown type '%c'\n", kind);
      exit(1);
    }




    get_word(infile, word);
  }

}

void next_word(char *line, char *word, char *delim)
{
  int i = 0;
  int j = 0;
  int m = 0;
  int flag = 1;
  while ((line[i] != '\0') && flag)
  {
    j = 0;
    while ((delim[j] != '\0') && (line[i] != delim[j]))
      j = j + 1;

    if (line[i] == delim[j])
      i++;
    else
      flag = 0;

  }

  while ((line[i] != '\0') && (!flag))
  {
    word[m++] = line[i++];
    if (line[i] != '\0')
    {
      j = 0;
      while ((delim[j] != '\0') && (line[i] != delim[j]))
        j = j + 1;

      if (line[i] == delim[j])
        flag = 1;

    }

  }

  j = 0;
  while (line[i] != '\0')
  {
    line[j++] = line[i++];
  }

  line[j] = '\0';
  word[m] = '\0';
}

struct date_record
{
  int month;
  int day;
  int year;
} yourDOB;
struct date_record spouseDOB;
struct date_record DL;
char *format_mmddyyyy(int month, int day, int year)
{
  char datestr[100];
  char dd[20];
  char yy[20];
  if (month < 10)
    sprintf(datestr, "0%d", month);
  else
    sprintf(datestr, "%d", month);

  if (day < 10)
    sprintf(dd, "0%d", day);
  else
    sprintf(dd, "%d", day);

  strcat(datestr, dd);
  sprintf(yy, "%d", year);
  strcat(datestr, yy);
  return strdup(datestr);
}

int interpret_date(char *datestr, int *month, int *day, int *year, char *emssg)
{
  char word1[500];
  char *owrd;
  owrd = strdup(datestr);
  next_word(owrd, word1, " /,-\t\n\r");
  if (strncasecmp(word1, "Jan", 3) == 0)
    *month = 1;
  else
    if (strncasecmp(word1, "Feb", 3) == 0)
    *month = 2;
  else
    if (strncasecmp(word1, "Mar", 3) == 0)
    *month = 3;
  else
    if (strncasecmp(word1, "Apr", 3) == 0)
    *month = 4;
  else
    if (strncasecmp(word1, "May", 3) == 0)
    *month = 5;
  else
    if (strncasecmp(word1, "Jun", 3) == 0)
    *month = 6;
  else
    if (strncasecmp(word1, "Jul", 3) == 0)
    *month = 7;
  else
    if (strncasecmp(word1, "Aug", 3) == 0)
    *month = 8;
  else
    if (strncasecmp(word1, "Sep", 3) == 0)
    *month = 9;
  else
    if (strncasecmp(word1, "Oct", 3) == 0)
    *month = 10;
  else
    if (strncasecmp(word1, "Nov", 3) == 0)
    *month = 11;
  else
    if (strncasecmp(word1, "Dec", 3) == 0)
    *month = 12;
  else
    if (((sscanf(word1, "%d", month) != 1) || ((*month) < 1)) || ((*month) > 12))
  {
;    fprintf(outfile, "Warning: Bad month '%s' on '%s'\n", word1, emssg);
    return 0;
  }













  next_word(owrd, word1, " /,-\t\n\r");
  if (((sscanf(word1, "%d", day) != 1) || ((*day) < 1)) || ((*day) > 31))
  {
;    fprintf(outfile, "ERROR: Bad day '%s' on '%s'\n", word1, emssg);
    return 0;
  }

  next_word(owrd, word1, " /,-\t\n\r");
  if (((sscanf(word1, "%d", year) != 1) || ((*year) < 0)) || ((*year) > 3000))
  {
;    fprintf(outfile, "ERROR: Bad year '%s' on '%s'\n", word1, emssg);
    return 0;
  }

  free(owrd);
  if ((*year) < 40)
    *year = (*year) + 2000;
  else
    if ((*year) < 1900)
    *year = (*year) + 1900;


  return 1;
}

int get_date(char *datestr, char *emssg)
{
  int month;
  int day;
  int year;
  int days;
  int result;
  result = interpret_date(datestr, &month, &day, &year, emssg);
  if (result != 1)
    exit(1);

  year = year - 1900;
  if ((year < 80) || (year > 150))
;
  switch (month)
  {
    case 1:
      days = 0;
      break;

    case 2:
      days = 31;
      break;

    case 3:
      days = 59;
      break;

    case 4:
      days = 90;
      break;

    case 5:
      days = 120;
      break;

    case 6:
      days = 151;
      break;

    case 7:
      days = 181;
      break;

    case 8:
      days = 212;
      break;

    case 9:
      days = 243;
      break;

    case 10:
      days = 273;
      break;

    case 11:
      days = 304;
      break;

    case 12:
      days = 334;
      break;

    default:
;      fprintf(outfile, "ERROR: Bad month '%d'\n", month);
      exit(1);
      break;

  }

  days = ((days + day) + (365 * (year - 80))) - 1;
  return days;
}

void read_line(FILE *infile, char *line)
{
  int j = 0;
  do
    line[j++] = getc(infile);
  while ((!feof(infile)) && (line[j - 1] != '\n'));
  line[j - 1] = '\0';
}

void showline(int j)
{
  fprintf(outfile, "L%d = %6.2f\n", j, L[j]);
}

void shownum(int j)
{
  fprintf(outfile, "L%d = %d\n", j, (int) L[j]);
}

void ShowLine(int j)
{
  if (L[j] != 0)
    showline(j);

}

void ShowLineNonZero(int j)
{
  if (L[j] != 0)
    showline(j);

}

void showline_wmsg(int j, char *msg)
{
  fprintf(outfile, "L%d = %6.2f\t\t%s\n", j, L[j], msg);
}

void ShowLineNonZero_wMsg(int j, char *msg)
{
  if (L[j] != 0)
    showline_wmsg(j, msg);

}

void showline_wrksht(char wrksht, int j, double *x)
{
  fprintf(outfile, " %c%d = %6.2f\n", wrksht, j, x[j]);
}

void showline_wlabel(char *label, double value)
{
  fprintf(outfile, "%s = %6.2f\n", label, value);
}

void showline_wlabelmsg(char *label, double value, char *msg)
{
  fprintf(outfile, "%s = %6.2f\t\t%s\n", label, value, msg);
}

int Round(double x)
{
  int y;
  if (x < 0.0)
    y = x - 0.5;
  else
    y = x + 0.5;

  return y;
}

void GetLine(char *linename, double *value)
{
  char word[1024];
  get_parameter(infile, 's', word, linename);
  get_parameters(infile, 'f', value, linename);
}

void GetLine1(char *linename, double *value)
{
  char word[1024];
  get_parameter(infile, 's', word, linename);
  get_parameter(infile, 'f', value, linename);
}

void GetLineF(char *linename, double *value)
{
  GetLine(linename, value);
  fprintf(outfile, "%s = %6.2f\n", linename, *value);
}

void GetLineFnz(char *linename, double *value)
{
  GetLine(linename, value);
  if ((*value) != 0.0)
    fprintf(outfile, "%s = %6.2f\n", linename, *value);

}

void GetOptionalLine(char *linename, char *label, double *value)
{
  get_parameter(infile, 'l', label, linename);
  get_parameters(infile, 'f', value, linename);
}

double smallerof(double a, double b)
{
  if (a < b)
    return a;
  else
    return b;

}

double largerof(double a, double b)
{
  if (a > b)
    return a;
  else
    return b;

}

double NotLessThanZero(double a)
{
  if (a < 0.0)
    return 0.0;
  else
    return a;

}

double absolutev(double val)
{
  if (val >= 0.0)
    return val;
  else
    return -val;

}

void Display_File(char *filename)
{
  FILE *infile;
  char line[500];
  infile = fopen(filename, "r");
  if (infile == 0)
  {
;    return;
  }

  fgets(line, 500, infile);
  while (!feof(infile))
  {
;    fgets(line, 500, infile);
  }

  fclose(infile);
}

void get_comment(FILE *infile, char *word)
{
  int j = 0;
  do
    word[j] = getc(infile);
  while ((!feof(infile)) && ((((word[j] == ' ') || (word[j] == '\t')) || (word[j] == '\n')) || (word[j] == '\r')));
  if (word[j] == '{')
  {
    do
      word[j++] = getc(infile);
    while ((!feof(infile)) && (word[j - 1] != '}'));
    if (word[j - 1] == '}')
      word[j - 1] = '\0';
    else
      word[j] = '\0';

  }
  else
  {
    ungetc(word[j], infile);
    word[0] = '\0';
  }

  if (verbose)
;
}

void consume_leading_trailing_whitespace(char *line)
{
  int j;
  int k;
  while (isspace(line[0]))
  {
    j = 0;
    do
    {
      line[j] = line[j + 1];
      j++;
    }
    while (line[j - 1] != '\0');
  }

  k = strlen(line) - 1;
  while ((k >= 0) && isspace(line[k]))
  {
    line[k] = '\0';
    k--;
  }

}

int do_all_caps = 0;
int writeout_line = 1;
char *GetTextLineF(char *linename)
{
  int k = 0;
  char line[5000];
  get_parameter(infile, 's', line, linename);
  line[k] = getc(infile);
  while ((!feof(infile)) && (line[k] != '\n'))
  {
    if (line[k] == '{')
    {
      do
        line[k] = getc(infile);
      while ((!feof(infile)) && (line[k] != '}'));
      if (line[k] == '}')
        line[k] = getc(infile);

    }
    else
    {
      k++;
      if (k >= 5000)
      {
        line[k - 1] = '\0';
        while ((!feof(infile)) && (getc(infile) != '\n'))
          ;

        consume_leading_trailing_whitespace(line);
        fprintf(outfile, "%s %s\n", linename, line);
        return strdup(line);
      }

      line[k] = getc(infile);
    }

  }

  line[k] = '\0';
  consume_leading_trailing_whitespace(line);
  if (do_all_caps)
  {
    k = 0;
    while (line[k] != '\0')
    {
      line[k] = toupper(line[k]);
      k++;
    }

  }

  if (writeout_line)
    fprintf(outfile, "%s %s\n", linename, line);

  return strdup(line);
}

char *GetTextLine(char *linename)
{
  int priorstate;
  char *chstr;
  priorstate = writeout_line;
  writeout_line = 0;
  chstr = GetTextLineF(linename);
  writeout_line = priorstate;
  return chstr;
}

void format_socsec(char *line, int kind)
{
  char buf[20] = "";
  int j = 0;
  int k = 0;
  while ((line[j] != '\0') && (k < 11))
  {
    if (isdigit(line[j]))
    {
      buf[k++] = line[j];
      if ((kind == 0) && ((k == 3) || (k == 6)))
        buf[k++] = ' ';

    }

    j++;
  }

  strcpy(line, buf);
}

void remove_certain_chars(char *line, char *badchars)
{
  int j = 0;
  int k;
  int m = 0;
  while (line[j] != '\0')
  {
    k = 0;
    while ((badchars[k] != '\0') && (line[j] != badchars[k]))
      k++;

    if (badchars[k] == '\0')
      line[m++] = line[j];

    j++;
  }

  line[m] = '\0';
}

void substitute_chars(char *line, char *badchars, char replace_char)
{
  int j = 0;
  int k;
  while (line[j] != '\0')
  {
    k = 0;
    while ((badchars[k] != '\0') && (line[j] != badchars[k]))
      k++;

    if (badchars[k] != '\0')
      line[j] = replace_char;

    j++;
  }

}


/* END of taxsolve_routines.c */
/* START of taxsolve_US_1040_2017.c */
float _us_thisversion = 15.04;
double _us_SchedA[1000];
double _us_SchedD[1000];
double _us_amtws[1000];
double _us_L8b = 0.0;
double _us_L9b = 0.0;
double _us_qcgws6 = 0.0;
double _us_qcgws7 = 0.0;
int _us_Do_SchedD = 0;
int _us_Do_QDCGTW = 0;
int _us_Do_SDTW = 0;
int _us_status;
int _us_under65 = 1;
int _us_over65 = 0;
int _us_dependent = 0;
int _us_force_print_all_pdf_forms = 0;
double _us_L60b = 0.0;
double _us_collectibles_gains = 0.0;
double _us_ws_sched_D[1000];
double _us_idws_thresh = 0.0;
double _us_idws[1000];
double _us_brkpt[4][9] = {{0.0, 9325.0, 37950.0, 91900.0, 191650.0, 416700.0, 418400.0, 9e9}, {0.0, 18650.0, 75900.0, 153100.0, 233350.0, 416700.0, 470700.0, 9e9}, {0.0, 9325.0, 37950.0, 76550.0, 116675.0, 208350.0, 235350.0, 9e9}, {0.0, 13350.0, 50800.0, 131200.0, 212500.0, 416700.0, 444550.0, 9e9}};
double _us_txrt[4][9] = {{0.1, 0.15, 0.25, 0.28, 0.33, 0.35, 0.396}, {0.1, 0.15, 0.25, 0.28, 0.33, 0.35, 0.396}, {0.1, 0.15, 0.25, 0.28, 0.33, 0.35, 0.396}, {0.1, 0.15, 0.25, 0.28, 0.33, 0.35, 0.396}};
double _us_TaxRateFormula(double x, int _us_status)
{
  double sum = 0.0;
  int bracket = 0;
  if (_us_status == 5)
    _us_status = 2;

  _us_status = _us_status - 1;
  while (_us_brkpt[_us_status][bracket + 1] < x)
  {
    sum = sum + ((_us_brkpt[_us_status][bracket + 1] - _us_brkpt[_us_status][bracket]) * _us_txrt[_us_status][bracket]);
    bracket = bracket + 1;
  }

  return ((x - _us_brkpt[_us_status][bracket]) * _us_txrt[_us_status][bracket]) + sum;
}

void _us_Report_bracket_info(double x, double addedtx, int _us_status)
{
  double tx;
  int bracket = 0;
  tx = _us_TaxRateFormula(x, _us_status);
  if (_us_status == 5)
    _us_status = 2;

  _us_status = _us_status - 1;
  while (_us_brkpt[_us_status][bracket + 1] < x)
    bracket++;

;  fprintf(outfile, " You are in the %2.1f%% marginal tax bracket,\n and you are paying an effective %2.1f%% tax on your income.\n", 100.0 * _us_txrt[_us_status][bracket], (100.0 * (tx + addedtx)) / (x + 1e-9));
}

double _us_TaxRateFunction(double income, int _us_status)
{
  double x;
  double dx;
  double tx;
  int k;
  if (income < 100000.0)
  {
    if (income < 25.0)
      x = 5.0;
    else
      if (income < 3000.0)
      x = 25.0;
    else
      x = 50.0;


    dx = 0.5 * x;
    k = income / x;
    x = (x * ((double) k)) + dx;
    tx = (int) (_us_TaxRateFormula(x, _us_status) + 0.5);
  }
  else
    tx = _us_TaxRateFormula(income, _us_status);

  return tx;
}

void _us_showschedA(int linenum)
{
  if (_us_SchedA[linenum] > 0.0)
    fprintf(outfile, " A%d = %6.2f\n", linenum, _us_SchedA[linenum]);

}

void _us_showschedA_wMsg(int linenum, char *msg)
{
  if (_us_SchedA[linenum] > 0.0)
    fprintf(outfile, " A%d = %6.2f	%s\n", linenum, _us_SchedA[linenum], msg);

}

void _us_print2(char *msg)
{
;  fprintf(outfile, "%s", msg);
}

void _us_capgains_qualdividends_worksheets(int _us_status, double _us_L9b)
{
  double ws[50];
  int j;
  for (j = 0; j < 50; j++)
    ws[j] = 0.0;

  ws[1] = L[43];
  ws[2] = _us_L9b;
  if (_us_Do_SchedD)
    ws[3] = NotLessThanZero(smallerof(_us_SchedD[15], _us_SchedD[16]));
  else
    ws[3] = L[13];

  ws[4] = ws[2] + ws[3];
  ws[5] = 0.0;
  ws[6] = NotLessThanZero(ws[4] - ws[5]);
  _us_qcgws6 = ws[6];
  ws[7] = NotLessThanZero(ws[1] - ws[6]);
  _us_qcgws7 = ws[7];
  switch (_us_status)
  {
    case 1:

    case 3:
      ws[8] = 37950.0;
      break;

    case 2:

    case 5:
      ws[8] = 75900.0;
      break;

    case 4:
      ws[8] = 50800.0;
      break;

  }

  ws[9] = smallerof(ws[1], ws[8]);
  ws[10] = smallerof(ws[7], ws[9]);
  ws[11] = ws[9] - ws[10];
  ws[12] = smallerof(ws[1], ws[6]);
  ws[13] = ws[11];
  ws[14] = ws[12] - ws[13];
  switch (_us_status)
  {
    case 1:
      ws[15] = 418400.0;
      break;

    case 3:
      ws[15] = 235350.0;
      break;

    case 2:

    case 5:
      ws[15] = 470700.0;
      break;

    case 4:
      ws[15] = 444550.0;
      break;

  }

  ws[16] = smallerof(ws[1], ws[15]);
  ws[17] = ws[7] + ws[11];
  ws[18] = NotLessThanZero(ws[16] - ws[17]);
  ws[19] = smallerof(ws[14], ws[18]);
  ws[20] = 0.15 * ws[19];
  ws[21] = ws[11] + ws[19];
  ws[22] = ws[12] - ws[21];
  ws[23] = 0.20 * ws[22];
  ws[24] = _us_TaxRateFunction(ws[7], _us_status);
  ws[25] = (ws[20] + ws[23]) + ws[24];
  ws[26] = _us_TaxRateFunction(ws[1], _us_status);
  ws[27] = smallerof(ws[25], ws[26]);
  for (j = 1; j <= 27; j++)
  {
;    if (j == 3)
    {
      if (_us_Do_SchedD)
        fprintf(outfile, "\t\t3: Check Yes.\n");
      else
        fprintf(outfile, "\t\t3: Check No.\n");

    }

    fprintf(outfile, "	Qual. Div & Gains WorkSheet %d:  %8.2f\n", j, ws[j]);
  }

  L[44] = ws[27];
}

double _us_form6251_AlternativeMinimumTax(int itemized)
{
  double ws[100];
  double thresholdA = 0;
  double thresholdB = 0;
  double thresholdC = 187800.0;
  double amtexmption;
  double offsetA = 3756.0;
  double sum8_27 = 0.0;
  int j;
  int file_amt = 1;
;  fprintf(outfile, "Review AMT form6251 routine for your situation.\n");
  for (j = 0; j < 100; j++)
  {
    ws[j] = 0.0;
  }

  if (itemized)
  {
    _us_amtws[1] = L[41];
    if (_us_under65 != 0)
      _us_amtws[2] = NotLessThanZero(smallerof(_us_SchedA[4], 0.025 * L[38]));

    _us_amtws[3] = _us_SchedA[9];
    _us_amtws[4] = 0.0;
    _us_amtws[5] = _us_SchedA[27];
    if (L[38] <= _us_idws_thresh)
      _us_amtws[6] = 0.0;
    else
      _us_amtws[6] = -absolutev(_us_idws[9]);

  }
  else
    _us_amtws[1] = L[38];

  _us_amtws[7] = -(L[10] + L[21]);
  _us_amtws[10] = absolutev(L[21]);
  for (j = 1; j <= 27; j++)
    _us_amtws[28] = _us_amtws[28] + _us_amtws[j];

  if (_us_status == 3)
  {
    if (_us_amtws[28] > 415050.0)
      _us_amtws[28] = _us_amtws[28] + 41900.0;
    else
      if (_us_amtws[28] > 249450.0)
      _us_amtws[28] = _us_amtws[28] + (0.25 * (_us_amtws[28] - 247450.0));


  }

  switch (_us_status)
  {
    case 1:

    case 4:
      thresholdA = 120700.0;
      thresholdB = 335300.0;
      amtexmption = 54300.0;
      break;

    case 2:

    case 5:
      thresholdA = 160900.0;
      thresholdB = 494900.0;
      amtexmption = 84500.0;
      break;

    case 3:
      thresholdA = 80450.0;
      thresholdB = 247450.0;
      thresholdC = 93900.0;
      offsetA = 1878.0;
      amtexmption = 42250.0;
      if (_us_amtws[29] > thresholdB)
    {
      if (_us_amtws[29] > 358800.0)
        _us_amtws[29] = _us_amtws[29] + 35475.0;
      else
        _us_amtws[29] = _us_amtws[29] + (0.25 * (_us_amtws[29] - thresholdB));

    }

      break;

    default:
;      exit(1);

  }

  if (_us_amtws[28] <= thresholdA)
    _us_amtws[29] = amtexmption;
  else
    if (_us_amtws[28] >= thresholdB)
    _us_amtws[29] = 0.0;
  else
  {
    ws[1] = amtexmption;
    ws[2] = _us_amtws[28];
    ws[3] = thresholdA;
    ws[4] = NotLessThanZero(ws[2] - ws[3]);
    ws[5] = 0.25 * ws[4];
    ws[6] = NotLessThanZero(ws[1] - ws[5]);
    for (j = 0; j < 10; j++)
      if (ws[j] != 0.0)
    {
;      fprintf(outfile, "\t\tAMT pg 8 WrkSht %d: %8.2f\n", j, ws[j]);
    }


    _us_amtws[29] = ws[6];
  }


  _us_amtws[30] = NotLessThanZero(_us_amtws[28] - _us_amtws[29]);
  if (_us_amtws[30] > 0.0)
  {
    if (((L[13] == 0.0) && (_us_L9b == 0.0)) && ((_us_SchedD[15] <= 0.0) || (_us_SchedD[16] <= 0.0)))
    {
      if (_us_amtws[30] <= thresholdC)
        _us_amtws[31] = 0.26 * _us_amtws[30];
      else
        _us_amtws[31] = (0.28 * _us_amtws[30]) - offsetA;

    }
    else
    {
      _us_amtws[36] = _us_amtws[30];
      _us_amtws[37] = largerof(_us_qcgws6, _us_ws_sched_D[13]);
      _us_amtws[38] = _us_SchedD[19];
      if (_us_Do_SDTW)
        _us_amtws[39] = smallerof(_us_amtws[37] + _us_amtws[38], _us_ws_sched_D[10]);
      else
        _us_amtws[39] = _us_amtws[37];

      _us_amtws[40] = smallerof(_us_amtws[36], _us_amtws[39]);
      _us_amtws[41] = _us_amtws[36] - _us_amtws[40];
      if (_us_amtws[41] <= thresholdC)
        _us_amtws[42] = 0.26 * _us_amtws[41];
      else
        _us_amtws[42] = (0.28 * _us_amtws[41]) - offsetA;

      switch (_us_status)
      {
        case 2:

        case 5:
          _us_amtws[43] = 75900.0;
          break;

        case 1:

        case 3:
          _us_amtws[43] = 37950.0;
          break;

        case 4:
          _us_amtws[43] = 50800.0;

      }

      if (_us_Do_QDCGTW)
        _us_amtws[44] = NotLessThanZero(_us_qcgws7);
      else
        if (_us_Do_SDTW)
        _us_amtws[44] = NotLessThanZero(_us_ws_sched_D[14]);
      else
        _us_amtws[44] = NotLessThanZero(L[43]);


      _us_amtws[45] = NotLessThanZero(_us_amtws[43] - _us_amtws[44]);
      _us_amtws[46] = smallerof(_us_amtws[36], _us_amtws[37]);
      _us_amtws[47] = smallerof(_us_amtws[45], _us_amtws[46]);
      _us_amtws[48] = _us_amtws[46] - _us_amtws[47];
      switch (_us_status)
      {
        case 1:
          _us_amtws[49] = 418400.0;
          break;

        case 3:
          _us_amtws[49] = 235350.0;
          break;

        case 2:

        case 5:
          _us_amtws[49] = 470700.0;
          break;

        case 4:
          _us_amtws[49] = 444550.0;
          break;

        default:
;          exit(1);

      }

      _us_amtws[50] = _us_amtws[45];
      if (_us_Do_QDCGTW)
        _us_amtws[51] = NotLessThanZero(_us_qcgws7);
      else
        if (_us_Do_SDTW)
        _us_amtws[51] = NotLessThanZero(_us_ws_sched_D[19]);
      else
        _us_amtws[51] = NotLessThanZero(L[43]);


      _us_amtws[52] = _us_amtws[50] + _us_amtws[51];
      _us_amtws[53] = NotLessThanZero(_us_amtws[49] - _us_amtws[52]);
      _us_amtws[54] = smallerof(_us_amtws[48], _us_amtws[53]);
      _us_amtws[55] = 0.15 * _us_amtws[54];
      _us_amtws[56] = _us_amtws[47] + _us_amtws[54];
      if (absolutev(_us_amtws[36] - _us_amtws[56]) > 0.005)
      {
        _us_amtws[57] = _us_amtws[46] - _us_amtws[56];
        _us_amtws[58] = 0.20 * _us_amtws[57];
        if (_us_amtws[38] != 0.0)
        {
          _us_amtws[59] = (_us_amtws[41] + _us_amtws[56]) + _us_amtws[57];
          _us_amtws[60] = _us_amtws[36] - _us_amtws[59];
          _us_amtws[61] = 0.25 * _us_amtws[60];
        }

      }

      _us_amtws[62] = ((_us_amtws[42] + _us_amtws[55]) + _us_amtws[58]) + _us_amtws[61];
      if (_us_amtws[36] <= thresholdC)
        _us_amtws[63] = 0.26 * _us_amtws[36];
      else
        _us_amtws[63] = (0.28 * _us_amtws[36]) - offsetA;

      _us_amtws[64] = smallerof(_us_amtws[62], _us_amtws[63]);
      _us_amtws[31] = _us_amtws[64];
    }

    _us_amtws[32] = L[48];
    _us_amtws[33] = _us_amtws[31] - _us_amtws[32];
  }

  _us_amtws[34] = (L[44] + L[46]) - L[48];
  _us_amtws[35] = NotLessThanZero(_us_amtws[33] - _us_amtws[34]);
  if (_us_amtws[31] > _us_amtws[34])
  {
    file_amt = 1;
    fprintf(outfile, "You MUST file AMT form 6251. (%g > %g)\n", _us_amtws[31], _us_amtws[34]);
  }
  else
  {
    for (j = 8; j < 27; j++)
      sum8_27 = sum8_27 + _us_amtws[j];

    fprintf(outfile, " Sum of Form 6251 Lines 8 through 27 = %8.2f\n", sum8_27);
    if (sum8_27 < 0.0)
    {
      file_amt = 1;
      fprintf(outfile, "You may need to file AMT form 6251.  (AMTws[31]=%g which is NOT more than AMTws[34]=%g)\n", _us_amtws[31], _us_amtws[34]);
      fprintf(outfile, " (See \"Who Must File\" on page-1 of Instructions for Form-6251.)\n");
    }
    else
      file_amt = 0;

  }

  if (_us_force_print_all_pdf_forms)
    file_amt = 1;

  if (file_amt)
    fprintf(outfile, "PDFpage: 7 7\n");

  for (j = 0; j < 100; j++)
  {
    if ((j == 35) || (_us_amtws[j] != 0.0))
    {
;      fprintf(outfile, " 		AMT_Form_6251_L%d = %8.2f\n", j, _us_amtws[j]);
    }

    if (file_amt && (j == 35))
      fprintf(outfile, "EndPDFpage.\nPDFpage: 8 8\n");

  }

  if (file_amt)
    fprintf(outfile, "EndPDFpage.\n");

  fprintf(outfile, "Your Alternative Minimum Tax = %8.2f\n", _us_amtws[35]);
;  return _us_amtws[35];
}

struct _us_FedReturnData
{
  double fedline[1000];
  double schedD[1000];
  int Exception;
  int Itemized;
} _us_LastYearsReturn;
void _us_convert_slashes(char *fname)
{
  char *ptr;
  char slash_sreach = '\\';
  char slash_replace = '/';
  ptr = strchr(fname, slash_sreach);
  while (ptr)
  {
    ptr[0] = slash_replace;
    ptr = strchr(fname, slash_sreach);
  }

}

void _us_ImportFederalReturnData(char *fedlogfile, struct _us_FedReturnData *fed_data)
{
  FILE *infile;
  char fline[1000];
  char word[1000];
  int linenum;
  for (linenum = 0; linenum < 1000; linenum++)
  {
    fed_data->fedline[linenum] = 0.0;
    fed_data->schedD[linenum] = 0.0;
  }

  _us_convert_slashes(fedlogfile);
  infile = fopen(fedlogfile, "r");
  if (infile == 0)
  {
;    fprintf(outfile, "Error: Could not open federal return '%s'\n", fedlogfile);
    exit(1);
  }

;  fed_data->Itemized = 1;
  read_line(infile, fline);
  linenum = 0;
  while (!feof(infile))
  {
    if (strstr(fline, "Use standard deduction.") != 0)
      fed_data->Itemized = 0;

    next_word(fline, word, " \t=");
    if ((strstr(word, "L") == word) && (strstr(fline, " = ") != 0))
    {
      if (sscanf(&word[1], "%d", &linenum) != 1)
;
      next_word(fline, word, " \t=");
      remove_certain_chars(word, ",");
      if (sscanf(word, "%lf", &fed_data->fedline[linenum]) != 1)
;
      if (verbose)
;
    }

    if ((strstr(word, "D") == word) && (strstr(fline, " = ") != 0))
    {
      if (sscanf(&word[1], "%d", &linenum) != 1)
;
      next_word(fline, word, " \t=");
      remove_certain_chars(word, ",");
      if (sscanf(word, "%lf", &fed_data->schedD[linenum]) != 1)
      {
        if (strcasecmp(word, "yes") == 0)
          fed_data->schedD[linenum] = 1;
        else
          if (strcasecmp(word, "no") == 0)
          fed_data->schedD[linenum] = 0;
        else
;

      }

      if (verbose)
;
    }

    read_line(infile, fline);
  }

  fclose(infile);
}

void _us_CapitalLossCarryOverWorksheet(char *fedlogfile, struct _us_FedReturnData *_us_LastYearsReturn)
{
  double ws[50];
  int k;
  _us_ImportFederalReturnData(fedlogfile, _us_LastYearsReturn);
  if (_us_LastYearsReturn->schedD[21] == 0.0)
  {
;    return;
  }

  if ((absolutev(_us_LastYearsReturn->schedD[21]) >= absolutev(_us_LastYearsReturn->schedD[16])) && (_us_LastYearsReturn->fedline[41] >= 0.0))
  {
;    return;
  }

  for (k = 0; k < 50; k++)
    ws[k] = 0.0;

  ws[1] = _us_LastYearsReturn->fedline[41];
  ws[2] = absolutev(_us_LastYearsReturn->schedD[21]);
  ws[3] = NotLessThanZero(ws[1] + ws[2]);
  ws[4] = smallerof(ws[2], ws[3]);
  for (k = 1; k <= 4; k++)
  {
;    fprintf(outfile, "\tCarryOverWs%d = %2.2f\n", k, ws[k]);
  }

  if (_us_LastYearsReturn->schedD[7] < 0.0)
  {
    ws[5] = -_us_LastYearsReturn->schedD[7];
    ws[6] = NotLessThanZero(_us_LastYearsReturn->schedD[15]);
    ws[7] = ws[4] + ws[6];
    ws[8] = NotLessThanZero(ws[5] - ws[7]);
    if (ws[8] > 0.0)
      _us_SchedD[6] = ws[8];

    for (k = 5; k <= 8; k++)
    {
;      fprintf(outfile, "\tCarryOverWs%d = %2.2f\n", k, ws[k]);
    }

  }
  else
;
  if (_us_LastYearsReturn->schedD[15] < 0.0)
  {
    ws[9] = absolutev(_us_LastYearsReturn->schedD[15]);
    ws[10] = NotLessThanZero(_us_LastYearsReturn->schedD[7]);
    ws[11] = NotLessThanZero(ws[4] - ws[5]);
    ws[12] = ws[10] + ws[11];
    ws[13] = NotLessThanZero(ws[9] - ws[12]);
    if (ws[13] > 0.0)
      _us_SchedD[14] = ws[13];

    for (k = 9; k <= 13; k++)
    {
;      fprintf(outfile, "\tCarryOverWs%d = %2.2f\n", k, ws[k]);
    }

  }
  else
;
}

struct capgain_record
{
  char *comment;
  char *buy_date;
  char *sell_date;
  double buy_amnt;
  double sell_amnt;
  struct capgain_record *nxt;
} *_us_short_trades = 0;
struct capgain_record *_us_long_trades = 0;
double _us_total_sales;
double _us_total_costs = 0.0;
void _us_new_capgain(struct capgain_record **list, char *comment, double buy_amnt, char *buy_date, double sell_amnt, char *sell_date)
{
  struct capgain_record *new_item;
  struct capgain_record *prev;
  new_item = (struct capgain_record *) malloc(sizeof(struct capgain_record));
  new_item->comment = strdup(comment);
  new_item->buy_amnt = buy_amnt;
  new_item->buy_date = strdup(buy_date);
  new_item->sell_amnt = sell_amnt;
  new_item->sell_date = strdup(sell_date);
  new_item->nxt = 0;
  prev = *list;
  if (prev == 0)
    *list = new_item;
  else
  {
    while (prev->nxt != 0)
      prev = prev->nxt;

    prev->nxt = new_item;
  }

}

void _us_print_capgain_list(struct capgain_record *list, int section, char *message, char *pdfmsg)
{
  struct capgain_record *item;
  char word[4096];
  char row = 'a';
  _us_total_sales = 0.0;
  _us_total_costs = 0.0;
  fprintf(outfile, "\n%s\n", message);
  fprintf(outfile, " %d. (a Description)         (b Buy Date) (c Date Sold) (d Sold Price) (e Cost) (h Gain)\n", section);
  fprintf(outfile, " ---------------------------------------------------------------------------------------\n");
  item = list;
  while (item != 0)
  {
    strcpy(word, item->comment);
    if (strlen(word) > 27)
      word[30] = '\0';

    if ((strlen(word) > 0) && (word[strlen(word) - 1] == '}'))
      word[strlen(word) - 1] = '\0';

    while (strlen(word) < 27)
      strcat(word, " ");

    fprintf(outfile, " %s %10s %10s %14.2f %14.2f %14.2f\n", word, item->buy_date, item->sell_date, item->sell_amnt, absolutev(item->buy_amnt), item->sell_amnt + item->buy_amnt);
    _us_total_sales = _us_total_sales + item->sell_amnt;
    _us_total_costs = _us_total_costs + item->buy_amnt;
    item = item->nxt;
  }

  fprintf(outfile, " ---------------------------------------------------------------------------------------\n");
  fprintf(outfile, " %d. Totals:                                        %14.2f %14.2f %14.2f\n\n", section + 1, _us_total_sales, absolutev(_us_total_costs), _us_total_sales + _us_total_costs);
  fprintf(outfile, "PDFpage: %s\n", pdfmsg);
  item = list;
  while (item != 0)
  {
    if (row > 'n')
    {
      fprintf(outfile, " F8949_2d = ...\n");
      fprintf(outfile, " F8949_2e = ...\n");
      fprintf(outfile, " F8949_2h = ...\n");
      fprintf(outfile, "EndPDFpage.\nPDFpage:  %s\n", pdfmsg);
      row = 'a';
    }

    fprintf(outfile, " F8949_1%ca: %s\n", row, item->comment);
    fprintf(outfile, " F8949_1%cb: %s\n", row, item->buy_date);
    fprintf(outfile, " F8949_1%cc: %s\n", row, item->sell_date);
    fprintf(outfile, " F8949_1%cd = %14.2f\n", row, item->sell_amnt);
    fprintf(outfile, " F8949_1%ce = %14.2f\n", row, absolutev(item->buy_amnt));
    fprintf(outfile, " F8949_1%ch = %14.2f\n", row, item->sell_amnt + item->buy_amnt);
    row++;
    item = item->nxt;
  }

  fprintf(outfile, " F8949_2d = %14.2f\n", _us_total_sales);
  fprintf(outfile, " F8949_2e = %14.2f\n", absolutev(_us_total_costs));
  fprintf(outfile, " F8949_2h = %14.2f\n", _us_total_sales + _us_total_costs);
  fprintf(outfile, "EndPDFpage.\n\n");
}

void _us_free_capgain_list(struct capgain_record **list)
{
  struct capgain_record *olditem;
  while ((*list) != 0)
  {
    olditem = *list;
    *list = (*list)->nxt;
    free(olditem->comment);
    free(olditem);
  }

}

void _us_get_gain_and_losses(char *label)
{
  char comment[4096];
  char comment2[2048];
  char date_str1[512];
  char date_str2[512];
  char word[4096];
  double amnt1;
  double amnt2;
  int toggle = 0;
  int date1 = 0;
  int date2;
  int variousdates = 0;
  get_parameter(infile, 's', word, label);
  get_word(infile, word);
  while (word[0] != ';')
  {
    if (feof(infile))
    {
;      fprintf(outfile, "ERROR: Unexpected EOF on '%s'\n", label);
      exit(1);
    }

    if (!_us_Do_SchedD)
    {
      fprintf(outfile, "\nForm(s) 8949:\n");
      _us_Do_SchedD = 1;
    }

    switch (toggle)
    {
      case 0:
        toggle++;
        if (sscanf(word, "%lf", &amnt1) != 1)
      {
;        fprintf(outfile, "ERROR: Bad float '%s', reading %s.\n", word, label);
        exit(1);
      }

        if (amnt1 > 0.0)
        amnt1 = -amnt1;

        break;

      case 1:
        toggle++;
        strcpy(date_str1, word);
        if (mystrcasestr(date_str1, "various-short") != 0)
        variousdates = 1;
      else
        if (mystrcasestr(date_str1, "various-long") != 0)
        variousdates = 2;
      else
      {
        date1 = get_date(word, label);
        variousdates = 0;
      }


        get_comment(infile, comment);
        break;

      case 2:
        toggle++;
        if (sscanf(word, "%lf", &amnt2) != 1)
      {
;        fprintf(outfile, "ERROR: Bad float '%s', reading %s.\n", word, label);
        exit(1);
      }

        break;

      case 3:
        toggle = 0;
        strcpy(date_str2, word);
        if (variousdates == 1)
        date2 = date1 + 2;
      else
        if (variousdates == 2)
        date2 = date1 + (2 * 365);
      else
        date2 = get_date(word, label);


        get_comment(infile, comment2);
        strcat(comment, comment2);
        if ((date2 - date1) < 0)
      {
;        fprintf(outfile, "ERROR: Buy-date after sell-date.\n");
        exit(1);
      }

        if ((date2 - date1) > 365)
      {
        _us_new_capgain(&_us_long_trades, comment, amnt1, date_str1, amnt2, date_str2);
      }
      else
      {
        _us_new_capgain(&_us_short_trades, comment, amnt1, date_str1, amnt2, date_str2);
      }

        break;

    }

    get_word(infile, word);
  }

  if (toggle != 0)
  {
;    fprintf(outfile, "ERROR: Imbalanced cap-gains entry (toggle=%d).\n", toggle);
    exit(1);
  }

}

void _us_get_cap_gains(char *emssg)
{
  char word[4092];
  char *LastYearsOutFile = 0;
  int j;
  int doline22 = 0;
  double stcg = 0.0;
  double ltcg = 0.0;
  double SchedDd[20];
  double SchedDe[20];
  for (j = 0; j < 20; j++)
  {
    SchedDd[j] = 0.0;
    SchedDe[j] = 0.0;
  }

  _us_get_gain_and_losses("CapGains-A/D");
  if (_us_short_trades)
  {
    _us_print_capgain_list(_us_short_trades, 1, "Form 8949 Part-I, Short-Term Cap Gains+Losses, CHECK (A) Basis Reported to IRS:", "9 9\n F8949_ckA X");
    SchedDd[1] = _us_total_sales;
    SchedDe[1] = _us_total_costs;
    _us_SchedD[1] = SchedDd[1] + SchedDe[1];
    _us_free_capgain_list(&_us_short_trades);
  }

  if (_us_long_trades)
  {
    _us_print_capgain_list(_us_long_trades, 3, "Form 8949 Part-II, Long-Term Cap Gains+Losses, CHECK (D) Basis Reported to IRS:", "10 10\n F8949_ckD X");
    SchedDd[8] = _us_total_sales;
    SchedDe[8] = _us_total_costs;
    _us_SchedD[8] = SchedDd[8] + SchedDe[8];
    _us_free_capgain_list(&_us_long_trades);
  }

  _us_get_gain_and_losses("CapGains-B/E");
  if (_us_short_trades)
  {
    _us_print_capgain_list(_us_short_trades, 1, "Form 8949 Part-I, Short-Term Cap Gains+Losses, CHECK (B) Basis NOT Reported to IRS:", "9 9\n F8949_ckB X");
    SchedDd[2] = _us_total_sales;
    SchedDe[2] = _us_total_costs;
    _us_SchedD[2] = SchedDd[2] + SchedDe[2];
    _us_free_capgain_list(&_us_short_trades);
  }

  if (_us_long_trades)
  {
    _us_print_capgain_list(_us_long_trades, 3, "Form 8949 Part-II, Long-Term Cap Gains+Losses, CHECK (E) Basis NOT Reported to IRS:", "10 10\n F8949_ckE X");
    SchedDd[9] = _us_total_sales;
    SchedDe[9] = _us_total_costs;
    _us_SchedD[9] = SchedDd[9] + SchedDe[9];
    _us_free_capgain_list(&_us_long_trades);
  }

  _us_get_gain_and_losses("CapGains-C/F");
  if (_us_short_trades)
  {
    _us_print_capgain_list(_us_short_trades, 1, "Form 8949 Part-I, Short-Term Cap Gains+Losses, CHECK (C) Not reported on Form 1099-B.\n", "9 9\n F8949_ckC X");
    SchedDd[3] = _us_total_sales;
    SchedDe[3] = _us_total_costs;
    _us_SchedD[3] = SchedDd[3] + SchedDe[3];
    _us_free_capgain_list(&_us_short_trades);
  }

  if (_us_long_trades)
  {
    _us_print_capgain_list(_us_long_trades, 3, "Form 8949 Part-II, Long-Term Cap Gains+Losses, CHECK (F) Not reported on Form 1099-B.\n", "10 10\n F8949_ckF X");
    SchedDd[10] = _us_total_sales;
    SchedDe[10] = _us_total_costs;
    _us_SchedD[10] = SchedDd[10] + SchedDe[10];
    _us_free_capgain_list(&_us_long_trades);
  }

  stcg = (_us_SchedD[1] + _us_SchedD[2]) + _us_SchedD[3];
  ltcg = (_us_SchedD[8] + _us_SchedD[9]) + _us_SchedD[10];
  GetLine("D4", &_us_SchedD[4]);
  GetLine("D5", &_us_SchedD[5]);
  get_parameter(infile, 's', word, "D6");
  get_word(infile, word);
  if (strcmp(word, ";") != 0)
  {
    if (sscanf(word, "%lf", &_us_SchedD[6]) != 1)
      LastYearsOutFile = strdup(word);

    do
    {
      get_word(infile, word);
      if ((strlen(word) > 0) && (strcmp(word, ";") != 0))
        fprintf(outfile, "Warning: Unexpected multiple values on line D6.  '%s' ignored.\n If multi-part filename, then surround it in quotes (\").", word);

    }
    while (strcmp(word, ";") != 0);
  }

  GetLine("D11", &_us_SchedD[11]);
  GetLine("D12", &_us_SchedD[12]);
  GetLine("D13", &_us_SchedD[13]);
  GetLine("D14", &_us_SchedD[14]);
  if (LastYearsOutFile != 0)
    _us_CapitalLossCarryOverWorksheet(LastYearsOutFile, &_us_LastYearsReturn);

  if (_us_SchedD[6] > 0.0)
  {
    _us_SchedD[6] = -_us_SchedD[6];
  }

  if (_us_SchedD[14] > 0.0)
  {
    _us_SchedD[14] = -_us_SchedD[14];
  }

  if (((((((_us_SchedD[4] != 0.0) || (_us_SchedD[5] != 0.0)) || (_us_SchedD[6] != 0.0)) || (_us_SchedD[11] != 0.0)) || (_us_SchedD[12] != 0.0)) || (_us_SchedD[13] != 0.0)) || (_us_SchedD[14] != 0.0))
  {
    _us_Do_SchedD = 1;
  }

  if (_us_Do_SchedD)
  {
    fprintf(outfile, " Cap Gains/Losses Schedule-D\n");
    fprintf(outfile, "PDFpage: 5 5\n");
    fprintf(outfile, "\tNet Forms-8949 Short-term Gains = %10.2f\n", stcg);
    fprintf(outfile, "\tNet Forms-8949 Long-term Gains  = %10.2f\n", ltcg);
    fprintf(outfile, " D1bd = %10.2f\n   D1be = %10.2f\n    D1bh = %10.2f\n", SchedDd[1], absolutev(SchedDe[1]), _us_SchedD[1]);
    fprintf(outfile, " D2d = %10.2f\n   D2e = %10.2f\n    D2h = %10.2f\n", SchedDd[2], absolutev(SchedDe[2]), _us_SchedD[2]);
    fprintf(outfile, " D3d = %10.2f\n   D3e = %10.2f\n    D3h = %10.2f\n", SchedDd[3], absolutev(SchedDe[3]), _us_SchedD[3]);
    fprintf(outfile, " D4 = %6.2f\n", _us_SchedD[4]);
    fprintf(outfile, " D5 = %6.2f\n", _us_SchedD[5]);
    fprintf(outfile, " D6 = %6.2f		(Carry-over Loss)\n", _us_SchedD[6]);
    _us_SchedD[7] = ((((_us_SchedD[1] + _us_SchedD[2]) + _us_SchedD[3]) + _us_SchedD[4]) + _us_SchedD[5]) + _us_SchedD[6];
    fprintf(outfile, " D7 = %6.2f		{ Net short-term capital gain or loss }\n", _us_SchedD[7]);
    fprintf(outfile, " D8bd = %10.2f\n   D8be = %10.2f\n   D8bh = %10.2f\n", SchedDd[8], absolutev(SchedDe[8]), _us_SchedD[8]);
    fprintf(outfile, " D9d = %10.2f\n   D9e = %10.2f\n   D9h = %10.2f\n", SchedDd[9], absolutev(SchedDe[9]), _us_SchedD[9]);
    fprintf(outfile, " D10d = %10.2f\n   D10e = %10.2f\n   D10h = %10.2f\n", SchedDd[10], absolutev(SchedDe[10]), _us_SchedD[10]);
    fprintf(outfile, " D11 = %6.2f\n", _us_SchedD[11]);
    fprintf(outfile, " D12 = %6.2f\n", _us_SchedD[12]);
    fprintf(outfile, " D13 = %6.2f\n", _us_SchedD[13]);
    fprintf(outfile, " D14 = %6.2f	(Carry-over Loss)\n", _us_SchedD[14]);
    _us_SchedD[15] = (((((_us_SchedD[8] + _us_SchedD[9]) + _us_SchedD[10]) + _us_SchedD[11]) + _us_SchedD[12]) + _us_SchedD[13]) + _us_SchedD[14];
    fprintf(outfile, " D15 = %6.2f		{ Net long-term capital gain or loss }\n", _us_SchedD[15]);
    fprintf(outfile, "EndPDFpage.\nPDFpage: 6 6\n");
    _us_SchedD[16] = _us_SchedD[7] + _us_SchedD[15];
    fprintf(outfile, " D16 = %6.2f\n", _us_SchedD[16]);
    if (_us_SchedD[16] > 0.0)
    {
      L[13] = _us_SchedD[16];
      if ((_us_SchedD[15] > 0.0) && (_us_SchedD[16] > 0.0))
      {
        double wsd[50];
        fprintf(outfile, " D17 = yes\n CkD17y X\n");
        wsd[1] = _us_collectibles_gains;
        wsd[2] = 0.0;
        wsd[3] = 0.0;
        wsd[4] = 0.0;
        wsd[5] = _us_SchedD[14];
        if (_us_SchedD[7] < 0.0)
          wsd[6] = _us_SchedD[7];
        else
          wsd[6] = 0.0;

        wsd[7] = NotLessThanZero(((((wsd[1] + wsd[2]) + wsd[3]) + wsd[4]) + wsd[5]) + wsd[6]);
        _us_SchedD[18] = wsd[7];
        fprintf(outfile, " D18 = %6.2f\n", _us_SchedD[18]);
        fprintf(outfile, " D19 = %6.2f\n", _us_SchedD[19]);
        if ((_us_SchedD[18] == 0.0) && (_us_SchedD[19] == 0.0))
        {
          fprintf(outfile, " D20 = Yes\n CkD20y X\n");
          _us_Do_QDCGTW = 1;
        }
        else
        {
          fprintf(outfile, " D20 = No\n CkD20n X\n");
          _us_Do_SDTW = 1;
          _us_Do_QDCGTW = 0;
        }

        doline22 = 0;
      }
      else
      {
;        doline22 = 1;
      }

    }
    else
      if (_us_SchedD[16] < 0.0)
    {
      double maxloss;
      if (_us_status == 3)
        maxloss = -1500.0;
      else
        maxloss = -3000.0;

      if (_us_SchedD[16] < maxloss)
        _us_SchedD[21] = maxloss;
      else
        _us_SchedD[21] = _us_SchedD[16];

      fprintf(outfile, " D21 = %6.2f\n", _us_SchedD[21]);
      L[13] = _us_SchedD[21];
      doline22 = 1;
    }
    else
    {
      L[13] = 0.0;
      doline22 = 1;
    }


    if (doline22)
    {
      if (_us_L9b > 0.0)
      {
        fprintf(outfile, " D22 = Yes\n CkD22y X\n");
        _us_Do_QDCGTW = 1;
      }
      else
      {
        fprintf(outfile, " D22 = No\n CkD22n X\n");
      }

    }

    fprintf(outfile, "EndPDFpage.\n\n");
  }

}

void _us_sched_D_tax_worksheet(int _us_status, double _us_L9b)
{
  double ws[100];
  int k;
  for (k = 0; k < 100; k++)
    ws[k] = 0.0;

  ws[1] = L[43];
  ws[2] = _us_L9b;
  ws[3] = 0.0;
  ws[4] = 0.0;
  ws[5] = NotLessThanZero(ws[3] - ws[4]);
  ws[6] = NotLessThanZero(ws[2] - ws[5]);
  ws[7] = smallerof(_us_SchedD[15], _us_SchedD[16]);
  ws[8] = smallerof(ws[3], ws[4]);
  ws[9] = NotLessThanZero(ws[7] - ws[8]);
  ws[10] = ws[6] + ws[9];
  fprintf(outfile, "  Sched-D tax Worksheet line 10 = %6.2f\n", ws[10]);
  ws[11] = _us_SchedD[18] + _us_SchedD[19];
  ws[12] = smallerof(ws[9], ws[11]);
  ws[13] = ws[10] - ws[12];
  ws[14] = NotLessThanZero(ws[1] - ws[13]);
  fprintf(outfile, "  Sched-D tax Worksheet line 13 = %6.2f\n", ws[13]);
  fprintf(outfile, "  Sched-D tax Worksheet line 14 = %6.2f\n", ws[14]);
  switch (_us_status)
  {
    case 1:

    case 3:
      ws[15] = 37950.0;
      break;

    case 2:

    case 5:
      ws[15] = 75900.0;
      break;

    case 4:
      ws[15] = 50800.0;
      break;

  }

  ws[16] = smallerof(ws[1], ws[15]);
  ws[17] = smallerof(ws[14], ws[16]);
  ws[18] = NotLessThanZero(ws[1] - ws[10]);
  ws[19] = largerof(ws[17], ws[18]);
  ws[20] = ws[16] - ws[17];
  if (ws[1] != ws[16])
  {
    ws[21] = smallerof(ws[1], ws[13]);
    ws[22] = ws[20];
    ws[23] = NotLessThanZero(ws[21] - ws[22]);
    switch (_us_status)
    {
      case 1:
        ws[24] = 418400.0;
        break;

      case 3:
        ws[24] = 235350.0;
        break;

      case 2:

      case 5:
        ws[24] = 470700.0;
        break;

      case 4:
        ws[24] = 444550.0;
        break;

    }

    ws[25] = smallerof(ws[1], ws[24]);
    ws[26] = ws[19] + ws[20];
    ws[27] = NotLessThanZero(ws[25] - ws[26]);
    ws[28] = smallerof(ws[23], ws[27]);
    ws[29] = 0.15 * ws[28];
    ws[30] = ws[22] + ws[28];
    if (ws[1] != ws[30])
    {
      ws[31] = ws[21] - ws[30];
      ws[32] = 0.20 * ws[31];
      if (_us_SchedD[19] != 0.0)
      {
        ws[33] = smallerof(ws[9], _us_SchedD[19]);
        ws[34] = ws[10] + ws[19];
        ws[35] = ws[1];
        ws[36] = NotLessThanZero(ws[34] - ws[35]);
        ws[37] = NotLessThanZero(ws[33] - ws[36]);
        ws[38] = 0.25 * ws[37];
      }

      if (_us_SchedD[18] != 0.0)
      {
        ws[39] = (((ws[19] + ws[20]) + ws[28]) + ws[31]) + ws[37];
        ws[40] = ws[1] - ws[39];
        ws[41] = 0.28 * ws[40];
      }

    }

  }

  ws[42] = _us_TaxRateFunction(ws[19], _us_status);
  ws[43] = (((ws[29] + ws[32]) + ws[38]) + ws[41]) + ws[42];
  ws[44] = _us_TaxRateFunction(ws[1], _us_status);
  ws[45] = smallerof(ws[43], ws[44]);
  L[44] = ws[45];
  for (k = 0; k < 100; k++)
    _us_ws_sched_D[k] = ws[k];

}

void _us_SocSec_Worksheet()
{
  double ws[100];
  int k;
  if (L[20] == 0.0)
    return;

  for (k = 0; k < 100; k++)
    ws[k] = 0.0;

  ws[1] = L[20];
  ws[2] = 0.5 * ws[1];
  ws[3] = ((((((((((((L[7] + L[8]) + L[9]) + L[10]) + L[11]) + L[12]) + L[13]) + L[14]) + L[15]) + L[16]) + L[17]) + L[18]) + L[19]) + L[21];
  ws[4] = _us_L8b;
  ws[5] = (ws[2] + ws[3]) + ws[4];
  for (k = 23; k <= 32; k++)
    ws[6] = ws[6] + L[k];

  for (k = 0; k <= 6; k++)
    fprintf(outfile, "\tSocSecWorkSheet[%d] = %6.2f\n", k, ws[k]);

  if (ws[6] >= ws[5])
  {
    L[20] = 0.0;
    fprintf(outfile, "\tSocSecWorkSheet[7]: Check 'No'\n");
;    fprintf(outfile, "None of your social security benefits are taxable.\n");
    return;
  }

  ws[7] = ws[5] - ws[6];
  fprintf(outfile, "\tSocSecWorkSheet[7] = %6.2f  (Check 'Yes')\n", ws[7]);
  if (_us_status == 2)
    ws[8] = 32000.0;
  else
    ws[8] = 25000.0;

  fprintf(outfile, "\tSocSecWorkSheet[8] = %6.2f\n", ws[8]);
  if (ws[8] >= ws[7])
  {
    L[20] = 0.0;
    fprintf(outfile, "\tSocSecWorkSheet[9]: Check 'No'\n");
;    fprintf(outfile, "None of your social security benefits are taxable.\n");
    return;
  }

  ws[9] = ws[7] - ws[8];
  fprintf(outfile, "\tSocSecWorkSheet[9] = %6.2f  (Check 'Yes')\n", ws[9]);
  if (_us_status == 2)
    ws[10] = 12000.0;
  else
    ws[10] = 9000.0;

  ws[11] = NotLessThanZero(ws[9] - ws[10]);
  ws[12] = smallerof(ws[9], ws[10]);
  ws[13] = ws[12] / 2.0;
  ws[14] = smallerof(ws[2], ws[13]);
  ws[15] = 0.85 * ws[11];
  ws[16] = ws[14] + ws[15];
  ws[17] = 0.85 * ws[1];
  ws[18] = smallerof(ws[16], ws[17]);
  for (k = 10; k <= 18; k++)
    fprintf(outfile, "\tSocSecWorkSheet[%d] = %6.2f\n", k, ws[k]);

  L[20] = ws[18];
}

void _us_pull_comment(char *line, char *word)
{
  int j = 0;
  int k = 0;
  while ((line[j] != '\0') && (line[j] != '{'))
    j++;

  if (line[j] != '\0')
  {
    j++;
    while ((line[j + k] != '\0') && (line[j + k] != '}'))
    {
      word[k] = line[j + k];
      k++;
    }

  }

  word[k] = '\0';
}

void _us_Grab_ScheduleB_Payer_Lines(char *infname, FILE *outfile)
{
  int state = 0;
  int cnt = 0;
  int pg = 0;
  int ncnt = 15;
  int newentry = 0;
  double value;
  double total = 0.0;
  char line[2048];
  char word1[1024];
  char word2[1024];
  char pgstr[10] = "";
  FILE *infile;
  infile = fopen(infname, "rb");
  if (infile == 0)
  {
;    return;
  }

  fprintf(outfile, "\nSchedules Data:\n");
  fgets(line, 200, infile);
  while (!feof(infile))
  {
    next_word(line, word1, " \t\n\r");
    switch (state)
    {
      case 0:
        if (strcmp(word1, "L8a") == 0)
      {
        state = 8;
        ncnt = 15;
        pg = 0;
        cnt = 0;
        newentry = 1;
        strcpy(pgstr, "B1_");
      }
      else
        if (strcmp(word1, "L9") == 0)
      {
        if (pg > 0)
        {
          fprintf(outfile, "EndPDFpage.\n");
        }

        state = 9;
        ncnt = 17;
        total = 0.0;
        pg = 0;
        cnt = 0;
        newentry = 1;
        strcpy(pgstr, "B5_");
      }


        break;

      case 8:
        if (word1[0] == ';')
      {
        state = 0;
        if (pg > 0)
        {
          fprintf(outfile, "Btotal = %8.2f\n", total);
          fprintf(outfile, "EndPDFpage.\n");
          pg = 0;
        }

      }
      else
        if ((word1[0] != '\0') && (word1[0] != '{'))
      {
        _us_pull_comment(line, word2);
        cnt++;
        if (cnt == ncnt)
        {
          if (pg > 0)
          {
            fprintf(outfile, "Btotal = %8.2f\n", total);
            fprintf(outfile, "EndPDFpage.\n");
          }

          fprintf(outfile, "PDFpage: 11 4\n");
          fprintf(outfile, "SchedB_Additional_form:  Schedule B - Additional Interest Income\n");
          strcpy(pgstr, "Baddi_");
          cnt = 1;
          ncnt = 30;
          total = 0.0;
          pg++;
        }

        fprintf(outfile, " %s%d_Text: %s\n", pgstr, cnt, word2);
        remove_certain_chars(word1, ",");
        if (sscanf(word1, "%lf", &value) != 1)
;        else
        {
          fprintf(outfile, " %s%d %8.2f\n", pgstr, cnt, value);
          total = total + value;
        }

      }


        break;

      case 9:
        if (word1[0] == ';')
      {
        state = 0;
        if (pg > 0)
        {
          fprintf(outfile, "Btotal = %8.2f\n", total);
          fprintf(outfile, "EndPDFpage.\n");
          pg = 0;
        }

      }
      else
        if ((word1[0] != '\0') && (word1[0] != '{'))
      {
        _us_pull_comment(line, word2);
        cnt++;
        if (cnt == ncnt)
        {
          if (pg > 0)
          {
            fprintf(outfile, "Btotal = %8.2f\n", total);
            fprintf(outfile, "EndPDFpage.\n");
          }

          fprintf(outfile, "PDFpage: 11 4\n");
          fprintf(outfile, "SchedB_Additional_form:  Schedule B - Additional Dividend Income\n");
          strcpy(pgstr, "Baddi_");
          cnt = 1;
          ncnt = 30;
          total = 0.0;
          pg++;
        }

        fprintf(outfile, " %s%d_Text: %s\n", pgstr, cnt, word2);
        remove_certain_chars(word1, ",");
        if (sscanf(word1, "%lf", &value) != 1)
;        else
        {
          fprintf(outfile, " %s%d %8.2f\n", pgstr, cnt, value);
          total = total + value;
        }

      }


        break;

    }

    if (!newentry)
      fgets(line, 200, infile);
    else
      newentry = 0;

  }

  if (pg > 0)
  {
;    fprintf(outfile, "Btotal = %6.2f\n", total);
    fprintf(outfile, "EndPDFpage.\n");
  }

  fclose(infile);
}

int _us_main(int argc, char *argv[])
{
  int argk;
  int j;
  int k;
  int itemize = 0;
  int gotL49 = 0;
  char word[2000];
  char outfname[2000];
  char *infname = "";
  char labelx[1024];
  time_t now;
  double exemption_threshold = 0.0;
  double dedexws[20];
  double tmpval;
  double S_STD_DEDUC;
  double MFS_STD_DEDUC;
  double MFJ_STD_DEDUC;
  double HH_STD_DEDUC;
  double std_deduc;
  char *Your1stName;
  char *YourLastName;
  char *Spouse1stName;
  char *SpouseLastName;
  char *socsec;
  char socsectmp[100];
;  argk = 1;
  k = 1;
  while (argk < argc)
  {
    if (strcmp(argv[argk], "-verbose") == 0)
    {
      verbose = 1;
    }
    else
      if (k == 1)
    {
      infname = strdup(argv[argk]);
      infile = fopen(infname, "r");
      if (infile == 0)
      {
;        exit(1);
      }

      k = 2;
      strcpy(outfname, infname);
      j = strlen(outfname) - 1;
      while ((j >= 0) && (outfname[j] != '.'))
        j--;

      if (j < 0)
        strcat(outfname, "_out.txt");
      else
        strcpy(&outfname[j], "_out.txt");

      outfile = fopen(outfname, "w");
      if (outfile == 0)
      {
;        exit(1);
      }

;    }
    else
    {
;      exit(1);
    }


    argk = argk + 1;
  }

  if (infile == 0)
  {
;    exit(1);
  }

  for (j = 0; j < 1000; j++)
  {
    L[j] = 0.0;
    _us_SchedA[j] = 0.0;
    _us_SchedD[j] = 0.0;
    _us_idws[j] = 0.0;
    _us_ws_sched_D[j] = 0.0;
    _us_amtws[j] = 0.0;
  }

  read_line(infile, word);
  now = time(0);
  fprintf(outfile, "\n%s,	 v%2.2f, %s\n", word, _us_thisversion, ctime(&now));
  get_parameter(infile, 's', word, "Status");
  get_parameter(infile, 'l', word, "Status?");
  if (strncasecmp(word, "Single", 4) == 0)
    _us_status = 1;
  else
    if (strncasecmp(word, "Married/Joint", 13) == 0)
    _us_status = 2;
  else
    if (strncasecmp(word, "Married/Sep", 11) == 0)
    _us_status = 3;
  else
    if (strncasecmp(word, "Head_of_House", 4) == 0)
    _us_status = 4;
  else
    if (strncasecmp(word, "Widow", 4) == 0)
    _us_status = 5;
  else
  {
;    fprintf(outfile, "Error: unrecognized status '%s'. Exiting.\n", word);
    exit(1);
  }





  fprintf(outfile, "Status = %s (%d)\n", word, _us_status);
  get_parameter(infile, 's', word, "Under65?");
  get_parameter(infile, 'l', word, "Under65?");
  if (strncasecmp(word, "Yes", 1) == 0)
  {
    if (_us_status == 2)
      _us_under65 = 2;
    else
      _us_under65 = 1;

  }
  else
    if (strncasecmp(word, "No", 1) == 0)
    _us_under65 = 0;
  else
  {
    if (((sscanf(word, "%d", &_us_under65) != 1) || (_us_under65 < 0)) || (_us_under65 > 2))
    {
;      fprintf(outfile, "Error: unrecognized answer to 'Under65?' ('%s'). Exiting.\n", word);
      exit(1);
    }

  }


  fprintf(outfile, " (Under65 = %d)\n", _us_under65);
  switch (_us_status)
  {
    case 1:
      fprintf(outfile, "CkSingle X\nCkYourself X\nL6ab = 1\n");
      break;

    case 2:
      fprintf(outfile, "CkMFJ X\nCkYourself X\nCkSpouse X\nL6ab = 2\n");
      break;

    case 3:
      fprintf(outfile, "CkMFS X\nCkYourself X\nL6ab = 1\n");
      break;

    case 4:
      fprintf(outfile, "CkHH X\nCkYourself X\nL6ab = 1\n");
      break;

    case 5:
      fprintf(outfile, "CkQW X\nCkYourself X\nL6ab = 1\n");
      break;

    default:
;
  }

  GetLine1("Dependents", &L[6]);
  shownum(6);
  GetLineF("L7", &L[7]);
  GetLineF("L8a", &L[8]);
  GetLineFnz("L8b", &_us_L8b);
  GetLineF("L9", &L[9]);
  GetLineF("L9b", &_us_L9b);
  if (_us_L9b > 0.0)
    _us_Do_QDCGTW = 1;

  if ((L[8] != 0.0) || (L[9] != 0.0))
  {
    fprintf(outfile, " Schedule-B:\n");
    fprintf(outfile, "  B2 = %6.2f\n", L[8]);
    fprintf(outfile, "  B4 = %6.2f\n", L[8]);
    fprintf(outfile, "  B6 = %6.2f\n", L[9]);
  }

  GetLineF("L10", &L[10]);
  GetLineF("L11", &L[11]);
  GetLineF("L12", &L[12]);
  GetLine("Collectibles", &_us_collectibles_gains);
  if (_us_collectibles_gains != 0.0)
    fprintf(outfile, "Collectibles_Gains = %6.2f\n", _us_collectibles_gains);

  _us_get_cap_gains("L13");
  showline(13);
  GetLine("L14", &L[14]);
  ShowLineNonZero(14);
  GetLine("L15a", &tmpval);
  showline_wlabel("L15a", tmpval);
  GetLine("L15b", &L[15]);
  ShowLineNonZero(15);
  GetLine("L16a", &tmpval);
  showline_wlabel("L16a", tmpval);
  GetLine("L16b", &L[16]);
  showline_wlabel("L16b", L[16]);
  GetLine("L17", &L[17]);
  ShowLineNonZero(17);
  GetLine("L18", &L[18]);
  ShowLineNonZero(18);
  GetLine("L19", &L[19]);
  ShowLineNonZero(19);
  GetLine("L20a", &L[20]);
  showline_wlabel("L20a", L[20]);
  GetLine("L21", &L[21]);
  GetLine("L23", &L[23]);
  GetLine("L24", &L[24]);
  GetLine("L25", &L[25]);
  GetLine("L26", &L[26]);
  GetLine("L27", &L[27]);
  GetLine("L28", &L[28]);
  GetLine("L29", &L[29]);
  GetLine("L30", &L[30]);
  GetLine("L31a", &L[31]);
  GetLine("L32", &L[32]);
  GetLine("L33", &L[33]);
  GetOptionalLine("L34 or L35", labelx, &tmpval);
  if (strcmp(labelx, "L34") == 0)
  {
    L[34] = tmpval;
    GetLine("L35", &L[35]);
  }
  else
    if (strcmp(labelx, "L35") == 0)
    L[35] = tmpval;
  else
  {
;    fprintf(outfile, "ERROR1: Found '%s' when expecting 'L34 or L35'\n", labelx);
    exit(1);
  }


  _us_SocSec_Worksheet();
  showline_wlabel("L20b", L[20]);
  ShowLineNonZero(21);
  for (j = 7; j <= 21; j++)
    L[22] = L[22] + L[j];

  showline_wmsg(22, "total income");
  if (_us_under65 == 0)
    _us_over65 = 1;

  switch (_us_status)
  {
    case 1:
      if (_us_under65)
      exemption_threshold = 10400.0;
    else
      exemption_threshold = 11950.0;

      break;

    case 2:
      if (_us_under65 == 2)
      exemption_threshold = 20800.0;
    else
      if (_us_under65 == 1)
      exemption_threshold = 22050.0;
    else
      exemption_threshold = 23300.0;


      if (_us_under65 != 2)
      _us_over65 = 1;

      break;

    case 3:
      exemption_threshold = 4050.0;
      break;

    case 4:
      if (_us_under65)
      exemption_threshold = 13400.0;
    else
      exemption_threshold = 14950.0;

      break;

    case 5:
      if (_us_under65)
      exemption_threshold = 16750.0;
    else
      exemption_threshold = 18000.0;


  }

  if (L[22] < exemption_threshold)
  {
;;    fprintf(outfile, "You may not need to file a return, due to your income level.\n");
  }

  ShowLineNonZero(23);
  ShowLineNonZero(24);
  if (L[33] != 0.0)
  {
    double ws[20];
    ws[1] = smallerof(L[33], 2500.0);
    ws[2] = L[22];
    ws[3] = (((((((L[23] + L[24]) + L[25]) + L[26]) + L[27]) + L[29]) + L[30]) + L[31]) + L[32];
    ws[4] = ws[2] - ws[3];
    if (_us_status == 2)
      ws[5] = 135000.0;
    else
      ws[5] = 65000.0;

    if (ws[4] > ws[5])
    {
      ws[6] = ws[4] - ws[5];
      if (_us_status == 2)
        ws[7] = ws[6] / 30000.0;
      else
        ws[7] = ws[6] / 15000.0;

      if (ws[7] >= 1.0)
        ws[7] = 1.0;

      ws[8] = ws[1] * ws[7];
    }
    else
      ws[8] = 0.0;

    ws[9] = ws[1] - ws[8];
    L[33] = ws[9];
  }

  ShowLineNonZero(25);
  ShowLineNonZero(26);
  ShowLineNonZero(27);
  ShowLineNonZero(28);
  ShowLineNonZero(29);
  ShowLineNonZero(30);
  ShowLineNonZero(31);
  ShowLineNonZero(32);
  ShowLineNonZero(33);
  ShowLineNonZero(34);
  ShowLineNonZero(35);
  for (j = 23; j <= 35; j++)
    L[36] = L[36] + L[j];

  showline(36);
  L[37] = L[22] - L[36];
  showline_wmsg(37, "Adjusted Gross Income");
  L[38] = L[37];
  showline(38);
  GetLine("L39a", &L[39]);
  fprintf(outfile, "L39a = %d\n", (int) L[39]);
  GetLine("A1", &_us_SchedA[1]);
  _us_showschedA(1);
  _us_SchedA[2] = L[38];
  _us_showschedA(2);
  _us_SchedA[3] = 0.075 * _us_SchedA[2];
  _us_showschedA(3);
  _us_SchedA[4] = NotLessThanZero(_us_SchedA[1] - _us_SchedA[3]);
  _us_showschedA(4);
  GetLine("A5", &_us_SchedA[5]);
  _us_showschedA(5);
  GetLine("A6", &_us_SchedA[6]);
  _us_showschedA(6);
  GetLine("A7", &_us_SchedA[7]);
  _us_showschedA(7);
  GetLine("A8", &_us_SchedA[8]);
  _us_showschedA(8);
  _us_SchedA[9] = ((_us_SchedA[5] + _us_SchedA[6]) + _us_SchedA[7]) + _us_SchedA[8];
  _us_showschedA(9);
  GetLine("A10", &_us_SchedA[10]);
  _us_showschedA(10);
  GetLine("A11", &_us_SchedA[11]);
  _us_showschedA(11);
  GetLine("A12", &_us_SchedA[12]);
  _us_showschedA(12);
  GetLine("A14", &_us_SchedA[14]);
  _us_showschedA(14);
  _us_SchedA[15] = (((_us_SchedA[10] + _us_SchedA[11]) + _us_SchedA[12]) + _us_SchedA[13]) + _us_SchedA[14];
  _us_showschedA(15);
  GetLine("A16", &_us_SchedA[16]);
  _us_showschedA(16);
  GetLine("A17", &_us_SchedA[17]);
  _us_showschedA(17);
  GetLine("A18", &_us_SchedA[18]);
  _us_showschedA_wMsg(18, "Carryover from prior year");
  _us_SchedA[19] = (_us_SchedA[16] + _us_SchedA[17]) + _us_SchedA[18];
  _us_showschedA(19);
  GetLine("A20", &_us_SchedA[20]);
  _us_showschedA(20);
  GetLine("A21", &_us_SchedA[21]);
  _us_showschedA(21);
  GetLine("A22", &_us_SchedA[22]);
  _us_showschedA(22);
  GetLine("A23", &_us_SchedA[23]);
  _us_showschedA(23);
  _us_SchedA[24] = (_us_SchedA[21] + _us_SchedA[22]) + _us_SchedA[23];
  _us_showschedA(24);
  _us_SchedA[25] = L[38];
  _us_showschedA(25);
  _us_SchedA[26] = 0.02 * _us_SchedA[25];
  _us_showschedA(26);
  _us_SchedA[27] = NotLessThanZero(_us_SchedA[24] - _us_SchedA[26]);
  _us_showschedA(27);
  GetLine("A28", &_us_SchedA[28]);
  _us_showschedA(28);
  switch (_us_status)
  {
    case 1:
      _us_idws_thresh = 261500.0;
      break;

    case 2:

    case 5:
      _us_idws_thresh = 313800.0;
      break;

    case 3:
      _us_idws_thresh = 156900.0;
      break;

    case 4:
      _us_idws_thresh = 287650.0;
      break;

    default:
;      exit(1);

  }

  if (L[38] <= 156900.0)
  {
    fprintf(outfile, " Your deduction is not limited.\nCkDedNotLim X\n");
    _us_SchedA[29] = (((((_us_SchedA[4] + _us_SchedA[9]) + _us_SchedA[15]) + _us_SchedA[19]) + _us_SchedA[20]) + _us_SchedA[27]) + _us_SchedA[28];
    fprintf(outfile, " CkDedNotLim X\n");
  }
  else
  {
    _us_idws[1] = (((((_us_SchedA[4] + _us_SchedA[9]) + _us_SchedA[15]) + _us_SchedA[19]) + _us_SchedA[20]) + _us_SchedA[27]) + _us_SchedA[28];
    fprintf(outfile, " Your deduction may be limited (from %8.2f).\n", _us_idws[1]);
    fprintf(outfile, " CkDedMayLim X\n");
    _us_idws[2] = (_us_SchedA[4] + _us_SchedA[14]) + _us_SchedA[20];
    if (_us_idws[2] >= _us_idws[1])
    {
      fprintf(outfile, " Your deduction is not limited. (%8.2f >= %8.2f)\n", _us_idws[2], _us_idws[1]);
      _us_SchedA[29] = _us_idws[1];
    }
    else
    {
      _us_idws[3] = _us_idws[1] - _us_idws[2];
      _us_idws[4] = 0.80 * _us_idws[3];
      _us_idws[5] = L[38];
      _us_idws[6] = _us_idws_thresh;
      if (_us_idws[6] < _us_idws[5])
      {
        _us_idws[7] = _us_idws[5] - _us_idws[6];
        _us_idws[8] = 0.03 * _us_idws[7];
        _us_idws[9] = smallerof(_us_idws[4], _us_idws[8]);
        _us_idws[10] = _us_idws[1] - _us_idws[9];
        _us_SchedA[29] = _us_idws[10];
      }
      else
        _us_SchedA[29] = _us_idws[1];

    }

    fprintf(outfile, " Itemized Deductions Worksheet:\n");
    for (j = 1; j <= 10; j++)
      if (_us_idws[j] != 0.0)
    {
;      fprintf(outfile, " IDWS[%d] = %6.2f\n", j, _us_idws[j]);
    }


  }

  _us_showschedA(29);
  L[40] = _us_SchedA[29];
  if (L[40] > 0.0)
    itemize = 1;
  else
    itemize = 0;

  if (L[39] == 0.0)
  {
    S_STD_DEDUC = 6350.0;
    MFS_STD_DEDUC = 6350.0;
    MFJ_STD_DEDUC = 12700.0;
    HH_STD_DEDUC = 9350.0;
  }
  else
  {
    switch ((int) L[39])
    {
      case 1:
        S_STD_DEDUC = 7900.0;
        MFJ_STD_DEDUC = 13950.0;
        MFS_STD_DEDUC = 7600.0;
        HH_STD_DEDUC = 10900.0;
        break;

      case 2:
        S_STD_DEDUC = 9450.0;
        MFJ_STD_DEDUC = 15200.0;
        MFS_STD_DEDUC = 8850.0;
        HH_STD_DEDUC = 12450.0;
        break;

      case 3:
        MFJ_STD_DEDUC = 16450.0;
        MFS_STD_DEDUC = 10100.0;
        S_STD_DEDUC = 9450.0;
        HH_STD_DEDUC = 12450.0;
        break;

      case 4:
        MFJ_STD_DEDUC = 17700.0;
        MFS_STD_DEDUC = 11350.0;
        S_STD_DEDUC = 9450.0;
        HH_STD_DEDUC = 12450.0;
        break;

      default:
        fprintf(outfile, "Error: L[39] (%g) not equal to 1, 2, 3, or 4.\n", L[39]);
;        exit(1);

    }

    fprintf(outfile, "(Assuming no one is claiming your or your joint-spouse as a dependent.)\n");
  }

  switch (_us_status)
  {
    case 1:
      std_deduc = S_STD_DEDUC;
      break;

    case 3:
      std_deduc = MFS_STD_DEDUC;
      break;

    case 5:

    case 2:
      std_deduc = MFJ_STD_DEDUC;
      break;

    case 4:
      std_deduc = HH_STD_DEDUC;
      break;

    default:
;      fprintf(outfile, "Case (Line 40) not handled.\n");
      exit(1);

  }

  if (L[40] <= std_deduc)
  {
;    fprintf(outfile, "	(Itemizations < Std-Deduction, %6.2f < %6.2f)\n", L[40], std_deduc);
    L[40] = std_deduc;
    fprintf(outfile, "Use standard deduction.\n");
    itemize = 0;
  }
  else
  {
;    fprintf(outfile, "	(Itemizations > Std-Deduction, %6.2f > %6.2f)\n", L[40], std_deduc);
    fprintf(outfile, "Itemizing.\n");
  }

  showline(40);
  L[41] = L[38] - L[40];
  showline(41);
  if (L[38] <= _us_idws_thresh)
    L[42] = 4050.0 * L[6];
  else
  {
    double thresh;
    dedexws[2] = 4050.0 * L[6];
    dedexws[3] = L[38];
    dedexws[4] = _us_idws_thresh;
    dedexws[5] = dedexws[3] - dedexws[4];
    if (_us_status == 3)
      thresh = 61250.0;
    else
      thresh = 122500.0;

    if (dedexws[5] > thresh)
      L[42] = 0.0;
    else
    {
      int j;
      if (_us_status != 3)
        j = (dedexws[5] / 2500.0) + 0.999999;
      else
        j = (dedexws[5] / 1250.0) + 0.999999;

      dedexws[6] = j;
      dedexws[7] = 0.02 * dedexws[6];
      dedexws[8] = dedexws[2] * dedexws[7];
      dedexws[9] = dedexws[2] - dedexws[8];
      L[42] = dedexws[9];
    }

  }

  showline(42);
  L[43] = NotLessThanZero(L[41] - L[42]);
  showline_wmsg(43, "Taxable income");
  L[44] = _us_TaxRateFunction(L[43], _us_status);
  if (L[43] <= 0.0)
  {
;  }
  else
  {
    if (((!_us_Do_SDTW) && (!_us_Do_QDCGTW)) && (((_us_L9b > 0.0) || (L[13] > 0.0)) || ((_us_SchedD[15] > 0.0) && (_us_SchedD[16] > 0.0))))
      _us_Do_QDCGTW = 1;

    if (_us_Do_QDCGTW)
    {
      fprintf(outfile, "Doing 'Qualified Dividends and Capital Gain tax Worksheet', page 44.\n");
      _us_capgains_qualdividends_worksheets(_us_status, _us_L9b);
    }
    else
      if (_us_Do_SDTW)
    {
      fprintf(outfile, "Doing 'Schedule D Tax Worksheet', page D9.\n");
      _us_sched_D_tax_worksheet(_us_status, _us_L9b);
    }


  }

  showline_wmsg(44, "Tax");
  GetLine("L46", &L[46]);
  GetLine("L48", &L[48]);
  while (!gotL49)
  {
    GetOptionalLine("L49 or AMTws28", labelx, &tmpval);
    if (strcmp(labelx, "L49") == 0)
    {
      L[49] = tmpval;
      gotL49 = 1;
    }
    else
      if (strstr(labelx, "AMTws") != 0)
    {
      if (((sscanf(&labelx[5], "%d", &j) == 1) && (j >= 8)) && (j < 27))
        _us_amtws[j] = tmpval;
      else
      {
;        fprintf(outfile, "ERROR reading '%s'.\n", labelx);
      }

    }
    else
    {
;      fprintf(outfile, "ERROR1: Found '%s' when expecting 'L49 or AMTwsXX'\n", labelx);
      exit(1);
    }


  }

  L[45] = _us_form6251_AlternativeMinimumTax(itemize);
  if (L[45] == 0.0)
    fprintf(outfile, " (Not subject to Alternative Minimum Tax.)\n");
  else
    fprintf(outfile, " (You must pay Alternative Minimum Tax.)\n");

  ShowLineNonZero_wMsg(45, "Alternative Minimum Tax");
  _us_Report_bracket_info(L[43], L[45], _us_status);
  showline(46);
  L[47] = (L[44] + L[45]) + L[46];
  showline(47);
  ShowLineNonZero(48);
  ShowLineNonZero(49);
  GetLine("L50", &L[50]);
  ShowLineNonZero(50);
  GetLine("L51", &L[51]);
  ShowLineNonZero(51);
  GetLine("L52", &L[52]);
  ShowLineNonZero(52);
  GetLine("L53", &L[53]);
  ShowLineNonZero(53);
  GetLine("L54", &L[54]);
  ShowLineNonZero(54);
  for (j = 48; j <= 54; j++)
    L[55] = L[55] + L[j];

  showline(55);
  L[56] = NotLessThanZero(L[47] - L[55]);
  showline(56);
  GetLine("L57", &L[57]);
  ShowLineNonZero(57);
  GetLine("L58", &L[58]);
  ShowLineNonZero(58);
  GetLine("L59", &L[59]);
  ShowLineNonZero(59);
  GetLine("L60a", &L[60]);
  if (L[60] != 0.0)
    fprintf(outfile, "L60a = %6.2f\n", L[60]);

  GetLine("L60b", &_us_L60b);
  if (_us_L60b != 0.0)
    fprintf(outfile, "L60b = %6.2f\n", _us_L60b);

  L[60] = L[60] + _us_L60b;
  GetLine("L61", &L[61]);
  ShowLineNonZero(61);
  GetLine("L62", &L[62]);
  ShowLineNonZero(62);
  for (j = 56; j <= 62; j++)
    L[63] = L[63] + L[j];

  showline_wmsg(63, "total tax");
  GetLineF("L64", &L[64]);
  GetLine("L65", &L[65]);
  ShowLineNonZero(65);
  GetLine("L66a", &L[66]);
  if (L[66] != 0.0)
    showline_wlabel("L66a", L[66]);

  GetLine("L66b", &tmpval);
  if (tmpval != 0.0)
    showline_wlabel("L66b", tmpval);

  GetLine("L67", &L[67]);
  ShowLineNonZero(67);
  GetLine("L68", &L[68]);
  ShowLineNonZero(68);
  GetLine("L69", &L[69]);
  ShowLineNonZero(69);
  GetLine("L70", &L[70]);
  ShowLineNonZero(70);
  GetLine("L71", &L[71]);
  ShowLineNonZero(71);
  GetLine("L72", &L[72]);
  ShowLineNonZero(72);
  GetLine("L73", &L[73]);
  ShowLineNonZero(73);
  for (j = 64; j <= 73; j++)
    L[74] = L[74] + L[j];

  showline_wmsg(74, "total payments");
  if (L[74] > L[63])
  {
    L[75] = L[74] - L[63];
    fprintf(outfile, "L75 = %6.2f  REBATE!!!\n", L[75]);
    fprintf(outfile, "L76a = %6.2f \n", L[75]);
  }
  else
  {
    L[78] = L[63] - L[74];
    fprintf(outfile, "L78 = %6.2f  DUE !!!\n", L[78]);
    fprintf(outfile, "         (Which is %2.1f%% of your Total Federal Tax.)\n", (100.0 * L[78]) / (L[63] + 1e-9));
  }

  fprintf(outfile, "\n{ --------- Identity-Information:  --------- }\n");
  Your1stName = GetTextLineF("Your1stName:");
  YourLastName = GetTextLineF("YourLastName:");
  writeout_line = 0;
  socsec = GetTextLineF("YourSocSec#:");
  strcpy(socsectmp, socsec);
  format_socsec(socsectmp, 0);
  fprintf(outfile, "YourSocSec#: %s\n", socsectmp);
  free(socsec);
  writeout_line = 1;
  Spouse1stName = GetTextLineF("Spouse1stName:");
  SpouseLastName = GetTextLineF("SpouseLastName:");
  writeout_line = 0;
  socsec = GetTextLineF("SpouseSocSec#:");
  strcpy(socsectmp, socsec);
  format_socsec(socsectmp, 0);
  fprintf(outfile, "SpouseSocSec#: %s\n", socsectmp);
  free(socsec);
  writeout_line = 1;
  if (strlen(YourLastName) > 0)
  {
    if (strcmp(YourLastName, SpouseLastName) == 0)
      fprintf(outfile, "YourNames: %s & %s, %s\n", Your1stName, Spouse1stName, YourLastName);
    else
      if (strlen(SpouseLastName) > 0)
      fprintf(outfile, "YourNames: %s %s & %s %s\n", Your1stName, YourLastName, Spouse1stName, SpouseLastName);
    else
      fprintf(outfile, "YourNames: %s %s\n", Your1stName, YourLastName);


  }

  GetTextLineF("Number&Street:");
  GetTextLineF("Apt#:");
  GetTextLineF("TownStateZip:");
  fclose(infile);
  _us_Grab_ScheduleB_Payer_Lines(infname, outfile);
  fclose(outfile);
;  Display_File(outfname);
  return 0;
}


/* END of taxsolve_US_1040_2017.c */
/* START of taxsolve_US_1040_Sched_C_2017.c */
float _us_c_thisversion = 15.01;
int _us_c_main(int argc, char *argv[])
{
  int i;
  int j;
  int k;
  char word[4000];
  char outfname[4000];
  char *EIN = 0;
  char *answ;
  time_t now;
  double L16b = 0.0;
  double L20b = 0.0;
  double L24b = 0.0;
  double Mileage = 0.0;
  int L32;
;  i = 1;
  k = 1;
  while (i < argc)
  {
    if (strcmp(argv[i], "-verbose") == 0)
    {
      verbose = 1;
    }
    else
      if (k == 1)
    {
      infile = fopen(argv[i], "r");
      if (infile == 0)
      {
;        exit(1);
      }

      k = 2;
      strcpy(outfname, argv[i]);
      j = strlen(outfname) - 1;
      while ((j >= 0) && (outfname[j] != '.'))
        j--;

      if (j < 0)
        strcat(outfname, "_out.txt");
      else
        strcpy(&outfname[j], "_out.txt");

      outfile = fopen(outfname, "w");
      if (outfile == 0)
      {
;        exit(1);
      }

;    }
    else
    {
;      exit(1);
    }


    i = i + 1;
  }

  if (infile == 0)
  {
;    exit(1);
  }

  for (i = 0; i < 1000; i++)
  {
    L[i] = 0.0;
  }

  read_line(infile, word);
  now = time(0);
  fprintf(outfile, "\n%s,	 v%2.2f, %s\n", word, _us_c_thisversion, ctime(&now));
  GetTextLineF("YourName:");
  GetTextLineF("YourSocSec#:");
  GetTextLineF("PrincipalBus:");
  GetTextLineF("BusinessName:");
  GetTextLineF("Number&Street:");
  GetTextLineF("TownStateZip:");
  GetTextLineF("ActivityCode:");
  writeout_line = 0;
  EIN = GetTextLineF("BusinessEIN:");
  format_socsec(EIN, 1);
  fprintf(outfile, "BusinessEIN: %s\n", EIN);
  answ = GetTextLineF("Fmethod:");
  next_word(answ, word, " \t;");
  if (strcasecmp(word, "Cash") == 0)
    fprintf(outfile, "CkFcash: X\n");
  else
    if (strcasecmp(word, "Accrual") == 0)
    fprintf(outfile, "CkFsccrual: X\n");
  else
    if (strcasecmp(word, "Other") == 0)
    fprintf(outfile, "CkFother: X\n");



  answ = GetTextLineF("GPartic:");
  next_word(answ, word, " \t;");
  if (toupper(word[0]) == 'Y')
    fprintf(outfile, "CkParticipate: X\n");
  else
    if ((mystrcasestr(word, "N/A") == 0) && (toupper(word[0]) == 'N'))
    fprintf(outfile, "CkNotParticipate: X\n");


  answ = GetTextLineF("Hacquired:");
  next_word(answ, word, " \t;");
  if (toupper(word[0]) == 'Y')
    fprintf(outfile, "CkAcquired: X\n");

  answ = GetTextLineF("Ireq1099s:");
  next_word(answ, word, " \t;");
  if (toupper(word[0]) == 'Y')
    fprintf(outfile, "CkReq1099: X\n");
  else
    if ((mystrcasestr(word, "N/A") == 0) && (toupper(word[0]) == 'N'))
    fprintf(outfile, "CkNotReq1099: X\n");


  answ = GetTextLineF("Jfile1099s:");
  next_word(answ, word, " \t;");
  if (toupper(word[0]) == 'Y')
    fprintf(outfile, "CkWillFile1099: X\n");
  else
    if ((mystrcasestr(word, "N/A") == 0) && (toupper(word[0]) == 'N'))
    fprintf(outfile, "CkNotFile1099: X\n");


  writeout_line = 1;
  GetLine("L1", &L[1]);
  GetLine("L2", &L[2]);
  GetLine("L6", &L[6]);
  GetLine("L8", &L[8]);
  GetLine("L9", &L[9]);
  GetLine("Miles", &Mileage);
  L[9] = L[9] + (0.535 * Mileage);
  GetLine("L10", &L[10]);
  GetLine("L11", &L[11]);
  GetLine("L12", &L[12]);
  GetLine("L13", &L[13]);
  GetLine("L14", &L[14]);
  GetLine("L15", &L[15]);
  GetLine("L16a", &L[16]);
  GetLine("L16b", &L16b);
  GetLine("L17", &L[17]);
  GetLine("L18", &L[18]);
  GetLine("L19", &L[19]);
  GetLine("L20a", &L[20]);
  GetLine("L20b", &L20b);
  GetLine("L21", &L[21]);
  GetLine("L22", &L[22]);
  GetLine("L23", &L[23]);
  GetLine("L24a", &L[24]);
  GetLine("L24b", &L24b);
  GetLine("L25", &L[25]);
  GetLine("L26", &L[26]);
  GetLine("L27", &L[27]);
  GetLine("L30", &L[30]);
  get_parameter(infile, 's', word, "L32a");
  get_parameter(infile, 'b', &L32, "L32a");
  writeout_line = 0;
  answ = GetTextLineF("L33:");
  next_word(answ, word, " \t;");
  if (strcasecmp(word, "Cost") == 0)
    fprintf(outfile, "Ck33aCost: X\n");
  else
    if (strcasecmp(word, "Market") == 0)
    fprintf(outfile, "Ck33bMarket: X\n");
  else
    if (strcasecmp(word, "Other") == 0)
    fprintf(outfile, "Ck33cOther: X\n");
  else
    if (word[0] != '\0')
;



  answ = GetTextLineF("L34:");
  next_word(answ, word, " \t;");
  if (toupper(word[0]) == 'Y')
    fprintf(outfile, "Ck34Yes: X\n");
  else
    if (toupper(word[0]) == 'N')
    fprintf(outfile, "Ck34No: X\n");


  writeout_line = 1;
  GetLine("L35", &L[35]);
  GetLine("L36", &L[36]);
  GetLine("L37", &L[37]);
  GetLine("L38", &L[38]);
  GetLine("L39", &L[39]);
  L[40] = (((L[35] + L[36]) + L[37]) + L[38]) + L[39];
  GetLine("L41", &L[41]);
  L[42] = L[40] - L[41];
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
  showline(9);
  showline(10);
  showline(11);
  showline(12);
  showline(13);
  showline(14);
  showline(15);
  fprintf(outfile, "L16a = %6.2f\n", L[16]);
  fprintf(outfile, "L16b = %6.2f\n", L16b);
  showline(17);
  showline(18);
  showline(19);
  fprintf(outfile, "L20a = %6.2f\n", L[20]);
  fprintf(outfile, "L20b = %6.2f\n", L20b);
  showline(21);
  showline(22);
  showline(23);
  fprintf(outfile, "L24a = %6.2f\n", L[24]);
  fprintf(outfile, "L24b = %6.2f\n", L24b);
  showline(25);
  showline(26);
  showline_wlabel("L27a", L[27]);
  L[28] = (((((((((((((((((((((L[8] + L[9]) + L[10]) + L[11]) + L[12]) + L[13]) + L[14]) + L[15]) + L[16]) + L16b) + L[17]) + L[18]) + L[19]) + L[20]) + L20b) + L[21]) + L[22]) + L[23]) + L[24]) + L24b) + L[25]) + L[26]) + L[27];
  showline_wmsg(28, "Total expenses");
  L[29] = L[7] - L[28];
  showline(29);
  showline(30);
  L[31] = L[29] - L[30];
  showline_wmsg(31, "Net Profit (loss)");
  if (L[31] > 0.0)
    fprintf(outfile, "Enter %2.2f on Form 1040 line 12. Sched-SE line 2. Estates/trusts on 1040 line 3.\n", L[31]);
  else
    if (L[31] < 0.0)
  {
    if (L32 == 1)
    {
      fprintf(outfile, "If you checked 32a, enter %2.2f on Form 1040 line 12.\n", L[31]);
      fprintf(outfile, "        Estates and trusts, enter on Form 1041, line 3.\n");
      fprintf(outfile, "Ck32a: x\n");
    }
    else
    {
      fprintf(outfile, "If you checked 32b, you must attach Form 6198. Your loss may be limited.\n");
      fprintf(outfile, "Ck32b: x\n");
    }

  }


  showline(35);
  showline(36);
  showline(37);
  showline(38);
  showline(39);
  showline(40);
  showline(41);
  showline_wmsg(42, "Cost of goods sold");
  writeout_line = 0;
  answ = GetTextLineF("L43:");
  next_word(answ, word, " \t-/.,;");
  fprintf(outfile, "L43mm: %s\n", word);
  next_word(answ, word, " \t-/.,;");
  fprintf(outfile, "L43dd: %s\n", word);
  next_word(answ, word, " \t-/.,;");
  fprintf(outfile, "L43yy: %s\n", word);
  writeout_line = 1;
  GetTextLineF("L44a");
  GetTextLineF("L44b");
  GetTextLineF("L44c");
  writeout_line = 0;
  answ = GetTextLineF("L45:");
  next_word(answ, word, " \t;");
  if (toupper(word[0]) == 'Y')
    fprintf(outfile, "Ck45Yes: X\n");
  else
    if (strcasecmp(word, "No") == 0)
    fprintf(outfile, "Ck45No: X\n");


  answ = GetTextLineF("L46:");
  next_word(answ, word, " \t;");
  if (toupper(word[0]) == 'Y')
    fprintf(outfile, "Ck46Yes: X\n");
  else
    if (strcasecmp(word, "No") == 0)
    fprintf(outfile, "Ck46No: X\n");


  answ = GetTextLineF("L47a:");
  next_word(answ, word, " \t;");
  if (toupper(word[0]) == 'Y')
    fprintf(outfile, "Ck47aYes: X\n");
  else
    if (strcasecmp(word, "No") == 0)
    fprintf(outfile, "Ck47aNo: X\n");


  answ = GetTextLineF("L47b:");
  next_word(answ, word, " \t;");
  if (toupper(word[0]) == 'Y')
    fprintf(outfile, "Ck47bYes: X\n");
  else
    if (strcasecmp(word, "No") == 0)
    fprintf(outfile, "Ck47bNo: X\n");


  fclose(infile);
  fclose(outfile);
;  Display_File(outfname);
  return 0;
}


/* END of taxsolve_US_1040_Sched_C_2017.c */
/* START of taxsolve_CA_540_2017.c */
float _ca_thisversion = 15.03;
int _ca_status = 0;
double _ca_TaxRateFormula(double income, int _ca_status)
{
  double tax;
  if ((_ca_status == 1) || (_ca_status == 3))
  {
    if (income < 8223.00)
      tax = 0.01 * income;
    else
      if (income < 19495.00)
      tax = 82.23 + (0.02 * (income - 8223.00));
    else
      if (income < 30769.00)
      tax = 307.67 + (0.04 * (income - 19495.00));
    else
      if (income < 42711.00)
      tax = 758.63 + (0.06 * (income - 30769.00));
    else
      if (income < 53980.00)
      tax = 1475.15 + (0.08 * (income - 42711.00));
    else
      if (income < 275738.00)
      tax = 2376.67 + (0.093 * (income - 53980.00));
    else
      if (income < 330884.00)
      tax = 23000.16 + (0.103 * (income - 275738.00));
    else
      if (income < 551473.00)
      tax = 28680.20 + (0.113 * (income - 330884.00));
    else
      tax = 53606.76 + (0.123 * (income - 551473.00));








  }
  else
    if ((_ca_status == 2) || (_ca_status == 5))
  {
    if (income < 16446.00)
      tax = 0.01 * income;
    else
      if (income < 38990.00)
      tax = 164.46 + (0.02 * (income - 16446.00));
    else
      if (income < 61538.00)
      tax = 615.34 + (0.04 * (income - 38990.00));
    else
      if (income < 85422.00)
      tax = 1517.26 + (0.06 * (income - 61538.00));
    else
      if (income < 107960.00)
      tax = 2950.30 + (0.08 * (income - 85422.00));
    else
      if (income < 551476.00)
      tax = 4753.34 + (0.093 * (income - 107960.00));
    else
      if (income < 661768.00)
      tax = 46000.33 + (0.103 * (income - 551476.00));
    else
      if (income < 1102946.00)
      tax = 57360.41 + (0.113 * (income - 661768.00));
    else
      tax = 107213.52 + (0.123 * (income - 1102946.00));








  }
  else
  {
    if (income < 16457.00)
      tax = 0.01 * income;
    else
      if (income < 38991.00)
      tax = 164.57 + (0.02 * (income - 16457.00));
    else
      if (income < 50264.00)
      tax = 615.25 + (0.04 * (income - 38991.00));
    else
      if (income < 62206.00)
      tax = 1066.17 + (0.06 * (income - 50264.00));
    else
      if (income < 73477.00)
      tax = 1782.69 + (0.08 * (income - 62206.00));
    else
      if (income < 375002.00)
      tax = 2684.37 + (0.093 * (income - 73477.00));
    else
      if (income < 450003.00)
      tax = 30726.20 + (0.103 * (income - 375002.00));
    else
      if (income < 750003.00)
      tax = 38451.30 + (0.113 * (income - 450003.00));
    else
      tax = 72351.30 + (0.123 * (income - 750003.00));








  }


  return (int) (tax + 0.5);
}

void _ca_Report_bracket_info(double income, int _ca_status)
{
  double tx;
  double rate;
  tx = _ca_TaxRateFormula(income, _ca_status);
  if ((_ca_status == 1) || (_ca_status == 3))
  {
    if (income < 8223.00)
      rate = 0.01;
    else
      if (income < 19495.00)
      rate = 0.02;
    else
      if (income < 30769.00)
      rate = 0.04;
    else
      if (income < 42711.00)
      rate = 0.06;
    else
      if (income < 53980.00)
      rate = 0.08;
    else
      if (income < 275738.00)
      rate = 0.093;
    else
      if (income < 330884.00)
      rate = 0.103;
    else
      if (income < 551473.00)
      rate = 0.113;
    else
      rate = 0.123;








  }
  else
    if ((_ca_status == 2) || (_ca_status == 5))
  {
    if (income < 16446.00)
      rate = 0.01;
    else
      if (income < 38990.00)
      rate = 0.02;
    else
      if (income < 61538.00)
      rate = 0.04;
    else
      if (income < 85422.00)
      rate = 0.06;
    else
      if (income < 107960.00)
      rate = 0.08;
    else
      if (income < 551476.00)
      rate = 0.093;
    else
      if (income < 661768.00)
      rate = 0.103;
    else
      if (income < 1102946.0)
      rate = 0.113;
    else
      rate = 0.123;








  }
  else
  {
    if (income < 16457.00)
      rate = 0.01;
    else
      if (income < 38991.00)
      rate = 0.02;
    else
      if (income < 50264.00)
      rate = 0.04;
    else
      if (income < 62206.00)
      rate = 0.06;
    else
      if (income < 73477.00)
      rate = 0.08;
    else
      if (income < 375002.00)
      rate = 0.093;
    else
      if (income < 450003.00)
      rate = 0.103;
    else
      if (income < 750003.00)
      rate = 0.113;
    else
      rate = 0.123;








  }


;  fprintf(outfile, " You are in the %2.1f%% marginal tax bracket,\n and you are paying an effective %2.1f%% tax on your total income.\n", 100.0 * rate, (100.0 * tx) / income);
}

double _ca_TaxRateFunction(double income, int _ca_status)
{
  double x;
  double tx;
  int k;
  if (income < 100000.0)
  {
    if (income < 99951.0)
    {
      k = (income + 49) / 100;
      x = 100 * ((double) k);
    }
    else
      x = 99975.0;

    tx = (int) _ca_TaxRateFormula(x, _ca_status);
  }
  else
    tx = _ca_TaxRateFormula(income, _ca_status);

  return tx;
}

void _ca_test_tax_function()
{
  double income;
  for (income = 50.0; income < 100000.0; income = income + 100.0)
;
  exit(0);
}

struct _ca_FedReturnData
{
  double fedline[1000];
  double schedA[1000];
  double fedl8b;
  double fedl9b;
  double fedl15a;
  double fedl16a;
  double fedl20a;
  int Exception;
  int Itemized;
} _ca_PrelimFedReturn;
void _ca_convert_slashes(char *fname)
{
  char *ptr;
  char slash_sreach = '\\';
  char slash_replace = '/';
  ptr = strchr(fname, slash_sreach);
  while (ptr)
  {
    ptr[0] = slash_replace;
    ptr = strchr(fname, slash_sreach);
  }

}

void _ca_grab_line_value(char *label, char *fline, double *value)
{
  char twrd[1024];
  next_word(fline, twrd, " \t=");
  if ((twrd[0] != '\0') && (sscanf(twrd, "%lf", value) != 1))
  {
;    fprintf(outfile, "Error: Reading Fed %s '%s%s'\n", label, twrd, fline);
  }

}

int _ca_ImportFederalReturnData(char *fedlogfile, struct _ca_FedReturnData *fed_data)
{
  FILE *infile;
  char fline[2000];
  char word[2000];
  int linenum;
  for (linenum = 0; linenum < 1000; linenum++)
  {
    fed_data->fedline[linenum] = 0.0;
    fed_data->schedA[linenum] = 0.0;
  }

  fed_data->fedl8b = 0.0;
  fed_data->fedl9b = 0.0;
  fed_data->fedl15a = 0.0;
  fed_data->fedl16a = 0.0;
  fed_data->fedl20a = 0.0;
  _ca_convert_slashes(fedlogfile);
  infile = fopen(fedlogfile, "r");
  if (infile == 0)
  {
;    fprintf(outfile, "Error: Could not open Federal return '%s'\n", fedlogfile);
    exit(1);
  }

  fed_data->Itemized = 1;
  read_line(infile, fline);
  linenum = 0;
  while (!feof(infile))
  {
    if (strstr(fline, "Use standard deduction.") != 0)
      fed_data->Itemized = 0;

    next_word(fline, word, " \t=");
    if ((word[0] == 'L') && (strstr(fline, " = ") != 0))
    {
      if (strcmp(word, "L8a") == 0)
        _ca_grab_line_value(word, fline, &fed_data->fedline[8]);
      else
        if (strcmp(word, "L8b") == 0)
        _ca_grab_line_value(word, fline, &fed_data->fedl8b);
      else
        if (strcmp(word, "L9a") == 0)
        _ca_grab_line_value(word, fline, &fed_data->fedline[9]);
      else
        if (strcmp(word, "L9b") == 0)
        _ca_grab_line_value(word, fline, &fed_data->fedl9b);
      else
        if (strcmp(word, "L15a") == 0)
        _ca_grab_line_value(word, fline, &fed_data->fedl15a);
      else
        if (strcmp(word, "L15b") == 0)
        _ca_grab_line_value(word, fline, &fed_data->fedline[15]);
      else
        if (strcmp(word, "L16a") == 0)
        _ca_grab_line_value(word, fline, &fed_data->fedl16a);
      else
        if (strcmp(word, "L16b") == 0)
        _ca_grab_line_value(word, fline, &fed_data->fedline[16]);
      else
        if (strcmp(word, "L20a") == 0)
        _ca_grab_line_value(word, fline, &fed_data->fedl20a);
      else
        if (strcmp(word, "L20b") == 0)
        _ca_grab_line_value(word, fline, &fed_data->fedline[20]);
      else
      {
        if (sscanf(&word[1], "%d", &linenum) != 1)
        {
;          fprintf(outfile, "Error: Reading Fed line number '%s%s'\n", word, fline);
        }

        next_word(fline, word, " \t=");
        if (sscanf(word, "%lf", &fed_data->fedline[linenum]) != 1)
        {
;          fprintf(outfile, "Error: Reading Fed line %d '%s%s'\n", linenum, word, fline);
        }

        if (verbose)
;
      }










    }
    else
      if (((strstr(word, "A") == word) && (strstr(word, "AMT") != word)) && (strstr(fline, " = ") != 0))
    {
      if (sscanf(&word[1], "%d", &linenum) != 1)
      {
;        fprintf(outfile, "Error: Reading Fed line number '%s%s'\n", word, fline);
      }

      next_word(fline, word, " \t=");
      if (sscanf(word, "%lf", &fed_data->schedA[linenum]) != 1)
      {
;        fprintf(outfile, "Error: Reading Fed schedA %d '%s%s'\n", linenum, word, fline);
      }

      if (verbose)
;
    }
    else
      if (strcmp(word, "Status") == 0)
    {
      next_word(fline, word, " \t=");
      if (strncasecmp(word, "Single", 4) == 0)
        _ca_status = 1;
      else
        if (strncasecmp(word, "Married/Joint", 13) == 0)
        _ca_status = 2;
      else
        if (strncasecmp(word, "Married/Sep", 11) == 0)
        _ca_status = 3;
      else
        if (strncasecmp(word, "Head_of_House", 4) == 0)
        _ca_status = 4;
      else
        if (strncasecmp(word, "Widow", 4) == 0)
        _ca_status = 5;
      else
      {
;        fprintf(outfile, "Error: unrecognized status '%s'. Exiting.\n", word);
        return 0;
      }





    }



    read_line(infile, fline);
  }

  fclose(infile);
  return 1;
}

char *_ca_pull_initial(char *name)
{
  int j = 0;
  char midinitial[10];
  while ((name[j] != '\0') && (name[j] != ','))
    j++;

  if (name[j] == ',')
  {
    name[j++] = '\0';
    while ((name[j] != '\0') && isspace(name[j]))
      j++;

    midinitial[0] = name[j];
    midinitial[1] = '\0';
  }
  else
    strcpy(midinitial, "");

  return strdup(midinitial);
}

int _ca_main(int argc, char *argv[])
{
  int argk;
  int j;
  int k;
  int iline7;
  int iline8;
  int iline9;
  int iline10;
  int gotAdj = 0;
  int got_explicit_adjustment = 0;
  double min2file = 0.0;
  double sched540[1000];
  double sched540b[1000];
  double sched540c[1000];
  double threshA = 0;
  double std_ded = 0;
  double tmpval;
  char word[4000];
  char outfname[4000];
  char prelim_1040_outfilename[5000];
  char labelx[4000];
  char *Your1stName = "";
  char *YourLastName = "";
  char YourName[2048] = "";
  char YourNames[2048] = "";
  char *YourMidInitial = "";
  char *SpouseMidInitial = "";
  char *Spouse1stName = "";
  char *SpouseLastName = "";
  char *socsec;
  time_t now;
  argk = 1;
  k = 1;
  while (argk < argc)
  {
    if (strcmp(argv[argk], "-verbose") == 0)
    {
      verbose = 1;
    }
    else
      if (k == 1)
    {
      infile = fopen(argv[argk], "r");
      if (infile == 0)
      {
;        exit(1);
      }

      k = 2;
      strcpy(outfname, argv[argk]);
      j = strlen(outfname) - 1;
      while ((j >= 0) && (outfname[j] != '.'))
        j--;

      if (j < 0)
        strcat(outfname, "_out.txt");
      else
        strcpy(&outfname[j], "_out.txt");

      outfile = fopen(outfname, "w");
      if (outfile == 0)
      {
;        exit(1);
      }

;    }
    else
    {
;      exit(1);
    }


    argk = argk + 1;
  }

  if (infile == 0)
  {
;    exit(1);
  }

  for (j = 0; j < 1000; j++)
  {
    L[j] = 0.0;
    sched540[j] = 0.0;
    sched540b[j] = 0.0;
    sched540c[j] = 0.0;
  }

;  read_line(infile, word);
  now = time(0);
  fprintf(outfile, "\n%s,	 v%2.2f, %s\n", word, _ca_thisversion, ctime(&now));
  get_parameter(infile, 's', word, "FileName");
  get_word(infile, prelim_1040_outfilename);
  _ca_ImportFederalReturnData(prelim_1040_outfilename, &_ca_PrelimFedReturn);
  switch (_ca_status)
  {
    case 1:
      fprintf(outfile, "Status = Single (%d)\nCkSingle: X\nL7a = 1\n", _ca_status);
      break;

    case 2:
      fprintf(outfile, "Status = Married/Joint (%d)\nCkMFJ: X\nL7a = 2\n", _ca_status);
      break;

    case 3:
      fprintf(outfile, "Status = Married/Sep (%d)\nCkMFS: X\nL7a = 1\n", _ca_status);
      break;

    case 4:
      fprintf(outfile, "Status = Head_of_Household (%d)\nCkHH: X\nL7a = 1\n", _ca_status);
      break;

    case 5:
      fprintf(outfile, "Status = Widow(er) (%d)\nCkQW: X\nL7a = 1\n", _ca_status);
      break;

  }

  fprintf(outfile, "\nStep-2 fill-in box %d\n", _ca_status);
  get_parameter(infile, 's', word, "L6");
  get_parameter(infile, 'b', &j, "L6");
  L[6] = j;
  if (L[6] == 0)
    fprintf(outfile, " L6 = no\n");
  else
    fprintf(outfile, " L6 = yes, (check box on line 6).\n  CkDep: X\n");

  if (((_ca_status == 1) || (_ca_status == 3)) || (_ca_status == 4))
    iline7 = 1;
  else
    iline7 = 2;

  if (L[6] != 0.0)
    iline7 = 0;

  L[7] = 114.0 * iline7;
  showline(7);
  get_parameter(infile, 's', word, "L8");
  get_parameter(infile, 'i', &iline8, "L8");
  L[8] = iline8 * 114.0;
  showline(8);
  if (iline8 > 0)
    fprintf(outfile, "  L8a = %d\n", iline8);

  get_parameter(infile, 's', word, "L9");
  get_parameter(infile, 'i', &iline9, "L9");
  L[9] = iline9 * 114.0;
  showline(9);
  if (iline9 > 0)
    fprintf(outfile, "  L9a = %d\n", iline9);

  get_parameter(infile, 's', word, "L10");
  get_parameter(infile, 'i', &iline10, "L10");
  L[10] = iline10 * 353.0;
  showline(10);
  if (iline10 > 0)
    fprintf(outfile, "  L10a = %d\n", iline10);

  L[11] = ((L[7] + L[8]) + L[9]) + L[10];
  showline_wmsg(11, "Exemption amount");
  GetLineF("L12", &L[12]);
  L[13] = _ca_PrelimFedReturn.fedline[37];
  showline(13);
  while (!gotAdj)
  {
    GetOptionalLine("CA540_Subtr_, CA540_Add_, or Adj", labelx, &tmpval);
    if (strcmp(labelx, "Adj") == 0)
    {
      sched540[41] = tmpval;
      gotAdj = 1;
    }
    else
      if (strstr(labelx, "CA540_Subtr_") != 0)
    {
      if (((sscanf(&labelx[12], "%d", &j) == 1) && (j >= 7)) && (j <= 35))
        sched540b[j] = tmpval;
      else
      {
;        fprintf(outfile, "ERROR reading '%s'.\n", labelx);
      }

      got_explicit_adjustment = 1;
    }
    else
      if (strstr(labelx, "CA540_Addit_") != 0)
    {
      if (((sscanf(&labelx[12], "%d", &j) == 1) && (j >= 7)) && (j <= 35))
        sched540c[j] = tmpval;
      else
      {
;        fprintf(outfile, "ERROR reading '%s'.\n", labelx);
      }

      got_explicit_adjustment = 1;
    }
    else
    {
;      fprintf(outfile, "ERROR1: Found '%s' when expecting 'CA540_Subtr_, CA540_Add_, or Adj'\n", labelx);
      exit(1);
    }



  }

  fprintf(outfile, " SchedCA540_8aa = %6.2f\n", _ca_PrelimFedReturn.fedl8b);
  fprintf(outfile, " SchedCA540_9aa = %6.2f\n", _ca_PrelimFedReturn.fedl9b);
  fprintf(outfile, " SchedCA540_15aa = %6.2f\n", _ca_PrelimFedReturn.fedl15a);
  fprintf(outfile, " SchedCA540_16aa = %6.2f\n", _ca_PrelimFedReturn.fedl16a);
  fprintf(outfile, " SchedCA540_20aa = %6.2f\n", _ca_PrelimFedReturn.fedl20a);
  for (j = 7; j <= 21; j++)
  {
    sched540[j] = _ca_PrelimFedReturn.fedline[j];
    sched540[22] = sched540[22] + sched540[j];
    if (sched540[j] != 0.0)
      fprintf(outfile, " SchedCA540_%d = %6.2f\n", j, sched540[j]);

    if (j == 20)
      sched540b[j] = sched540[j];

    sched540b[22] = sched540b[22] + sched540b[j];
    if (sched540b[j] != 0.0)
      fprintf(outfile, " SchedCA540_%db = %6.2f\n", j, sched540b[j]);

    sched540c[22] = sched540c[22] + sched540c[j];
    if (sched540c[j] != 0.0)
      fprintf(outfile, " SchedCA540_%dc = %6.2f\n", j, sched540c[j]);

  }

  fprintf(outfile, " SchedCA540_%d = %6.2f\n", 22, sched540[22]);
  fprintf(outfile, " SchedCA540_%db = %6.2f\n", 22, sched540b[22]);
  fprintf(outfile, " SchedCA540_%dc = %6.2f\n", 22, sched540c[22]);
  for (j = 23; j <= 35; j++)
  {
    sched540[j] = _ca_PrelimFedReturn.fedline[j];
    sched540[36] = sched540[36] + sched540[j];
    if (sched540[j] != 0.0)
      fprintf(outfile, " SchedCA540_%d = %6.2f\n", j, sched540[j]);

    sched540b[36] = sched540b[36] + sched540b[j];
    if (sched540b[j] != 0.0)
      fprintf(outfile, " SchedCA540_%db = %6.2f\n", j, sched540b[j]);

    sched540c[36] = sched540c[36] + sched540c[j];
    if (sched540c[j] != 0.0)
      fprintf(outfile, " SchedCA540_%dc = %6.2f\n", j, sched540c[j]);

  }

  fprintf(outfile, " SchedCA540_%d = %6.2f\n", 36, sched540[36]);
  sched540[37] = sched540[22] - sched540[36];
  fprintf(outfile, " SchedCA540_%d = %6.2f\n", 37, sched540[37]);
  fprintf(outfile, " SchedCA540_%db = %6.2f\n", 36, sched540b[36]);
  sched540b[37] = sched540b[22] - sched540b[36];
  fprintf(outfile, " SchedCA540_%db = %6.2f\n", 37, sched540b[37]);
  fprintf(outfile, " SchedCA540_%dc = %6.2f\n", 36, sched540c[36]);
  sched540c[37] = sched540c[22] - sched540c[36];
  fprintf(outfile, " SchedCA540_%dc = %6.2f\n", 37, sched540c[37]);
  for (j = 7; j <= 37; j++)
    if (sched540b[j] != 0.0)
    fprintf(outfile, " SchedCA540_%db = %6.2f\n", j, sched540b[j]);


  sched540[38] = (((((_ca_PrelimFedReturn.schedA[4] + _ca_PrelimFedReturn.schedA[9]) + _ca_PrelimFedReturn.schedA[15]) + _ca_PrelimFedReturn.schedA[19]) + _ca_PrelimFedReturn.schedA[20]) + _ca_PrelimFedReturn.schedA[27]) + _ca_PrelimFedReturn.schedA[28];
  sched540[39] = _ca_PrelimFedReturn.schedA[5] + _ca_PrelimFedReturn.schedA[8];
  sched540[40] = sched540[38] - sched540[39];
  sched540[42] = sched540[40] + sched540[41];
  switch (_ca_status)
  {
    case 1:

    case 3:
      threshA = 187203.0;
      std_ded = 4236.0;
      break;

    case 2:

    case 5:
      threshA = 374411.0;
      std_ded = 8472.0;
      break;

    case 4:
      threshA = 280808.0;
      std_ded = 8472.0;
      break;

  }

  if (L[13] > threshA)
  {
    double ws[40];
    for (j = 1; j <= 10; j++)
      ws[j] = 0.0;

    ws[1] = sched540[42];
    ws[2] = ((_ca_PrelimFedReturn.schedA[4] + _ca_PrelimFedReturn.schedA[14]) + _ca_PrelimFedReturn.schedA[20]) + _ca_PrelimFedReturn.schedA[28];
    ws[3] = ws[1] - ws[2];
    if (ws[3] == 0.0)
      sched540[43] = ws[1];
    else
    {
      ws[4] = 0.8 * ws[3];
      ws[5] = L[13];
      ws[6] = threshA;
      ws[7] = ws[5] - ws[6];
      if (ws[7] == 0.0)
        sched540[43] = ws[1];
      else
      {
        ws[8] = 0.06 * ws[7];
        ws[9] = smallerof(ws[4], ws[8]);
        ws[10] = ws[1] - ws[9];
        sched540[43] = ws[10];
      }

    }

    for (j = 1; j <= 10; j++)
      if (ws[j] != 0.0)
      fprintf(outfile, "  ItemizedDedWS%d = %6.2f\n", j, ws[j]);


  }
  else
  {
    sched540[43] = sched540[42];
  }

  sched540[44] = largerof(sched540[43], std_ded);
  for (j = 38; j <= 44; j++)
    fprintf(outfile, " SchedCA540_%d = %6.2f\n", j, sched540[j]);

  L[18] = sched540[44];
  if (got_explicit_adjustment)
    L[14] = sched540b[37];
  else
    GetLine("L14", &L[14]);

  showline(14);
  L[15] = L[13] - L[14];
  if (L[15] < 0.0)
    fprintf(outfile, "L15 = (%f6.2)\n", -L[15]);
  else
    showline(15);

  if (got_explicit_adjustment)
    L[16] = sched540c[37];
  else
    GetLine("L16", &L[16]);

  showline(16);
  L[17] = L[15] + L[16];
  showline(17);
  switch (_ca_status)
  {
    case 1:

    case 4:
      if (iline9 == 0)
      switch (iline10)
    {
      case 0:
        min2file = 13623.0;
        break;

      case 1:
        min2file = 25390.0;
        break;

      default:
        min2file = 34215.0;
        break;

    }

    else
      switch (iline10)
    {
      case 0:
        min2file = 19323.0;
        break;

      case 1:
        min2file = 28148.0;
        break;

      default:
        min2file = 35208.0;
        break;

    }


      break;

    case 2:
      if (iline9 == 0)
      switch (iline10)
    {
      case 0:
        min2file = 27249.0;
        break;

      case 1:
        min2file = 39016.0;
        break;

      default:
        min2file = 47841.0;
        break;

    }

    else
      if (iline9 == 1)
      switch (iline10)
    {
      case 0:
        min2file = 32949.0;
        break;

      case 1:
        min2file = 41774.0;
        break;

      default:
        min2file = 48534.0;
        break;

    }

    else
      switch (iline10)
    {
      case 0:
        min2file = 38649.0;
        break;

      case 1:
        min2file = 47474.0;
        break;

      default:
        min2file = 54534.0;
        break;

    }



      break;

    case 5:
      if (iline9 == 0)
      switch (iline10)
    {
      case 0:
        min2file = 0.0;
        break;

      case 1:
        min2file = 25390.0;
        break;

      default:
        min2file = 34215.0;
        break;

    }

    else
      switch (iline10)
    {
      case 0:
        min2file = 0.0;
        break;

      case 1:
        min2file = 28148.0;
        break;

      default:
        min2file = 35208.0;
        break;

    }


      break;

  }

  if (L[17] <= min2file)
    fprintf(outfile, "You may not need to file CA Taxes, due to your California Adjusted Gross Income (%6.2f <= %6.2f).\n", L[17], min2file);

  showline(18);
  L[19] = NotLessThanZero(L[17] - L[18]);
  showline_wmsg(19, "Taxable Income");
  if (L[19] < 100000.00)
    fprintf(outfile, "Fill in circle from: Tax Table.\nCkTxTable: X\n");
  else
    fprintf(outfile, "Fill in circle from: Tax Rate Schedule.\nCkTxRateSchd: X\n");

  L[31] = _ca_TaxRateFunction(L[19], _ca_status);
  showline(31);
  _ca_Report_bracket_info(L[19], _ca_status);
  if (L[13] > threshA)
  {
    double ws_a;
    double ws_b;
    double ws_c;
    double ws_d;
    double ws_e;
    double ws_f;
    double ws_g;
    double ws_h;
    double ws_i;
    double ws_j;
    double ws_k;
    double ws_l;
    double ws_m;
    double ws_n;
;    ws_a = L[13];
    ws_b = threshA;
    ws_c = ws_a - ws_b;
    if (_ca_status != 3)
      ws_d = Round(ws_c / 2500.0);
    else
      ws_d = Round(ws_c / 1250.0);

    ws_e = 6.0 * ws_d;
    ws_f = (iline7 + iline8) + iline9;
    ws_g = ws_e * ws_f;
    ws_h = (L[7] + L[8]) + L[9];
    ws_i = NotLessThanZero(ws_h - ws_g);
    ws_j = iline10;
    ws_k = ws_e * ws_j;
    ws_l = L[10];
    ws_m = NotLessThanZero(ws_l - ws_k);
    ws_n = ws_i + ws_m;
    fprintf(outfile, " AGI Worksheet:\n   a: %6.2f\n", ws_a);
    fprintf(outfile, "  b: %6.2f\n", ws_b);
    fprintf(outfile, "  c: %6.2f\n", ws_c);
    fprintf(outfile, "  d: %6.2f\n", ws_d);
    fprintf(outfile, "  e: %6.2f\n", ws_e);
    fprintf(outfile, "  f: %6.2f\n", ws_f);
    fprintf(outfile, "  g: %6.2f\n", ws_g);
    fprintf(outfile, "  h: %6.2f\n", ws_h);
    fprintf(outfile, "  i: %6.2f\n", ws_i);
    fprintf(outfile, "  j: %6.2f\n", ws_j);
    fprintf(outfile, "  k: %6.2f\n", ws_k);
    fprintf(outfile, "  l: %6.2f\n", ws_l);
    fprintf(outfile, "  m: %6.2f\n", ws_m);
    fprintf(outfile, "  n: %6.2f\n", ws_n);
    fprintf(outfile, " Your exemptions may be limited. Used Exemptions-Credits-Worksheet for Line 21.\n");
    fprintf(outfile, "   WorkSheet[n]=%6.2f (vs. L11=%6.2f)\n", ws_n, L[11]);
    L[32] = ws_n;
  }
  else
    L[32] = L[11];

  showline(32);
  L[33] = NotLessThanZero(L[31] - L[32]);
  showline(33);
  GetLineF("L34", &L[34]);
  showline(34);
  L[35] = L[33] + L[34];
  showline(35);
  GetLineF("L40", &L[40]);
  fprintf(outfile, " ");
  GetLineF("L41", &L[41]);
  GetLineF("L42", &L[42]);
  GetLineF("L43", &L[43]);
  GetLineF("L44", &L[44]);
  GetLineF("L45", &L[45]);
  GetLineF("L46", &L[46]);
  L[47] = ((((L[40] + L[42]) + L[43]) + L[44]) + L[45]) + L[46];
  showline(47);
  L[48] = NotLessThanZero(L[35] - L[47]);
  showline(48);
  GetLineF("L61", &L[61]);
  GetLineF("L62", &L[62]);
  GetLineF("L63", &L[63]);
  L[64] = ((L[48] + L[61]) + L[62]) + L[63];
  showline_wmsg(64, "Total Tax");
  GetLineF("L71", &L[71]);
  GetLineF("L72", &L[72]);
  GetLineF("L73", &L[73]);
  GetLineF("L74", &L[74]);
  GetLineF("L75", &L[75]);
  L[76] = (((L[71] + L[72]) + L[73]) + L[74]) + L[75];
  showline_wmsg(76, "Total Payments");
  GetLineF("L91", &L[91]);
  if (L[76] > L[91])
  {
    L[92] = L[76] - L[91];
    showline(92);
  }
  else
    if (L[91] > L[76])
  {
    L[93] = L[91] - L[76];
    showline(93);
  }


  if (L[92] > L[64])
  {
    L[94] = L[92] - L[64];
    fprintf(outfile, "L94 = %6.2f  REBATE!!!\n", L[94]);
    showline(95);
    L[96] = L[94] - L[95];
    showline(96);
    L[115] = L[96] - ((L[110] + L[112]) + L[113]);
  }
  else
  {
    L[97] = L[64] - L[92];
    fprintf(outfile, "L97 = %6.2f  DUE !!!\n", L[97]);
    fprintf(outfile, "         (Which is %2.1f%% of your total tax.)\n", (100.0 * L[97]) / (L[64] + 1e-9));
    L[111] = (L[93] + L[97]) + L[110];
    showline(111);
  }

  fprintf(outfile, "\nSelect any charity contributions and complete\n form accordingly.\n");
  fprintf(outfile, "\n{ --------- }\n");
  writeout_line = 0;
  Your1stName = GetTextLineF("Your1stName:");
  YourMidInitial = _ca_pull_initial(Your1stName);
  Your1stName[11] = '\0';
  fprintf(outfile, "Your1stName: %s\n", Your1stName);
  fprintf(outfile, "YourMidInit: %s\n", YourMidInitial);
  YourLastName = GetTextLineF("YourLastName:");
  YourLastName[15] = '\0';
  fprintf(outfile, "YourLastName: %s\n", YourLastName);
  socsec = GetTextLineF("YourSocSec#:");
  format_socsec(socsec, 1);
  fprintf(outfile, "YourSocSec#: %s\n", socsec);
  free(socsec);
  Spouse1stName = GetTextLineF("Spouse1stName:");
  SpouseMidInitial = _ca_pull_initial(Spouse1stName);
  Spouse1stName[11] = '\0';
  fprintf(outfile, "Spouse1stName: %s\n", Spouse1stName);
  fprintf(outfile, "SpouseMidInit: %s\n", SpouseMidInitial);
  SpouseLastName = GetTextLineF("SpouseLastName:");
  SpouseLastName[15] = '\0';
  fprintf(outfile, "SpouseLastName: %s\n", SpouseLastName);
  socsec = GetTextLineF("SpouseSocSec#:");
  format_socsec(socsec, 1);
  fprintf(outfile, "SpouseSocSec#: %s\n", socsec);
  free(socsec);
  writeout_line = 1;
  if (strlen(YourLastName) > 0)
  {
    strcpy(YourName, Your1stName);
    strcat(YourName, " ");
    strcat(YourName, YourLastName);
    YourName[15] = '\0';
    fprintf(outfile, "YourName: %s\n", YourName);
    if (strcmp(YourLastName, SpouseLastName) == 0)
      sprintf(YourNames, "%s & %s, %s", Your1stName, Spouse1stName, YourLastName);
    else
      if (strlen(SpouseLastName) > 0)
      sprintf(YourNames, "%s %s & %s %s", Your1stName, YourLastName, Spouse1stName, SpouseLastName);
    else
      sprintf(YourNames, "%s %s", Your1stName, YourLastName);


    YourNames[33] = '\0';
    fprintf(outfile, "YourNames: %s\n", YourNames);
  }

  GetTextLineF("Number&Street:");
  GetTextLineF("Apt#:");
  GetTextLineF("Town:");
  fprintf(outfile, "State: CA\n");
  GetTextLineF("Zipcode:");
  fclose(infile);
  fclose(outfile);
  Display_File(outfname);
  return 0;
}


/* END of taxsolve_CA_540_2017.c */
/* START of taxsolve_MA_1_2017.c */
float _ma_thisversion = 15.00;
double _ma_Sum(double *v, int start_slot, int end_slot)
{
  int j;
  double result = 0.0;
  for (j = start_slot; j <= end_slot; j++)
    result += v[j];

  return result;
}

double _ma_ComputeTax(double taxableIncome)
{
  double taxrate = 0.051;
  if (taxableIncome < 24000.0)
    return (int) ((taxrate * (taxableIncome + 25.0)) + 0.5);
  else
    return taxableIncome * taxrate;

}

int _ma_main(int argc, char *argv[])
{
  int i;
  int j;
  int k;
  int status = 0;
  int i65;
  int iblind;
  int ndep;
  int dep_deduct;
  int flag;
  int notaxstatus = 0;
  char word[2000];
  char outfname[2000];
  char *answ;
  time_t now;
  double Exemptions[10];
  double MassBankInterest;
  double Iexempt;
  double AGI;
  double Unemployment;
  double Lottery;
  double MassRetirement[2];
  double L23a = 0.0;
  double L33[6];
  double L35a = 0.0;
  double L35b = 0.0;
  double L35c = 0.0;
  double L42a = 0.0;
;  i = 1;
  k = 1;
  while (i < argc)
  {
    if (strcmp(argv[i], "-verbose") == 0)
    {
      verbose = 1;
    }
    else
      if (k == 1)
    {
      infile = fopen(argv[i], "r");
      if (infile == 0)
      {
;        exit(1);
      }

      k = 2;
      strcpy(outfname, argv[i]);
      j = strlen(outfname) - 1;
      while ((j >= 0) && (outfname[j] != '.'))
        j--;

      if (j < 0)
        strcat(outfname, "_out.txt");
      else
        strcpy(&outfname[j], "_out.txt");

      outfile = fopen(outfname, "w");
      if (outfile == 0)
      {
;        exit(1);
      }

;    }
    else
    {
;      exit(1);
    }


    i = i + 1;
  }

  if (infile == 0)
  {
;    exit(1);
  }

  for (i = 0; i < 1000; i++)
  {
    L[i] = 0.0;
  }

  read_line(infile, word);
  now = time(0);
  fprintf(outfile, "\n%s,	 v%2.2f, %s\n", word, _ma_thisversion, ctime(&now));
  get_parameter(infile, 's', word, "Status");
  get_parameter(infile, 'l', word, "Status?");
  if (strncasecmp(word, "Single", 4) == 0)
    status = 1;
  else
    if (strncasecmp(word, "Married/Joint", 13) == 0)
    status = 2;
  else
    if (strncasecmp(word, "Married/Sep", 11) == 0)
    status = 3;
  else
    if (strncasecmp(word, "Head_of_House", 4) == 0)
    status = 4;
  else
    if (strncasecmp(word, "Widow", 4) == 0)
    status = 5;
  else
  {
;    fprintf(outfile, "Error: unrecognized status '%s'. Exiting.\n", word);
    exit(1);
  }





  fprintf(outfile, "Status = %s (%d)\n", word, status);
  for (i = 0; i < 10; i++)
  {
    Exemptions[i] = 0.0;
  }

  switch (status)
  {
    case 1:
      Exemptions[0] = 4400.0;
      fprintf(outfile, " Check_single x\n");
      break;

    case 3:
      Exemptions[0] = 4400.0;
      fprintf(outfile, " Check_sep x\n");
      break;

    case 4:
      Exemptions[0] = 6800.0;
      fprintf(outfile, " Check_hh x\n");
      break;

    case 2:
      Exemptions[0] = 8800.0;
      fprintf(outfile, " Check_mfj x\n");
      break;

  }

  GetLineF("L1a", &L[0]);
  GetLineF("L1b", &L[0]);
  fprintf(outfile, "L2. Exemptions: \n");
  fprintf(outfile, "  2a.  = %6.2f   Personal exemptions\n", Exemptions[0]);
  get_parameter(infile, 's', word, "Dependents");
  get_parameter(infile, 'i', &ndep, "Dependents");
  Exemptions[1] = ((double) ndep) * 1000.0;
  if (Exemptions[1] > 0.0)
  {
    fprintf(outfile, "  2bnum  = %d  Number of dependents\n", ndep);
    fprintf(outfile, "  2b.  = %6.2f  %d x 1,000\n", Exemptions[1], ndep);
  }

  i65 = 0;
  iblind = 0;
  get_parameter(infile, 's', word, "Age65You");
  get_parameter(infile, 'b', &flag, "Your age over 65?");
  if (flag)
  {
    i65++;
    fprintf(outfile, "Check_2cyou X\n");
  }

  get_parameter(infile, 's', word, "Age65Spouse");
  get_parameter(infile, 'b', &flag, "Spouse age over 65?");
  if (flag)
  {
    i65++;
    fprintf(outfile, "Check_2csp X\n");
  }

  Exemptions[2] = ((double) i65) * 700.0;
  if (Exemptions[2] > 0)
  {
    fprintf(outfile, "  2cnum = %d    Age 65 or over\n", i65);
    fprintf(outfile, "  2c. = %6.2f    %d x 700\n", Exemptions[2], i65);
  }

  get_parameter(infile, 's', word, "BlindYou");
  get_parameter(infile, 'b', &flag, "Your Blindness?");
  if (flag)
  {
    iblind++;
    fprintf(outfile, "Check_2dyou X\n");
  }

  get_parameter(infile, 's', word, "BlindSpouse");
  get_parameter(infile, 'b', &flag, "Spouse Blindness?");
  if (flag)
    if (flag)
  {
    iblind++;
    fprintf(outfile, "Check_2dsp X\n");
  }


  Exemptions[3] = ((double) iblind) * 2200.0;
  if (Exemptions[3] > 0)
  {
    fprintf(outfile, "  2dnum = %d    Blindness\n", iblind);
    fprintf(outfile, "  2d. = %6.2f     %d x 2,200\n", Exemptions[3], iblind);
  }

  GetLine("Med/Dental", &Exemptions[4]);
  fprintf(outfile, "  2e. = %6.2f\n", Exemptions[4]);
  GetLine("Adoption", &Exemptions[5]);
  fprintf(outfile, "  2f. = %6.2f\n", Exemptions[5]);
  L[2] = _ma_Sum(Exemptions, 0, 5);
  fprintf(outfile, "  2g. = %6.2f Total Exemptions\n", L[2]);
  GetLine("L3", &L[3]);
  showline(3);
  GetLine("L4", &L[4]);
  ShowLineNonZero(4);
  GetLineF("L5a", &MassBankInterest);
  if (status == 2)
    Iexempt = 200;
  else
    Iexempt = 100;

  fprintf(outfile, "L5b = %6.2f\n", Iexempt);
  L[5] = MassBankInterest - Iexempt;
  if (L[5] < 0.0)
    L[5] = 0.0;

  if (L[5] > 0.0)
  {
    sprintf(word, "Mass. Bank Interest: a. %6.2f - b. exemption %6.2f", MassBankInterest, Iexempt);
    showline_wmsg(5, word);
  }

  GetLine("L6", &L[6]);
  ShowLineNonZero(6);
  GetLine("L7", &L[7]);
  ShowLineNonZero(7);
  GetLineF("L8a", &Unemployment);
  GetLineF("L8b", &Lottery);
  L[8] = Unemployment + Lottery;
  if (L[8] > 0)
  {
    sprintf(word, "a. %6.2f + b. %6.2f", Unemployment, Lottery);
    showline_wmsg(8, word);
  }

  GetLine("L9", &L[9]);
  ShowLineNonZero(9);
  L[10] = _ma_Sum(L, 3, 9);
  showline_wmsg(10, "TOTAL 5.1% INCOME");
  GetLine("L11a", &MassRetirement[0]);
  if (MassRetirement[0] > 2000)
    MassRetirement[0] = 2000;

  showline_wlabel("L11a", MassRetirement[0]);
  GetLine("L11b", &MassRetirement[1]);
  if (MassRetirement[1] > 2000)
    MassRetirement[1] = 2000;

  showline_wlabel("L11b", MassRetirement[1]);
  L[11] = _ma_Sum(MassRetirement, 0, 1);
  if (L[11] > 0)
  {
    sprintf(word, "you %6.2f + spouse %6.2f", MassRetirement[0], MassRetirement[1]);
    showline_wmsg(11, word);
  }

  GetLine("L12", &L[12]);
  ShowLineNonZero(12);
  get_parameter(infile, 's', word, "L13");
  get_parameter(infile, 'i', &dep_deduct, "L13");
  if (dep_deduct > 2)
    dep_deduct = 2;

  if (((L[12] == 0) && ((status == 2) || (status == 4))) && (dep_deduct > 0))
  {
    L[13] = dep_deduct * 3600.0;
    sprintf(word, "a. %d x 3,600 ", dep_deduct);
    showline_wmsg(13, word);
  }

  GetLine("L14a", &L[14]);
  showline_wlabel("14a", L[14]);
  L[14] = L[14] / 2.0;
  if (status == 3)
    L[14] = smallerof(L[14], 1500.0);
  else
    L[14] = smallerof(L[14], 3000.0);

  ShowLineNonZero(14);
  GetLine("L15", &L[15]);
  ShowLineNonZero(15);
  L[16] = _ma_Sum(L, 11, 15);
  showline_wmsg(16, "Total Deductions");
  L[17] = NotLessThanZero(L[10] - L[16]);
  showline(17);
  L[18] = L[2];
  showline(18);
  L[19] = NotLessThanZero(L[17] - L[18]);
  showline(19);
  GetLine("L20", &L[20]);
  L[20] = NotLessThanZero(L[20]);
  showline(20);
  L[21] = L[19] + L[20];
  showline_wmsg(21, "Total 5.1% Taxable Income");
  L[22] = _ma_ComputeTax(L[21]);
  showline_wmsg(22, "5.1% Tax");
  GetLine("L23a", &L23a);
  L[23] = NotLessThanZero(L23a * 0.12);
  if (L23a > 0.0)
  {
    sprintf(word, "12%% Income tax: a. %6.2f x 0.12", L23a);
    showline_wmsg(23, word);
  }

  GetLine("L24", &L[24]);
  ShowLineNonZero(24);
  GetLine("L25", &L[25]);
  ShowLineNonZero(25);
  GetLine("L26", &L[26]);
  ShowLineNonZero(26);
  L[28] = _ma_Sum(L, 22, 26);
  if (((status == 1) || (status == 4)) || (status == 2))
  {
    double ws[20];
    double threshA;
    double threshB;
    for (j = 0; j < 20; j++)
      ws[j] = 0.0;

    ws[1] = NotLessThanZero(L[10]);
    ws[2] = 0.0;
    ws[3] = NotLessThanZero(ws[1] - ws[2]);
    ws[4] = smallerof(MassBankInterest, Iexempt);
    if (L[10] < 0.0)
      ws[4] = NotLessThanZero(ws[4] + L[10]);

    ws[5] = L[20];
    ws[6] = 0.0;
    ws[7] = ((ws[3] + ws[4]) + ws[5]) + ws[6];
    AGI = ws[7];
    for (j = 1; j <= 7; j++)
      fprintf(outfile, "     AGI_Worksheet[%d] = %6.2f\n", j, ws[j]);

    fprintf(outfile, "   AGI = %6.2f\n", AGI);
    if (status != 3)
    {
      switch (status)
      {
        case 1:
          threshA = 8000.0;
          threshB = 14000.0;
          break;

        case 4:
          threshA = 14400.0 + (1000.0 * ndep);
          threshB = 25200.0 + (1750.0 * ndep);
          break;

        case 2:
          threshA = 16400.0 + (1000.0 * ndep);
          threshB = 28700.0 + (1750.0 * ndep);
          break;

        default:
          fprintf(outfile, "Bad filing status.\n");
;          exit(1);
          break;

      }

      if (AGI <= threshA)
      {
        notaxstatus = 1;
        fprintf(outfile, "    (%6.2f <= %6.2f)\n", AGI, threshA);
        fprintf(outfile, "You qualify for No Tax Status.\n");
      }
      else
        if (AGI <= threshB)
        fprintf(outfile, "See Form 1 Line 29 special instructions for Limited Income.\n");


    }

  }

  if (notaxstatus)
    L[28] = 0.0;

  showline_wmsg(28, "Total Tax");
  GetLine1("L29", &L[29]);
  GetLine1("L30", &L[30]);
  GetLine1("L31", &L[31]);
  if (notaxstatus)
  {
    L[29] = 0.0;
    L[3] = 0.0;
  }

  ShowLineNonZero(29);
  ShowLineNonZero(30);
  ShowLineNonZero(31);
  L[32] = NotLessThanZero(L[28] - ((L[29] + L[30]) + L[31]));
  showline_wmsg(32, "Income Tax After Credits");
  GetLine1("L33a", &L33[0]);
  if (L33[0] != 0)
    showline_wlabel("L33a", L33[0]);

  GetLine1("L33b", &L33[1]);
  if (L33[1] != 0)
    showline_wlabel("L33b", L33[1]);

  GetLine1("L33c", &L33[2]);
  if (L33[2] != 0)
    showline_wlabel("L33c", L33[2]);

  GetLine1("L33d", &L33[3]);
  if (L33[3] != 0)
    showline_wlabel("L33d", L33[3]);

  GetLine1("L33e", &L33[4]);
  if (L33[4] != 0)
    showline_wlabel("L33e", L33[4]);

  GetLine1("L33f", &L33[5]);
  if (L33[5] != 0)
    showline_wlabel("L33f", L33[5]);

  L[33] = _ma_Sum(L33, 0, 5);
  ShowLineNonZero(33);
  GetLine1("L34", &L[34]);
  showline(34);
  GetLine1("L35a", &L35a);
  showline_wlabel("L35a", L35a);
  GetLine1("L35b", &L35b);
  showline_wlabel("L35b", L35b);
  GetLine1("L35c", &L35c);
  showline_wlabel("L35c", L35c);
  L[35] = (L35a + L35b) + L35c;
  if (L[35] != 0)
    showline_wmsg(35, "Health Care penalty");

  L[36] = _ma_Sum(L, 32, 35);
  showline_wmsg(36, "Income Tax After Credits Contributions, Use Tax + HC Penalty");
  GetLine("L37", &L[37]);
  ShowLineNonZero(37);
  GetLine("L38", &L[38]);
  ShowLineNonZero(38);
  GetLine("L39", &L[39]);
  ShowLineNonZero(39);
  GetLine("L40", &L[40]);
  ShowLineNonZero(40);
  GetLine("L41", &L[41]);
  ShowLineNonZero(41);
  GetLine("L42a", &L42a);
  if (L42a != 0.0)
    fprintf(outfile, " L42a = %6.2f  x 0.23 = .....  ", L42a);

  L[42] = L42a * 0.23;
  ShowLineNonZero(42);
  GetLine("L43", &L[43]);
  ShowLineNonZero(43);
  GetLine("L44", &L[44]);
  ShowLineNonZero(43);
  L[45] = _ma_Sum(L, 37, 44);
  showline_wmsg(44, "total payments");
  GetLine("L47", &L[47]);
  if (L[36] < L[45])
  {
    L[46] = L[45] - L[36];
    fprintf(outfile, "L46 = %6.2f  Overpayment!\n", L[46]);
    if (L[47] > L[46])
      L[47] = L[46];

    showline_wmsg(47, "Overpayment to be applied to next year's estimated tax");
    L[48] = L[46] - L[47];
    fprintf(outfile, "L48 = %6.2f  THIS IS YOUR REFUND\n", L[48]);
  }
  else
  {
    L[49] = L[36] - L[45];
    fprintf(outfile, "L49 = %6.2f  TAX DUE !!!\n", L[49]);
    fprintf(outfile, "         (Which is %2.1f%% of your total tax.)\n", (100.0 * L[49]) / (L[36] + 1e-9));
    if ((L[49] > 400.0) && (L[45] < (0.80 * L[36])))
      fprintf(outfile, " You may owe Underpayment of Estimated Tax penalty.\n");

  }

  fprintf(outfile, "\n{ --------- }\n");
  GetTextLineF("Your1stName:");
  GetTextLineF("YourInitial:");
  GetTextLineF("YourLastName:");
  answ = GetTextLine("YourSocSec#:");
  format_socsec(answ, 1);
  fprintf(outfile, "YourSocSec#: %s\n", answ);
  GetTextLineF("Spouse1stName:");
  GetTextLineF("SpouseInitial:");
  GetTextLineF("SpouseLastName:");
  answ = GetTextLine("SpouseSocSec#:");
  format_socsec(answ, 1);
  fprintf(outfile, "SpouseSocSec#: %s\n", answ);
  GetTextLineF("Number&Street:");
  GetTextLineF("Town:");
  GetTextLineF("State:");
  GetTextLineF("Zipcode:");
  fclose(infile);
  fclose(outfile);
;  Display_File(outfname);
;  return 0;
}


/* END of taxsolve_MA_1_2017.c */
/* START of taxsolve_NC_D400_2017.c */
float _nc_thisversion = 15.00;
double _nc_flat_tax_rate = 0.05499;
struct _nc_FedReturnData
{
  double fedline[1000];
  int Itemized;
  int Limited;
  int Limited_L6;
  double Sched_A[1000];
};
void _nc_convert_slashes(char *fname)
{
  char *ptr;
  char slash_sreach = '\\';
  char slash_replace = '/';
  ptr = strchr(fname, slash_sreach);
  while (ptr)
  {
    ptr[0] = slash_replace;
    ptr = strchr(fname, slash_sreach);
  }

}

void _nc_ImportFederalReturnData(char *fedlogfile, struct _nc_FedReturnData *fed_data)
{
  FILE *infile;
  char fline[1000];
  char word[1000];
  int linenum;
  _nc_convert_slashes(fedlogfile);
  infile = fopen(fedlogfile, "r");
  if (infile == 0)
  {
;    fprintf(outfile, "\nError: Could not open federal return '%s'\n", fedlogfile);
    system("pwd");
    system("ls -l");
    exit(1);
  }

;  fed_data->Itemized = 1;
  fed_data->Limited = 1;
  fed_data->Limited_L6 = 1;
  for (linenum = 0; linenum < 1000; linenum++)
    fed_data->fedline[linenum] = 0.0;

  for (linenum = 0; linenum < 1000; linenum++)
    fed_data->Sched_A[linenum] = 0.0;

  read_line(infile, fline);
  linenum = 0;
  while (!feof(infile))
  {
    if (verbose)
;
    if (strstr(fline, "Use standard deduction.") != 0)
      fed_data->Itemized = 0;

    if (strstr(fline, "Deductions not limited") != 0)
    {
      fed_data->Limited = 0;
      if (strstr(fline, "line 6"))
        fed_data->Limited_L6 = 0;

    }

    next_word(fline, word, " \t=");
    if (((strstr(word, "A") == word) && isdigit(word[1])) && (strstr(fline, " = ") != 0))
    {
      if (sscanf(&word[1], "%d", &linenum) != 1)
;
      next_word(fline, word, " 	=");
      if (sscanf(word, "%lf", &fed_data->Sched_A[linenum]) != 1)
;
      if (verbose)
;
    }

    if ((strstr(word, "L") == word) && (strstr(fline, " = ") != 0))
    {
      if (sscanf(&word[1], "%d", &linenum) != 1)
;
      next_word(fline, word, " 	=");
      if (sscanf(word, "%lf", &fed_data->fedline[linenum]) != 1)
;
      if (verbose)
;
    }

    read_line(infile, fline);
  }

  fclose(infile);
}

int _nc_main(int argc, char *argv[])
{
  int j;
  int jj;
  int k;
  int status;
  char word[1000];
  char outfname[1000];
  char *socsec;
  char socsectmp[100];
  time_t now;
  struct _nc_FedReturnData fed_data;
  double stdded;
  double min_payment = 0.0;
  double min2file;
  double L21a = 0.0;
  double L21b = 0.0;
  double L21c = 0.0;
  double L21d = 0.0;
;  jj = 1;
  k = 1;
  while (jj < argc)
  {
    if (strcmp(argv[jj], "-verbose") == 0)
    {
      verbose = 1;
    }
    else
      if (k == 1)
    {
      infile = fopen(argv[jj], "r");
      if (infile == 0)
      {
;        fprintf(outfile, "ERROR: Parameter file '%s' could not be opened.\n", argv[jj]);
        exit(1);
      }

      k = 2;
      strcpy(outfname, argv[jj]);
      j = strlen(outfname) - 1;
      while ((j >= 0) && (outfname[j] != '.'))
        j--;

      if (j < 0)
        strcat(outfname, "_out.txt");
      else
        strcpy(&outfname[j], "_out.txt");

      outfile = fopen(outfname, "w");
      if (outfile == 0)
      {
;        fprintf(outfile, "ERROR: Output file '%s' could not be opened.\n", outfname);
        exit(1);
      }

;    }
    else
    {
;      fprintf(outfile, "Unknown command-line parameter '%s'\n", argv[jj]);
      exit(1);
    }


    jj++;
  }

  if (infile == 0)
  {
;    fprintf(outfile, "Error: No input file on command line.\n");
    exit(1);
  }

  for (j = 0; j < 1000; j++)
  {
    L[j] = 0.0;
  }

  read_line(infile, word);
  now = time(0);
  fprintf(outfile, "\n%s,	 v%2.2f, %s\n", word, _nc_thisversion, ctime(&now));
  get_parameter(infile, 's', word, "FedReturn");
  if (verbose)
;
  get_word(infile, word);
  _nc_ImportFederalReturnData(word, &fed_data);
  get_parameter(infile, 's', word, "Status");
  get_parameter(infile, 'l', word, "Status ?");
  if ((word[0] > '0') && (word[0] < '6'))
    status = word[0] - 48;
  else
    if (strncasecmp(word, "Single", 4) == 0)
    status = 1;
  else
    if (strncasecmp(word, "Married/Joint", 13) == 0)
    status = 2;
  else
    if (strncasecmp(word, "Married/Sep", 11) == 0)
    status = 3;
  else
    if (strncasecmp(word, "Head_of_House", 4) == 0)
    status = 4;
  else
    if (strncasecmp(word, "Widow", 4) == 0)
    status = 5;
  else
  {
;    fprintf(outfile, "Error: unrecognized status '%s'. Must be: Single, Married/joint, Married/sep, Head_of_house, Widow(er)\nExiting.\n", word);
    exit(1);
  }






  fprintf(outfile, "Status = %s (%d)\n", word, status);
  GetLine("L7", &L[7]);
  GetLine("L9", &L[9]);
  GetLine("L11", &L[11]);
  GetLine("L13", &L[13]);
  GetLine("L16", &L[16]);
  GetLine("L18", &L[18]);
  GetLine("L20", &L[20]);
  GetLine("L21a", &L21a);
  GetLine("L21b", &L21b);
  GetLine("L21c", &L21c);
  GetLine("L21d", &L21d);
  L[6] = fed_data.fedline[37];
  switch (status)
  {
    case 1:
      stdded = 8750.0;
      min2file = 8750.0;
      break;

    case 2:
      stdded = 17500.0;
      min2file = 17500.0;
      break;

    case 5:
      stdded = 17500.0;
      min2file = 17500.0;
      break;

    case 3:
      stdded = 8750.0;
      min2file = 8750.0;
      break;

    case 4:
      stdded = 14000.0;
      min2file = 14000.0;
      break;

    default:
      stdded = 0;
;      fprintf(outfile, "Unknown status\n");
      exit(1);

  }

  if (L[6] <= min2file)
    fprintf(outfile, "You may not need to file NC tax return, due to your income.\n");

  L[8] = L[6] + L[7];
  L[10] = L[8] - L[9];
  if (L[11] < stdded)
    L[11] = stdded;

  L[12] = L[10] - L[11];
  L[14] = L[13] * L[12];
  L[15] = _nc_flat_tax_rate * L[14];
  L[17] = L[15] - L[16];
;  L[19] = L[17] + L[18];
  L[21] = ((L21a + L21b) + L21c) + L21d;
  L[23] = (L[20] + L[21]) + L[22];
  L[25] = L[23] - L[24];
  if (L[19] > L[25])
  {
    L[26] = L[19] - L[25];
;    min_payment = 0.9 * L[19];
    if ((L[23] < min_payment) && (L[19] > 1000.00))
    {
;    }

    L[27] = L[26];
  }
  else
  {
    L[28] = L[25] - L[19];
    L[32] = (L[29] + L[30]) + L[31];
    L[33] = L[28] - L[32];
  }

  showline(6);
  showline(7);
  showline(8);
  showline(9);
  showline(10);
  showline(11);
  if (L[11] < stdded)
    fprintf(outfile, " Check_UsedStdDed: X\n");
  else
    fprintf(outfile, " Check_ItemizedDed: X\n");

  showline(12);
  if (L[13] < 1.0)
    showline(13);

  showline_wmsg(14, "North Carolina Taxable Income");
  showline_wmsg(15, "North Carolina Income Tax");
  showline(16);
  showline(17);
  showline(18);
  showline(19);
  showline_wlabelmsg("L20", L[20], "North Carolina Tax Withheld");
  showline_wlabel("L21a", L21a);
  showline_wlabel("L21b", L21b);
  showline_wlabel("L21c", L21c);
  showline_wlabel("L21d", L21d);
  showline(22);
  showline(23);
  showline(25);
  if (L[19] > L[25])
  {
    showline_wmsg(26, "TAX DUE");
    showline_wmsg(27, "Pay this amount");
    fprintf(outfile, "         (Which is %2.1f%% of your total tax.)\n", (100.0 * L[26]) / (L[19] + 1e-9));
    if ((L[23] < min_payment) && (L[19] > 1000.00))
    {
      fprintf(outfile, " You may owe underpayment interest and penalties.\n");
      fprintf(outfile, " See page 6+7 instructions to calculate them according to your situation.\n");
    }

  }
  else
  {
    showline_wmsg(28, "OVERPAYMENT");
    showline(32);
    showline(33);
  }

  do_all_caps = 1;
  fprintf(outfile, "\n{ --------- }\n");
  GetTextLineF("Your1stName:");
  GetTextLineF("YourInitial:");
  GetTextLineF("YourLastName:");
  writeout_line = 0;
  socsec = GetTextLineF("YourSocSec#:");
  strcpy(socsectmp, socsec);
  format_socsec(socsectmp, 0);
  fprintf(outfile, "YourSocSec#: %s\n", socsectmp);
  free(socsec);
  writeout_line = 1;
  GetTextLineF("Spouse1stName:");
  GetTextLineF("SpouseInitial:");
  GetTextLineF("SpouseLastName:");
  writeout_line = 0;
  socsec = GetTextLineF("SpouseSocSec#:");
  strcpy(socsectmp, socsec);
  format_socsec(socsectmp, 0);
  fprintf(outfile, "SpouseSocSec#: %s\n", socsectmp);
  free(socsec);
  writeout_line = 1;
  GetTextLineF("Number&Street:");
  GetTextLineF("Apt#:");
  GetTextLineF("Town:");
  GetTextLineF("State:");
  GetTextLineF("Zipcode:");
  fclose(infile);
  fclose(outfile);
  Display_File(outfname);
;  return 0;
}


/* END of taxsolve_NC_D400_2017.c */
/* START of taxsolve_NJ_1040_2017.c */
float _nj_thisversion = 15.00;
double _nj_A[1000];
double _nj_S[1000];
double _nj_E[1000];
double _nj_TaxRateFormula(double x, int status)
{
  if ((status == 1) || (status == 3))
  {
    if (x < 20000.0)
      return x * 0.014;
    else
      if (x < 35000.0)
      return (x * 0.0175) - 70.0;
    else
      if (x < 40000.0)
      return (x * 0.035) - 682.5;
    else
      if (x < 75000.0)
      return (x * 0.05525) - 1492.5;
    else
      if (x < 500000.0)
      return (x * 0.0637) - 2126.25;
    else
      return (x * 0.0897) - 15126.25;





  }
  else
    if (((status == 2) || (status == 4)) || (status == 5))
  {
    if (x < 20000.0)
      return x * 0.014;
    else
      if (x < 50000.0)
      return (x * 0.0175) - 70.0;
    else
      if (x < 70000.0)
      return (x * 0.0245) - 420.0;
    else
      if (x < 80000.0)
      return (x * 0.035) - 1154.5;
    else
      if (x < 150000.0)
      return (x * 0.05525) - 2775.0;
    else
      if (x < 500000.0)
      return (x * 0.0637) - 4042.5;
    else
      return (x * 0.0897) - 17042.5;






  }
  else
  {
;    exit(1);
  }


}

void _nj_Report_bracket_info(double x, int status)
{
  double tx;
  double rate;
  tx = _nj_TaxRateFormula(x, status);
  if ((status == 1) || (status == 3))
  {
    if (x < 20000.0)
      rate = 0.014;
    else
      if (x < 35000.0)
      rate = 0.0175;
    else
      if (x < 40000.0)
      rate = 0.035;
    else
      if (x < 75000.0)
      rate = 0.05525;
    else
      if (x < 500000.0)
      rate = 0.0637;
    else
      rate = 0.0897;





  }
  else
  {
    if (x < 20000.0)
      rate = 0.014;
    else
      if (x < 50000.0)
      rate = 0.0175;
    else
      if (x < 70000.0)
      rate = 0.0245;
    else
      if (x < 80000.0)
      rate = 0.035;
    else
      if (x < 150000.0)
      rate = 0.05525;
    else
      if (x < 500000.0)
      rate = 0.0637;
    else
      rate = 0.0897;






  }

;  fprintf(outfile, " You are in the %2.1f%% marginal tax bracket,\n and you are paying an effective %2.1f%% tax on your total income.\n", 100.0 * rate, (100.0 * tx) / x);
}

double _nj_TaxRateFunction(double income, int status)
{
  double x;
  double dx;
  double tx;
  int k;
  if (income < 100000.0)
  {
    x = 50.0;
    dx = 0.5 * x;
    k = (income - 0.000001) / x;
    x = (x * ((double) k)) + dx;
    tx = (int) (_nj_TaxRateFormula(x, status) + 0.5);
  }
  else
    tx = _nj_TaxRateFormula(income, status);

  return tx;
}

int _nj_main(int argc, char *argv[])
{
  int i;
  int j;
  int k;
  char word[1000];
  char outfname[4000];
  int status = 0;
  time_t now;
  int L12a = 0;
  int L12b = 0;
  double L27a = 0.0;
  double L27b = 0.0;
  double L29a = 0.0;
  double L29b = 0.0;
  double L37a = 0.0;
  double Ab[10];
  double A9a = 0.0;
  double proptxcredit;
  double F[10];
  double Fb[10];
  double I[10];
  double Ib[10];
  char *Your1stName = "";
  char *YourLastName = "";
  char *YourInitial = "";
  char *Spouse1stName = "";
  char *SpouseLastName = "";
  char *SpouseInitial = "";
  char YourNames[2048] = "";
;  i = 1;
  k = 1;
  while (i < argc)
  {
    if (strcmp(argv[i], "-verbose") == 0)
      verbose = 1;
    else
      if (k == 1)
    {
      infile = fopen(argv[i], "r");
      if (infile == 0)
      {
;        exit(1);
      }

      k = 2;
      strcpy(outfname, argv[i]);
      j = strlen(outfname) - 1;
      while ((j >= 0) && (outfname[j] != '.'))
        j--;

      if (j < 0)
        strcat(outfname, "_out.txt");
      else
        strcpy(&outfname[j], "_out.txt");

      outfile = fopen(outfname, "w");
      if (outfile == 0)
      {
;        exit(1);
      }

;    }
    else
    {
;      exit(1);
    }


    i = i + 1;
  }

  if (infile == 0)
  {
;    exit(1);
  }

  for (i = 0; i < 1000; i++)
  {
    L[i] = 0.0;
    _nj_A[i] = 0.0;
    _nj_S[i] = 0.0;
    _nj_E[i] = 0.0;
  }

  read_line(infile, word);
  now = time(0);
  fprintf(outfile, "\n%s,	 v%2.2f, %s\n", word, _nj_thisversion, ctime(&now));
  get_parameter(infile, 's', word, "Status");
  get_parameter(infile, 'l', word, "Status ?");
  if (strncasecmp(word, "Single", 4) == 0)
    status = 1;
  else
    if (strncasecmp(word, "Married/Joint", 13) == 0)
    status = 2;
  else
    if (strncasecmp(word, "Married/Sep", 11) == 0)
    status = 3;
  else
    if (strncasecmp(word, "Head_of_House", 4) == 0)
    status = 4;
  else
    if (strncasecmp(word, "Widow", 4) == 0)
    status = 5;
  else
  {
;    fprintf(outfile, "Error: unrecognized status '%s'. Must be: Single, Married/joint, Married/sep, Head_of_house, Widow(er)\nExiting.\n", word);
    exit(1);
  }





  switch (status)
  {
    case 1:
      fprintf(outfile, "Status = Single (%d)\n", status);
      L[6] = 1;
      break;

    case 2:
      fprintf(outfile, "Status = Married/Joint (%d)\n", status);
      fprintf(outfile, " Check_Spouse = X\n");
      L[6] = 2;
      break;

    case 3:
      fprintf(outfile, "Status = Married/Sep (%d)\n", status);
      L[6] = 1;
      break;

    case 4:
      fprintf(outfile, "Status = Head_of_Household (%d)\n", status);
      L[6] = 1;
      break;

    case 5:
      fprintf(outfile, "Status = Widow(er) (%d)\n", status);
      L[6] = 1;
      break;

  }

  shownum(6);
  get_parameter(infile, 's', word, "YouOver65");
  get_parameter(infile, 'b', &j, "YouOver65");
  L[7] = j;
  if (j)
    fprintf(outfile, " Check_Over65 = X\n");

  get_parameter(infile, 's', word, "SpouseOver65");
  get_parameter(infile, 'b', &j, "SpouseOver65");
  if (status == 2)
  {
    L[7] = L[7] + j;
    if (j)
      fprintf(outfile, " Check_Spover65 = X\n");

  }

  shownum(7);
  get_parameter(infile, 's', word, "YouBlindDisa");
  get_parameter(infile, 'b', &j, "YouBlindDisa");
  L[8] = j;
  if (j)
    fprintf(outfile, " Check_Blind = X\n");

  get_parameter(infile, 's', word, "SpouseBlindDisa");
  get_parameter(infile, 'b', &j, "SpouseBlindDisa");
  if (status == 2)
  {
    L[8] = L[8] + j;
    if (j)
      fprintf(outfile, " Check_SpBlind = X\n");

  }

  shownum(8);
  get_parameter(infile, 's', word, "L9");
  get_parameter(infile, 'i', &j, "L9");
  L[9] = j;
  shownum(9);
  get_parameter(infile, 's', word, "L10");
  get_parameter(infile, 'i', &j, "L10");
  L[10] = j;
  shownum(10);
  get_parameter(infile, 's', word, "L11");
  get_parameter(infile, 'i', &j, "L11");
  L[11] = j;
  shownum(11);
  L12a = ((L[6] + L[7]) + L[8]) + L[11];
  fprintf(outfile, "L12a = %d\n", L12a);
  L12b = L[9] + L[10];
  fprintf(outfile, "L12b = %d\n", L12b);
  GetLineF("L14", &L[14]);
  GetLineF("L15a", &L[15]);
  GetLineF("L16", &L[16]);
  GetLine("L17", &L[17]);
  if (L[17] < 0.0)
    L[17] = 0.0;

  showline(17);
  GetLine("L18", &L[18]);
  if (L[18] < 0.0)
    L[18] = 0.0;

  showline(18);
  GetLineF("L19a", &L[19]);
  GetLineF("L20", &L[20]);
  GetLineF("L21", &L[21]);
  GetLineF("L22", &L[22]);
  GetLineF("L23", &L[23]);
  GetLineF("L24", &L[24]);
  GetLineF("L25", &L[25]);
  L[26] = ((((((((((L[14] + L[15]) + L[16]) + L[17]) + L[18]) + L[19]) + L[20]) + L[21]) + L[22]) + L[23]) + L[24]) + L[25];
  showline_wmsg(26, "Total Income");
  GetLineF("L27a", &L27a);
  GetLineF("L27b", &L27b);
  L[27] = L27a + L27b;
  showline(27);
  L[28] = L[26] - L[27];
  showline_wmsg(28, "NJ Gross Income");
  if ((status == 1) || (status == 3))
  {
    if (L[28] < 10000.0)
      fprintf(outfile, " --- You do not need to file, (except to get refund).  Income < $10,000. ---\n");

  }
  else
  {
    if (L[28] < 20000.0)
      fprintf(outfile, " --- You do not need to file, (except to get refund).  Income < $20,000. ---\n");

  }

  L29a = L12a * 1000.0;
  fprintf(outfile, " L29a = %6.2f\n", L29a);
  L29b = L12b * 1500.0;
  fprintf(outfile, " L29b = %6.2f\n", L29b);
  L[29] = L29a + L29b;
  fprintf(outfile, "L29c = %6.2f	Total Exemption Amount\n", L[29]);
  fprintf(outfile, "\n");
  GetLine("E1", &_nj_E[1]);
  showline_wrksht('E', 1, _nj_E);
  _nj_E[2] = 0.02 * L[28];
  showline_wrksht('E', 2, _nj_E);
  _nj_E[3] = NotLessThanZero(_nj_E[1] - _nj_E[2]);
  showline_wrksht('E', 3, _nj_E);
  GetLine("E4", &_nj_E[4]);
  showline_wrksht('E', 4, _nj_E);
  GetLine("E5", &_nj_E[5]);
  showline_wrksht('E', 5, _nj_E);
  _nj_E[6] = NotLessThanZero((_nj_E[3] + _nj_E[4]) + _nj_E[5]);
  showline_wrksht('E', 6, _nj_E);
  fprintf(outfile, "\n");
  L[30] = _nj_E[6];
  if (L[30] != 0.0)
    showline_wmsg(30, " Medical Expenses Worksheet E (See pg 27)");

  GetLineF("L31", &L[31]);
  GetLineF("L32", &L[32]);
  GetLineF("L33", &L[33]);
  GetLineF("L34", &L[34]);
  L[35] = ((((L[29] + L[30]) + L[31]) + L[32]) + L[33]) + L[34];
  showline_wmsg(35, "Total Exemptions and Deductions");
  L[36] = L[28] - L[35];
  if (L[36] > 0.0)
    showline_wmsg(36, "(Taxable Income)");

  GetLineF("L37a", &L37a);
  L[37] = L37a;
  GetLine("A1", &_nj_A[1]);
  GetLine("A9a", &A9a);
  fprintf(outfile, "\n");
  F[1] = L37a;
  showline_wrksht('F', 1, F);
  if (status != 3)
    F[2] = smallerof(F[1], 10000.0);
  else
    F[2] = smallerof(F[1], 5000.0);

  showline_wrksht('F', 2, F);
  if (status != 3)
    proptxcredit = 50.0;
  else
    proptxcredit = 25.0;

  if (A9a == 0.0)
  {
    F[3] = L[36];
    Fb[3] = L[36];
    fprintf(outfile, " F3a = %6.2f	F3b = %6.2f\n", F[3], Fb[3]);
    F[4] = F[2];
    Fb[4] = 0.0;
    fprintf(outfile, " F4a = %6.2f	F4b = %6.2f\n", F[4], Fb[4]);
    F[5] = F[3] - F[4];
    Fb[5] = Fb[3] - Fb[4];
    fprintf(outfile, " F5a = %6.2f	F5b = %6.2f\n", F[5], Fb[5]);
    F[6] = _nj_TaxRateFunction(F[5], status);
    Fb[6] = _nj_TaxRateFunction(Fb[5], status);
    fprintf(outfile, " F6a = %6.2f	F6b = %6.2f\n", F[6], Fb[6]);
    F[7] = Fb[6] - F[6];
    showline_wrksht('F', 7, F);
    if (F[7] >= proptxcredit)
    {
      fprintf(outfile, " F8. Yes. (Take Property Tax Deduction.)\n");
      L[38] = F[4];
      L[39] = F[5];
      L[40] = F[6];
      L[49] = 0.0;
    }
    else
    {
      fprintf(outfile, " F8. No. (Take Property Tax Credit.)\n");
      L[38] = 0.0;
      L[39] = Fb[5];
      L[40] = Fb[6];
      L[49] = proptxcredit;
    }

  }
  else
  {
    fprintf(outfile, "\nSchedule A:\n");
    showline_wrksht('A', 1, _nj_A);
    _nj_A[2] = L[28];
    showline_wrksht('A', 2, _nj_A);
    _nj_A[3] = smallerof(1.0, _nj_A[1] / _nj_A[2]);
    fprintf(outfile, " A3 = %6.2f %%\n", 100.0 * _nj_A[3]);
    _nj_A[4] = L[36];
    fprintf(outfile, " A4a = %6.2f	A4b = %6.2f\n", _nj_A[4], _nj_A[4]);
    fprintf(outfile, " (5a = %6.2f)\n", F[1]);
    _nj_A[5] = F[2];
    fprintf(outfile, " A5a = %6.2f	A5b = %6.2f\n", _nj_A[5], 0.0);
    _nj_A[6] = _nj_A[4] - _nj_A[5];
    Ab[6] = _nj_A[4] - 0.0;
    fprintf(outfile, " A6a = %6.2f	A6b = %6.2f\n", _nj_A[6], Ab[6]);
    _nj_A[7] = _nj_TaxRateFunction(_nj_A[6], status);
    Ab[7] = _nj_TaxRateFunction(Ab[6], status);
    fprintf(outfile, " A7a = %6.2f	A7b = %6.2f\n", _nj_A[7], Ab[7]);
    _nj_A[8] = _nj_A[3] * _nj_A[7];
    Ab[8] = _nj_A[3] * Ab[7];
    fprintf(outfile, " A8a = %6.2f	A8b = %6.2f\n", _nj_A[8], Ab[8]);
    fprintf(outfile, "  (9a = %6.2f)\n", A9a);
    _nj_A[9] = smallerof(smallerof(A9a, _nj_A[8]), _nj_A[7]);
    Ab[9] = smallerof(smallerof(A9a, Ab[8]), Ab[7]);
    fprintf(outfile, " A9a = %6.2f	A9b = %6.2f\n", _nj_A[9], Ab[9]);
    fprintf(outfile, "\nWorksheet I:\n");
    I[1] = _nj_A[7];
    Ib[1] = Ab[7];
    fprintf(outfile, " I1a = %6.2f	I1b = %6.2f\n", I[1], Ib[1]);
    I[2] = _nj_A[9];
    Ib[2] = Ab[9];
    fprintf(outfile, " I2a = %6.2f	I2b = %6.2f\n", I[2], Ib[2]);
    I[3] = I[1] - I[2];
    Ib[3] = Ib[1] - Ib[2];
    fprintf(outfile, " I3a = %6.2f	I3b = %6.2f\n", I[3], Ib[3]);
    Ib[4] = Ib[3] - I[3];
    showline_wrksht('I', 4, Ib);
    if (Ib[4] >= proptxcredit)
    {
      fprintf(outfile, " Sched-I, Yes:  Take PropTax Deduction\n\n");
      L[38] = _nj_A[5];
      L[39] = _nj_A[6];
      L[40] = _nj_A[7];
      L[41] = I[2];
      L[49] = 0.0;
    }
    else
    {
      fprintf(outfile, " Sched-I, No:  Take PropTax Credit\n\n");
      L[38] = 0.0;
      L[39] = Ab[6];
      L[40] = Ab[7];
      L[41] = Ib[2];
      L[49] = proptxcredit;
    }

  }

  if (L37a == 0.0)
    L[49] = 0.0;

  if (L[37] > 0.0)
    fprintf(outfile, "L37a = %6.2f\n", L[37]);

  showline(38);
  fprintf(outfile, "\n");
  if (L[39] > 0.0)
    showline_wmsg(39, "NJ Taxable Income");

  showline_wmsg(40, "TAX");
  _nj_Report_bracket_info(L[39], status);
  if (_nj_A[1] > 0.0)
    showline_wmsg(41, "( Credit for Taxes paid to other jurisdictions. )\n");

  L[42] = L[40] - L[41];
  showline_wmsg(42, "( Balance of Tax )");
  GetLineF("L43", &L[43]);
  L[44] = L[42] - L[43];
  showline(44);
  GetLineF("L45", &L[45]);
  GetLineF("L46", &L[46]);
  L[47] = (L[44] + L[45]) + L[46];
  showline(47);
  GetLine("L48", &L[48]);
  showline_wmsg(48, "Total NJ Income Tax Withheld");
  showline_wmsg(49, "Property tax Credit");
  GetLineF("L50", &L[50]);
  GetLineF("L51", &L[51]);
  GetLineF("L52", &L[52]);
  GetLineF("L53", &L[53]);
  GetLineF("L54", &L[54]);
  L[55] = (((((L[48] + L[49]) + L[50]) + L[51]) + L[52]) + L[53]) + L[54];
  showline_wmsg(55, "Total Payments/Credits");
  if (L[55] < L[47])
  {
    L[56] = L[47] - L[55];
    fprintf(outfile, "L56 = %6.2f	DUE !!!\n", L[56]);
    fprintf(outfile, "         (Which is %2.1f%% of your total tax.)\n", (100.0 * L[56]) / (L[40] + 1e-9));
  }
  else
  {
    L[57] = L[55] - L[47];
    fprintf(outfile, "L57 = %6.2f	Overpayment\n", L[57]);
    L[65] = 0.0;
    showline_wmsg(65, "( Total Contributions from overpayment )");
    L[66] = L[57] - L[65];
    showline_wmsg(66, "Refund !!!");
  }

  fprintf(outfile, "\n{ --------- }\n");
  Your1stName = GetTextLineF("Your1stName:");
  YourInitial = GetTextLineF("YourInitial:");
  YourLastName = GetTextLineF("YourLastName:");
  GetTextLineF("YourSocSec#:");
  Spouse1stName = GetTextLineF("Spouse1stName:");
  SpouseInitial = GetTextLineF("SpouseInitial:");
  SpouseLastName = GetTextLineF("SpouseLastName:");
  GetTextLineF("SpouseSocSec#:");
  if (strlen(YourLastName) > 0)
  {
    strcpy(YourNames, YourLastName);
    strcat(YourNames, ", ");
    strcat(YourNames, Your1stName);
    if (YourInitial[0] != '\0')
    {
      strcat(YourNames, ", ");
      strcat(YourNames, YourInitial);
    }

    if (Spouse1stName[0] != '\0')
    {
      strcat(YourNames, ", ");
      if ((SpouseLastName[0] != '\0') && (strcmp(YourLastName, SpouseLastName) != 0))
      {
        strcat(YourNames, SpouseLastName);
        strcat(YourNames, ", ");
      }

      strcat(YourNames, Spouse1stName);
      if (SpouseInitial[0] != '\0')
      {
        strcat(YourNames, ", ");
        strcat(YourNames, SpouseInitial);
      }

    }

    fprintf(outfile, "YourNames: %s\n", YourNames);
  }

  GetTextLineF("Number&Street:");
  GetTextLineF("Town:");
  GetTextLineF("State:");
  GetTextLineF("Zipcode:");
  fclose(infile);
  fclose(outfile);
  Display_File(outfname);
;  return 0;
}


/* END of taxsolve_NJ_1040_2017.c */
/* START of taxsolve_NY_IT201_2017.c */
float _ny_thisversion = 15.00;
double _ny_A[10];
double _ny_S[10];
int _ny_status = 0;
char _ny_statusnames[10][20] = {"0", "Single", "Married/Joint", "Married/Sep", "Head_of_House", "Widow"};
char *_ny_Your1stName = "";
char *_ny_YourLastName = "";
char *_ny_YourInitial = "";
char *_ny_Spouse1stName = "";
char *_ny_SpouseLastName = "";
char *_ny_SpouseInitial = "";
char *_ny_YourSocSec = 0;
char *_ny_SpouseSocSec = 0;
char *_ny_MailAddress = 0;
char *_ny_AptNumber = 0;
char _ny_Town[2048] = "";
char _ny_StateName[1024] = "";
char _ny_Zipcode[1024] = "";
struct _ny_FedReturnData
{
  double fedline[1000];
  double schedA[1000];
  double schedD[1000];
  int Exception;
  int Itemized;
} _ny_PrelimFedReturn;
void _ny_convert_slashes(char *fname)
{
  char *ptr;
  char slash_sreach = '\\';
  char slash_replace = '/';
  ptr = strchr(fname, slash_sreach);
  while (ptr)
  {
    ptr[0] = slash_replace;
    ptr = strchr(fname, slash_sreach);
  }

}

char *_ny_pull_initial(char *name)
{
  int j = 0;
  char midinitial[10];
  while ((name[j] != '\0') && (name[j] != ','))
    j++;

  if (name[j] == ',')
  {
    name[j++] = '\0';
    while ((name[j] != '\0') && isspace(name[j]))
      j++;

    midinitial[0] = name[j];
    midinitial[1] = '\0';
  }
  else
    strcpy(midinitial, "");

  return strdup(midinitial);
}

int _ny_ImportFederalReturnData(char *fedlogfile, struct _ny_FedReturnData *fed_data)
{
  FILE *infile;
  char fline[2000];
  char word[2000];
  int linenum;
  for (linenum = 0; linenum < 1000; linenum++)
  {
    fed_data->fedline[linenum] = 0.0;
    fed_data->schedA[linenum] = 0.0;
    fed_data->schedD[linenum] = 0.0;
  }

  _ny_convert_slashes(fedlogfile);
  infile = fopen(fedlogfile, "r");
  if (infile == 0)
  {
;    fprintf(outfile, "Error: Could not open Federal return '%s'\n", fedlogfile);
    return 0;
  }

  fed_data->Itemized = 1;
  read_line(infile, fline);
  linenum = 0;
  while (!feof(infile))
  {
    if (strstr(fline, "Use standard deduction.") != 0)
      fed_data->Itemized = 0;

    next_word(fline, word, " \t=");
    if ((strstr(word, "L") == word) && (strstr(fline, " = ") != 0))
    {
      if (strcmp(word, "L9b") != 0)
      {
        if (sscanf(&word[1], "%d", &linenum) != 1)
        {
;          fprintf(outfile, "Error: Reading Fed line number '%s%s'\n", word, fline);
        }

        next_word(fline, word, " \t=");
        if (sscanf(word, "%lf", &fed_data->fedline[linenum]) != 1)
        {
;          fprintf(outfile, "Error: Reading Fed line %d '%s%s'\n", linenum, word, fline);
        }

        if (verbose)
;
      }

    }
    else
      if (((strstr(word, "A") == word) && (strstr(word, "AMT") != word)) && (strstr(fline, " = ") != 0))
    {
      if (sscanf(&word[1], "%d", &linenum) != 1)
      {
;        fprintf(outfile, "Error: Reading Fed line number '%s%s'\n", word, fline);
      }

      next_word(fline, word, " \t=");
      if (sscanf(word, "%lf", &fed_data->schedA[linenum]) != 1)
      {
;        fprintf(outfile, "Error: Reading Fed schedA %d '%s%s'\n", linenum, word, fline);
      }

      if (verbose)
;
    }
    else
      if ((strstr(word, "D") == word) && (strstr(fline, " = ") != 0))
    {
      if (sscanf(&word[1], "%d", &linenum) != 1)
      {
;        fprintf(outfile, "Error: Reading Fed line number '%s%s'\n", word, fline);
      }

      next_word(fline, word, " \t=");
      if (strcmp(word, "d") == 0)
      {
        next_word(fline, word, " \teh=");
        while (word[0] != '\0')
        {
          if (sscanf(word, "%lf", &fed_data->schedD[linenum]) != 1)
            fprintf(outfile, "Error: Reading Fed schedD %d '%s %s'\n", linenum, word, fline);

          next_word(fline, word, " \teh=");
        }

      }
      else
        if (sscanf(word, "%lf", &fed_data->schedD[linenum]) != 1)
      {
        if (strncasecmp(word, "yes", 1) == 0)
          fed_data->schedD[linenum] = 1;
        else
          if (strncasecmp(word, "no", 1) == 0)
          fed_data->schedD[linenum] = 0;
        else
        {
;          fprintf(outfile, "Error: Reading Fed schedD %d '%s%s'\n", linenum, word, fline);
        }


      }


      if (verbose)
;
    }
    else
      if (strcmp(word, "Status") == 0)
    {
      next_word(fline, word, " \t=");
      fprintf(outfile, " Status %s\n", word);
      if (strncasecmp(word, "Single", 4) == 0)
        _ny_status = 1;
      else
        if (strncasecmp(word, "Married/Joint", 13) == 0)
        _ny_status = 2;
      else
        if (strncasecmp(word, "Married/Sep", 11) == 0)
        _ny_status = 3;
      else
        if (strncasecmp(word, "Head_of_House", 4) == 0)
        _ny_status = 4;
      else
        if (strncasecmp(word, "Widow", 4) == 0)
        _ny_status = 5;
      else
      {
;        fprintf(outfile, "Error: unrecognized status '%s'. Exiting.\n", word);
        return 0;
      }





    }
    else
      if (strcmp(word, "Your1stName:") == 0)
    {
      _ny_Your1stName = strdup(fline);
      _ny_YourInitial = _ny_pull_initial(_ny_Your1stName);
    }
    else
      if (strcmp(word, "YourLastName:") == 0)
    {
      _ny_YourLastName = strdup(fline);
    }
    else
      if (strcmp(word, "YourSocSec#:") == 0)
    {
      _ny_YourSocSec = strdup(fline);
    }
    else
      if (strcmp(word, "Spouse1stName:") == 0)
    {
      _ny_Spouse1stName = strdup(fline);
      _ny_SpouseInitial = _ny_pull_initial(_ny_Spouse1stName);
    }
    else
      if (strcmp(word, "SpouseLastName:") == 0)
    {
      _ny_SpouseLastName = strdup(fline);
    }
    else
      if (strcmp(word, "SpouseSocSec#:") == 0)
    {
      _ny_SpouseSocSec = strdup(fline);
    }
    else
      if (strcmp(word, "Number&Street:") == 0)
    {
      _ny_MailAddress = strdup(fline);
    }
    else
      if (strcmp(word, "Apt#:") == 0)
    {
      _ny_AptNumber = strdup(fline);
    }
    else
      if (strcmp(word, "TownStateZip:") == 0)
    {
      next_word(fline, _ny_Town, ",");
      next_word(fline, _ny_StateName, " \t,");
      next_word(fline, _ny_Zipcode, " \t,");
    }













    read_line(infile, fline);
  }

  fclose(infile);
  return 1;
}

double _ny_TaxRateFunction(double income, int _ny_status)
{
  double tax;
  switch (_ny_status)
  {
    case 2:

    case 5:
      if (income <= 17150.0)
      tax = 0.04 * income;
    else
      if (income <= 23600.0)
      tax = 686.0 + (0.045 * (income - 17150.0));
    else
      if (income <= 27900.0)
      tax = 976.0 + (0.0525 * (income - 23600.0));
    else
      if (income <= 43000.0)
      tax = 1202.0 + (0.059 * (income - 27900.0));
    else
      if (income <= 161550.0)
      tax = 2093.0 + (0.0645 * (income - 43000.0));
    else
      if (income <= 323200.0)
      tax = 9739.0 + (0.0665 * (income - 161550.0));
    else
      if (income <= 2155350.0)
      tax = 20498.0 + (0.0685 * (income - 323200.0));
    else
      tax = 145991.0 + (0.0882 * (income - 2155350.0));







      break;

    case 1:

    case 3:
      if (income <= 8500.0)
      tax = 0.04 * income;
    else
      if (income <= 11700.0)
      tax = 340.0 + (0.045 * (income - 8500.0));
    else
      if (income <= 13900.0)
      tax = 484.0 + (0.0525 * (income - 11700.0));
    else
      if (income <= 21400.0)
      tax = 600.0 + (0.059 * (income - 13900.0));
    else
      if (income <= 80650.0)
      tax = 1042.0 + (0.0645 * (income - 21400.0));
    else
      if (income <= 215400.0)
      tax = 4864.0 + (0.0665 * (income - 80650.0));
    else
      if (income <= 1077550.0)
      tax = 13825.0 + (0.0685 * (income - 215400.0));
    else
      tax = 72882.0 + (0.0882 * (income - 1077550.0));







      break;

    case 4:
      if (income <= 12080.0)
      tax = 0.04 * income;
    else
      if (income <= 17650.0)
      tax = 512.0 + (0.045 * (income - 12800.0));
    else
      if (income <= 20900.0)
      tax = 730.0 + (0.0525 * (income - 17650.0));
    else
      if (income <= 32200.0)
      tax = 901.0 + (0.059 * (income - 20900.0));
    else
      if (income <= 107650.0)
      tax = 1568.0 + (0.0645 * (income - 32200.0));
    else
      if (income <= 269300.0)
      tax = 6434.0 + (0.0665 * (income - 107650.0));
    else
      if (income <= 1616450.0)
      tax = 17184.0 + (0.0685 * (income - 269300.0));
    else
      tax = 109464.0 + (0.0882 * (income - 1616450.0));







      break;

    default:
;      exit(0);
      break;

  }

  return tax;
}

void _ny_Report_bracket_info(double income, double tx, int _ny_status)
{
  double rate;
  switch (_ny_status)
  {
    case 2:

    case 5:
      if (income <= 17150.0)
      rate = 0.04;
    else
      if (income <= 23600.0)
      rate = 0.045;
    else
      if (income <= 27900.0)
      rate = 0.0525;
    else
      if (income <= 43000.0)
      rate = 0.059;
    else
      if (income <= 161550.0)
      rate = 0.0645;
    else
      if (income <= 323200.0)
      rate = 0.0665;
    else
      if (income <= 2155350.0)
      rate = 0.0685;
    else
      rate = 0.0882;







      break;

    case 1:

    case 3:
      if (income <= 8500.0)
      rate = 0.04;
    else
      if (income <= 11700.0)
      rate = 0.045;
    else
      if (income <= 13900.0)
      rate = 0.0525;
    else
      if (income <= 21400.0)
      rate = 0.059;
    else
      if (income <= 80650.0)
      rate = 0.0645;
    else
      if (income <= 215400.0)
      rate = 0.0665;
    else
      if (income <= 1077550.0)
      rate = 0.0685;
    else
      rate = 0.0882;







      break;

    case 4:
      if (income <= 12800.0)
      rate = 0.04;
    else
      if (income <= 17650.0)
      rate = 0.045;
    else
      if (income <= 20900.0)
      rate = 0.0525;
    else
      if (income <= 32200.0)
      rate = 0.059;
    else
      if (income <= 107650.0)
      rate = 0.0645;
    else
      if (income <= 269300.0)
      rate = 0.0665;
    else
      if (income <= 1616450.0)
      rate = 0.0685;
    else
      rate = 0.0882;







      break;

    default:
;      exit(0);
      break;

  }

;  fprintf(outfile, " You are in the %2.1f%% marginal tax bracket,\n and you are paying an effective %2.1f%% tax on your total income.\n", 100.0 * rate, (100.0 * tx) / income);
}

double _ny_TaxRateLookup(double income, int _ny_status)
{
  double tax;
  double dx;
  int m;
  if (income < 25.0)
    dx = 12.5;
  else
    if (income < 50.0)
    dx = 25.0;
  else
    dx = 50.0;


  m = income / dx;
  income = (((double) m) * dx) + (0.5 * dx);
  tax = _ny_TaxRateFunction(income, _ny_status);
  return (int) (tax + 0.5);
}

double _ny_NYcityTaxRateFunction(double income, int _ny_status)
{
  double tax;
  double dx;
  int m;
  if (income < 25.0)
    dx = 12.5;
  else
    if (income < 50.0)
    dx = 25.0;
  else
    dx = 50.0;


  m = income / dx;
  if (income < 65000.0)
    income = (m * dx) + (0.5 * dx);

  if ((_ny_status == 2) || (_ny_status == 5))
  {
    if (income < 21600.0)
      tax = income * 0.03078;
    else
      if (income < 45000.0)
      tax = ((income - 21600.00) * 0.03762) + 665.00;
    else
      if (income < 90000.0)
      tax = ((income - 45000.00) * 0.03819) + 1545.0;
    else
      tax = ((income - 90000.00) * 0.03876) + 3264.0;



  }
  else
    if ((_ny_status == 1) || (_ny_status == 3))
  {
    if (income < 12000.0)
      tax = income * 0.03078;
    else
      if (income < 25000.0)
      tax = ((income - 12000.00) * 0.03762) + 369.0;
    else
      if (income < 50000.0)
      tax = ((income - 25000.00) * 0.03819) + 858.0;
    else
      tax = ((income - 50000.00) * 0.03876) + 1813.00;



  }
  else
    if (_ny_status == 4)
  {
    if (income < 14400.00)
      tax = income * 0.03078;
    else
      if (income < 30000.00)
      tax = ((income - 14400.00) * 0.03762) + 443.0;
    else
      if (income < 60000.00)
      tax = ((income - 30000.00) * 0.03819) + 1030.0;
    else
      tax = ((income - 60000.00) * 0.03876) + 2176.0;



  }
  else
  {
;    exit(1);
  }



  if (income < 65000.0)
    tax = (int) (tax + 0.5);

  return tax;
}

void _ny_worksheet1()
{
  double ws[100];
  ws[1] = L[33];
  ws[2] = L[38];
  ws[3] = 0.0645 * ws[2];
  if (ws[1] >= 157650.0)
    ws[9] = ws[3];
  else
  {
    ws[4] = _ny_TaxRateFunction(ws[2], _ny_status);
    ws[5] = ws[3] - ws[4];
    ws[6] = ws[1] - 107650.0;
    ws[7] = 0.0001 * ((double) Round(10000.0 * (ws[6] / 50000.0)));
    ws[8] = ws[5] * ws[7];
    ws[9] = ws[4] + ws[8];
  }

  L[39] = ws[9];
}

void _ny_worksheet2()
{
  double ws[100];
  ws[1] = L[33];
  ws[2] = L[38];
  ws[3] = 0.0665 * ws[2];
  if (ws[1] >= 211550.0)
    ws[11] = ws[3];
  else
  {
    ws[4] = _ny_TaxRateFunction(ws[2], _ny_status);
    ws[5] = ws[3] - ws[4];
    ws[6] = 681.0;
    ws[7] = ws[5] - ws[6];
    ws[8] = ws[1] - 160500.0;
    ws[9] = 0.0001 * ((double) Round(10000.0 * (ws[8] / 50000.0)));
    ws[10] = ws[7] * ws[9];
    ws[11] = (ws[4] + ws[6]) + ws[10];
  }

  L[39] = ws[11];
}

void _ny_worksheet3()
{
  double ws[100];
  ws[1] = L[33];
  ws[2] = L[38];
  ws[3] = 0.0685 * ws[2];
  if (ws[1] >= 373200.0)
    ws[11] = ws[3];
  else
  {
    ws[4] = _ny_TaxRateFunction(ws[2], _ny_status);
    ws[5] = ws[3] - ws[4];
    ws[6] = 1004.0;
    ws[7] = ws[5] - ws[6];
    ws[8] = ws[1] - 323200.0;
    ws[9] = 0.0001 * ((double) Round(10000.0 * (ws[8] / 50000.0)));
    ws[10] = ws[7] * ws[9];
    ws[11] = (ws[4] + ws[6]) + ws[10];
  }

  L[39] = ws[11];
}

void _ny_worksheet4()
{
  double ws[100];
  ws[1] = L[33];
  ws[2] = L[38];
  ws[3] = 0.0882 * ws[2];
  if (ws[1] >= 2205350.0)
    ws[11] = ws[3];
  else
  {
    ws[4] = _ny_TaxRateFunction(ws[2], _ny_status);
    ws[5] = ws[3] - ws[4];
    if (ws[2] <= 161550.0)
      ws[6] = 681.0;
    else
      if (ws[2] <= 323200.0)
      ws[6] = 1004.0;
    else
      ws[6] = 1650.0;


    ws[7] = ws[5] - ws[6];
    ws[8] = ws[1] - 2155350.0;
    ws[9] = 0.0001 * ((double) Round(10000.0 * (ws[8] / 50000.0)));
    ws[10] = ws[7] * ws[9];
    ws[11] = (ws[4] + ws[6]) + ws[10];
  }

  L[39] = ws[11];
}

void _ny_worksheet5()
{
  double ws[100];
  ws[1] = L[33];
  ws[2] = L[38];
  ws[3] = 0.0665 * ws[2];
  if (ws[1] >= 157650.0)
    ws[9] = ws[3];
  else
  {
    ws[4] = _ny_TaxRateFunction(ws[2], _ny_status);
    ws[5] = ws[3] - ws[4];
    ws[6] = ws[1] - 107650.0;
    ws[7] = 0.0001 * ((double) Round(10000.0 * (ws[6] / 50000.0)));
    ws[8] = ws[5] * ws[7];
    ws[9] = ws[4] + ws[8];
  }

  L[39] = ws[9];
}

void _ny_worksheet6()
{
  double ws[100];
  ws[1] = L[33];
  ws[2] = L[38];
  ws[3] = 0.0685 * ws[2];
  if (ws[1] >= 265400.0)
    ws[11] = ws[3];
  else
  {
    ws[4] = _ny_TaxRateFunction(ws[2], _ny_status);
    ws[5] = ws[3] - ws[4];
    ws[6] = 500.0;
    ws[7] = ws[5] - ws[6];
    ws[8] = ws[1] - 214000.0;
    ws[9] = 0.0001 * ((double) Round(10000.0 * (ws[8] / 50000.0)));
    ws[10] = ws[7] * ws[9];
    ws[11] = (ws[4] + ws[6]) + ws[10];
  }

  L[39] = ws[11];
}

void _ny_worksheet7()
{
  double ws[100];
  ws[1] = L[33];
  ws[2] = L[38];
  ws[3] = 0.0882 * ws[2];
  if (ws[1] >= 1127550.0)
    ws[11] = ws[3];
  else
  {
    ws[4] = _ny_TaxRateFunction(ws[2], _ny_status);
    ws[5] = ws[3] - ws[4];
    if (ws[2] <= 215400.0)
      ws[6] = 500.0;
    else
      ws[6] = 930.0;

    ws[7] = ws[5] - ws[6];
    ws[8] = ws[1] - 1077550.0;
    ws[9] = 0.0001 * ((double) Round(10000.0 * (ws[8] / 50000.0)));
    ws[10] = ws[7] * ws[9];
    ws[11] = (ws[4] + ws[6]) + ws[10];
  }

  L[39] = ws[11];
}

void _ny_worksheet8()
{
  double ws[100];
  ws[1] = L[33];
  ws[2] = L[38];
  ws[3] = 0.0665 * ws[2];
  if (ws[1] >= 157650.0)
    ws[9] = ws[3];
  else
  {
    ws[4] = _ny_TaxRateFunction(ws[2], _ny_status);
    ws[5] = ws[3] - ws[4];
    ws[6] = ws[1] - 107650.0;
    ws[7] = 0.0001 * ((double) Round(10000.0 * (ws[6] / 50000.0)));
    ws[8] = ws[5] * ws[7];
    ws[9] = ws[4] + ws[8];
  }

  L[39] = ws[9];
}

void _ny_worksheet9()
{
  double ws[100];
  ws[1] = L[33];
  ws[2] = L[38];
  ws[3] = 0.0685 * ws[2];
  if (ws[1] >= 319300.0)
    ws[11] = ws[3];
  else
  {
    ws[4] = _ny_TaxRateFunction(ws[2], _ny_status);
    ws[5] = ws[3] - ws[4];
    ws[6] = 725.0;
    ws[7] = ws[5] - ws[6];
    ws[8] = ws[1] - 269300.0;
    ws[9] = 0.0001 * ((double) Round(10000.0 * (ws[8] / 50000.0)));
    ws[10] = ws[7] * ws[9];
    ws[11] = (ws[4] + ws[6]) + ws[10];
  }

  L[39] = ws[11];
}

void _ny_worksheet10()
{
  double ws[100];
  ws[1] = L[33];
  ws[2] = L[38];
  ws[3] = 0.0882 * ws[2];
  if (ws[1] >= 1666450.0)
    ws[11] = ws[3];
  else
  {
    ws[4] = _ny_TaxRateFunction(ws[2], _ny_status);
    ws[5] = ws[3] - ws[4];
    if (ws[2] <= 269300.0)
      ws[6] = 725.0;
    else
      ws[6] = 1263.0;

    ws[7] = ws[5] - ws[6];
    ws[8] = ws[1] - 1616550.0;
    ws[9] = 0.0001 * ((double) Round(10000.0 * (ws[8] / 50000.0)));
    ws[10] = ws[7] * ws[9];
    ws[11] = (ws[4] + ws[6]) + ws[10];
  }

  L[39] = ws[11];
}

void _ny_tax_computation_worksheet(int _ny_status)
{
  switch (_ny_status)
  {
    case 2:

    case 5:
      if (L[33] <= 2155350.0)
    {
      if (L[38] <= 161550.0)
        _ny_worksheet1();
      else
        if ((L[33] > 161550.0) && (L[38] <= 323200.0))
        _ny_worksheet2();
      else
        if ((L[33] > 323200.0) && (L[38] > 323200.0))
        _ny_worksheet3();
      else
        _ny_worksheet4();



    }
    else
      _ny_worksheet4();

      break;

    case 1:

    case 3:
      if (L[33] <= 1077550.0)
    {
      if (L[38] <= 215400.0)
        _ny_worksheet5();
      else
        _ny_worksheet6();

    }
    else
      _ny_worksheet7();

      break;

    case 4:
      if (L[33] <= 1616450.0)
    {
      if (L[38] <= 269300.0)
        _ny_worksheet8();
      else
        _ny_worksheet9();

    }
    else
      _ny_worksheet10();

      break;

    default:
;      fprintf(outfile, "Case not handled.\n");
      exit(1);

  }

}

int _ny_main(int argc, char *argv[])
{
  int j;
  int k;
  int argk;
  int day;
  int month;
  int yyyy;
  char word[1000];
  char outfname[1000];
  char *answ;
  time_t now;
  int Dependent;
  int Exemptions;
  int nyc_resident;
  double itemized_ded;
  double std_ded = 0.0;
  double LTC = 0;
  double AddAdj = 0.0;
  double CollegeDed = 0.0;
  double ded_sched[1000];
  char prelim_1040_outfilename[5000];
  char YourNames[2048] = "";
;  argk = 1;
  k = 1;
  while (argk < argc)
  {
    if (strcmp(argv[argk], "-verbose") == 0)
      verbose = 1;
    else
      if (k == 1)
    {
      infile = fopen(argv[argk], "r");
      if (infile == 0)
      {
;        exit(1);
      }

      k = 2;
      strcpy(outfname, argv[argk]);
      j = strlen(outfname) - 1;
      while ((j >= 0) && (outfname[j] != '.'))
        j--;

      if (j < 0)
        strcat(outfname, "_out.txt");
      else
        strcpy(&outfname[j], "_out.txt");

      outfile = fopen(outfname, "w");
      if (outfile == 0)
      {
;        exit(1);
      }

;    }
    else
    {
;      exit(1);
    }


    argk = argk + 1;
  }

  if (infile == 0)
  {
;    exit(1);
  }

  for (j = 0; j < 1000; j++)
  {
    L[j] = 0.0;
    ded_sched[j] = 0.0;
  }

  read_line(infile, word);
  now = time(0);
  fprintf(outfile, "\n%s,	 v%2.2f, %s\n", word, _ny_thisversion, ctime(&now));
  get_parameter(infile, 's', word, "FileName");
  get_word(infile, prelim_1040_outfilename);
  if (_ny_ImportFederalReturnData(prelim_1040_outfilename, &_ny_PrelimFedReturn) == 0)
  {
    fclose(infile);
    fclose(outfile);
    Display_File(outfname);
    exit(1);
  }

  answ = GetTextLine("YourDOB");
  if (interpret_date(answ, &day, &month, &yyyy, "reading 'YourDOB'"))
    fprintf(outfile, "YourDOB %s\n", format_mmddyyyy(month, day, yyyy));
  else
    fprintf(outfile, "YourDOB %s\n", answ);

  answ = GetTextLine("SpouseDOB");
  if (interpret_date(answ, &day, &month, &yyyy, "reading 'SpouseDOB'"))
    fprintf(outfile, "SpouseDOB %s\n", format_mmddyyyy(month, day, yyyy));
  else
    fprintf(outfile, "SpouseDOB %s\n", answ);

  GetTextLineF("County");
  GetTextLineF("SchooldDist");
  GetTextLineF("SchoolCode");
  answ = GetTextLineF("D1_ForeignAcct");
  next_word(answ, word, " \t;");
  if (toupper(word[0]) == 'Y')
    fprintf(outfile, "CkD1y: X\n");
  else
    fprintf(outfile, "CkD1n: X\n");

  answ = GetTextLineF("D2_1-YonkRelCred");
  next_word(answ, word, " \t;");
  if (toupper(word[0]) == 'Y')
    fprintf(outfile, "CkD2_1y: X\n");
  else
    if ((mystrcasestr(word, "N/A") == 0) && (toupper(word[0]) == 'N'))
    fprintf(outfile, "CkD2_1n: X\n");


  GetTextLineF("D2_2-YRCamount");
  answ = GetTextLineF("D3-NonQualComp");
  next_word(answ, word, " \t;");
  if (toupper(word[0]) == 'Y')
    fprintf(outfile, "CkD3y: X\n");
  else
    fprintf(outfile, "CkD3n: X\n");

  answ = GetTextLineF("E1_LivedNYC");
  next_word(answ, word, " \t;");
  if (toupper(word[0]) == 'Y')
    fprintf(outfile, "CkE1y: X\n");
  else
    fprintf(outfile, "CkE1n: X\n");

  GetTextLineF("E2_DaysNYC");
  GetTextLineF("F1_MonthsYouNYC");
  GetTextLineF("F2_MonthsSpNYC");
  GetTextLineF("G_SpecCondCode");
  get_parameter(infile, 's', word, "Dependent");
  get_parameter(infile, 'b', &Dependent, "Dependent?");
  L[1] = _ny_PrelimFedReturn.fedline[7];
  if (_ny_PrelimFedReturn.Itemized)
  {
    fprintf(outfile, " Check box B = Yes\n");
    fprintf(outfile, "  Check_Itemized = X\n");
  }
  else
  {
    fprintf(outfile, " Check box B = No\n");
    fprintf(outfile, "  Check_NoItemiz = X\n");
  }

  if (Dependent == 1)
  {
    fprintf(outfile, " Check box C = Yes\n");
    fprintf(outfile, "  Check_Depend = X\n");
  }
  else
  {
    fprintf(outfile, " Check box C = No\n");
    fprintf(outfile, "  Check_NotDep = X\n");
  }

  showline(1);
  L[2] = _ny_PrelimFedReturn.fedline[8];
  showline(2);
  L[3] = _ny_PrelimFedReturn.fedline[9];
  showline(3);
  L[4] = _ny_PrelimFedReturn.fedline[10];
  showline(4);
  L[5] = _ny_PrelimFedReturn.fedline[11];
  showline(5);
  L[6] = _ny_PrelimFedReturn.fedline[12];
  showline(6);
  L[7] = _ny_PrelimFedReturn.fedline[13];
  showline(7);
  L[8] = _ny_PrelimFedReturn.fedline[14];
  showline(8);
  L[9] = _ny_PrelimFedReturn.fedline[15];
  showline(9);
  L[10] = _ny_PrelimFedReturn.fedline[16];
  showline(10);
  L[11] = _ny_PrelimFedReturn.fedline[17];
  showline(11);
  L[13] = _ny_PrelimFedReturn.fedline[18];
  showline(13);
  L[14] = _ny_PrelimFedReturn.fedline[19];
  showline(14);
  L[15] = _ny_PrelimFedReturn.fedline[20];
  showline(15);
  L[27] = L[15];
  L[16] = _ny_PrelimFedReturn.fedline[21];
  showline(16);
  for (j = 1; j <= 11; j++)
    L[17] = L[17] + L[j];

  for (j = 13; j <= 16; j++)
    L[17] = L[17] + L[j];

  showline(17);
  if (absolutev(L[17] - _ny_PrelimFedReturn.fedline[22]) > 0.1)
  {
;    fprintf(outfile, " Warning: L[17] = %6.2f, while Fed-line[22] = %6.2f\n", L[17], _ny_PrelimFedReturn.fedline[22]);
  }

  L[18] = _ny_PrelimFedReturn.fedline[36];
  showline(18);
  L[19] = L[17] - L[18];
  showline_wmsg(19, "Federal adjusted gross income");
  if (absolutev(L[19] - _ny_PrelimFedReturn.fedline[38]) > 0.1)
  {
;    fprintf(outfile, " Warning: L[19] = %6.2f, while Fed-line[38] = %6.2f\n", L[19], _ny_PrelimFedReturn.fedline[38]);
  }

  GetLineF("L20", &L[20]);
  GetLineF("L21", &L[21]);
  GetLineF("L22", &L[22]);
  GetLineF("L23", &L[23]);
  for (j = 19; j <= 23; j++)
    L[24] = L[24] + L[j];

  showline(24);
  L[25] = L[4];
  showline(25);
  GetLineF("L26", &L[26]);
  L[27] = L[15];
  showline(27);
  GetLineF("L28", &L[28]);
  GetLine("L29", &L[29]);
  if (L[29] > 20000.0)
  {
    L[29] = 20000.0;
    showline_wmsg(29, "(Limited to 20,000.)");
  }
  else
    showline(29);

  GetLine("L30", &L[30]);
  if (_ny_status == 2)
    L[30] = smallerof(L[30], 10000.0);
  else
    L[30] = smallerof(L[30], 5000.0);

  showline(30);
  GetLineF("L31", &L[31]);
  for (j = 25; j <= 31; j++)
    L[32] = L[32] + L[j];

  showline(32);
  L[33] = L[24] - L[32];
  showline_wmsg(33, "New York adjusted gross income (AGI)");
  GetLine("LTcare%", &LTC);
  GetLine("AddAdj", &AddAdj);
  GetLine("CollegeDed", &CollegeDed);
  ded_sched[1] = _ny_PrelimFedReturn.schedA[4];
  ded_sched[2] = _ny_PrelimFedReturn.schedA[9];
  ded_sched[3] = _ny_PrelimFedReturn.schedA[15];
  ded_sched[4] = _ny_PrelimFedReturn.schedA[19];
  ded_sched[5] = _ny_PrelimFedReturn.schedA[20];
  ded_sched[6] = _ny_PrelimFedReturn.schedA[27];
  ded_sched[7] = _ny_PrelimFedReturn.schedA[28];
  ded_sched[8] = _ny_PrelimFedReturn.schedA[29];
  ded_sched[9] = (_ny_PrelimFedReturn.schedA[5] + _ny_PrelimFedReturn.schedA[8]) + (LTC * _ny_PrelimFedReturn.schedA[4]);
  ded_sched[10] = ded_sched[8] - ded_sched[9];
  ded_sched[11] = AddAdj;
  ded_sched[12] = ded_sched[10] + ded_sched[11];
  if (L[33] <= 100000.0)
    ded_sched[13] = 0.0;
  else
  {
    double ws[50];
    if (L[33] <= 475000.0)
    {
      ws[1] = L[33];
      switch (_ny_status)
      {
        case 1:

        case 3:
          ws[2] = 100000.0;
          break;

        case 4:
          ws[2] = 150000.0;
          break;

        case 2:

        case 5:
          ws[2] = 200000.0;
          break;

        default:
          ws[2] = 0.0;

      }

      ws[3] = ws[1] - ws[2];
      if (ws[3] < 0.0)
        ded_sched[13] = 0.0;
      else
      {
        ws[4] = smallerof(ws[3], 50000.0);
        ws[5] = 0.0001 * Round(10000.0 * (ws[4] / 50000));
        ws[6] = 0.25 * ded_sched[1];
        ws[7] = ws[5] * ws[6];
        ded_sched[13] = ws[7];
      }

    }
    else
      if (L[33] <= 525000.0)
    {
      ws[1] = L[33] - 475000.0;
      ws[2] = 0.0001 * Round(10000.0 * (ws[1] / 50000));
      ws[3] = 0.25 * ded_sched[1];
      ws[4] = ws[2] * ws[3];
      ws[5] = ws[3] + ws[4];
      ded_sched[13] = ws[5];
    }
    else
      if (L[33] <= 1000000.0)
      ded_sched[13] = 0.5 * ded_sched[12];
    else
      if (L[33] <= 10000000.0)
    {
      ws[1] = L[33];
      ws[2] = 0.5 * ded_sched[4];
      ws[3] = ws[1] - ws[2];
      ded_sched[13] = ws[3];
    }
    else
    {
      ws[1] = L[33];
      ws[2] = 0.25 * ded_sched[4];
      ws[3] = ws[1] - ws[2];
      ded_sched[13] = ws[3];
    }




  }

  ded_sched[14] = ded_sched[12] - ded_sched[13];
  ded_sched[15] = CollegeDed;
  ded_sched[16] = ded_sched[14] + ded_sched[15];
  itemized_ded = ded_sched[16];
  switch (_ny_status)
  {
    case 1:
      if (Dependent)
      std_ded = 3100.0;
    else
      std_ded = 8000.0;

      break;

    case 2:
      std_ded = 16050.0;
      break;

    case 3:
      std_ded = 8000.0;
      break;

    case 4:
      std_ded = 11200.0;
      break;

    case 5:
      std_ded = 16050.0;
      break;

  }

  if (std_ded > itemized_ded)
  {
    L[34] = std_ded;
    fprintf(outfile, "Check_Std = X\n");
    showline_wmsg(34, "(Mark Std-deduction)");
  }
  else
  {
    L[34] = itemized_ded;
    fprintf(outfile, "Check_Item = X\n");
    showline_wmsg(34, "(Mark Itemized-deduction)");
  }

  L[35] = L[33] - L[34];
  if (L[35] < 0.0)
    L[35] = 0.0;
  else
    showline(35);

  get_parameter(infile, 's', word, "L36");
  get_parameters(infile, 'i', &k, "L36");
  L[36] = 1000.0 * ((double) k);
  showline(36);
  if (k > 0)
    fprintf(outfile, "L36_enter %d\n", k);

  L[37] = L[35] - L[36];
  if (L[37] < 0.0)
    L[37] = 0.0;

  showline_wmsg(37, "taxable income");
  L[38] = L[37];
  showline(38);
  if (L[33] <= 106950.0)
    L[39] = _ny_TaxRateLookup(L[38], _ny_status);
  else
    _ny_tax_computation_worksheet(_ny_status);

  showline(39);
  _ny_Report_bracket_info(L[38], L[39], _ny_status);
  get_parameter(infile, 's', word, "Exemptions");
  get_parameter(infile, 'i', &Exemptions, "Exemptions");
  if (Dependent)
    L[40] = 0.0;
  else
    if (_ny_status == 1)
  {
    if (L[19] < 5000.0)
      L[40] = 75.0;
    else
      if (L[19] < 6000.0)
      L[40] = 60.0;
    else
      if (L[19] < 7000.0)
      L[40] = 50.0;
    else
      if (L[19] < 20000.0)
      L[40] = 45.0;
    else
      if (L[19] < 25000.0)
      L[40] = 40.0;
    else
      if (L[19] < 28000.0)
      L[40] = 20.0;
    else
      L[40] = 0.0;






  }
  else
    if (_ny_status != 3)
  {
    if (L[19] < 5000.0)
      L[40] = 90.0 + (15.0 * (Exemptions - 1));
    else
      if (L[19] < 6000.0)
      L[40] = 75.0 + (15.0 * (Exemptions - 1));
    else
      if (L[19] < 7000.0)
      L[40] = 65.0 + (15.0 * (Exemptions - 1));
    else
      if (L[19] < 20000.0)
      L[40] = 60.0 + (15.0 * (Exemptions - 1));
    else
      if (L[19] < 22000.0)
      L[40] = 60.0 + (10.0 * (Exemptions - 1));
    else
      if (L[19] < 25000.0)
      L[40] = 50.0 + (10.0 * (Exemptions - 1));
    else
      if (L[19] < 28000.0)
      L[40] = 40.0 + (5.0 * (Exemptions - 1));
    else
      if (L[19] < 32000.0)
      L[40] = 20.0 + (5.0 * (Exemptions - 1));
    else
      L[40] = 0.0;








  }
  else
  {
    if (L[19] < 5000.0)
      L[40] = 45.0 + (8.0 * (Exemptions - 1));
    else
      if (L[19] < 6000.0)
      L[40] = 37.5 + (8.0 * (Exemptions - 1));
    else
      if (L[19] < 7000.0)
      L[40] = 32.5 + (8.0 * (Exemptions - 1));
    else
      if (L[19] < 20000.0)
      L[40] = 30.0 + (8.0 * (Exemptions - 1));
    else
      if (L[19] < 22000.0)
      L[40] = 30.0 + (5.0 * (Exemptions - 1));
    else
      if (L[19] < 25000.0)
      L[40] = 25.0 + (5.0 * (Exemptions - 1));
    else
      if (L[19] < 28000.0)
      L[40] = 20.0 + (3.0 * (Exemptions - 1));
    else
      if (L[19] < 32000.0)
      L[40] = 10.0 + (3.0 * (Exemptions - 1));
    else
      L[40] = 0.0;








  }



  showline_wmsg(40, "NY state household credit");
  GetLineF("L41", &L[41]);
  GetLineF("L42", &L[42]);
  L[43] = (L[40] + L[41]) + L[42];
  showline(43);
  L[44] = L[39] - L[43];
  if (L[44] < 0.0)
    L[44] = 0.0;
  else
    showline(44);

  GetLineF("L45", &L[45]);
  L[46] = L[44] + L[45];
  showline_wmsg(46, "Total New York State taxes");
  get_parameter(infile, 's', word, "NYC_Resident");
  get_parameters(infile, 'b', &nyc_resident, "NYC_Resident (yes/no) ?");
  GetLine("L50", &L[50]);
  GetLine("L51", &L[51]);
  GetLine("L53", &L[53]);
  if (nyc_resident)
  {
    L[47] = _ny_NYcityTaxRateFunction(L[38], _ny_status);
    showline(47);
    if (Dependent)
      L[48] = 0.0;
    else
      if (_ny_status == 1)
    {
      if (L[19] < 10000.0)
        L[48] = 15.0;
      else
        if (L[19] < 12500.0)
        L[48] = 10.0;
      else
        L[48] = 0.0;


    }
    else
      if (_ny_status != 3)
    {
      if (L[19] < 15000.0)
        L[48] = 30.0 * Exemptions;
      else
        if (L[19] < 17500.0)
        L[48] = 25.0 * Exemptions;
      else
        if (L[19] < 20000.0)
        L[48] = 15.0 * Exemptions;
      else
        if (L[19] < 22500.0)
        L[48] = 10.0 * Exemptions;
      else
        L[48] = 0.0;




    }
    else
    {
      if (L[19] < 15000.0)
        L[48] = 15.0 * Exemptions;
      else
        if (L[19] < 17500.0)
        L[48] = 13.0 * Exemptions;
      else
        if (L[19] < 20000.0)
        L[48] = 8.0 * Exemptions;
      else
        if (L[19] < 22500.0)
        L[48] = 5.0 * Exemptions;
      else
        L[48] = 0.0;




    }



    showline_wmsg(48, "NY City household credit");
    L[49] = L[47] - L[48];
    if (L[49] > 0.0)
      showline(49);
    else
      L[49] = 0.0;

    showline(50);
    showline(51);
    L[52] = (L[49] + L[50]) + L[51];
    showline(52);
    showline(53);
    L[54] = L[52] - L[53];
    if (L[54] > 0.0)
      showline(54);
    else
      L[54] = 0.0;

    L[58] = ((L[54] + L[55]) + L[56]) + L[57];
    showline_wmsg(58, "NYC taxes");
  }

  GetLineF("L59", &L[59]);
  GetLineF("L60", &L[60]);
  L[61] = ((L[46] + L[58]) + L[59]) + L[60];
  showline(61);
  L[62] = L[61];
  showline(62);
  GetLineF("L63", &L[63]);
  GetLineF("L64", &L[64]);
  GetLineF("L65", &L[65]);
  GetLineF("L66", &L[66]);
  GetLineF("L67", &L[67]);
  GetLineF("L68", &L[68]);
  if (nyc_resident)
  {
    if (Dependent)
      L[69] = 0.0;
    else
      if (L[37] < 250000)
    {
      if (((_ny_status == 1) || (_ny_status == 3)) || (_ny_status == 4))
        L[69] = 63.0;
      else
        if ((_ny_status == 2) || (_ny_status == 5))
        L[69] = 125.0;


    }
    else
      L[69] = 0.0;


    showline(69);
  }

  GetLineF("L71", &L[71]);
  GetLineF("L72", &L[72]);
  GetLineF("L73", &L[73]);
  GetLineF("L74", &L[74]);
  GetLineF("L75", &L[75]);
  for (j = 63; j <= 75; j++)
    L[76] = L[76] + L[j];

  showline(76);
  if (L[76] > L[62])
  {
    L[77] = L[76] - L[62];
    fprintf(outfile, "L77 = %6.2f	REFUND !!!\n", L[77]);
    L[78] = L[77];
    showline(78);
  }
  else
  {
    L[80] = L[62] - L[76];
    fprintf(outfile, "L80 = %6.2f	DUE !!!\n", L[80]);
    fprintf(outfile, "         (Which is %2.1f%% of your total tax.)\n", (100.0 * L[80]) / (L[62] + 1e-9));
  }

  if (_ny_Your1stName)
    fprintf(outfile, "Your1stName: %s\n", _ny_Your1stName);

  if (_ny_YourInitial)
    fprintf(outfile, "YourInitial: %s\n", _ny_YourInitial);

  if (_ny_YourLastName)
    fprintf(outfile, "YourLastName: %s\n", _ny_YourLastName);

  if (_ny_YourSocSec)
    fprintf(outfile, "YourSocSec#: %s\n", _ny_YourSocSec);

  if (_ny_Spouse1stName)
    fprintf(outfile, "Spouse1stName: %s\n", _ny_Spouse1stName);

  if (_ny_SpouseInitial)
    fprintf(outfile, "SpouseInitial: %s\n", _ny_SpouseInitial);

  if (_ny_SpouseLastName)
    fprintf(outfile, "SpouseLastName: %s\n", _ny_SpouseLastName);

  if (_ny_SpouseSocSec)
    fprintf(outfile, "SpouseSocSec#: %s\n", _ny_SpouseSocSec);

  if (_ny_MailAddress)
    fprintf(outfile, "Number&Street: %s\n", _ny_MailAddress);

  if (_ny_AptNumber)
    fprintf(outfile, "Apt#: %s\n", _ny_AptNumber);

  if (_ny_Town[0] != '\0')
    fprintf(outfile, "Town: %s\n", _ny_Town);

  if (_ny_StateName[0] != '\0')
    fprintf(outfile, "StateName: %s\n", _ny_StateName);

  if (_ny_Zipcode[0] != '\0')
    fprintf(outfile, "Zipcode: %s\n", _ny_Zipcode);

  if (strlen(_ny_YourLastName) > 0)
  {
    strcpy(YourNames, _ny_YourLastName);
    strcat(YourNames, ", ");
    strcat(YourNames, _ny_Your1stName);
    if (_ny_YourInitial[0] != '\0')
    {
      strcat(YourNames, ", ");
      strcat(YourNames, _ny_YourInitial);
    }

    if (_ny_Spouse1stName[0] != '\0')
    {
      strcat(YourNames, ", ");
      if ((_ny_SpouseLastName[0] != '\0') && (strcmp(_ny_YourLastName, _ny_SpouseLastName) != 0))
      {
        strcat(YourNames, _ny_SpouseLastName);
        strcat(YourNames, ", ");
      }

      strcat(YourNames, _ny_Spouse1stName);
      if (_ny_SpouseInitial[0] != '\0')
      {
        strcat(YourNames, ", ");
        strcat(YourNames, _ny_SpouseInitial);
      }

    }

    fprintf(outfile, "YourNames: %s\n", YourNames);
  }

  fclose(infile);
  fclose(outfile);
;  Display_File(outfname);
  return 0;
}


/* END of taxsolve_NY_IT201_2017.c */
/* START of taxsolve_OH_IT1040_2017.c */
double _oh_thisversion = 15.00;
double _oh_TaxRateFunction(double x, int status)
{
  if (x <= 10650.0)
    return 0.0;
  else
    if (x < 16000.0)
    return 79.08 + ((x - 10650.0) * 0.01980);
  else
    if (x < 21350.0)
    return 185.01 + ((x - 16000.0) * 0.02476);
  else
    if (x < 42650.0)
    return 317.48 + ((x - 21350.0) * 0.02969);
  else
    if (x < 85300.0)
    return 949.88 + ((x - 42650.0) * 0.03465);
  else
    if (x < 106650.0)
    return 2427.70 + ((x - 85300.0) * 0.03960);
  else
    if (x < 213350.0)
    return 3273.16 + ((x - 106650.0) * 0.04597);
  else
    return 8178.16 + ((x - 213350.0) * 0.04997);







}

void _oh_Report_bracket_info(double income, double tx, int status)
{
  double rate;
  if (income <= 10650.0)
    rate = 0.00990;
  else
    if (income < 16000.0)
    rate = 0.01980;
  else
    if (income < 21350.0)
    rate = 0.02476;
  else
    if (income < 42650.0)
    rate = 0.02969;
  else
    if (income < 85300.0)
    rate = 0.03465;
  else
    if (income < 106650.0)
    rate = 0.03960;
  else
    if (income < 213350.0)
    rate = 0.04597;
  else
    rate = 0.04997;







;  fprintf(outfile, " You are in the %2.1f%% marginal tax bracket,\n and you are paying an effective %2.1f%% tax on your total income.\n", 100.0 * rate, (100.0 * tx) / income);
}

char *_oh_pull_initial(char *name)
{
  int j = 0;
  char midinitial[10];
  while ((name[j] != '\0') && (name[j] != ','))
    j++;

  if (name[j] == ',')
  {
    name[j++] = '\0';
    while ((name[j] != '\0') && isspace(name[j]))
      j++;

    midinitial[0] = name[j];
    midinitial[1] = '\0';
  }
  else
    strcpy(midinitial, "");

  return strdup(midinitial);
}

int _oh_main(int argc, char *argv[])
{
  int j;
  int k;
  int mm;
  char word[1000];
  char outfname[1000];
  char label[50];
  char *socsec;
  char *pname;
  char *MidInit;
  int status = 0;
  int exemptions = 0;
  int qualify_jfc = 0;
  time_t now;
  double factorA;
  double factorB;
  double L2a;
  double L2b;
  double L7a;
  double L8a;
  double L8b;
  double L8c;
  double jfc;
  double exemption_amnt;
  double SchedA[1000];
  double SchedC[1000];
;  mm = 1;
  k = 1;
  while (mm < argc)
  {
    if (strcmp(argv[mm], "-verbose") == 0)
      verbose = 1;
    else
      if (k == 1)
    {
      infile = fopen(argv[mm], "r");
      if (infile == 0)
      {
;        exit(1);
      }

      k = 2;
      strcpy(outfname, argv[mm]);
      j = strlen(outfname) - 1;
      while ((j >= 0) && (outfname[j] != '.'))
        j--;

      if (j < 0)
        strcat(outfname, "_out.txt");
      else
        strcpy(&outfname[j], "_out.txt");

      outfile = fopen(outfname, "w");
      if (outfile == 0)
      {
;        exit(1);
      }

;    }
    else
    {
;      exit(1);
    }


    mm++;
  }

  if (infile == 0)
  {
;    exit(1);
  }

  for (mm = 0; mm < 1000; mm++)
  {
    L[mm] = 0.0;
    SchedA[mm] = 0.0;
    SchedC[mm] = 0.0;
  }

  read_line(infile, word);
  now = time(0);
  fprintf(outfile, "\n%s,	 v%2.2f, %s\n", word, _oh_thisversion, ctime(&now));
  get_parameter(infile, 's', word, "Status");
  get_parameter(infile, 'l', word, "Status ?");
  if (strncasecmp(word, "Single", 4) == 0)
    status = 1;
  else
    if (strncasecmp(word, "Married/Joint", 11) == 0)
    status = 2;
  else
    if (strncasecmp(word, "Married/Sep", 11) == 0)
    status = 3;
  else
    if (strncasecmp(word, "Head_of_House", 4) == 0)
    status = 1;
  else
  {
;    fprintf(outfile, "Error: unrecognized status '%s'. Must be: Single, Married/joint, Married/sep, Head_of_house.\nExiting.\n", word);
    exit(1);
  }




  fprintf(outfile, "Status = %s (%d)\n", word, status);
  get_parameter(infile, 's', word, "Exemptions");
  get_parameters(infile, 'i', &exemptions, "Exemptions");
  get_parameter(infile, 's', word, "JointCredit");
  get_parameter(infile, 'b', &qualify_jfc, "JointCredit ?");
  GetLine("L1", &L[1]);
  GetLine("L4", &L[4]);
  GetLine("L6", &L[6]);
  GetLine("L8b", &L8b);
  GetLine("L11", &L[11]);
  GetLine("L12", &L[12]);
  GetLine("L14", &L[14]);
  GetLine("L15", &L[15]);
  GetLine("L17", &L[17]);
  GetLine("L19", &L[19]);
  GetLine("SchedA_1", &SchedA[1]);
  GetLine("SchedA_2", &SchedA[2]);
  GetLine("SchedA_3", &SchedA[3]);
  GetLine("SchedA_4", &SchedA[4]);
  GetLine("SchedA_5", &SchedA[5]);
  GetLine("SchedA_6", &SchedA[6]);
  GetLine("SchedA_7", &SchedA[7]);
  GetLine("SchedA_8", &SchedA[8]);
  GetLine("SchedA_9", &SchedA[9]);
  GetLine("SchedA_11", &SchedA[11]);
  GetLine("SchedA_12", &SchedA[12]);
  GetLine("SchedA_13", &SchedA[13]);
  GetLine("SchedA_14", &SchedA[14]);
  GetLine("SchedA_15", &SchedA[15]);
  GetLine("SchedA_16", &SchedA[16]);
  GetLine("SchedA_17", &SchedA[17]);
  GetLine("SchedA_18", &SchedA[18]);
  GetLine("SchedA_19", &SchedA[19]);
  GetLine("SchedA_20", &SchedA[20]);
  GetLine("SchedA_21", &SchedA[21]);
  GetLine("SchedA_22", &SchedA[22]);
  GetLine("SchedA_23", &SchedA[23]);
  GetLine("SchedA_24", &SchedA[24]);
  GetLine("SchedA_25", &SchedA[25]);
  GetLine("SchedA_26", &SchedA[26]);
  GetLine("SchedA_27", &SchedA[27]);
  GetLine("SchedA_28", &SchedA[28]);
  GetLine("SchedA_29", &SchedA[29]);
  GetLine("SchedA_30", &SchedA[30]);
  GetLine("SchedA_31", &SchedA[31]);
  GetLine("SchedA_32", &SchedA[32]);
  GetLine("SchedA_33", &SchedA[33]);
  GetLine("SchedA_34", &SchedA[34]);
  GetLine("Credits_2", &SchedC[2]);
  SchedC[2] = smallerof(SchedC[2], 200.0);
  GetLine("Credits_3", &SchedC[3]);
  GetLine("Credits_4", &SchedC[4]);
  SchedC[4] = smallerof(SchedC[4], 50.0);
  GetLine("Credits_5", &SchedC[5]);
  GetLine("Credits_6", &SchedC[6]);
  GetLine("Credits_7", &SchedC[7]);
  GetLine("Credits_8", &SchedC[8]);
  if (status == 2)
  {
    SchedC[7] = smallerof(SchedC[7], 1000.0);
    SchedC[8] = smallerof(SchedC[8], 100.0);
  }
  else
  {
    SchedC[7] = smallerof(SchedC[7], 500.0);
    SchedC[8] = smallerof(SchedC[8], 50.0);
  }

  GetLine("Credits_13", &SchedC[13]);
  GetLine("Credits_14", &SchedC[14]);
  GetLine("Credits_15", &SchedC[15]);
  GetLine("Credits_16", &SchedC[16]);
  GetLine("Credits_17", &SchedC[17]);
  GetLine("Credits_18", &SchedC[18]);
  GetLine("Credits_19", &SchedC[19]);
  GetLine("Credits_20", &SchedC[20]);
  GetLine("Credits_21", &SchedC[21]);
  GetLine("Credits_22", &SchedC[22]);
  GetLine("Credits_25", &SchedC[25]);
  GetLine("Credits_28", &SchedC[28]);
  GetLine("Credits_31", &SchedC[31]);
  GetLine("Credits_34", &SchedC[34]);
  GetLine("Credits_35", &SchedC[35]);
  GetLine("Credits_36", &SchedC[36]);
  GetLine("Credits_37", &SchedC[37]);
  GetLine("Credits_38", &SchedC[38]);
  GetLine("Credits_39", &SchedC[39]);
  for (j = 1; j <= 9; j++)
    SchedA[10] = SchedA[10] + SchedA[j];

  for (j = 11; j <= 34; j++)
    SchedA[35] = SchedA[35] + SchedA[j];

  L2a = SchedA[10];
  L2b = SchedA[35];
  L[3] = (L[1] + L2a) + L2b;
  if (L[3] <= 40000.0)
    exemption_amnt = 2300.0;
  else
    if (L[3] <= 80000.0)
    exemption_amnt = 2050.0;
  else
    exemption_amnt = 1800.0;


  L[4] = exemption_amnt * exemptions;
  L[5] = NotLessThanZero(L[3] - L[4]);
  L[7] = NotLessThanZero(L[5] - L[6]);
  L7a = L[7];
  L8a = _oh_TaxRateFunction(L7a, status);
  L8c = L8a + L8b;
  SchedC[1] = L8c;
  if (L[5] < 30000.0)
    SchedC[9] = 20.0 * exemptions;

  for (j = 2; j <= 9; j++)
    SchedC[10] = SchedC[10] + SchedC[j];

  SchedC[11] = NotLessThanZero(SchedC[1] - SchedC[10]);
  if ((status == 2) && qualify_jfc)
  {
    if (L[5] < 25000)
      jfc = 0.20;
    else
      if (L[5] < 50000)
      jfc = 0.15;
    else
      if (L[5] < 75000)
      jfc = 0.10;
    else
      jfc = 0.05;



    SchedC[12] = smallerof(jfc * L[11], 650.0);
  }

  for (j = 12; j <= 22; j++)
    SchedC[23] = SchedC[23] + SchedC[j];

  SchedC[24] = NotLessThanZero(SchedC[11] - SchedC[23]);
  SchedC[26] = L[3];
  j = (10000.0 * SchedC[25]) / SchedC[26];
  factorA = ((double) j) / 10000.0;
  SchedC[27] = SchedC[24] * factorA;
  SchedC[29] = L[3];
  j = (10000.0 * SchedC[28]) / SchedC[29];
  factorB = ((double) j) / 10000.0;
  SchedC[30] = SchedC[24] * factorB;
  SchedC[32] = smallerof(SchedC[30], SchedC[31]);
  SchedC[33] = ((SchedC[10] + SchedC[23]) + SchedC[27]) + SchedC[32];
  L[9] = SchedC[33];
  for (j = 34; j <= 39; j++)
    SchedC[40] = SchedC[40] + SchedC[j];

  L[16] = SchedC[40];
  L[10] = NotLessThanZero(L8c - L[9]);
  L[13] = (L[10] + L[11]) + L[12];
  L[18] = ((L[14] + L[15]) + L[16]) + L[17];
  L[20] = L[18] - L[19];
  if (L[13] >= L[20])
  {
    L[21] = L[13] - L[20];
    L[23] = L[21] + L[22];
  }
  else
  {
    L[24] = L[20] - L[13];
    L[27] = L[24];
  }

  if ((L[1] < 12950.0) && (L[3] < 0.0))
    fprintf(outfile, "You do not need to file Ohio tax return (Fed AGI < minimum).\n");

  if ((L[1] < 12950.0) && (L[4] >= L[3]))
    fprintf(outfile, "You do not need to file Ohio tax return (L[4] >= L[3]).\n");

  showline(1);
  showline_wlabel("L2a", L2a);
  showline_wlabel("L2b", L2b);
  showline(3);
  showline(4);
  fprintf(outfile, " Exemptions = %d\n", exemptions);
  showline(5);
  showline(6);
  showline(7);
  showline_wlabel("L7a", L[7]);
  showline_wlabel("L8a", L8a);
  showline_wlabel("L8b", L8b);
  showline_wlabel("L8c", L8c);
  for (j = 9; j <= 12; j++)
    showline(j);

  showline_wmsg(13, "Total Ohio tax liability");
  _oh_Report_bracket_info(L[7], L[13], status);
  showline_wmsg(14, "Ohio income tax withheld");
  for (j = 15; j <= 17; j++)
    showline(j);

  showline_wmsg(18, "Total Ohio tax payments");
  for (j = 19; j <= 20; j++)
    showline(j);

  if (L[13] >= L[20])
  {
    showline(21);
    showline(22);
    showline_wmsg(23, "TOTAL AMOUNT DUE !!!");
    fprintf(outfile, "         (Which is %2.1f%% of your total tax.)\n", (100.0 * L[23]) / (L[13] + 1e-9));
  }
  else
  {
    showline_wmsg(24, "Overpayment");
    showline_wmsg(27, "YOUR REFUND !!!");
  }

  fprintf(outfile, "\n-- 2017 Ohio Schedule A --\n");
  for (j = 1; j <= 35; j++)
  {
    sprintf(label, "SchedA%d", j);
    showline_wlabel(label, SchedA[j]);
  }

  fprintf(outfile, "\n-- 2017 Ohio Schedule of Credits --\n");
  for (j = 1; j <= 27; j++)
  {
    sprintf(label, "Credits%d", j);
    showline_wlabel(label, SchedC[j]);
  }

  sprintf(word, "%5.4f", factorA);
;  fprintf(outfile, "   Credits27_Factor %s\n", &word[2]);
  showline_wlabel("Credits28", SchedC[28]);
  showline_wlabel("Credits29", SchedC[29]);
  showline_wlabel("Credits30", SchedC[30]);
  sprintf(word, "%5.4f", factorB);
;  fprintf(outfile, "   Credits30_Factor %s\n", &word[2]);
  for (j = 31; j <= 41; j++)
  {
    sprintf(label, "Credits%d", j);
    showline_wlabel(label, SchedC[j]);
  }

  fprintf(outfile, "\n{ --------- }\n");
  pname = GetTextLine("Your1stName:");
  MidInit = _oh_pull_initial(pname);
  fprintf(outfile, "Your1stName: %s\n", pname);
  fprintf(outfile, "YourMidInit: %s\n", MidInit);
  GetTextLineF("YourLastName:");
  writeout_line = 0;
  socsec = GetTextLineF("YourSocSec#:");
  format_socsec(socsec, 0);
  fprintf(outfile, "YourSocSec#: %s\n", socsec);
  free(socsec);
  writeout_line = 1;
  pname = GetTextLine("Spouse1stName:");
  MidInit = _oh_pull_initial(pname);
  fprintf(outfile, "Spouse1stName: %s\n", pname);
  fprintf(outfile, "SpouseMidInit: %s\n", MidInit);
  GetTextLineF("SpouseLastName:");
  writeout_line = 0;
  socsec = GetTextLineF("SpouseSocSec#:");
  format_socsec(socsec, 0);
  fprintf(outfile, "SpouseSocSec#: %s\n", socsec);
  free(socsec);
  writeout_line = 1;
  GetTextLineF("Number&Street:");
  GetTextLineF("Town:");
  fprintf(outfile, "State: OH\n");
  GetTextLineF("Zipcode:");
  fprintf(outfile, "CkFYrRes: X\n");
  if (status == 2)
    fprintf(outfile, "CkFYrResSp: X\n");

  fclose(infile);
  fclose(outfile);
  Display_File(outfname);
;  return 0;
}


/* END of taxsolve_OH_IT1040_2017.c */
/* START of taxsolve_PA_40_2017.c */
double _pa_Tax_Rate = 0.0307;
double _pa_pos(double x)
{
  if (x > 0.0)
    return x;
  else
    return 0.0;

}

int _pa_main(int argc, char *argv[])
{
  int i;
  int j;
  int k;
  int status = 0;
  char word[2000];
  char outfname[1500];
  time_t now;
  double oneA;
  double oneB;
  char *Your1stName = 0;
  char *YourLastName = 0;
  char *Spouse1stName = 0;
  char *SpouseLastName;
  char *YourNames;
;  i = 1;
  k = 1;
  while (i < argc)
  {
    if (strcmp(argv[i], "-verbose") == 0)
    {
      verbose = 1;
    }
    else
      if (k == 1)
    {
      infile = fopen(argv[i], "r");
      if (infile == 0)
      {
;        exit(1);
      }

      k = 2;
      strcpy(outfname, argv[i]);
      j = strlen(outfname) - 1;
      while ((j >= 0) && (outfname[j] != '.'))
        j--;

      if (j < 0)
        strcat(outfname, "_out.txt");
      else
        strcpy(&outfname[j], "_out.txt");

      outfile = fopen(outfname, "w");
      if (outfile == 0)
      {
;        exit(1);
      }

;    }
    else
    {
;      exit(1);
    }


    i = i + 1;
  }

  if (infile == 0)
  {
;    exit(1);
  }

  for (i = 0; i < 1000; i++)
    L[i] = 0.0;

  read_line(infile, word);
  now = time(0);
  fprintf(outfile, "\n%s,	 v%2.2f, %s\n", word, 15.00, ctime(&now));
  get_parameter(infile, 's', word, "Status");
  get_parameter(infile, 'l', word, "Status?");
  if (strncasecmp(word, "Single", 4) == 0)
    status = 1;
  else
    if (strncasecmp(word, "Married/Joint", 13) == 0)
    status = 2;
  else
    if (strncasecmp(word, "Married/Sep", 11) == 0)
    status = 3;
  else
    if (strncasecmp(word, "Widow", 4) == 0)
    status = 1;
  else
  {
;    fprintf(outfile, "Error: unrecognized status '%s'. Exiting.\n", word);
    exit(1);
  }




  fprintf(outfile, "Status = %s (%d)\n", word, status);
  GetLineF("L1a", &oneA);
  GetLineF("L1b", &oneB);
  L[1] = oneA - oneB;
  fprintf(outfile, "L1c = %2.2f\n", L[1]);
  GetLineF("L2", &L[2]);
  GetLineF("L3", &L[3]);
  GetLine("L4", &L[4]);
  fprintf(outfile, "L4 = %6.2f\n", absolutev(L[4]));
  if (L[4] < 0.0)
    fprintf(outfile, " Check_4Loss X\n");

  GetLine("L5", &L[5]);
  fprintf(outfile, "L5 = %6.2f\n", absolutev(L[5]));
  if (L[5] < 0.0)
    fprintf(outfile, " Check_5Loss X\n");

  GetLine("L6", &L[6]);
  fprintf(outfile, "L6 = %6.2f\n", absolutev(L[6]));
  if (L[6] < 0.0)
    fprintf(outfile, " Check_6Loss X\n");

  GetLineF("L7", &L[7]);
  GetLineF("L8", &L[8]);
  for (j = 1; j <= 8; j++)
    if (L[j] < 0.0)
    L[j] = 0.0;


  L[9] = ((((((_pa_pos(L[1]) + _pa_pos(L[2])) + _pa_pos(L[3])) + _pa_pos(L[4])) + _pa_pos(L[5])) + _pa_pos(L[6])) + _pa_pos(L[7])) + _pa_pos(L[8]);
  showline_wmsg(9, "Total PA Taxable Income");
  GetLineF("L10", &L[10]);
  L[11] = L[9] - L[10];
  showline_wmsg(11, "Adjusted PA Taxable Income");
  L[12] = _pa_Tax_Rate * L[11];
  showline_wmsg(12, "PA Tax Liability");
  GetLine("L13", &L[13]);
  showline_wmsg(13, "Total PA tax withheld");
  GetLineF("L14", &L[14]);
  GetLineF("L15", &L[15]);
  GetLineF("L16", &L[16]);
  GetLineF("L17", &L[17]);
  L[18] = ((L[14] + L[15]) + L[16]) + L[17];
  showline_wmsg(18, "Total Estimated Payments and Credits");
  GetLine("L21", &L[21]);
  showline_wmsg(21, "Tax Back/Tax Foregiveness Credit");
  GetLineF("L22", &L[22]);
  GetLineF("L23", &L[23]);
  L[24] = (((L[13] + L[18]) + L[21]) + L[22]) + L[23];
  showline_wmsg(24, "Total Payments and Credits");
  GetLineF("L25", &L[25]);
  GetLine("L27", &L[27]);
  if ((L[12] + L[25]) > L[24])
  {
    L[26] = (L[12] + L[25]) - L[24];
    showline_wmsg(26, "TAX DUE");
    showline(27);
    L[28] = L[26] + L[27];
    if (L[28] > 0.0)
    {
      showline_wmsg(28, "Total Payment Due");
      fprintf(outfile, "         (Which is %2.1f%% of your total tax.)\n", (100.0 * L[28]) / ((L[12] + L[25]) + 1e-9));
    }

  }
  else
    if (L[24] > ((L[12] + L[25]) + L[27]))
  {
    showline(27);
    L[29] = L[24] - ((L[12] + L[25]) + L[27]);
    showline_wmsg(29, "OVERPAYMENT");
    L[30] = L[29];
    showline_wmsg(30, "REFUND");
  }


  fprintf(outfile, "\n{ --------- }\n");
  Your1stName = GetTextLineF("Your1stName:");
  GetTextLineF("MidInitial:");
  YourLastName = GetTextLineF("YourLastName:");
  GetTextLineF("YourSocSec#:");
  Spouse1stName = GetTextLineF("Spouse1stName:");
  GetTextLineF("SpouseMidInit:");
  SpouseLastName = GetTextLineF("SpouseLastName:");
  GetTextLineF("SpouseSocSec#:");
  GetTextLineF("Number&Street:");
  GetTextLineF("Town:");
  fprintf(outfile, "State: PA\n");
  GetTextLineF("Zipcode:");
  GetTextLineF("Phone:");
  GetTextLineF("SchoolCode:");
  GetTextLineF("SchooldDist:");
  GetTextLineF("YourOccupation:");
  GetTextLineF("SpouseOccupat:");
  if (YourLastName[0] != '\0')
  {
    if (status == 2)
    {
      YourNames = (char *) malloc((((strlen(YourLastName) + strlen(Your1stName)) + strlen(SpouseLastName)) + strlen(Spouse1stName)) + 20);
      strcpy(YourNames, Your1stName);
      if (strcmp(YourLastName, SpouseLastName) == 0)
      {
        strcat(YourNames, " & ");
        strcat(YourNames, Spouse1stName);
        strcat(YourNames, ", ");
        strcat(YourNames, YourLastName);
      }
      else
      {
        strcat(YourNames, " ");
        strcat(YourNames, YourLastName);
        strcat(YourNames, ", ");
        strcat(YourNames, Spouse1stName);
        strcat(YourNames, " ");
        strcat(YourNames, SpouseLastName);
      }

    }
    else
    {
      YourNames = (char *) malloc((strlen(YourLastName) + strlen(Your1stName)) + 10);
      strcpy(YourNames, Your1stName);
      strcat(YourNames, ", ");
      strcat(YourNames, YourLastName);
    }

    fprintf(outfile, "YourNames: %s\n", YourNames);
  }

  fclose(infile);
  fclose(outfile);
;  Display_File(outfname);
  return 0;
}


/* END of taxsolve_PA_40_2017.c */
/* START of taxsolve_VA_760_2017.c */
float _va_thisversion = 15.02;
double _va_TaxRateFunction(double income, int status)
{
  if (income < 3000.0)
    return income * 0.02;
  else
    if (income < 5000.0)
    return 60.0 + ((income - 3000.0) * 0.03);
  else
    if (income < 17000.0)
    return 120.0 + ((income - 5000.0) * 0.05);
  else
    return 720.0 + ((income - 17000.0) * 0.0575);



}

void _va_Report_bracket_info(double income, double tx, int status)
{
  double rate;
  if (income < 3000.0)
    rate = 0.02;
  else
    if (income < 5000.0)
    rate = 0.03;
  else
    if (income < 17000.0)
    rate = 0.05;
  else
    rate = 0.0575;



;  fprintf(outfile, " You are in the %2.1f%% marginal tax bracket,\n and you are paying an effective %2.1f%% tax on your total income.\n", 100.0 * rate, (100.0 * tx) / income);
}

struct date_record _va_yourDOB;
struct date_record _va_spouseDOB;
struct date_record _va_DL;
int _va_main(int argc, char *argv[])
{
  int i;
  int j;
  int k;
  char word[1000];
  char outfname[4000];
  char *lnameptr;
  char lastname[1024];
  char *socsec;
  char *datestr;
  char *twrd;
  int status = 0;
  int exemptionsA = 0;
  int exemptionsB = 0;
  int youBlind = 0;
  int spouseBlind = 0;
  time_t now;
  double L20b = 0.0;
  double std_ded = 0.0;
  double min2file;
;  i = 1;
  k = 1;
  while (i < argc)
  {
    if (strcmp(argv[i], "-verbose") == 0)
      verbose = 1;
    else
      if (k == 1)
    {
      infile = fopen(argv[i], "r");
      if (infile == 0)
      {
;        exit(1);
      }

      k = 2;
      strcpy(outfname, argv[i]);
      j = strlen(outfname) - 1;
      while ((j >= 0) && (outfname[j] != '.'))
        j--;

      if (j < 0)
        strcat(outfname, "_out.txt");
      else
        strcpy(&outfname[j], "_out.txt");

      outfile = fopen(outfname, "w");
      if (outfile == 0)
      {
;        exit(1);
      }

;    }
    else
    {
;      exit(1);
    }


    i = i + 1;
  }

  if (infile == 0)
  {
;    exit(1);
  }

  for (i = 0; i < 1000; i++)
  {
    L[i] = 0.0;
  }

  read_line(infile, word);
  now = time(0);
  fprintf(outfile, "\n%s,	 v%2.2f, %s\n", word, _va_thisversion, ctime(&now));
  get_parameter(infile, 's', word, "Status");
  get_parameter(infile, 'l', word, "Status ?");
  if (strncasecmp(word, "Single", 4) == 0)
    status = 1;
  else
    if (strncasecmp(word, "Married/Joint", 13) == 0)
    status = 2;
  else
    if (strncasecmp(word, "Married/Sep", 11) == 0)
    status = 3;
  else
    if (strncasecmp(word, "Head_of_House", 4) == 0)
    status = 4;
  else
  {
;    fprintf(outfile, "Error: unrecognized status '%s'. Must be: Single, Married/joint, Married/sep, Head_of_house, Widow(er)\nExiting.\n", word);
    exit(1);
  }




  fprintf(outfile, "Status = %s\n", word);
  fprintf(outfile, " FilingStatus: %d\n", status);
  GetTextLineF("Your1stName:");
  GetTextLineF("YourMI:");
  lnameptr = GetTextLineF("YourLastName:");
  strcpy(lastname, lnameptr);
  lastname[4] = '\0';
  fprintf(outfile, "Y1st4: %s\n", lastname);
  writeout_line = 0;
  socsec = GetTextLineF("YourSocSec#:");
  format_socsec(socsec, 1);
  fprintf(outfile, "YourSocSec#: %s\n", socsec);
  datestr = GetTextLineF("YourDOB:");
  if (datestr[0] == '\0')
  {
    show_errmsg("\nMissing 'YourDOB' -- needed to check age exemptions.\n");
    datestr = strdup("1 / 1 / 2000");
  }

  if (interpret_date(datestr, &_va_yourDOB.month, &_va_yourDOB.day, &_va_yourDOB.year, "Bad YourDOB") != 1)
    exit(1);

  twrd = format_mmddyyyy(_va_yourDOB.month, _va_yourDOB.day, _va_yourDOB.year);
  fprintf(outfile, "YourDOB: %s\n", twrd);
  writeout_line = 1;
  GetTextLineF("YourDrivLic:");
  writeout_line = 0;
  datestr = GetTextLineF("YourDLdate:");
  if ((datestr[0] != '\0') && interpret_date(datestr, &_va_DL.month, &_va_DL.day, &_va_DL.year, "Bad YourDL"))
  {
    twrd = format_mmddyyyy(_va_DL.month, _va_DL.day, _va_DL.year);
    fprintf(outfile, "YourDLdate: %s\n", twrd);
  }

  writeout_line = 1;
  GetTextLineF("Spouse1stName:");
  GetTextLineF("SpouseMI:");
  lnameptr = GetTextLineF("SpouseLastName:");
  strcpy(lastname, lnameptr);
  lastname[4] = '\0';
  fprintf(outfile, "S1st4: %s\n", lastname);
  writeout_line = 0;
  socsec = GetTextLineF("SpouseSocSec#:");
  format_socsec(socsec, 1);
  fprintf(outfile, "SpouseSocSec#: %s\n", socsec);
  datestr = GetTextLineF("SpouseDOB:");
  if (status == 2)
  {
    if (datestr[0] == '\0')
    {
      show_errmsg("\nMissing 'SpouseDOB' -- needed to check age exemptions.\n");
      datestr = strdup("1 / 1 / 2000");
    }

    if (interpret_date(datestr, &_va_spouseDOB.month, &_va_spouseDOB.day, &_va_spouseDOB.year, "Bad SpouseDOB") != 1)
      exit(1);

    twrd = format_mmddyyyy(_va_spouseDOB.month, _va_spouseDOB.day, _va_spouseDOB.year);
    fprintf(outfile, "SpouseDOB: %s\n", twrd);
  }

  writeout_line = 1;
  GetTextLineF("SpouseDrivLic:");
  writeout_line = 0;
  datestr = GetTextLineF("SpouseDLdate:");
  if ((datestr[0] != '\0') && interpret_date(datestr, &_va_DL.month, &_va_DL.day, &_va_DL.year, "Bad YourDL"))
  {
    twrd = format_mmddyyyy(_va_DL.month, _va_DL.day, _va_DL.year);
    fprintf(outfile, "SpouseDLdate: %s\n", twrd);
  }

  writeout_line = 1;
  GetTextLineF("Number&Street:");
  GetTextLineF("Town:");
  fprintf(outfile, "State: VA\n");
  GetTextLineF("Zipcode:");
  fprintf(outfile, "\n{ --------- }\n");
  get_parameter(infile, 's', word, "OtherDependents");
  get_parameters(infile, 'i', &exemptionsA, "OtherDependents");
  if (exemptionsA > 0)
    fprintf(outfile, "ExmpDeps: %d\n", exemptionsA);

  if (status == 2)
    exemptionsA = 2 + exemptionsA;
  else
    exemptionsA = 1 + exemptionsA;

  fprintf(outfile, "NExemptionsA = %d\n", exemptionsA);
  fprintf(outfile, "ExemptionsA = %d\n", 930 * exemptionsA);
  if (_va_yourDOB.year < 1953)
  {
    fprintf(outfile, "YouOver65 = 1\n");
    exemptionsB = 1;
  }
  else
    exemptionsB = 0;

  get_parameter(infile, 's', word, "YouBlind");
  get_parameter(infile, 'b', &youBlind, "YouBlind");
  if (youBlind != 0)
  {
    fprintf(outfile, "YouBlind = 1\n");
    exemptionsB++;
  }

  get_parameter(infile, 's', word, "SpouseBlind");
  get_parameter(infile, 'b', &spouseBlind, "SpouseBlind");
  if (status == 2)
  {
    if (_va_spouseDOB.year < 1953)
    {
      fprintf(outfile, "SpouseOver65 = 1\n");
      exemptionsB++;
    }

    if (spouseBlind != 0)
    {
      fprintf(outfile, "SpouseBlind = 1\n");
      exemptionsB++;
    }

  }

  fprintf(outfile, "NExemptionsB = %d\n", exemptionsB);
  fprintf(outfile, "ExemptionsB = %d\n", 800 * exemptionsB);
  if (status == 2)
  {
    fprintf(outfile, "ExmpSpouse:  1\n");
    if (exemptionsA > 2)
      fprintf(outfile, "ExmpDeps: %d\n", exemptionsA - 2);

  }
  else
    if (exemptionsA > 1)
    fprintf(outfile, "ExmpDeps: %d\n", exemptionsA - 1);


  GetLineF("L1", &L[1]);
  GetLineF("L2", &L[2]);
  L[3] = L[1] + L[2];
  showline(3);
  GetLineF("L4", &L[4]);
  GetLineF("L5", &L[5]);
  GetLineF("L6", &L[6]);
  GetLineF("L7", &L[7]);
  L[8] = ((L[4] + L[5]) + L[6]) + L[7];
  showline(8);
  L[9] = L[3] - L[8];
  showline(9);
  GetLineF("L10", &L[10]);
  GetLineF("L11", &L[11]);
  switch (status)
  {
    case 1:
      std_ded = 3000.0;
      min2file = 11950.0;
      break;

    case 2:
      std_ded = 6000.0;
      min2file = 23900.0;
      break;

    case 3:
      std_ded = 3000.0;
      min2file = 11950.0;
      break;

    default:
;      fprintf(outfile, "Unexpected status.\n");
      exit(1);
      break;

  }

  if (L[10] != 0.0)
    L[12] = L[10] - L[11];
  else
    L[12] = std_ded;

  showline(12);
  L[13] = (930.0 * exemptionsA) + (800.0 * exemptionsB);
  showline(13);
  GetLineF("L14", &L[14]);
  L[15] = (L[12] + L[13]) + L[14];
  showline(15);
  L[16] = L[9] - L[15];
  showline(16);
  L[17] = _va_TaxRateFunction(L[16], status);
  showline(17);
  _va_Report_bracket_info(L[16], L[17], status);
  GetLine("L18", &L[18]);
  showline(18);
  L[19] = L[17] - L[18];
  showline_wmsg(19, "Net Amount of Tax");
  GetLineF("L20a", &L[20]);
  GetLineF("L20b", &L20b);
  GetLineF("L21", &L[21]);
  GetLineF("L22", &L[22]);
  GetLineF("L23", &L[23]);
  GetLine("L24", &L[24]);
  if (L[24] > L[19])
    L[24] = L[19];

  if ((L[24] > 0.0) && (exemptionsB > 0.0))
  {
    fprintf(outfile, " Cannot claim both Low-Income Credit and Age or Blind Exemptions.\n");
    L[24] = 0.0;
  }

  showline(24);
  GetLineF("L25", &L[25]);
  GetLineF("L26", &L[26]);
  GetLineF("L27", &L[27]);
  L[28] = (((((((L[20] + L20b) + L[21]) + L[22]) + L[23]) + L[24]) + L[25]) + L[26]) + L[27];
  showline(28);
  if (L[28] < L[19])
  {
    L[29] = L[19] - L[28];
    showline_wmsg(29, "Tax You Owe");
  }
  else
  {
    L[30] = L[28] - L[19];
    showline_wmsg(30, "Your Tax OverPayment");
  }

  GetLineF("L31", &L[31]);
  GetLineF("L32", &L[32]);
  GetLineF("L33", &L[33]);
  GetLineF("L34", &L[34]);
  GetLineF("L35", &L[35]);
  for (j = 31; j < 35; j++)
    L[36] = L[36] + L[j];

  showline(36);
  if (L[29] > 0.0)
  {
    L[37] = L[29] + L[36];
    showline_wmsg(37, "AMOUNT DUE");
    fprintf(outfile, "         (Which is %2.1f%% of your total tax.)\n", (100.0 * L[37]) / (L[19] + 1e-9));
  }
  else
    if (L[30] < L[36])
  {
    L[37] = L[36] - L[30];
    showline_wmsg(37, "AMOUNT DUE");
    fprintf(outfile, "         (Which is %2.1f%% of your total tax.)\n", (100.0 * L[37]) / (L[19] + 1e-9));
  }
  else
    if (L[30] > L[36])
  {
    L[38] = L[30] - L[36];
    showline_wmsg(38, "YOUR REFUND");
  }



  if (L[9] < min2file)
  {
    fprintf(outfile, "\nYour VAGI is less than the minimum required to file a return.\n");
    if (((L[20] + L20b) + L[21]) > 0.0)
      fprintf(outfile, " But you need to file return to receive refund of withheld taxes.\n");
    else
      fprintf(outfile, "You do not need to file return.  Your VA Tax is zero.\n");

  }

  fclose(infile);
  fclose(outfile);
  Display_File(outfname);
;  return 0;
}


/* END of taxsolve_VA_760_2017.c */
