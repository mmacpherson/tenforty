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
int single_line_entry = 0;
int whole_line_entry = 0;
void show_errmsg(char *emsg)
{
  printf("%s\n", emsg);
  if (outfile != 0)
    fprintf(outfile, "%s\n", emsg);

}

void get_word(FILE *infile, char *word)
{
  int j = 0;
  char ltc = '\n';
  char spc = ' ';
  if (single_line_entry)
    ltc = ' ';

  if (whole_line_entry)
    spc = '\n';

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
  while ((!feof(infile)) && ((((word[j] == ' ') || (word[j] == '\t')) || (word[j] == ltc)) || (word[j] == '\r')));
  if (word[j] == '$')
    word[j] = getc(infile);

  if (word[j] == ';')
    j++;
  else
    if (word[j] == '\n')
    word[j] = '\0';
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
    while ((!feof(infile)) && ((((word[j] != spc) && (word[j] != '\t')) && (word[j] != '\n')) && (word[j] != ';')));
    if (word[j] == ';')
      ungetc(word[j], infile);

  }



  word[j] = '\0';
  if (verbose)
    printf("Read: '%s'\n", word);

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
  char word[2048];
  char *owrd;
  int i;
  int *ii;
  double y;
  double *yy;
  if (kind == 'w')
  {
    single_line_entry = 1;
    whole_line_entry = 1;
  }

  get_word(infile, word);
  if (feof(infile))
  {
    printf("ERROR: Unexpected EOF on '%s'\n", emssg);
    if (outfile)
      fprintf(outfile, "ERROR: Unexpected EOF on '%s'\n", emssg);

    exit(1);
  }

  if (kind == 'i')
  {
    if (sscanf(word, "%d", &i) != 1)
    {
      printf("ERROR: Bad integer '%s', reading %s.\n", word, emssg);
      fprintf(outfile, "ERROR: Bad integer '%s', reading %s.\n", word, emssg);
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
      printf("ERROR: Bad float '%s', reading %s.\n", word, emssg);
      fprintf(outfile, "ERROR: Bad float '%s', reading %s.\n", word, emssg);
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
        printf("ERROR1: Found '%s' when expecting '%s'\n", word, emssg);
        fprintf(outfile, "ERROR1: Found '%s' when expecting '%s'\n", word, emssg);
        exit(1);
      }

    }

  }
  else
    if (kind == 'w')
  {
    owrd = (char *) x;
    owrd[0] = '\0';
    strcat(owrd, word);
    strcat(owrd, " ");
    single_line_entry = 0;
    whole_line_entry = 0;
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
      if (single_line_entry && (strlen(word) == 0))
      i = notappvalue;
    else
    {
      printf("ERROR1: Bad boolean '%s', reading %s.\n", word, emssg);
      fprintf(outfile, "ERROR: Bad boolean '%s', reading %s.\n", word, emssg);
      exit(1);
    }






    ii = (int *) x;
    *ii = i;
  }
  else
  {
    printf("ERROR: Unknown type '%c'\n", kind);
    fprintf(outfile, "ERROR: Unknown type '%c'\n", kind);
    exit(1);
  }






}

void get_param_single_line(FILE *infile, char kind, void *x, char *emssg)
{
  single_line_entry = 1;
  get_parameter(infile, kind, x, emssg);
  single_line_entry = 0;
}

void get_parameters(FILE *infile, char kind, void *x, char *emssg)
{
  char word[2048];
  char *owrd = 0;
  int j;
  int *ii;
  double y;
  double *yy;
  if (kind == 'f')
  {
    yy = (double *) x;
    *yy = 0.0;
  }
  else
    if (kind == 'w')
  {
    owrd = (char *) x;
    owrd[0] = '\0';
  }


  get_word(infile, word);
  while (word[0] != ';')
  {
    if (feof(infile))
    {
      printf("ERROR: Unexpected EOF on '%s'\n", emssg);
      fprintf(outfile, "ERROR: Unexpected EOF on '%s'\n", emssg);
      exit(1);
    }

    if (kind == 'i')
    {
      if (sscanf(word, "%d", &j) != 1)
      {
        printf("ERROR: Bad integer '%s', reading %s.\n", word, emssg);
        fprintf(outfile, "ERROR: Bad integer '%s', reading %s.\n", word, emssg);
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
        printf("ERROR: Bad float '%s', reading %s.\n", word, emssg);
        fprintf(outfile, "ERROR: Bad float '%s', reading %s.\n", word, emssg);
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
          printf("ERROR2: Found '%s' when expecting '%s'\n", word, emssg);
          fprintf(outfile, "ERROR2: Found '%s' when expecting '%s'\n", word, emssg);
          exit(1);
        }

      }

    }
    else
      if (kind == 'w')
    {
      strcat(owrd, word);
      strcat(owrd, " ");
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
        if (strcasecmp(word, "n/a") == 0)
      {
        get_word(infile, word);
        return;
      }
      else
      {
        printf("ERROR2: Bad boolean '%s', reading %s.\n", word, emssg);
        fprintf(outfile, "ERROR: Bad boolean '%s', reading %s.\n", word, emssg);
        exit(1);
      }



      ii = (int *) x;
      *ii = j;
    }
    else
    {
      printf("ERROR: Unknown type '%c'\n", kind);
      fprintf(outfile, "ERROR: Unknown type '%c'\n", kind);
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
    printf("Wanring: Bad month '%s' on '%s'\n", word1, emssg);
    fprintf(outfile, "Warning: Bad month '%s' on '%s'\n", word1, emssg);
    return 0;
  }













  next_word(owrd, word1, " /,-\t\n\r");
  if (((sscanf(word1, "%d", day) != 1) || ((*day) < 1)) || ((*day) > 31))
  {
    printf("ERROR: Bad day '%s' on '%s'\n", word1, emssg);
    fprintf(outfile, "ERROR: Bad day '%s' on '%s'\n", word1, emssg);
    return 0;
  }

  next_word(owrd, word1, " /,-\t\n\r");
  if (((sscanf(word1, "%d", year) != 1) || ((*year) < 0)) || ((*year) > 3000))
  {
    printf("ERROR: Bad year '%s' on '%s'\n", word1, emssg);
    fprintf(outfile, "ERROR: Bad year '%s' on '%s'\n", word1, emssg);
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
    printf("Warning:  Unusual year in '%s' .  Use mm-dd-yy date like 5-23-02.\n", datestr);

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
      printf("ERROR: Bad month '%d'\n", month);
      fprintf(outfile, "ERROR: Bad month '%d'\n", month);
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

void read_comment_filtered_line(FILE *infile, char *line, int maxlen)
{
  int j = 0;
  do
  {
    line[j] = getc(infile);
    if (line[j] == '{')
    {
      do
        line[j] = getc(infile);
      while ((line[j] != '}') && (!feof(infile)));
      line[j] = getc(infile);
      line[j] = ' ';
    }

    j++;
  }
  while (((!feof(infile)) && (line[j - 1] != '\n')) && (j < (maxlen - 2)));
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

void showline_wlabelnz(char *label, double value)
{
  if (value != 0.0)
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
    printf("Could not open %s\n", filename);
    return;
  }

  fgets(line, 500, infile);
  while (!feof(infile))
  {
    printf("%s", line);
    fgets(line, 500, infile);
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
    printf("Read Coment: {%s}\n", word);

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

struct pdf_markup_record
{
  char *tagname;
  char *value;
  int page;
  float xpos;
  float ypos;
  struct pdf_markup_record *next;
} *pdf_markup_list = 0;
void add_pdf_markup(char *tagname, int page, float xpos, float ypos, char *value)
{
  struct pdf_markup_record *new;
  new = (struct pdf_markup_record *) calloc(1, sizeof(struct pdf_markup_record));
  new->next = pdf_markup_list;
  pdf_markup_list = new;
  new->tagname = strdup(tagname);
  new->value = strdup(value);
  new->page = page;
  new->xpos = xpos;
  new->ypos = ypos;
}

void process_pdf_markup_command(char *line)
{
  char word[4096];
  char tagname[4096];
  char value[4096];
  int pgnum = -1;
  float xpos = 0.0;
  float ypos = 0.0;
  if (mystrcasestr(line, "MarkupPDF") == 0)
    return;

  if (mystrcasestr(line, "MarkupPDF(") != 0)
  {
    next_word(line, word, " \t(");
    next_word(line, word, " \t(,");
    if (sscanf(word, "%d", &pgnum) != 1)
    {
      printf("Error reading MarkupPDF page-num '%s'\n", word);
      fprintf(outfile, "Error reading MarkupPDF page-num '%s'\n", word);
      return;
    }

    next_word(line, word, " \t,");
    if (sscanf(word, "%f", &xpos) != 1)
    {
      printf("Error reading MarkupPDF Xposition '%s'\n", word);
      fprintf(outfile, "Error reading MarkupPDF Xposition '%s'\n", word);
      return;
    }

    next_word(line, word, " \t,)");
    if (sscanf(word, "%f", &ypos) != 1)
    {
      printf("Error reading MarkupPDF Yposition '%s'\n", word);
      fprintf(outfile, "Error reading MarkupPDF Yposition '%s'\n", word);
      return;
    }

    next_word(line, word, " \t,)=");
  }
  else
  {
    next_word(line, word, " \t");
    next_word(line, word, " \t=");
  }

  strcpy(tagname, word);
  next_word(line, value, " \t=");
  strcat(value, line);
  add_pdf_markup(tagname, pgnum, xpos, ypos, value);
}

void intercept_any_pdf_markups(FILE *infile)
{
  char line[8192];
  if (!outfile)
    return;

  read_comment_filtered_line(infile, line, 8192);
  while (!feof(infile))
  {
    if (strstr(line, "MarkupPDF") != 0)
      process_pdf_markup_command(line);

    read_comment_filtered_line(infile, line, 8192);
  }

}

void exude_pdf_markups(FILE *outfile)
{
  struct pdf_markup_record *old;
  if (!outfile)
    return;

  while (pdf_markup_list)
  {
    if (pdf_markup_list->page > 0)
      fprintf(outfile, "NewPDFMarkup( %d, %g, %g ) %s\n", pdf_markup_list->page, pdf_markup_list->xpos, pdf_markup_list->ypos, pdf_markup_list->tagname);

    fprintf(outfile, "%s = %s\n", pdf_markup_list->tagname, pdf_markup_list->value);
    old = pdf_markup_list;
    pdf_markup_list = pdf_markup_list->next;
    free(old->tagname);
    free(old->value);
    free(old);
  }

}

void grab_any_pdf_markups(char *infname, FILE *outfile)
{
  FILE *infile;
  infile = fopen(infname, "rb");
  if (infile == 0)
  {
    printf("GAPM: Cannot open '%s' for reading.\n", infname);
    return;
  }

  intercept_any_pdf_markups(infile);
  fclose(infile);
  exude_pdf_markups(outfile);
}


/* END of taxsolve_routines.c */
/* START of taxsolve_US_1040_2018.c */
float us_thisversion = 16.05;
double us_SchedA[1000];
double us_SchedD[1000];
double us_amtws[1000];
double us_Sched1[1000];
double us_Sched2[1000];
double us_Sched3[1000];
double us_Sched4[1000];
double us_Sched5[1000];
double us_L2a = 0.0;
double us_L3a = 0.0;
double us_L4a = 0.0;
double us_L5a = 0.0;
double us_L11a = 0.0;
double us_S4_60b = 0.0;
double us_qcgws6 = 0.0;
double us_qcgws7 = 0.0;
double us_amtws2c = 0.0;
double us_amtws2g = 0.0;
int us_Do_SchedD = 0;
int us_Do_QDCGTW = 0;
int us_Do_SDTW = 0;
int us_status;
int us_under65 = 1;
int us_over65 = 0;
int us_dependent = 0;
int us_force_print_all_pdf_forms = 0;
double us_collectibles_gains = 0.0;
double us_ws_sched_D[1000];
double us_L17a = 0.0;
double us_L17b = 0.0;
double us_L17c = 0.0;
double us_brkpt[4][9] = {{0.0, 9525.0, 38700.0, 82500.0, 157500.0, 200000.0, 500000.0, 9e9}, {0.0, 19050.0, 77400.0, 165000.0, 315000.0, 400000.0, 600000.0, 9e9}, {0.0, 9525.0, 38700.0, 82500.0, 157500.0, 200000.0, 300000.0, 9e9}, {0.0, 13600.0, 51800.0, 82500.0, 157500.0, 200000.0, 500000.0, 9e9}};
double us_txrt[4][9] = {{0.1, 0.12, 0.22, 0.24, 0.32, 0.35, 0.37}, {0.1, 0.12, 0.22, 0.24, 0.32, 0.35, 0.37}, {0.1, 0.12, 0.22, 0.24, 0.32, 0.35, 0.37}, {0.1, 0.12, 0.22, 0.24, 0.32, 0.35, 0.37}};
double us_TaxRateFormula(double x, int us_status)
{
  double sum = 0.0;
  int bracket = 0;
  if (us_status == 5)
    us_status = 2;

  us_status = us_status - 1;
  while (us_brkpt[us_status][bracket + 1] < x)
  {
    sum = sum + ((us_brkpt[us_status][bracket + 1] - us_brkpt[us_status][bracket]) * us_txrt[us_status][bracket]);
    bracket = bracket + 1;
  }

  return ((x - us_brkpt[us_status][bracket]) * us_txrt[us_status][bracket]) + sum;
}

void us_Report_bracket_info(double income, double addedtx, int us_status)
{
  double tx;
  int bracket = 0;
  tx = us_TaxRateFormula(income, us_status);
  if (us_status == 5)
    us_status = 2;

  us_status = us_status - 1;
  while (us_brkpt[us_status][bracket + 1] < income)
    bracket++;

  printf(" You are in the %2.1f%% marginal tax bracket,\n and you are paying an effective %2.1f%% tax on your income.\n", 100.0 * us_txrt[us_status][bracket], (100.0 * (tx + addedtx)) / (income + 1e-9));
  fprintf(outfile, " You are in the %2.1f%% marginal tax bracket,\n and you are paying an effective %2.1f%% tax on your income.\n", 100.0 * us_txrt[us_status][bracket], (100.0 * (tx + addedtx)) / (income + 1e-9));
}

double us_TaxRateFunction(double income, int us_status)
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
    tx = (int) (us_TaxRateFormula(x, us_status) + 0.5);
  }
  else
    tx = us_TaxRateFormula(income, us_status);

  return tx;
}

void us_showschedA(int linenum)
{
  if (us_SchedA[linenum] > 0.0)
    fprintf(outfile, " A%d = %6.2f\n", linenum, us_SchedA[linenum]);

}

void us_showschedA_wMsg(int linenum, char *msg)
{
  if (us_SchedA[linenum] > 0.0)
    fprintf(outfile, " A%d = %6.2f	%s\n", linenum, us_SchedA[linenum], msg);

}

void us_print2(char *msg)
{
  printf("%s", msg);
  fprintf(outfile, "%s", msg);
}

void us_capgains_qualdividends_worksheets(int us_status)
{
  double ws[50];
  int j;
  for (j = 0; j < 50; j++)
    ws[j] = 0.0;

  ws[1] = L[10];
  ws[2] = us_L3a;
  if (us_Do_SchedD)
    ws[3] = NotLessThanZero(smallerof(us_SchedD[15], us_SchedD[16]));
  else
    ws[3] = us_Sched1[13];

  ws[4] = ws[2] + ws[3];
  ws[5] = 0.0;
  ws[6] = NotLessThanZero(ws[4] - ws[5]);
  us_qcgws6 = ws[6];
  ws[7] = NotLessThanZero(ws[1] - ws[6]);
  us_qcgws7 = ws[7];
  switch (us_status)
  {
    case 1:

    case 3:
      ws[8] = 38600.0;
      break;

    case 2:

    case 5:
      ws[8] = 77200.0;
      break;

    case 4:
      ws[8] = 51700.0;
      break;

  }

  ws[9] = smallerof(ws[1], ws[8]);
  ws[10] = smallerof(ws[7], ws[9]);
  ws[11] = ws[9] - ws[10];
  ws[12] = smallerof(ws[1], ws[6]);
  ws[13] = ws[11];
  ws[14] = ws[12] - ws[13];
  switch (us_status)
  {
    case 1:
      ws[15] = 425800.0;
      break;

    case 3:
      ws[15] = 239500.0;
      break;

    case 2:

    case 5:
      ws[15] = 479000.0;
      break;

    case 4:
      ws[15] = 452400.0;
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
  ws[24] = us_TaxRateFunction(ws[7], us_status);
  ws[25] = (ws[20] + ws[23]) + ws[24];
  ws[26] = us_TaxRateFunction(ws[1], us_status);
  ws[27] = smallerof(ws[25], ws[26]);
  for (j = 1; j <= 27; j++)
  {
    printf("	Qual. Div & Gains WorkSheet %d:  %8.2f\n", j, ws[j]);
    if (j == 3)
    {
      if (us_Do_SchedD)
        fprintf(outfile, "\t\t3: Check Yes.\n");
      else
        fprintf(outfile, "\t\t3: Check No.\n");

    }

    fprintf(outfile, "	Qual. Div & Gains WorkSheet %d:  %8.2f\n", j, ws[j]);
  }

  us_L11a = ws[27];
}

double us_form6251_AlternativeMinimumTax(int itemized)
{
  double thresholdA = 0;
  double thresholdB = 0;
  double thresholdC = 0.0;
  double amtexmption;
  double offsetA = 0.0;
  double amtws2a;
  double amtws2b;
  double amtws2e;
  int j;
  int file_amt = 1;
  printf("Review AMT form6251 routine for your situation.\n");
  fprintf(outfile, "Review AMT form6251 routine for your situation.\n");
  if (L[10] > 0.0)
    us_amtws[1] = L[10];
  else
    us_amtws[1] = (L[7] - L[8]) - L[9];

  if (itemized)
    amtws2a = us_SchedA[7];
  else
    amtws2a = L[8];

  amtws2b = -(us_Sched1[10] + us_Sched1[21]);
  amtws2e = absolutev(us_Sched1[21]);
  us_amtws[2] = (((amtws2a + amtws2b) + us_amtws2c) + amtws2e) + us_amtws2g;
  for (j = 1; j <= 3; j++)
    us_amtws[4] = us_amtws[4] + us_amtws[j];

  if ((us_status == 3) && (us_amtws[4] > 718800.0))
  {
    if (us_amtws[4] > 937600.0)
      us_amtws[4] = us_amtws[4] + 54700.0;
    else
      us_amtws[4] = us_amtws[4] + (0.25 * (us_amtws[4] - 718800.0));

  }

  switch (us_status)
  {
    case 1:

    case 4:
      thresholdA = 500000.0;
      thresholdB = 781200.0;
      thresholdC = 191100.0;
      offsetA = 3822.0;
      amtexmption = 70300.0;
      break;

    case 2:

    case 5:
      thresholdA = 1000000.0;
      thresholdB = 1437600.0;
      thresholdC = 191100.0;
      offsetA = 3822.0;
      amtexmption = 109400.0;
      break;

    case 3:
      thresholdA = 500000.0;
      thresholdB = 718800.0;
      thresholdC = 95550.0;
      offsetA = 1911.0;
      amtexmption = 54700.0;
      break;

    default:
      printf("Status %d not handled.\n", us_status);
      exit(1);

  }

  if (us_amtws[4] > thresholdA)
  {
    double ews[20];
    if (us_amtws[4] > thresholdB)
      amtexmption = 0.0;
    else
    {
      ews[1] = amtexmption;
      ews[2] = us_amtws[4];
      ews[3] = thresholdA;
      ews[4] = NotLessThanZero(ews[2] - ews[3]);
      ews[5] = 0.25 * ews[4];
      ews[6] = NotLessThanZero(ews[1] - ews[5]);
      amtexmption = ews[6];
    }

  }

  us_amtws[5] = amtexmption;
  us_amtws[6] = NotLessThanZero(us_amtws[4] - us_amtws[5]);
  if (us_amtws[6] > 0.0)
  {
    if (((us_Sched1[13] != 0.0) || (us_L3a != 0.0)) || ((us_SchedD[15] > 0.0) && (us_SchedD[16] > 0.0)))
    {
      us_amtws[12] = us_amtws[6];
      us_amtws[13] = largerof(us_qcgws6, us_ws_sched_D[13]);
      us_amtws[14] = us_SchedD[19];
      if (us_Do_SDTW)
        us_amtws[15] = smallerof(us_amtws[13] + us_amtws[14], us_ws_sched_D[10]);
      else
        us_amtws[15] = us_amtws[13];

      us_amtws[16] = smallerof(us_amtws[12], us_amtws[15]);
      us_amtws[17] = us_amtws[12] - us_amtws[16];
      if (us_amtws[17] <= thresholdC)
        us_amtws[18] = 0.26 * us_amtws[17];
      else
        us_amtws[18] = (0.28 * us_amtws[17]) - offsetA;

      switch (us_status)
      {
        case 2:

        case 5:
          us_amtws[19] = 77200.0;
          break;

        case 1:

        case 3:
          us_amtws[19] = 38600.0;
          break;

        case 4:
          us_amtws[19] = 51700.0;

      }

      if (us_Do_QDCGTW)
        us_amtws[20] = NotLessThanZero(us_qcgws7);
      else
        if (us_Do_SDTW)
        us_amtws[20] = NotLessThanZero(us_ws_sched_D[14]);
      else
        us_amtws[20] = NotLessThanZero(L[10]);


      us_amtws[21] = NotLessThanZero(us_amtws[19] - us_amtws[20]);
      us_amtws[22] = smallerof(us_amtws[12], us_amtws[13]);
      us_amtws[23] = smallerof(us_amtws[21], us_amtws[22]);
      us_amtws[24] = us_amtws[22] - us_amtws[23];
      switch (us_status)
      {
        case 1:
          us_amtws[25] = 425800.0;
          break;

        case 3:
          us_amtws[25] = 239500.0;
          break;

        case 2:

        case 5:
          us_amtws[25] = 479900.0;
          break;

        case 4:
          us_amtws[25] = 452400.0;
          break;

        default:
          printf("Status %d not handled.\n", us_status);
          exit(1);

      }

      us_amtws[26] = us_amtws[21];
      if (us_Do_QDCGTW)
        us_amtws[27] = NotLessThanZero(us_qcgws7);
      else
        if (us_Do_SDTW)
        us_amtws[27] = NotLessThanZero(us_ws_sched_D[19]);
      else
        us_amtws[27] = NotLessThanZero(L[10]);


      us_amtws[28] = us_amtws[26] + us_amtws[27];
      us_amtws[29] = NotLessThanZero(us_amtws[25] - us_amtws[28]);
      us_amtws[30] = smallerof(us_amtws[24], us_amtws[29]);
      us_amtws[31] = 0.15 * us_amtws[30];
      us_amtws[32] = us_amtws[23] + us_amtws[30];
      if (absolutev(us_amtws[12] - us_amtws[32]) > 0.005)
      {
        us_amtws[33] = us_amtws[22] - us_amtws[32];
        us_amtws[34] = 0.20 * us_amtws[33];
        if (us_amtws[35] != 0.0)
        {
          us_amtws[35] = (us_amtws[17] + us_amtws[32]) + us_amtws[33];
          us_amtws[36] = us_amtws[12] - us_amtws[35];
          us_amtws[37] = 0.25 * us_amtws[36];
        }

      }

      us_amtws[38] = ((us_amtws[18] + us_amtws[31]) + us_amtws[34]) + us_amtws[37];
      if (us_amtws[12] <= thresholdC)
        us_amtws[39] = 0.26 * us_amtws[12];
      else
        us_amtws[39] = (0.28 * us_amtws[12]) - offsetA;

      us_amtws[40] = smallerof(us_amtws[38], us_amtws[39]);
      us_amtws[7] = us_amtws[40];
    }
    else
    {
      if (us_amtws[6] <= thresholdC)
        us_amtws[7] = 0.26 * us_amtws[6];
      else
        us_amtws[7] = (0.28 * us_amtws[6]) - offsetA;

    }

    us_amtws[9] = us_amtws[7] - us_amtws[8];
  }

  us_amtws[10] = (us_L11a + us_Sched2[46]) - us_Sched3[48];
  us_amtws[11] = NotLessThanZero(us_amtws[9] - us_amtws[10]);
  printf("	AMTws[11] = Abs( %6.2f - %6.2f ) = Abs( %6.2f )\n", us_amtws[9], us_amtws[10], us_amtws[9] - us_amtws[10]);
  if (us_amtws[7] > us_amtws[10])
  {
    file_amt = 1;
    fprintf(outfile, "You MUST file AMT form 6251. (%g > %g)\n", us_amtws[7], us_amtws[10]);
  }
  else
  {
    if ((amtws2e + us_amtws[3]) < 0.0)
    {
      file_amt = 1;
      fprintf(outfile, "You may need to file AMT form 6251.  (AMTws[31]=%g which is NOT more than AMTws[34]=%g)\n", us_amtws[31], us_amtws[34]);
      fprintf(outfile, " (See \"Who Must File\" on page-1 of Instructions for Form-6251.)\n");
    }
    else
      file_amt = 0;

  }

  if (us_force_print_all_pdf_forms)
    file_amt = 1;

  if (file_amt)
    fprintf(outfile, "PDFpage: 15 15\n");

  for (j = 0; j < 100; j++)
  {
    if (j == 2)
    {
      char tmplabel[1024];
      sprintf(tmplabel, " 		AMT_Form_6251_L2a");
      showline_wlabelnz(tmplabel, amtws2a);
      sprintf(tmplabel, " 		AMT_Form_6251_L2b");
      showline_wlabelnz(tmplabel, amtws2b);
      sprintf(tmplabel, " 		AMT_Form_6251_L2c");
      showline_wlabelnz(tmplabel, us_amtws2c);
      sprintf(tmplabel, " 		AMT_Form_6251_L2e");
      showline_wlabelnz(tmplabel, amtws2e);
      sprintf(tmplabel, " 		AMT_Form_6251_L2g");
      showline_wlabelnz(tmplabel, us_amtws2g);
    }

    if ((j == 11) || (us_amtws[j] != 0.0))
    {
      printf(" 		AMT Form 6251 L%d = %8.2f\n", j, us_amtws[j]);
      fprintf(outfile, " 		AMT_Form_6251_L%d = %8.2f\n", j, us_amtws[j]);
    }

    if (file_amt && (j == 11))
      fprintf(outfile, "EndPDFpage.\nPDFpage: 16 16\n");

  }

  if (file_amt)
    fprintf(outfile, "EndPDFpage.\n");

  fprintf(outfile, "	AMTws[11] = Abs( %6.2f - %6.2f ) = Abs( %6.2f )\n", us_amtws[9], us_amtws[10], us_amtws[9] - us_amtws[10]);
  fprintf(outfile, "Your Alternative Minimum Tax = %8.2f\n", us_amtws[11]);
  printf("Your Alternative Minimum Tax = %8.2f\n", us_amtws[11]);
  return us_amtws[11];
}

struct us_FedReturnData
{
  double fedline[1000];
  double schedD[1000];
  int Exception;
  int Itemized;
} us_LastYearsReturn;
void us_convert_slashes(char *fname)
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

void us_ImportFederalReturnData(char *fedlogfile, struct us_FedReturnData *fed_data)
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

  us_convert_slashes(fedlogfile);
  infile = fopen(fedlogfile, "r");
  if (infile == 0)
  {
    printf("Error: Could not open federal return '%s'\n", fedlogfile);
    fprintf(outfile, "Error: Could not open federal return '%s'\n", fedlogfile);
    exit(1);
  }

  printf("Importing Last Year's Federal Return Data from file '%s'\n", fedlogfile);
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
      if (sscanf(&word[1], "%d", &linenum) != 1)
        printf("Error: Reading fed line number '%s%s'\n", word, fline);

      next_word(fline, word, " \t=");
      remove_certain_chars(word, ",");
      if (sscanf(word, "%lf", &fed_data->fedline[linenum]) != 1)
        printf("Error: Reading fed line %d '%s%s'\n", linenum, word, fline);

      if (verbose)
        printf("FedLin[%d] = %2.2f\n", linenum, fed_data->fedline[linenum]);

    }

    if ((strstr(word, "D") == word) && (strstr(fline, " = ") != 0))
    {
      if (sscanf(&word[1], "%d", &linenum) != 1)
        printf("Error: Reading fed line number '%s%s'\n", word, fline);

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
          printf("Error: Reading fed schedD %d '%s%s'\n", linenum, word, fline);


      }

      if (verbose)
        printf("FedLin[%d] = %2.2f\n", linenum, fed_data->schedD[linenum]);

    }

    read_line(infile, fline);
  }

  fclose(infile);
}

void us_CapitalLossCarryOverWorksheet(char *fedlogfile, struct us_FedReturnData *us_LastYearsReturn)
{
  double ws[50];
  int k;
  us_ImportFederalReturnData(fedlogfile, us_LastYearsReturn);
  if (us_LastYearsReturn->schedD[21] == 0.0)
  {
    printf(" No carry-over loss.\n");
    fprintf(outfile, " No carry-over loss.\n");
    return;
  }

  if ((absolutev(us_LastYearsReturn->schedD[21]) >= absolutev(us_LastYearsReturn->schedD[16])) && (us_LastYearsReturn->fedline[41] >= 0.0))
  {
    printf(" No carry-over loss.\n");
    fprintf(outfile, " No carry-over loss.\n");
    return;
  }

  for (k = 0; k < 50; k++)
    ws[k] = 0.0;

  ws[1] = us_LastYearsReturn->fedline[41];
  ws[2] = absolutev(us_LastYearsReturn->schedD[21]);
  ws[3] = NotLessThanZero(ws[1] + ws[2]);
  ws[4] = smallerof(ws[2], ws[3]);
  for (k = 1; k <= 4; k++)
  {
    printf("\tCarryOverWs%d = %2.2f\n", k, ws[k]);
    fprintf(outfile, "\tCarryOverWs%d = %2.2f\n", k, ws[k]);
  }

  if (us_LastYearsReturn->schedD[7] < 0.0)
  {
    ws[5] = -us_LastYearsReturn->schedD[7];
    ws[6] = NotLessThanZero(us_LastYearsReturn->schedD[15]);
    ws[7] = ws[4] + ws[6];
    ws[8] = NotLessThanZero(ws[5] - ws[7]);
    if (ws[8] > 0.0)
      us_SchedD[6] = ws[8];

    for (k = 5; k <= 8; k++)
    {
      printf("\tCarryOverWs%d = %2.2f\n", k, ws[k]);
      fprintf(outfile, "\tCarryOverWs%d = %2.2f\n", k, ws[k]);
    }

  }
  else
    printf("\t(Skip CarryOverWs lines 5-8.)\n");

  if (us_LastYearsReturn->schedD[15] < 0.0)
  {
    ws[9] = absolutev(us_LastYearsReturn->schedD[15]);
    ws[10] = NotLessThanZero(us_LastYearsReturn->schedD[7]);
    ws[11] = NotLessThanZero(ws[4] - ws[5]);
    ws[12] = ws[10] + ws[11];
    ws[13] = NotLessThanZero(ws[9] - ws[12]);
    if (ws[13] > 0.0)
      us_SchedD[14] = ws[13];

    for (k = 9; k <= 13; k++)
    {
      printf("\tCarryOverWs%d = %2.2f\n", k, ws[k]);
      fprintf(outfile, "\tCarryOverWs%d = %2.2f\n", k, ws[k]);
    }

  }
  else
    printf("\t(Skip CarryOverWorkSheet lines 9-13.)\n");

}

struct capgain_record
{
  char *comment;
  char *buy_date;
  char *sell_date;
  double buy_amnt;
  double sell_amnt;
  struct capgain_record *nxt;
} *us_short_trades = 0;
struct capgain_record *us_long_trades = 0;
double us_total_sales;
double us_total_costs = 0.0;
void us_new_capgain(struct capgain_record **list, char *comment, double buy_amnt, char *buy_date, double sell_amnt, char *sell_date)
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

void us_print_capgain_list(struct capgain_record *list, int section, char *message, char *pdfmsg)
{
  struct capgain_record *item;
  char word[4096];
  char row = 'a';
  us_total_sales = 0.0;
  us_total_costs = 0.0;
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
    us_total_sales = us_total_sales + item->sell_amnt;
    us_total_costs = us_total_costs + item->buy_amnt;
    item = item->nxt;
  }

  fprintf(outfile, " ---------------------------------------------------------------------------------------\n");
  fprintf(outfile, " %d. Totals:                                        %14.2f %14.2f %14.2f\n\n", section + 1, us_total_sales, absolutev(us_total_costs), us_total_sales + us_total_costs);
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

  fprintf(outfile, " F8949_2d = %14.2f\n", us_total_sales);
  fprintf(outfile, " F8949_2e = %14.2f\n", absolutev(us_total_costs));
  fprintf(outfile, " F8949_2h = %14.2f\n", us_total_sales + us_total_costs);
  fprintf(outfile, "EndPDFpage.\n\n");
}

void us_free_capgain_list(struct capgain_record **list)
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

void us_get_gain_and_losses(char *label)
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
      printf("ERROR: Unexpected EOF on '%s'\n", label);
      fprintf(outfile, "ERROR: Unexpected EOF on '%s'\n", label);
      exit(1);
    }

    if (!us_Do_SchedD)
    {
      fprintf(outfile, "\nForm(s) 8949:\n");
      us_Do_SchedD = 1;
    }

    switch (toggle)
    {
      case 0:
        toggle++;
        if (sscanf(word, "%lf", &amnt1) != 1)
      {
        printf("ERROR: Bad float '%s', reading %s.\n", word, label);
        fprintf(outfile, "ERROR: Bad float '%s', reading %s.\n", word, label);
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
        printf("ERROR: Bad float '%s', reading %s.\n", word, label);
        fprintf(outfile, "ERROR: Bad float '%s', reading %s.\n", word, label);
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
        printf("ERROR: Buy-date after sell-date.\n");
        fprintf(outfile, "ERROR: Buy-date after sell-date.\n");
        exit(1);
      }

        if ((date2 - date1) > 365)
      {
        us_new_capgain(&us_long_trades, comment, amnt1, date_str1, amnt2, date_str2);
      }
      else
      {
        us_new_capgain(&us_short_trades, comment, amnt1, date_str1, amnt2, date_str2);
      }

        break;

    }

    get_word(infile, word);
  }

  if (toggle != 0)
  {
    printf("ERROR: Imbalanced cap-gains entry (toggle=%d).\n", toggle);
    fprintf(outfile, "ERROR: Imbalanced cap-gains entry (toggle=%d).\n", toggle);
    exit(1);
  }

}

void us_get_cap_gains()
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

  us_get_gain_and_losses("CapGains-A/D");
  if (us_short_trades)
  {
    us_print_capgain_list(us_short_trades, 1, "Form 8949 Part-I, Short-Term Cap Gains+Losses, CHECK (A) Basis Reported to IRS:", "13 13\n F8949_ckA X");
    SchedDd[1] = us_total_sales;
    SchedDe[1] = us_total_costs;
    us_SchedD[1] = SchedDd[1] + SchedDe[1];
    us_free_capgain_list(&us_short_trades);
  }

  if (us_long_trades)
  {
    us_print_capgain_list(us_long_trades, 3, "Form 8949 Part-II, Long-Term Cap Gains+Losses, CHECK (D) Basis Reported to IRS:", "14 14\n F8949_ckD X");
    SchedDd[8] = us_total_sales;
    SchedDe[8] = us_total_costs;
    us_SchedD[8] = SchedDd[8] + SchedDe[8];
    us_free_capgain_list(&us_long_trades);
  }

  us_get_gain_and_losses("CapGains-B/E");
  if (us_short_trades)
  {
    us_print_capgain_list(us_short_trades, 1, "Form 8949 Part-I, Short-Term Cap Gains+Losses, CHECK (B) Basis NOT Reported to IRS:", "13 13\n F8949_ckB X");
    SchedDd[2] = us_total_sales;
    SchedDe[2] = us_total_costs;
    us_SchedD[2] = SchedDd[2] + SchedDe[2];
    us_free_capgain_list(&us_short_trades);
  }

  if (us_long_trades)
  {
    us_print_capgain_list(us_long_trades, 3, "Form 8949 Part-II, Long-Term Cap Gains+Losses, CHECK (E) Basis NOT Reported to IRS:", "14 14\n F8949_ckE X");
    SchedDd[9] = us_total_sales;
    SchedDe[9] = us_total_costs;
    us_SchedD[9] = SchedDd[9] + SchedDe[9];
    us_free_capgain_list(&us_long_trades);
  }

  us_get_gain_and_losses("CapGains-C/F");
  if (us_short_trades)
  {
    us_print_capgain_list(us_short_trades, 1, "Form 8949 Part-I, Short-Term Cap Gains+Losses, CHECK (C) Not reported on Form 1099-B.\n", "13 13\n F8949_ckC X");
    SchedDd[3] = us_total_sales;
    SchedDe[3] = us_total_costs;
    us_SchedD[3] = SchedDd[3] + SchedDe[3];
    us_free_capgain_list(&us_short_trades);
  }

  if (us_long_trades)
  {
    us_print_capgain_list(us_long_trades, 3, "Form 8949 Part-II, Long-Term Cap Gains+Losses, CHECK (F) Not reported on Form 1099-B.\n", "14 14\n F8949_ckF X");
    SchedDd[10] = us_total_sales;
    SchedDe[10] = us_total_costs;
    us_SchedD[10] = SchedDd[10] + SchedDe[10];
    us_free_capgain_list(&us_long_trades);
  }

  stcg = (us_SchedD[1] + us_SchedD[2]) + us_SchedD[3];
  ltcg = (us_SchedD[8] + us_SchedD[9]) + us_SchedD[10];
  GetLine("D4", &us_SchedD[4]);
  GetLine("D5", &us_SchedD[5]);
  get_parameter(infile, 's', word, "D6");
  get_word(infile, word);
  if (strcmp(word, ";") != 0)
  {
    if (sscanf(word, "%lf", &us_SchedD[6]) != 1)
      LastYearsOutFile = strdup(word);

    do
    {
      get_word(infile, word);
      if ((strlen(word) > 0) && (strcmp(word, ";") != 0))
        fprintf(outfile, "Warning: Unexpected multiple values on line D6.  '%s' ignored.\n If multi-part filename, then surround it in quotes (\").", word);

    }
    while (strcmp(word, ";") != 0);
  }

  GetLine("D11", &us_SchedD[11]);
  GetLine("D12", &us_SchedD[12]);
  GetLine("D13", &us_SchedD[13]);
  GetLine("D14", &us_SchedD[14]);
  GetLine("Collectibles", &us_collectibles_gains);
  if (us_collectibles_gains != 0.0)
    fprintf(outfile, "Collectibles_Gains = %6.2f\n", us_collectibles_gains);

  if (LastYearsOutFile != 0)
    us_CapitalLossCarryOverWorksheet(LastYearsOutFile, &us_LastYearsReturn);

  if (us_SchedD[6] > 0.0)
  {
    us_SchedD[6] = -us_SchedD[6];
  }

  if (us_SchedD[14] > 0.0)
  {
    us_SchedD[14] = -us_SchedD[14];
  }

  if (((((((us_SchedD[4] != 0.0) || (us_SchedD[5] != 0.0)) || (us_SchedD[6] != 0.0)) || (us_SchedD[11] != 0.0)) || (us_SchedD[12] != 0.0)) || (us_SchedD[13] != 0.0)) || (us_SchedD[14] != 0.0))
  {
    us_Do_SchedD = 1;
  }

  if (us_Do_SchedD)
  {
    fprintf(outfile, " Cap Gains/Losses Schedule-D\n");
    fprintf(outfile, "PDFpage: 11 11\n");
    fprintf(outfile, "\tNet Forms-8949 Short-term Gains = %10.2f\n", stcg);
    fprintf(outfile, "\tNet Forms-8949 Long-term Gains  = %10.2f\n", ltcg);
    fprintf(outfile, " D1bd = %10.2f\n   D1be = %10.2f\n    D1bh = %10.2f\n", SchedDd[1], absolutev(SchedDe[1]), us_SchedD[1]);
    fprintf(outfile, " D2d = %10.2f\n   D2e = %10.2f\n    D2h = %10.2f\n", SchedDd[2], absolutev(SchedDe[2]), us_SchedD[2]);
    fprintf(outfile, " D3d = %10.2f\n   D3e = %10.2f\n    D3h = %10.2f\n", SchedDd[3], absolutev(SchedDe[3]), us_SchedD[3]);
    fprintf(outfile, " D4 = %6.2f\n", us_SchedD[4]);
    fprintf(outfile, " D5 = %6.2f\n", us_SchedD[5]);
    fprintf(outfile, " D6 = %6.2f		(Carry-over Loss)\n", us_SchedD[6]);
    us_SchedD[7] = ((((us_SchedD[1] + us_SchedD[2]) + us_SchedD[3]) + us_SchedD[4]) + us_SchedD[5]) + us_SchedD[6];
    fprintf(outfile, " D7 = %6.2f		{ Net short-term capital gain or loss }\n", us_SchedD[7]);
    fprintf(outfile, " D8bd = %10.2f\n   D8be = %10.2f\n   D8bh = %10.2f\n", SchedDd[8], absolutev(SchedDe[8]), us_SchedD[8]);
    fprintf(outfile, " D9d = %10.2f\n   D9e = %10.2f\n   D9h = %10.2f\n", SchedDd[9], absolutev(SchedDe[9]), us_SchedD[9]);
    fprintf(outfile, " D10d = %10.2f\n   D10e = %10.2f\n   D10h = %10.2f\n", SchedDd[10], absolutev(SchedDe[10]), us_SchedD[10]);
    fprintf(outfile, " D11 = %6.2f\n", us_SchedD[11]);
    fprintf(outfile, " D12 = %6.2f\n", us_SchedD[12]);
    fprintf(outfile, " D13 = %6.2f\n", us_SchedD[13]);
    fprintf(outfile, " D14 = %6.2f	(Carry-over Loss)\n", us_SchedD[14]);
    us_SchedD[15] = (((((us_SchedD[8] + us_SchedD[9]) + us_SchedD[10]) + us_SchedD[11]) + us_SchedD[12]) + us_SchedD[13]) + us_SchedD[14];
    fprintf(outfile, " D15 = %6.2f		{ Net long-term capital gain or loss }\n", us_SchedD[15]);
    fprintf(outfile, "EndPDFpage.\nPDFpage: 12 12\n");
    us_SchedD[16] = us_SchedD[7] + us_SchedD[15];
    fprintf(outfile, " D16 = %6.2f\n", us_SchedD[16]);
    if (us_SchedD[16] > 0.0)
    {
      us_Sched1[13] = us_SchedD[16];
      if ((us_SchedD[15] > 0.0) && (us_SchedD[16] > 0.0))
      {
        double wsd[50];
        fprintf(outfile, " D17 = yes\n CkD17y X\n");
        wsd[1] = us_collectibles_gains;
        wsd[2] = 0.0;
        wsd[3] = 0.0;
        wsd[4] = 0.0;
        wsd[5] = us_SchedD[14];
        if (us_SchedD[7] < 0.0)
          wsd[6] = us_SchedD[7];
        else
          wsd[6] = 0.0;

        wsd[7] = NotLessThanZero(((((wsd[1] + wsd[2]) + wsd[3]) + wsd[4]) + wsd[5]) + wsd[6]);
        us_SchedD[18] = wsd[7];
        fprintf(outfile, " D18 = %6.2f\n", us_SchedD[18]);
        fprintf(outfile, " D19 = %6.2f\n", us_SchedD[19]);
        if ((us_SchedD[18] == 0.0) && (us_SchedD[19] == 0.0))
        {
          fprintf(outfile, " D20 = Yes\n CkD20y X\n");
          us_Do_QDCGTW = 1;
        }
        else
        {
          fprintf(outfile, " D20 = No\n CkD20n X\n");
          us_Do_SDTW = 1;
          us_Do_QDCGTW = 0;
        }

        doline22 = 0;
      }
      else
      {
        printf(" D17 = no\n CkD17n X\n");
        doline22 = 1;
      }

    }
    else
      if (us_SchedD[16] < 0.0)
    {
      double maxloss;
      if (us_status == 3)
        maxloss = -1500.0;
      else
        maxloss = -3000.0;

      if (us_SchedD[16] < maxloss)
        us_SchedD[21] = maxloss;
      else
        us_SchedD[21] = us_SchedD[16];

      fprintf(outfile, " D21 = %6.2f\n", us_SchedD[21]);
      us_Sched1[13] = us_SchedD[21];
      doline22 = 1;
    }
    else
    {
      us_Sched1[13] = 0.0;
      doline22 = 1;
    }


    if (doline22)
    {
      if (us_L3a > 0.0)
      {
        fprintf(outfile, " D22 = Yes\n CkD22y X\n");
        us_Do_QDCGTW = 1;
      }
      else
      {
        fprintf(outfile, " D22 = No\n CkD22n X\n");
      }

    }

    fprintf(outfile, "EndPDFpage.\n\n");
  }

}

void us_sched_D_tax_worksheet(int us_status)
{
  double ws[100];
  int k;
  for (k = 0; k < 100; k++)
    ws[k] = 0.0;

  ws[1] = L[10];
  ws[2] = us_L3a;
  ws[3] = 0.0;
  ws[4] = 0.0;
  ws[5] = NotLessThanZero(ws[3] - ws[4]);
  ws[6] = NotLessThanZero(ws[2] - ws[5]);
  ws[7] = smallerof(us_SchedD[15], us_SchedD[16]);
  ws[8] = smallerof(ws[3], ws[4]);
  ws[9] = NotLessThanZero(ws[7] - ws[8]);
  ws[10] = ws[6] + ws[9];
  fprintf(outfile, "  Sched-D tax Worksheet line 10 = %6.2f\n", ws[10]);
  ws[11] = us_SchedD[18] + us_SchedD[19];
  ws[12] = smallerof(ws[9], ws[11]);
  ws[13] = ws[10] - ws[12];
  ws[14] = NotLessThanZero(ws[1] - ws[13]);
  fprintf(outfile, "  Sched-D tax Worksheet line 13 = %6.2f\n", ws[13]);
  fprintf(outfile, "  Sched-D tax Worksheet line 14 = %6.2f\n", ws[14]);
  switch (us_status)
  {
    case 1:

    case 3:
      ws[15] = 38600.0;
      break;

    case 2:

    case 5:
      ws[15] = 77200.0;
      break;

    case 4:
      ws[15] = 51700.0;
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
    switch (us_status)
    {
      case 1:
        ws[24] = 425800.0;
        break;

      case 3:
        ws[24] = 239500.0;
        break;

      case 2:

      case 5:
        ws[24] = 479000.0;
        break;

      case 4:
        ws[24] = 452400.0;
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
      if (us_SchedD[19] != 0.0)
      {
        ws[33] = smallerof(ws[9], us_SchedD[19]);
        ws[34] = ws[10] + ws[19];
        ws[35] = ws[1];
        ws[36] = NotLessThanZero(ws[34] - ws[35]);
        ws[37] = NotLessThanZero(ws[33] - ws[36]);
        ws[38] = 0.25 * ws[37];
      }

      if (us_SchedD[18] != 0.0)
      {
        ws[39] = (((ws[19] + ws[20]) + ws[28]) + ws[31]) + ws[37];
        ws[40] = ws[1] - ws[39];
        ws[41] = 0.28 * ws[40];
      }

    }

  }

  ws[42] = us_TaxRateFunction(ws[19], us_status);
  ws[43] = (((ws[29] + ws[32]) + ws[38]) + ws[41]) + ws[42];
  ws[44] = us_TaxRateFunction(ws[1], us_status);
  ws[45] = smallerof(ws[43], ws[44]);
  us_L11a = ws[45];
  for (k = 0; k < 100; k++)
    us_ws_sched_D[k] = ws[k];

}

void us_SocSec_Worksheet()
{
  double ws[100];
  int k;
  if (us_L5a == 0.0)
    return;

  for (k = 0; k < 100; k++)
    ws[k] = 0.0;

  ws[1] = us_L5a;
  ws[2] = 0.5 * ws[1];
  ws[3] = (((L[1] + L[2]) + L[3]) + L[4]) + us_Sched1[22];
  ws[4] = us_L2a;
  ws[5] = (ws[2] + ws[3]) + ws[4];
  for (k = 23; k <= 32; k++)
    ws[6] = ws[6] + us_Sched1[k];

  for (k = 0; k <= 6; k++)
    fprintf(outfile, "\tSocSecWorkSheet[%d] = %6.2f\n", k, ws[k]);

  if (ws[6] >= ws[5])
  {
    L[5] = 0.0;
    fprintf(outfile, "\tSocSecWorkSheet[7]: Check 'No'\n");
    printf("None of your social security benefits are taxable.\n");
    fprintf(outfile, "None of your social security benefits are taxable.\n");
    return;
  }

  ws[7] = ws[5] - ws[6];
  fprintf(outfile, "\tSocSecWorkSheet[7] = %6.2f  (Check 'Yes')\n", ws[7]);
  if (us_status == 2)
    ws[8] = 32000.0;
  else
    ws[8] = 25000.0;

  fprintf(outfile, "\tSocSecWorkSheet[8] = %6.2f\n", ws[8]);
  if (ws[8] >= ws[7])
  {
    L[5] = 0.0;
    fprintf(outfile, "\tSocSecWorkSheet[9]: Check 'No'\n");
    printf("None of your social security benefits are taxable.\n");
    fprintf(outfile, "None of your social security benefits are taxable.\n");
    return;
  }

  ws[9] = ws[7] - ws[8];
  fprintf(outfile, "\tSocSecWorkSheet[9] = %6.2f  (Check 'Yes')\n", ws[9]);
  if (us_status == 2)
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

  L[5] = ws[18];
}

void us_pull_comment(char *line, char *word)
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

void us_Grab_ScheduleB_Payer_Lines(char *infname, FILE *outfile)
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
    printf("Can no longer read '%s'.\n", infname);
    return;
  }

  fprintf(outfile, "\nSchedules Data:\n");
  fgets(line, 200, infile);
  while (!feof(infile))
  {
    next_word(line, word1, " \t\n\r");
    switch (state)
    {
      case 0:
        if (strcmp(word1, "L2b") == 0)
      {
        state = 8;
        ncnt = 15;
        pg = 0;
        cnt = 0;
        newentry = 1;
        strcpy(pgstr, "B1_");
      }
      else
        if (strcmp(word1, "L3b") == 0)
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
        us_pull_comment(line, word2);
        cnt++;
        if (cnt == ncnt)
        {
          if (pg > 0)
          {
            fprintf(outfile, "Btotal = %8.2f\n", total);
            fprintf(outfile, "EndPDFpage.\n");
          }

          fprintf(outfile, "PDFpage: 10 10\n");
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
          printf(" Error reading L2b value '%s'\n", word1);
        else
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
        us_pull_comment(line, word2);
        cnt++;
        if (cnt == ncnt)
        {
          if (pg > 0)
          {
            fprintf(outfile, "Btotal = %8.2f\n", total);
            fprintf(outfile, "EndPDFpage.\n");
          }

          fprintf(outfile, "PDFpage: 10 10\n");
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
          printf(" Error reading L3b value '%s'\n", word1);
        else
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
    printf("Error: Missing ending ';' on L%d\n", state);
    fprintf(outfile, "Btotal = %6.2f\n", total);
    fprintf(outfile, "EndPDFpage.\n");
  }

  fclose(infile);
}

int us_main(int argc, char *argv[])
{
  int argk;
  int j;
  int k;
  int itemize = 0;
  char word[2000];
  char outfname[2000];
  char *infname = "";
  char labelx[1024] = "";
  time_t now;
  double exemption_threshold = 0.0;
  double tmpval = 0.0;
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
  double NumDependents = 0.0;
  double localtax[10];
  double loctaxlimit;
  double homemort[10];
  int StdDedChart_NumBoxesChecked = 0;
  int HealthCoverageChecked = 0;
  int gotS1_32 = 0;
  int gotS2_46 = 0;
  int SchedB7a = 0;
  int SchedB7aa = -1;
  int SchedB8 = 0;
  char SchedB7b[0124] = "";
  printf("US 1040 2018 - v%3.2f\n", us_thisversion);
  argk = 1;
  k = 1;
  while (argk < argc)
  {
    if (strcmp(argv[argk], "-verbose") == 0)
    {
      verbose = 1;
    }
    else
      if (strcmp(argv[argk], "-allforms") == 0)
    {
      us_force_print_all_pdf_forms = 1;
    }
    else
      if (k == 1)
    {
      infname = strdup(argv[argk]);
      infile = fopen(infname, "r");
      if (infile == 0)
      {
        printf("ERROR: Parameter file '%s' could not be opened.\n", infname);
        exit(1);
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
        printf("ERROR: Output file '%s' could not be opened.\n", outfname);
        exit(1);
      }

      printf("Writing results to file:  %s\n", outfname);
    }
    else
    {
      printf("Unknown command-line parameter '%s'\n", argv[argk]);
      exit(1);
    }



    argk = argk + 1;
  }

  if (infile == 0)
  {
    printf("Error: No input file on command line.\n");
    exit(1);
  }

  for (j = 0; j < 1000; j++)
  {
    L[j] = 0.0;
    us_SchedA[j] = 0.0;
    us_SchedD[j] = 0.0;
    us_Sched1[j] = 0.0;
    us_Sched2[j] = 0.0;
    us_Sched3[j] = 0.0;
    us_Sched4[j] = 0.0;
    us_Sched5[j] = 0.0;
    us_ws_sched_D[j] = 0.0;
    us_amtws[j] = 0.0;
  }

  read_line(infile, word);
  now = time(0);
  fprintf(outfile, "\n%s,	 v%2.2f, %s\n", word, us_thisversion, ctime(&now));
  get_parameter(infile, 's', word, "Status");
  get_parameter(infile, 'l', word, "Status?");
  if (strncasecmp(word, "Single", 4) == 0)
    us_status = 1;
  else
    if (strncasecmp(word, "Married/Joint", 13) == 0)
    us_status = 2;
  else
    if (strncasecmp(word, "Married/Sep", 11) == 0)
    us_status = 3;
  else
    if (strncasecmp(word, "Head_of_House", 4) == 0)
    us_status = 4;
  else
    if (strncasecmp(word, "Widow", 4) == 0)
    us_status = 5;
  else
  {
    printf("Error: unrecognized status '%s'. Exiting.\n", word);
    fprintf(outfile, "Error: unrecognized status '%s'. Exiting.\n", word);
    exit(1);
  }





  fprintf(outfile, "Status = %s (%d)\n", word, us_status);
  get_parameter(infile, 's', word, "You_65+Over?");
  get_parameter(infile, 'b', &j, "You_65+Over?");
  StdDedChart_NumBoxesChecked = j;
  if (j == 0)
    us_under65 = 1;
  else
  {
    us_under65 = 0;
    fprintf(outfile, "CkYouOver65 X\n");
  }

  get_parameter(infile, 's', word, "You_Blind?");
  get_parameter(infile, 'b', &j, "You_Blind?");
  StdDedChart_NumBoxesChecked = StdDedChart_NumBoxesChecked + j;
  if (j)
    fprintf(outfile, "CkYouBlind X\n");

  get_parameter(infile, 's', word, "Spouse_65+Over?");
  get_param_single_line(infile, 'b', &j, "Spouse_65+Over?");
  StdDedChart_NumBoxesChecked = StdDedChart_NumBoxesChecked + j;
  if (j == 0)
    us_under65++;
  else
    fprintf(outfile, "CkSpouseOver65 X\n");

  get_parameter(infile, 's', word, "Spouse_Blind?");
  get_param_single_line(infile, 'b', &j, "Spouse_Blind?");
  StdDedChart_NumBoxesChecked = StdDedChart_NumBoxesChecked + j;
  if (j)
    fprintf(outfile, "CkSpouseBlind X\n");

  get_parameter(infile, 's', word, "HealthCoverage?");
  get_parameter(infile, 'b', &HealthCoverageChecked, "HealthCoverage?");
  if (HealthCoverageChecked)
    fprintf(outfile, "CkHealthCoverage X\n");

  switch (us_status)
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
      printf("Error: Unknown filing status %d.\n", us_status);

  }

  GetLine1("Dependents", &NumDependents);
  GetLineF("L1", &L[1]);
  GetLineFnz("L2a", &us_L2a);
  GetLineF("L2b", &L[2]);
  GetLineF("L3a", &us_L3a);
  if (us_L3a > 0.0)
    us_Do_QDCGTW = 1;

  GetLineF("L3b", &L[3]);
  GetLineF("L4a", &us_L4a);
  GetLineF("L4b", &L[4]);
  GetLineF("L5a", &us_L5a);
  GetLine("L9", &L[9]);
  GetLine("L12a", &L[12]);
  GetLine("L16", &L[16]);
  GetLine("L17a", &us_L17a);
  GetLine("L17b", &us_L17b);
  GetLine("L17c", &us_L17c);
  us_get_cap_gains();
  GetLineF("S1_10", &us_Sched1[10]);
  GetLineF("S1_11", &us_Sched1[11]);
  GetLineF("S1_12", &us_Sched1[12]);
  showline_wlabel("S1_13", us_Sched1[13]);
  GetLineFnz("S1_14", &us_Sched1[14]);
  GetLineFnz("S1_17", &us_Sched1[17]);
  GetLineFnz("S1_18", &us_Sched1[18]);
  GetLineFnz("S1_19", &us_Sched1[19]);
  GetLineFnz("S1_21", &us_Sched1[21]);
  for (j = 10; j <= 21; j++)
    us_Sched1[22] = us_Sched1[22] + us_Sched1[j];

  showline_wlabel("S1_22", us_Sched1[22]);
  GetLineFnz("S1_23", &us_Sched1[23]);
  GetLineFnz("S1_24", &us_Sched1[24]);
  GetLineFnz("S1_25", &us_Sched1[25]);
  GetLineFnz("S1_26", &us_Sched1[26]);
  GetLineFnz("S1_27", &us_Sched1[27]);
  GetLineFnz("S1_28", &us_Sched1[28]);
  GetLineFnz("S1_29", &us_Sched1[29]);
  GetLineFnz("S1_30", &us_Sched1[30]);
  GetLineFnz("S1_31a", &us_Sched1[31]);
  while (!gotS1_32)
  {
    get_parameter(infile, 'l', labelx, "S1_32 or AlimRecipSSN: or AlimRecipName:");
    if (strcmp(labelx, "S1_32") == 0)
    {
      get_parameters(infile, 'f', &tmpval, "S1_32");
      us_Sched1[32] = tmpval;
      showline_wlabelnz("S1_32", us_Sched1[32]);
      gotS1_32 = 1;
    }
    else
      if (strncmp(labelx, "AlimRecipSSN", 12) == 0)
    {
      get_parameter(infile, 'w', word, "AlimRecipSSN:");
      if (strlen(word) > 0)
        fprintf(outfile, " AlimRecipSSN: %s\n", word);

    }
    else
      if (strncmp(labelx, "AlimRecipName", 13) == 0)
    {
      get_parameter(infile, 'w', word, "AlimRecipName:");
      if (strlen(word) > 0)
        fprintf(outfile, " AlimRecipName: %s\n", word);

    }
    else
    {
      printf("ERROR1: Found '%s' when expecting 'S1_32 or AlimRecipSSN: or AlimRecipName:'\n", labelx);
      fprintf(outfile, "ERROR1: Found '%s' when expecting 'S1_32 or AlimRecipSSN: or AlimRecipName:'\n", labelx);
      exit(1);
    }



  }

  us_SocSec_Worksheet();
  for (j = 1; j <= 5; j++)
    L[6] = L[6] + L[j];

  L[6] = L[6] + us_Sched1[22];
  GetLine("S1_33", &us_Sched1[33]);
  if (us_Sched1[33] != 0.0)
  {
    double ws[20];
    double sum = 0.0;
    ws[1] = smallerof(us_Sched1[33], 2500.0);
    ws[2] = L[6];
    for (j = 23; j <= 32; j++)
      sum = sum + us_Sched1[j];

    ws[3] = sum;
    ws[4] = ws[2] - ws[3];
    if (us_status == 2)
      ws[5] = 135000.0;
    else
      ws[5] = 65000.0;

    if (ws[4] > ws[5])
    {
      ws[6] = ws[4] - ws[5];
      if (us_status == 2)
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
    us_Sched1[33] = ws[9];
  }

  showline_wlabel("S1_33", us_Sched1[33]);
  for (j = 23; j <= 35; j++)
    us_Sched1[36] = us_Sched1[36] + us_Sched1[j];

  showline_wlabel("S1_36", us_Sched1[36]);
  showline_wlabel("L5b", L[5]);
  showline_wmsg(6, "Total Income");
  if (us_under65 == 0)
    us_over65 = 1;

  switch (us_status)
  {
    case 1:
      if (us_under65)
      exemption_threshold = 12000.0;
    else
      exemption_threshold = 13600.0;

      break;

    case 2:
      if (us_under65 == 2)
      exemption_threshold = 24000.0;
    else
      if (us_under65 == 1)
      exemption_threshold = 25300.0;
    else
      exemption_threshold = 26600.0;


      if (us_under65 != 2)
      us_over65 = 1;

      break;

    case 3:
      exemption_threshold = 5.0;
      break;

    case 4:
      if (us_under65)
      exemption_threshold = 18000.0;
    else
      exemption_threshold = 19600.0;

      break;

    case 5:
      if (us_under65)
      exemption_threshold = 24000.0;
    else
      exemption_threshold = 25300.0;


  }

  if (L[6] < exemption_threshold)
  {
    printf(" (L6 = %3.2f < Threshold = %3.2f)\n", L[6], exemption_threshold);
    printf("You may not need to file a return, due to your income level.\n");
    fprintf(outfile, "You may not need to file a return, due to your income level.\n");
  }

  L[7] = L[6] - us_Sched1[36];
  showline_wmsg(7, "Adjusted Gross Income");
  GetLine("A1", &us_SchedA[1]);
  us_showschedA(1);
  us_SchedA[2] = L[7];
  us_showschedA(2);
  us_SchedA[3] = 0.075 * us_SchedA[2];
  us_showschedA(3);
  us_SchedA[4] = NotLessThanZero(us_SchedA[1] - us_SchedA[3]);
  us_showschedA(4);
  GetLine("A5a", &localtax[1]);
  showline_wlabel("A5a", localtax[1]);
  GetLine("A5b", &localtax[2]);
  showline_wlabel("A5b", localtax[2]);
  GetLine("A5c", &localtax[3]);
  showline_wlabel("A5c", localtax[3]);
  localtax[4] = (localtax[1] + localtax[2]) + localtax[3];
  showline_wlabel("A5d", localtax[4]);
  if (us_status != 3)
    loctaxlimit = 10000.0;
  else
    loctaxlimit = 5000.0;

  localtax[5] = smallerof(localtax[4], loctaxlimit);
  showline_wlabel("A5e", localtax[5]);
  GetLine("A6", &us_SchedA[6]);
  us_showschedA(6);
  us_SchedA[7] = localtax[5] + us_SchedA[6];
  us_showschedA(7);
  GetLine("A8a", &homemort[1]);
  showline_wlabel("A8a", homemort[1]);
  GetLine("A8b", &homemort[2]);
  showline_wlabel("A8b", homemort[2]);
  GetLine("A8c", &homemort[3]);
  showline_wlabel("A8b", homemort[3]);
  homemort[5] = (homemort[1] + homemort[2]) + homemort[3];
  showline_wlabel("A8e", homemort[5]);
  GetLine("A9", &us_SchedA[14]);
  us_showschedA(9);
  us_SchedA[10] = homemort[5] + us_SchedA[14];
  us_showschedA(10);
  GetLine("A11", &us_SchedA[11]);
  us_showschedA(11);
  GetLine("A12", &us_SchedA[12]);
  us_showschedA(12);
  GetLine("A13", &us_SchedA[13]);
  us_showschedA_wMsg(13, "Carryover from prior year");
  us_SchedA[14] = (us_SchedA[11] + us_SchedA[12]) + us_SchedA[13];
  us_showschedA(14);
  GetLine("A15", &us_SchedA[15]);
  us_showschedA(15);
  GetLine("A16", &us_SchedA[16]);
  us_showschedA(16);
  us_SchedA[17] = ((((us_SchedA[4] + us_SchedA[7]) + us_SchedA[10]) + us_SchedA[14]) + us_SchedA[15]) + us_SchedA[16];
  us_showschedA(17);
  L[8] = us_SchedA[17];
  if (L[8] > 0.0)
    itemize = 1;
  else
    itemize = 0;

  if ((L[2] != 0.0) || (L[3] != 0.0))
  {
    fprintf(outfile, " Schedule-B:\n");
    fprintf(outfile, "  B2 = %6.2f\n", L[2]);
    fprintf(outfile, "  B4 = %6.2f\n", L[2]);
    fprintf(outfile, "  B6 = %6.2f\n", L[3]);
  }

  fprintf(outfile, "StdDedChart_NumBoxesChecked = %d\n", StdDedChart_NumBoxesChecked);
  if (StdDedChart_NumBoxesChecked == 0)
  {
    S_STD_DEDUC = 12000.0;
    MFJ_STD_DEDUC = 24000.0;
    MFS_STD_DEDUC = 12000.0;
    HH_STD_DEDUC = 18000.0;
  }
  else
  {
    switch (StdDedChart_NumBoxesChecked)
    {
      case 1:
        S_STD_DEDUC = 13600.0;
        MFJ_STD_DEDUC = 25300.0;
        MFS_STD_DEDUC = 13300.0;
        HH_STD_DEDUC = 19600.0;
        break;

      case 2:
        S_STD_DEDUC = 15200.0;
        MFJ_STD_DEDUC = 26600.0;
        MFS_STD_DEDUC = 14600.0;
        HH_STD_DEDUC = 21200.0;
        break;

      case 3:
        MFJ_STD_DEDUC = 27900.0;
        MFS_STD_DEDUC = 15900.0;
        S_STD_DEDUC = 15200.0;
        HH_STD_DEDUC = 21200.0;
        break;

      case 4:
        MFJ_STD_DEDUC = 29200.0;
        MFS_STD_DEDUC = 17200.0;
        S_STD_DEDUC = 15200.0;
        HH_STD_DEDUC = 21200.0;
        break;

      default:
        fprintf(outfile, "Error: StdDedChart_NumBoxesChecked (%d) not equal to 1, 2, 3, or 4.\n", StdDedChart_NumBoxesChecked);
        printf("Error: StdDedChart_NumBoxesChecked (%d) not equal to 1, 2, 3, or 4.\n", StdDedChart_NumBoxesChecked);
        exit(1);

    }

    fprintf(outfile, "(Assuming no one is claiming your or your joint-spouse as a dependent.)\n");
  }

  switch (us_status)
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
      printf("Case (Line 8) not handled.\n");
      fprintf(outfile, "Case (Line 8) not handled.\n");
      exit(1);

  }

  if (L[8] <= std_deduc)
  {
    printf("	(Itemizations < Std-Deduction, %6.2f < %6.2f)\n", L[8], std_deduc);
    fprintf(outfile, "	(Itemizations < Std-Deduction, %6.2f < %6.2f)\n", L[8], std_deduc);
    L[8] = std_deduc;
    fprintf(outfile, "Use standard deduction.\n");
    itemize = 0;
  }
  else
  {
    printf("	(Itemizations > Std-Deduction, %6.2f > %6.2f)\n", L[8], std_deduc);
    fprintf(outfile, "	(Itemizations > Std-Deduction, %6.2f > %6.2f)\n", L[8], std_deduc);
    fprintf(outfile, "Itemizing.\n");
  }

  showline(8);
  showline(9);
  L[10] = NotLessThanZero((L[7] - L[8]) - L[9]);
  showline_wmsg(10, "Taxable Income");
  us_L11a = us_TaxRateFunction(L[10], us_status);
  if (L[10] <= 0.0)
  {
    printf(" Exception (Sched-D Instructions page 14) - Do not use QDCGT or Sched-D Tax Worksheets.\n");
  }
  else
  {
    if (((!us_Do_SDTW) && (!us_Do_QDCGTW)) && (((L[3] > 0.0) || (us_Sched1[13] > 0.0)) || ((us_SchedD[15] > 0.0) && (us_SchedD[16] > 0.0))))
      us_Do_QDCGTW = 1;

    if (us_Do_QDCGTW)
    {
      fprintf(outfile, "Doing 'Qualified Dividends and Capital Gain tax Worksheet', page 44.\n");
      us_capgains_qualdividends_worksheets(us_status);
    }
    else
      if (us_Do_SDTW)
    {
      fprintf(outfile, "Doing 'Schedule D Tax Worksheet', page D9.\n");
      us_sched_D_tax_worksheet(us_status);
    }


  }

  showline_wlabel("L11a", us_L11a);
  while (!gotS2_46)
  {
    get_parameter(infile, 'l', labelx, "S2_46 or AMTwsXX or B7a");
    if (strcmp(labelx, "S2_46") == 0)
    {
      get_parameters(infile, 'f', &tmpval, labelx);
      us_Sched2[46] = tmpval;
      gotS2_46 = 1;
    }
    else
      if (strcasecmp(labelx, "AMTws2c") == 0)
    {
      get_parameters(infile, 'f', &us_amtws2c, labelx);
    }
    else
      if (strcasecmp(labelx, "AMTws2g") == 0)
    {
      get_parameters(infile, 'f', &us_amtws2g, labelx);
    }
    else
      if (strstr(labelx, "AMTws") != 0)
    {
      get_parameters(infile, 'f', &tmpval, labelx);
      if (((sscanf(&labelx[5], "%d", &j) == 1) && (j >= 3)) && (j < 3))
        us_amtws[j] = tmpval;
      else
      {
        printf("ERROR reading '%s'.\n", labelx);
        fprintf(outfile, "ERROR reading '%s'.\n", labelx);
      }

    }
    else
      if (strcmp(labelx, "B7a") == 0)
    {
      get_parameters(infile, 'b', &SchedB7a, labelx);
    }
    else
      if (strcmp(labelx, "B7aa") == 0)
    {
      get_parameters(infile, 'b', &SchedB7aa, labelx);
    }
    else
      if (strcmp(labelx, "B7b") == 0)
    {
      get_parameters(infile, 'w', &SchedB7b, labelx);
    }
    else
      if (strcmp(labelx, "B8") == 0)
    {
      get_parameters(infile, 'b', &SchedB8, labelx);
    }
    else
    {
      printf("ERROR1: Found '%s' when expecting 'S2_46 or AMTwsXX or B7a'\n", labelx);
      fprintf(outfile, "ERROR1: Found '%s' when expecting 'S2_46 or AMTwsXX'\n", labelx);
      exit(1);
    }








  }

  if (SchedB7a)
    fprintf(outfile, "CkB7a_Y X\n");
  else
    fprintf(outfile, "CkB7a_N X\n");

  if (SchedB7aa == 1)
    fprintf(outfile, "CkB7aa_Y X\n");
  else
    if (SchedB7aa == 0)
    fprintf(outfile, "CkB7aa_N X\n");


  if (strlen(SchedB7b) > 0)
    fprintf(outfile, "B7b = %s\n", SchedB7b);

  if (SchedB8)
    fprintf(outfile, "CkB8_Y X\n");
  else
    fprintf(outfile, "CkB8_N X\n");

  GetLine("S3_48", &us_Sched3[48]);
  us_Sched2[45] = us_form6251_AlternativeMinimumTax(itemize);
  if (us_Sched2[45] == 0.0)
    fprintf(outfile, " (Not subject to Alternative Minimum Tax.)\n");
  else
  {
    fprintf(outfile, " (You must pay Alternative Minimum Tax.)\n");
    showline_wlabelmsg("S2_45", us_Sched2[45], "Alternative Minimum Tax");
  }

  showline_wlabel("S2_46", us_Sched2[46]);
  us_Sched2[47] = us_Sched2[45] + us_Sched2[46];
  showline_wlabel("S2_47", us_Sched2[47]);
  L[11] = us_L11a + us_Sched2[47];
  showline(11);
  us_Report_bracket_info(L[10], us_Sched2[47], us_status);
  showline_wlabel("S3_48", us_Sched3[48]);
  GetLine("S3_49", &us_Sched3[49]);
  showline_wlabel("S3_49", us_Sched3[49]);
  GetLine("S3_50", &us_Sched3[50]);
  showline_wlabel("S3_50", us_Sched3[50]);
  GetLine("S3_51", &us_Sched3[51]);
  showline_wlabel("S3_51", us_Sched3[51]);
  GetLine("S3_53", &us_Sched3[53]);
  showline_wlabel("S3_53", us_Sched3[53]);
  GetLine("S3_54", &us_Sched3[54]);
  showline_wlabel("S3_54", us_Sched3[54]);
  for (j = 48; j <= 54; j++)
    us_Sched3[55] = us_Sched3[55] + us_Sched3[j];

  showline_wlabel("S3_55", us_Sched3[55]);
  L[12] = L[12] + us_Sched3[55];
  showline(12);
  L[13] = NotLessThanZero(L[11] - L[12]);
  showline(13);
  GetLine("S4_57", &us_Sched4[57]);
  showline_wlabelnz("S4_57", us_Sched4[57]);
  GetLine("S4_58", &us_Sched4[58]);
  showline_wlabelnz("S4_58", us_Sched4[58]);
  GetLine("S4_59", &us_Sched4[59]);
  showline_wlabelnz("S4_59", us_Sched4[59]);
  GetLine("S4_60a", &us_Sched4[60]);
  showline_wlabelnz("S4_60a", us_Sched4[60]);
  GetLine("S4_60b", &us_S4_60b);
  showline_wlabelnz("S4_60b", us_S4_60b);
  us_Sched4[60] = us_Sched4[60] + us_S4_60b;
  GetLine("S4_61", &us_Sched4[61]);
  showline_wlabelnz("S4_61", us_Sched4[61]);
  GetLine("S4_62", &us_Sched4[62]);
  showline_wlabelnz("S4_62", us_Sched4[62]);
  GetLine("S4_63", &us_Sched4[63]);
  showline_wlabelnz("S4_63", us_Sched4[63]);
  for (j = 57; j <= 62; j++)
    us_Sched4[64] = us_Sched4[64] + us_Sched4[j];

  showline_wlabel("S4_64", us_Sched4[64]);
  L[14] = us_Sched4[64];
  showline(14);
  L[15] = L[13] + L[14];
  showline_wmsg(15, "Total Tax");
  showline(16);
  GetLine("S5_66", &us_Sched5[66]);
  showline_wlabelnz("S5_66", us_Sched5[66]);
  GetLine("S5_70", &us_Sched5[70]);
  showline_wlabelnz("S5_70", us_Sched5[70]);
  GetLine("S5_71", &us_Sched5[71]);
  showline_wlabelnz("S5_71", us_Sched5[71]);
  GetLine("S5_72", &us_Sched5[72]);
  showline_wlabelnz("S5_72", us_Sched5[72]);
  GetLine("S5_73", &us_Sched5[73]);
  showline_wlabelnz("S5_73", us_Sched5[73]);
  GetLine("S5_74", &us_Sched5[74]);
  showline_wlabelnz("S5_74", us_Sched5[74]);
  for (j = 66; j <= 74; j++)
    us_Sched5[75] = us_Sched5[75] + us_Sched5[j];

  showline_wlabelnz("S5_75", us_Sched5[75]);
  showline_wlabelnz("L17a", us_L17a);
  showline_wlabelnz("L17b", us_L17b);
  showline_wlabelnz("L17c", us_L17c);
  L[17] = ((us_L17a + us_L17b) + us_L17c) + us_Sched5[75];
  showline(17);
  L[18] = L[16] + L[17];
  showline_wmsg(18, "Total Payments");
  if (L[18] > L[15])
  {
    L[19] = L[18] - L[15];
    fprintf(outfile, "L19 = %6.2f  Amount you Overpaid!!!\n", L[19]);
    fprintf(outfile, "L20a = %6.2f \n", L[19]);
  }
  else
  {
    L[22] = L[15] - L[18];
    fprintf(outfile, "L22 = %6.2f  DUE !!!\n", L[22]);
    fprintf(outfile, "         (Which is %2.1f%% of your Total Federal Tax.)\n", (100.0 * L[22]) / (L[15] + 1e-9));
  }

  fprintf(outfile, "------------------------------\n");
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
  us_Grab_ScheduleB_Payer_Lines(infname, outfile);
  grab_any_pdf_markups(infname, outfile);
  fclose(outfile);
  printf("\nListing results from file: %s\n\n", outfname);
  Display_File(outfname);
  return 0;
}


/* END of taxsolve_US_1040_2018.c */
/* START of taxsolve_US_1040_Sched_C_2018.c */
float us_c_thisversion = 16.02;
int us_c_main(int argc, char *argv[])
{
  int i;
  int j;
  int k;
  char word[4000];
  char outfname[4000];
  char *EIN = 0;
  char *answ;
  char *infname = 0;
  time_t now;
  double L16b = 0.0;
  double L20b = 0.0;
  double L24b = 0.0;
  double Mileage = 0.0;
  int L32;
  printf("US 1040 Schedule C, 2018 - v%3.2f\n", us_c_thisversion);
  i = 1;
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
      infname = strdup(argv[i]);
      infile = fopen(infname, "r");
      if (infile == 0)
      {
        printf("ERROR: Parameter file '%s' could not be opened.\n", infname);
        exit(1);
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
        printf("ERROR: Output file '%s' could not be opened.\n", outfname);
        exit(1);
      }

      printf("Writing results to file:  %s\n", outfname);
    }
    else
    {
      printf("Unknown command-line parameter '%s'\n", argv[i]);
      exit(1);
    }


    i = i + 1;
  }

  if (infile == 0)
  {
    printf("Error: No input file on command line.\n");
    exit(1);
  }

  for (i = 0; i < 1000; i++)
  {
    L[i] = 0.0;
  }

  read_line(infile, word);
  now = time(0);
  fprintf(outfile, "\n%s,	 v%2.2f, %s\n", word, us_c_thisversion, ctime(&now));
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
  L[9] = L[9] + (0.545 * Mileage);
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
  GetLine("L27a", &L[27]);
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
    printf("Warning: Unexpted answer for L33: '%s'\n", word);




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
    fprintf(outfile, "Enter %2.2f on Form 1040 line S1_12. Sched-SE line 2. Estates/trusts on Form 1041 line 3.\n", L[31]);
  else
    if (L[31] < 0.0)
  {
    if (L32 == 1)
    {
      fprintf(outfile, "If you checked 32a, enter %2.2f on Form 1040 line S1_12.\n", L[31]);
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
  grab_any_pdf_markups(infname, outfile);
  fclose(outfile);
  printf("\nListing results from file: %s\n\n", outfname);
  Display_File(outfname);
  return 0;
}


/* END of taxsolve_US_1040_Sched_C_2018.c */
/* START of taxsolve_CA_540_2018.c */
float ca_thisversion = 16.04;
int ca_status = 0;
double ca_sched540part2[1000];
double ca_sched540part2_sub[1000];
double ca_sched540part2_add[1000];
double ca_sched540part2_5a = 0.0;
double ca_sched540part2_5b = 0.0;
double ca_sched540part2_5c = 0.0;
double ca_sched540part2_5d = 0.0;
double ca_sched540part2_8a = 0.0;
double ca_sched540part2_8b = 0.0;
double ca_sched540part2_8c = 0.0;
double ca_sched540part2_add8a = 0.0;
double ca_sched540part2_add8b = 0.0;
double ca_sched540part2_add8c = 0.0;
double ca_TaxRateFormula(double income, int ca_status)
{
  double tax;
  if ((ca_status == 1) || (ca_status == 3))
  {
    if (income < 8544.00)
      tax = 0.01 * income;
    else
      if (income < 20255.00)
      tax = 85.44 + (0.02 * (income - 8544.00));
    else
      if (income < 31969.00)
      tax = 319.66 + (0.04 * (income - 20255.00));
    else
      if (income < 44377.00)
      tax = 788.22 + (0.06 * (income - 31969.00));
    else
      if (income < 56085.00)
      tax = 1532.70 + (0.08 * (income - 44377.00));
    else
      if (income < 286492.00)
      tax = 2469.34 + (0.093 * (income - 56085.00));
    else
      if (income < 343788.00)
      tax = 23897.19 + (0.103 * (income - 286492.00));
    else
      if (income < 572980.00)
      tax = 29798.68 + (0.113 * (income - 343788.00));
    else
      tax = 55697.38 + (0.123 * (income - 572980.00));








  }
  else
    if ((ca_status == 2) || (ca_status == 5))
  {
    if (income < 17088.00)
      tax = 0.01 * income;
    else
      if (income < 40510.00)
      tax = 170.88 + (0.02 * (income - 17088.00));
    else
      if (income < 63938.00)
      tax = 639.32 + (0.04 * (income - 40510.00));
    else
      if (income < 88754.00)
      tax = 1576.44 + (0.06 * (income - 63938.00));
    else
      if (income < 112170.00)
      tax = 3065.40 + (0.08 * (income - 88754.00));
    else
      if (income < 572984.00)
      tax = 4938.68 + (0.093 * (income - 112170.00));
    else
      if (income < 687576.00)
      tax = 47794.38 + (0.103 * (income - 572984.00));
    else
      if (income < 1145960.00)
      tax = 59597.36 + (0.113 * (income - 687576.00));
    else
      tax = 111394.75 + (0.123 * (income - 1145960.00));








  }
  else
  {
    if (income < 17099.00)
      tax = 0.01 * income;
    else
      if (income < 40512.00)
      tax = 170.99 + (0.02 * (income - 17099.00));
    else
      if (income < 52224.00)
      tax = 639.25 + (0.04 * (income - 40512.00));
    else
      if (income < 64632.00)
      tax = 1107.73 + (0.06 * (income - 52224.00));
    else
      if (income < 76343.00)
      tax = 1852.21 + (0.08 * (income - 64632.00));
    else
      if (income < 389627.00)
      tax = 2789.09 + (0.093 * (income - 76343.00));
    else
      if (income < 467553.00)
      tax = 31924.50 + (0.103 * (income - 389627.00));
    else
      if (income < 779253.00)
      tax = 39950.88 + (0.113 * (income - 467553.00));
    else
      tax = 75172.98 + (0.123 * (income - 779253.00));








  }


  return (int) (tax + 0.5);
}

void ca_Report_bracket_info(double income, int ca_status)
{
  double tx;
  double rate;
  tx = ca_TaxRateFormula(income, ca_status);
  if ((ca_status == 1) || (ca_status == 3))
  {
    if (income < 8544.00)
      rate = 0.01;
    else
      if (income < 20255.00)
      rate = 0.02;
    else
      if (income < 31969.00)
      rate = 0.04;
    else
      if (income < 44377.00)
      rate = 0.06;
    else
      if (income < 56085.00)
      rate = 0.08;
    else
      if (income < 286492.00)
      rate = 0.093;
    else
      if (income < 343788.00)
      rate = 0.103;
    else
      if (income < 572980.00)
      rate = 0.113;
    else
      rate = 0.123;








  }
  else
    if ((ca_status == 2) || (ca_status == 5))
  {
    if (income < 17088.00)
      rate = 0.01;
    else
      if (income < 40510.00)
      rate = 0.02;
    else
      if (income < 63938.00)
      rate = 0.04;
    else
      if (income < 88754.00)
      rate = 0.06;
    else
      if (income < 112170.00)
      rate = 0.08;
    else
      if (income < 572984.00)
      rate = 0.093;
    else
      if (income < 687576.00)
      rate = 0.103;
    else
      if (income < 1145960.0)
      rate = 0.113;
    else
      rate = 0.123;








  }
  else
  {
    if (income < 17099.00)
      rate = 0.01;
    else
      if (income < 40512.00)
      rate = 0.02;
    else
      if (income < 52224.00)
      rate = 0.04;
    else
      if (income < 64632.00)
      rate = 0.06;
    else
      if (income < 76343.00)
      rate = 0.08;
    else
      if (income < 389627.00)
      rate = 0.093;
    else
      if (income < 467553.00)
      rate = 0.103;
    else
      if (income < 779253.00)
      rate = 0.113;
    else
      rate = 0.123;








  }


  printf(" You are in the %2.1f%% marginal tax bracket,\n and you are paying an effective %2.1f%% tax on your total income.\n", 100.0 * rate, (100.0 * tx) / income);
  fprintf(outfile, " You are in the %2.1f%% marginal tax bracket,\n and you are paying an effective %2.1f%% tax on your total income.\n", 100.0 * rate, (100.0 * tx) / income);
}

double ca_TaxRateFunction(double income, int ca_status)
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

    tx = (int) ca_TaxRateFormula(x, ca_status);
  }
  else
    tx = ca_TaxRateFormula(income, ca_status);

  return tx;
}

void ca_test_tax_function()
{
  double income;
  for (income = 50.0; income < 100000.0; income = income + 100.0)
    printf("%g: %8g %8g %8g\n", income, ca_TaxRateFunction(income, 1), ca_TaxRateFunction(income, 2), ca_TaxRateFunction(income, 4));

  exit(0);
}

struct ca_FedReturnData
{
  double fedline[1000];
  double schedA[1000];
  double schedA5a;
  double schedA5b;
  double schedA5c;
  double schedA8a;
  double schedA8b;
  double schedA8c;
  double sched1[1000];
  double fedl8b;
  double fedl9b;
  double fedl15a;
  double fedl16a;
  double fedl20a;
  int Exception;
  int Itemized;
  char AlimRecipSSN[256];
  char AlimRecipName[1024];
} ca_PrelimFedReturn;
void ca_convert_slashes(char *fname)
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

void ca_grab_line_value(char *label, char *fline, double *value)
{
  char twrd[2048];
  next_word(fline, twrd, " \t=;");
  if ((twrd[0] != '\0') && (sscanf(twrd, "%lf", value) != 1))
  {
    printf("Error: Reading Fed %s '%s%s'\n", label, twrd, fline);
    fprintf(outfile, "Error: Reading Fed %s '%s%s'\n", label, twrd, fline);
  }

}

void ca_grab_line_string(char *fline, char *strng)
{
  char twrd[2048];
  strng[0] = '\0';
  do
  {
    next_word(fline, twrd, " \t=");
    if (twrd[0] != ';')
    {
      strcat(strng, twrd);
      strcat(strng, " ");
    }

  }
  while ((fline[0] != '\0') && (strstr(twrd, ";") == 0));
}

int ca_ImportFederalReturnData(char *fedlogfile, struct ca_FedReturnData *fed_data)
{
  FILE *infile;
  char fline[2000];
  char word[2000];
  char tword[2000];
  int linenum;
  for (linenum = 0; linenum < 1000; linenum++)
  {
    fed_data->fedline[linenum] = 0.0;
    fed_data->schedA[linenum] = 0.0;
    fed_data->sched1[linenum] = 0.0;
  }

  fed_data->schedA5a = 0.0;
  fed_data->schedA5b = 0.0;
  fed_data->schedA5c = 0.0;
  fed_data->schedA8a = 0.0;
  fed_data->schedA8b = 0.0;
  fed_data->schedA8c = 0.0;
  fed_data->fedl8b = 0.0;
  fed_data->fedl9b = 0.0;
  fed_data->fedl15a = 0.0;
  fed_data->fedl16a = 0.0;
  fed_data->fedl20a = 0.0;
  strcpy(fed_data->AlimRecipSSN, "");
  strcpy(fed_data->AlimRecipName, "");
  ca_convert_slashes(fedlogfile);
  infile = fopen(fedlogfile, "r");
  if (infile == 0)
  {
    printf("Error: Could not open Federal return '%s'\n", fedlogfile);
    fprintf(outfile, "Error: Could not open Federal return '%s'\n", fedlogfile);
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
        ca_grab_line_value(word, fline, &fed_data->fedline[8]);
      else
        if (strcmp(word, "L8b") == 0)
        ca_grab_line_value(word, fline, &fed_data->fedl8b);
      else
        if (strcmp(word, "L9a") == 0)
        ca_grab_line_value(word, fline, &fed_data->fedline[9]);
      else
        if (strcmp(word, "L9b") == 0)
        ca_grab_line_value(word, fline, &fed_data->fedl9b);
      else
        if (strcmp(word, "L15a") == 0)
        ca_grab_line_value(word, fline, &fed_data->fedl15a);
      else
        if (strcmp(word, "L15b") == 0)
        ca_grab_line_value(word, fline, &fed_data->fedline[15]);
      else
        if (strcmp(word, "L16a") == 0)
        ca_grab_line_value(word, fline, &fed_data->fedl16a);
      else
        if (strcmp(word, "L16b") == 0)
        ca_grab_line_value(word, fline, &fed_data->fedline[16]);
      else
        if (strcmp(word, "L20a") == 0)
        ca_grab_line_value(word, fline, &fed_data->fedl20a);
      else
        if (strcmp(word, "L20b") == 0)
        ca_grab_line_value(word, fline, &fed_data->fedline[20]);
      else
      {
        if (sscanf(&word[1], "%d", &linenum) != 1)
        {
          printf("Error: Reading Fed line number '%s%s'\n", word, fline);
          fprintf(outfile, "Error: Reading Fed line number '%s%s'\n", word, fline);
        }

        next_word(fline, word, " \t=");
        if (sscanf(word, "%lf", &fed_data->fedline[linenum]) != 1)
        {
          printf("Error: Reading Fed line %d '%s%s'\n", linenum, word, fline);
          fprintf(outfile, "Error: Reading Fed line %d '%s%s'\n", linenum, word, fline);
        }

        if (verbose)
          printf("FedLin[%d] = %2.2f\n", linenum, fed_data->fedline[linenum]);

      }










    }
    else
      if (strncmp(word, "AlimRecipSSN", 12) == 0)
      ca_grab_line_string(fline, fed_data->AlimRecipSSN);
    else
      if (strncmp(word, "AlimRecipName", 13) == 0)
      ca_grab_line_string(fline, fed_data->AlimRecipName);
    else
      if (((word[0] == 'A') && (strstr(word, "AMT") != word)) && (strstr(fline, " = ") != 0))
    {
      if (strcmp(word, "A5a") == 0)
        ca_grab_line_value(word, fline, &fed_data->schedA5a);
      else
        if (strcmp(word, "A5b") == 0)
        ca_grab_line_value(word, fline, &fed_data->schedA5b);
      else
        if (strcmp(word, "A5c") == 0)
        ca_grab_line_value(word, fline, &fed_data->schedA5c);
      else
        if (strcmp(word, "A8a") == 0)
        ca_grab_line_value(word, fline, &fed_data->schedA8a);
      else
        if (strcmp(word, "A8b") == 0)
        ca_grab_line_value(word, fline, &fed_data->schedA8b);
      else
        if (strcmp(word, "A8c") == 0)
        ca_grab_line_value(word, fline, &fed_data->schedA8c);
      else
      {
        if (sscanf(&word[1], "%d", &linenum) != 1)
        {
          printf("Error: Reading Fed line number '%s%s'\n", word, fline);
          fprintf(outfile, "Error: Reading Fed line number '%s%s'\n", word, fline);
        }

        next_word(fline, word, " \t=");
        if (sscanf(word, "%lf", &fed_data->schedA[linenum]) != 1)
        {
          printf("Error: Reading Fed schedA %d '%s%s'\n", linenum, word, fline);
          fprintf(outfile, "Error: Reading Fed schedA %d '%s%s'\n", linenum, word, fline);
        }

        if (verbose)
          printf("FedLin.A[%d] = %2.2f\n", linenum, fed_data->schedA[linenum]);

      }






    }
    else
      if ((strncmp(word, "S1_", 3) == 0) && (strstr(fline, " = ") != 0))
    {
      next_word(&word[3], tword, " \t: =");
      if (sscanf(tword, "%d", &linenum) != 1)
      {
        printf("Error: Reading Fed line number 'S1_%s %s'\n", tword, fline);
        fprintf(outfile, "Error: Reading Fed line number 'S1_%s %s'\n", tword, fline);
      }

      next_word(fline, word, " \t=");
      if (sscanf(word, "%lf", &fed_data->sched1[linenum]) != 1)
      {
        printf("Error: Reading Fed sched1 %d '%s%s'\n", linenum, word, fline);
        fprintf(outfile, "Error: Reading Fed sched1 %d '%s%s'\n", linenum, word, fline);
      }

      if (verbose)
        printf("FedLin.S1[%d] = %2.2f\n", linenum, fed_data->sched1[linenum]);

    }
    else
      if (strcmp(word, "Status") == 0)
    {
      next_word(fline, word, " \t=");
      if (strncasecmp(word, "Single", 4) == 0)
        ca_status = 1;
      else
        if (strncasecmp(word, "Married/Joint", 13) == 0)
        ca_status = 2;
      else
        if (strncasecmp(word, "Married/Sep", 11) == 0)
        ca_status = 3;
      else
        if (strncasecmp(word, "Head_of_House", 4) == 0)
        ca_status = 4;
      else
        if (strncasecmp(word, "Widow", 4) == 0)
        ca_status = 5;
      else
      {
        printf("Error: unrecognized status '%s'. Exiting.\n", word);
        fprintf(outfile, "Error: unrecognized status '%s'. Exiting.\n", word);
        return 0;
      }





    }






    read_line(infile, fline);
  }

  fclose(infile);
  return 1;
}

char *ca_pull_initial(char *name)
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

void ca_display_part2column(int j, int col)
{
  switch (col)
  {
    case 0:
      if (ca_sched540part2[j] != 0.0)
      fprintf(outfile, " SchedCA540_Part2_%d = %6.2f\n", j, ca_sched540part2[j]);

      break;

    case 'a':
      if (ca_sched540part2[j] != 0.0)
      fprintf(outfile, " SchedCA540_Part2_%da = %6.2f\n", j, ca_sched540part2[j]);

      break;

    case 'b':
      if (ca_sched540part2_sub[j] != 0.0)
      fprintf(outfile, " SchedCA540_Part2_%db = %6.2f\n", j, ca_sched540part2_sub[j]);

      break;

    case 'c':
      if (ca_sched540part2_add[j] != 0.0)
      fprintf(outfile, " SchedCA540_Part2_%dc = %6.2f\n", j, ca_sched540part2_add[j]);

      break;

    default:
      fprintf(outfile, " Bad Case\n");

  }

}

void ca_display_part2(int j)
{
  ca_display_part2column(j, 'a');
  ca_display_part2column(j, 'b');
  ca_display_part2column(j, 'c');
}

int ca_main(int argc, char *argv[])
{
  int argk;
  int j;
  int k;
  int iline7;
  int iline8;
  int iline9;
  int iline10;
  double min2file = 0.0;
  double sched540[1000];
  double sched540b[1000];
  double sched540c[1000];
  double threshA = 0;
  double std_ded = 0;
  char word[4000];
  char *infname = 0;
  char outfname[4000];
  char prelim_1040_outfilename[5000];
  char *Your1stName = "";
  char *YourLastName = "";
  char YourName[2048] = "";
  char YourNames[2048] = "";
  char *YourMidInitial = "";
  char *SpouseMidInitial = "";
  char *Spouse1stName = "";
  char *SpouseLastName = "";
  char *socsec;
  double sched540b21a = 0.0;
  double sched540b21b = 0.0;
  double sched540c21c = 0.0;
  double sched540b21d = 0.0;
  double sched540b21e = 0.0;
  double sched540b21f = 0.0;
  double sched540c21f = 0.0;
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
      infname = strdup(argv[argk]);
      infile = fopen(argv[argk], "r");
      if (infile == 0)
      {
        printf("ERROR: Parameter file '%s' could not be opened.\n", argv[argk]);
        exit(1);
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
        printf("ERROR: Output file '%s' could not be opened.\n", outfname);
        exit(1);
      }

      printf("Writing results to file:  %s\n", outfname);
    }
    else
    {
      printf("Unknown command-line parameter '%s'\n", argv[argk]);
      exit(1);
    }


    argk = argk + 1;
  }

  if (infile == 0)
  {
    printf("Error: No input file on command line.\n");
    exit(1);
  }

  for (j = 0; j < 1000; j++)
  {
    L[j] = 0.0;
    sched540[j] = 0.0;
    sched540b[j] = 0.0;
    sched540c[j] = 0.0;
    ca_sched540part2[j] = 0.0;
    ca_sched540part2_sub[j] = 0.0;
    ca_sched540part2_add[j] = 0.0;
  }

  printf("CA-540 2018 - v%3.2f\n", ca_thisversion);
  read_line(infile, word);
  now = time(0);
  fprintf(outfile, "\n%s,	 v%2.2f, %s\n", word, ca_thisversion, ctime(&now));
  get_parameter(infile, 's', word, "FileName");
  get_word(infile, prelim_1040_outfilename);
  ca_ImportFederalReturnData(prelim_1040_outfilename, &ca_PrelimFedReturn);
  switch (ca_status)
  {
    case 1:
      fprintf(outfile, "Status = Single (%d)\nCkSingle: X\nL7a = 1\n", ca_status);
      break;

    case 2:
      fprintf(outfile, "Status = Married/Joint (%d)\nCkMFJ: X\nL7a = 2\n", ca_status);
      break;

    case 3:
      fprintf(outfile, "Status = Married/Sep (%d)\nCkMFS: X\nL7a = 1\n", ca_status);
      break;

    case 4:
      fprintf(outfile, "Status = Head_of_Household (%d)\nCkHH: X\nL7a = 1\n", ca_status);
      break;

    case 5:
      fprintf(outfile, "Status = Widow(er) (%d)\nCkQW: X\nL7a = 1\n", ca_status);
      break;

  }

  fprintf(outfile, "\nStep-2 fill-in box %d\n", ca_status);
  get_parameter(infile, 's', word, "L6");
  get_parameter(infile, 'b', &j, "L6");
  L[6] = j;
  if (L[6] == 0)
    fprintf(outfile, " L6 = no\n");
  else
    fprintf(outfile, " L6 = yes, (check box on line 6).\n  CkDep: X\n");

  if (((ca_status == 1) || (ca_status == 3)) || (ca_status == 4))
    iline7 = 1;
  else
    iline7 = 2;

  if (L[6] != 0.0)
    iline7 = 0;

  L[7] = 118.0 * iline7;
  showline(7);
  get_parameter(infile, 's', word, "L8");
  get_parameter(infile, 'i', &iline8, "L8");
  L[8] = iline8 * 118.0;
  showline(8);
  if (iline8 > 0)
    fprintf(outfile, "  L8a = %d\n", iline8);

  get_parameter(infile, 's', word, "L9");
  get_parameter(infile, 'i', &iline9, "L9");
  L[9] = iline9 * 118.0;
  showline(9);
  if (iline9 > 0)
    fprintf(outfile, "  L9a = %d\n", iline9);

  get_parameter(infile, 's', word, "L10");
  get_parameter(infile, 'i', &iline10, "L10");
  L[10] = iline10 * 367.0;
  showline(10);
  if (iline10 > 0)
    fprintf(outfile, "  L10a = %d\n", iline10);

  L[11] = ((L[7] + L[8]) + L[9]) + L[10];
  showline_wmsg(11, "Exemption amount");
  GetLineF("L12", &L[12]);
  L[13] = ca_PrelimFedReturn.fedline[7];
  showline(13);
  GetLine("CA540_Subtr_1", &sched540b[1]);
  GetLine("CA540_Addit_1", &sched540c[1]);
  GetLine("CA540_Subtr_2", &sched540b[2]);
  GetLine("CA540_Addit_2", &sched540c[2]);
  GetLine("CA540_Subtr_3", &sched540b[3]);
  GetLine("CA540_Addit_3", &sched540c[3]);
  GetLine("CA540_Subtr_4", &sched540b[4]);
  GetLine("CA540_Addit_4", &sched540c[4]);
  GetLine("CA540_Subtr_10", &sched540b[10]);
  GetLine("CA540_Addit_11", &sched540c[11]);
  GetLine("CA540_Subtr_12", &sched540b[12]);
  GetLine("CA540_Addit_12", &sched540c[12]);
  GetLine("CA540_Subtr_13", &sched540b[13]);
  GetLine("CA540_Addit_13", &sched540c[13]);
  GetLine("CA540_Subtr_14", &sched540b[14]);
  GetLine("CA540_Addit_14", &sched540c[14]);
  GetLine("CA540_Subtr_17", &sched540b[17]);
  GetLine("CA540_Addit_17", &sched540c[17]);
  GetLine("CA540_Subtr_18", &sched540b[18]);
  GetLine("CA540_Addit_18", &sched540c[18]);
  GetLine("CA540_Subtr_19", &sched540b[19]);
  GetLine("CA540_Subtr_21a", &sched540b21a);
  GetLine("CA540_Subtr_21b", &sched540b21b);
  GetLine("CA540_Addit_21c", &sched540c21c);
  GetLine("CA540_Subtr_21d", &sched540b21d);
  GetLine("CA540_Subtr_21e", &sched540b21e);
  GetLine("CA540_Subtr_21f", &sched540b21f);
  GetLine("CA540_Addit_21f", &sched540c21f);
  GetLine("CA540_Subtr_23", &sched540b[23]);
  GetLine("CA540_Subtr_24", &sched540b[24]);
  GetLine("CA540_Addit_24", &sched540c[24]);
  GetLine("CA540_Subtr_25", &sched540b[25]);
  GetLine("CA540_Addit_31", &sched540c[31]);
  GetLine("CA540_Addit_33", &sched540c[33]);
  for (j = 1; j <= 20; j++)
  {
    if (j <= 5)
      sched540[j] = ca_PrelimFedReturn.fedline[j];
    else
      sched540[j] = ca_PrelimFedReturn.sched1[j];

    sched540[22] = sched540[22] + sched540[j];
    if (sched540[j] != 0.0)
      fprintf(outfile, " SchedCA540_%d = %6.2f\n", j, sched540[j]);

    if (j == 5)
      sched540b[j] = sched540[j];

    sched540b[22] = sched540b[22] + sched540b[j];
    if (sched540b[j] != 0.0)
      fprintf(outfile, " SchedCA540_%db = %6.2f\n", j, sched540b[j]);

    sched540c[22] = sched540c[22] + sched540c[j];
    if (sched540c[j] != 0.0)
      fprintf(outfile, " SchedCA540_%dc = %6.2f\n", j, sched540c[j]);

  }

  sched540[21] = ca_PrelimFedReturn.sched1[21];
  showline_wlabelnz(" SchedCA540_21", sched540[21]);
  sched540[22] = sched540[22] + sched540[21];
  showline_wlabelnz(" SchedCA540_21ba", sched540b21a);
  sched540b[22] = sched540b[22] + sched540b21a;
  showline_wlabelnz(" SchedCA540_21bb", sched540b21b);
  sched540b[22] = sched540b[22] + sched540b21b;
  showline_wlabelnz(" SchedCA540_21cc", sched540c21c);
  sched540c[22] = sched540c[22] + sched540c21c;
  showline_wlabelnz(" SchedCA540_21bd", sched540b21d);
  sched540b[22] = sched540b[22] + sched540b21d;
  showline_wlabelnz(" SchedCA540_21be", sched540b21e);
  sched540b[22] = sched540b[22] + sched540b21e;
  showline_wlabelnz(" SchedCA540_21bf", sched540b21f);
  sched540b[22] = sched540b[22] + sched540b21f;
  showline_wlabelnz(" SchedCA540_21cf", sched540c21f);
  sched540c[22] = sched540c[22] + sched540c21f;
  fprintf(outfile, " SchedCA540_%d = %6.2f\n", 22, sched540[22]);
  fprintf(outfile, " SchedCA540_%db = %6.2f\n", 22, sched540b[22]);
  fprintf(outfile, " SchedCA540_%dc = %6.2f\n", 22, sched540c[22]);
  for (j = 23; j <= 35; j++)
  {
    sched540[j] = ca_PrelimFedReturn.sched1[j];
    sched540[36] = sched540[36] + sched540[j];
    if (sched540[j] != 0.0)
      fprintf(outfile, " SchedCA540_%d = %6.2f\n", j, sched540[j]);

    sched540b[36] = sched540b[36] + sched540b[j];
    if (sched540b[j] != 0.0)
      fprintf(outfile, " SchedCA540_%db = %6.2f\n", j, sched540b[j]);

    sched540c[36] = sched540c[36] + sched540c[j];
    if (sched540c[j] != 0.0)
      fprintf(outfile, " SchedCA540_%dc = %6.2f\n", j, sched540c[j]);

    if (j == 31)
    {
      if (ca_PrelimFedReturn.AlimRecipSSN[0] != '\0')
        fprintf(outfile, " AlimRecipSSN: %s\n", ca_PrelimFedReturn.AlimRecipSSN);

      if (ca_PrelimFedReturn.AlimRecipName[0] != '\0')
        fprintf(outfile, " AlimRecipName: %s\n", ca_PrelimFedReturn.AlimRecipName);

    }

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
  for (j = 1; j <= 37; j++)
    if (sched540b[j] != 0.0)
    fprintf(outfile, " SchedCA540_%db = %6.2f\n", j, sched540b[j]);


  GetLine("CA540_P2_1", &ca_sched540part2[1]);
  ca_sched540part2[2] = ca_PrelimFedReturn.fedline[7];
  ca_sched540part2[3] = 0.075 * ca_sched540part2[2];
  ca_sched540part2[4] = NotLessThanZero(ca_sched540part2[1] - ca_sched540part2[3]);
  ca_sched540part2_5a = ca_PrelimFedReturn.schedA5a;
  ca_sched540part2_5b = ca_PrelimFedReturn.schedA5b;
  ca_sched540part2_5c = ca_PrelimFedReturn.schedA5c;
  ca_sched540part2_5d = (ca_sched540part2_5a + ca_sched540part2_5b) + ca_sched540part2_5c;
  if (ca_status != 3)
    ca_sched540part2[5] = smallerof(ca_sched540part2_5d, 10000.0);
  else
    ca_sched540part2[5] = smallerof(ca_sched540part2_5d, 5000.0);

  GetLine("CA540_P2_Sub_5a", &ca_sched540part2_sub[5]);
  ca_sched540part2_add[5] = ca_sched540part2_5d - ca_sched540part2[5];
  ca_sched540part2[6] = ca_PrelimFedReturn.schedA[6];
  GetLine("CA540_P2_Sub_6", &ca_sched540part2_sub[6]);
  ca_sched540part2[7] = ca_sched540part2[5] + ca_sched540part2[6];
  ca_sched540part2_sub[7] = ca_sched540part2_sub[5] + ca_sched540part2_sub[6];
  ca_sched540part2_add[7] = ca_sched540part2_add[5];
  ca_sched540part2_8a = ca_PrelimFedReturn.schedA8a;
  GetLine("CA540_P2_Add_8a", &ca_sched540part2_add8a);
  ca_sched540part2_8b = ca_PrelimFedReturn.schedA8b;
  GetLine("CA540_P2_Add_8b", &ca_sched540part2_add8b);
  ca_sched540part2_8c = ca_PrelimFedReturn.schedA8c;
  GetLine("CA540_P2_Add_8c", &ca_sched540part2_add8c);
  ca_sched540part2[8] = (ca_sched540part2_8a + ca_sched540part2_8b) + ca_sched540part2_8c;
  ca_sched540part2_add[8] = (ca_sched540part2_add8a + ca_sched540part2_add8b) + ca_sched540part2_add8c;
  ca_sched540part2[9] = ca_PrelimFedReturn.schedA[9];
  GetLine("CA540_P2_Sub_9", &ca_sched540part2_sub[9]);
  GetLine("CA540_P2_Add_9", &ca_sched540part2_add[9]);
  ca_sched540part2[10] = ca_sched540part2[8] + ca_sched540part2[9];
  ca_sched540part2_sub[10] = ca_sched540part2_sub[9];
  ca_sched540part2_add[10] = ca_sched540part2_add[8] + ca_sched540part2_add[9];
  ca_sched540part2[11] = ca_PrelimFedReturn.schedA[11];
  GetLine("CA540_P2_Sub_11", &ca_sched540part2_sub[11]);
  GetLine("CA540_P2_Add_11", &ca_sched540part2_add[11]);
  ca_sched540part2[12] = ca_PrelimFedReturn.schedA[12];
  GetLine("CA540_P2_Sub_12", &ca_sched540part2_sub[12]);
  GetLine("CA540_P2_Add_12", &ca_sched540part2_add[12]);
  ca_sched540part2[13] = ca_PrelimFedReturn.schedA[13];
  GetLine("CA540_P2_Sub_13", &ca_sched540part2_sub[13]);
  GetLine("CA540_P2_Add_13", &ca_sched540part2_add[13]);
  ca_sched540part2[14] = (ca_sched540part2[11] + ca_sched540part2[12]) + ca_sched540part2[13];
  ca_sched540part2_sub[14] = (ca_sched540part2_sub[11] + ca_sched540part2_sub[12]) + ca_sched540part2_sub[13];
  ca_sched540part2_add[14] = (ca_sched540part2_add[11] + ca_sched540part2_add[12]) + ca_sched540part2_add[13];
  ca_sched540part2[15] = ca_PrelimFedReturn.schedA[15];
  GetLine("CA540_P2_Sub_15", &ca_sched540part2_sub[15]);
  GetLine("CA540_P2_Add_15", &ca_sched540part2_add[15]);
  ca_sched540part2[16] = ca_PrelimFedReturn.schedA[13];
  GetLine("CA540_P2_Sub_16", &ca_sched540part2_sub[16]);
  GetLine("CA540_P2_Add_16", &ca_sched540part2_add[16]);
  ca_sched540part2[17] = ((((ca_sched540part2[4] + ca_sched540part2[7]) + ca_sched540part2[10]) + ca_sched540part2[14]) + ca_sched540part2[15]) + ca_sched540part2[16];
  ca_sched540part2_sub[17] = ((((ca_sched540part2_sub[4] + ca_sched540part2_sub[7]) + ca_sched540part2_sub[10]) + ca_sched540part2_sub[14]) + ca_sched540part2_sub[15]) + ca_sched540part2_sub[16];
  ca_sched540part2_add[17] = ((((ca_sched540part2_add[4] + ca_sched540part2_add[7]) + ca_sched540part2_add[10]) + ca_sched540part2_add[14]) + ca_sched540part2_add[15]) + ca_sched540part2_add[16];
  ca_sched540part2[18] = (ca_sched540part2[17] - ca_sched540part2_sub[17]) + ca_sched540part2_add[17];
  GetLine("CA540_P2_19", &ca_sched540part2[19]);
  GetLine("CA540_P2_20", &ca_sched540part2[20]);
  GetLine("CA540_P2_21", &ca_sched540part2[21]);
  ca_sched540part2[22] = (ca_sched540part2[19] + ca_sched540part2[20]) + ca_sched540part2[21];
  ca_sched540part2[23] = ca_PrelimFedReturn.fedline[7];
  ca_sched540part2[24] = NotLessThanZero(0.02 * ca_sched540part2[23]);
  ca_sched540part2[25] = NotLessThanZero(ca_sched540part2[22] - ca_sched540part2[24]);
  ca_sched540part2[26] = ca_sched540part2[18] + ca_sched540part2[25];
  ca_sched540part2[28] = ca_sched540part2[26] + ca_sched540part2[27];
  switch (ca_status)
  {
    case 1:

    case 3:
      threshA = 194504.0;
      std_ded = 4401.0;
      break;

    case 2:

    case 5:
      threshA = 389013.0;
      std_ded = 8802.0;
      break;

    case 4:
      threshA = 291760.0;
      std_ded = 8802.0;
      break;

  }

  if (L[13] > threshA)
  {
    double ws[40];
    for (j = 1; j <= 10; j++)
      ws[j] = 0.0;

    ws[1] = ca_sched540part2[28];
    ws[2] = ((ca_PrelimFedReturn.schedA[4] + ca_PrelimFedReturn.schedA[9]) + ca_PrelimFedReturn.schedA[15]) + ca_PrelimFedReturn.schedA[16];
    ws[3] = ws[1] - ws[2];
    if (ws[3] == 0.0)
      ca_sched540part2[29] = ws[1];
    else
    {
      ws[4] = 0.8 * ws[3];
      ws[5] = L[13];
      ws[6] = threshA;
      ws[7] = ws[5] - ws[6];
      if (ws[7] == 0.0)
        ca_sched540part2[29] = ws[1];
      else
      {
        ws[8] = 0.06 * ws[7];
        ws[9] = smallerof(ws[4], ws[8]);
        ws[10] = ws[1] - ws[9];
        ca_sched540part2[29] = ws[10];
      }

    }

    for (j = 1; j <= 10; j++)
      if (ws[j] != 0.0)
      fprintf(outfile, "  ItemizedDedWS%d = %6.2f\n", j, ws[j]);


  }
  else
  {
    ca_sched540part2[29] = ca_sched540part2[28];
  }

  ca_sched540part2[30] = largerof(ca_sched540part2[29], std_ded);
  ca_display_part2column(1, 0);
  ca_display_part2column(2, 0);
  ca_display_part2column(3, 0);
  ca_display_part2column(4, 'a');
  if (ca_sched540part2_5a != 0.0)
    fprintf(outfile, " SchedCA540_Part2_5aa = %6.2f\n", ca_sched540part2_5a);

  if (ca_sched540part2_sub[5] != 0.0)
    fprintf(outfile, " SchedCA540_Part2_5ab = %6.2f\n", ca_sched540part2_sub[5]);

  if (ca_sched540part2_5b != 0.0)
    fprintf(outfile, " SchedCA540_Part2_5ba = %6.2f\n", ca_sched540part2_5b);

  if (ca_sched540part2_5c != 0.0)
    fprintf(outfile, " SchedCA540_Part2_5ca = %6.2f\n", ca_sched540part2_5c);

  if (ca_sched540part2_5d != 0.0)
    fprintf(outfile, " SchedCA540_Part2_5da = %6.2f\n", ca_sched540part2_5d);

  if (ca_sched540part2[5] != 0.0)
    fprintf(outfile, " SchedCA540_Part2_5ea = %6.2f\n", ca_sched540part2[5]);

  if (ca_sched540part2_sub[5] != 0.0)
    fprintf(outfile, " SchedCA540_Part2_5eb = %6.2f\n", ca_sched540part2_sub[5]);

  if (ca_sched540part2_add[5] != 0.0)
    fprintf(outfile, " SchedCA540_Part2_5ec = %6.2f\n", ca_sched540part2_add[5]);

  ca_display_part2(6);
  ca_display_part2(7);
  if (ca_sched540part2_8a != 0.0)
    fprintf(outfile, " SchedCA540_Part2_8aa = %6.2f\n", ca_sched540part2_8a);

  if (ca_sched540part2_add8a != 0.0)
    fprintf(outfile, " SchedCA540_Part2_8ac = %6.2f\n", ca_sched540part2_add8a);

  if (ca_sched540part2_8b != 0.0)
    fprintf(outfile, " SchedCA540_Part2_8ba = %6.2f\n", ca_sched540part2_8b);

  if (ca_sched540part2_add8b != 0.0)
    fprintf(outfile, " SchedCA540_Part2_8bc = %6.2f\n", ca_sched540part2_add8b);

  if (ca_sched540part2_8c != 0.0)
    fprintf(outfile, " SchedCA540_Part2_8ca = %6.2f\n", ca_sched540part2_8c);

  if (ca_sched540part2_add8c != 0.0)
    fprintf(outfile, " SchedCA540_Part2_8cc = %6.2f\n", ca_sched540part2_add8c);

  if (ca_sched540part2[8] != 0.0)
    fprintf(outfile, " SchedCA540_Part2_8ea = %6.2f\n", ca_sched540part2[8]);

  if (ca_sched540part2_add[8] != 0.0)
    fprintf(outfile, " SchedCA540_Part2_8ec = %6.2f\n", ca_sched540part2_add[8]);

  for (j = 9; j <= 17; j++)
    ca_display_part2(j);

  ca_display_part2column(18, 0);
  L[18] = ca_sched540part2[30];
  for (j = 19; j <= 30; j++)
    ca_display_part2column(j, 0);

  L[14] = sched540b[37];
  showline(14);
  L[15] = L[13] - L[14];
  if (L[15] < 0.0)
    fprintf(outfile, "L15 = (%f6.2)\n", -L[15]);
  else
    showline(15);

  L[16] = sched540c[37];
  showline(16);
  L[17] = L[15] + L[16];
  showline(17);
  switch (ca_status)
  {
    case 1:

    case 4:
      if (iline9 == 0)
      switch (iline10)
    {
      case 0:
        min2file = 14154.0;
        break;

      case 1:
        min2file = 26387.0;
        break;

      default:
        min2file = 35562.0;
        break;

    }

    else
      switch (iline10)
    {
      case 0:
        min2file = 20054.0;
        break;

      case 1:
        min2file = 29229.0;
        break;

      default:
        min2file = 36569.0;
        break;

    }


      break;

    case 2:
      if (iline9 == 0)
      switch (iline10)
    {
      case 0:
        min2file = 28312.0;
        break;

      case 1:
        min2file = 40545.0;
        break;

      default:
        min2file = 49720.0;
        break;

    }

    else
      if (iline9 == 1)
      switch (iline10)
    {
      case 0:
        min2file = 34212.0;
        break;

      case 1:
        min2file = 43387.0;
        break;

      default:
        min2file = 50727.0;
        break;

    }

    else
      switch (iline10)
    {
      case 0:
        min2file = 40112.0;
        break;

      case 1:
        min2file = 49287.0;
        break;

      default:
        min2file = 56627.0;
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
        min2file = 26387.0;
        break;

      default:
        min2file = 35562.0;
        break;

    }

    else
      switch (iline10)
    {
      case 0:
        min2file = 0.0;
        break;

      case 1:
        min2file = 29229.0;
        break;

      default:
        min2file = 36569.0;
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

  L[31] = ca_TaxRateFunction(L[19], ca_status);
  showline(31);
  ca_Report_bracket_info(L[19], ca_status);
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
    printf(" Doing AGI Limitations worksheet.\n");
    ws_a = L[13];
    ws_b = threshA;
    ws_c = ws_a - ws_b;
    if (ca_status != 3)
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
  GetLineF("L43", &L[43]);
  GetLineF("L44", &L[44]);
  GetLineF("L45", &L[45]);
  GetLineF("L46", &L[46]);
  L[47] = (((L[40] + L[43]) + L[44]) + L[45]) + L[46];
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


  GetLine("L112", &L[112]);
  GetLine("L113", &L[113]);
  if (L[92] > L[64])
  {
    L[94] = L[92] - L[64];
    fprintf(outfile, "L94 = %6.2f  REBATE!!!\n", L[94]);
    showline(95);
    L[96] = L[94] - L[95];
    showline(96);
    showline(112);
    showline(113);
    L[115] = L[96] - ((L[110] + L[112]) + L[113]);
    showline(115);
  }
  else
  {
    L[97] = L[64] - L[92];
    fprintf(outfile, "L97 = %6.2f  DUE !!!\n", L[97]);
    fprintf(outfile, "         (Which is %2.1f%% of your total tax.)\n", (100.0 * L[97]) / (L[64] + 1e-9));
    L[111] = (L[93] + L[97]) + L[110];
    showline(111);
    showline(112);
    showline(113);
    L[114] = (L[111] + L[112]) + L[113];
    showline(114);
  }

  fprintf(outfile, "\nSelect any charity contributions and complete\n form accordingly.\n");
  fprintf(outfile, "\n{ --------- }\n");
  writeout_line = 0;
  Your1stName = GetTextLineF("Your1stName:");
  YourMidInitial = ca_pull_initial(Your1stName);
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
  SpouseMidInitial = ca_pull_initial(Spouse1stName);
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
  grab_any_pdf_markups(infname, outfile);
  fclose(outfile);
  Display_File(outfname);
  return 0;
}


/* END of taxsolve_CA_540_2018.c */
/* START of taxsolve_MA_1_2018.c */
float ma_thisversion = 16.01;
double ma_Sum(double *v, int start_slot, int end_slot)
{
  int j;
  double result = 0.0;
  for (j = start_slot; j <= end_slot; j++)
    result += v[j];

  return result;
}

double ma_ComputeTax(double taxableIncome)
{
  double taxrate = 0.051;
  if (taxableIncome < 24000.0)
    return (int) ((taxrate * (taxableIncome + 25.0)) + 0.5);
  else
    return taxableIncome * taxrate;

}

int ma_main(int argc, char *argv[])
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
  char word[4000];
  char *infname = 0;
  char outfname[4000];
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
  double L43a = 0.0;
  printf("Massachusetts Form-1 2018 - v%3.2f\n", ma_thisversion);
  i = 1;
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
      infname = strdup(argv[i]);
      infile = fopen(argv[i], "r");
      if (infile == 0)
      {
        printf("ERROR: Parameter file '%s' could not be opened.\n", argv[i]);
        exit(1);
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
        printf("ERROR: Output file '%s' could not be opened.\n", outfname);
        exit(1);
      }

      printf("Writing results to file:  %s\n", outfname);
    }
    else
    {
      printf("Unknown command-line parameter '%s'\n", argv[i]);
      exit(1);
    }


    i = i + 1;
  }

  if (infile == 0)
  {
    printf("Error: No input file on command line.\n");
    exit(1);
  }

  for (i = 0; i < 1000; i++)
  {
    L[i] = 0.0;
  }

  read_line(infile, word);
  now = time(0);
  fprintf(outfile, "\n%s,	 v%2.2f, %s\n", word, ma_thisversion, ctime(&now));
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
    printf("Error: unrecognized status '%s'. Exiting.\n", word);
    fprintf(outfile, "Error: unrecognized status '%s'. Exiting.\n", word);
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
  get_param_single_line(infile, 'b', &flag, "Spouse age over 65?");
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
  get_param_single_line(infile, 'b', &flag, "Spouse Blindness?");
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
  L[2] = ma_Sum(Exemptions, 0, 5);
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
  L[10] = ma_Sum(L, 3, 9);
  showline_wmsg(10, "TOTAL 5.1% INCOME");
  GetLine("L11a", &MassRetirement[0]);
  if (MassRetirement[0] > 2000)
    MassRetirement[0] = 2000;

  showline_wlabel("L11a", MassRetirement[0]);
  GetLine("L11b", &MassRetirement[1]);
  if (MassRetirement[1] > 2000)
    MassRetirement[1] = 2000;

  showline_wlabel("L11b", MassRetirement[1]);
  L[11] = ma_Sum(MassRetirement, 0, 1);
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
  L[16] = ma_Sum(L, 11, 15);
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
  L[22] = ma_ComputeTax(L[21]);
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
  L[28] = ma_Sum(L, 22, 26);
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
          printf("Bad filing status.\n");
          exit(1);
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

  L[33] = ma_Sum(L33, 0, 5);
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

  GetLine1("L36", &L[36]);
  L[36] = NotLessThanZero(L[36]);
  ShowLineNonZero(36);
  L[37] = ma_Sum(L, 32, 36);
  showline_wmsg(37, "Income Tax After Credits Contributions, Use Tax + HC Penalty");
  GetLine("L38", &L[38]);
  ShowLineNonZero(38);
  GetLine("L39", &L[39]);
  ShowLineNonZero(39);
  GetLine("L40", &L[40]);
  ShowLineNonZero(40);
  GetLine("L41", &L[41]);
  ShowLineNonZero(41);
  GetLine("L42", &L[42]);
  ShowLineNonZero(42);
  GetLine("L43a", &L43a);
  if (L43a != 0.0)
    fprintf(outfile, " L43a = %6.2f  x 0.23 = .....  ", L43a);

  L[43] = L43a * 0.23;
  ShowLineNonZero(43);
  GetLine("L44", &L[44]);
  ShowLineNonZero(44);
  GetLine("L45", &L[45]);
  ShowLineNonZero(45);
  L[46] = ma_Sum(L, 38, 45);
  showline_wmsg(45, "total payments");
  GetLine("L48", &L[48]);
  if (L[37] < L[46])
  {
    L[47] = L[46] - L[37];
    fprintf(outfile, "L47 = %6.2f  Overpayment!\n", L[47]);
    if (L[48] > L[47])
      L[48] = L[47];

    showline_wmsg(48, "Overpayment to be applied to next year's estimated tax");
    L[49] = L[47] - L[48];
    fprintf(outfile, "L49 = %6.2f  THIS IS YOUR REFUND\n", L[49]);
  }
  else
  {
    L[50] = L[37] - L[46];
    fprintf(outfile, "L50 = %6.2f  TAX DUE !!!\n", L[50]);
    fprintf(outfile, "         (Which is %2.1f%% of your total tax.)\n", (100.0 * L[50]) / (L[37] + 1e-9));
    if ((L[50] > 400.0) && (L[46] < (0.80 * L[37])))
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
  grab_any_pdf_markups(infname, outfile);
  fclose(outfile);
  printf("\nListing results from file: %s\n\n", outfname);
  Display_File(outfname);
  printf("\nResults writen to file: %s\n", outfname);
  return 0;
}


/* END of taxsolve_MA_1_2018.c */
/* START of taxsolve_NC_D400_2018.c */
float nc_thisversion = 16.01;
double nc_flat_tax_rate = 0.05499;
struct nc_FedReturnData
{
  double fedline[1000];
  int Itemized;
  int Limited;
  int Limited_L6;
  double Sched_A[1000];
};
void nc_convert_slashes(char *fname)
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

void nc_ImportFederalReturnData(char *fedlogfile, struct nc_FedReturnData *fed_data)
{
  FILE *infile;
  char fline[1000];
  char word[1000];
  int linenum;
  nc_convert_slashes(fedlogfile);
  infile = fopen(fedlogfile, "r");
  if (infile == 0)
  {
    printf("Error: Could not open federal return '%s'\n", fedlogfile);
    fprintf(outfile, "\nError: Could not open federal return '%s'\n", fedlogfile);
    system("pwd");
    system("ls -l");
    exit(1);
  }

  printf(" Reading file: %s\n", fedlogfile);
  fed_data->Itemized = 1;
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
      printf("Read Line: %s", fline);

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
        printf("Error: Reading fed sched-A line number '%s%s'\n", word, fline);

      next_word(fline, word, " 	=");
      if (sscanf(word, "%lf", &fed_data->Sched_A[linenum]) != 1)
        printf("Error: Reading fed sched-A line %d '%s%s'\n", linenum, word, fline);

      if (verbose)
        printf("Sched_A[%d] = %2.2f\n", linenum, fed_data->Sched_A[linenum]);

    }

    if ((strstr(word, "L") == word) && (strstr(fline, " = ") != 0))
    {
      if (sscanf(&word[1], "%d", &linenum) != 1)
        printf("Error: Reading fed line number '%s%s'\n", word, fline);

      next_word(fline, word, " 	=");
      if (sscanf(word, "%lf", &fed_data->fedline[linenum]) != 1)
        printf("Error: Reading fed line %d '%s%s'\n", linenum, word, fline);

      if (verbose)
        printf("FedLin[%d] = %2.2f\n", linenum, fed_data->fedline[linenum]);

    }

    read_line(infile, fline);
  }

  fclose(infile);
}

int nc_main(int argc, char *argv[])
{
  int j;
  int jj;
  int k;
  int status;
  char word[1000];
  char *infname = 0;
  char outfname[1000];
  char *socsec;
  char socsectmp[100];
  time_t now;
  struct nc_FedReturnData fed_data;
  double stdded;
  double min_payment = 0.0;
  double min2file;
  double L20a = 0.0;
  double L20b = 0.0;
  double L21a = 0.0;
  double L21b = 0.0;
  double L21c = 0.0;
  double L21d = 0.0;
  printf("NC D400 2018 - v%3.2f\n", nc_thisversion);
  jj = 1;
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
      infname = strdup(argv[jj]);
      infile = fopen(argv[jj], "r");
      if (infile == 0)
      {
        printf("ERROR: Parameter file '%s' could not be opened.\n", argv[jj]);
        fprintf(outfile, "ERROR: Parameter file '%s' could not be opened.\n", argv[jj]);
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
        printf("ERROR: Output file '%s' could not be opened.\n", outfname);
        fprintf(outfile, "ERROR: Output file '%s' could not be opened.\n", outfname);
        exit(1);
      }

      printf("Writing results to file:  %s\n", outfname);
    }
    else
    {
      printf("Unknown command-line parameter '%s'\n", argv[jj]);
      fprintf(outfile, "Unknown command-line parameter '%s'\n", argv[jj]);
      exit(1);
    }


    jj++;
  }

  if (infile == 0)
  {
    printf("Error: No input file on command line.\n");
    fprintf(outfile, "Error: No input file on command line.\n");
    exit(1);
  }

  for (j = 0; j < 1000; j++)
  {
    L[j] = 0.0;
  }

  read_line(infile, word);
  now = time(0);
  fprintf(outfile, "\n%s,	 v%2.2f, %s\n", word, nc_thisversion, ctime(&now));
  get_parameter(infile, 's', word, "FedReturn");
  if (verbose)
    printf("word: %s\n", word);

  get_word(infile, word);
  nc_ImportFederalReturnData(word, &fed_data);
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
    printf("Error: unrecognized status '%s'. Must be: Single, Married/joint, Married/sep, Head_of_house, Widow(er)\nExiting.\n", word);
    fprintf(outfile, "Error: unrecognized status '%s'. Must be: Single, Married/joint, Married/sep, Head_of_house, Widow(er)\nExiting.\n", word);
    exit(1);
  }






  fprintf(outfile, "Status = %s (%d)\n", word, status);
  GetLine("L7", &L[7]);
  GetLine("L9", &L[9]);
  GetLine("L11", &L[11]);
  GetLine("L13", &L[13]);
  GetLine("L16", &L[16]);
  GetLine("L18", &L[18]);
  GetLine("L20a", &L20a);
  GetLine("L20b", &L20b);
  L[20] = L20a + L20b;
  GetLine("L21a", &L21a);
  GetLine("L21b", &L21b);
  GetLine("L21c", &L21c);
  GetLine("L21d", &L21d);
  L[6] = fed_data.fedline[7];
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
      printf("Unknown status\n");
      fprintf(outfile, "Unknown status\n");
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
  L[15] = nc_flat_tax_rate * L[14];
  L[17] = L[15] - L[16];
  printf("Assuming you have calculated your USE tax (%2.2f) according to instructions pg 9\n", L[18]);
  L[19] = L[17] + L[18];
  L[21] = ((L21a + L21b) + L21c) + L21d;
  L[23] = (L[20] + L[21]) + L[22];
  L[25] = L[23] - L[24];
  if (L[19] > L[25])
  {
    L[26] = L[19] - L[25];
    printf("         (Which is %2.1f%% of the total amount owed.)\n", (100.0 * L[26]) / (L[19] + 1e-9));
    min_payment = 0.9 * L[19];
    if ((L[23] < min_payment) && (L[19] > 1000.00))
    {
      printf("WARNING: Possible underpayment of est. taxes penalty. Calculation not performed.\n");
    }

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
  showline_wlabel("L20a", L20a);
  showline_wlabel("L20b", L20b);
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
  grab_any_pdf_markups(infname, outfile);
  fclose(outfile);
  Display_File(outfname);
  printf("\nResults written to file '%s'\n", outfname);
  return 0;
}


/* END of taxsolve_NC_D400_2018.c */
/* START of taxsolve_NJ_1040_2018.c */
float nj_thisversion = 16.03;
double nj_A[1000];
double nj_S[1000];
double nj_E[1000];
double nj_TaxRateFormula(double x, int status)
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
      if (x < 5000000.0)
      return (x * 0.0897) - 15126.25;
    else
      return (x * 0.1075) - 104126.25;






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
      if (x < 5000000.0)
      return (x * 0.0897) - 17042.5;
    else
      return (x * 0.1075) - 106042.50;







  }
  else
  {
    printf("Status not covered.\n");
    exit(1);
  }


}

void nj_Report_bracket_info(double x, int status)
{
  double tx;
  double rate;
  tx = nj_TaxRateFormula(x, status);
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
      if (x < 5000000.0)
      rate = 0.0897;
    else
      rate = 0.1075;






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
      if (x < 500000.0)
      rate = 0.0897;
    else
      rate = 0.1075;







  }

  printf(" You are in the %2.1f%% marginal tax bracket,\n and you are paying an effective %2.1f%% tax on your total income.\n", 100.0 * rate, (100.0 * tx) / x);
  fprintf(outfile, " You are in the %2.1f%% marginal tax bracket,\n and you are paying an effective %2.1f%% tax on your total income.\n", 100.0 * rate, (100.0 * tx) / x);
}

double nj_TaxRateFunction(double income, int status)
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
    tx = (int) (nj_TaxRateFormula(x, status) + 0.5);
  }
  else
    tx = nj_TaxRateFormula(income, status);

  return tx;
}

int nj_main(int argc, char *argv[])
{
  int i;
  int j;
  int k;
  char word[1000];
  char *infname = 0;
  char outfname[4000];
  int status = 0;
  time_t now;
  double L16b = 0.0;
  double L20b = 0.0;
  double L28a = 0.0;
  double L28b = 0.0;
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
  printf("NJ 1040 2018 - v%3.1f\n", nj_thisversion);
  i = 1;
  k = 1;
  while (i < argc)
  {
    if (strcmp(argv[i], "-verbose") == 0)
      verbose = 1;
    else
      if (k == 1)
    {
      infname = strdup(argv[i]);
      infile = fopen(argv[i], "r");
      if (infile == 0)
      {
        printf("ERROR: Parameter file '%s' could not be opened.\n", argv[i]);
        exit(1);
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
        printf("ERROR: Output file '%s' could not be opened.\n", outfname);
        exit(1);
      }

      printf("Writing results to file:  %s\n", outfname);
    }
    else
    {
      printf("Unknown command-line parameter '%s'\n", argv[i]);
      exit(1);
    }


    i = i + 1;
  }

  if (infile == 0)
  {
    printf("Error: No input file on command line.\n");
    exit(1);
  }

  for (i = 0; i < 1000; i++)
  {
    L[i] = 0.0;
    nj_A[i] = 0.0;
    nj_S[i] = 0.0;
    nj_E[i] = 0.0;
  }

  read_line(infile, word);
  now = time(0);
  fprintf(outfile, "\n%s,	 v%2.2f, %s\n", word, nj_thisversion, ctime(&now));
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
    printf("Error: unrecognized status '%s'. Must be: Single, Married/joint, Married/sep, Head_of_house, Widow(er)\nExiting.\n", word);
    fprintf(outfile, "Error: unrecognized status '%s'. Must be: Single, Married/joint, Married/sep, Head_of_house, Widow(er)\nExiting.\n", word);
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

  fprintf(outfile, "L6a = %d\n", (int) L[6]);
  L[6] = 1000.0 * L[6];
  shownum(6);
  get_parameter(infile, 's', word, "YouOver65");
  get_parameter(infile, 'b', &j, "YouOver65");
  L[7] = j;
  if (j)
    fprintf(outfile, " Check_Over65 = X\n");

  get_parameter(infile, 's', word, "SpouseOver65");
  get_param_single_line(infile, 'b', &j, "SpouseOver65");
  if (status == 2)
  {
    L[7] = L[7] + j;
    if (j)
      fprintf(outfile, " Check_SpOver65 = X\n");

  }

  fprintf(outfile, "L7a = %d\n", (int) L[7]);
  L[7] = 1000.0 * L[7];
  shownum(7);
  get_parameter(infile, 's', word, "YouBlindDisa");
  get_parameter(infile, 'b', &j, "YouBlindDisa");
  L[8] = j;
  if (j)
    fprintf(outfile, " Check_Blind = X\n");

  get_parameter(infile, 's', word, "SpouseBlindDisa");
  get_param_single_line(infile, 'b', &j, "SpouseBlindDisa");
  if (status == 2)
  {
    L[8] = L[8] + j;
    if (j)
      fprintf(outfile, " Check_SpBlind = X\n");

  }

  fprintf(outfile, "L8a = %d\n", (int) L[8]);
  L[8] = 1000.0 * L[8];
  shownum(8);
  get_parameter(infile, 's', word, "YouVeteran");
  get_parameter(infile, 'b', &j, "YouVeteran");
  L[9] = j;
  if (j)
    fprintf(outfile, " Check_Vet = X\n");

  get_parameter(infile, 's', word, "SpouseVeteran");
  get_param_single_line(infile, 'b', &j, "SpouseVeteran");
  if (status == 2)
  {
    L[8] = L[8] + j;
    if (j)
      fprintf(outfile, " Check_SpVet = X\n");

  }

  fprintf(outfile, "L9a = %d\n", (int) L[9]);
  L[9] = 3000.0 * L[9];
  shownum(9);
  get_parameter(infile, 's', word, "L10");
  get_parameter(infile, 'i', &j, "L10");
  fprintf(outfile, "L10a = %d\n", j);
  L[10] = 1500.0 * j;
  shownum(10);
  get_parameter(infile, 's', word, "L11");
  get_parameter(infile, 'i', &j, "L11");
  fprintf(outfile, "L11a = %d\n", j);
  L[11] = 1500.0 * j;
  shownum(11);
  get_parameter(infile, 's', word, "L12");
  get_parameter(infile, 'i', &j, "L12");
  fprintf(outfile, "L11a = %d\n", j);
  L[12] = 1000.0 * j;
  shownum(12);
  fprintf(outfile, " FillOutForm_wRoundedNumbers_wZerosAfterDecPt\n");
  L[13] = (((((L[6] + L[7]) + L[8]) + L[9]) + L[10]) + L[11]) + L[12];
  showline(13);
  GetLineF("L15", &L[15]);
  GetLineF("L16a", &L[16]);
  GetLineF("L16b", &L16b);
  GetLineF("L17", &L[17]);
  GetLine("L18", &L[18]);
  if (L[18] < 0.0)
    L[18] = 0.0;

  showline(18);
  GetLine("L19", &L[19]);
  if (L[19] < 0.0)
    L[19] = 0.0;

  showline(19);
  GetLineF("L20a", &L[20]);
  GetLineF("L20b", &L20b);
  GetLineF("L21", &L[21]);
  GetLineF("L22", &L[22]);
  GetLineF("L23", &L[23]);
  GetLineF("L24", &L[24]);
  GetLineF("L25", &L[25]);
  GetLineF("L26", &L[26]);
  for (j = 15; j <= 26; j++)
    L[27] = L[27] + L[j];

  showline_wmsg(27, "Total Income");
  GetLineF("L28a", &L28a);
  GetLineF("L28b", &L28b);
  L[28] = L28a + L28b;
  showline(28);
  L[29] = L[27] - L[28];
  showline_wmsg(29, "NJ Gross Income");
  if ((status == 1) || (status == 3))
  {
    if (L[29] < 10000.0)
      fprintf(outfile, " --- You do not need to file, (except to get refund).  Income < $10,000. ---\n");

  }
  else
  {
    if (L[29] < 20000.0)
      fprintf(outfile, " --- You do not need to file, (except to get refund).  Income < $20,000. ---\n");

  }

  L[30] = L[13];
  showline(30);
  fprintf(outfile, "\n");
  GetLine("E1", &nj_E[1]);
  showline_wrksht('E', 1, nj_E);
  nj_E[2] = 0.02 * L[28];
  showline_wrksht('E', 2, nj_E);
  nj_E[3] = NotLessThanZero(nj_E[1] - nj_E[2]);
  showline_wrksht('E', 3, nj_E);
  GetLine("E4", &nj_E[4]);
  showline_wrksht('E', 4, nj_E);
  GetLine("E5", &nj_E[5]);
  showline_wrksht('E', 5, nj_E);
  nj_E[6] = NotLessThanZero((nj_E[3] + nj_E[4]) + nj_E[5]);
  showline_wrksht('E', 6, nj_E);
  fprintf(outfile, "\n");
  L[10] = nj_E[6];
  if (L[31] != 0.0)
    showline_wmsg(31, " Medical Expenses Worksheet E (See pg 27)");

  GetLineF("L32", &L[32]);
  GetLineF("L33", &L[33]);
  GetLineF("L34", &L[34]);
  GetLineF("L35", &L[35]);
  for (j = 30; j <= 35; j++)
    L[36] = L[36] + L[j];

  showline_wmsg(36, "Total Exemptions and Deductions");
  L[37] = L[29] - L[36];
  if (L[37] > 0.0)
    showline_wmsg(37, "(Taxable Income)");

  GetLineF("L38a", &L[38]);
  GetLine("A1", &nj_A[1]);
  GetLine("A9a", &A9a);
  fprintf(outfile, "\n");
  F[1] = L[38];
  showline_wrksht('H', 1, F);
  if (status != 3)
    F[2] = smallerof(F[1], 15000.0);
  else
    F[2] = smallerof(F[1], 7500.0);

  showline_wrksht('H', 2, F);
  if (status != 3)
    proptxcredit = 50.0;
  else
    proptxcredit = 25.0;

  if (A9a == 0.0)
  {
    F[3] = L[37];
    Fb[3] = L[37];
    fprintf(outfile, " H3a = %6.2f	H3b = %6.2f\n", F[3], Fb[3]);
    F[4] = F[2];
    Fb[4] = 0.0;
    fprintf(outfile, " H4a = %6.2f	H4b = %6.2f\n", F[4], Fb[4]);
    F[5] = F[3] - F[4];
    Fb[5] = Fb[3] - Fb[4];
    fprintf(outfile, " H5a = %6.2f	H5b = %6.2f\n", F[5], Fb[5]);
    F[6] = nj_TaxRateFunction(F[5], status);
    Fb[6] = nj_TaxRateFunction(Fb[5], status);
    fprintf(outfile, " H6a = %6.2f	H6b = %6.2f\n", F[6], Fb[6]);
    F[7] = Fb[6] - F[6];
    showline_wrksht('H', 7, F);
    if (F[7] >= proptxcredit)
    {
      fprintf(outfile, " H8. Yes. (Take Property Tax Deduction.)\n");
      L[39] = F[4];
      L[40] = F[5];
      L[41] = F[6];
      L[54] = 0.0;
    }
    else
    {
      fprintf(outfile, " H8. No. (Take Property Tax Credit.)\n");
      L[39] = 0.0;
      L[40] = Fb[5];
      L[41] = Fb[6];
      L[54] = proptxcredit;
    }

  }
  else
  {
    fprintf(outfile, "\nSchedule A:\n");
    showline_wrksht('A', 1, nj_A);
    nj_A[2] = L[29];
    showline_wrksht('A', 2, nj_A);
    nj_A[3] = smallerof(1.0, nj_A[1] / nj_A[2]);
    fprintf(outfile, " A3 = %6.2f %%\n", 100.0 * nj_A[3]);
    nj_A[4] = L[37];
    fprintf(outfile, " A4a = %6.2f	A4b = %6.2f\n", nj_A[4], nj_A[4]);
    fprintf(outfile, " (5a = %6.2f)\n", F[1]);
    nj_A[5] = F[2];
    fprintf(outfile, " A5a = %6.2f	A5b = %6.2f\n", nj_A[5], 0.0);
    nj_A[6] = nj_A[4] - nj_A[5];
    Ab[6] = nj_A[4] - 0.0;
    fprintf(outfile, " A6a = %6.2f	A6b = %6.2f\n", nj_A[6], Ab[6]);
    nj_A[7] = nj_TaxRateFunction(nj_A[6], status);
    Ab[7] = nj_TaxRateFunction(Ab[6], status);
    fprintf(outfile, " A7a = %6.2f	A7b = %6.2f\n", nj_A[7], Ab[7]);
    nj_A[8] = nj_A[3] * nj_A[7];
    Ab[8] = nj_A[3] * Ab[7];
    fprintf(outfile, " A8a = %6.2f	A8b = %6.2f\n", nj_A[8], Ab[8]);
    fprintf(outfile, "  (9a = %6.2f)\n", A9a);
    nj_A[9] = smallerof(smallerof(A9a, nj_A[8]), nj_A[7]);
    Ab[9] = smallerof(smallerof(A9a, Ab[8]), Ab[7]);
    fprintf(outfile, " A9a = %6.2f	A9b = %6.2f\n", nj_A[9], Ab[9]);
    fprintf(outfile, "\nWorksheet I:\n");
    I[1] = nj_A[7];
    Ib[1] = Ab[7];
    fprintf(outfile, " I1a = %6.2f	I1b = %6.2f\n", I[1], Ib[1]);
    I[2] = nj_A[9];
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
      L[39] = nj_A[5];
      L[40] = nj_A[6];
      L[41] = nj_A[7];
      L[42] = I[2];
      L[54] = 0.0;
    }
    else
    {
      fprintf(outfile, " Sched-I, No:  Take PropTax Credit\n\n");
      L[39] = 0.0;
      L[40] = Ab[6];
      L[41] = Ab[7];
      L[42] = Ib[2];
      L[54] = proptxcredit;
    }

  }

  if (L[38] == 0.0)
    L[54] = 0.0;

  if (L[38] > 0.0)
    fprintf(outfile, "L38a = %6.2f\n", L[38]);

  showline(39);
  fprintf(outfile, "\n");
  if (L[40] > 0.0)
    showline_wmsg(40, "NJ Taxable Income");

  showline_wmsg(41, "TAX");
  nj_Report_bracket_info(L[40], status);
  if (nj_A[1] > 0.0)
    showline_wmsg(42, "( Credit for Taxes paid to other jurisdictions. )\n");

  L[43] = L[41] - L[42];
  showline_wmsg(43, "( Balance of Tax )");
  GetLineF("L44", &L[44]);
  L[45] = L[43] - L[45];
  showline(45);
  GetLineF("L46", &L[46]);
  L[47] = L[45] - L[46];
  showline(47);
  GetLineF("L48", &L[48]);
  L[49] = NotLessThanZero(L[47] - L[48]);
  showline_wmsg(49, "Balance of Tax after Credits.");
  GetLineF("L50", &L[50]);
  GetLineF("L51", &L[51]);
  L[52] = (L[49] + L[50]) + L[51];
  showline_wmsg(52, "Total Tax Due");
  GetLine("L53", &L[53]);
  showline_wmsg(53, "Total NJ Income Tax Withheld");
  showline_wmsg(54, "Property tax Credit");
  GetLineF("L55", &L[55]);
  GetLineF("L56", &L[56]);
  GetLineF("L57", &L[57]);
  GetLineF("L58", &L[58]);
  GetLineF("L59", &L[59]);
  GetLineF("L60", &L[60]);
  for (j = 53; j <= 60; j++)
    L[61] = L[61] + L[j];

  showline_wmsg(61, "Total Withholding Payments & Credits");
  for (j = 64; j <= 72; j++)
    L[73] = L[73] + L[j];

  if (L[61] < L[52])
  {
    L[62] = L[52] - L[61];
    fprintf(outfile, "L62 = %6.2f	DUE !!!\n", L[62]);
    fprintf(outfile, "         (Which is %2.1f%% of your total tax.)\n", (100.0 * L[62]) / (L[43] + 1e-9));
    showline_wmsg(73, "( Total Adjustments to tax due )");
    L[74] = L[62] + L[73];
    showline_wmsg(74, "Balance Due");
  }
  else
  {
    L[63] = L[61] - L[52];
    fprintf(outfile, "L63 = %6.2f	Overpayment\n", L[63]);
    showline_wmsg(73, "( Total Adjustments to overpayment )");
    L[75] = L[63] - L[73];
    showline_wmsg(75, "Refund !!!");
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
  grab_any_pdf_markups(infname, outfile);
  fclose(outfile);
  Display_File(outfname);
  printf("\nResults written to file:  %s\n", outfname);
  return 0;
}


/* END of taxsolve_NJ_1040_2018.c */
/* START of taxsolve_NY_IT201_2018.c */
float ny_thisversion = 16.01;
double ny_A[10];
double ny_S[10];
int ny_status = 0;
char ny_statusnames[10][20] = {"0", "Single", "Married/Joint", "Married/Sep", "Head_of_House", "Widow"};
char *ny_Your1stName = "";
char *ny_YourLastName = "";
char *ny_YourInitial = "";
char *ny_Spouse1stName = "";
char *ny_SpouseLastName = "";
char *ny_SpouseInitial = "";
char *ny_YourSocSec = 0;
char *ny_SpouseSocSec = 0;
char *ny_MailAddress = 0;
char *ny_AptNumber = 0;
char ny_Town[2048] = "";
char ny_StateName[1024] = "";
char ny_Zipcode[1024] = "";
struct ny_FedReturnData
{
  double fedline[1000];
  double schedA[1000];
  double schedD[1000];
  double sched[8][1000];
  int Exception;
  int Itemized;
} ny_PrelimFedReturn;
void ny_convert_slashes(char *fname)
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

char *ny_pull_initial(char *name)
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

int ny_ImportFederalReturnData(char *fedlogfile, struct ny_FedReturnData *fed_data)
{
  FILE *infile;
  char fline[4000];
  char word[4000];
  char tword[2000];
  int linenum;
  int j;
  for (linenum = 0; linenum < 1000; linenum++)
  {
    fed_data->fedline[linenum] = 0.0;
    fed_data->schedA[linenum] = 0.0;
    fed_data->schedD[linenum] = 0.0;
    for (j = 0; j < 8; j++)
      fed_data->sched[j][linenum] = 0.0;

  }

  ny_convert_slashes(fedlogfile);
  infile = fopen(fedlogfile, "r");
  if (infile == 0)
  {
    printf("Error: Could not open Federal return '%s'\n", fedlogfile);
    fprintf(outfile, "Error: Could not open Federal return '%s'\n", fedlogfile);
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
          printf("Error: Reading Fed line number '%s%s'\n", word, fline);
          fprintf(outfile, "Error: Reading Fed line number '%s%s'\n", word, fline);
        }

        next_word(fline, word, " \t=");
        if (sscanf(word, "%lf", &fed_data->fedline[linenum]) != 1)
        {
          printf("Error: Reading Fed line %d '%s%s'\n", linenum, word, fline);
          fprintf(outfile, "Error: Reading Fed line %d '%s%s'\n", linenum, word, fline);
        }

        if (verbose)
          printf("FedLin[%d] = %2.2f\n", linenum, fed_data->fedline[linenum]);

      }

    }
    else
      if (((strstr(word, "A") == word) && (strstr(word, "AMT") != word)) && (strstr(fline, " = ") != 0))
    {
      if (sscanf(&word[1], "%d", &linenum) != 1)
      {
        printf("Error: Reading Fed line number '%s%s'\n", word, fline);
        fprintf(outfile, "Error: Reading Fed line number '%s%s'\n", word, fline);
      }

      next_word(fline, word, " \t=");
      if (sscanf(word, "%lf", &fed_data->schedA[linenum]) != 1)
      {
        printf("Error: Reading Fed schedA %d '%s%s'\n", linenum, word, fline);
        fprintf(outfile, "Error: Reading Fed schedA %d '%s%s'\n", linenum, word, fline);
      }

      if (verbose)
        printf("FedLin[%d] = %2.2f\n", linenum, fed_data->schedA[linenum]);

    }
    else
      if ((strstr(word, "D") == word) && (strstr(fline, " = ") != 0))
    {
      if (sscanf(&word[1], "%d", &linenum) != 1)
      {
        printf("Error: Reading Fed line number '%s%s'\n", word, fline);
        fprintf(outfile, "Error: Reading Fed line number '%s%s'\n", word, fline);
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
          printf("Error: Reading fed schedD %d '%s%s'\n", linenum, word, fline);
          fprintf(outfile, "Error: Reading Fed schedD %d '%s%s'\n", linenum, word, fline);
        }


      }


      if (verbose)
        printf("FedLin[%d] = %2.2f\n", linenum, fed_data->schedD[linenum]);

    }
    else
      if (strncmp(word, "S1_", 3) == 0)
    {
      next_word(&word[3], tword, " \t=:");
      if (sscanf(tword, "%d", &linenum) != 1)
        printf("Error: Reading Fed sched1 line-number '%s'\n", word);
      else
      {
        next_word(fline, word, " \t=:");
        if (sscanf(word, "%lf", &fed_data->sched[1][linenum]) != 1)
          printf("Error: Reading Fed sched1 line '%s'\n", word);

      }

    }
    else
      if (strncmp(word, "S2_", 3) == 0)
    {
      next_word(&word[3], tword, " \t=:");
      if (sscanf(tword, "%d", &linenum) != 1)
        printf("Error: Reading Fed sched2 line-number '%s'\n", word);
      else
      {
        next_word(fline, word, " \t=:");
        if (sscanf(word, "%lf", &fed_data->sched[2][linenum]) != 1)
          printf("Error: Reading Fed sched2 line '%s'\n", word);

      }

    }
    else
      if (strncmp(word, "S3_", 3) == 0)
    {
      next_word(&word[3], tword, " \t=:");
      if (sscanf(tword, "%d", &linenum) != 1)
        printf("Error: Reading Fed sched3 line-number '%s'\n", word);
      else
      {
        next_word(fline, word, " \t=:");
        if (sscanf(word, "%lf", &fed_data->sched[3][linenum]) != 1)
          printf("Error: Reading Fed sched3 line '%s'\n", word);

      }

    }
    else
      if (strncmp(word, "S4_", 3) == 0)
    {
      next_word(&word[3], tword, " \t=:");
      if (sscanf(tword, "%d", &linenum) != 1)
        printf("Error: Reading Fed sched4 line-number '%s'\n", word);
      else
      {
        next_word(fline, word, " \t=:");
        if (sscanf(word, "%lf", &fed_data->sched[4][linenum]) != 1)
          printf("Error: Reading Fed sched4 line '%s'\n", word);

      }

    }
    else
      if (strncmp(word, "S5_", 3) == 0)
    {
      next_word(&word[3], tword, " \t=:");
      if (sscanf(tword, "%d", &linenum) != 1)
        printf("Error: Reading Fed sched5 line-number '%s'\n", word);
      else
      {
        next_word(fline, word, " \t=:");
        if (sscanf(word, "%lf", &fed_data->sched[5][linenum]) != 1)
          printf("Error: Reading Fed sched5 line '%s'\n", word);

      }

    }
    else
      if (strcmp(word, "Status") == 0)
    {
      next_word(fline, word, " \t=");
      fprintf(outfile, " Status %s\n", word);
      if (strncasecmp(word, "Single", 4) == 0)
        ny_status = 1;
      else
        if (strncasecmp(word, "Married/Joint", 13) == 0)
        ny_status = 2;
      else
        if (strncasecmp(word, "Married/Sep", 11) == 0)
        ny_status = 3;
      else
        if (strncasecmp(word, "Head_of_House", 4) == 0)
        ny_status = 4;
      else
        if (strncasecmp(word, "Widow", 4) == 0)
        ny_status = 5;
      else
      {
        printf("Error: unrecognized status '%s'. Exiting.\n", word);
        fprintf(outfile, "Error: unrecognized status '%s'. Exiting.\n", word);
        return 0;
      }





    }
    else
      if (strcmp(word, "Your1stName:") == 0)
    {
      ny_Your1stName = strdup(fline);
      ny_YourInitial = ny_pull_initial(ny_Your1stName);
    }
    else
      if (strcmp(word, "YourLastName:") == 0)
    {
      ny_YourLastName = strdup(fline);
    }
    else
      if (strcmp(word, "YourSocSec#:") == 0)
    {
      ny_YourSocSec = strdup(fline);
    }
    else
      if (strcmp(word, "Spouse1stName:") == 0)
    {
      ny_Spouse1stName = strdup(fline);
      ny_SpouseInitial = ny_pull_initial(ny_Spouse1stName);
    }
    else
      if (strcmp(word, "SpouseLastName:") == 0)
    {
      ny_SpouseLastName = strdup(fline);
    }
    else
      if (strcmp(word, "SpouseSocSec#:") == 0)
    {
      ny_SpouseSocSec = strdup(fline);
    }
    else
      if (strcmp(word, "Number&Street:") == 0)
    {
      ny_MailAddress = strdup(fline);
    }
    else
      if (strcmp(word, "Apt#:") == 0)
    {
      ny_AptNumber = strdup(fline);
    }
    else
      if (strcmp(word, "TownStateZip:") == 0)
    {
      next_word(fline, ny_Town, ",");
      next_word(fline, ny_StateName, " \t,");
      next_word(fline, ny_Zipcode, " \t,");
    }


















    read_line(infile, fline);
  }

  fclose(infile);
  return 1;
}

double ny_TaxRateFunction(double income, int ny_status)
{
  double tax;
  switch (ny_status)
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
      tax = 2093.0 + (0.0633 * (income - 43000.0));
    else
      if (income <= 323200.0)
      tax = 9597.0 + (0.0657 * (income - 161550.0));
    else
      if (income <= 2155350.0)
      tax = 20218.0 + (0.0685 * (income - 323200.0));
    else
      tax = 145720.0 + (0.0882 * (income - 2155350.0));







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
      tax = 1042.0 + (0.0633 * (income - 21400.0));
    else
      if (income <= 215400.0)
      tax = 4793.0 + (0.0657 * (income - 80650.0));
    else
      if (income <= 1077550.0)
      tax = 13646.0 + (0.0685 * (income - 215400.0));
    else
      tax = 72703.0 + (0.0882 * (income - 1077550.0));







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
      tax = 1568.0 + (0.0633 * (income - 32200.0));
    else
      if (income <= 269300.0)
      tax = 6344.0 + (0.0657 * (income - 107650.0));
    else
      if (income <= 1616450.0)
      tax = 16964.0 + (0.0685 * (income - 269300.0));
    else
      tax = 109244.0 + (0.0882 * (income - 1616450.0));







      break;

    default:
      printf("Error: Unhandled status\n");
      exit(0);
      break;

  }

  return tax;
}

void ny_Report_bracket_info(double income, double tx, int ny_status)
{
  double rate;
  switch (ny_status)
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
      rate = 0.0633;
    else
      if (income <= 323200.0)
      rate = 0.0657;
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
      rate = 0.0633;
    else
      if (income <= 215400.0)
      rate = 0.0657;
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
      rate = 0.0633;
    else
      if (income <= 269300.0)
      rate = 0.0657;
    else
      if (income <= 1616450.0)
      rate = 0.0685;
    else
      rate = 0.0882;







      break;

    default:
      printf("Error: Unhandled status\n");
      exit(0);
      break;

  }

  printf(" You are in the %2.1f%% marginal tax bracket,\n and you are paying an effective %2.1f%% tax on your total income.\n", 100.0 * rate, (100.0 * tx) / income);
  fprintf(outfile, " You are in the %2.1f%% marginal tax bracket,\n and you are paying an effective %2.1f%% tax on your total income.\n", 100.0 * rate, (100.0 * tx) / income);
}

double ny_TaxRateLookup(double income, int ny_status)
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
  tax = ny_TaxRateFunction(income, ny_status);
  return (int) (tax + 0.5);
}

double ny_NYcityTaxRateFunction(double income, int ny_status)
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

  if ((ny_status == 2) || (ny_status == 5))
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
    if ((ny_status == 1) || (ny_status == 3))
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
    if (ny_status == 4)
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
    printf("Status not covered.\n");
    exit(1);
  }



  if (income < 65000.0)
    tax = (int) (tax + 0.5);

  return tax;
}

void ny_worksheet1()
{
  double ws[100];
  ws[1] = L[33];
  ws[2] = L[38];
  ws[3] = 0.0633 * ws[2];
  if (ws[1] >= 157650.0)
    ws[9] = ws[3];
  else
  {
    ws[4] = ny_TaxRateFunction(ws[2], ny_status);
    ws[5] = ws[3] - ws[4];
    ws[6] = ws[1] - 107650.0;
    ws[7] = 0.0001 * ((double) Round(10000.0 * (ws[6] / 50000.0)));
    ws[8] = ws[5] * ws[7];
    ws[9] = ws[4] + ws[8];
  }

  L[39] = ws[9];
}

void ny_worksheet2()
{
  double ws[100];
  ws[1] = L[33];
  ws[2] = L[38];
  ws[3] = 0.0657 * ws[2];
  if (ws[1] >= 211550.0)
    ws[11] = ws[3];
  else
  {
    ws[4] = ny_TaxRateFunction(ws[2], ny_status);
    ws[5] = ws[3] - ws[4];
    ws[6] = 629.0;
    ws[7] = ws[5] - ws[6];
    ws[8] = ws[1] - 161550.0;
    ws[9] = 0.0001 * ((double) Round(10000.0 * (ws[8] / 50000.0)));
    ws[10] = ws[7] * ws[9];
    ws[11] = (ws[4] + ws[6]) + ws[10];
  }

  L[39] = ws[11];
}

void ny_worksheet3()
{
  double ws[100];
  ws[1] = L[33];
  ws[2] = L[38];
  ws[3] = 0.0685 * ws[2];
  if (ws[1] >= 373200.0)
    ws[11] = ws[3];
  else
  {
    ws[4] = ny_TaxRateFunction(ws[2], ny_status);
    ws[5] = ws[3] - ws[4];
    ws[6] = 1017.0;
    ws[7] = ws[5] - ws[6];
    ws[8] = ws[1] - 323200.0;
    ws[9] = 0.0001 * ((double) Round(10000.0 * (ws[8] / 50000.0)));
    ws[10] = ws[7] * ws[9];
    ws[11] = (ws[4] + ws[6]) + ws[10];
  }

  L[39] = ws[11];
}

void ny_worksheet4()
{
  double ws[100];
  ws[1] = L[33];
  ws[2] = L[38];
  ws[3] = 0.0882 * ws[2];
  if (ws[1] >= 2205350.0)
    ws[11] = ws[3];
  else
  {
    ws[4] = ny_TaxRateFunction(ws[2], ny_status);
    ws[5] = ws[3] - ws[4];
    if (ws[2] <= 161550.0)
      ws[6] = 629.0;
    else
      if (ws[2] <= 323200.0)
      ws[6] = 1017.0;
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

void ny_worksheet5()
{
  double ws[100];
  ws[1] = L[33];
  ws[2] = L[38];
  ws[3] = 0.0657 * ws[2];
  if (ws[1] >= 157650.0)
    ws[9] = ws[3];
  else
  {
    ws[4] = ny_TaxRateFunction(ws[2], ny_status);
    ws[5] = ws[3] - ws[4];
    ws[6] = ws[1] - 107650.0;
    ws[7] = 0.0001 * ((double) Round(10000.0 * (ws[6] / 50000.0)));
    ws[8] = ws[5] * ws[7];
    ws[9] = ws[4] + ws[8];
  }

  L[39] = ws[9];
}

void ny_worksheet6()
{
  double ws[100];
  ws[1] = L[33];
  ws[2] = L[38];
  ws[3] = 0.0685 * ws[2];
  if (ws[1] >= 265400.0)
    ws[11] = ws[3];
  else
  {
    ws[4] = ny_TaxRateFunction(ws[2], ny_status);
    ws[5] = ws[3] - ws[4];
    ws[6] = 506.0;
    ws[7] = ws[5] - ws[6];
    ws[8] = ws[1] - 215400.0;
    ws[9] = 0.0001 * ((double) Round(10000.0 * (ws[8] / 50000.0)));
    ws[10] = ws[7] * ws[9];
    ws[11] = (ws[4] + ws[6]) + ws[10];
  }

  L[39] = ws[11];
}

void ny_worksheet7()
{
  double ws[100];
  ws[1] = L[33];
  ws[2] = L[38];
  ws[3] = 0.0882 * ws[2];
  if (ws[1] >= 1127550.0)
    ws[11] = ws[3];
  else
  {
    ws[4] = ny_TaxRateFunction(ws[2], ny_status);
    ws[5] = ws[3] - ws[4];
    if (ws[2] <= 215400.0)
      ws[6] = 506.0;
    else
      ws[6] = 1109.0;

    ws[7] = ws[5] - ws[6];
    ws[8] = ws[1] - 1077550.0;
    ws[9] = 0.0001 * ((double) Round(10000.0 * (ws[8] / 50000.0)));
    ws[10] = ws[7] * ws[9];
    ws[11] = (ws[4] + ws[6]) + ws[10];
  }

  L[39] = ws[11];
}

void ny_worksheet8()
{
  double ws[100];
  ws[1] = L[33];
  ws[2] = L[38];
  ws[3] = 0.0657 * ws[2];
  if (ws[1] >= 157650.0)
    ws[9] = ws[3];
  else
  {
    ws[4] = ny_TaxRateFunction(ws[2], ny_status);
    ws[5] = ws[3] - ws[4];
    ws[6] = ws[1] - 107650.0;
    ws[7] = 0.0001 * ((double) Round(10000.0 * (ws[6] / 50000.0)));
    ws[8] = ws[5] * ws[7];
    ws[9] = ws[4] + ws[8];
  }

  L[39] = ws[9];
}

void ny_worksheet9()
{
  double ws[100];
  ws[1] = L[33];
  ws[2] = L[38];
  ws[3] = 0.0685 * ws[2];
  if (ws[1] >= 319300.0)
    ws[11] = ws[3];
  else
  {
    ws[4] = ny_TaxRateFunction(ws[2], ny_status);
    ws[5] = ws[3] - ws[4];
    ws[6] = 729.0;
    ws[7] = ws[5] - ws[6];
    ws[8] = ws[1] - 269300.0;
    ws[9] = 0.0001 * ((double) Round(10000.0 * (ws[8] / 50000.0)));
    ws[10] = ws[7] * ws[9];
    ws[11] = (ws[4] + ws[6]) + ws[10];
  }

  L[39] = ws[11];
}

void ny_worksheet10()
{
  double ws[100];
  ws[1] = L[33];
  ws[2] = L[38];
  ws[3] = 0.0882 * ws[2];
  if (ws[1] >= 1666450.0)
    ws[11] = ws[3];
  else
  {
    ws[4] = ny_TaxRateFunction(ws[2], ny_status);
    ws[5] = ws[3] - ws[4];
    if (ws[2] <= 269300.0)
      ws[6] = 729.0;
    else
      ws[6] = 1483.0;

    ws[7] = ws[5] - ws[6];
    ws[8] = ws[1] - 1616450.0;
    ws[9] = 0.0001 * ((double) Round(10000.0 * (ws[8] / 50000.0)));
    ws[10] = ws[7] * ws[9];
    ws[11] = (ws[4] + ws[6]) + ws[10];
  }

  L[39] = ws[11];
}

void ny_tax_computation_worksheet(int ny_status)
{
  switch (ny_status)
  {
    case 2:

    case 5:
      if (L[33] <= 2155350.0)
    {
      if (L[38] <= 161550.0)
        ny_worksheet1();
      else
        if ((L[33] > 161550.0) && (L[38] <= 323200.0))
        ny_worksheet2();
      else
        if ((L[33] > 323200.0) && (L[38] > 323200.0))
        ny_worksheet3();
      else
        ny_worksheet4();



    }
    else
      ny_worksheet4();

      break;

    case 1:

    case 3:
      if (L[33] <= 1077550.0)
    {
      if (L[38] <= 215400.0)
        ny_worksheet5();
      else
        ny_worksheet6();

    }
    else
      ny_worksheet7();

      break;

    case 4:
      if (L[33] <= 1616450.0)
    {
      if (L[38] <= 269300.0)
        ny_worksheet8();
      else
        ny_worksheet9();

    }
    else
      ny_worksheet10();

      break;

    default:
      printf("Case not handled.\n");
      fprintf(outfile, "Case not handled.\n");
      exit(1);

  }

}

int ny_main(int argc, char *argv[])
{
  int j;
  int k;
  int argk;
  int day;
  int month;
  int yyyy;
  char word[1000];
  char *infname = 0;
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
  printf("NY-IT201 - 2011 - v%3.1f\n", ny_thisversion);
  argk = 1;
  k = 1;
  while (argk < argc)
  {
    if (strcmp(argv[argk], "-verbose") == 0)
      verbose = 1;
    else
      if (k == 1)
    {
      infname = strdup(argv[argk]);
      infile = fopen(argv[argk], "r");
      if (infile == 0)
      {
        printf("ERROR: Parameter file '%s' could not be opened.\n", argv[argk]);
        exit(1);
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
        printf("ERROR: Output file '%s' could not be opened.\n", outfname);
        exit(1);
      }

      printf("Writing results to file:  %s\n", outfname);
    }
    else
    {
      printf("Unknown command-line parameter '%s'\n", argv[argk]);
      exit(1);
    }


    argk = argk + 1;
  }

  if (infile == 0)
  {
    printf("Error: No input file on command line.\n");
    exit(1);
  }

  for (j = 0; j < 1000; j++)
  {
    L[j] = 0.0;
    ded_sched[j] = 0.0;
  }

  read_line(infile, word);
  now = time(0);
  fprintf(outfile, "\n%s,	 v%2.2f, %s\n", word, ny_thisversion, ctime(&now));
  get_parameter(infile, 's', word, "FileName");
  get_word(infile, prelim_1040_outfilename);
  if (ny_ImportFederalReturnData(prelim_1040_outfilename, &ny_PrelimFedReturn) == 0)
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
  L[1] = ny_PrelimFedReturn.fedline[1];
  if (ny_PrelimFedReturn.Itemized)
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
  L[2] = ny_PrelimFedReturn.fedline[2];
  showline(2);
  L[3] = ny_PrelimFedReturn.fedline[3];
  showline(3);
  L[4] = ny_PrelimFedReturn.sched[1][10];
  showline(4);
  L[5] = ny_PrelimFedReturn.sched[1][11];
  showline(5);
  L[6] = ny_PrelimFedReturn.sched[1][12];
  showline(6);
  L[7] = ny_PrelimFedReturn.sched[1][13];
  showline(7);
  L[8] = ny_PrelimFedReturn.sched[1][14];
  showline(8);
  GetLine("L9", &L[9]);
  showline(9);
  GetLine("L10", &L[10]);
  showline(10);
  L[11] = ny_PrelimFedReturn.sched[1][17];
  showline(11);
  L[13] = ny_PrelimFedReturn.sched[1][18];
  showline(13);
  L[14] = ny_PrelimFedReturn.sched[1][19];
  showline(14);
  L[15] = ny_PrelimFedReturn.fedline[5];
  showline(15);
  L[27] = L[15];
  L[16] = ny_PrelimFedReturn.sched[1][21];
  showline(16);
  for (j = 1; j <= 11; j++)
    L[17] = L[17] + L[j];

  for (j = 13; j <= 16; j++)
    L[17] = L[17] + L[j];

  showline(17);
  if (absolutev(L[17] - ny_PrelimFedReturn.fedline[6]) > 0.1)
  {
    printf(" Warning: L[17] = %6.2f, while Fed-line[6] = %6.2f\n", L[17], ny_PrelimFedReturn.fedline[6]);
    fprintf(outfile, " Warning: L[17] = %6.2f, while Fed-line[6] = %6.2f\n", L[17], ny_PrelimFedReturn.fedline[6]);
  }

  L[18] = ny_PrelimFedReturn.sched[1][36];
  showline(18);
  L[19] = L[17] - L[18];
  showline_wmsg(19, "Federal adjusted gross income");
  if (absolutev(L[19] - ny_PrelimFedReturn.fedline[7]) > 0.1)
  {
    printf(" Warning: L[19] = %6.2f, while Fed-line[7] = %6.2f\n", L[19], ny_PrelimFedReturn.fedline[7]);
    fprintf(outfile, " Warning: L[19] = %6.2f, while Fed-line[7] = %6.2f\n", L[19], ny_PrelimFedReturn.fedline[7]);
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
  if (ny_status == 2)
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
  ded_sched[1] = ny_PrelimFedReturn.schedA[1];
  ded_sched[2] = L[19];
  ded_sched[3] = 0.10 * ded_sched[2];
  ded_sched[4] = ded_sched[1] - ded_sched[3];
  ded_sched[9] = ny_PrelimFedReturn.schedA[7];
  ded_sched[15] = ny_PrelimFedReturn.schedA[10];
  ded_sched[19] = ny_PrelimFedReturn.schedA[14];
  ded_sched[20] = ny_PrelimFedReturn.schedA[15];
  ded_sched[39] = ny_PrelimFedReturn.schedA[16];
  ded_sched[40] = (((((ded_sched[4] + ded_sched[9]) + ded_sched[15]) + ded_sched[19]) + ded_sched[20]) + ded_sched[28]) + ded_sched[39];
  itemized_ded = ded_sched[40];
  switch (ny_status)
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
  if (L[33] <= 107650.0)
    L[39] = ny_TaxRateLookup(L[38], ny_status);
  else
    ny_tax_computation_worksheet(ny_status);

  showline(39);
  ny_Report_bracket_info(L[38], L[39], ny_status);
  get_parameter(infile, 's', word, "Exemptions");
  get_parameter(infile, 'i', &Exemptions, "Exemptions");
  if (Dependent)
    L[40] = 0.0;
  else
    if (ny_status == 1)
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
    if (ny_status != 3)
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
    L[47] = ny_NYcityTaxRateFunction(L[38], ny_status);
    showline(47);
    if (Dependent)
      L[48] = 0.0;
    else
      if (ny_status == 1)
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
      if (ny_status != 3)
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
      if (((ny_status == 1) || (ny_status == 3)) || (ny_status == 4))
        L[69] = 63.0;
      else
        if ((ny_status == 2) || (ny_status == 5))
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

  if (ny_Your1stName)
    fprintf(outfile, "Your1stName: %s\n", ny_Your1stName);

  if (ny_YourInitial)
    fprintf(outfile, "YourInitial: %s\n", ny_YourInitial);

  if (ny_YourLastName)
    fprintf(outfile, "YourLastName: %s\n", ny_YourLastName);

  if (ny_YourSocSec)
    fprintf(outfile, "YourSocSec#: %s\n", ny_YourSocSec);

  if (ny_Spouse1stName)
    fprintf(outfile, "Spouse1stName: %s\n", ny_Spouse1stName);

  if (ny_SpouseInitial)
    fprintf(outfile, "SpouseInitial: %s\n", ny_SpouseInitial);

  if (ny_SpouseLastName)
    fprintf(outfile, "SpouseLastName: %s\n", ny_SpouseLastName);

  if (ny_SpouseSocSec)
    fprintf(outfile, "SpouseSocSec#: %s\n", ny_SpouseSocSec);

  if (ny_MailAddress)
    fprintf(outfile, "Number&Street: %s\n", ny_MailAddress);

  if (ny_AptNumber)
    fprintf(outfile, "Apt#: %s\n", ny_AptNumber);

  if (ny_Town[0] != '\0')
    fprintf(outfile, "Town: %s\n", ny_Town);

  if (ny_StateName[0] != '\0')
    fprintf(outfile, "StateName: %s\n", ny_StateName);

  if (ny_Zipcode[0] != '\0')
    fprintf(outfile, "Zipcode: %s\n", ny_Zipcode);

  if (strlen(ny_YourLastName) > 0)
  {
    strcpy(YourNames, ny_YourLastName);
    strcat(YourNames, ", ");
    strcat(YourNames, ny_Your1stName);
    if (ny_YourInitial[0] != '\0')
    {
      strcat(YourNames, ", ");
      strcat(YourNames, ny_YourInitial);
    }

    if (ny_Spouse1stName[0] != '\0')
    {
      strcat(YourNames, ", ");
      if ((ny_SpouseLastName[0] != '\0') && (strcmp(ny_YourLastName, ny_SpouseLastName) != 0))
      {
        strcat(YourNames, ny_SpouseLastName);
        strcat(YourNames, ", ");
      }

      strcat(YourNames, ny_Spouse1stName);
      if (ny_SpouseInitial[0] != '\0')
      {
        strcat(YourNames, ", ");
        strcat(YourNames, ny_SpouseInitial);
      }

    }

    fprintf(outfile, "YourNames: %s\n", YourNames);
  }

  fclose(infile);
  grab_any_pdf_markups(infname, outfile);
  fclose(outfile);
  printf("\nListing results from file: %s\n\n", outfname);
  Display_File(outfname);
  return 0;
}


/* END of taxsolve_NY_IT201_2018.c */
/* START of taxsolve_OH_IT1040_2018.c */
double oh_thisversion = 16.01;
double oh_TaxRateFunction(double x, int status)
{
  if (x <= 10850.0)
    return 0.0;
  else
    if (x < 16300.0)
    return 80.56 + ((x - 10850.0) * 0.01980);
  else
    if (x < 21750.0)
    return 188.47 + ((x - 16300.0) * 0.02476);
  else
    if (x < 43450.0)
    return 323.41 + ((x - 21750.0) * 0.02969);
  else
    if (x < 86900.0)
    return 967.68 + ((x - 43450.0) * 0.03465);
  else
    if (x < 108700.0)
    return 2473.22 + ((x - 86900.0) * 0.03960);
  else
    if (x < 217400.0)
    return 3336.50 + ((x - 108700.0) * 0.04597);
  else
    return 8333.44 + ((x - 217400.0) * 0.04997);







}

void oh_Report_bracket_info(double income, double tx, int status)
{
  double rate;
  if (income <= 10850.0)
    rate = 0.0;
  else
    if (income < 16300.0)
    rate = 0.01980;
  else
    if (income < 21750.0)
    rate = 0.02476;
  else
    if (income < 43450.0)
    rate = 0.02969;
  else
    if (income < 86900.0)
    rate = 0.03465;
  else
    if (income < 108700.0)
    rate = 0.03960;
  else
    if (income < 217400.0)
    rate = 0.04597;
  else
    rate = 0.04997;







  printf(" You are in the %2.1f%% marginal tax bracket,\n and you are paying an effective %2.1f%% tax on your total income.\n", 100.0 * rate, (100.0 * tx) / income);
  fprintf(outfile, " You are in the %2.1f%% marginal tax bracket,\n and you are paying an effective %2.1f%% tax on your total income.\n", 100.0 * rate, (100.0 * tx) / income);
}

char *oh_pull_initial(char *name)
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

int oh_main(int argc, char *argv[])
{
  int j;
  int k;
  int mm;
  char word[4000];
  char *infname = 0;
  char outfname[4000];
  char label[90];
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
  printf("OH IT1040 2018 - v%3.1f\n", oh_thisversion);
  mm = 1;
  k = 1;
  while (mm < argc)
  {
    if (strcmp(argv[mm], "-verbose") == 0)
      verbose = 1;
    else
      if (k == 1)
    {
      infname = strdup(argv[mm]);
      infile = fopen(argv[mm], "r");
      if (infile == 0)
      {
        printf("ERROR: Parameter file '%s' could not be opened.\n", argv[mm]);
        exit(1);
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
        printf("ERROR: Output file '%s' could not be opened.\n", outfname);
        exit(1);
      }

      printf("Writing results to file:  %s\n", outfname);
    }
    else
    {
      printf("Unknown command-line parameter '%s'\n", argv[mm]);
      exit(1);
    }


    mm++;
  }

  if (infile == 0)
  {
    printf("Error: No input file on command line.\n");
    exit(1);
  }

  for (mm = 0; mm < 1000; mm++)
  {
    L[mm] = 0.0;
    SchedA[mm] = 0.0;
    SchedC[mm] = 0.0;
  }

  read_line(infile, word);
  now = time(0);
  fprintf(outfile, "\n%s,	 v%2.2f, %s\n", word, oh_thisversion, ctime(&now));
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
    printf("Error: unrecognized status '%s'. Must be: Single, Married/joint, Married/sep, Head_of_house.\nExiting.\n", word);
    fprintf(outfile, "Error: unrecognized status '%s'. Must be: Single, Married/joint, Married/sep, Head_of_house.\nExiting.\n", word);
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
  GetLine("SchedA_31", &SchedA[21]);
  GetLine("SchedA_32", &SchedA[32]);
  GetLine("SchedA_33", &SchedA[33]);
  GetLine("SchedA_34", &SchedA[34]);
  GetLine("SchedA_35", &SchedA[35]);
  GetLine("SchedA_36", &SchedA[36]);
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

  for (j = 11; j <= 36; j++)
    SchedA[37] = SchedA[37] + SchedA[j];

  L2a = SchedA[10];
  L2b = SchedA[37];
  L[3] = (L[1] + L2a) - L2b;
  if (L[3] <= 40000.0)
    exemption_amnt = 2350.0;
  else
    if (L[3] <= 80000.0)
    exemption_amnt = 2100.0;
  else
    exemption_amnt = 1850.0;


  L[4] = exemption_amnt * exemptions;
  L[5] = NotLessThanZero(L[3] - L[4]);
  L[7] = NotLessThanZero(L[5] - L[6]);
  L7a = L[7];
  L8a = oh_TaxRateFunction(L7a, status);
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
  oh_Report_bracket_info(L[7], L[13], status);
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

  fprintf(outfile, "\n-- 2018 Ohio Schedule A --\n");
  for (j = 1; j <= 37; j++)
  {
    sprintf(label, "SchedA%d", j);
    showline_wlabel(label, SchedA[j]);
  }

  fprintf(outfile, "\n-- 2018 Ohio Schedule of Credits --\n");
  for (j = 1; j <= 27; j++)
  {
    sprintf(label, "Credits%d", j);
    showline_wlabel(label, SchedC[j]);
  }

  sprintf(word, "%5.4f", factorA);
  printf("factorA = %g, word = '%s'\n", factorA, word);
  fprintf(outfile, "   Credits27_Factor %s\n", &word[2]);
  showline_wlabel("Credits28", SchedC[28]);
  showline_wlabel("Credits29", SchedC[29]);
  showline_wlabel("Credits30", SchedC[30]);
  sprintf(word, "%5.4f", factorB);
  printf("factorB = %g, word = '%s'\n", factorB, word);
  fprintf(outfile, "   Credits30_Factor %s\n", &word[2]);
  for (j = 31; j <= 40; j++)
  {
    sprintf(label, "Credits%d", j);
    showline_wlabel(label, SchedC[j]);
  }

  fprintf(outfile, "\n{ --------- }\n");
  pname = GetTextLine("Your1stName:");
  MidInit = oh_pull_initial(pname);
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
  MidInit = oh_pull_initial(pname);
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
  grab_any_pdf_markups(infname, outfile);
  fclose(outfile);
  Display_File(outfname);
  printf("\nResults written to file:  %s\n", outfname);
  return 0;
}


/* END of taxsolve_OH_IT1040_2018.c */
/* START of taxsolve_PA_40_2018.c */
double pa_Tax_Rate = 0.0307;
double pa_pos(double x)
{
  if (x > 0.0)
    return x;
  else
    return 0.0;

}

int pa_main(int argc, char *argv[])
{
  int i;
  int j;
  int k;
  int status = 0;
  char word[2000];
  char *infname = 0;
  char outfname[1500];
  time_t now;
  double oneA;
  double oneB;
  char *Your1stName = 0;
  char *YourLastName = 0;
  char *Spouse1stName = 0;
  char *SpouseLastName;
  char *YourNames;
  printf("PA40 - 2018 - v%3.1f\n", 16.01);
  i = 1;
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
      infname = strdup(argv[i]);
      infile = fopen(argv[i], "r");
      if (infile == 0)
      {
        printf("ERROR: Parameter file '%s' could not be opened.\n", argv[i]);
        exit(1);
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
        printf("ERROR: Output file '%s' could not be opened.\n", outfname);
        exit(1);
      }

      printf("Writing results to file:  %s\n", outfname);
    }
    else
    {
      printf("Unknown command-line parameter '%s'\n", argv[i]);
      exit(1);
    }


    i = i + 1;
  }

  if (infile == 0)
  {
    printf("Error: No input file on command line.\n");
    exit(1);
  }

  for (i = 0; i < 1000; i++)
    L[i] = 0.0;

  read_line(infile, word);
  now = time(0);
  fprintf(outfile, "\n%s,	 v%2.2f, %s\n", word, 16.01, ctime(&now));
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
    printf("Error: unrecognized status '%s'. Exiting.\n", word);
    fprintf(outfile, "Error: unrecognized status '%s'. Exiting.\n", word);
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


  L[9] = ((((((pa_pos(L[1]) + pa_pos(L[2])) + pa_pos(L[3])) + pa_pos(L[4])) + pa_pos(L[5])) + pa_pos(L[6])) + pa_pos(L[7])) + pa_pos(L[8]);
  showline_wmsg(9, "Total PA Taxable Income");
  GetLineF("L10", &L[10]);
  L[11] = L[9] - L[10];
  showline_wmsg(11, "Adjusted PA Taxable Income");
  L[12] = pa_Tax_Rate * L[11];
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
  grab_any_pdf_markups(infname, outfile);
  fclose(outfile);
  printf("\nListing results from file: %s\n\n", outfname);
  Display_File(outfname);
  return 0;
}


/* END of taxsolve_PA_40_2018.c */
/* START of taxsolve_VA_760_2018.c */
float va_thisversion = 16.01;
double va_TaxRateFunction(double income, int status)
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

void va_Report_bracket_info(double income, double tx, int status)
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



  printf(" You are in the %2.1f%% marginal tax bracket,\n and you are paying an effective %2.1f%% tax on your total income.\n", 100.0 * rate, (100.0 * tx) / income);
  fprintf(outfile, " You are in the %2.1f%% marginal tax bracket,\n and you are paying an effective %2.1f%% tax on your total income.\n", 100.0 * rate, (100.0 * tx) / income);
}

struct date_record va_yourDOB;
struct date_record va_spouseDOB;
struct date_record va_DL;
int va_main(int argc, char *argv[])
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
  char *infname = 0;
  int status = 0;
  int exemptionsA = 0;
  int exemptionsB = 0;
  int youBlind = 0;
  int spouseBlind = 0;
  time_t now;
  double L20b = 0.0;
  double std_ded = 0.0;
  double min2file;
  printf("VA-760 2018 - v%3.1f\n", va_thisversion);
  i = 1;
  k = 1;
  while (i < argc)
  {
    if (strcmp(argv[i], "-verbose") == 0)
      verbose = 1;
    else
      if (k == 1)
    {
      infname = strdup(argv[i]);
      infile = fopen(argv[i], "r");
      if (infile == 0)
      {
        printf("ERROR: Parameter file '%s' could not be opened.\n", argv[i]);
        exit(1);
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
        printf("ERROR: Output file '%s' could not be opened.\n", outfname);
        exit(1);
      }

      printf("Writing results to file:  %s\n", outfname);
    }
    else
    {
      printf("Unknown command-line parameter '%s'\n", argv[i]);
      exit(1);
    }


    i = i + 1;
  }

  if (infile == 0)
  {
    printf("Error: No input file on command line.\n");
    exit(1);
  }

  for (i = 0; i < 1000; i++)
  {
    L[i] = 0.0;
  }

  read_line(infile, word);
  now = time(0);
  fprintf(outfile, "\n%s,	 v%2.2f, %s\n", word, va_thisversion, ctime(&now));
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
    printf("Error: unrecognized status '%s'. Must be: Single, Married/joint, Married/sep, Head_of_house, Widow(er)\nExiting.\n", word);
    fprintf(outfile, "Error: unrecognized status '%s'. Must be: Single, Married/joint, Married/sep, Head_of_house, Widow(er)\nExiting.\n", word);
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

  if (interpret_date(datestr, &va_yourDOB.month, &va_yourDOB.day, &va_yourDOB.year, "Bad YourDOB") != 1)
    exit(1);

  twrd = format_mmddyyyy(va_yourDOB.month, va_yourDOB.day, va_yourDOB.year);
  fprintf(outfile, "YourDOB: %s\n", twrd);
  writeout_line = 1;
  GetTextLineF("YourDrivLic:");
  writeout_line = 0;
  datestr = GetTextLineF("YourDLdate:");
  if ((datestr[0] != '\0') && interpret_date(datestr, &va_DL.month, &va_DL.day, &va_DL.year, "Bad YourDL"))
  {
    twrd = format_mmddyyyy(va_DL.month, va_DL.day, va_DL.year);
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

    if (interpret_date(datestr, &va_spouseDOB.month, &va_spouseDOB.day, &va_spouseDOB.year, "Bad SpouseDOB") != 1)
      exit(1);

    twrd = format_mmddyyyy(va_spouseDOB.month, va_spouseDOB.day, va_spouseDOB.year);
    fprintf(outfile, "SpouseDOB: %s\n", twrd);
  }

  writeout_line = 1;
  GetTextLineF("SpouseDrivLic:");
  writeout_line = 0;
  datestr = GetTextLineF("SpouseDLdate:");
  if ((datestr[0] != '\0') && interpret_date(datestr, &va_DL.month, &va_DL.day, &va_DL.year, "Bad YourDL"))
  {
    twrd = format_mmddyyyy(va_DL.month, va_DL.day, va_DL.year);
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
  if (va_yourDOB.year < 1954)
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
  get_param_single_line(infile, 'b', &spouseBlind, "SpouseBlind");
  if (status == 2)
  {
    if (va_spouseDOB.year < 1954)
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
      printf("Unexpected status.\n");
      fprintf(outfile, "Unexpected status.\n");
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
  L[17] = va_TaxRateFunction(L[16], status);
  showline(17);
  va_Report_bracket_info(L[16], L[17], status);
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
  grab_any_pdf_markups(infname, outfile);
  fclose(outfile);
  Display_File(outfname);
  printf("\nResults written to file:  %s\n", outfname);
  return 0;
}


/* END of taxsolve_VA_760_2018.c */
