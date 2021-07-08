# -*- coding: utf-8 -*-
# cython: language_level=3

from common cimport call_argc_argv, PyUnicode_AsUTF8

import tempfile
import shutil

cdef extern from "_ots_2020.c":
    double _us_TaxRateFormula(double x, int us_status)
    int _us_main(int argc, char *argv[])

def tax_rate(double x, int status):
    return _us_TaxRateFormula(x, status)


def us_main(infile_text):

    tmpdir = tempfile.mkdtemp()

    returnfile = f"{tmpdir}/peace.txt"
    returnfile_completed = f"{tmpdir}/peace_out.txt"

    with open(returnfile, "w") as fp:
        print(infile_text, file=fp)

    retcode = call_argc_argv(_us_main, PyUnicode_AsUTF8(returnfile))

    with open(returnfile_completed) as fp:
        result = fp.read()

    shutil.rmtree(tmpdir)

    return result
