# -*- coding: utf-8 -*-
# cython: language_level=3

from common cimport call_argc_argv, PyUnicode_AsUTF8

import tempfile
import shutil

cdef extern from "_ots_2020.c":
    double _us_TaxRateFormula(double x, int us_status)
    int _us_main(int argc, char *argv[])
    int _ca_main(int argc, char *argv[])

def tax_rate(double x, int status):
    return _us_TaxRateFormula(x, status)


def us_main(infile_text):

    tmpdir = tempfile.mkdtemp()

    returnfile = f"{tmpdir}/fed.txt"
    returnfile_completed = f"{tmpdir}/fed_out.txt"

    with open(returnfile, "w") as fp:
        print(infile_text, file=fp)

    retcode = call_argc_argv(_us_main, PyUnicode_AsUTF8(returnfile))

    with open(returnfile_completed) as fp:
        result = fp.read()

    shutil.rmtree(tmpdir)

    return result


def ca_main(cafile_text, fed_out_text):

    tmpdir = tempfile.mkdtemp()

    fedfile = f"{tmpdir}/fed_out.txt"
    with open(fedfile, "w") as fp:
        print(fed_out_text, file=fp)

    cafile = f"{tmpdir}/ca.txt"
    cafile_completed = f"{tmpdir}/ca_out.txt"


    # -- Prep to run state return:
    #    1. Insert Federal filename above.
    #

    cafile_text = "\n".join(f"FileName {tmpdir}/fed_out.txt" if s.startswith("FileName") else s for s in
                cafile_text.strip().split("\n"))


    with open(cafile, "w") as fp:
        print(cafile_text, file=fp)

    # print("FEDERAL")
    # print(fed_out_text)

    # print("CALIFORNIA")

    # print(cafile_text)

    retcode = call_argc_argv(_ca_main, PyUnicode_AsUTF8(cafile))

    with open(cafile_completed) as fp:
        result = fp.read()

    # shutil.rmtree(tmpdir)

    return result
