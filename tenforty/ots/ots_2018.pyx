# -*- coding: utf-8 -*-
# cython: language_level=3

import tempfile
import shutil

from libc.stdlib cimport malloc, free

ctypedef int (*f_type)(int, char**)


cdef extern from "Python.h":
    const char* PyUnicode_AsUTF8(object unicode)


cdef extern from "_ots_2018.c":
    double _us_TaxRateFormula(double x, int us_status)
    int _us_main(int argc, char *argv[])


cdef int call_argc_argv(f_type fn, char* infile):
    cdef char** c_argv = NULL
    c_argv = <char**>malloc(sizeof(char*) * 2)
    if c_argv is NULL:
        raise MemoryError()
    try:
        c_argv[1] = infile
        result = fn(2, c_argv)
    finally:
        free(c_argv)

    return result


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
