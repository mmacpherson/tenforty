# -*- coding: utf-8 -*-
# cython: language_level=3
# distutils: language = c++
# cython: c_string_type=unicode, c_string_encoding=utf8


import os
import sys
import tempfile

from libc.stdlib cimport free, malloc

ctypedef int (*f_type)(int, char **)

{CIMPORTS}

cdef f_type lookup_ots_call(int year, char* form):
{LOOKUP_FN_BODY}


def _evaluate_form(year, form, form_text, fed_form_text=None):
    """Evaluate an OTS tax form given year, form, and form content.

    year: int
    form: str
    form_text: str
    fed_file: str | None

    Returns completed form as str.

    Validation of year, form, form_text is not done here, but in
    `tenforty.evaluate_form`, because it's more conveniently outside the cython
    context.
    """

    cdef f_type ots_form_function = lookup_ots_call(year, form)

    cdef char** c_argv = NULL
    cdef object py_unicode_returnfile

    with tempfile.TemporaryDirectory() as tmpdir:

        # Run federal tax file first as needed.
        if fed_form_text is not None:
            # Write out federal file to hardcoded location.
            ff_path = f"{{tmpdir}}/fed-form.txt"
            with open(ff_path, "w") as fp:
                print(fed_form_text, file=fp)

            # Update location of fed file in state form.
            form_text= form_text.replace("{FED_FILENAME}", ff_path)

        returnfile = f"{{tmpdir}}/form.txt"
        with open(returnfile, "w") as fp:
            print(form_text, file=fp)

        c_argv = <char**>malloc(sizeof(char*) * 2)
        if c_argv is NULL:
            raise MemoryError()
        try:
            py_unicode_returnfile = returnfile
            c_argv[1] = py_unicode_returnfile
            ots_form_function(2, c_argv)
        finally:
            free(c_argv)

        # OTS generates an output file based on the name of the input file.
        returnfile_completed = returnfile.replace(".txt", "_out.txt")
        with open(returnfile_completed) as fp:
            result = fp.read()

    return result
