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

_OTS_KEY_TO_INDEX = {{
{LOOKUP_DICT_KEYS}
}}

cdef f_type _ots_get_function(int index):
{LOOKUP_SWITCH}
    return NULL

cdef f_type lookup_ots_call(int year, str form):
    cdef tuple key = (year, form)
    if key in _OTS_KEY_TO_INDEX:
        return _ots_get_function(_OTS_KEY_TO_INDEX[key])
    return NULL


# TEMPORARY WORKAROUND for an upstream OTS 2025 defect (docs/upstream-ots-reports.md,
# item 15). The 2025 US 1040 program declares these flags at file scope with a zero
# initializer and sets each to 1 only on a Y answer to its age-65 question; nothing
# sets it back to 0. OTS expects one return per process, so the initializer is the
# reset. tenforty runs many returns in one process, so without this a return
# answering N would inherit an earlier Y and take Schedule 1-A senior deduction
# lines 36a/36b it does not claim. The vendored source is not modified: the binding
# layer restores the fresh-process value before each call. Remove this block (and
# its call below) once an OTS release resets the flags itself;
# tests/ots_age_flag_reset_test.py has a strict-xfail tripwire that flips then.
cdef extern from "ots_amalgamation.cpp" namespace "OpenTaxSolver2025::taxsolve_US_1040_2025":
    int over65you
    int over65spouse


cdef void _restore_fresh_process_state(int year, str form):
    """Reset file-scope OTS state that a fresh process would start at zero."""
    global over65you, over65spouse
    if year == 2025 and form == "US_1040":
        over65you = 0
        over65spouse = 0


def _evaluate_form(year, form, form_text, fed_form_text=None, on_error="raise"):
    """Evaluate an OTS tax form given year, form, and form content.

    year: int
    form: str
    form_text: str
    fed_file: str | None
    on_error: str - "raise", "warn", or "ignore"

    Returns completed form as str.

    Validation of year, form, form_text is not done here, but in
    `tenforty.evaluate_form`, because it's more conveniently outside the cython
    context.
    """
    import warnings

    cdef f_type ots_form_function = lookup_ots_call(year, form)
    if ots_form_function is NULL:
        raise ValueError(f"Unknown year/form combination: {{year}}/{{form}}")

    cdef bytes program_name = b"ots"
    cdef bytes file_path_bytes
    cdef char** c_argv = NULL
    cdef int result_code

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

        file_path_bytes = returnfile.encode('utf-8')
        c_argv = <char**>malloc(sizeof(char*) * 3)  # +1 for NULL terminator
        if c_argv is NULL:
            raise MemoryError()
        try:
            c_argv[0] = program_name
            c_argv[1] = file_path_bytes
            c_argv[2] = NULL  # NULL-terminate argv
            _restore_fresh_process_state(year, form)
            result_code = ots_form_function(2, c_argv)
            if result_code != 0:
                error_msg = f"OTS returned non-zero exit code: {{result_code}}"
                if on_error == "raise":
                    from tenforty.models import OTSError
                    raise OTSError(result_code, year, form, error_msg)
                elif on_error == "warn":
                    warnings.warn(error_msg, RuntimeWarning)
        finally:
            free(c_argv)

        # OTS generates an output file based on the name of the input file.
        returnfile_completed = returnfile.replace(".txt", "_out.txt")
        with open(returnfile_completed) as fp:
            result = fp.read()

    return result
