from libc.stdlib cimport malloc, free


ctypedef int (*f_type)(int, char**)


cdef extern from "Python.h":
    const char* PyUnicode_AsUTF8(object unicode)


cdef extern from "_ots_2018.c":
    double us_TaxRateFormula(double x, int us_status)    
    int us_main(int argc, char *argv[])


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
    return us_TaxRateFormula(x, status) 


def US_main(infile):
    return call_argc_argv(us_main, PyUnicode_AsUTF8(infile))
