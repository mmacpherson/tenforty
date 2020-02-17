from libc.stdlib cimport malloc, free

cdef extern from "Python.h":
    const char* PyUnicode_AsUTF8(object unicode)

ctypedef int (*f_type)(int, char**)

cdef inline int call_argc_argv(f_type fn, char* infile):
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
