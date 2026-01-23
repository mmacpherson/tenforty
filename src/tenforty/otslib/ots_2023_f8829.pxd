# distutils: language = c++

cdef extern from "ots_2023_f8829.cpp" namespace "OpenTaxSolver2023::taxsolve_f8829_2023":
    int main( int argc, char *argv[] )
