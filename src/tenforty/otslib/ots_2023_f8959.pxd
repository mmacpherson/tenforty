# distutils: language = c++

cdef extern from "ots_2023_f8959.cpp" namespace "OpenTaxSolver2023::taxsolve_f8959_2023":
    int main( int argc, char *argv[] )
