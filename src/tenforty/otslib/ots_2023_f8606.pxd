# distutils: language = c++

cdef extern from "ots_2023.cpp" namespace "OpenTaxSolver2023::taxsolve_f8606":
    int main( int argc, char *argv[] )

