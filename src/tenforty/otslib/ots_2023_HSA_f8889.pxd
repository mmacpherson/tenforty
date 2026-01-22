# distutils: language = c++

cdef extern from "ots_2023.cpp" namespace "OpenTaxSolver2023::taxsolve_HSA_f8889":
    int main( int argc, char *argv[] )
