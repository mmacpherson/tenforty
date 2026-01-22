# distutils: language = c++

cdef extern from "ots_2021.cpp" namespace "OpenTaxSolver2021::taxsolve_HSA_f8889":
    int main( int argc, char *argv[] )

