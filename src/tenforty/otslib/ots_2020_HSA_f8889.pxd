# distutils: language = c++

cdef extern from "ots_2020.cpp" namespace "OpenTaxSolver2020::taxsolve_HSA_f8889":
    int main( int argc, char *argv[] )
