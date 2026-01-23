# distutils: language = c++

cdef extern from "ots_2024_HSA_f8889.cpp" namespace "OpenTaxSolver2024::taxsolve_HSA_f8889":
    int main( int argc, char *argv[] )
