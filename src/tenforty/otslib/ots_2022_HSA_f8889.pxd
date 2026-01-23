# distutils: language = c++

cdef extern from "ots_2022_HSA_f8889.cpp" namespace "OpenTaxSolver2022::taxsolve_HSA_f8889":
    int main( int argc, char *argv[] )
