# distutils: language = c++

cdef extern from "ots_2024_f2210.cpp" namespace "OpenTaxSolver2024::taxsolve_f2210_2024":
    int main( int argc, char *argv[] )

