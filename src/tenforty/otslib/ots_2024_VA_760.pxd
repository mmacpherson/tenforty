# distutils: language = c++

cdef extern from "ots_2024_VA_760.cpp" namespace "OpenTaxSolver2024::taxsolve_VA_760_2024":
    int main( int argc, char *argv[] )
