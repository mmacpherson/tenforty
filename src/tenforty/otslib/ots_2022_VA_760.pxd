# distutils: language = c++

cdef extern from "ots_2022_VA_760.cpp" namespace "OpenTaxSolver2022::taxsolve_VA_760_2022":
    int main( int argc, char *argv[] )

