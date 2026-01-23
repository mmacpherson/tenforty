# distutils: language = c++

cdef extern from "ots_2020_VA_760.cpp" namespace "OpenTaxSolver2020::taxsolve_VA_760_2020":
    int main( int argc, char *argv[] )

