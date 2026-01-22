# distutils: language = c++

cdef extern from "ots_2020.cpp" namespace "OpenTaxSolver2020::taxsolve_PA_40_2020":
    int main( int argc, char *argv[] )

