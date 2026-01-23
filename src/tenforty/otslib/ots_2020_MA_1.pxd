# distutils: language = c++

cdef extern from "ots_2020_MA_1.cpp" namespace "OpenTaxSolver2020::taxsolve_MA_1_2020":
    int main( int argc, char *argv[] )

