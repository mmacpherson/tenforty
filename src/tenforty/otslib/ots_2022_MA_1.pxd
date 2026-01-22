# distutils: language = c++

cdef extern from "ots_2022.cpp" namespace "OpenTaxSolver2022::taxsolve_MA_1_2022":
    int main( int argc, char *argv[] )

