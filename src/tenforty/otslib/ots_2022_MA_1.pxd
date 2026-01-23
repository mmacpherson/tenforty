# distutils: language = c++

cdef extern from "ots_2022_MA_1.cpp" namespace "OpenTaxSolver2022::taxsolve_MA_1_2022":
    int main( int argc, char *argv[] )
