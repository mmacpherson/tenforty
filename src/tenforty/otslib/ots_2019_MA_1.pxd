# distutils: language = c++

cdef extern from "ots_2019_MA_1.cpp" namespace "OpenTaxSolver2019::taxsolve_MA_1_2019":
    int main( int argc, char *argv[] )

