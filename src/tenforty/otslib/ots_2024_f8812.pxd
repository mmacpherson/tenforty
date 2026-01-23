# distutils: language = c++

cdef extern from "ots_2024_f8812.cpp" namespace "OpenTaxSolver2024::taxsolve_f8812_2024":
    int main( int argc, char *argv[] )

