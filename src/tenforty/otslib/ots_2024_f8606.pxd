# distutils: language = c++

cdef extern from "ots_2024_f8606.cpp" namespace "OpenTaxSolver2024::taxsolve_f8606":
    int main( int argc, char *argv[] )

