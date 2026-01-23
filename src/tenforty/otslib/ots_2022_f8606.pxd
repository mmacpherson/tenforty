# distutils: language = c++

cdef extern from "ots_2022_f8606.cpp" namespace "OpenTaxSolver2022::taxsolve_f8606":
    int main( int argc, char *argv[] )
