# distutils: language = c++

cdef extern from "ots_2022_f8960.cpp" namespace "OpenTaxSolver2022::taxsolve_f8960_2022":
    int main( int argc, char *argv[] )
