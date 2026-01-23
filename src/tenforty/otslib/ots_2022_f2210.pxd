# distutils: language = c++

cdef extern from "ots_2022_f2210.cpp" namespace "OpenTaxSolver2022::taxsolve_f2210_2022":
    int main( int argc, char *argv[] )

