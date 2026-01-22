# distutils: language = c++

cdef extern from "ots_2021.cpp" namespace "OpenTaxSolver2021::taxsolve_f2210_2021":
    int main( int argc, char *argv[] )

