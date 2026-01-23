# distutils: language = c++

cdef extern from "ots_2022_f8829.cpp" namespace "OpenTaxSolver2022::taxsolve_f8829_2022":
    int main( int argc, char *argv[] )
