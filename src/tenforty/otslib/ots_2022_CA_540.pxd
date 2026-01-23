# distutils: language = c++

cdef extern from "ots_2022_CA_540.cpp" namespace "OpenTaxSolver2022::taxsolve_CA_540_2022":
    int main( int argc, char *argv[] )

