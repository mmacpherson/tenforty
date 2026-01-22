# distutils: language = c++

cdef extern from "ots_2018.cpp" namespace "OpenTaxSolver2018::taxsolve_CA_540_2018":
    int main( int argc, char *argv[] )

