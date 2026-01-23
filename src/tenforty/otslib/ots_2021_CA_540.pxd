# distutils: language = c++

cdef extern from "ots_2021_CA_540.cpp" namespace "OpenTaxSolver2021::taxsolve_CA_540_2021":
    int main( int argc, char *argv[] )
