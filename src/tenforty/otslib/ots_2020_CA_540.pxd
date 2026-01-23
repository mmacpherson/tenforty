# distutils: language = c++

cdef extern from "ots_2020_CA_540.cpp" namespace "OpenTaxSolver2020::taxsolve_CA_540_2020":
    int main( int argc, char *argv[] )
