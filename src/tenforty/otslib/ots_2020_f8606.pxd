# distutils: language = c++

cdef extern from "ots_2020_f8606.cpp" namespace "OpenTaxSolver2020::taxsolve_f8606":
    int main( int argc, char *argv[] )
