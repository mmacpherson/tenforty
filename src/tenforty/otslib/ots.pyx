# -*- coding: utf-8 -*-
# cython: language_level=3
# distutils: language = c++
# cython: c_string_type=unicode, c_string_encoding=utf8


import os
import sys
import tempfile

from libc.stdlib cimport free, malloc

ctypedef int (*f_type)(int, char **)

cimport ots_2018_MA_1
cimport ots_2018_US_1040_Sched_C
cimport ots_2018_VA_760
cimport ots_2018_OH_IT1040
cimport ots_2018_CA_540
cimport ots_2018_NJ_1040
cimport ots_2018_PA_40
cimport ots_2018_US_1040
cimport ots_2018_NY_IT201
cimport ots_2018_NC_D400
cimport ots_2019_MA_1
cimport ots_2019_NY_IT201
cimport ots_2019_US_1040_Sched_C
cimport ots_2019_VA_760
cimport ots_2019_OH_IT1040
cimport ots_2019_PA_40
cimport ots_2019_US_1040
cimport ots_2019_NJ_1040
cimport ots_2019_NC_D400
cimport ots_2019_CA_540
cimport ots_2020_OH_IT1040
cimport ots_2020_NC_D400
cimport ots_2020_US_1040
cimport ots_2020_NY_IT201
cimport ots_2020_HSA_f8889
cimport ots_2020_MA_1
cimport ots_2020_VA_760
cimport ots_2020_f8606
cimport ots_2020_NJ_1040
cimport ots_2020_US_1040_Sched_C
cimport ots_2020_PA_40
cimport ots_2020_CA_540
cimport ots_2021_NJ_1040
cimport ots_2021_US_1040_Sched_SE
cimport ots_2021_f8606
cimport ots_2021_CA_540
cimport ots_2021_HSA_f8889
cimport ots_2021_MA_1
cimport ots_2021_NC_D400
cimport ots_2021_NY_IT201
cimport ots_2021_OH_IT1040
cimport ots_2021_PA_40
cimport ots_2021_US_1040
cimport ots_2021_US_1040_Sched_C
cimport ots_2021_VA_760
cimport ots_2021_CA_5805
cimport ots_2021_f2210
cimport ots_2021_f8960
cimport ots_2021_f8959
cimport ots_2022_HSA_f8889
cimport ots_2022_MA_1
cimport ots_2022_NJ_1040
cimport ots_2022_f8959
cimport ots_2022_f8960
cimport ots_2022_f8606
cimport ots_2022_CA_540
cimport ots_2022_CA_5805
cimport ots_2022_NC_D400
cimport ots_2022_NY_IT201
cimport ots_2022_OH_IT1040
cimport ots_2022_PA_40
cimport ots_2022_US_1040
cimport ots_2022_US_1040_Sched_C
cimport ots_2022_US_1040_Sched_SE
cimport ots_2022_VA_760
cimport ots_2022_f2210
cimport ots_2022_f8829
cimport ots_2022_f8995
cimport ots_2023_NJ_1040
cimport ots_2023_US_1040_Sched_C
cimport ots_2023_f8829
cimport ots_2023_f8959
cimport ots_2023_f8960
cimport ots_2023_f8995
cimport ots_2023_f8606
cimport ots_2023_CA_540
cimport ots_2023_HSA_f8889
cimport ots_2023_MA_1
cimport ots_2023_NC_D400
cimport ots_2023_NY_IT201
cimport ots_2023_OH_IT1040
cimport ots_2023_PA_40
cimport ots_2023_US_1040
cimport ots_2023_US_1040_Sched_SE
cimport ots_2023_VA_760
cimport ots_2023_f2210
cimport ots_2023_US_1040_Sched_E_brokerage_royalties
cimport ots_2023_CA_5805
cimport ots_2023_f8812
cimport ots_2024_NJ_1040
cimport ots_2024_US_1040_Sched_E_brokerage_royalties
cimport ots_2024_f8812
cimport ots_2024_f8829
cimport ots_2024_f8959
cimport ots_2024_f8960
cimport ots_2024_f8995
cimport ots_2024_US_1040_Sched_SE
cimport ots_2024_f8606
cimport ots_2024_CA_540
cimport ots_2024_CA_5805
cimport ots_2024_HSA_f8889
cimport ots_2024_MA_1
cimport ots_2024_NC_D400
cimport ots_2024_NY_IT201
cimport ots_2024_OH_IT1040
cimport ots_2024_PA_40
cimport ots_2024_US_1040
cimport ots_2024_US_1040_Sched_C
cimport ots_2024_VA_760
cimport ots_2024_f2210
cimport ots_2024_MI_1040
cimport ots_2024_OR_40

_OTS_LOOKUP = {
    (2018, "MA_1"): ots_2018_MA_1.main,
    (2018, "US_1040_Sched_C"): ots_2018_US_1040_Sched_C.main,
    (2018, "VA_760"): ots_2018_VA_760.main,
    (2018, "OH_IT1040"): ots_2018_OH_IT1040.main,
    (2018, "CA_540"): ots_2018_CA_540.main,
    (2018, "NJ_1040"): ots_2018_NJ_1040.main,
    (2018, "PA_40"): ots_2018_PA_40.main,
    (2018, "US_1040"): ots_2018_US_1040.main,
    (2018, "NY_IT201"): ots_2018_NY_IT201.main,
    (2018, "NC_D400"): ots_2018_NC_D400.main,
    (2019, "MA_1"): ots_2019_MA_1.main,
    (2019, "NY_IT201"): ots_2019_NY_IT201.main,
    (2019, "US_1040_Sched_C"): ots_2019_US_1040_Sched_C.main,
    (2019, "VA_760"): ots_2019_VA_760.main,
    (2019, "OH_IT1040"): ots_2019_OH_IT1040.main,
    (2019, "PA_40"): ots_2019_PA_40.main,
    (2019, "US_1040"): ots_2019_US_1040.main,
    (2019, "NJ_1040"): ots_2019_NJ_1040.main,
    (2019, "NC_D400"): ots_2019_NC_D400.main,
    (2019, "CA_540"): ots_2019_CA_540.main,
    (2020, "OH_IT1040"): ots_2020_OH_IT1040.main,
    (2020, "NC_D400"): ots_2020_NC_D400.main,
    (2020, "US_1040"): ots_2020_US_1040.main,
    (2020, "NY_IT201"): ots_2020_NY_IT201.main,
    (2020, "HSA_f8889"): ots_2020_HSA_f8889.main,
    (2020, "MA_1"): ots_2020_MA_1.main,
    (2020, "VA_760"): ots_2020_VA_760.main,
    (2020, "f8606"): ots_2020_f8606.main,
    (2020, "NJ_1040"): ots_2020_NJ_1040.main,
    (2020, "US_1040_Sched_C"): ots_2020_US_1040_Sched_C.main,
    (2020, "PA_40"): ots_2020_PA_40.main,
    (2020, "CA_540"): ots_2020_CA_540.main,
    (2021, "NJ_1040"): ots_2021_NJ_1040.main,
    (2021, "US_1040_Sched_SE"): ots_2021_US_1040_Sched_SE.main,
    (2021, "f8606"): ots_2021_f8606.main,
    (2021, "CA_540"): ots_2021_CA_540.main,
    (2021, "HSA_f8889"): ots_2021_HSA_f8889.main,
    (2021, "MA_1"): ots_2021_MA_1.main,
    (2021, "NC_D400"): ots_2021_NC_D400.main,
    (2021, "NY_IT201"): ots_2021_NY_IT201.main,
    (2021, "OH_IT1040"): ots_2021_OH_IT1040.main,
    (2021, "PA_40"): ots_2021_PA_40.main,
    (2021, "US_1040"): ots_2021_US_1040.main,
    (2021, "US_1040_Sched_C"): ots_2021_US_1040_Sched_C.main,
    (2021, "VA_760"): ots_2021_VA_760.main,
    (2021, "CA_5805"): ots_2021_CA_5805.main,
    (2021, "f2210"): ots_2021_f2210.main,
    (2021, "f8960"): ots_2021_f8960.main,
    (2021, "f8959"): ots_2021_f8959.main,
    (2022, "HSA_f8889"): ots_2022_HSA_f8889.main,
    (2022, "MA_1"): ots_2022_MA_1.main,
    (2022, "NJ_1040"): ots_2022_NJ_1040.main,
    (2022, "f8959"): ots_2022_f8959.main,
    (2022, "f8960"): ots_2022_f8960.main,
    (2022, "f8606"): ots_2022_f8606.main,
    (2022, "CA_540"): ots_2022_CA_540.main,
    (2022, "CA_5805"): ots_2022_CA_5805.main,
    (2022, "NC_D400"): ots_2022_NC_D400.main,
    (2022, "NY_IT201"): ots_2022_NY_IT201.main,
    (2022, "OH_IT1040"): ots_2022_OH_IT1040.main,
    (2022, "PA_40"): ots_2022_PA_40.main,
    (2022, "US_1040"): ots_2022_US_1040.main,
    (2022, "US_1040_Sched_C"): ots_2022_US_1040_Sched_C.main,
    (2022, "US_1040_Sched_SE"): ots_2022_US_1040_Sched_SE.main,
    (2022, "VA_760"): ots_2022_VA_760.main,
    (2022, "f2210"): ots_2022_f2210.main,
    (2022, "f8829"): ots_2022_f8829.main,
    (2022, "f8995"): ots_2022_f8995.main,
    (2023, "NJ_1040"): ots_2023_NJ_1040.main,
    (2023, "US_1040_Sched_C"): ots_2023_US_1040_Sched_C.main,
    (2023, "f8829"): ots_2023_f8829.main,
    (2023, "f8959"): ots_2023_f8959.main,
    (2023, "f8960"): ots_2023_f8960.main,
    (2023, "f8995"): ots_2023_f8995.main,
    (2023, "f8606"): ots_2023_f8606.main,
    (2023, "CA_540"): ots_2023_CA_540.main,
    (2023, "HSA_f8889"): ots_2023_HSA_f8889.main,
    (2023, "MA_1"): ots_2023_MA_1.main,
    (2023, "NC_D400"): ots_2023_NC_D400.main,
    (2023, "NY_IT201"): ots_2023_NY_IT201.main,
    (2023, "OH_IT1040"): ots_2023_OH_IT1040.main,
    (2023, "PA_40"): ots_2023_PA_40.main,
    (2023, "US_1040"): ots_2023_US_1040.main,
    (2023, "US_1040_Sched_SE"): ots_2023_US_1040_Sched_SE.main,
    (2023, "VA_760"): ots_2023_VA_760.main,
    (2023, "f2210"): ots_2023_f2210.main,
    (2023, "US_1040_Sched_E_brokerage_royalties"): ots_2023_US_1040_Sched_E_brokerage_royalties.main,
    (2023, "CA_5805"): ots_2023_CA_5805.main,
    (2023, "f8812"): ots_2023_f8812.main,
    (2024, "NJ_1040"): ots_2024_NJ_1040.main,
    (2024, "US_1040_Sched_E_brokerage_royalties"): ots_2024_US_1040_Sched_E_brokerage_royalties.main,
    (2024, "f8812"): ots_2024_f8812.main,
    (2024, "f8829"): ots_2024_f8829.main,
    (2024, "f8959"): ots_2024_f8959.main,
    (2024, "f8960"): ots_2024_f8960.main,
    (2024, "f8995"): ots_2024_f8995.main,
    (2024, "US_1040_Sched_SE"): ots_2024_US_1040_Sched_SE.main,
    (2024, "f8606"): ots_2024_f8606.main,
    (2024, "CA_540"): ots_2024_CA_540.main,
    (2024, "CA_5805"): ots_2024_CA_5805.main,
    (2024, "HSA_f8889"): ots_2024_HSA_f8889.main,
    (2024, "MA_1"): ots_2024_MA_1.main,
    (2024, "NC_D400"): ots_2024_NC_D400.main,
    (2024, "NY_IT201"): ots_2024_NY_IT201.main,
    (2024, "OH_IT1040"): ots_2024_OH_IT1040.main,
    (2024, "PA_40"): ots_2024_PA_40.main,
    (2024, "US_1040"): ots_2024_US_1040.main,
    (2024, "US_1040_Sched_C"): ots_2024_US_1040_Sched_C.main,
    (2024, "VA_760"): ots_2024_VA_760.main,
    (2024, "f2210"): ots_2024_f2210.main,
    (2024, "MI_1040"): ots_2024_MI_1040.main,
    (2024, "OR_40"): ots_2024_OR_40.main,
}

cdef f_type lookup_ots_call(int year, str form):
    cdef tuple key = (year, form)
    if key in _OTS_LOOKUP:
        return _OTS_LOOKUP[key]
    return NULL


def _evaluate_form(year, form, form_text, fed_form_text=None, on_error="raise"):
    """Evaluate an OTS tax form given year, form, and form content.

    year: int
    form: str
    form_text: str
    fed_file: str | None
    on_error: str - "raise", "warn", or "ignore"

    Returns completed form as str.

    Validation of year, form, form_text is not done here, but in
    `tenforty.evaluate_form`, because it's more conveniently outside the cython
    context.
    """
    import warnings

    cdef f_type ots_form_function = lookup_ots_call(year, form)
    if ots_form_function is NULL:
        raise ValueError(f"Unknown year/form combination: {year}/{form}")

    cdef bytes program_name = b"ots"
    cdef bytes file_path_bytes
    cdef char** c_argv = NULL
    cdef int result_code

    with tempfile.TemporaryDirectory() as tmpdir:

        # Run federal tax file first as needed.
        if fed_form_text is not None:
            # Write out federal file to hardcoded location.
            ff_path = f"{tmpdir}/fed-form.txt"
            with open(ff_path, "w") as fp:
                print(fed_form_text, file=fp)

            # Update location of fed file in state form.
            form_text= form_text.replace("__FED_FILENAME__", ff_path)

        returnfile = f"{tmpdir}/form.txt"
        with open(returnfile, "w") as fp:
            print(form_text, file=fp)

        file_path_bytes = returnfile.encode('utf-8')
        c_argv = <char**>malloc(sizeof(char*) * 3)  # +1 for NULL terminator
        if c_argv is NULL:
            raise MemoryError()
        try:
            c_argv[0] = program_name
            c_argv[1] = file_path_bytes
            c_argv[2] = NULL  # NULL-terminate argv
            result_code = ots_form_function(2, c_argv)
            if result_code != 0:
                error_msg = f"OTS returned non-zero exit code: {result_code}"
                if on_error == "raise":
                    from tenforty.models import OTSError
                    raise OTSError(result_code, year, form, error_msg)
                elif on_error == "warn":
                    warnings.warn(error_msg, RuntimeWarning)
        finally:
            free(c_argv)

        # OTS generates an output file based on the name of the input file.
        returnfile_completed = returnfile.replace(".txt", "_out.txt")
        with open(returnfile_completed) as fp:
            result = fp.read()

    return result
