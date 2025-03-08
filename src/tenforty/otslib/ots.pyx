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

cdef f_type lookup_ots_call(int year, char* form):
    if (year == 2018) and (form == "MA_1"):
        return ots_2018_MA_1.main
    if (year == 2018) and (form == "US_1040_Sched_C"):
        return ots_2018_US_1040_Sched_C.main
    if (year == 2018) and (form == "VA_760"):
        return ots_2018_VA_760.main
    if (year == 2018) and (form == "OH_IT1040"):
        return ots_2018_OH_IT1040.main
    if (year == 2018) and (form == "CA_540"):
        return ots_2018_CA_540.main
    if (year == 2018) and (form == "NJ_1040"):
        return ots_2018_NJ_1040.main
    if (year == 2018) and (form == "PA_40"):
        return ots_2018_PA_40.main
    if (year == 2018) and (form == "US_1040"):
        return ots_2018_US_1040.main
    if (year == 2018) and (form == "NY_IT201"):
        return ots_2018_NY_IT201.main
    if (year == 2018) and (form == "NC_D400"):
        return ots_2018_NC_D400.main
    if (year == 2019) and (form == "MA_1"):
        return ots_2019_MA_1.main
    if (year == 2019) and (form == "NY_IT201"):
        return ots_2019_NY_IT201.main
    if (year == 2019) and (form == "US_1040_Sched_C"):
        return ots_2019_US_1040_Sched_C.main
    if (year == 2019) and (form == "VA_760"):
        return ots_2019_VA_760.main
    if (year == 2019) and (form == "OH_IT1040"):
        return ots_2019_OH_IT1040.main
    if (year == 2019) and (form == "PA_40"):
        return ots_2019_PA_40.main
    if (year == 2019) and (form == "US_1040"):
        return ots_2019_US_1040.main
    if (year == 2019) and (form == "NJ_1040"):
        return ots_2019_NJ_1040.main
    if (year == 2019) and (form == "NC_D400"):
        return ots_2019_NC_D400.main
    if (year == 2019) and (form == "CA_540"):
        return ots_2019_CA_540.main
    if (year == 2020) and (form == "OH_IT1040"):
        return ots_2020_OH_IT1040.main
    if (year == 2020) and (form == "NC_D400"):
        return ots_2020_NC_D400.main
    if (year == 2020) and (form == "US_1040"):
        return ots_2020_US_1040.main
    if (year == 2020) and (form == "NY_IT201"):
        return ots_2020_NY_IT201.main
    if (year == 2020) and (form == "HSA_f8889"):
        return ots_2020_HSA_f8889.main
    if (year == 2020) and (form == "MA_1"):
        return ots_2020_MA_1.main
    if (year == 2020) and (form == "VA_760"):
        return ots_2020_VA_760.main
    if (year == 2020) and (form == "f8606"):
        return ots_2020_f8606.main
    if (year == 2020) and (form == "NJ_1040"):
        return ots_2020_NJ_1040.main
    if (year == 2020) and (form == "US_1040_Sched_C"):
        return ots_2020_US_1040_Sched_C.main
    if (year == 2020) and (form == "PA_40"):
        return ots_2020_PA_40.main
    if (year == 2020) and (form == "CA_540"):
        return ots_2020_CA_540.main
    if (year == 2021) and (form == "NJ_1040"):
        return ots_2021_NJ_1040.main
    if (year == 2021) and (form == "US_1040_Sched_SE"):
        return ots_2021_US_1040_Sched_SE.main
    if (year == 2021) and (form == "f8606"):
        return ots_2021_f8606.main
    if (year == 2021) and (form == "CA_540"):
        return ots_2021_CA_540.main
    if (year == 2021) and (form == "HSA_f8889"):
        return ots_2021_HSA_f8889.main
    if (year == 2021) and (form == "MA_1"):
        return ots_2021_MA_1.main
    if (year == 2021) and (form == "NC_D400"):
        return ots_2021_NC_D400.main
    if (year == 2021) and (form == "NY_IT201"):
        return ots_2021_NY_IT201.main
    if (year == 2021) and (form == "OH_IT1040"):
        return ots_2021_OH_IT1040.main
    if (year == 2021) and (form == "PA_40"):
        return ots_2021_PA_40.main
    if (year == 2021) and (form == "US_1040"):
        return ots_2021_US_1040.main
    if (year == 2021) and (form == "US_1040_Sched_C"):
        return ots_2021_US_1040_Sched_C.main
    if (year == 2021) and (form == "VA_760"):
        return ots_2021_VA_760.main
    if (year == 2021) and (form == "CA_5805"):
        return ots_2021_CA_5805.main
    if (year == 2021) and (form == "f2210"):
        return ots_2021_f2210.main
    if (year == 2021) and (form == "f8960"):
        return ots_2021_f8960.main
    if (year == 2021) and (form == "f8959"):
        return ots_2021_f8959.main
    if (year == 2022) and (form == "HSA_f8889"):
        return ots_2022_HSA_f8889.main
    if (year == 2022) and (form == "MA_1"):
        return ots_2022_MA_1.main
    if (year == 2022) and (form == "NJ_1040"):
        return ots_2022_NJ_1040.main
    if (year == 2022) and (form == "f8959"):
        return ots_2022_f8959.main
    if (year == 2022) and (form == "f8960"):
        return ots_2022_f8960.main
    if (year == 2022) and (form == "f8606"):
        return ots_2022_f8606.main
    if (year == 2022) and (form == "CA_540"):
        return ots_2022_CA_540.main
    if (year == 2022) and (form == "CA_5805"):
        return ots_2022_CA_5805.main
    if (year == 2022) and (form == "NC_D400"):
        return ots_2022_NC_D400.main
    if (year == 2022) and (form == "NY_IT201"):
        return ots_2022_NY_IT201.main
    if (year == 2022) and (form == "OH_IT1040"):
        return ots_2022_OH_IT1040.main
    if (year == 2022) and (form == "PA_40"):
        return ots_2022_PA_40.main
    if (year == 2022) and (form == "US_1040"):
        return ots_2022_US_1040.main
    if (year == 2022) and (form == "US_1040_Sched_C"):
        return ots_2022_US_1040_Sched_C.main
    if (year == 2022) and (form == "US_1040_Sched_SE"):
        return ots_2022_US_1040_Sched_SE.main
    if (year == 2022) and (form == "VA_760"):
        return ots_2022_VA_760.main
    if (year == 2022) and (form == "f2210"):
        return ots_2022_f2210.main
    if (year == 2022) and (form == "f8829"):
        return ots_2022_f8829.main
    if (year == 2022) and (form == "f8995"):
        return ots_2022_f8995.main
    if (year == 2023) and (form == "NJ_1040"):
        return ots_2023_NJ_1040.main
    if (year == 2023) and (form == "US_1040_Sched_C"):
        return ots_2023_US_1040_Sched_C.main
    if (year == 2023) and (form == "f8829"):
        return ots_2023_f8829.main
    if (year == 2023) and (form == "f8959"):
        return ots_2023_f8959.main
    if (year == 2023) and (form == "f8960"):
        return ots_2023_f8960.main
    if (year == 2023) and (form == "f8995"):
        return ots_2023_f8995.main
    if (year == 2023) and (form == "f8606"):
        return ots_2023_f8606.main
    if (year == 2023) and (form == "CA_540"):
        return ots_2023_CA_540.main
    if (year == 2023) and (form == "HSA_f8889"):
        return ots_2023_HSA_f8889.main
    if (year == 2023) and (form == "MA_1"):
        return ots_2023_MA_1.main
    if (year == 2023) and (form == "NC_D400"):
        return ots_2023_NC_D400.main
    if (year == 2023) and (form == "NY_IT201"):
        return ots_2023_NY_IT201.main
    if (year == 2023) and (form == "OH_IT1040"):
        return ots_2023_OH_IT1040.main
    if (year == 2023) and (form == "PA_40"):
        return ots_2023_PA_40.main
    if (year == 2023) and (form == "US_1040"):
        return ots_2023_US_1040.main
    if (year == 2023) and (form == "US_1040_Sched_SE"):
        return ots_2023_US_1040_Sched_SE.main
    if (year == 2023) and (form == "VA_760"):
        return ots_2023_VA_760.main
    if (year == 2023) and (form == "f2210"):
        return ots_2023_f2210.main
    if (year == 2023) and (form == "US_1040_Sched_E_brokerage_royalties"):
        return ots_2023_US_1040_Sched_E_brokerage_royalties.main
    if (year == 2023) and (form == "CA_5805"):
        return ots_2023_CA_5805.main
    if (year == 2023) and (form == "f8812"):
        return ots_2023_f8812.main
    if (year == 2024) and (form == "NJ_1040"):
        return ots_2024_NJ_1040.main
    if (year == 2024) and (form == "US_1040_Sched_E_brokerage_royalties"):
        return ots_2024_US_1040_Sched_E_brokerage_royalties.main
    if (year == 2024) and (form == "f8812"):
        return ots_2024_f8812.main
    if (year == 2024) and (form == "f8829"):
        return ots_2024_f8829.main
    if (year == 2024) and (form == "f8959"):
        return ots_2024_f8959.main
    if (year == 2024) and (form == "f8960"):
        return ots_2024_f8960.main
    if (year == 2024) and (form == "f8995"):
        return ots_2024_f8995.main
    if (year == 2024) and (form == "US_1040_Sched_SE"):
        return ots_2024_US_1040_Sched_SE.main
    if (year == 2024) and (form == "f8606"):
        return ots_2024_f8606.main
    if (year == 2024) and (form == "CA_540"):
        return ots_2024_CA_540.main
    if (year == 2024) and (form == "CA_5805"):
        return ots_2024_CA_5805.main
    if (year == 2024) and (form == "HSA_f8889"):
        return ots_2024_HSA_f8889.main
    if (year == 2024) and (form == "MA_1"):
        return ots_2024_MA_1.main
    if (year == 2024) and (form == "NC_D400"):
        return ots_2024_NC_D400.main
    if (year == 2024) and (form == "NY_IT201"):
        return ots_2024_NY_IT201.main
    if (year == 2024) and (form == "OH_IT1040"):
        return ots_2024_OH_IT1040.main
    if (year == 2024) and (form == "PA_40"):
        return ots_2024_PA_40.main
    if (year == 2024) and (form == "US_1040"):
        return ots_2024_US_1040.main
    if (year == 2024) and (form == "US_1040_Sched_C"):
        return ots_2024_US_1040_Sched_C.main
    if (year == 2024) and (form == "VA_760"):
        return ots_2024_VA_760.main
    if (year == 2024) and (form == "f2210"):
        return ots_2024_f2210.main
    if (year == 2024) and (form == "MI_1040"):
        return ots_2024_MI_1040.main


def _evaluate_form(year, form, form_text, fed_form_text=None):
    """Evaluate an OTS tax form given year, form, and form content.

    year: int
    form: str
    form_text: str
    fed_file: str | None

    Returns completed form as str.

    Validation of year, form, form_text is not done here, but in
    `tenforty.evaluate_form`, because it's more conveniently outside the cython
    context.
    """

    cdef f_type ots_form_function = lookup_ots_call(year, form)

    cdef char** c_argv = NULL
    cdef object py_unicode_returnfile

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

        c_argv = <char**>malloc(sizeof(char*) * 2)
        if c_argv is NULL:
            raise MemoryError()
        try:
            py_unicode_returnfile = returnfile
            c_argv[1] = py_unicode_returnfile
            ots_form_function(2, c_argv)
        finally:
            free(c_argv)

        # OTS generates an output file based on the name of the input file.
        returnfile_completed = returnfile.replace(".txt", "_out.txt")
        with open(returnfile_completed) as fp:
            result = fp.read()

    return result
