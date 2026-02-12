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
cimport ots_2023_AZ_140
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
cimport ots_2024_AZ_140
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
cimport ots_2025_OR_40
cimport ots_2025_MA_1
cimport ots_2025_VA_760
cimport ots_2025_f8812
cimport ots_2025_US_1040_Sched_SE
cimport ots_2025_f8829
cimport ots_2025_CA_5805
cimport ots_2025_f8959
cimport ots_2025_f8960
cimport ots_2025_MI_1040
cimport ots_2025_US_1040
cimport ots_2025_NC_D400
cimport ots_2025_AZ_140
cimport ots_2025_NY_IT201
cimport ots_2025_f8606
cimport ots_2025_HSA_f8889
cimport ots_2025_US_1040_Sched_E_brokerage_royalties
cimport ots_2025_f8995
cimport ots_2025_CA_540
cimport ots_2025_NJ_1040
cimport ots_2025_OH_IT1040
cimport ots_2025_f2210
cimport ots_2025_PA_40
cimport ots_2025_US_1040_Sched_C

_OTS_KEY_TO_INDEX = {
    (2018, "MA_1"): 0,
    (2018, "US_1040_Sched_C"): 1,
    (2018, "VA_760"): 2,
    (2018, "OH_IT1040"): 3,
    (2018, "CA_540"): 4,
    (2018, "NJ_1040"): 5,
    (2018, "PA_40"): 6,
    (2018, "US_1040"): 7,
    (2018, "NY_IT201"): 8,
    (2018, "NC_D400"): 9,
    (2019, "MA_1"): 10,
    (2019, "NY_IT201"): 11,
    (2019, "US_1040_Sched_C"): 12,
    (2019, "VA_760"): 13,
    (2019, "OH_IT1040"): 14,
    (2019, "PA_40"): 15,
    (2019, "US_1040"): 16,
    (2019, "NJ_1040"): 17,
    (2019, "NC_D400"): 18,
    (2019, "CA_540"): 19,
    (2020, "OH_IT1040"): 20,
    (2020, "NC_D400"): 21,
    (2020, "US_1040"): 22,
    (2020, "NY_IT201"): 23,
    (2020, "HSA_f8889"): 24,
    (2020, "MA_1"): 25,
    (2020, "VA_760"): 26,
    (2020, "f8606"): 27,
    (2020, "NJ_1040"): 28,
    (2020, "US_1040_Sched_C"): 29,
    (2020, "PA_40"): 30,
    (2020, "CA_540"): 31,
    (2021, "NJ_1040"): 32,
    (2021, "US_1040_Sched_SE"): 33,
    (2021, "f8606"): 34,
    (2021, "CA_540"): 35,
    (2021, "HSA_f8889"): 36,
    (2021, "MA_1"): 37,
    (2021, "NC_D400"): 38,
    (2021, "NY_IT201"): 39,
    (2021, "OH_IT1040"): 40,
    (2021, "PA_40"): 41,
    (2021, "US_1040"): 42,
    (2021, "US_1040_Sched_C"): 43,
    (2021, "VA_760"): 44,
    (2021, "CA_5805"): 45,
    (2021, "f2210"): 46,
    (2021, "f8960"): 47,
    (2021, "f8959"): 48,
    (2022, "HSA_f8889"): 49,
    (2022, "MA_1"): 50,
    (2022, "NJ_1040"): 51,
    (2022, "f8959"): 52,
    (2022, "f8960"): 53,
    (2022, "f8606"): 54,
    (2022, "CA_540"): 55,
    (2022, "CA_5805"): 56,
    (2022, "NC_D400"): 57,
    (2022, "NY_IT201"): 58,
    (2022, "OH_IT1040"): 59,
    (2022, "PA_40"): 60,
    (2022, "US_1040"): 61,
    (2022, "US_1040_Sched_C"): 62,
    (2022, "US_1040_Sched_SE"): 63,
    (2022, "VA_760"): 64,
    (2022, "f2210"): 65,
    (2022, "f8829"): 66,
    (2022, "f8995"): 67,
    (2023, "NJ_1040"): 68,
    (2023, "US_1040_Sched_C"): 69,
    (2023, "f8829"): 70,
    (2023, "f8959"): 71,
    (2023, "f8960"): 72,
    (2023, "f8995"): 73,
    (2023, "f8606"): 74,
    (2023, "CA_540"): 75,
    (2023, "HSA_f8889"): 76,
    (2023, "MA_1"): 77,
    (2023, "NC_D400"): 78,
    (2023, "NY_IT201"): 79,
    (2023, "OH_IT1040"): 80,
    (2023, "PA_40"): 81,
    (2023, "US_1040"): 82,
    (2023, "US_1040_Sched_SE"): 83,
    (2023, "VA_760"): 84,
    (2023, "f2210"): 85,
    (2023, "AZ_140"): 86,
    (2023, "US_1040_Sched_E_brokerage_royalties"): 87,
    (2023, "CA_5805"): 88,
    (2023, "f8812"): 89,
    (2024, "NJ_1040"): 90,
    (2024, "US_1040_Sched_E_brokerage_royalties"): 91,
    (2024, "f8812"): 92,
    (2024, "f8829"): 93,
    (2024, "f8959"): 94,
    (2024, "f8960"): 95,
    (2024, "f8995"): 96,
    (2024, "US_1040_Sched_SE"): 97,
    (2024, "f8606"): 98,
    (2024, "AZ_140"): 99,
    (2024, "CA_540"): 100,
    (2024, "CA_5805"): 101,
    (2024, "HSA_f8889"): 102,
    (2024, "MA_1"): 103,
    (2024, "NC_D400"): 104,
    (2024, "NY_IT201"): 105,
    (2024, "OH_IT1040"): 106,
    (2024, "PA_40"): 107,
    (2024, "US_1040"): 108,
    (2024, "US_1040_Sched_C"): 109,
    (2024, "VA_760"): 110,
    (2024, "f2210"): 111,
    (2024, "MI_1040"): 112,
    (2024, "OR_40"): 113,
    (2025, "OR_40"): 114,
    (2025, "MA_1"): 115,
    (2025, "VA_760"): 116,
    (2025, "f8812"): 117,
    (2025, "US_1040_Sched_SE"): 118,
    (2025, "f8829"): 119,
    (2025, "CA_5805"): 120,
    (2025, "f8959"): 121,
    (2025, "f8960"): 122,
    (2025, "MI_1040"): 123,
    (2025, "US_1040"): 124,
    (2025, "NC_D400"): 125,
    (2025, "AZ_140"): 126,
    (2025, "NY_IT201"): 127,
    (2025, "f8606"): 128,
    (2025, "HSA_f8889"): 129,
    (2025, "US_1040_Sched_E_brokerage_royalties"): 130,
    (2025, "f8995"): 131,
    (2025, "CA_540"): 132,
    (2025, "NJ_1040"): 133,
    (2025, "OH_IT1040"): 134,
    (2025, "f2210"): 135,
    (2025, "PA_40"): 136,
    (2025, "US_1040_Sched_C"): 137,
}

cdef f_type _ots_get_function(int index):
    if index == 0:
        return ots_2018_MA_1.main
    elif index == 1:
        return ots_2018_US_1040_Sched_C.main
    elif index == 2:
        return ots_2018_VA_760.main
    elif index == 3:
        return ots_2018_OH_IT1040.main
    elif index == 4:
        return ots_2018_CA_540.main
    elif index == 5:
        return ots_2018_NJ_1040.main
    elif index == 6:
        return ots_2018_PA_40.main
    elif index == 7:
        return ots_2018_US_1040.main
    elif index == 8:
        return ots_2018_NY_IT201.main
    elif index == 9:
        return ots_2018_NC_D400.main
    elif index == 10:
        return ots_2019_MA_1.main
    elif index == 11:
        return ots_2019_NY_IT201.main
    elif index == 12:
        return ots_2019_US_1040_Sched_C.main
    elif index == 13:
        return ots_2019_VA_760.main
    elif index == 14:
        return ots_2019_OH_IT1040.main
    elif index == 15:
        return ots_2019_PA_40.main
    elif index == 16:
        return ots_2019_US_1040.main
    elif index == 17:
        return ots_2019_NJ_1040.main
    elif index == 18:
        return ots_2019_NC_D400.main
    elif index == 19:
        return ots_2019_CA_540.main
    elif index == 20:
        return ots_2020_OH_IT1040.main
    elif index == 21:
        return ots_2020_NC_D400.main
    elif index == 22:
        return ots_2020_US_1040.main
    elif index == 23:
        return ots_2020_NY_IT201.main
    elif index == 24:
        return ots_2020_HSA_f8889.main
    elif index == 25:
        return ots_2020_MA_1.main
    elif index == 26:
        return ots_2020_VA_760.main
    elif index == 27:
        return ots_2020_f8606.main
    elif index == 28:
        return ots_2020_NJ_1040.main
    elif index == 29:
        return ots_2020_US_1040_Sched_C.main
    elif index == 30:
        return ots_2020_PA_40.main
    elif index == 31:
        return ots_2020_CA_540.main
    elif index == 32:
        return ots_2021_NJ_1040.main
    elif index == 33:
        return ots_2021_US_1040_Sched_SE.main
    elif index == 34:
        return ots_2021_f8606.main
    elif index == 35:
        return ots_2021_CA_540.main
    elif index == 36:
        return ots_2021_HSA_f8889.main
    elif index == 37:
        return ots_2021_MA_1.main
    elif index == 38:
        return ots_2021_NC_D400.main
    elif index == 39:
        return ots_2021_NY_IT201.main
    elif index == 40:
        return ots_2021_OH_IT1040.main
    elif index == 41:
        return ots_2021_PA_40.main
    elif index == 42:
        return ots_2021_US_1040.main
    elif index == 43:
        return ots_2021_US_1040_Sched_C.main
    elif index == 44:
        return ots_2021_VA_760.main
    elif index == 45:
        return ots_2021_CA_5805.main
    elif index == 46:
        return ots_2021_f2210.main
    elif index == 47:
        return ots_2021_f8960.main
    elif index == 48:
        return ots_2021_f8959.main
    elif index == 49:
        return ots_2022_HSA_f8889.main
    elif index == 50:
        return ots_2022_MA_1.main
    elif index == 51:
        return ots_2022_NJ_1040.main
    elif index == 52:
        return ots_2022_f8959.main
    elif index == 53:
        return ots_2022_f8960.main
    elif index == 54:
        return ots_2022_f8606.main
    elif index == 55:
        return ots_2022_CA_540.main
    elif index == 56:
        return ots_2022_CA_5805.main
    elif index == 57:
        return ots_2022_NC_D400.main
    elif index == 58:
        return ots_2022_NY_IT201.main
    elif index == 59:
        return ots_2022_OH_IT1040.main
    elif index == 60:
        return ots_2022_PA_40.main
    elif index == 61:
        return ots_2022_US_1040.main
    elif index == 62:
        return ots_2022_US_1040_Sched_C.main
    elif index == 63:
        return ots_2022_US_1040_Sched_SE.main
    elif index == 64:
        return ots_2022_VA_760.main
    elif index == 65:
        return ots_2022_f2210.main
    elif index == 66:
        return ots_2022_f8829.main
    elif index == 67:
        return ots_2022_f8995.main
    elif index == 68:
        return ots_2023_NJ_1040.main
    elif index == 69:
        return ots_2023_US_1040_Sched_C.main
    elif index == 70:
        return ots_2023_f8829.main
    elif index == 71:
        return ots_2023_f8959.main
    elif index == 72:
        return ots_2023_f8960.main
    elif index == 73:
        return ots_2023_f8995.main
    elif index == 74:
        return ots_2023_f8606.main
    elif index == 75:
        return ots_2023_CA_540.main
    elif index == 76:
        return ots_2023_HSA_f8889.main
    elif index == 77:
        return ots_2023_MA_1.main
    elif index == 78:
        return ots_2023_NC_D400.main
    elif index == 79:
        return ots_2023_NY_IT201.main
    elif index == 80:
        return ots_2023_OH_IT1040.main
    elif index == 81:
        return ots_2023_PA_40.main
    elif index == 82:
        return ots_2023_US_1040.main
    elif index == 83:
        return ots_2023_US_1040_Sched_SE.main
    elif index == 84:
        return ots_2023_VA_760.main
    elif index == 85:
        return ots_2023_f2210.main
    elif index == 86:
        return ots_2023_AZ_140.main
    elif index == 87:
        return ots_2023_US_1040_Sched_E_brokerage_royalties.main
    elif index == 88:
        return ots_2023_CA_5805.main
    elif index == 89:
        return ots_2023_f8812.main
    elif index == 90:
        return ots_2024_NJ_1040.main
    elif index == 91:
        return ots_2024_US_1040_Sched_E_brokerage_royalties.main
    elif index == 92:
        return ots_2024_f8812.main
    elif index == 93:
        return ots_2024_f8829.main
    elif index == 94:
        return ots_2024_f8959.main
    elif index == 95:
        return ots_2024_f8960.main
    elif index == 96:
        return ots_2024_f8995.main
    elif index == 97:
        return ots_2024_US_1040_Sched_SE.main
    elif index == 98:
        return ots_2024_f8606.main
    elif index == 99:
        return ots_2024_AZ_140.main
    elif index == 100:
        return ots_2024_CA_540.main
    elif index == 101:
        return ots_2024_CA_5805.main
    elif index == 102:
        return ots_2024_HSA_f8889.main
    elif index == 103:
        return ots_2024_MA_1.main
    elif index == 104:
        return ots_2024_NC_D400.main
    elif index == 105:
        return ots_2024_NY_IT201.main
    elif index == 106:
        return ots_2024_OH_IT1040.main
    elif index == 107:
        return ots_2024_PA_40.main
    elif index == 108:
        return ots_2024_US_1040.main
    elif index == 109:
        return ots_2024_US_1040_Sched_C.main
    elif index == 110:
        return ots_2024_VA_760.main
    elif index == 111:
        return ots_2024_f2210.main
    elif index == 112:
        return ots_2024_MI_1040.main
    elif index == 113:
        return ots_2024_OR_40.main
    elif index == 114:
        return ots_2025_OR_40.main
    elif index == 115:
        return ots_2025_MA_1.main
    elif index == 116:
        return ots_2025_VA_760.main
    elif index == 117:
        return ots_2025_f8812.main
    elif index == 118:
        return ots_2025_US_1040_Sched_SE.main
    elif index == 119:
        return ots_2025_f8829.main
    elif index == 120:
        return ots_2025_CA_5805.main
    elif index == 121:
        return ots_2025_f8959.main
    elif index == 122:
        return ots_2025_f8960.main
    elif index == 123:
        return ots_2025_MI_1040.main
    elif index == 124:
        return ots_2025_US_1040.main
    elif index == 125:
        return ots_2025_NC_D400.main
    elif index == 126:
        return ots_2025_AZ_140.main
    elif index == 127:
        return ots_2025_NY_IT201.main
    elif index == 128:
        return ots_2025_f8606.main
    elif index == 129:
        return ots_2025_HSA_f8889.main
    elif index == 130:
        return ots_2025_US_1040_Sched_E_brokerage_royalties.main
    elif index == 131:
        return ots_2025_f8995.main
    elif index == 132:
        return ots_2025_CA_540.main
    elif index == 133:
        return ots_2025_NJ_1040.main
    elif index == 134:
        return ots_2025_OH_IT1040.main
    elif index == 135:
        return ots_2025_f2210.main
    elif index == 136:
        return ots_2025_PA_40.main
    elif index == 137:
        return ots_2025_US_1040_Sched_C.main
    return NULL

cdef f_type lookup_ots_call(int year, str form):
    cdef tuple key = (year, form)
    if key in _OTS_KEY_TO_INDEX:
        return _ots_get_function(_OTS_KEY_TO_INDEX[key])
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
