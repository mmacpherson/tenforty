# -*- coding: utf-8 -*-
from . import data, helpers, ots_2017, ots_2018, ots_2019, ots_2020


def us_1040(form_values, year="latest"):
    """Compute US federal tax return."""
    _dispatch = {
        "latest": (ots_2020.us_main, data.US_1040_2020),
        "2020": (ots_2020.us_main, data.US_1040_2020),
        "2019": (ots_2019.us_main, data.US_1040_2019),
        "2018": (ots_2018.us_main, data.US_1040_2018),
        "2017": (ots_2017.us_main, data.US_1040_2017),
    }
    main_fn, schema = _dispatch[str(year)]
    return helpers.parse_ots_return(
        main_fn(helpers.generate_ots_return(form_values, schema["input_wrap"])),
        schema["output_wrap"],
    )


def tax_rate(adjusted_gross_income, status=1, year="latest"):
    return ots_2018.tax_rate(adjusted_gross_income, status)
