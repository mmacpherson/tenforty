# -*- coding: utf-8 -*-
from . import data, helpers, ots_2017, ots_2018, ots_2019, ots_2020


def us_1040(form_values, year="latest", parse_return=True):
    """Compute US federal tax return."""
    _dispatch = {
        "latest": (ots_2020.us_main, data.US_1040_2020),
        "2020": (ots_2020.us_main, data.US_1040_2020),
        "2019": (ots_2019.us_main, data.US_1040_2019),
        "2018": (ots_2018.us_main, data.US_1040_2018),
        "2017": (ots_2017.us_main, data.US_1040_2017),
    }

    main_fn, schema = _dispatch[str(year)]
    return_text = main_fn(
        helpers.generate_ots_return(form_values, schema["input_wrap"])
    )

    if parse_return:
        return helpers.parse_ots_return(
            return_text,
            schema["output_wrap"],
        )

    return return_text


def ca_540(fed_form_values, state_form_values, year="latest"):
    """Compute US federal tax return."""
    _dispatch = {
        "latest": (ots_2020.ca_main, data.CA_540_2020),
        "2020": (ots_2020.ca_main, data.CA_540_2020),
        # "2019": (ots_2019.ca_main, data.CA_540_2019),
    }
    main_fn, stateschema = _dispatch[str(year)]

    fed_out_text = us_1040(fed_form_values, year=year, parse_return=False)

    return helpers.parse_ots_return(
        main_fn(
            helpers.generate_ots_return(state_form_values, stateschema["input_wrap"]),
            fed_out_text,
        ),
        stateschema["output_wrap"],
    )


def tax_rate(adjusted_gross_income, status=1, year="latest"):
    return ots_2018.tax_rate(adjusted_gross_income, status)
