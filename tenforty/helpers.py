# -*- coding: utf-8 -*-
from . import data


def listify(vals):
    if isinstance(vals, str):
        return [vals]
    return list(vals)


def coalesce(dct, keys, default):

    for k in keys:
        if k in dct:
            return dct[k]

    return default


def generate_ots_return(form_values, schema):

    form_lines = []
    for field in schema:
        value = coalesce(
            form_values, [field.key] + listify(field.aliases), field.default
        )
        value = "" if (value is None) else value
        line = f"{field.key} {value}{field.terminator}"
        form_lines.append(line)

    return "\n".join(form_lines)


def parse_ots_return(text):
    out = {}
    for line in text.split("\n"):
        if "=" in line:
            identifier, rhs = line.split("=")[:2]
            identifier = identifier.strip()
            value = rhs.strip().split()[0]
            try:
                value = int(value)
            except ValueError:
                try:
                    value = float(value)
                except ValueError:
                    pass
            out[identifier] = value
        elif line.startswith(" You are"):
            (_bracket,) = [e.rstrip("%") for e in line.split() if e.endswith("%")]
            out["tax_bracket"] = float(_bracket)
        elif line.startswith(" and you are"):
            (_effective,) = [e.rstrip("%") for e in line.split() if e.endswith("%")]
            out["effective_tax_rate"] = float(_effective)

    out["total_income"] = out["L6"]
    out["adjusted_gross_income"] = out["L7"]
    out["agi"] = out["L7"]
    out["amt"] = out["Your Alternative Minimum Tax"]
    out["taxable_income"] = out["L10"]
    out["total_tax"] = out["L15"]
    out["total_payments"] = out["L18"]
    out["tax_owed"] = out["L22"]

    return out
