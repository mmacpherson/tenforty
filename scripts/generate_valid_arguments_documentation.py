import os
import sys
from collections import OrderedDict
import tenforty


"""Generate HTML that displays conformant arguments for specifying tax
return, from the source metadata."""


def doc_table_from_spec(specs):
    return [OrderedDict(key=e.key, default=e.default, aliases=e.aliases) for e in specs]


def unroll(e):
    if e is None or isinstance(e, str) or isinstance(e, int):
        e = [e]

    return ",".join(map(str, e))


def table_to_html(frame):
    def fixed_width_font(s):
        return f"<code>{s}</code>"

    def process(s):
        return ", ".join(fixed_width_font(e) for e in unroll(s))

    def makerow(data, tag="td", mono=True):
        return "<tr>{}</tr>".format(
            "".join(f"<{tag}>{process(e)}</{tag}>" for e in data)
        )

    header = makerow(frame[0].keys(), tag="th", mono=False)
    body = "\n".join(makerow(row.values()) for row in frame)

    return f"""\
<table>
  <thead>
     {header}
  </thead>
  <tbody>
    {body}
  </tbody>
</table>
    """


def table_to_markdown(frame):

    header = "|".join(frame[0].keys())
    sep = "|".join("---" for _ in frame[0].keys())
    rows = ["|".join(map(unroll, row.values())) for row in frame]

    return "\n".join(f"|{e}|" for e in [header, sep] + rows)


def main(args):

    specs = (
        tenforty.data.US_1040_2019["input_wrap"],
        tenforty.data.US_1040_2018["input_wrap"],
        tenforty.data.US_1040_2017["input_wrap"],
    )

    for spec in specs:
        print(table_to_markdown(doc_table_from_spec(spec)))


if __name__ == "__main__":
    main(sys.argv[1:])
