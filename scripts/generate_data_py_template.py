import os
import sys
import re

"Run over an OTS template files to extract all the (line) names from the file."


def main(args):
    (templatefile,) = args

    text = open(templatefile).read()

    text = re.sub(re.compile(r"{[^{]*}"), "", text)

    for line in text.split("\n"):
        line = line.replace(";", "").strip()
        if not line:
            continue
        first_thing = line.split()[0]
        if ":" in first_thing:
            terminator = ', terminator="\\n"'
        else:
            terminator = ""
        print(f'Field(key="{first_thing}"{terminator}),')


if __name__ == "__main__":
    main(sys.argv[1:])
