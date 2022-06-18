#!/bin/python3
#
# This script adjusts the `trace` output from racket to be more compatible
# with vscode's automatic code folding.
#
# Emits spaces and adds in a `>` and a `<` at the start of each line.

import re
import sys

# When things get deeply nested, racket's tracing
# prints like `> > > >[10]`, where the `10` represents
# the total number of characteres of indent depth.
COUNT_RE = re.compile(r"^(c?[<> ]*)\[(\d+)\](.*)$")

# Otherwise it prints like `> > >Foo` when first
# entering Foo and `< < <Blah` when returning a result.
#
# Sometimes it also prints `> > > Bar` -- I *think* that
# is when `Bar` is a subtask of `Foo`.
INDENT_RE = re.compile(r"^(c?[<> ]*)(.*)$")


def main():
    header = '  '
    for line in sys.stdin.readlines():
        mo = COUNT_RE.match(line)
        if mo:
            indent = mo.group(1)
            count = int(mo.group(2))
            header = indent_header(header, indent)
            text = mo.group(3)
            print_line(count, header, text)
            continue

        mo = INDENT_RE.match(line)
        if not mo:
            raise "bad line: {}".format(line)
        if mo:
            indent = mo.group(1)
            header = indent_header(header, indent)
            text = mo.group(2)
            print_line(len(indent), header, text)
            continue


def print_line(count, header, text):
    print("{}{}{}".format(" " * count, header, text))


def indent_header(old_header, indent):
    if '>' in indent:
        return '>'
    elif '<' in indent:
        return '<'
    elif old_header == '>':
        # putting this character lets you fold the contents of a new
        # block without folding children
        return ' ⋮'
    else:
        return '  '


main()
