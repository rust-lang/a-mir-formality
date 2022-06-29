#!/bin/python3
# coding=utf-8
#
# This script adjusts the `trace` output from racket to be more compatible
# with vscode's automatic code folding.
#
# Emits spaces and adds in a `>` and a `<` at the start of each line.

import re
import sys
import fileinput

# When things get deeply nested, racket's tracing
# prints like `> > > >[10]`, where the `10` represents
# the total number of characteres of indent depth.
COUNT_RE = re.compile(r"^((c?[<> ]*)\[(\d+)\]) (.*)$")

# Otherwise it prints like `> > >Foo` when first
# entering Foo and `< < <Blah` when returning a result.
#
# Sometimes it also prints `> > > Bar` -- I *think* that
# is when `Bar` is a subtask of `Foo`.
INDENT_RE = re.compile(r"^(c?[<> ]*)(.*)$")


class Section:
    def __init__(self, input_indent, input_ident_count, output_indent):
        # number of lines of indent in the actual input text
        self.input_indent_count = input_ident_count

        # input header
        if '>' in input_indent:
            self.header = '>'
        else:
            self.header = '<'

        # number of spaces we indented by in the output
        self.output_indent_indent = output_indent
        self.lines = 0
        pass

    def print_header(self, text):
        print_line(self.output_indent_indent, self.header, text)

    def print_body(self, input_indent_count, text):
        output_indent = input_indent_count - \
            self.input_indent_count + self.output_indent_indent
        if self.lines == 0:
            # putting this character lets you fold the contents of a new
            # block without folding children
            header = ' ⋮'
            self.lines += 1
        else:
            header = '  '

        print_line(output_indent, header, text)


def main():
    current_section = None

    for line in fileinput.input():
        mo = COUNT_RE.match(line)
        if mo:
            input_indent_count = len(mo.group(1))
            input_indent = mo.group(2)
            output_indent_count = int(mo.group(3)) + 1
            text = mo.group(4)
            current_section = Section(
                input_indent, input_indent_count, output_indent_count
            )
            current_section.print_header(text)
            continue

        mo = INDENT_RE.match(line)
        if not mo:
            raise "bad line: {}".format(line)
        indent = mo.group(1)
        text = mo.group(2)
        if not indent or indent.isspace():
            if current_section:
                current_section.print_body(len(indent), text)
            else:
                print_line(len(indent), "", text)
        else:
            current_section = Section(
                indent, len(indent), len(indent)
            )
            current_section.print_header(text)


def print_line(count, header, text):
    print("{}{}{}".format(" " * count, header, text))


def adjust_count(new_header, old_count, count):
    # when redex prints "> > >[18]", we adjust that to be 18 spaces,
    # but the subsequent lines only have a smaller amount of whitespace,
    # so we have to make it match.
    if '>' not in new_header and '<' not in new_header:
        if old_count > count:
            return old_count
    return count


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
