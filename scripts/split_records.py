#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import re
import csv
import argparse


def extract_number(line):
    """Detect if a line matches a pattern for a numbered bibliography item
    Return a tuple with a number and a text line. If a line doesn't have the
    number return zero and full line as output.
    """
    num = re.match('\s*([1-9][0-9]*)\.\s+(.+)', line)
    if num:
        return (int(num.group(1)), num.group(2))
    else:
        return (0, line)


def numbered_lines(infile):
    """Generator producing numbered lines as tuples"""
    with open(infile, 'r') as f:
        for line in f:
            line = line.strip()
            if line.startswith('#END'):
                break
            if line:
                yield extract_number(line)


def iter_records(numlines, k=10):
    """Join a series of numbered lines into a list of sequentially
numbered items"""
    itemno = 0
    stack = []
    for num, txt in numlines:
        if num > itemno:
            if num == itemno + 1:
                if stack:
                    yield (itemno, stack)
                    stack = []
            else:
                if num - itemno > k:
                    stack.append('{}. {}'.format(num, txt))
                    continue
                yield (itemno, stack)
                stack = []
                itemno += 1
                while itemno < num:
                    yield (itemno, ['MISSING'])
                    itemno += 1
            itemno = num
            stack.append(txt)
        elif num == 0 and itemno > 0:
            stack.append(txt)
        elif num < itemno:
            stack.append('{}. {}'.format(num, txt))
    else:
        yield (itemno, stack)


def parse_arguments():
    parser = argparse.ArgumentParser(description='Split scanned txt file into numbered records (CSV)', epilog=""" The idea is to rely on the sequentially numbered items. The script
identifies all lines that look like a numbered item. All non-itemlike
lines are joined to the previous numbered line, until the next tem in
a sequence is encountered. When an expected next item is missing, a
'MISSING' tag is printed in the output CSV file.""")
    parser.add_argument('infile', help='Inpout file (txt)')
    parser.add_argument('outfile', help='Output file (csv)')
    return parser.parse_args()

def main():
    """main processing"""
    args = parse_arguments()
    out = open(args.outfile, 'w')
    csv_writer = csv.writer(out)
    for num, stack in iter_records(numbered_lines(args.infile)):
        csv_writer.writerow([num, ' '.join(stack)])

if __name__ == '__main__':
    main()
