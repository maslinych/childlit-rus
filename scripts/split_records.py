#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import regex as re
import csv
import argparse

AUTHOR_NAME = r"""
(?!Герой\sСоветского\s+Союза)
(?<last>           # Фамилия:
\p{Lu}\p{Ll}+      # Воронцов
(-\p{Lu}\p{Ll}+|   # Воронцов-Вельяминов
\s+\p{Lu}\p{Ll}+(?=,))?# Сервантес Сааведра, Мигель 
)
(\s+\((?<real>     # расшифровка псевдонима:
\p{Lu}\p{Ll}+)\))? #  Петров (Бирюк)
(  # альтернатива — без запятой:
\s+(?<ini>                                        # инициалы или имена:
\p{Lu}\p{Ll}{0,2}\.[\s-]?\p{Lu}\p{Ll}{0,2}\.      # Дм. Ив.; Г.-Х.
|\p{Lu}\p{Ll}{0,2}\.                              # А.
|\p{Lu}\p{Ll}+\s+\p{Lu}\p{Ll}{3,}(?=(\.|,\s+\p{Lu}\p{Ll}+|и\s+др\.)) # Иван Ильич
|\p{Lu}\p{Ll}+\s+\p{Lu}\p{Ll}{0,2}\.              # Фенимор Д.
|\p{Lu}\p{Ll}{3,}(-\p{Lu}\p{Ll}+)?(\s+де)?(?=(\.|,\s+\p{Lu}\p{Ll}+|и\s+др\.|\s+и\s+\p{Lu}\p{Ll}+))) # Иоганн-Вольфганг; Шарль де
|  # альтернатива — после запятой:
,\s+(?<ini>братья|\p{Lu}\p{Ll}+)(?=\.)                          # Гримм, братья
)(\s+
\((?<real> # расшифровка псевдонима:
(проф\.\s+)?(\p{Lu}\p{Ll}{0,2}\.\s+){0,2}\p{Lu}\p{Ll}+)\) # (проф. П. П. Петров)
(?=\.)
|,\s+                                       # титулы:
((?<real>Герой\s+Советского\s+Союза(?=\.)|   # Чкалов, Герой Советского Союза
проф\.|[Дд]-р.|инж\.|акад\.|доцент|сост\.),?                            # , проф.; Д-р.
|\[(?<real>[^]]+)\]                     # [явная маркировка] 
))?
"""
INI_AUTHOR = r"""
(?<ini>\p{Lu}\p{Ll}{0,2}\.([\s-]?\p{Lu}\p{Ll}{0,2}\.)?)\s+
(?<last>\p{Lu}\p{Ll}+(-\p{Lu}\p{Ll}+)?)
"""
SINGLE_AUTHORS = r'Джамбул|Майн-Рид|Мольер|Эсхил|Айбек|Обос-Апер'


class BibItem(object):
    """A class to hold a sequential bibliographic number.  In contrast to
    the standard integer it can have a letter suffix for the items
    inserted in the list. The class provides methods for comparison of
    standard and suffixed numbers for correct item numebering and
    alignment.  Supports operations with integers: comparison,
    addition, subtraction. These result in integers in all cases.
    Similar operations with two BibItems result in a BibItem.
    """
    def __init__(self, num=0, suffix=0, string=None):
        if string is None:
            self.num = num
            self.suffix = suffix
        elif string == 0:
            self.num = 0
            self.suffix = 0
        else:
            m = re.match(r"(?<num>[1-9][0-9]*)(?<suffix>[aаб])?$", string)
            suffixdict = {None: 0, 'a': 1, 'а': 1, 'б': 2}
            try:
                self.num = int(m.group('num'))
                self.suffix = suffixdict[m.group('suffix')]
            except AttributeError:
                raise ValueError("Incorrect value for BibItem: %s" % string)
        self.value = (self.num, self.suffix)

    def __str__(self):
        numtosuf = {0: '', 1: 'а', 2: 'б'}
        return ''.join([str(self.num), numtosuf[self.suffix]])

    def __eq__(self, other):
        if isinstance(other, int):
            return self.num + self.suffix == other
        else:
            return self.value == other.value

    def __lt__(self, other):
        if isinstance(other, int):
            return self.num + self.suffix < other
        else:
            return self.value < other.value

    def __gt__(self, other):
        if isinstance(other, int):
            return self.num + self.suffix > other
        else:
            return self.value > other.value

    def __add__(self, other):
        if isinstance(other, int):
            return self.num + self.suffix + other
        else:
            return self.num + other.num + self.suffix + other.suffix

    def __sub__(self, other):
        if isinstance(other, int):
            return self.num + self.suffix - other
        else:
            return self.num + self.suffix - other.num - other.suffix


def extract_number(line):
    """Detect if a line matches a pattern for a numbered bibliography item
    Return a tuple with a number and a text line. If a line doesn't have the
    number return zero and full line as output.
    """
    num = re.match(r'\s*(?<num>[1-9][0-9]*[aаб]?)\.\s+(?<tail>.+)', line)
    if num:
        return (num.group('num'), num.group('tail'))
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
    for n, txt in numlines:
        if n == 0:
            num = 0
        else:
            num = BibItem(string=n)
        if num > itemno:
            if num - itemno == 1:
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
                while num > itemno:
                    yield (itemno, ['MISSING'])
                    itemno += 1
            itemno = num
            stack.append(txt)
        elif num == 0 and itemno > 0:
            stack.append(txt)
        elif num < itemno:
            stack.append('{}. {}'.format(num, txt))
    else:
        yield (str(itemno), stack)


def format_multi_authors(authors):
    out = []
    if not authors.endswith('.'):
        authors = authors + '.'
    for author in re.finditer(AUTHOR_NAME, authors, re.U | re.VERBOSE):
        if author.group('real'):
            out.append("{last}, {ini} [{real}]".format(**author.groupdict()))
        else:
            out.append("{last}, {ini}".format(**author.groupdict()))
    return "; ".join(out)


def format_other_authors(others):
    others = others.strip(' ;.,')
    out = []
    ini_author = re.compile(INI_AUTHOR, re.U | re.VERBOSE)
    for author in re.split(r'[,;]\s+', others):
        try:
            m = ini_author.match(author)
            out.append("{last}, {ini}".format(**m.groupdict()))
        except AttributeError:
            raise ValueError("Unrecognized author in the others list: %s" % author)
    return "; ".join(out)
    

def extract_author(num, txt, prev=None, verbose=False):
    """Process numbered lines, extract author name as a separate column,
or indicate that it is missing with the NOAUTHOR tag. Inconsistencies
are marked with ERRAUTHOR tag.
    """
    one_author = re.compile(AUTHOR_NAME + r"([[\W\s]--[,«]]+|\s+@[[\W\s]--[«]]+)(?<tail>.*)$", re.U | re.VERBOSE | re.V1)
    dash = re.compile(r"^[\W\s]*[—][\W\s]*(?<tail>.*)$", re.U)
    multi_author = re.compile(r'(?<all>' + AUTHOR_NAME +
                              r'((\s+и\s+|,\s+)' + AUTHOR_NAME + r')+' +
                              r')[\W\s]+(?<tail>.*)$', re.U | re.VERBOSE)
    single_name_authors = re.compile(r"(?<last>" + SINGLE_AUTHORS +
                                     r")[\W\s]+(?<tail>.*)$", re.U)
    and_others = re.compile(AUTHOR_NAME +
                            r"\s+и\s+др\.\s+(?<tail>(?<head>[^/]+)" +
                            r"(/\s*(?<others>(" + INI_AUTHOR + r"[,;.]\s+)+))?.*)$",
                            re.U | re.VERBOSE)
    author_tag = re.compile(r"^\s*@NOAUTHOR@[\W\s]+(?<tail>.*)$")
    hasone = one_author.match(txt)
    hasdash = dash.match(txt)
    hasmulti = multi_author.match(txt)
    hassingle = single_name_authors.match(txt)
    hasothers = and_others.match(txt)
    hastag = author_tag.match(txt)
    if hasmulti:
        if verbose:
            print("hasmulti:", hasmulti.groupdict())
        author = format_multi_authors(hasmulti.group('all'))
        tail = hasmulti.group('tail')
    elif hasothers:
        if verbose:
            print("hasothers:", hasothers.groupdict())     
        author = "{last}, {ini}; OTHERS".format(**hasothers.groupdict())
        tail = hasothers.group('tail')
        if hasothers.group('others'):
            others = format_other_authors(hasothers.group('others'))
            if others:
                author = others
    elif hasone:
        if verbose:
            print("hasone:", hasone.groupdict())
        author = "{last}, {ini}".format(**hasone.groupdict())
        if hasone.group('real'):
            author = "{0} [{1}]".format(author, hasone.group('real'))
        tail = hasone.group('tail')
    elif hasdash:
        if prev is None:
            author = "ERRAUTHOR"
        elif prev == "NOAUTHOR":
            author = "ERRAUTHOR"
        else:
            author = prev
        tail = hasdash.group('tail')
    elif hassingle:
        author = hassingle.group('last')
        tail = hassingle.group('tail')
    elif hastag:
        author = "NOAUTHOR"
        tail = hastag.group('tail')
    else:
        author = "NOAUTHOR"
        tail = txt
    return author, (num, author, tail)


# def extract_title(row):
#     head = row[:-1]
#     tail = row[-1]
#     re.compile(r'')
    
    
def parse_arguments():
    parser = argparse.ArgumentParser(description='Split scanned txt file into numbered records (CSV)', epilog=""" The idea is to rely on the sequentially numbered items. The script
identifies all lines that look like a numbered item. All non-itemlike
lines are joined to the previous numbered line, until the next tem in
a sequence is encountered. When an expected next item is missing, a
'MISSING' tag is printed in the output CSV file.""")
    parser.add_argument('infile', help='Inpout file (txt)')
    parser.add_argument('outfile', help='Output file (csv)')
    parser.add_argument('-v', '--verbose', help='Show regex debugging output',
                        action='store_true')
    return parser.parse_args()


def main():
    """main processing"""
    args = parse_arguments()
    out = open(args.outfile, 'w')
    csv_writer = csv.writer(out)
    author = None
    for num, stack in iter_records(numbered_lines(args.infile)):
        author, row = extract_author(num, ' '.join(stack), author, verbose=args.verbose)
        csv_writer.writerow(row)


if __name__ == '__main__':
    main()
