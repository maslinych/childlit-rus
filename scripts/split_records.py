#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import regex as re
import csv
import argparse
import sys
from collections import OrderedDict

AUTHOR_NAME = r"""
(?!Герой\sСоветского\s+Союза)
(?<last>           # Фамилия:
([Дд]['’]|[Дд]е[ -])?([Лл]а\s+)?\p{Lu}\p{Ll}+      # Воронцов / д'Амичис де(-)?
(-\p{Lu}\p{Ll}+|   # Воронцов-Вельяминов
-(отец|старший|заде|зода|оол|улы)|   # Дюма-отец Сарыг-оол
\s+\p{Lu}\p{Ll}+(?=,))?# Сервантес Сааведра, Мигель 
)
(\s+\((?<real>     # расшифровка псевдонима:
\p{Lu}\p{Ll}+)\))? #  Петров (Бирюк)
(  # альтернатива — без запятой:
\s+(?<ini>                                                  # инициалы или имена:
\p{Lu}\p{Ll}{0,2}\.[\s-]?\p{Lu}\p{Ll}{0,2}\.([\s-]\p{Lu}\.)?  # Дм. Ив.; Г.-Х. (-A.)?
|\p{Lu}\p{Ll}{0,2}\.(\s+де|,\s+отец)?                                        # А. (де ,отец)?
|\p{Lu}\p{Ll}+\s+\p{Lu}\p{Ll}{3,}(?=(\.|,\s+\p{Lu}\p{Ll}+|\s+и\s+др\.|\s+и\s+\p{Lu}\p{Ll}+|,?\s+\[|\s+\((\p{Lu}\p{Ll}{0,2}\.\s+)+\p{Lu}\p{Ll}+)) # Иван Ильич
|\p{Lu}\p{Ll}+-(Булат|бао|ф[эе]й|и|мин|Фу|Ю|хуа|цзин?|линь|юй|нань|чжень|ян|заде)  # Хас-Булат, Юй-бао и тп.
|\p{Lu}\p{Ll}+\s+\p{Lu}\p{Ll}{0,2}\.              # Фенимор Д.
|\p{Lu}\p{Ll}{1,}(-\p{Lu}\p{Ll}+)?(\s+де)?(?=(\.|,\s+\p{Lu}\p{Ll}+|\s+и\s+др\.|\s+и\s+\p{Lu}\p{Ll}+|,?\s+\[|\s+\((\p{Lu}\p{Ll}{0,2}\.\s+)+\p{Lu}\p{Ll}+|\s+\(\p{Lu}+\p{Ll}*))) # Василий; Иоганн-Вольфганг; Шарль де
|  # альтернатива — после запятой:
,\s+(?<ini>братья|\p{Lu}\p{Ll}+)(?=\.)                          # Гримм, братья
)(\s+
\((?<real> # расшифровка псевдонима:
((проф\.\s+)?(\p{Lu}\p{Ll}{0,2}\.\s+){0,2}\p{Lu}\p{Ll}+) # (проф. П. П. Петров)
|
(\p{Lu}{2,5}|\p{Lu}\p{Ll}{2,}))\)
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
SINGLE_AUTHORS = r"""
Aaсамурти|
Айбек|
Алтан-Хайша|
Анко|
Антониорроблес|
Аригапуди|
Аткай|
Гайрати|
Гомер|
Джамбул|
Елин-Пелин|
Конан-Дойль|
Кукрыниксы|
Лесник|
Луда|
Магомед-Расул|
Майн-Рид|
Мирмухсин|
Миртемир|
Михайлова|
Мольер|
Мультатули|
Обос-Апер|
Плутарх|
Решетов-Жнивник|
Садов|
Сайяр|
Сан-Марку|
Сат-Окх|
Стендаль|
Уйда|
Улуро Адо|
Физули|
Фирдоуси|
Фуиг-Куан|
Хнко-Апер|
Шанкар|
Шолом-Алейхем|
Эзоп|
Элляй|
Эль-Регистан|
Эльчин|
Энба|
Эсхил|
д’Актиль|
д’Эрвильи
"""


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


class Record(OrderedDict):
    def __init__(self, tail='', start=0, end=0):
        super(Record, self).__init__()
        self.tail = tail
        self.start = start
        self.end = end

    def serialize(self):
        out = []
        out.append(self.start)
        out.append(self.end)
        for k, v in self.items():
            out.append(str(v))
        out.append(self.tail)
        return out


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
    for lineno, line in enumerate(infile, start=1):
        line = line.strip()
        if line.startswith('#END'):
            break
        if line:
            num, tail = extract_number(line)
            yield (lineno, num, tail)


def iter_records(numlines, k=10):
    """Join a series of numbered lines into a list of sequentially
numbered items (Record instances with a defined 'num' key, tail
attribute and start and end line numbers)
    """
    itemno = 0
    stack = []
    startline = 0
    for lineno, n, txt in numlines:
        if n == 0:
            num = 0
        else:
            num = BibItem(string=n)
        if num > itemno:
            if num - itemno == 1:
                # we have a regular next item
                if stack:
                    rec = Record(tail = ' '.join(stack))
                    rec['num'] = itemno
                    rec.start = startline
                    rec.end = lineno - 1
                    yield rec
                    stack = []
                    startline = lineno
            else:
                if num - itemno > k:
                    # gap in numbers is too large, unlikely to be the next
                    # number, treat as a regular textual line (with an
                    # accidental number in the beginning, like a year or
                    # a printrun figure)
                    stack.append('{}. {}'.format(num, txt))
                    continue
                # we have a moderate gap in numbering, treat as next item
                rec = Record(tail = ' '.join(stack))
                rec['num'] = itemno
                rec.start = startline
                rec.end = lineno - 1
                yield rec
                stack = []
                itemno += 1
                startline = lineno
                while num > itemno:
                    rec = Record(tail = 'MISSING', start = startline, end = lineno - 1)
                    rec['num'] = itemno
                    yield rec
                    itemno += 1
            itemno = num
            stack.append(txt)
        elif num == 0 and itemno > 0:
            # non-numbered line, collect as a continuation of a curent item
            stack.append(txt)
        elif num < itemno:
            # a lesser number, not a next item, treat as an item continuation
            stack.append('{}. {}'.format(num, txt))
    else:
        # end of file: yield a final record
        rec = Record(tail = ' '.join(stack))
        rec['num'] = str(itemno)
        rec.start = startline
        rec.end = lineno
        yield rec


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
    

def extract_author(rec, prev=None, verbose=False):
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
                                     r")[\W\s]+(?<tail>.*)$", re.U | re.VERBOSE )
    and_others = re.compile(AUTHOR_NAME +
                            r"\s+и\s+др\.\s+(?<tail>(?<head>[^/]+)" +
                            r"(/\s*(?<others>(" + INI_AUTHOR + r"[,;.]\s+)+))?.*)$",
                            re.U | re.VERBOSE)
    noauthor_tag = re.compile(r"^\s*@NOAUTHOR@[[\W\s]--[«]]+(?<tail>.*)$", re.V1)
    author_tag = re.compile(r"^\s*@AUTHOR:(?<all>[^@]+)@[[\W\s]--[«]]+(?<tail>.*)$", re.V1)
    hasone = one_author.match(rec.tail)
    hasdash = dash.match(rec.tail)
    hasmulti = multi_author.match(rec.tail)
    hassingle = single_name_authors.match(rec.tail)
    hasothers = and_others.match(rec.tail)
    hasnoauthor = noauthor_tag.match(rec.tail)
    hastag = author_tag.match(rec.tail)
    if hasmulti:
        if verbose:
            print("hasmulti:", hasmulti.groupdict())
        rec['author'] = format_multi_authors(hasmulti.group('all'))
        rec.tail = hasmulti.group('tail')
    elif hasothers:
        if verbose:
            print("hasothers:", hasothers.groupdict())     
        rec['author'] = "{last}, {ini}; OTHERS".format(**hasothers.groupdict())
        rec.tail = hasothers.group('tail')
        if hasothers.group('others'):
            others = format_other_authors(hasothers.group('others'))
            if others:
                rec['author'] = others
    elif hasone:
        if verbose:
            print("hasone:", hasone.groupdict())
        rec['author'] = "{last}, {ini}".format(**hasone.groupdict())
        if hasone.group('real'):
            rec['author'] = "{0} [{1}]".format(rec['author'], hasone.group('real'))
        rec.tail = hasone.group('tail')
    elif hasdash:
        if prev is None:
            rec['author'] = "ERRAUTHOR"
        elif prev == "NOAUTHOR":
            rec['author'] = "ERRAUTHOR"
        else:
            rec['author'] = prev
        rec.tail = hasdash.group('tail')
    elif hassingle:
        rec['author'] = hassingle.group('last')
        rec.tail = hassingle.group('tail')
    elif hasnoauthor:
        rec['author'] = "NOAUTHOR"
        rec.tail = hasnoauthor.group('tail')
    elif hastag:
        rec['author'] = hastag.group('all')
        rec.tail = hastag.group('tail')
    else:
        rec['author'] = "NOAUTHOR"
    return rec


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
    parser.add_argument('infile', nargs='?', help='Input file (txt)',
                        type=argparse.FileType('r', encoding='UTF-8'), default=sys.stdin)
    parser.add_argument('outfile', nargs='?', help='Output file (csv)',
                        type=argparse.FileType('w', encoding='UTF-8'), default=sys.stdout)
    parser.add_argument('-v', '--verbose', help='Show regex debugging output',
                        action='store_true')
    return parser.parse_args()


def main():
    """main processing"""
    args = parse_arguments()
    csv_writer = csv.writer(args.outfile)
    author = None
    for rec in iter_records(numbered_lines(args.infile)):
        row = extract_author(rec, author, verbose=args.verbose)
        author = row['author']
        csv_writer.writerow(row.serialize())


if __name__ == '__main__':
    main()
