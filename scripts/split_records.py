#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import regex as re
import csv
import argparse
import sys
import os
from string import Formatter

AUTHOR_NAME = r"""
(?!Герой\sСоветского\s+Союза)
(?<last>           # Фамилия:
([ДдО]['’]|[Дд]е[ -])?([Лл]а\s+)?\p{Lu}(\p{Ll}+|\p{Lu}+)      # Воронцов / д'Амичис де(-)?
(-\p{Lu}(\p{Ll}+|\p{Lu}+)|   # Воронцов-Вельяминов
-(отец|старший|заде|зода|оол|улы|яшь)|   # Дюма-отец Сарыг-оол
\s+\p{Lu}\p{Ll}+(?=,))?# Сервантес Сааведра, Мигель 
)
(\s+\((?<real>     # расшифровка псевдонима:
\p{Lu}(\p{Ll}+|\p{Lu}+))\))? #  Петров (Бирюк)
(  # альтернатива — без запятой:
\s+(?<ini>                                                  # инициалы или имена:
\p{Lu}\p{Ll}{0,2}\.[\s-]?\p{Lu}\p{Ll}{0,2}\.([\s-]\p{Lu}\.)?  # Дм. Ив.; Г.-Х. (-A.)?
|\p{Lu}\p{Ll}{0,2}\.(\s+де|,\s+отец)?                                        # А. (де ,отец)?
|\p{Lu}\p{Ll}+\s+\p{Lu}\p{Ll}{3,}(?=(\.|,\s+\p{Lu}\p{Ll}+|\s+и\s+др\.|\s+и\s+\p{Lu}\p{Ll}+|,?\s+\[|\s+\((\p{Lu}\p{Ll}{0,2}\.\s+)+\p{Lu}\p{Ll}+)) # Иван Ильич
|\p{Lu}\p{Ll}+-(Булат|бао|ф[эе]й|и|мин|Фу|Ю|хуа|цзин?|линь|юй|нань|чжень|ян|заде|ань)  # Хас-Булат, Юй-бао и тп.
|\p{Lu}\p{Ll}+\s+\p{Lu}\p{Ll}{0,2}\.              # Фенимор Д.
|\p{Lu}(\p{Ll}+|\p{Lu}+)(-\p{Lu}\p{Ll}+)?(\s+де)?(?=(\.|,\s+\p{Lu}\p{Ll}+|\s+и\s+др\.|\s+и\s+\p{Lu}\p{Ll}+|,?\s+\[|\s+\((\p{Lu}\p{Ll}{0,2}\.\s+)+\p{Lu}\p{Ll}+|\s+\(\p{Lu}+\p{Ll}*))) # Василий; Иоганн-Вольфганг; Шарль де
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
Адалло|
Айбек|
Алтан-Хайша|
Алтыншаш|
Анар|
Анко|
Антониорроблес|
Аригапуди|
Аткай|
Аэл|
Бенашвили|
Васамурти|
Габиба|
Гайрати|
Гомер|
Джамбул|
Зульфия|
Елин-Пелин|
Конан-Дойль|
Кукрыниксы|
Лабулэ|
Леони|
Лерхе|
Лесник|
Луда|
Магомед-Расул|
Майн-Рид|
Матвиенко|
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
Сат-Окх?|
Стендаль|
Тулкун|
Уйда|
Улуро Адо|
Физули|
Фирдоуси|
Фуиг-Куан|
Хнко-Апер|
Шанкар|
Шиан-Ю|
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

CITY = r"""
Б\.\s*м\.|
Абакан|
Алапаевск|
Алма-Ата|
Анапа|
Архангельск|
Астрахань|
Ашхабад|
Баку|
Балаково|
Барановичи|
Барнаул|
Батуми|
Бежица|
Белгород|
Березники|
Берлин|
Биробиджан|
Бийск|
Бирск|
Благовещенск|
Бобруйск|
Боровичи|
Брянск|
В\.(-|\s+)Луки|
Варшава|
Великий\s+Устюг|
Вильнюс|
Винница|
Витебск|
Владивосток|
Владимир|
Волгоград|
Вологда|
Волоколамск|
Волочиск|
Воронеж|
Ворошиловград|
Ворошиловск|
Выкса|
Вятка|
г\.\sСлободской|
Геленджик|
Гомель|
Горно-Алтайск|
Горький|
Гродно|
Грозный|
Гурьев|
Дзауджикау|
Дзержинск|
Днепропетровск|
Донецк|
Душанбе|
Екатеринбург|
Екатеринослав|
Елец|
Ереван|
Загорск|
Запорожье|
Зарайск|
Зея|
Златоуст|
Ив\.-Вознесенск|
Иваново-Вознесенск|
Иваново|
Ижевск|
Ирбит|
Иркутск|
Йошкар-Ола|
Казань|
Калинин(?!град)|
Калининград|
Калуга|
Камень|
Камышин|
Караганда|
Каунас|
Кашира|
Кемерово|
Кзыл-Орда|
Ки[еi]в|
Киров(?!оград)|
Кировоград|
Кисловодск|
Кишинев|
Клин|
Кострома|
Краснодар|
Красноярск|
Кременчуг|
Крым|
Кудымкар|
Куйбышев|
Кунгур|
Курган|
Курск|
Кызыл|
Л\.|
Ленинград|
Липецк|
Лодейное\s+[Пп]оле|
Луга(?!нск)|
Луганск|
Луцк|
Львов|
М\.|
Магадан|
Майкоп|
Махачкала|
Минск|
Минусинск|
Мичуринск|
Могил[её]в|
Молодечно|
Молотов|
Москва|
Мурманск|
Муром|
Мытищи|
Н\.-Новгород|
Нальчик|
Нижний-Ломов\s+[(]Пензенск\.\s+губ\.[)]|
Николаев|
Новгород|
Ново-Николаевск|
Новокузнецк|
Новониколаевск|
Новороссийск|
Ново-Сибирск|
Новосибирск|
Новочеркасск|
Нукус|
Одесса|
Омск|
Орджоникидзе(?!град)|
Орджоникидзеград|
Орел|
Оренбург|
Ош|
Павлодар|
Пг?\.|
Пенза|
Пермь|
Петрозаводск|
Петропавловск-Камчатский|
Покровск|
Полтава|
Псков|
Пучеж|
Пятигорск|
Рига|
Ржев|
Ростов-Дон|
Ростов-Ярославск([.]|ий)|
Ростов-на-Дону|
Ростов\s+н/Д\.?|
Ростов|
Рыбинск|
Рязань|
С\.\s+Гаютино|
С\.\s+Нюксеница|
С?ПБ\.|
Самара|
Самарканд|
Саранск|
Саратов|
Свердловск|
Севастополь|
Село\s+Атмис\s+[(]Н\.-Ломовского\s+уезда,\s+Пензенск\.\s+губ\.[)]|
Семипалатинск|
Сергиев|
Серпухов|
Симферополь|
Смоленск|
Сочи|
Ст\.\s+Тимошевская|
Ставрополье?|
Сталинабад|
Сталинград|
Сталинир|
Сталино|
Станислав|
Сухуми?|
Сызрань|
Сыктывкар|
Таганрог|
Таллин|
Тамбов|
Ташкент|
Тбилиси|
Тверь|
Темир-Тау|
Тифлис|
Тобольск|
Томск|
Тула|
Тюмень|
Ужгород|
Улан-Батор|
Улан-Удэ|
Ульяновск|
Уральск|
Усть-Каменогорск|
Уфа|
Фергана|
Фрунзе|
Хабаровск|
Ханой|
Харьк[оi]в|
Херсон|
Целиноград|
Цхинвали|
Чебоксары|
Челябинск|
Череповец|
Черкассы|
Черкесск|
Чернигов|
Черновицы|
Черновцы|
Чимкент|
Чита|
Чкалов|
Шадринск|
Шахты|
Щербаков|
Элиста|
Юж\.-Уральск|
Ю(ж?\.|жн[.о])[-\s]Сахалинск|
Якутск|
Ялта|
Ярославль
"""


class ExtendedFormatter(Formatter):
    """An extended format string formatter

    Formatter with extended conversion symbol
    """
    def convert_field(self, value, conversion):
        """ Extend conversion symbol
        Following additional symbol has been added
        * c: convert to string and capitalize

        default are:
        * s: convert with str()
        * r: convert with repr()
        * a: convert with ascii()
        """

        if conversion == "c":
            if re.search(r"[-'’ ]", str(value)):
                if str(value).isupper():
                    return ''.join(i.capitalize() for i in re.split(r"([-'’ ])", str(value)))
                else:
                    return str(value)
            else:
                return str(value).capitalize()
        # Do the default conversion or raise error if no matching conversion found
        return super(ExtendedFormatter, self).convert_field(value, conversion)


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
            m = re.match(r"(?<num>[1-9][0-9]*)(-?(?<suffix>[aабвгде]))?$", string)
            suffixdict = {None: 0, 'a': 1, 'а': 1, 'б': 2, 'в': 3, 'г': 4, 'д': 5, 'е': 6}
            try:
                self.num = int(m.group('num'))
                self.suffix = suffixdict[m.group('suffix')]
            except AttributeError:
                raise ValueError("Incorrect value for BibItem: %s" % string)
        self.value = (self.num, self.suffix)

    def __str__(self):
        numtosuf = {0: '', 1: 'а', 2: 'б', 3: 'в', 4: 'г', 5: 'д', 6: 'е'}
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


class Record(dict):
    def __init__(self, tail='', start=0, end=0):
        super(Record, self).__init__()
        self.tail = tail
        self.start = start
        self.end = end
        self.fields = ['vol', 'num', 'author', 'title',
                       'subtitle', 'editorial', 'city', 'publisher',
                       'year', 'series', 'pages', 'printrun', 'price',
                       'addressee', 'contents', 'tail', 'bibaddon',
                       'section', 'thesame', 'start', 'end']

    def serialize(self):
        out = {}
        out['start'] = self.start
        out['end'] = self.end
        out['tail'] = self.tail
        out.update(self)
        return out

    @property
    def isthesame(self):
        try:
            return self['thesame'] == 'THESAME'
        except KeyError:
            return False


def get_volume_id(path):
    return os.path.basename(path)[:-4]


def extract_number(line):
    """Detect if a line matches a pattern for a numbered bibliography item
    Return a tuple with a number and a text line. If a line doesn't have the
    number return zero and full line as output.
    """
    num = re.match(r'\s*(?<num>[1-9][0-9]*-?[aабвгде]?)\.\s+(?<tail>.+)', line)
    if num:
        return (num.group('num'), num.group('tail'))
    else:
        return (0, line)


def numbered_lines(infile):
    """Generator producing numbered lines as tuples"""
    vol = get_volume_id(infile)
    section = ''
    with open(infile, 'r') as f:
        for lineno, line in enumerate(f, start=1):
            line = line.strip()
            if line.startswith('#'):
                if line.startswith('#END'):
                    break
                else:
                    section = line
            if line:
                num, tail = extract_number(line)
                yield (lineno, num, tail, section, vol)


def collect_sections(prev, sec):
    """format nested section string [str], str -> [str]"""
    try:
        last = prev[-1]
        depth = len(last) - len(last.strip('#'))
        sec_depth = len(sec) - len(sec.strip('#'))
        if sec_depth > depth:
            prev.append(sec)
        elif sec_depth < depth:
            while len(prev) > sec_depth - 1:
                del prev[-1]
            prev.append(sec)
        elif sec_depth == depth and not last == sec:
            del prev[-1]
            prev.append(sec)
        return prev
    except IndexError:
        return [sec]


def join_record(stack):
    """join a series of strings (lines from a txt file) into a single record string"""
    tail = ' '.join(stack)
    hypen_re = r"(?<pre>\p{Ll})[­-]\s+(?=\p{Ll})|(?<pre>\p{Lu})[­-]\s+(?=\p{Lu})|(?<pre>\p{Ll})­(?=\p{Ll})"
    tail = re.sub(hypen_re, r'\g<pre>', tail)
    return tail
    

def iter_records(numlines, k=10):
    """Join a series of numbered lines into a list of sequentially
numbered items (Record instances with a defined 'num' key, tail
attribute and start and end line numbers)
    """
    itemno = 0
    stack = []
    startline = 0
    seclist = []
    for lineno, n, txt, section, vol in numlines:
        if n == 0:
            num = 0
            if seclist:
                if not section == seclist[-1]:
                    seclist = collect_sections(seclist, section)
            else:
                if section:
                    seclist = collect_sections(seclist, section)                 
        else:
            num = BibItem(string=n)
        if num > itemno:
            if num - itemno == 1:
                # we have a regular next item
                if stack:
                    rec = Record(tail = join_record(stack))
                    rec['num'] = itemno
                    rec.start = startline
                    rec.end = lineno - 1
                    rec['section'] = ' '.join(seclist)
                    rec['vol'] = vol
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
                rec = Record(tail = join_record(stack))
                rec['num'] = itemno
                rec.start = startline
                rec.end = lineno - 1
                rec['section'] = ' '.join(seclist)
                rec['vol'] = vol
                yield rec
                stack = []
                itemno += 1
                startline = lineno
                while num > itemno:
                    rec = Record(tail = 'MISSING', start = startline, end = lineno - 1)
                    rec['num'] = itemno
                    rec['section'] = ' '.join(seclist)
                    rec['vol'] = vol
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
        rec = Record(tail = join_record(stack))
        rec['num'] = str(itemno)
        rec.start = startline
        rec.end = lineno
        rec['section'] = section
        rec['vol'] = vol
        yield rec


def format_multi_authors(authors):
    """return normalized string representafor multiple authors"""
    out = []
    fmtr = ExtendedFormatter()
    if not authors.endswith('.'):
        authors = authors + '.'
    for author in re.finditer(AUTHOR_NAME, authors, re.U | re.VERBOSE):
        if author.group('real'):
            out.append(fmtr.format("{last!c}, {ini} [{real}]", **author.groupdict()))
        else:
            out.append(fmtr.format("{last!c}, {ini}", **author.groupdict()))
    return "; ".join(out)


def format_other_authors(others):
    others = others.strip(' ;.,')
    out = []
    fmtr = ExtendedFormatter()
    ini_author = re.compile(INI_AUTHOR, re.U | re.VERBOSE)
    for author in re.split(r'[,;]\s+', others):
        try:
            m = ini_author.match(author)
            out.append(fmtr.format("{last!c}, {ini}", **m.groupdict()))
        except AttributeError:
            raise ValueError("Unrecognized author in the others list: %s" % author)
    return "; ".join(out)
    

def extract_author(rec, prev=None, verbose=False):
    """Process numbered lines, extract author name as a separate column,
or indicate that it is missing with the NOAUTHOR tag. Inconsistencies
are marked with ERRAUTHOR tag.
    """
    fmtr = ExtendedFormatter()   
    one_author = re.compile(AUTHOR_NAME + r"\.?(?(?=\s*\.\.\.)\s*(?<tail>.*)|([[\W\s]--[,«]]+|\s+@[[\W\s]--[«]]+)(?<tail>.*))$", re.U | re.VERBOSE | re.V1)
    dash = re.compile(r"^[\W\s]*[—][\W\s]*(?<tail>.*)$", re.U)
    multi_author = re.compile(r'(?<all>' + AUTHOR_NAME +
                              r'((\s+и\s+|,\s+)' + AUTHOR_NAME + r')+' +
                              r')(?(?=\s+\.\.\.)\s*(?<tail>.*)|[\W\s]+(?<tail>.*))$', re.U | re.VERBOSE)
    single_name_authors = re.compile(r"(?<last>" + SINGLE_AUTHORS +
                                     r")(?(?=\s+\.\.\.)\s*(?<tail>.*)|[\W\s]+(?<tail>.*))$", re.U | re.VERBOSE )
    and_others = re.compile(AUTHOR_NAME +
                            r"\s+и\s+др\.\s+(?<tail>(?<head>[^/]+)" +
                            r"(/\s*(?<others>(" + INI_AUTHOR + r"[,;.]\s+)+))?.*)$",
                            re.U | re.VERBOSE)
    noauthor_tag = re.compile(r"^\s*@NOAUTHOR@(?(?=\s+\.\.\.)\s*(?<tail>.*)|[[\W\s]--[«]]+(?<tail>.*))$", re.V1)
    author_tag = re.compile(r"^\s*@AUTHOR:(?<all>[^@]+)@(?(?=\s+\.\.\.)\s*(?<tail>.*)|[[\W\s]--[«]]+(?<tail>.*))$", re.V1)
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
        rec['author'] = fmtr.format("{last!c}, {ini}; OTHERS", **hasothers.groupdict())
        rec.tail = hasothers.group('tail')
        if hasothers.group('others'):
            others = format_other_authors(hasothers.group('others'))
            if others:
                rec['author'] = others
    elif hasone:
        if verbose:
            print("hasone:", hasone.groupdict())
        rec['author'] = fmtr.format("{last!c}, {ini}", **hasone.groupdict())
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
        if hassingle.group('last').isupper():
            autr = hassingle.group('last').capitalize()
        else:
            autr = hassingle.group('last')
        rec['author'] = autr
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


def format_multi_cities(cities):
    out = []
    for city in re.finditer(CITY, cities, re.U | re.VERBOSE):
        out.append(city.group(0))
    return "; ".join(out)


def split_title_at_city(tail):
    """break record into title-related part and publication data using city as a hint"""
    INFO = r'(?<city>(' + CITY + r')(\s?(;|—-?)\s?(' + CITY + r')){0,4})[.,:](?<publisher>.*?)\s+(?<year>19[1-8][0-9]|[Бб]\.\s+г\.|[Бб]/г\.?)[\p{P}\s](\s*—\s*)?(?<tail>.*)$'
    if '@' in tail:
        break_at_city = re.compile(r'(?<alltitle>[^@]+\s+)@\s*' +
                                   INFO, re.U | re.VERBOSE)
    else:
        break_at_city = re.compile(r'(?<alltitle>([\p{Lu}\d«(i№]|\.\.\.).+?[,.)?!—-])\s*'
                   + INFO, re.U | re.VERBOSE)
    return break_at_city.match(tail)


def extract_title(rec, verbose=False):
    the_same = re.compile(r'(?<alltitle>Т\s*о\s*ж\s*е\s*[.,])(?<remainder>.*)$')
    ref = re.compile(r'(?<alltitle>.+)\s+[(]?См\.\s*№?\s*(?<ref>[1-9][0-9]{0,4})\s*[)]?\.?$')
    hascity = split_title_at_city(rec.tail)
    is_the_same = the_same.match(rec.tail)
    is_ref = ref.match(rec.tail)
    if is_the_same:
        # то же
        rec['thesame'] = 'THESAME'
        rec.tail = is_the_same.group('remainder')
        return rec
    elif hascity:
        if verbose:
            print("hascity:", hascity.groupdict())
        rec['maintitle'] = hascity.group('alltitle').strip(' ,—-')
        rec['city'] = format_multi_cities(hascity.group('city'))
        rec['publisher'] = hascity.group('publisher').strip(' ,')
        rec['year'] = hascity.group('year')
        rec.tail = hascity.group('tail')
    elif is_ref:
        rec['maintitle'] = is_ref.group('alltitle').strip(' ,—-')
        rec['ref'] = is_ref.group('ref')
        rec['city'] = 'REF' + rec['ref']
        rec['publisher'] = ''
        rec['year'] = ''
        rec.tail = ''
    else:
        rec['maintitle'] = "NOTITLE"
        rec['city'] = ''
        rec['publisher'] = ''
        rec['year'] = ''
    if 'title' not in rec:
        rec['title'] = rec['maintitle']
    try:
        rec['year'] = int(rec['year'])
    except ValueError:
        rec['year'] = 'NA'
    rec['thesame'] = ''
    return rec


def process_the_same(rec, prev, verbose=False):
    """Process records with type SAME"""
    delimiter = re.compile(r'(?<subtitle>[^/]+)\s*/:?\s*(?<tail>.*)')
    has_delimiter = delimiter.match(rec.tail)
    if has_delimiter:
        rec['subtitle'] = has_delimiter.group('subtitle')
        rec.tail = has_delimiter.group('tail')
    the_same_addon = re.compile(r'((?<addon>[^@]+?)?(\s*—\s*)?(?<year>19[1-8][0-9]|[Бб]\.\s+г\.))?(?<tail>.*)$', re.U | re.VERBOSE)
    has_the_same_addon = the_same_addon.match(rec.tail)
    hascity = split_title_at_city('. '.join(['TITLE', rec.tail.strip()]))
    if hascity:
        rec['city'] = format_multi_cities(hascity.group('city'))
        rec['publisher'] = hascity.group('publisher').strip(' .,')
        rec['year'] = hascity.group('year')
        has_addon = re.match(r'TITLE.\s+(?<addon>.+)$', hascity.group('alltitle'))
        if has_addon:
            rec['editorial'] = has_addon.group('addon').strip(' .,—')
        rec.tail = hascity.group('tail')
    else:
        if has_the_same_addon.group('year'):
            rec['year'] = has_the_same_addon.group('year')
            addon = has_the_same_addon.group('addon')
            if addon:
                rec['editorial'] = addon.strip(' ,.—')
        rec.tail = has_the_same_addon.group('tail')
    # populate current record with fields from previous rec,
    # in case there's no such field already filled
    for k, v in prev.items():
        if k not in rec:
            rec[k] = v
    return rec


def normalize_printrun(pr, part):
    if pr:
        pr = pr.replace(' ', '')
        pr = pr.replace('О', '0')
        pr = pr.replace('о', '0')
        pr = pr.strip()
    # if part:
    #     pr = '%s [%s]' % (pr, part)
        try:
            return int(pr)
        except ValueError:
            print("INVALID PRINTRUN VALUE: ", str(pr))
    return pr


def normalize_price(match):
    rub = match.group('rub') or 0
    kop = match.group('kop') or 0
    try:
        price = float('%d.%d' % (int(rub), int(kop)))
    except ValueError:
        price = 0
    if price > 0:
        return price
    else:
        return None


def extract_printinfo(rec, verbose=False):
    PAGES = r'(С[тг]р\.?\s+(?<pages>[0-9]+)(?<pagecomment>(\s+и)?\s+[0-9]+\s+л[.]\s+(черт|илл))?[,.]|(?<pages>[0-9]+)\s+лист(ов|а)?[.,])'
    PRINTRUN = r'(\s*[Тт][.]\s*(?<printrun>[1-9][0-9 Оо]+)[,.]?(\s*[(]((?<part>[0-9]+[ —-]+[0-9]+)\s+т(ыс)?\.|[1-9].+?завод.?(\s+.+?[0-9]+\s+т\.)?)[)]\.?)?)'
    PRICE = r'(\s*[Цц][.]\s+(?<price>((?<rub>[0-9]+)\s+[рР]\.)(\s*((?<kop>[0-9]+)\s+[кК]\.))?|((?<kop>[0-9]+)\s+[кК]\.)))'
    SERIES = r'(\s*[(](?<series>\p{Lu}[^)]+?)\.? ?[)]\.?\s*)'
    PR_EARLY = r'(' + SERIES + PAGES + PRINTRUN + '?' + PRICE + '?' + '|' + PAGES + SERIES + '?' + PRINTRUN + '?' + PRICE + '?' + '|' + PRINTRUN + PRICE + '?' + '|' + PRICE + ')'
    pr_1918 = re.compile(r'(?<head>.*?)' + PR_EARLY + r'(?<tail>.*)$', re.U)
    has_early = pr_1918.match(rec.tail)
    N_PAGES = r'(?<pages>[0-9]+)\s+([Сс]тр|л|с)[.,](?<pagecomment>((,\s+|\s+и)?.+?(вклейки|иллюстр|чертежей|черт|илл|таблицы|нот|портр|[0-9]+\s+л[.]\s+(ил|портр)|слож\.\s+в\s+[0-9]+\s+с)\.|\s*[(](\p{Ll}|[0-9])[^)]+[)])\.?)?(\s*[—-]+\s*)?'
    N_PRINTRUN = r'(\s*[[]?(?<printrun>[1-9][0-9 Оо]+)[]]?(\s*[(]((?<part>[0-9]+[ —-]+[0-9]+)\s+т(ыс)?\.|[1-9]-й\s+завод\s+(?<part>[0-9]+([ —-]+[0-9]+)?)?(\s+т(ыс)?\.)?)[)]\.?)?\s*[Ээ]кз[.,]?|\s*(?<printrun>[1-9][0-9 Оо]+)\s*[(](?<part>[0-9]+[ —-]+[0-9]+)\s+тыс\.\s+[Ээ]кз[.,]?[)])'
    N_PRICE = r'(\s*(?<price>((?<rub>[0-9]+)\s+[рР]\.)(\s*((?<kop>[0-9]+)\s+[кК][.,]))?|((?<kop>[0-9]+)\s+[кК][.,])|Б/ц\.|Б\.\s+ц\.)),?'
    N_SERIES = r'(\s+[(](?<series>\p{Lu}[^)]+)[)]\.?\s*—?\s*(?<dop>[^—]+—\s*)?|\s+—(?<dop>[^—]+)—\s*)'
    PR_LATE = r'(' + '\s*—?\s*' + N_PAGES + N_SERIES + '?' + '\s*—?\s*' + N_PRICE + '\s*—?\s*' + N_PRINTRUN + '|' + '—?\s*' + N_SERIES + '?' + N_PRICE + '\s*—?\s*' + N_PRINTRUN + '|' + N_PAGES + SERIES + '?' + '(' + PRINTRUN + '|' + N_PRINTRUN + ')?' + N_PRICE + '?' + '|' + '(' + PRINTRUN + '|' + N_PRINTRUN + ')' + N_PRICE + '?' + '|' + N_PRICE + '|' + N_PRINTRUN + ')'
    pr_1946 = re.compile(r'(?<head>.*?)' + PR_LATE + r'(?<tail>.*)$', re.U)
    has_late = pr_1946.match(rec.tail)
    try:
        n_early = len(list(filter(None, has_early.groups())))
    except AttributeError:
        n_early = 0
    try:
        n_late = len(list(filter(None, has_late.groups())))
    except AttributeError:
        n_late = 0
    if n_early > n_late:
        more_early = True
    else:
        more_early = False
    if has_early and more_early:
        if verbose:
            print("has_early:", has_early.groupdict())
        rec['pages'] = has_early.group('pages') or 'NOPAGES'
        rec['printrun'] = normalize_printrun(has_early.group('printrun'), has_early.group('part')) or 'NOPRINTRUN'
        rec['price'] = normalize_price(has_early) or 'NOPRICE'
        if has_early.group('series'):
            series = has_early.group('series').strip('.')
        else:
            series = 'NOSERIES'
        rec['series'] = series
        rec.tail = ' '.join([has_early.group('head'), has_early.group('tail')])
    elif has_late:
        if verbose:
            print("has_late:", has_late.groupdict())
        rec['pages'] = has_late.group('pages') or 'NOPAGES'
        rec['printrun'] = normalize_printrun(has_late.group('printrun'), has_late.group('part')) or 'NOPRINTRUN'
        rec['price'] = normalize_price(has_late) or 'NOPRICE'
        if has_late.group('series'):
            series = has_late.group('series').strip('.')
        else:
            series = 'NOSERIES'
        rec['series'] = series
        rec['bibaddon'] = has_late.group('dop') or ''
        rec.tail = ' '.join([has_late.group('head'), has_late.group('tail')])        
    else:
        rec['pages'] = 'NOPAGES'
        rec['printrun'] = 'NOPRINTRUN'
        rec['price'] = 'NOPRICE'
        rec['series'] = 'NOSERIES'
        rec['bibaddon'] = ''
    return rec


def extract_addressee(rec, verbose=False):
    """Extract info on age of addresse from either tail or title"""
    ADDR_PAREN = r'[({<]?(Для(?<age>[^)]+?(возраста|детей|пионеров|школьников|подростков|маленьких|юношества|кл(асс|[.])(ов|а)?|шк([.]|олы?)))[. ]?|(?<age>(Ст|Ср|Мл|Дошк)[^)]+возр[.,]))(?=[)])'
    paren_tail = re.compile(r'(?<head>.*?)' + ADDR_PAREN + r'(?<tail>.*)$', re.U)
    has_tail_paren = paren_tail.match(rec.tail)
    if has_tail_paren:
        rec['addressee'] = 'для ' + has_tail_paren.group('age').strip()
        rec.tail = ' '.join([has_tail_paren.group('head'), has_tail_paren.group('tail')])
        return rec
    ADDR_SLASH = r'([Дд]ля|(?<= )[Дд]ли(?= ))(?<age>[^/]+)(во[зэ][*-]?\s*ра[-]?с[-]?[тг]|кл[.]|шк([.]|олы?))[^/ ]+?\s*/'
    title_slash = re.compile(r'(?<head>.+?)' + ADDR_SLASH + r'(?<tail>.*)$', re.U)
    has_title_slash = title_slash.match(rec['title'])
    ADDR_GENERAL = r'[Дд]л[яи]((?<age>[^)\p{Lu}]+?)?во[зэ][*-]?\s*ра[-]?с[-]?[тг][ ]?|(?<age>[^)\p{Lu}]*?(юношества|подростков|школьников|детей|шк([.]|олы?)|кл(асс|[.])(ов|а)?)))[^/) ]+?\s*[:]?'
    title_general = re.compile(r'^(?<head>.+?)' + ADDR_GENERAL + r'(?<tail>.*)$', re.U)
    has_title_general = title_general.match(rec['title'])
    has_in_series = title_general.match(rec['series'])
    if has_title_slash:
        rec['addressee'] = 'для ' + has_title_slash.group('age').strip() + ' возраста'
        rec['title'] = ' '.join([has_title_slash.group('head'), '/', has_title_slash.group('tail')])
    elif has_title_general:
        rec['addressee'] = 'для ' + has_title_general.group('age').strip() + ' возраста'
        rec['title'] = ' '.join([has_title_general.group('head'), has_title_general.group('tail')])
    elif has_in_series:
        rec['addressee'] = 'для ' + has_in_series.group('age').strip() + ' возраста'
        rec['series'] = ' '.join([has_in_series.group('head'), has_in_series.group('tail')])
    return rec


def parse_title(rec, verbose=False):
    """Extract genre/subtitle and editorial info from a title string"""
    TITLE = r'(?<maintitle>(\p{Lu}|[«"0-9]||[.][.][.]\s*[\p{Ll}\p{Lu}]).+?)[.?!]?\s*'
    SUBTITLE = r'(([({](?<subtitle>«?\p{Lu}[^)]+[^.])[.]?[)}]\s*|(?<=[.])\s+(?<subtitle>\p{Lu}[^()]+?[^.]))[.]?\s*)'
    ILL = r'((?<editorial>((Рис|Ил-?л(юстр)?[.]?|Обл[.]?|Фотограф|Худ(ож)?н?|Оформл|С\s+фотоилл|Общ)[.,]|Грав([.]|юры)?|Автолитографи[ия]|Линогравюры|Силуэты|Картинки|Оформление|Фотомонтаж|Фотоиллюстрации|С\s+фотоиллюстрациями|Фото|Портрет|Переплет[,]?|Обложка[,]?|Супер-обложка|Художники?|Художественное)\s+.+)\s*)'
    EDITORIAL = r'([(]?(?<editorial>(/:|Ред[.]?(-сост[.])?|Под ред(акц)?(ией)?|Под общ(ей|[.]) ред(акц)?|Редакц(ия)?|Редколлегия:|Науч|Предисловие|С предисл|Со (вступ. )?статьей|Со вступительной|Статья|Пред(исл)?|Примеч([.]|ания)?|Коммент(арии)?|Вступ(ит)?(ельн(ая|ый))?|С приложением|Послесл(овие)?|Сост(авил|авила|авили|авитель|авлено|авление|[.])?[:]?|Обр(аб)?(отка|отал|отала)?|Перев(од)?(ы)?|Перевел(а|и)?|Пер(ераб)?|[Вв] (пер|обработке)|В обраб|Лит|Литобработка|В изложении|В сокращении|В пересказе|Пересказал(а|и)?|Переделка с|Вольный пер(евод)?|Сокращенный пер(евод)?|Собрал|Авториз(ов)?(анный)?|Сокр(ащ)?(eно|ение)?|Редактор-составитель|Пересказ(ал|ала)?|Рассказал(а)?|Записал(а)?|Запись|Подготовка текста|Подгот[.]?|Пояснит|Авт|Слова|Музыка|[(]?Текст|Сюжет|Постановка|Режиссерские|Беседу ведет)[.,]?\s+.+)\s*)'
    ADDON = r'((?<editorial>((::|Вып|Изд|Ч[.,]|Кн|Издание)[.,]?(\s+[^ ]+){1,4}|В\s+[^ ]+\s+вып(усках|[.])))[.]?\s*.*)'
    SOURCE = r'(?<editorial>[(]По\s+[^)]+[)])'
    LANG = r'((?<editorial>На\s+.+?яз[.]?|С\s+.+?словарем)[.,]?\s*.*)'
    TITLE_1VOL = r'(' + TITLE + SUBTITLE + '?' + '(' + EDITORIAL + '|' + ILL + '|' + SOURCE + '|' + ADDON + '|' + LANG + ')?' +  '|' + TITLE + SUBTITLE + '(' + ILL + '|' + EDITORIAL + '|' + ADDON + '|' + LANG + ')?' + ')' + '[ .,:]*$'
    TITLE_GOST = r'((?<maintitle>(\p{Lu}|[«"0-9]|[.][.][.]\s*[\p{Ll}\p{Lu}])[^/:]+?(:\s+\p{Ll}[^/:]+?)?)[.:]?\s*' + '(:[\s.]*' + r'(?<subtitle>([\p{Lu}0-9([«]|в\s+[0-9])[^:/]+?)[.:]?\s*' + ')?/\s*' + '(?<editorial>.*$))|' + '(?<maintitle>(\p{Lu}|[«"0-9]||[.][.][.]\s*[\p{Ll}\p{Lu}])[^/:]+)[.]?\s*' + ':[ .]*' + r'(?<subtitle>([\p{Lu}0-9([«]|в\s+[0-9])[^:/]+)[.]?\s*$' + '|' + '(?<maintitle>(\p{Lu}|[«"0-9]||[.][.][.]\s*[\p{Ll}\p{Lu}])[^/:]+?:[^/:]+?)\s*:[\s.]*(?<subtitle>([\p{Lu}0-9([«]|в\s+[0-9])[^/]+)\s*' + '/\s*(?<editorial>.*$)?'
    try:
        is_gost = int(rec['year']) >= 1972
    except ValueError:
        if '/' in rec['title'] and not re.search('/:', rec['title']):
            is_gost = True
        else:
            is_gost = False
    if is_gost: #  or '/' in rec['title'] and not '/:' in rec['title']
        if rec['title'].count(':') >= 3:
            SPLIT = r'(?<maintitle>[^:]+?)[.]?\s*:\s*(?<subtitle>[^/]+)(/\s*(?<editorial>.*))?$'
            gost_split = re.compile(SPLIT, re.U)
            has_title_gost = gost_split.match(rec['title'])
        else:    
            re_gost = re.compile(TITLE_GOST, re.U)
            has_title_gost = re_gost.match(rec['title'])
        if has_title_gost:
            rec['title'] = has_title_gost.group('maintitle')
            rec['subtitle'] = has_title_gost.group('subtitle')
            rec['editorial'] = has_title_gost.group('editorial') or ''
    else:
        re_1vol = re.compile(TITLE_1VOL, re.U)
        has_title_1vol = re_1vol.match(rec['title'])
        if has_title_1vol:
            rec['title'] = has_title_1vol.group('maintitle')
            rec['subtitle'] = has_title_1vol.group('subtitle')
            rec['editorial'] = has_title_1vol.group('editorial') or ''
        # addon = has_title_1vol.group('addon')
        # if addon:
        #     try:
        #         rec['bibaddon'] = ' :: '.join([rec['bibaddon'], addon])
        #     except KeyError:
        #         rec['bibaddon'] = addon
    rec['title'] = '; '.join(re.split(r'[.]\s*—\s*', rec['title']))
    try:
        rec['subtitle'] = '; '.join(re.split(r'[.]\s*—\s*', rec['subtitle']))
    except (KeyError, TypeError):
        pass
    return rec


def retry_series(rec, verbose=False):
    """Retry to find series in record tail after processing"""
    series_paren = r'^[ —.]*[(](?<series>[«\p{Lu}][^)]+?) ?[)](?<tail>.*)$'
    has_series = re.match(series_paren, rec.tail)
    if has_series:
        if rec['series'] == 'NOSERIES' or rec.isthesame:
            if verbose:
                print('RETRY: %s %s' % (rec['num'], has_series.groupdict()))
            rec['series'] = has_series.group('series').strip('.')
            rec.tail = has_series.group('tail')
    return rec


def extract_contents(rec, verbose=False):
    """Extract the contents of the work, if present"""
    CONTENTS = r'С\s*о\s*д\s*е\s*р\s*ж\s*([.]?\s*:|и|ание:)\s+(?<contents>.+?)[.]?\s*'
    contents_re = re.compile(r'(?<head>.*?)' + '(' + CONTENTS + '(?<tail>Р\s*е\s*ц[.]?:?\s+.*)$' + '|' + CONTENTS + '$' + ')', re.U)
    has_contents = contents_re.match(rec.tail)
    items = []
    if has_contents:
        contents = has_contents.group('contents').strip('.')
        if re.search(r'[.]\s*—\s*|;\s+', contents):
            split_re = r'[.]\s*—\s*|;\s+'
        else:
            split_re = r'[.]\s+'
        for item in re.split(split_re, contents):
            m = re.match(r'(?<title>.+?)([.]?\s+(Рис[,.]?|Инсценировка)\s+|/.+).*$', item, re.U)
            if m:
                item = m.group('title')
            items.append(item)
        rec['contents'] = '; '.join(items)
        try:
            tail = has_contents.group('tail')
            rec.tail = ''.join([has_contents.group('head'), tail])
        except TypeError:
            rec.tail = has_contents.group('head')
    return rec


def parse_arguments():
    parser = argparse.ArgumentParser(description='Split scanned txt file into numbered records (CSV)', epilog=""" The idea is to rely on the sequentially numbered items. The script
identifies all lines that look like a numbered item. All non-itemlike
lines are joined to the previous numbered line, until the next item in
a sequence is encountered. When an expected next item is missing, a
'MISSING' tag is printed in the output CSV file.""")
    parser.add_argument('infile', nargs='?', help='Input file (txt)')
    parser.add_argument('outfile', nargs='?', help='Output file (csv)',
                        type=argparse.FileType('w', encoding='UTF-8'), default=sys.stdout)
    parser.add_argument('-v', '--verbose', help='Show regex debugging output',
                        action='store_true')
    return parser.parse_args()


def main():
    """main processing"""
    args = parse_arguments()
    csv_writer = csv.DictWriter(args.outfile, fieldnames = Record().fields, extrasaction='ignore', lineterminator='\n')
    author = None
    titlerec = None
    csv_writer.writeheader()
    for rec in iter_records(numbered_lines(args.infile)):
        row = extract_author(rec, author, verbose=args.verbose)
        author = row['author']
        row = extract_title(row, verbose=args.verbose)
        row = extract_contents(row, verbose=args.verbose)
        if row.isthesame:
            row = process_the_same(row, titlerec, verbose=args.verbose)
        row = extract_printinfo(row, verbose=args.verbose)
        row = extract_addressee(row, verbose=args.verbose)
        if row.isthesame:
            for k, v in row.items():
                try:
                    if row[k].startswith('NO') and titlerec[k]:
                        row[k] = titlerec[k]
                except AttributeError:
                    pass
        else:
            row = parse_title(row, verbose=args.verbose)
        row = retry_series(row, verbose=args.verbose)
        titlerec = row
        csv_writer.writerow(row.serialize())


if __name__ == '__main__':
    main()
