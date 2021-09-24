#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import regex as re
import csv
import argparse
import sys
from string import Formatter

AUTHOR_NAME = r"""
(?!Герой\sСоветского\s+Союза)
(?<last>           # Фамилия:
([ДдО]['’]|[Дд]е[ -])?([Лл]а\s+)?\p{Lu}(\p{Ll}+|\p{Lu}+)      # Воронцов / д'Амичис де(-)?
(-\p{Lu}(\p{Ll}+|p{Lu}+)|   # Воронцов-Вельяминов
-(отец|старший|заде|зода|оол|улы|яшь)|   # Дюма-отец Сарыг-оол
\s+\p{Lu}\p{Ll}+(?=,))?# Сервантес Сааведра, Мигель 
)
(\s+\((?<real>     # расшифровка псевдонима:
\p{Lu}(\p{Ll}+|p{Lu}+))\))? #  Петров (Бирюк)
(  # альтернатива — без запятой:
\s+(?<ini>                                                  # инициалы или имена:
\p{Lu}\p{Ll}{0,2}\.[\s-]?\p{Lu}\p{Ll}{0,2}\.([\s-]\p{Lu}\.)?  # Дм. Ив.; Г.-Х. (-A.)?
|\p{Lu}\p{Ll}{0,2}\.(\s+де|,\s+отец)?                                        # А. (де ,отец)?
|\p{Lu}\p{Ll}+\s+\p{Lu}\p{Ll}{3,}(?=(\.|,\s+\p{Lu}\p{Ll}+|\s+и\s+др\.|\s+и\s+\p{Lu}\p{Ll}+|,?\s+\[|\s+\((\p{Lu}\p{Ll}{0,2}\.\s+)+\p{Lu}\p{Ll}+)) # Иван Ильич
|\p{Lu}\p{Ll}+-(Булат|бао|ф[эе]й|и|мин|Фу|Ю|хуа|цзин?|линь|юй|нань|чжень|ян|заде|ань)  # Хас-Булат, Юй-бао и тп.
|\p{Lu}\p{Ll}+\s+\p{Lu}\p{Ll}{0,2}\.              # Фенимор Д.
|\p{Lu}(\p{Ll}+|p{Lu}+)(-\p{Lu}\p{Ll}+)?(\s+де)?(?=(\.|,\s+\p{Lu}\p{Ll}+|\s+и\s+др\.|\s+и\s+\p{Lu}\p{Ll}+|,?\s+\[|\s+\((\p{Lu}\p{Ll}{0,2}\.\s+)+\p{Lu}\p{Ll}+|\s+\(\p{Lu}+\p{Ll}*))) # Василий; Иоганн-Вольфганг; Шарль де
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
Улан-Удэ|
Ульяновск|
Уральск|
Усть-Каменогорск|
Уфа|
Фергана|
Фрунзе|
Хабаровск|
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
Ю(ж?\.|жно)[-\s]Сахалинск|
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
            m = re.match(r"(?<num>[1-9][0-9]*)(-?(?<suffix>[aаб]))?$", string)
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


class Record(dict):
    def __init__(self, tail='', start=0, end=0):
        super(Record, self).__init__()
        self.tail = tail
        self.start = start
        self.end = end
        self.fields = ['start', 'end', 'num', 'author', 'title',
                       'city', 'publisher', 'year', 'series',
                       'pages', 'printrun', 'price', 'bibaddon', 'tail']

    def serialize(self):
        out = {}
        out['start'] = self.start
        out['end'] = self.end
        out['tail'] = self.tail
        out.update(self)
        return out


def extract_number(line):
    """Detect if a line matches a pattern for a numbered bibliography item
    Return a tuple with a number and a text line. If a line doesn't have the
    number return zero and full line as output.
    """
    num = re.match(r'\s*(?<num>[1-9][0-9]*-?[aаб]?)\.\s+(?<tail>.+)', line)
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


def extract_title(rec, prev=None, verbose=False):
    INFO = r'(?<city>(' + CITY + r')(\s?(;|—-?)\s?(' + CITY + r')){0,4})[.,:](?<publisher>.*?)\s+(?<year>19[1-8][0-9]|[Бб]\.\s+г\.|[Бб]/г\.?)[\p{P}\s](\s*—\s*)?(?<tail>.*)$'
    if '@' in rec.tail:
        break_at_city = re.compile(r'(?<alltitle>[^@]+\s+)@\s*' +
                                   INFO, re.U | re.VERBOSE)
    else:
        break_at_city = re.compile(r'(?<alltitle>([\p{Lu}\d«(i№]|\.\.\.).+?[,.)?!—-])\s*'
                   + INFO, re.U | re.VERBOSE )
    the_same = re.compile(r'(?<alltitle>Т\s*о\s*ж\s*е\s*[.,])(?<remainder>((?<addon>[^@]+?)?(\s*—\s*)?(?<year>19[1-8][0-9]|[Бб]\.\s+г\.))?(?<tail>.*))$', re.U | re.VERBOSE)
    ref = re.compile(r'(?<alltitle>.+)\s+[(]?См\.\s*№?\s*(?<ref>[1-9][0-9]{0,4})\s*[)]?\.?$')
    hascity = break_at_city.match(rec.tail)
    is_the_same = the_same.match(rec.tail)
    is_ref = ref.match(rec.tail)
    if is_the_same:
        the_same_hascity = break_at_city.match(is_the_same.group('remainder').strip())
        rec['maintitle'] = prev['maintitle']
        if verbose:
            print("is_the_same", is_the_same.groupdict())
            print("hascity", hascity.groupdict())
        if hascity and is_the_same.group('alltitle') == hascity.group('alltitle'):
            is_breakable = hascity
        else:
            is_breakable = the_same_hascity
        if is_breakable:
            if verbose:
                print("is_breakable:", is_breakable.groupdict())
            rec['city'] = format_multi_cities(is_breakable.group('city'))
            rec['publisher'] = is_breakable.group('publisher').strip(' .,')
            rec['year'] = is_breakable.group('year')
            try:
                rec['titleaddon'] = the_same_hascity.group('alltitle').strip(' ,.')
                rec['title'] = ' : '.join([rec['maintitle'], rec['titleaddon']])
            except AttributeError:
                pass
            rec.tail = is_breakable.group('tail')
        else:
            if verbose:
                print("is_breakable FALSE", is_the_same.groupdict())
            rec['city'] = prev['city']
            rec['publisher'] = prev['publisher']
            if is_the_same.group('year'):
                rec['year'] = is_the_same.group('year')
                addon = is_the_same.group('addon')
                if addon:
                    rec['titleaddon'] = addon.strip(' ,.')
                    rec['title'] = ' : '.join([rec['maintitle'], rec['titleaddon']])
            else:
                rec['year'] = prev['year']
            rec.tail = is_the_same.group('tail')
    elif hascity:
        if verbose:
            print("hascity:", hascity.groupdict())
        rec['maintitle'] = hascity.group('alltitle').strip(' ,.—-')
        rec['city'] = format_multi_cities(hascity.group('city'))
        rec['publisher'] = hascity.group('publisher').strip(' .,')
        rec['year'] = hascity.group('year')
        rec.tail = hascity.group('tail')
    elif is_ref:
        rec['maintitle'] = is_ref.group('alltitle').strip(' ,.—-')
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
    SERIES = r'(\s*[(](?<series>\p{Lu}[^)]+?)\.?[)]\s*)'
    PR_EARLY = r'(' + SERIES + PAGES + PRINTRUN + '?' + PRICE + '?' + '|' + PAGES + SERIES + '?' + PRINTRUN + '?' + PRICE + '?' + '|' + PRINTRUN + PRICE + '?' + '|' + PRICE + ')'
    pr_1918 = re.compile(r'(?<head>.*?)' + PR_EARLY + r'(?<tail>.*)$', re.U)
    has_early = pr_1918.match(rec.tail)
    N_PAGES = r'(?<pages>[0-9]+)\s+([Сс]тр|л|с)[.,](?<pagecomment>((,\s+|\s+и)?.+?(вклейки|иллюстр|чертежей|черт|илл|таблицы|нот|портр|[0-9]+\s+л[.]\s+ил|слож\.\s+в\s+[0-9]+\s+с)\.|\s*[(](\p{Ll}|[0-9])[^)]+[)])\.?)?(\s*[—-]+\s*)?'
    N_PRINTRUN = r'(\s*(?<printrun>[1-9][0-9 Оо]+)(\s*[(]((?<part>[0-9]+[ —-]+[0-9]+)\s+т(ыс)?\.|[1-9]-й\s+завод\s+(?<part>[0-9]+([ —-]+[0-9]+)?)?(\s+т(ыс)?\.)?)[)]\.?)?\s*[Ээ]кз[.,]|\s*(?<printrun>[1-9][0-9 Оо]+)\s*[(](?<part>[0-9]+[ —-]+[0-9]+)\s+тыс\.\s+[Ээ]кз[.,][)])'
    N_PRICE = r'(\s*(?<price>((?<rub>[0-9]+)\s+[рР]\.)(\s*((?<kop>[0-9]+)\s+[кК][.,]))?|((?<kop>[0-9]+)\s+[кК][.,])|Б/ц\.|Б\.\s+ц\.))'
    N_SERIES = r'(\s+[(](?<series>\p{Lu}[^)]+)[)]\.?(\s*—\s*(?<dop>[^—]+—\s*)?)?|\s+—(?<dop>[^—]+)—\s*)'
    PR_LATE = r'(' + N_PAGES + N_SERIES + '?' + N_PRICE + N_PRINTRUN + '|' + '—\s*' + N_SERIES + '?' + N_PRICE + N_PRINTRUN + '|' + N_PAGES + SERIES + '?' + '(' + PRINTRUN + '|' + N_PRINTRUN + ')?' + N_PRICE + '?' + '|' + '(' + PRINTRUN + '|' + N_PRINTRUN + ')' + N_PRICE + '?' + '|' + N_PRICE + '|' + N_PRINTRUN + ')'
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
        rec['series'] = has_early.group('series') or 'NOSERIES'
        rec.tail = ' '.join([has_early.group('head'), has_early.group('tail')])
    elif has_late:
        if verbose:
            print("has_late:", has_late.groupdict())
        rec['pages'] = has_late.group('pages') or 'NOPAGES'
        rec['printrun'] = normalize_printrun(has_late.group('printrun'), has_late.group('part')) or 'NOPRINTRUN'
        rec['price'] = normalize_price(has_late) or 'NOPRICE'
        rec['series'] = has_late.group('series') or 'NOSERIES'
        rec['bibaddon'] = has_late.group('dop') or ''
        rec.tail = ' '.join([has_late.group('head'), has_late.group('tail')])        
    else:
        rec['pages'] = 'NOPAGES'
        rec['printrun'] = 'NOPRINTRUN'
        rec['price'] = 'NOPRICE'
        rec['series'] = 'NOSERIES'
        rec['bibaddon'] = ''
    return rec


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
    csv_writer = csv.DictWriter(args.outfile, fieldnames = Record().fields, extrasaction='ignore')
    author = None
    titlerec = None
    csv_writer.writeheader()
    for rec in iter_records(numbered_lines(args.infile)):
        row = extract_author(rec, author, verbose=args.verbose)
        author = row['author']
        row = extract_title(row, titlerec, verbose=args.verbose)
        titlerec = row
        row = extract_printinfo(row, verbose=args.verbose)
        csv_writer.writerow(row.serialize())


if __name__ == '__main__':
    main()
