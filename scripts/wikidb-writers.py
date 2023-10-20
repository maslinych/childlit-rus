import sys
import time
import re
import argparse


def find_all_occupations(db, cf, wd_list, get_qid=True):
    n = len(wd_list)
    params = [[cf.ATTR_OPTS.AND, "P31", "Q5"]]
    searchlist = list(zip([cf.ATTR_OPTS.OR]*n, [None]*n, wd_list))
    params.extend(searchlist)
    start = time.time()
    wd_ids = db.get_haswbstatements(params, get_qid=get_qid)
    end = time.time() - start
    print(f"Answers: Found {len(wd_ids):,} items in {end:.5f}s")
    return wd_ids


def get_subclasses(db, cf, wd_id, get_qid=True):
    params = [[cf.ATTR_OPTS.AND, "P279", wd_id]]
    return db.get_haswbstatements(params, get_qid=get_qid)


def extract_labels(db, wd_id):
    out = {}
    out['qid'] = wd_id
    out['label'] = db.get_label(wd_id)
    out['rulabel'] = db.get_labels(wd_id, 'ru')
    out['rualiases'] = db.get_aliases(wd_id, 'ru')
    return out


def produce_ru_labels(db, wd_id):
    d = extract_labels(db, wd_id)
    ru = []
    if d['rulabel'] and d['rulabel'] != d['label']:
        ru.append(d['rulabel'])
    if d['rualiases']:
        for alias in d['rualiases']:
            ru.append(alias)
    for rulabel in ru:
        if re.match("[А-Яа-яЁё]+", rulabel):
            yield (d['qid'], d['label'], rulabel)


def extract_year(wd_timestr):
    re_date = r'(?P<era>-)?(?P<year>[0-9]{4})-[0-9]{2}-[0-9]{2}.*$'
    m = re.match(re_date, wd_timestr)
    try:
        d = m.groupdict()
    except AttributeError:
        print(f'Unrecognized date format: {wd_timestr}')
        return wd_timestr
    if d['era']:
        return f"{d['era']}{d['year']}"
    return d['year']


def get_life_year(db, wd_id, pid):
    dates = db._get_ptype_pid(ptype="time", pid=pid, wd_id=wd_id)
    try:
        years = [extract_year(d) for d in dates]
    except TypeError:
        return 'NA'
    years.sort()
    return ';'.join(years)


def parse_arguments():
    parser = argparse.ArgumentParser(description='Get data on writers/authors from wikidata db')
    parser.add_argument('-r', '--dbroot',
                        help='Directory where wikidb code and data are stored')

    parser.add_argument('-q', '--qlist',
                        help='File where to output list of rulabels found')
    parser.add_argument('-m', '--metadata',
                        help='File where to output author metadata')
    parser.add_argument('-b', '--birth_threshold', default=2023,
                        help='Skip authors born on or after a given year')
    return parser.parse_args()


def main():
    start = time.time()
    args = parse_arguments()
    sys.path.append(args.dbroot)
    from core.db_wd import DBWikidata
    import config as cf
    db = DBWikidata()
    # list of all direct subclasses of writer
    writers = get_subclasses(db, cf, "Q36180")
    writers.insert(0, 'Q36180')
    qwriters = find_all_occupations(db, cf, writers)
    mwriters = set()
    with open(args.qlist, 'w') as qfile:
        for writer in qwriters:
            byear = get_life_year(db, "P569", writer)
            if byear < args.birth_threshold:
                for rulabel in produce_ru_labels(db, writer):
                    qfile.write('\t'.join(rulabel))
                    qfile.write('\n')
                    mwriters.add(writer)
    print(f'Total persons left: {len(mwriters)}')
    end = time.time() - start
    print(f"Total time: {end:.2f}s")


if __name__ == '__main__':
    main()
