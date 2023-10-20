import sys
import time
import re
import argparse
import csv


def get_property_values(db, wd_id, pid):
    qids = db._get_ptype_pid(ptype="wikibase-entityid", pid=pid, wd_id=wd_id)
    if qids is None:
        return 'NA'
    try:
        return ';'.join(filter(None, [db.get_label(q) for q in qids]))
    except TypeError:
        print(f'Property retrieval ERROR: {wd_id} — {pid} — {qids}')


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
        years = list(set([extract_year(d) for d in dates]))
    except TypeError:
        return 'NA'
    years.sort()
    return ';'.join(years)


def extract_metadata(db, wd_id):
    d = {}
    d['qid'] = wd_id
    d['label'] = db.get_labels(wd_id, "ru") or db.get_labels(wd_id, "en")
    d['sex'] = get_property_values(db, pid="P21", wd_id=wd_id)
    d['birth_year'] = get_life_year(db, pid="P569", wd_id=wd_id)
    d['death_year'] = get_life_year(db, pid="P570", wd_id=wd_id)
    d['occupations'] = get_property_values(db, pid="P106", wd_id=wd_id)
    d['desc_ru'] = db.get_descriptions(wd_id, 'ru') or ''
    d['desc_en'] = db.get_descriptions(wd_id, 'en') or ''
    d['langs'] = get_property_values(db, pid="P6886", wd_id=wd_id)
    return d
    

def parse_arguments():
    parser = argparse.ArgumentParser(description='Get data on writers/authors from wikidata db')
    parser.add_argument('-r', '--dbroot',
                        help='Directory where wikidb code and data are stored')

    parser.add_argument('-i', '--infile',
                        help='Input tsv file, wikidata qid in the first column')
    parser.add_argument('-m', '--metadata',
                        help='File where to output author metadata')
    return parser.parse_args()


def main():
    start = time.time()
    args = parse_arguments()
    sys.path.append(args.dbroot)
    from core.db_wd import DBWikidata
    db = DBWikidata()
    qids = set()
    with open(args.infile, 'r') as infile:
        with open(args.metadata, 'w') as metafile:
            fieldnames = ['qid', 'label', 'sex', 'birth_year', 'death_year', 'langs', 'desc_ru', 'desc_en', 'occupations']
            dwriter = csv.DictWriter(metafile, fieldnames)
            for line in infile:
                fields = line.strip().split('\t')
                qid = fields[0]
                if qid not in qids:
                    meta = extract_metadata(db, fields[0])
                    dwriter.writerow(meta)
                    qids.add(qid)
    end = time.time() - start
    print(f"Total time: {end:.2f}s")


if __name__ == '__main__':
    main()
