import argparse
import regex as re
import csv
from collections import defaultdict
from itertools import groupby
from functools import reduce


def parse_arguments():
    parser = argparse.ArgumentParser(description='Join wikidata names to Startsev names')
    parser.add_argument('-w', '--wikidata',
                        help='Wikidata persons ru aliases list')
    parser.add_argument('-s', '--startsev',
                        help='authors_disamb.csv')
    parser.add_argument('-o', '--output',
                        help='output match table data')
    return parser.parse_args()


def parse_wd_name(rulabel):
    re_comma = r"""(?P<prep>(д|де|да|аль|ибн|фон)[`'’ -])?(?P<last>\p{Lu}[\p{Ll}\p{Lu} `'’ʼ-]*)(\s+[(](?P<altname>\p{Lu}[\p{Ll} ]+)[)])?,\s*((?P<first>\p{Lu}([.]|\p{Ll}+[.]?)?(-\p{Lu}?\p{Ll}+)*)|(?P<tail>[\p{Ll}1-9].+))((\s+|-)(?P<second>\p{Lu}([.]|\p{Ll}+[.]?))(\s+(?P<third>\p{Lu}\p{Ll}+))?(\s+(?P<tail>.*))?)?"""
    re_space = r"""(?P<first>\p{Lu}([.]|\p{Ll}+[.]?)?(-\p{Lu}?\p{Ll}+)*)((\s+|-)(?P<second>\p{Lu}([.]|\p{Ll}+[.]?))(\s+(?P<third>\p{Lu}\p{Ll}+))?)?\s+(?P<prep>(д|де|да|аль|ибн|фон)[`'’ -])?(?P<last>\p{Lu}[\p{Ll}\p{Lu} `'’ʼ-]*)(\s+[(](?P<tail>\p{Lu}[\p{Ll} ]+)[)])?"""
    re_dot = r"""(?P<first>\p{Lu}\p{Ll}*[.])\s*(?P<second>\p{Lu}\p{Ll}*[.])?\s+(?P<last>\p{Lu}[\p{Ll}\p{Lu} `'’ʼ-]*).*?"""
    d = {'last': rulabel, 'first': None, 'second': None}
    if ',' in rulabel:
        m = re.match(re_comma, rulabel)
        if m:
            d = m.groupdict()
            d['comma'] = True
        else:
            d = {'error': rulabel}
    elif ' ' in rulabel:
        m = re.match(re_space, rulabel)
        if not m and '.' in rulabel:
            m = re.match(re_dot, rulabel)
        if m:
            d = m.groupdict()
        else:
            d = {'error': rulabel}
    return d


def wd_cleanup_inversed(namelist):
    if len(namelist) > 1:
        lastnames = [d['last'] for d in namelist if 'comma' in d]
        if lastnames:
            filtered = []
            for d in namelist:
                if d['first'] not in lastnames and d['second'] not in lastnames:
                    filtered.append(d)
            # filtered = [d for d in namelist if d['first'] not in lastnames and d['second'] not in lastnames]
            return filtered
    return namelist


def make_namekey(d):
    try:
        prep = d['prep'] or ''
    except KeyError:
        prep = ''
    name = f"{prep}{d['last']}"
    name = re.sub(r"[`'’ -]", '', name)
    return name.lower()


def parse_match(match):
    re_name = r"""(?P<prep>(д|де|да|аль|ибн|фон|братья)[`'’-]?)?(?P<last>[\p{Lu}\p{Ll}]\p{Ll}*(-[\p{Lu}\p{Ll}]\p{Ll}+)?)((?P<first>\p{Lu}\p{Ll}*)(?P<second>\p{Lu}\p{Ll}*)?)?"""
    m = re.match(re_name, match)
    if m:
        return m.groupdict()
    else:
        return {'error': match}


def parse_st_name(name):
    re_comma = r"""(?P<prep>(д|де|да|аль|ибн|фон)[`'’ -])?(?P<last>\p{Lu}[\p{Ll}\p{Lu} `'’ʼ-]*)(\s+[(](?P<altname>\p{Lu}[\p{Ll} ]+)[)])?,\s*(?P<first>\p{Lu}([.]|\p{Ll}+[.]?))?((-(?<second>\p{Lu}?([.]|\p{Ll}+[.]?)))?|(?P<tail>[\p{Ll}1-9].+))(\s+(?P<second>\p{Lu}([.]|\p{Ll}+[.]?))(\s+(?P<third>\p{Lu}\p{Ll}+))?(\s+(?P<tail>.*))?)?"""
    if ',' in name:
        m = re.match(re_comma, name)
        if m:
            d = m.groupdict()
        else:
            d = {'error': name}
    else:
        d = {'last': name, 'first': None, 'second': None}
    return d


def xnames(pair):
    a, b = pair
    if b.startswith(a.strip('.')):
        return b
    elif a.startswith(b.strip('.')):
        return a
    else:
        return False


def cmpnames(nlist, pair):
    if not nlist:
        return [pair]
    f, s = pair
    out = []
    hasmatch = False
    for fx, sx in nlist:
        fm = xnames((f, fx))
        sm = xnames((s, sx))
        if fm is not False and sm is not False:
            out.append((fm, sm))
            hasmatch = True
        else:
            out.append((fx, sx))
    if not hasmatch:
        out.append(pair)
    return out


def get_firstnames(named):
    f = named['first'] or ''
    s = named['second'] or ''
    return (f, s)


def get_nametuples(namelist):
    """main function to arrive to a list of different names"""
    return reduce(cmpnames, map(get_firstnames, namelist), [])


def aggregate_names(namelist):
    namelist.sort(key=make_namekey)
    grouped = [list(g) for k, g in groupby(namelist, make_namekey)]
    for group in grouped:
        nametups = get_nametuples(group)
        fsd = defaultdict(dict)
        for d in group:
            f, s = get_firstnames(d)
            fsd[f][s] = d
        for f, s in nametups:
            try:
                yield fsd[f][s]
            except KeyError:
                # get the only or a random item if there's more than one
                k, v = list(fsd[f].items())[0]
                v['second'] = s
                yield v


def compare_names(aname, bname):
    nmatch = re.match(aname, bname)
    if not nmatch and len(bname) < len(aname):
        nmatch = re.match(bname, aname)
    return nmatch


def compare_namedicts(adict, bdict):
    ad = defaultdict(str)
    ad.update(adict)
    bd = defaultdict(str)
    bd.update(bdict)
    if ad['first'] and bd['first']:
        afirst = ad['first'].strip('.')
        bfirst = bd['first'].strip('.')
        firstmatch = compare_names(afirst, bfirst)
    else:
        firstmatch = True
    if ad['second'] and bd['second']:
        asecond = ad['second'].strip('.')
        bsecond = bd['second'].strip('.')
        secondmatch = compare_names(asecond, bsecond)
    else:
        secondmatch = True
    return bool(firstmatch and secondmatch)


def print_name(named):
    d = defaultdict(str)
    d.update(named)
    for k, v in d.items():
        if v is None:
            d[k] = ''
    return f"{d['prep']}{d['last']}, {d['first']} {d['second']} {d['tail']}"


def find_qid_matches(startsev, wd):
    matched = defaultdict(lambda: defaultdict(list))
    for key in startsev:
        if key in wd:
            for stname, match in startsev[key]:
                for wdname, qid in wd[key]:
                    joined = list(aggregate_names([stname, wdname]))
                    if len(joined) == 1:
                        matched[match][qid].append((print_name(stname), print_name(wdname)))
                        #print('MATCH', match, qid, print_name(stname), print_name(wdname))
    return matched


def main():
    args = parse_arguments()
    # process startsev
    disamb = defaultdict(list)
    matches = set()
    with open(args.startsev, newline='') as stfile:
        reader = csv.DictReader(stfile)
        for row in reader:
            if row['match'] not in matches:
                md = parse_match(row['match'])
                if 'error' in md:
                    print(f"Error parsing match: {md['error']}")
                else:
                    disamb[row['match']].append(md)
                    matches.add(row['match'])
            nd = parse_st_name(row['author'])
            if 'error' in nd:
                print(f'Error parsing name: {nd["error"]}')
            else:
                disamb[row['match']].append(nd)
    startsev = defaultdict(list)
    for match in disamb:
        diffnames = aggregate_names(disamb[match])
        for name in diffnames:
            key = make_namekey(name)
            startsev[key].append((name, match))
            #print(f'DISAMB {match}\t{print_name(name)}')
    ## process wikidata
    wd_fieldnames = ['qid', 'en', 'rulabel']
    qids = defaultdict(list)
    with open(args.wikidata, newline='') as wdfile:
        reader = csv.DictReader(wdfile, delimiter='\t', fieldnames=wd_fieldnames)
        for row in reader:
            qid = row['qid']
            parsed = parse_wd_name(row['rulabel'])
            if 'error' in parsed:
                pass
                #print(f'Error parsing wd name: {parsed["error"]}')
            else:
                qids[qid].append(parsed)
    wd_persons = defaultdict(list)
    for qid in qids:
        clean = wd_cleanup_inversed(qids[qid])
        diffnames = aggregate_names(clean)
        for name in diffnames:
            key = make_namekey(name)
            wd_persons[key].append((name, qid))
            ##print(f'{qid}\t{print_name(name)}')
    ## now join them
    matched = find_qid_matches(startsev, wd_persons)
    with open(args.output, 'w') as ofile:
        ofile.write('author_std\tqid\tstname\twdname\n')
        for m, qids in matched.items():
            for qid, namelist in qids.items():
                stnames, wdnames = zip(*namelist)
                ofile.write(f'{m}\t{qid}\t{";".join(stnames)}\t{";".join(wdnames)}\n')
    # for match, qid in find_qid_matches(disamb, wd_persons):
    #     print(f"MATCH:\t{match}\t{qid}")


if __name__ == '__main__':
    main()
