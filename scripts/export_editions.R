suppressPackageStartupMessages(require(optparse))
require(readr)
require(dplyr)
require(tidyr)
require(stringr)

option_list = list(
  make_option(c("-i", "--infile"), action="store", default=NA, type='character',
              help="input file (all.rec.csv)"),
  make_option(c("-o", "--outfile"), action="store", default=NA, type='character',
              help="output file (editions.csv)"),
  make_option(c("-a", "--authors"), action="store", default=NA, type='character',
              help="authors disamb file (authors_disamb.csv)"),
  make_option(c("-l", "--langs"), action="store", default=NA, type='character',
              help="languages disamb file (lang_disamb.csv)"),
  make_option(c("-s", "--author_langs"), action="store", default=NA, type='character',
              help="languages of the authors logic file (author_langs.csv)"),
  make_option(c("-g", "--genres"), action="store", default=NA, type='character',
              help="genres info file (genres.csv)"),
  make_option(c("-n", "--normalized"), action="store", default=NA, type='character',
              help="normalized fields file (normalized_fields.csv)")
)
opt = parse_args(OptionParser(option_list=option_list))

d <- read_csv(opt$infile, na=c("NA", "NOPRICE", "NOPRINTRUN", "NOPAGES", "NOSERIES"))
a <- read_csv(opt$authors)
g <- read_csv(opt$genres)
nf <- read_csv(opt$normalized)
l <- read_csv(opt$langs)
al <- read_csv(opt$author_langs)

## тест на дубликаты в авторах
cat("Duplicate authors in disamb\n")
a |> group_by(author) |> filter(n()>1) |> pull(author) |> unique()

d.a <- d %>% select(vol, num, author, orig_lang) %>%
    separate_rows(author, sep="; ") %>%
    left_join(select(a, author, author_std=match, author_gender=gender), relationship = "many-to-one") %>%
    mutate(author_std = ifelse(author == "OTHERS", "OTHERS", author_std)) %>%
    left_join(select(al, author_std, deflang=default, enforce)) |>
    left_join(select(l, orig_lang, orig_lang_std=fullname)) |>
    rename(xlang=orig_lang) |>
    mutate(orig_lang = ifelse( (!is.na(deflang)) & orig_lang_std=='NOLANG' | (is.na(orig_lang_std) & enforce=='yes'), deflang, orig_lang_std))

d.l <- d.a |> ungroup() |> arrange(author_std) |> group_by(author_std) |>
    filter(all(unique(orig_lang) == "NOLANG") ) |>
    mutate(n = n(), nolang = sum(orig_lang=='NOLANG'), nas = sum(is.na(orig_lang))) |>
    left_join(select(d, vol, num, title, subtitle, editorial))

## |>
##     filter(!is.na(orig_lang) & !orig_lang=="NOLANG") |>
##     group_by(author_std, n, nolang, nas) |>
##     summarise(langs=paste(unique(orig_lang), collapse=";")) |>
    ## mutate(default=langs)

## тест на несматченных авторов
cat("Nonmatched authors\n")
d.a %>% filter(author!="NOAUTHOR" & is.na(author_std))

a.std <- d.a %>% group_by(vol, num) %>%
    summarise(author_std = paste(author_std, collapse="; "),
              author_gender = paste(author_gender, collapse = "; "),
              orig_lang = paste(unique(orig_lang[!is.na(orig_lang)]), collapse = "; "))


d.out <- d %>% select(-one_of('orig_lang')) %>%
    left_join(a.std) %>%
    relocate(author_std, author_gender, .after = author) %>%
    filter(!str_detect(tail, "MISSING")) %>%
    left_join(select(g, vol, num, genre), relationship = "one-to-one") %>%
    relocate(genre, .after = subtitle) %>%
    select(-one_of(c('publisher', 'series', 'bibaddon', 'addressee'))) %>%
    left_join(select(nf, -one_of(c('author', 'title', 'subtitle'))), by=c('vol', 'num'), relationship="one-to-one") %>%
    select(vol, num, author, author_std, author_gender, title, subtitle, genre, editorial, orig_lang, transformed, volume, city, publisher, year, series, pages, printrun, price, addressee, contents, tail, bibaddon, section, thesame, start, end)


## тест на несматченных авторов
cat("Nonmatched author lists\n")
d.out %>% filter(author != "NOAUTHOR" & is.na(author_std))

## записываем результат
write_csv(d.out, opt$outfile, eol="\n")




