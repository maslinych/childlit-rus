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
  make_option(c("-g", "--genres"), action="store", default=NA, type='character',
              help="genres info file (genres.csv)")
)
opt = parse_args(OptionParser(option_list=option_list))

d <- read_csv(opt$infile, na=c("NA", "NOPRICE", "NOPRINTRUN", "NOPAGES", "NOSERIES"))
a <- read_csv(opt$authors)
g <- read_csv(opt$genres)

## тест на дубликаты в авторах
cat("Duplicate authors in disamb\n")
a |> group_by(author) |> filter(n()>1) |> pull(author) |> unique()

d.a <- d %>% select(vol, num, author) %>%
    separate_rows(author, sep="; ") %>%
    left_join(select(a, author, author_std=match, author_gender=gender), relationship = "many-to-one") %>%
    mutate(author_std = ifelse(author == "OTHERS", "OTHERS", author_std))

## тест на несматченных авторов
cat("Nonmatched authors\n")
d.a %>% filter(author!="NOAUTHOR" & is.na(author_std))

a.std <- d.a %>% group_by(vol, num) %>%
    summarise(author_std = paste(author_std, collapse="; "), author_gender = paste(author_gender, collapse = "; "))

d.out <- d %>% left_join(a.std) %>%
    relocate(author_std, author_gender, .after = author) %>%
    filter(!str_detect(tail, "MISSING")) %>%
    left_join(select(g, vol, num, genre), relationship = "one-to-one") %>%
    relocate(genre, .after = subtitle)

## тест на несматченных авторов
cat("Nonmatched author lists\n")
d.out %>% filter(author != "NOAUTHOR" & is.na(author_std))

## записываем результат
write_csv(d.out, opt$outfile)
