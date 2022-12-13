suppressPackageStartupMessages(require(optparse))
require(readr)
require(dplyr)
require(tidyr)

option_list = list(
  make_option(c("-i", "--infile"), action="store", default=NA, type='character',
              help="input file (all.rec.csv)"),
  make_option(c("-o", "--outfile"), action="store", default=NA, type='character',
              help="output file (editions.csv)"),
  make_option(c("-a", "--authors"), action="store", default=NA, type='character',
              help="authors disamb file (authors_disamb.csv)")
)
opt = parse_args(OptionParser(option_list=option_list))

d <- read_csv(opt$infile, na=c("NA", "NOPRICE", "NOPRINTRUN", "NOPAGES", "NOSERIES"))
a <- read_csv(opt$authors)

d.a <- d %>% select(vol, num, author) %>%
    separate_rows(author, sep="; ") %>%
    left_join(select(a, author, author_std=match)) %>%
    mutate(author_std = ifelse(author == "OTHERS", "OTHERS", author_std))

## тест на несматченных авторов
cat("Nonmatched authors\n")
d.a %>% filter(author!="NOAUTHOR" & is.na(author_std))

a.std <- d.a %>% group_by(vol, num) %>%
    summarise(author_std = paste(author_std, collapse="; "))

d.out <- d %>% left_join(a.std) %>%
    relocate(author_std, .after = author)

## тест на несматченных авторов
cat("Nonmatched author lists\n")
d.out %>% filter(author != "NOAUTHOR" & is.na(author_std))

## записываем результат
write_csv(d.out, opt$outfile)
