suppressPackageStartupMessages(require(optparse))
require(readr)
require(dplyr)

option_list = list(
  make_option(c("-i", "--infile"), action="store", default=NA, type='character',
              help="input file (authors_joined.rec.csv)"),
  make_option(c("-o", "--outfile"), action="store", default=NA, type='character',
              help="output file (editions.csv)")
)
opt = parse_args(OptionParser(option_list=option_list))

d <- read_csv(opt$infile)

## переименовать и переставить колонку со стандартизованным именем автора
d.out <- d %>% rename(author_std = match) %>%
    relocate(author_std, .after = author)

write_csv(d.out, opt$outfile)
