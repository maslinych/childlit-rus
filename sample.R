library(dplyr)
library(readr)
library(stringr)

## build a list of all relevant database segments
## (assuming git is installed and we are in a git repo)
csv.list <- system2("git", c("ls-files", "19[0-9][0-9]-19[0-9][0-9].csv"), stdout=TRUE)

## manually set column types (to check value type compliance)
db.types <- list(author=col_character(),
                 title=col_character(),
                 genre=col_character(),
                 year=col_date(format = "%Y"),
                 printrun=col_integer(),
                 kid=col_logical(),
                 junior=col_logical(),
                 teen=col_logical(),
                 exclude=col_logical(),
                 location=col_character(),
                 sex=col_character())

## read all csv files and bind them together to create db
df.all <- do.call(dplyr::bind_rows, lapply(csv.list, read_csv, col_types=db.types))

## create normalized forms for author/title
df.all <- df.all %>%
    mutate(lastname = str_match(author, "(\\w+)\\b")[,1]) %>%
    mutate(lastname = str_replace_all(lastname, "ё", "е")) %>%
    mutate(ntitle = str_replace_all(title, "ё", "е")) %>%
    mutate(ntitle = str_replace_all(ntitle, " ?[\\p{Pd}] ?", " ")) %>%    
    mutate(ntitle = str_replace_all(ntitle, " ?[\\p{Pd}\\p{Pc}] ?", " ")) %>%
    mutate(ntitle = str_replace_all(ntitle, "[\\p{P}\\{S}]+$", "")) %>%
    mutate(ntitle = str_replace_all(ntitle, "\\b\\p{P}", "")) %>%    mutate(ntitle = str_replace_all(ntitle, "\\p{P}\\b", "")) %>%
    mutate(ntitle = tolower(ntitle))    

## select first editions
df.byauthor <- df.all %>%
    group_by(lastname, ntitle) %>%
    slice(which.min(year)) %>%
    arrange(lastname, title, author, year)

write_csv(df.byaithor, "firstprints-byauthor.csv")

## authors to filter out (of irrelevant period or genre)
skipauthors <- read_lines("filter-authors.csv")

##
df.firstprints <- df.byauthor %>%
    filter(!lastname %in% skipauthors) %>%
    filter(exclude==FALSE | is.na(exclude)) %>%
    arrange(year, lastname, title)

write_csv(df.firstprints, "firstprints-sample.csv")
