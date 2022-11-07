library(readr)
library(dplyr)
library(tidyr)
library(stringr)

d <- read_csv("csv/all.rec.csv", na=c("NA", "NOPRICE", "NOPRINTRUN", "NOPAGES"))
disamb <- read_csv("csv/authors_disamb.csv")

d.joined <- d %>% separate_rows(author, sep="; ") %>%
    left_join(select(disamb, author, match))
    
write_csv(d.joined, "csv/authors_joined.rec.csv")

## Общий тираж по авторам за все время (дизамбигуизированный)
printruns <- d.joined %>%
    group_by(match) %>%
    summarise(тираж=sum(printrun, na.rm=TRUE)) %>%
    arrange(desc(тираж))

## красивый топ
printruns %>% filter(!is.na(match)) %>% mutate(млн= тираж/1e6) %>% select(автор=match, млн)

## общее количество изданий за все время
printings <- d.joined %>%
    count(match, sort=TRUE)
