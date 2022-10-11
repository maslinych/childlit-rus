library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)

d <- read_csv("csv/all.rec.csv")

author.str <- d %>% filter(author!="NOAUTHOR") %>%
    separate_rows(author, sep="; +") %>%
    separate_rows(title, sep=" *; +") %>%
    separate_rows(contents, sep=" *; +") %>%
    group_by(author) %>%
    summarise(n=n(), titles = paste(unique(c(title, discard(contents, is.na))), collapse="; ")) %>%
    mutate(prefix1 = str_replace(author, "^([^,]+), (.).*$", "\\1\\2")) %>%
    mutate(prefix2 = str_replace(author, "^([^,]+), (.)[^.]*[.] (.)[^.]*[.].*$", "\\1\\2\\3")) %>%
    mutate(prefix2 = ifelse(prefix2==author, NA, prefix2))


## функция дизамбигуизации авторов
get_best_match <- function(titles, prefix) {
    out <- list()
    candidates <- seq(1:length(prefix))[!is.na(prefix)]
    if (length(candidates) > 0) {
        for (i in seq(1:length(prefix))) {
            if (is.na(prefix[i])) {
                xt <- outer(titles[i], titles[candidates], function(x, y) {mapply(function(a, b){length(intersect(a,b))}, x, y)})
                if (any(xt>0)) {
                    out[i] <- prefix[candidates][which.max(xt)]
                } else {
                    out[i] <- prefix[i]
                } 
            } else {
                out[i] <- prefix[i]
            }
        }
        return(unlist(out))
    } else { # no prefix2 in a group - do nothing
        return(prefix)
    }
}

## неоднозначные авторы
## author.str %>%
##     group_by(prefix1) %>%
##     filter(n_distinct(prefix2, na.rm=TRUE)>1) %>%
##     mutate(titles = str_split(titles, "; ")) %>%
##     mutate(ntit = sapply(titles, length)) %>%
##     mutate(match = get_best_match(titles, prefix2)) %>%
##     mutate(match = ifelse(is.na(match), prefix1, match)) %>%
##     View

## все авторы
disamb <- author.str %>%
    group_by(prefix1) %>%
    mutate(titlist = str_split(titles, "; ")) %>%
    mutate(ntit = sapply(titlist, length)) %>%
    mutate(match = get_best_match(titlist, prefix2)) %>%
    mutate(match = ifelse(is.na(match), prefix1, match)) %>%
    select(author, n, match, titles, ntit, prefix1, prefix2)


write_csv(disamb, "csv/authors_disamb.csv")
