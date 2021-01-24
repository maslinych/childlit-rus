library(readr)
library(tidyr)
library(reshape2)
library(tidyverse)
library(rebus)
library(igraph)
library(tidytext)
library(optparse)

tidy <- function(data, authors=FALSE) {
  data <- data %>% 
    mutate(volume = str_remove_all(volume, "[,]|[.]")) %>%
    mutate(volume = str_replace_all(volume, "— ", "—")) %>%
    mutate(volume = str_replace_all(volume, " — ", "—"))
  if(authors){
    data <- data %>% 
      mutate(volume = str_replace_all(volume, "—-", "—"))
    comp.ed <- or(START %R% "Сост ",
                  START %R% "Сост. ",
                  START %R% "Сост и обраб. ",
                  START %R% "В обработке ")
    data <- data %>%
      mutate(author = str_remove(author, comp.ed)) %>%
      mutate(author = str_remove(author, "и обраб. ")) %>%
      mutate(author = str_remove(author, " и др."))
    bugs <- unite(as.data.frame(str_extract_all(data$author, pattern = "[[:digit:]]", simplify = TRUE)), col = "Work", sep = "", remove = TRUE)
    data <- data %>%
      mutate(volume = ifelse(is.na(volume) == TRUE, bugs[,1], volume)) %>%
      mutate(author = str_remove_all(author, pattern = "[[:digit:]]"))
  }
  return(data)
}

int_extract <- function(cleaned.data) {
  interval <- one_or_more('[[:digit:]]') %R%
    "—" %R%
    one_or_more('[[:digit:]]')
  intervals <- str_extract_all(cleaned.data$volume, interval, simplify = TRUE)
  return(intervals)
}

int_to_seq <- function(intervals) {
  seq <- data.frame(matrix(NA, nrow = nrow(intervals), ncol = 0))
  for(i in 1:ncol(intervals)){
    col <- intervals[,i]
    col <- as.data.frame(col)
    col <- separate(col, col, into = c("Ed.1", "Ed.x"), sep = "—")
    col <- col %>%
      mutate(Ed.x = as.numeric(as.character(col$Ed.x))) %>% 
      mutate(Ed.1 = as.numeric(as.character(col$Ed.1))) %>%
      mutate(Ed.x = ifelse(is.na(Ed.x), 0, Ed.x)) %>%
      mutate(Ed.1 = ifelse(is.na(Ed.1), 0, Ed.1))
    dataset <- data.frame()
    for(i in 1:nrow(col)){
      r <- col[i,1]:col[i,2]
      r <- t(as.data.frame(r))
      r <- as.data.frame(r)
      r <- unite(r, "Sequence", 1:ncol(r), sep = ", ", remove = TRUE)
      dataset <- rbind(dataset, r)
    }
    row.names(dataset) <- 1:nrow(dataset)
    seq <- cbind(seq, dataset)
  }
  return(seq)
}

int_replace <- function(seq, cleaned.data, content) {
  for(i in 1:ncol(seq)){
    seq[,i] <- ifelse(seq[,i] == "0", "", seq[,i])
  }
  seq.col.naming <- sprintf("Sequence %s", 1:ncol(seq))
  colnames(seq) <- seq.col.naming
  interval <- one_or_more('[[:digit:]]') %R%
    "—" %R%
    one_or_more('[[:digit:]]')
  for(x in 1:ncol(seq)){
    for(i in 1:nrow(cleaned.data)){
      cleaned.data[i,2] <- ifelse(str_detect(cleaned.data[i,2], interval) == TRUE, str_replace(cleaned.data[i,2], interval, seq[i, x]), cleaned.data[i,2])
    }
  }
  if(content == "illustrators"){
    cleaned.data$volume <- str_replace_all(cleaned.data$volume, "[[:punct:]]", "")
    cleaned.col.naming <- sprintf("Work %s", 1:max(str_count(cleaned.data$volume, pattern = " ")+1))
    cleaned.data <- separate(cleaned.data, volume, into = cleaned.col.naming, sep = " ")
    return(cleaned.data)
  }
  if(content == "authors"){
    many.authors <- or("[[:space:]]" %R%
                         "и" %R%
                        "[[:space:]]",
                      "," %R%
                        "[[:space:]]") 
    cleaned.authors.col.naming <- sprintf("Author %s", 1:max(str_count(cleaned.data$author, many.authors)+1))
    cleaned.data <- separate(cleaned.data, author, into = cleaned.authors.col.naming, sep = many.authors)
    cleaned.data$volume <- str_remove_all(cleaned.data$volume, "[[:punct:]]")
    return(cleaned.data)
  }
}

vol_list <- function(intervals.data, content, period) {
  if(content == "authors"){
    intervals.data <- intervals.data %>% 
      pivot_longer(c("Author 1", "Author 2", "Author 3"), 'Author', 4) %>%
      na.omit()
    colnames(intervals.data) <- c("Work", "Status", "Author")
    intervals.data <- intervals.data %>%
      select(Author, Work)
    v3 <- sprintf("Work %s", 1:max(str_count(intervals.data$Work, pattern = " ")+1))
    intervals.data <- separate(intervals.data, Work, into = v3, sep = " ")
    list <- intervals.data %>%
      pivot_longer(!Author, names_to = "Work", values_to = "Index")
  }
  if(content == "illustrators"){
    list <- intervals.data %>%
      pivot_longer(!illustrator, names_to = "Work", values_to = "Index")
    colnames(list) <- c("Illustrator", "Work", "Index")
  }
  list <- na.omit(list)
  occurrences <- list %>%
    count(Index) %>%
    filter(n < 3)
  list <- list %>%
    filter(Index %in% occurrences$Index)
  list <- list %>%
    select(-Work)
  options(scipen = 100)
  write.csv(list, paste(period, content, "vol_list.csv", sep = "_"), row.names = FALSE)
  return(list)
}

edge.list <- function(authors.list, illus.list, period) {
  proto.edge.list <- authors.list %>%
    inner_join(illus.list, by = "Index")
  colnames(proto.edge.list) <- c("Author", "Index", "Illustrator")
  edge.list <- proto.edge.list %>%
    select(-Index)
  write.csv(edge.list, paste(period, "edge_list.csv", sep = "_"))
  return(edge.list)
}

incidence.mat <- function(edge.list) {
  inc.mat <- edge.list %>%
    count(Author, Illustrator) %>%
    cast_sparse(row = Author, column = Illustrator, value = n) %>%
    as.matrix(inc.mat)
  return(inc.mat)
}

adjacency.mat <- function(inc.mat, illustrators = FALSE) {
  if(illustrators){
    inc.mat <- t(inc.mat)
  }
  adj.mat <- inc.mat %*% t(inc.mat)
  return(adj.mat)
}

attribute <- function(edge.list) {
  authors.attr <- edge.list %>%
    select(Author) %>%
    mutate(Status = "Author")
  illus.attr <- edge.list %>%
    select(Illustrator) %>%
    mutate(Status = "Illustrator")
  colnames(authors.attr) <- c('Artist', 'Status')
  colnames(illus.attr) <- c('Artist', 'Status')
    attr <- rbind(authors.attr, illus.attr)
    attr <- unique(attr)
    return(attr)
}

network <- function(matrix, attribute, content) {
  if(content == "a-i"){
    net <- graph_from_incidence_matrix(matrix, directed = FALSE, weighted = TRUE, multiple = TRUE)
    V(net)$Status <- attribute$Status
    V(net)$color <- ifelse(V(net)$Status == "Author", "lightsteelblue3", "firebrick3")
    V(net)$shape <- ifelse(V(net)$Status == "Illustrator", "square", "circle")
  }
  if(content == "a-a"){
    net <- graph_from_adjacency_matrix(matrix, diag = FALSE, mode = "undirected", weighted = TRUE)
    V(net)$color <- "lightsteelblue3"
    V(net)$shape <- 'circle'
  }
  if(content == "i-i"){
    net <- graph_from_adjacency_matrix(matrix, diag = FALSE, mode = "undirected", weighted = TRUE)
    V(net)$color <- "firebrick3"
    V(net)$shape <- 'square'
  }
  return(net)
}

measure <- function(net, bimodal=FALSE, content, period, filename) {
  metrics <- data.frame(Name = V(net)$name, 
                       Degree = graph.strength(net), 
                       Betweeness = betweenness(net),
                       Closeness = closeness(net))
  if(bimodal){
    metrics <- metrics %>%
      mutate(Status = V(net)$Status)
  }
  options(scipen=100)
  write.csv(metrics, paste(period, content, filename, sep = "_"), row.names = FALSE)
  return(metrics)
}

visualize <- function(net, content, period, filename) {
  png(paste(period, content, filename), width = 2000, height = 2000)
  if(content == "general"){
    plot.igraph(net, vertex.size = 4, vertex.label = NA, edge.width = E(net)$weight)
  }
  if(content == "communities"){
    plot(net, vertex.size = 4, edge.width = E(net)$weight, vertex.label = NA)
  }
  if(content == "single community"){
    plot(net, vertex.size = 4, label.cex = graph.strength(net), edge.width = E(net)$weight)
  }
  dev.off()
}

com.detect <- function(net, content, period) {
  set.seed(1234)
  fgcommune <- fastgreedy.community(net, weights = E(net)$weight)
  V(net)$community <- fgcommune$membership
  community <- as.data.frame(V(net)$community)
  nodes <- community %>%
    group_by(community$`V(net)$community`) %>%
    count() %>%
    filter(n >= 10)
  colnames(nodes) <- c("Community", "Members")
  net.c <- delete.vertices(net, !V(net)$community %in% nodes$Community)
  for(i in unique(V(net.c)$community)){
    V(net.c)[V(net.c)$community == i]$color <- rainbow(max(unique(V(net.c)$community)), v = 0.75)[i]
  }
  membership <- data.frame(Name = as.vector(V(net.c)$name), Community = as.vector(V(net.c)$community))
  write.csv(membership, paste(period, content, "metrics.csv", sep = "_"))
  return(net.c)
}

select.com <- function(net, community) {
  net.r <- delete.vertices(net, !V(net)$community %in% community)
  return(net.r)
}

parser <- OptionParser()
parser <- add_option(parser, c("-i", "--inillus"),
                     help="Input illustrators list")
parser <- add_option(parser, c("-a", "--inauthors"),
                     help="Input authors list")
parser <- add_option(parser, c("-o", "--outdir"), 
                     help="Ouptput directory")
parser <- add_option(parser, c("-y", "--years"),
                    help="Publishing years covered in a volume")
args <- parse_args(parser)

main <- function(args) {
  setwd(args$outdir)
  illus <- read_tsv(args$inillus, col_names = c("illustrator", "volume"))
  authors <- read_tsv(args$inauthors, col_names = c('author', 'volume'))
  illus.tidy <- tidy(illus)
  authors.tidy <- tidy(authors, TRUE)
  illus.int <- int_extract(illus.tidy)
  authors.int <- int_extract(authors.tidy)
  illus.seq <- int_to_seq(illus.int)
  authors.seq <- int_to_seq(authors.int)
  illus.wide <- int_replace(illus.seq, illus.tidy, "illustrators")
  authors.wide <- int_replace(authors.seq, authors.tidy, "authors")
  illus.list <- vol_list(illus.wide, "illustrators", args$years)
  authors.list <- vol_list(authors.wide, "authors", args$years)
  el <- edge.list(authors.list, illus.list, args$years)
  inc.mat <- incidence.mat(el)
  adj.mat.a <- adjacency.mat(inc.mat)
  adj.mat.i <- adjacency.mat(inc.mat, TRUE)
  attr <- attribute(el)
  net <- network(inc.mat, attr, "a-i")
  print(paste("The density of a general graph is:", graph.density(net, loops = TRUE), sep = " "))
  net.i <- network(adj.mat.i, content = "i-i")
  print(paste("The density of an illustrators graph is:", graph.density(net.i, loops = TRUE), sep = " "))
  net.a <- network(adj.mat.a, content = "a-a")
  print(paste("The density of an authors graph is:", graph.density(net.a, loops = TRUE), sep = " "))
  metrics <- measure(net, TRUE, "general", args$years, ".csv")
  visualize(net <- net, "general", args$years, ".png")
  net.c <- com.detect(net, "general", args$years)
  visualize(net.c, content = "communities", args$years, ".png")
  for(i in unique(V(net.c)$community)){
    net.r <- select.com(net.c, i)
    visualize(net.r, content = "single community", args$years, filename = paste(paste("#", i, sep = ""), ".png",  sep = "_"))
    metrics.r <- measure(net.r, TRUE, "single_community", args$years, paste(paste("#", i, sep = ""), ".csv", sep = "_"))
  }
  metrics.i <- measure(net.i, FALSE, "illustrators", args$years, ".csv")
  visualize(net.i, content = "general", args$years, paste("illustrators", ".png", sep = ""))
  net.i.c <- com.detect(net.i, "illustrators", args$years)
  visualize(net = net.i.c, content = "communities", args$years, paste("illustrators", ".png", sep = ""))
  for(i in unique(V(net.i.c)$community)){
    net.i.r <- select.com(net.i.c, i)
    visualize(net.i.r, content = "single community", args$years, filename = paste("illustrators", paste("#", i, sep = ""), ".png", sep = "_"))
  }
  metrics.a <- measure(net.a, FALSE, "authors", args$years, ".csv")
  visualize(net.a, content = "general", args$years, paste("authors", ".png", sep = ""))
  net.a.c <- com.detect(net.a, "authors", args$years)
  visualize(net = net.a.c, content = "communities", args$years, paste("authors", ".png", sep = ""))
  for(i in unique(V(net.a.c)$community)){
    net.a.r <- select.com(net.a.c, i)
    visualize(net.a.r, content = "single community", args$years, filename = paste("authors", paste("#", i, sep = ""), ".png", sep = "_"))
  }
  }

main(args)