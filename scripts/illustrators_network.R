library(readr)
library(tidyr)
library(dplyr)
library(stringr)
library(rebus)
library(igraph)
library(optparse) 
library(gsubfn)
library(tidytext)
correct_ranges = function(data) {
  data = data %>% 
    mutate(volume = str_replace_all(volume, "(?<=\\d{1,4})-(?=[абвгдеёжзийклмнопрстуфхцчшщэюяыьъ])", "")) %>%
    mutate(volume = str_replace_all(volume, "(\\s*—-?\\s*|(?<=\\d{1,4})-(?=\\d{1,4}))", "—")) %>%
    mutate(volume = str_replace_all(volume, "[/.?*,\\-']", " ")) %>%
    mutate(volume = str_squish(volume))
  first_pattern_ranges = "(?<!\\d)\\d{3}[абвгдеёжзийклмнопрстуфхцчшщэюяыьъ]?—\\d{2}(?!\\d)[абвгдеёжзийклмнопрстуфхцчшщэюяыьъ]?"
  second_pattern_ranges = "(?<!\\d)\\d{4}[абвгдеёжзийклмнопрстуфхцчшщэюяыьъ]?—\\d{2}(?!\\d)[абвгдеёжзийклмнопрстуфхцчшщэюяыьъ]?"
  third_pattern_ranges = "(?<!\\d)\\d{4}[абвгдеёжзийклмнопрстуфхцчшщэюяыьъ]?—\\d{1}(?!\\d)[абвгдеёжзийклмнопрстуфхцчшщэюяыьъ]?"
  fourth_pattern_ranges = "(?<!\\d)\\d{3}[абвгдеёжзийклмнопрстуфхцчшщэюяыьъ]?—\\d{1}(?!\\d)[абвгдеёжзийклмнопрстуфхцчшщэюяыьъ]?"
  fifth_pattern_ranges = "(?<!\\d)\\d{2}[абвгдеёжзийклмнопрстуфхцчшщэюяыьъ]?—\\d{1}(?!\\d)[абвгдеёжзийклмнопрстуфхцчшщэюяыьъ]?"
  sixth_pattern_ranges = "(?<!\\d)\\d{4}[абвгдеёжзийклмнопрстуфхцчшщэюяыьъ]?—\\d{3}(?!\\d)[абвгдеёжзийклмнопрстуфхцчшщэюяыьъ]?"
  #patterns_ranges = c(first_pattern_ranges, second_pattern_ranges, third_pattern_ranges, fourth_pattern_ranges, 
  #                    fifth_pattern_ranges, sixth_pattern_ranges)
  #addition_patterns = c('^\\d', '^\\d{2}', '^\\d{3}', '^\\d{2}', '^\\d{1}', '^\\d{1}')
  #for(i in patterns_ranges, x in addition_patterns){
  if(sum(str_detect(data$volume, first_pattern_ranges)) != 0){
    ranges = str_extract_all(data$volume, first_pattern_ranges, simplify = TRUE)
    for(i in 1:ncol(ranges)){
      col_range = ranges[,i]
      col_range = as.data.frame(str_split(col_range, "—", n = 2, simplify = TRUE))
      col_range = col_range %>%
        mutate(omitted_number = str_extract(col_range$V1, '^\\d')) %>%
        mutate(V2 = str_c(omitted_number, V2, sep = "")) %>%
        select(-omitted_number) %>%
        unite(range, V1, V2, sep = "—", remove = TRUE) %>%
        mutate(range = ifelse(range == "—NA", "", range))
      data = data %>%
        mutate(volume = ifelse(str_detect(volume, first_pattern_ranges), str_replace(volume, 
                                                                                     first_pattern_ranges, 
                                                                                     col_range$range), volume))
    }
  }
  if(sum(str_detect(data$volume, second_pattern_ranges)) != 0){
    ranges = str_extract_all(data$volume, second_pattern_ranges, simplify = TRUE)
    for(i in 1:ncol(ranges)){
      col_range = ranges[,i]
      col_range = as.data.frame(str_split(col_range, "—", n = 2, simplify = TRUE))
      col_range = col_range %>%
        mutate(omitted_number = str_extract(col_range$V1, '^\\d{2}')) %>%
        mutate(V2 = str_c(omitted_number, V2, sep = "")) %>%
        select(-omitted_number) %>%
        unite(range, V1, V2, sep = "—", remove = TRUE) %>%
        mutate(range = ifelse(range == "—NA", "", range))
      data = data %>%
        mutate(volume = ifelse(str_detect(volume, second_pattern_ranges), str_replace(volume, 
                                                                                      second_pattern_ranges, 
                                                                                      col_range$range), volume))
    }
  }
  if(sum(str_detect(data$volume, third_pattern_ranges)) != 0){
    ranges = str_extract_all(data$volume, third_pattern_ranges, simplify = TRUE)
    for(i in 1:ncol(ranges)){
      col_range = ranges[,i]
      col_range = as.data.frame(str_split(col_range, "—", n = 2, simplify = TRUE))
      col_range = col_range %>%
        mutate(omitted_number = str_extract(col_range$V1, '^\\d{3}')) %>%
        mutate(V2 = str_c(omitted_number, V2, sep = "")) %>%
        select(-omitted_number) %>%
        unite(range, V1, V2, sep = "—", remove = TRUE) %>%
        mutate(range = ifelse(range == "—NA", "", range))
      data = data %>%
        mutate(volume = ifelse(str_detect(volume, third_pattern_ranges), str_replace(volume, 
                                                                                     third_pattern_ranges, 
                                                                                     col_range$range), volume))
    }
  }
  if(sum(str_detect(data$volume, fourth_pattern_ranges)) != 0){
    ranges = str_extract_all(data$volume, fourth_pattern_ranges, simplify = TRUE)
    for(i in 1:ncol(ranges)){
      col_range = ranges[,i]
      col_range = as.data.frame(str_split(col_range, "—", n = 2, simplify = TRUE))
      col_range = col_range %>%
        mutate(omitted_number = str_extract(col_range$V1, '^\\d{2}')) %>%
        mutate(V2 = str_c(omitted_number, V2, sep = "")) %>%
        select(-omitted_number) %>%
        unite(range, V1, V2, sep = "—", remove = TRUE) %>%
        mutate(range = ifelse(range == "—NA", "", range))
      data = data %>%
        mutate(volume = ifelse(str_detect(volume, fourth_pattern_ranges), str_replace(volume, 
                                                                                      fourth_pattern_ranges, 
                                                                                      col_range$range), volume))
    }
  }
  if(sum(str_detect(data$volume, fifth_pattern_ranges)) != 0){
    ranges = str_extract_all(data$volume, fifth_pattern_ranges, simplify = TRUE)
    for(i in 1:ncol(ranges)){
      col_range = ranges[,i]
      col_range = as.data.frame(str_split(col_range, "—", n = 2, simplify = TRUE))
      col_range = col_range %>%
        mutate(omitted_number = str_extract(col_range$V1, '^\\d{1}')) %>%
        mutate(V2 = str_c(omitted_number, V2, sep = "")) %>%
        select(-omitted_number) %>%
        unite(range, V1, V2, sep = "—", remove = TRUE) %>%
        mutate(range = ifelse(range == "—NA", "", range))
      data = data %>%
        mutate(volume = ifelse(str_detect(volume, fifth_pattern_ranges), str_replace(volume, 
                                                                                     fifth_pattern_ranges, 
                                                                                     col_range$range), volume))
    }
  }
  if(sum(str_detect(data$volume, sixth_pattern_ranges)) != 0){
    ranges = str_extract_all(data$volume, sixth_pattern_ranges, simplify = TRUE)
    for(i in 1:ncol(ranges)){
      col_range = ranges[,i]
      col_range = as.data.frame(str_split(col_range, "—", n = 2, simplify = TRUE))
      col_range = col_range %>%
        mutate(omitted_number = str_extract(col_range$V1, '^\\d{1}')) %>%
        mutate(V2 = str_c(omitted_number, V2, sep = "")) %>%
        select(-omitted_number) %>%
        unite(range, V1, V2, sep = "—", remove = TRUE) %>%
        mutate(range = ifelse(range == "—NA", "", range))
      data = data %>%
        mutate(volume = ifelse(str_detect(volume, sixth_pattern_ranges), str_replace(volume, 
                                                                                     sixth_pattern_ranges, 
                                                                                     col_range$range), volume))
    }
  }
  #}
  return(data)
}

check_for_letters <- function(data) {
  first_pattern <- "\\d+[абвгдеёжзийклмнопрстуфхцчшщэюяыьъ]—"
  second_pattern <- "—\\d+[абвгдеёжзийклмнопрстуфхцчшщэюяыьъ]"
  if(sum(str_detect(data$volume, first_pattern)) != 0){
    first_pattern_indexes <- as.data.frame(str_extract_all(
      data$volume, first_pattern, simplify = TRUE))
    colnames(first_pattern_indexes) <- c("letters")
    first_pattern_indexes <- first_pattern_indexes %>%
      mutate(numbers = str_replace_all(first_pattern_indexes$letters, 
                                       "[абвгдеёжзийклмнопрстуфхцчшщэюяыьъ]", 
                                       "")) %>%
      mutate(numbers = str_replace(numbers, "—", "")) %>%
      mutate(numbers = as.numeric(as.character(numbers))+1) %>%
      mutate(range = "—") %>%
      mutate(numbers = str_c(numbers, range)) %>%
      mutate(letters = str_replace_all(letters, "—", ", ")) %>%
      unite(indexes, letters, numbers, sep = "", remove = TRUE)
    data <- data %>%
      mutate(volume = ifelse(str_detect(data$volume, first_pattern), 
                             str_replace(data$volume, first_pattern, 
                                         first_pattern_indexes$indexes), 
                             data$volume))
  }
  if(sum(str_detect(data$volume, second_pattern)) != 0){
    second_pattern_indexes = as.data.frame(str_extract_all(
      data$volume, second_pattern, simplify = TRUE))
    colnames(second_pattern_indexes) = c("letters")
    second_pattern_indexes = second_pattern_indexes %>%
      mutate(numbers = str_extract_all(second_pattern_indexes$letters, "—\\d+", 
                                       simplify = TRUE)) %>%
      mutate(letters = str_replace_all(letters, "—", ", ")) %>%
      unite(indexes, numbers, letters, sep = "", remove = TRUE)
    data <- data %>%
      mutate(volume = ifelse(str_detect(data$volume, second_pattern), 
                             str_replace(data$volume, second_pattern, 
                                         second_pattern_indexes$indexes), 
                             data$volume))
  }
  return(data)
}

int_to_seq <- function(interval) {
  ss <- str_split(interval, "—")[[1]]
  return(paste(seq(ss[1], ss[2]), collapse=" "))
}

clean <- function(data, content) {
  if(content == "illustrators"){
    data <- data %>%
      mutate(illustrator = str_replace_all(illustrator, "\\s+и\\s+", ", "))
  }
  if(content == "authors"){
    comp.ed <- or(START %R% "Сост ",
                  START %R% "Сост. ",
                  START %R% "Сост и обраб. ",
                  START %R% "В обработке ")
    data <- data %>% 
      mutate(author = str_remove(author, comp.ed)) %>%
      mutate(author = str_remove(author, "и обраб.\\s*")) %>%
      mutate(author = str_remove(author, "\\s+и\\s+др.")) %>%
      mutate(author = str_replace_all(author, "\\s+и\\s+", ", "))
  }
  return(data)
}

prepare_data <- function(data, content) {
  data <- data %>%
    mutate(volume = gsubfn("\\d+—\\d+", int_to_seq, volume))
  data <- clean(data, content=content)
  return(data)
}

to_long_format <- function(data, content) {
  out <- data %>%
    separate_rows(volume, sep = "\\s")
  if (content == "authors") {
    out <- out %>%
      separate_rows(author, sep = "\\s*,\\s*")
  }
  if (content == "illustrators"){
    out <- out %>%
      separate_rows(illustrator, sep = "\\s*,\\s*")
  }
  return(out)
}

filter_data <- function(data) {
  data %>%
    na.omit %>% 
    group_by(volume) %>%
    filter(n() < 3) %>%
    ungroup()
}

preprocess <- function(data, composition) {
  data %>%
    correct_ranges %>%
    check_for_letters %>%
    prepare_data(composition) %>%
    to_long_format(composition) %>%
    filter_data
}

edge.list <- function(authors.list, illus.list, period, filepath) {
  proto.edge.list <- authors.list %>%
    inner_join(illus.list, by = "volume")
  edge.list <- proto.edge.list %>%
    select(author, illustrator)
  write.csv(edge.list, paste0(filepath, "/", paste(period, "edge_list.csv", sep = "_")), row.names = FALSE)
  return(edge.list)
}

incidence.mat <- function(edge.list) {
  inc.mat <- edge.list %>%
    count(author, illustrator) %>%
    cast_sparse(row = author, column = illustrator, value = n) %>%
    as.matrix()
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
    select(author) %>%
    mutate(status = "Author")
  illus.attr <- edge.list %>%
    select(illustrator) %>%
    mutate(status = "Illustrator")
  colnames(authors.attr) <- c('artist', 'status')
  colnames(illus.attr) <- c('artist', 'status')
    attr <- rbind(authors.attr, illus.attr)
    attr <- unique(attr)
    return(attr)
}

network <- function(matrix, attribute, content) {
  if(content == "a-i"){
    net <- graph_from_incidence_matrix(matrix, directed = FALSE, weighted = TRUE, multiple = TRUE)
    V(net)$status <- attribute$status
    V(net)$color <- ifelse(V(net)$status == "Author", "lightsteelblue3", "firebrick3")
    V(net)$shape <- ifelse(V(net)$status == "Illustrator", "square", "circle")
    V(net)$label_color <- ifelse(V(net)$status == "Author", "darkred", "black")
  }
  if(content == "a-a"){
    net <- graph_from_adjacency_matrix(matrix, diag = FALSE, mode = "undirected", weighted = TRUE)
    V(net)$color <- "lightsteelblue3"
    V(net)$shape <- 'circle'
    V(net)$label_color <- "darkred"
  }
  if(content == "i-i"){
    net <- graph_from_adjacency_matrix(matrix, diag = FALSE, mode = "undirected", weighted = TRUE)
    V(net)$color <- "firebrick3"
    V(net)$shape <- 'square'
    V(net)$label_color <- "black"
  }
  return(net)
}

measure <- function(net, bimodal=FALSE, content, period, filepath) {
  metrics <- data.frame(period = period, Name = V(net)$name, 
                       Degree = graph.strength(net), 
                       Betweeness = betweenness(net),
                       Closeness = closeness(net))
  if(bimodal){
    metrics <- metrics %>%
      mutate(status = V(net)$status)
  }
  options(scipen=100)
  write.csv(metrics, paste0(filepath, "/", paste(period, content, sep = "_"), ".csv"), row.names = FALSE)
  return(metrics)
}

visualize <- function(net, content, period, filename, filepath) {
  png(paste0(filepath, "/", paste(period, content, filename, sep = "_")), width = 2000, height = 2000)
  if(content == "general"){
    plot.igraph(net, vertex.size = 4, vertex.label = NA, edge.width = E(net)$weight)
  }
  if(content == "communities"){
    plot(net, vertex.size = 4, edge.width = E(net)$weight, vertex.label = NA)
  }
  if(content == "single community"){
    plot(net, vertex.size = 4, edge.width = E(net)$weight, vertex.label.color=V(net)$label_color, vertex.label.family="Georgia", 
         vertex.label.cex = log(graph.strength(net)))
  }
  dev.off()
}

com.detect <- function(net, content, period, filepath) {
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
    V(net.c)[V(net.c)$community == i]$color <- rainbow(max(unique(V(net.c)$community)), 
                                                       v = 0.55, alpha = 0.4)[i]
  }
  membership <- data.frame(Name = as.vector(V(net.c)$name), 
                           Community = as.vector(V(net.c)$community))
  write.csv(membership, paste0(filepath, "/", paste(period, content, "membership.csv", sep = "_")), 
            row.names = FALSE)
  return(net.c)
}

select.com <- function(net, community) {
  net.r <- delete.vertices(net, !V(net)$community %in% community)
  return(net.r)
}

weight_communities_table <- function(net, years, filepath) {
  crews <- data.frame()
  for(i in unique(V(net)$community)){
    net_reduced <- subgraph(net, which(V(net)$community == i))
    crew <- graph.strength(net_reduced)
    crew <- as.data.frame(crew)
    crew$name <- V(net_reduced)$name
    row.names(crew) <- 1:nrow(crew)
    crew$status <- V(net_reduced)$status
    crew$community_id <- i
    crew$years <- years
    colnames(crew) <- c("weights", "name", "status", "community_id", "years")
    crew <- crew %>%
      select(name, status, weights, community_id, years)
    crews <- rbind(crews, crew) 
  }
  write.csv(crews, paste(filepath, "/", paste("nodes", years, sep = "_"), ".csv", sep = ""), 
            row.names = FALSE)
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
  print("preprocessing input files")
  illus.list <- read_tsv(args$inillus, col_names = c("illustrator", "volume"))
  illus.list <- preprocess(illus.list, "illustrators")
  authors.list <- read_tsv(args$inauthors, col_names = c("author", "volume"))
  authors.list <- preprocess(authors.list, "authors")
  print("creating edge list")
  el <- edge.list(authors.list, illus.list, period = args$years, filepath = args$outdir)
  print("constructing incidence matrix")
  inc.mat <- incidence.mat(el)
  print("constructing adjacency matrices")
  adj.mat.a <- adjacency.mat(inc.mat)
  adj.mat.i <- adjacency.mat(inc.mat, TRUE)
  print("retrieving attributes")
  attr <- attribute(el)
  print("constructing networks")
  net <- network(inc.mat, attr, "a-i")
  net.i <- network(adj.mat.i, content = "i-i")
  net.a <- network(adj.mat.a, content = "a-a")
  print("computing graph density")
  write.csv(data.frame(general = graph.density(net, loops = FALSE), illustrators = graph.density(net.i, loops = FALSE), authors = graph.density(net.a, loops = FALSE)), paste0(args$outdir, "/",paste(args$years, "density.csv", sep = "_")))
  print("calculating metrics")
  metrics <- measure(net, TRUE, "general", args$years, filepath = args$outdir)
  print("drawing visualizations")
  visualize(net <- net, "general", args$years, ".png", filepath = args$outdir)
  print("detecting communities")
  net.c <- com.detect(net, content = "general", period = args$years, filepath = args$outdir)
  print("generating tables for the-cluster-to-node contributions calculation")
  weight_communities_table(net.c, args$years, filepath = args$outdir)
  print("visualizing communities")
  visualize(net.c, content = "communities", args$years, ".png", filepath = args$outdir)
  print("visualizing every community and calculating centrality metrics in every community")
  for(i in unique(V(net.c)$community)){
    net.r <- select.com(net.c, i)
    visualize(net.r, content = paste0("single_community", i), args$years, filename = ".png", filepath = args$outdir)
    metrics.r <- measure(net.r, TRUE, content = paste0("single_community", i), args$years, filepath = args$outdir)
 }
  print("calculating centrality metrics in illustrators network")
  metrics.i <- measure(net.i, FALSE, "illustrators", args$years, filepath = args$outdir)
  print("visualizing illustrators network")
  visualize(net.i, content = "general", args$years, paste("illustrators", ".png", sep = ""), filepath = args$outdir)
  print("detecting communities of illustrators")
  net.i.c <- com.detect(net.i, "illustrators", args$years, filepath = args$outdir) 
  print("visualizing communities of illustrators")
  visualize(net = net.i.c, content = "communities", args$years, paste("illustrators", ".png", sep = ""), filepath = args$outdir)
  print("visualizing every community of illustrators and calculating centrality metrics in every community of illustrators")
  for(i in unique(V(net.i.c)$community)){
    net.i.r <- select.com(net.i.c, i)
    visualize(net.i.r, content = "single community", args$years, filename = paste("illustrators", paste(i, sep = ""), ".png", sep = "_"), filepath = args$outdir)
  }
  print("calculating centrality metrics in authors network")
  metrics.a <- measure(net.a, FALSE, "authors", args$years, filepath = args$outdir)
  print("visualizing authors network")
  visualize(net.a, content = "general", args$years, paste("authors", ".png", sep = ""), filepath = args$outdir)
  print("detecting communities of authors")
  net.a.c <- com.detect(net.a, "authors", args$years, filepath = args$outdir)
  print("visualizing communities of authors")
  visualize(net = net.a.c, content = "communities", args$years, paste("authors", ".png", sep = ""), filepath = args$outdir)
  print("visualizing every community of authors and calculating centrality metrics in every community of authors")
  for(i in unique(V(net.a.c)$community)){
    net.a.r <- select.com(net.a.c, i)
    visualize(net.a.r, content = "single community", args$years, filename = paste("authors", paste(i, sep = ""), ".png", sep = "_"), filepath = args$outdir)
  }
  }

main(args)
