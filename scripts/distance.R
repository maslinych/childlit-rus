library(dplyr)
library(optparse)
library(ggplot2)
library(stringr)

cluster_to_node_contribution <- function(nodes) {
  contributions <- data.frame()
  for(i in unique(nodes$community_id)){
    group_nodes <- nodes %>%
      filter(community_id == i)
    group_nodes$name <- str_c(group_nodes$name, "(", group_nodes$status, ")", sep = "")
    all_nodes_weights <- sum(group_nodes$weights)
      for(y in unique(group_nodes$name)){
        contribution <- data.frame()
        contribution <- group_nodes %>%
          filter(name == y)
        contribution <- contribution %>%
          mutate(coefficient = contribution$weights/all_nodes_weights) %>%
          select(name, coefficient, community_id, years)
        contributions <- rbind(contributions, contribution)
    }
  }
  return(contributions)
}


bhattacharyya <- function(curr_comm, prev_comm) {
  nodes_both_comms <- unique(c(c(curr_comm$name), c(prev_comm$name)))
  nodes_both_comms <- data.frame(node = nodes_both_comms)
  
  curr_vs <- c()
  for(v in nodes_both_comms$node){
    if(v %in% curr_comm$name){
      v <- curr_comm$coefficient[curr_comm$name == v]
    } else {
      v <- 0
    }
    curr_vs <- c(curr_vs, v)
  }
  
  prev_vs <- c()
  for(v in nodes_both_comms$node){
    if(v %in% prev_comm$name){
      v <- prev_comm$coefficient[prev_comm$name == v]
    } else {
      v <- 0
    }
    prev_vs <- c(prev_vs, v)
  }
  
  nodes_both_comms <- nodes_both_comms %>%
    mutate(curr_comm_contr = curr_vs) %>%
    mutate(prev_comm_contr = prev_vs) %>%
    mutate(squared = (sqrt(curr_comm_contr) - sqrt(prev_comm_contr))**2)
  r <- 1 - sqrt(sum(nodes_both_comms$squared))/sqrt(2)
  
  return(r)
}


prepare_data <- function(inp, period) {
  crews <- read.csv(paste0(paste0(inp, "/", period,
                                  "/nodes_", period),
                           ".csv"))
  coeffs <- cluster_to_node_contribution(crews)
  
  return(coeffs)
}


leave_comm <- function(contrs, index) {
  contrs %>%
    filter(community_id == index) %>%
    select(name, coefficient)
}


generate_output <- function(dists, outp) {
  colnames(dists) <- c("volume2", "cluster_id2", "volume1", "cluster_id1", "metric") 
  ggplot(dists, aes(x = metric)) + geom_histogram() +
    ggsave(paste("bhattacharyya_distance_distribution.png", sep = ""), device = png(), 
           path = outp, width = 5, height = 5, limitsize = FALSE)
  options(scipen = 100)
  write.csv(dists, paste(outp, "bhattacharyya_distance.csv", sep = "/"), 
            row.names = FALSE)
}

parser <- OptionParser()
parser <- add_option(parser, c("-i", "--indir"),
                     help="Input directory with folders containing nodes files")
parser <- add_option(parser, c("-o", "--outdir"), 
                     help="Ouptput directory where histogram of bhattachryya distance and table with metric are uploaded")
args <- parse_args(parser)

main <- function(args) {
  volumes <- c("1932-1939", "1940-1945", "1946-1948", "1949-1950", "1951-1952", "1953-1954", "1955-1957", "1958-1960", "1961-1963",
               "1964-1966", "1967-1969", "1970-1971", "1972-1973", "1974-1975", "1976-1978", "1979-1981", "1982-1984")
  distances <- data.frame()
  for(n_volume in 1:length(volumes)){
    if(n_volume < length(volumes)){
      curr_period_coeff <- prepare_data(args$indir, volumes[n_volume+1])
      prev_period_coeff <- prepare_data(args$indir, volumes[n_volume])
      for(i in unique(curr_period_coeff$community_id)){
        curr_comm <- leave_comm(curr_period_coeff, i)
        for(j in unique(prev_period_coeff$community_id)){
          prev_comm <- leave_comm(prev_period_coeff, j)
          r <- bhattacharyya(curr_comm, prev_comm)
          distance <- data.frame(volumes[n_volume+1], i, volumes[n_volume], j, r)
          distances <- rbind(distances, distance)
        }
      }
    }
  }
  generate_output(distances, args$outdir)
}

main(args)

