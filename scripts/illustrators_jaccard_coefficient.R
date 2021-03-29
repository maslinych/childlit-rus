library(igraph)
library(dplyr)
library(optparse)
library(readr)
library(ggplot2)
library(stringr)

vertices_list <- function(network, cluster_id) {
  cluster <- network %>%
    filter(community_id == cluster_id)
  cluster$source_node <- str_c(cluster$source_node< "(author)", sep="")
  cluster$target_node <- str_c(cluster$target_node, "(illustrator)", sep="")
  nodes_list <- unique(c(c(cluster$source_node), c(cluster$target_node)))
  return(nodes_list)
}

parser <- OptionParser()
parser <- add_option(parser, c("-i", "--indir"),
                     help="directory that contains folders by each volume with the data on edges")
parser <- add_option(parser, c("-o", "--outdir"), 
                     help="output directory")
args <- parse_args(parser)

calculate <- function(args$indir, args$outdir) {
  
  volumes <- c("32-39", "40-45", "46-48", "49-50", "51-52", "53-54", "55-57", "58-60", "61-63",
               "64-66", "67-69", "70-71", "74-75", "76-78", "79-81", "82-84")
  
  coeff = data.frame(volume1 = character(), volume2 = character(), 
                     cluster_id1 = character(), cluster_id2 = character(),
                     jaccard_coeff = numeric())
  
  n_row = 0
  
  for(n_volume in 1:length(volumes)){
    if(n_volume < length(volumes)){
      curr_period_crews <- read.csv(paste0(paste0(args$indir, "/", volumes[n_volume+1],
                                                  "/crews_", volumes[n_volume+1]),
                                           ".csv"), col.names = c("source_node",
                                                                  "target_node",
                                                                  "weight",
                                                                  "community_id",
                                                                  "years"))
      
      prev_period_crews <- read.csv(paste0(paste0(args$indir, "/", volumes[n_volume],
                                                  "/crews_", volumes[n_volume]),
                                           ".csv"), col.names = c("source_node",
                                                                  "target_node",
                                                                  "weight",
                                                                  "community_id",
                                                                  "years"))
      
      for(i in unique(curr_period_crews$community_id)){
        curr_vertices <- vertices_list(curr_period_crews, i)
        for(j in unique(prev_period_crews$community_id)){
          prev_vertices <- vertices_list(prev_period_crews, j)
          n_row = n_row + 1
          j_c <- length(intersect(curr_vertices, prev_vertices))/length(union(curr_vertices,
                                                                              prev_vertices))
          dist_two_clusters <- c(volumes[n_volume], volumes[n_volume+1], j, i, j_c)
          coeff[n_row,] <- dist_two_clusters
        }
      }
    }
  }
  coeff$jaccard_coeff <- as.numeric(as.character(coeff$jaccard_coeff))
  ggplot(coeff, aes(x = jaccard_coeff)) + geom_histogram()
  ggsave(paste("j_c_distribution.png", sep = ""), device = png(), path = args$outdir, width = 5,
         height = 5, limitsize = FALSE)
  write.csv(coeff, paste(args$outdir, "jaccard_coefficient.csv", sep = "/"), 
            row.names = FALSE)
}

calculate(args)
