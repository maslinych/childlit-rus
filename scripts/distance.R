library(igraph)
library(dplyr)
cluster_to_node_contribution <- function(graph, years, outdir) {
  setwd(outdir)
  net <- readRDS(graph)
  contributions <- data.frame()
  for(i in unique(V(net)$community)){
    net_reduced <- subgraph(net, which(V(net)$community == i))
    edges <- as.data.frame(as_edgelist(net_reduced, names = T))
    edges$weight <- E(net_reduced)$weight
    all_edges_weights <- sum(edges$weight)
    for(x in 1:2){
      for(y in unique(edges[,x])){
        contribution <- data.frame()
        contribution <- edges %>%
          filter(edges[,x] == y) %>%
          summarise(coefficient = sum(weight)/all_edges_weights) %>%
          mutate(name = y) %>%
          mutate(cluster = i) %>%
          mutate(status = ifelse(x == 1, "Author", "Illustrator"))
        contributions <- rbind(contributions, contribution)
      }
    }
  }
  contributions$years = years
  write.csv(contributions, paste(paste("contributions", years, sep = "_"), ".csv", sep = ""), row.names = FALSE)
}
cluster_to_node_contribution("C:/Users/Roman.LAPTOP-088R18A9/Desktop/40-45/network_general_40-45", "40-45", "C:/Users/Roman.LAPTOP-088R18A9/Desktop/Internship")
