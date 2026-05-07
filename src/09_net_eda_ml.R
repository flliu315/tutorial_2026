# --------------------------------------------
# Script Name: Eco-network analysis
# Purpose: This script is shown how to construct a
#          eco-network and analyze the eco-network
#          properties, as well as predict possible
#          links of the econetwork.

# Author:     Fanglin Liu
# Email:      flliu315@163.com
# Date:       2026-04-29
# --------------------------------------------
cat("\014") # Clears the console
rm(list = ls()) # Remove all variables

###############################################################
# 01- Econetwork graph representation and visualization

# # As the format of a spreadsheet or edgelist
# 
from <- c("1","1","2","2","3","4","4","5")
to <- c("2","4","3","4","4","5","6","6")
weight <- c(0.1,0.5,0.8,0.2,0.4,0.9,1.0,0.5)

# Join the variables to create a data frame
edgelist <- data.frame(from, to, weight)

# write.csv(edgelist, "data/netdata/edgelist.csv", row.names = F)

# import a edgelist (regular spreedsheet)

edgelist <- readr::read_csv("data/netdata/edgelist.csv",
                            show_col_types = FALSE)
edgelist

# visualization with igraph package

library(igraph)

# Undirected Graph
edgelist_g <- graph_from_data_frame(d = edgelist, 
                                    directed = F)
is_weighted(edgelist_g)

set.seed(3523) # Set random seed to ensure graph layout stays
plot(edgelist_g, edge.width = E(edgelist_g)$weight)

?tkplot
tkplot(edgelist_g, edge.width = 1:10)

# B) As the format of an adjacency matrix

library(networkR)
from <- c("1","1","2","2","3","4","4","5")
to <- c("2","4","3","4","4","5","6","6")
weight <- c(0.1,0.5,0.8,0.2,0.4,0.9,1.0,0.5)

adj_mat <- adjacency(from, to, weight)
rownames(adj_mat) <- 1:6
colnames(adj_mat) <- 1:6
adj_mat

adj_mat_g <- graph_from_adjacency_matrix(adj_mat,
                                       weighted=T,
                                       mode="max", 
                                       diag=F)

tkplot(adj_mat_g) # plot without edge weights
plot(adj_mat_g, # plot with edge weights
     edge.width = E(adj_mat_g)$weight) 

# C) As the format of incidence matrix 
# https://rpubs.com/lgadar/load-bipartite-graph

inc_mat <- matrix(
  c(1,0,1,
    0,1,1,
    1,1,0),
  nrow = 3,
  byrow = TRUE
)

rownames(inc_mat) <- c("Bee1","Bee2","Bee3")
colnames(inc_mat) <- c("PlantA","PlantB","PlantC")

inc_mat

inc_mat_g <- graph_from_biadjacency_matrix(inc_mat)
inc_mat_g

V(inc_mat_g)$type # FALSE for rows and TURE for cols
layout <- layout_as_bipartite(inc_mat_g)

plot(inc_mat_g, layout = layout,
  vertex.color = ifelse(V(inc_mat_g)$type, "lightblue", "salmon"),
  vertex.label.color = "black",
  vertex.size = 20,
  edge.color = "grey50",
  main = "Bipartite Network"
)

#####################################################
# 02-molecular network construction and visualization
#####################################################
# https://github.com/YongxinLiu/Note/blob/master/R/igraph/co-occurrence_network.R

# 1) load 16s RNA sequencing data (otu)
otu_warming <- read.table("data/netdata/warming.txt", 
                          head=T, row.names = 1, sep = "\t")
head(otu_warming)
dim(otu_warming) 
sum(!is.na(otu_warming)) 

otu_control <- read.table("data/netdata/control.txt", 
                          head=T, row.names = 1, sep = "\t")
head(otu_control)
dim(otu_control)
sum(!is.na(otu_control))

# save(otu_warming, otu_control, 
#      file = "data/netdata/otu_data.RData")

# 2) keep the rows in which the otus with < 7 NA in 14 plots

# A) for warming plots
na_counts_warming <- apply(otu_warming, 1, function(z) sum(is.na(z)))
length(na_counts_warming)

# delete the rows in which the number of NA OUTs > 7
otu_warming_clean <- otu_warming[na_counts_warming < 7,]
head(otu_warming_clean)
dim(otu_warming_clean)

# B) for control plots

na_counts_control <- apply(otu_control, 1, function(z) sum(is.na(z)))
otu_control_clean <- otu_control[na_counts_control< 7,]
dim(otu_control_clean)

# 3) calculating the relative abundance of each OTU in each plot

# A) for warming plots

otu_warming_clean_transform <- otu_warming_clean |>
  replace(is.na(otu_warming_clean), 0) |> # replace NA with 0's value
  t() # transfer from outs x plot to the  plot x otus table

otu_warming_rel <- 
  prop.table(as.matrix(otu_warming_clean_transform), 
                              margin = 1)*100 # each OTU relative abundance in each plot
head(otu_warming_rel)

# B) for control plots

otu_control_transform <- otu_control_clean |>
  replace(is.na(otu_control_clean), 0) |>
  t()

otu_control_rel <- prop.table(as.matrix(otu_control_transform), 
                              margin = 1)*100
head(otu_control_rel)

# 4) calculating correlation coefficient

# A) for warming plots

library(psych) # for correlation coefficient
otu_warming_corr <- corr.test(otu_warming_rel, use = "pairwise",
                              method = "spearman", adjust = "fdr",
                              alpha = 0.05)
otu_warming_r <- otu_warming_corr$r # extracting r values
otu_warming_p <- otu_warming_corr$p # extracting p valutes
otu_warming_r[otu_warming_p > 0.5 | abs(otu_warming_r) < 0.70] <-0

otu_warming_r

# B) for control plots

otu_control_corr <- corr.test(otu_control_rel, use = "pairwise",
                              method = "spearman", adjust = "fdr",
                              alpha = 0.05)
otu_control_r <- otu_control_corr$r # extracting r values
otu_control_p <- otu_control_corr$p # extracting p valutes
otu_control_r[otu_control_p >0.5 | abs(otu_control_r)<0.70] <-0

otu_control_r

# 5) constructing molecular ecological network

# A) for warming plots 

library(igraph)
class(otu_warming_r)
otu_warming_g <- graph_from_adjacency_matrix(otu_warming_r,
                                             mode = "undirected",
                                             weighted = TRUE,
                                             diag = FALSE)
plot(otu_warming_g)

otu_warming_isol_vertex <- 
  V(otu_warming_g)[igraph::degree(otu_warming_g) == 0] # isolated vertices
otu_warming_g_optimal <- 
  igraph::delete_vertices(otu_warming_g, 
                          otu_warming_isol_vertex)

set.seed(123)
plot(otu_warming_g_optimal, main ="co-occurrence network",
     vertex.frame.color = NA,  # Node border color
     vertex.label = NA,
     edge.width =1,
     vertex.size=5,  # Size of the node (default is 15)
     edge.lty =1,
     edge.curved =TRUE)
tkplot(otu_warming_g_optimal)
# write_graph(otu_warming_g_optimal,
#             "data/netdata/otu_warming_net.txt", "edgelist")


# B) for control plots

otu_control_g <- graph_from_adjacency_matrix(otu_control_r,
                                             mode = "undirected",
                                             weighted = TRUE,
                                             diag = FALSE)
plot(otu_control_g)

otu_control_isol_vertex <- 
  V(otu_control_g)[igraph::degree(otu_control_g) == 0] # isolated vertices
otu_control_g_optimal <- igraph::delete_vertices(otu_control_g, 
                                                 otu_control_isol_vertex)

set.seed(123)
plot(otu_control_g_optimal, main ="co-occurrence network",
     vertex.frame.color = NA,  # Node border color
     vertex.label = NA,
     edge.width =1,
     vertex.size=5,  # Size of the node (default is 15)
     edge.lty =1,
     edge.curved =TRUE)

# write_graph(otu_control_g_optimal,
#             "data/netdata/otu_control_net.txt", "edgelist")

################################################
# 03-network properties and exploratory analysis
################################################
# 1) Network level properties

#  A) connectance
# https://bookdown.org/creakysinger/r-note-learn/_book/Nchpter20.html
library(igraph)

g <- read_graph("data/netdata/otu_warming_net.txt","edgelist")
g <- as_undirected(g, mode = "collapse")
tkplot(g,
     vertex.frame.color=NA,
     vertex.label=NA,
     edge.width=1,
     vertex.size=5,
     edge.lty=1,
     edge.curved=F)

connectance = edge_density(g,loops=FALSE) # connectance
connectance

# B) modularity
# https://biosakshat.github.io/network-analysis.html

library(igraph)
g<-read_graph("data/netdata/otu_warming_net.txt","edgelist")
g <- as_undirected(g, mode = "collapse")
ceb <- cluster_edge_betweenness(g)
modularity(ceb)
plot(ceb, g)

# 2) Node level properties

# A) degree and degree distribution

library(igraph)
g <-read_graph("data/netdata/otu_warming_net.txt","edgelist") 
g <- as_undirected(g, mode = "collapse")
plot(g,vertex.frame.color=NA,vertex.label=NA,edge.width=1,
     vertex.size=5,edge.lty=1,edge.curved=F)
deg <- igraph::degree(g, mode="all") # calculate degree
deg

hist(deg, breaks=1:vcount(g)-1) # degree distribution

# B) closeness and betweenness centrality

deg=igraph::degree(g) 
lay <- layout_with_fr(g) # fix layout
lay
fine = 500 # increase fine regulation
palette = colorRampPalette(c('blue','red')) # set color
degCol = palette(fine)[as.numeric(cut(deg,breaks = fine))]
plot(g, layout=lay, vertex.color=degCol, 
     vertex.size=deg*1.5, vertex.label=NA)

betw <- igraph::betweenness(g) # betweenness
plot(g,layout=lay, vertex.color=degCol,
     vertex.size=betw*0.8, vertex.label=NA)

clos <- igraph::closeness(g) # closeness
plot(g,layout=lay, vertex.color=degCol,
     vertex.size=clos*15000,vertex.label=NA)
ev <- igraph::eigen_centrality(g)
ev <- igraph::eigen_centrality(g)$vector
ev
plot(g,layout=lay, vertex.color=degCol,
     vertex.size=ev*10, vertex.label=NA)

# C) triads and clustering coefficients

triad <- igraph::count_triangles(g)
triad
cc_global <- igraph::transitivity(g, type = "global")
cc_global
cc_local <- igraph::transitivity(g, type = "local")
cc_local

#####################################################
# 04- link prediction of otu_warming net with keras
#####################################################

# 1) traditional machine learning for link pred

# A) the positive/negative edges

# Load otu_warming_net data
library(igraph)
g <- read_graph("data/netdata/otu_warming_net.txt", 
                format = "edgelist")
g <- as_undirected(g, mode = "collapse")
tkplot(g)  # Visualize the graph
node_ids <- V(g)  # Get the node IDs
print(node_ids)

# total possible edges
possible_edges_mat <- t(combn(V(g), 2))
possible_edges_links <- apply(possible_edges_mat, 1, function(x) paste(sort(x), collapse = "-"))
length(possible_edges_links)

# positive or existing edges
positive_edgelist_mat <- as_edgelist(g)  # Getting the edge list
positive_edges_links <- apply(positive_edgelist_mat, 1, function(x) paste(sort(x), collapse = "-"))
length(positive_edges_links)

library(dplyr)
positive_edges_labels <- positive_edges_links %>%
  strsplit("-") %>%  # Splitting char
  do.call(rbind, .) %>%  # Merging into a matrix
  as.data.frame(stringsAsFactors = FALSE) %>%  # Converting to dataframe
  rename(from = V1, to = V2) %>%  # Renaming columns
  mutate(label = 1)  # Adding labels for positive edges

# the negative or non-existing edges
existing_edges_links <- apply(positive_edgelist_mat, 1, function(x) paste(sort(x), collapse = "-"))
negative_edges_links <- setdiff(possible_edges_links, existing_edges_links)
length(negative_edges_links)

negative_edges_labels <- negative_edges_links %>%
  strsplit("-") %>%  # Splitting char
  do.call(rbind, .) %>%  # Merging into a matrix
  as.data.frame(stringsAsFactors = FALSE) %>%  # Converting to dataframe
  rename(from = V1, to = V2) %>%  # Renaming columns
  mutate(label = 0)  # Adding labels for negative edges

# combining and shuffling all edges
all_edges_labels <- bind_rows(positive_edges_labels, negative_edges_labels) %>%
  sample_frac(1)  # Shuffling edges

# saveRDS(all_edges_labels, "data/netdata/all_edge_label.rds")

# B) constructing features

get_edge_features <- function(i, j, g) {
  
  ni <- neighbors(g, i)
  nj <- neighbors(g, j)
  
  cn <- length(intersect(ni, nj))  # Common Neighbors
  
  union_size <- length(union(ni, nj))  # Jaccard Index
  jaccard <- ifelse(union_size == 0, 0, cn / union_size)
  
  pa <- length(ni) * length(nj)  # Preferential Attachment
  
  common_nodes <- intersect(ni, nj)  # Adamic-Adar Index
  aa <- ifelse(length(common_nodes) == 0, 0,
               sum(1 / log(degree(g, common_nodes) + 1)))
  
  return(c(cn, jaccard, pa, aa))
}

edge_features <- do.call(rbind, lapply(1:nrow(all_edges_df), function(i) {
  e <- all_edges_df[i, 1:2]
  get_edge_features(e[1], e[2], g)
}))

colnames(edge_features) <- c("CN", "Jaccard", "PA", "AA")

# Standardizing edge features
edge_features <- scale(edge_features)
# saveRDS(edge_features, "data/netdata/edge_features.rds")

# C) splitting training and validation sets
edge_features <- readRDS("data/netdata/edge_features.rds")
all_edges_labels <- readRDS("data/netdata/all_edge_label.rds")

set.seed(123)
idx <- sample(1:nrow(edge_features), size = 0.8 * nrow(edge_features))
x_train <- edge_features[idx, ]
y_train <- all_edges_labels$label[idx]

x_val <- edge_features[-idx, ]
y_val <- all_edges_labels$label[-idx]

# D) training and evaluating rf model

library(randomForest)
model <- randomForest(
  x = x_train,
  y = as.factor(y_train),
  ntree = 200  # Number of trees
)

# predicting link probabilities
pred_prob <- predict(model, x_val, type = "prob")[, 2]

# evaluating rf performance
library(pROC)
roc_obj <- roc(y_val, pred_prob)
plot(roc_obj)
auc(roc_obj)

# 2) deep learning for link pre with keras in R

# A) configuring a python env for running keras

# library(reticulate)
# virtualenv_list()
# virtualenv_remove("r-reticulate")
# virtualenv_create("r-reticulate", python = "/usr/bin/python3.10")
# # use_virtualenv("r-reticulate", required = TRUE)
# 
# # installing tensorflow and keras
# 
# reticulate::py_install("tensorflow")
# reticulate::py_install("keras")
# 
# library(tensorflow)
# tf$constant("ok")

# B) training and evaluation DL model

# a. defining the model

library(keras3)

model <- keras_model_sequential() |>
  layer_dense(32, activation = "relu", input_shape = c(ncol(edge_features))) |>
  layer_dense(16, activation = "relu") |>
  layer_dropout(0.5) |>
  layer_dense(1, activation = "sigmoid")

model |> compile(
  loss = "binary_crossentropy",
  optimizer = "adam",
  metrics = list("accuracy", "AUC")
)

# b. training DL model
history <- model |> fit(
  x_train,
  y_train,
  validation_data = list(x_val, y_val),
  epochs = 50,
  batch_size = 32
  )

plot(history)

# c. evaluating DL model

pred_prob <- model |> 
  predict(x_val) |> 
  as.vector()

library(pROC)
roc_obj <- roc(y_val, pred_prob)
plot(roc_obj, main = "ROC Curve")
auc(roc_obj)

library(caret)
pred_label <- ifelse(pred_prob > 0.5, 1, 0)
conf_matrix <- confusionMatrix(
  factor(pred_label, levels = c(0,1)),
  factor(y_val, levels = c(0,1))
)
conf_matrix
