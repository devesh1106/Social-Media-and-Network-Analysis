
getwd()

# Save the data file to a location on your hard drive and specify the path here (Windows systems use forward slashes)
dir_path <-"C:/Personal/Fall 2018/Social Media and Network Analysis/SMNA Assignments/SMNA Advanced Lab/Advanced Labs/Advanced Lab 3"
setwd(dir_path)

# clear everything out of memory
rm(list=ls())  

# Loading the edges and nodes data
infile_edges<-"CollabNetEdgeListFilteredDec7_2012.csv"
infile_nodes<-"NodesNetList_corrected_Feb19_2016.csv"

# Install the igraph package
install.packages("igraph")
library(igraph)

# Combine data and plotting the SAP community graph
edge_frame=read.csv(infile_edges, header = TRUE, sep = ",")
node_frame=read.csv(infile_nodes, header = TRUE, sep = ",")
g_sapcommunity=graph.data.frame(edge_frame, directed = TRUE, vertices= node_frame)
plot(g_sapcommunity)

# Displaying the original weights
E(g_sapcommunity)$weight

# Since original weights are NULL, therefore assigning 1 as weight and simplifying the graph
E(g_sapcommunity)$weight <- 1 
g_sapcommunity_simple <- simplify(g_sapcommunity)

# Displaying new weights of the simple graph
E(g_sapcommunity_simple)$weight
plot(g_sapcommunity_simple)

# Analyzing the original community structure
# Edges
ecount(g_sapcommunity)
# Vertices
vcount(g_sapcommunity)
is.weighted(g_sapcommunity)
is.simple(g_sapcommunity)
is.connected(g_sapcommunity, mode = "strong")
is.connected(g_sapcommunity, mode = "weak")
is.directed(g_sapcommunity)
transitivity(g_sapcommunity,type="global")
diameter(g_sapcommunity)

# Analyzing the simplified community structure
# Edges
ecount(g_sapcommunity_simple)
# Vertices
vcount(g_sapcommunity_simple)
is.weighted(g_sapcommunity_simple)
is.simple(g_sapcommunity_simple)
is.connected(g_sapcommunity_simple, mode = "strong")
is.connected(g_sapcommunity_simple, mode = "weak")
is.directed(g_sapcommunity_simple) 
transitivity(g_sapcommunity_simple,type="global")
diameter(g_sapcommunity_simple)

# Community Detection Algorithms

# Fast-Greedy Algorithm
#does not work because the graph is directed and fast greedy community detection works for undirected graphs only
sap_community_FG <- fastgreedy.community(g_sapcommunity_simple, weights=E(g_sapcommunity_simple)$weight)
#plot(sap_community_FG,g_sapcommunity_simple, vertex.label= NA, vertex.size=2)

g_sapcommunity_simple_undir <-as.undirected(g_sapcommunity_simple, mode=c("collapse"), edge.attr.comb = list("sum"))
sap_community_undir_FG <- fastgreedy.community(g_sapcommunity_simple_undir, weights=E(g_sapcommunity_simple_undir)$weight)
plot(sap_community_undir_FG,g_sapcommunity_simple_undir)
modularity(sap_community_undir_FG)
length(sap_community_undir_FG)


# Walktrap algorithm :
sap_community_walk  <- walktrap.community(g_sapcommunity_simple, weights = E(g_sapcommunity_simple)$weight)
head(sort(sizes(sap_community_walk), decreasing = TRUE),10)
modularity(sap_community_walk)
length(sap_community_walk)


sap_community_walk_graph <- induced.subgraph(g_sapcommunity_simple, sap_community_walk[['9']])
plot(sap_community_walk_graph,
     vertex.label= NA,
     vertex.size =sqrt(degree(sap_community_walk_graph)),
     vertex.color = as.numeric(as.factor(V(sap_community_walk_graph)$country)),
     edge.arrow.size = 0,
     main = "SAP Community using the Walktrap algorithm - Vertex Color based on Country")

# Spinglass algorithm: 
#does not work because the graph is unconnected and this 'Cannot work with unconnected graph'
#sap_community_SG <- cluster_spinglass(g_sapcommunity_simple, weights = NULL, vertex = NULL, spins = 25,
#                                      parupdate = FALSE, start.temp = 1, stop.temp = 0.01, cool.fact = 0.99,
#                                      update.rule = c("config", "random", "simple"), gamma = 1,
#                                      implementation = c("orig", "neg"), gamma.minus = 1)



# Infomap algorithm:
sap_community_infomap <- cluster_infomap(g_sapcommunity_simple, e.weights = NULL, v.weights = NULL, nb.trials = 10,
                modularity = TRUE)
head(sort(sizes(sap_community_infomap), decreasing = TRUE),10)
modularity(sap_community_infomap)
length(sap_community_infomap)

sap_community_infomap_graph <- induced.subgraph(g_sapcommunity_simple, sap_community_infomap[['1']])
plot(sap_community_infomap_graph,
     vertex.label= NA,
     vertex.size =sqrt(degree(sap_community_infomap_graph)),
     vertex.color = as.numeric(as.factor(V(sap_community_infomap_graph)$country)),
     edge.arrow.size = 0,
     main = "SAP Community using the Infomap algorithm - Vertex color based on Country")

# Plot for community points vs. Country:
plot(node_frame$country, node_frame$ln_points)
plot(node_frame$internet_users_percent, node_frame$ln_points)
plot(node_frame$immigration_pct, node_frame$ln_points)


