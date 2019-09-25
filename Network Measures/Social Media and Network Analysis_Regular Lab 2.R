#Read in the hs0 data over the internet using the read.table() function.
getwd()

# Save the data file to a location on your hard drive and specify the path here (Windows systems use forward slashes)
dir_path <- "C:\\Personal\\Fall'18\\Social Media and Network Analysis\\SMNA Assignments\\SMNA Lab 2"
setwd(dir_path)

# Clear everything out of memory
rm(list=ls())  
infile <- "MergerNet_Jan21_2016_forR.csv"

## Load package and plotting the graph
# This function creates an igraph graph from one or two data frames containing the (symbolic) edge list and edge/
# vertex attributes.
# d:A data frame containing a symbolic edge list in the first two columns. Additional columns are considered as edge 
# attributes.
# directed:	Logical scalar, whether or not to create a directed graph.
# vertices:	A data frame with vertex metadata, or NULL.
# If vertices is NULL, then the first two columns of d are used as a symbolic edge list and additional columns as edge 
# attributes. The names of the attributes are taken from the names of the columns.
# If vertices is not NULL, then it must be a data frame giving vertex metadata. The first column of vertices is 
# assumed to contain symbolic vertex names, this will be added to the graphs as the 'name' vertex attribute. Other
# columns will be added as additional vertex attributes. If vertices is not NULL then the symbolic edge list given
# in d is checked to contain only vertex names listed in vertices.
library(igraph)
el = read.csv(infile, header = TRUE, sep = ",")
g_acq = graph.data.frame(el, directed = TRUE, vertices= NULL)
plot(g_acq)

### List of all the years represented in the set
el[,"year"]
df <- data.frame(el)
class(df$weight)  #returns data type of the object #integer
class(df$source)  #factor
class(el)         #data.frame

# Edges
ecount(g_acq)     #returns the number of edges (54300 in this dataset, which is the total # rows here)

# Vertices
vcount(g_acq)     #returns the number of vertices (101 in this dataset, which is the unique number of targets here)

#Is it a simple graph? No!
## Check whether Self_loops exist, as do multiple edges
is.simple(g_acq)  #FALSE

E(g_acq)$weight   #Displaying the original weights
g_acq_simpl <- simplify(g_acq)    #To simplify the graph i.e. removing loops and/or multiple edges
### The above should default to the option below, to sum the existing edge weights ### when combining them
##g_acq_simpl<-simplify(g_acq,edge.attr.comb="sum" )

E(g_acq_simpl)$weight   #Displaying the weights after converting to a simple graph

# Will use the inverse of log weight for shortest path calculations
inv_weight <- 1/log(E(g_acq_simpl)$weight  + 1)
num_weight <- E(g_acq_simpl)$weight 
length(inv_weight)


# Generating induced sub-graph
sub_net<-induced.subgraph(g_acq_simpl, v=c('511', '541', '518', '519', '517', '325', '423', '446', '512', '523',
                                           '561', '621', '115', '482', '485', '487', '491', '492', '521', '712' ))
plot(sub_net)

# Changing the shape and color of nodes 511, 541, 518 and 519
library(igraphdata)
V(sub_net)$shape <- "circle"
V(sub_net)[c("511","541","518","519")]$shape <- "rectangle"
V(sub_net)$color <- "blue"
V(sub_net)[c("511","541","518","519")]$color <- "red"
plot(sub_net)

# Calculating weights of subnet
inverse_weight_sub_net <- 1/log(E(sub_net)$weight  + 1)
num_weight_sub_net <- E(sub_net)$weight

# Changing the size of nodes
V(sub_net)$size <- 70*betweenness(sub_net,weights=num_weight_sub_net,directed = TRUE,nobigint = TRUE, normalized = TRUE)


# Changing width of edges proportional to log of edge weights
plot.igraph(sub_net,layout=layout.fruchterman.reingold, edge.color="grey", 
            #vertex.size=map(bet,c_store), 
            #vertex.size2=map(bet,c_store), 
            edge.width=log(E(sub_net)$weight+1))


# Answering Quiz Questions:

# Q2
is.connected(g_acq, mode="weak")
is.connected(g_acq, mode="strong")

# Q3
diameter(g_acq_simpl, weights = inv_weight)

# Q4
betweenness(g_acq_simpl, weights = inv_weight, directed = TRUE,nobigint = TRUE, normalized = TRUE)

# Q5
closeness(g_acq_simpl, mode = "out", weights = num_weight)

# Q6
closeness(g_acq_simpl, mode = "in", weights = num_weight)

# Q7
transitivity(g_acq_simpl,type = "global")

# Q8
shortest.paths(g_acq_simpl, to="814",mode="out", weights = inv_weight)

# Q9
shortest.paths(g_acq_simpl,to="711",mode="in", weights = inv_weight)

# Q10
sub_net<-induced.subgraph(g_acq_simpl, v=c('511', '541','518', '519', '517', '325', '423', '446', '512', '523', 
                                           '561', '621', '115', '482', '485', '487', '491', '492', '521', '712' ))

diameter(sub_net, weights = 1/log(E(sub_net)$weight  + 1))