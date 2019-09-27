
# Reading the input csv
nasdaq_nw <- read.csv("C:/Personal/Fall 2018/Social Media and Network Analysis/SMNA Assignments/SMNA Advanced Lab/Advanced Labs/Advanced Lab 5/wide_twitter_daily_lab5.csv")

library(igraph)

# Creating data graph
nasdaq_graph=graph.data.frame(nasdaq_nw[,c(2:93)], directed = FALSE)
plot(nasdaq_graph)

# Changing the graph node size based on its degree
E(nasdaq_graph)$weight <- 1
V(nasdaq_graph)$vertex_degree <-  degree(nasdaq_graph)
plot(nasdaq_graph, vertex.label.cex = 0.8, edge.width = E(nasdaq_graph)$weight, vertex.size = V(nasdaq_graph)$vertex_degree)

# Analyzing the original community structure
is.weighted(nasdaq_graph)   # True
is.simple(nasdaq_graph)     # True
is.connected(nasdaq_graph, mode = "strong")   # False
is.connected(nasdaq_graph, mode = "weak")     # False
is.directed(nasdaq_graph)   # False
diameter(nasdaq_graph)      # 6

# Basic stats
nasdaq_df <- data.frame(nasdaq_nw,directed = FALSE)
head(nasdaq_df)
vcount(nasdaq_graph)        # 224
ecount(nasdaq_graph)        # 198

# Calculating pearson coeffecient
library(ppcor)
library(psych)
library(fdrtool)
pearson_coeff = cor(nasdaq_nw[,2:93],method = "pearson")
pearson.vec <- pearson_coeff[upper.tri(pearson_coeff)]
fdr <- fdrtool(pearson.vec, statistic="correlation",plot=TRUE)

# Partial correlations to predict edges
pcorr.pvals <- matrix(0, dim(pearson_coeff)[1], dim(pearson_coeff)[2])
n <- dim(nasdaq_nw[,2:93])[1]
for(i in seq(1, 92)){
  for(j in seq(1, 92)){
    rowi <- pearson_coeff[i, -c(i, j)]
    rowj <- pearson_coeff[j, -c(i, j)]
    tmp <- (pearson_coeff[i, j] - 
              rowi*rowj)/sqrt((1-rowi^2) * (1-rowj^2))
    tmp.zvals <- (0.5) * log((1+tmp) / (1-tmp))
    tmp.s.zvals <- sqrt(n-4) * tmp.zvals
    tmp.pvals <- 2 * pnorm(abs(tmp.s.zvals), 
                           0, 1, lower.tail=FALSE)
    pcorr.pvals[i, j] <- max(tmp.pvals)
  }
}

#Fisher"s Transformation
nasdaq_matrix <- data.matrix(nasdaq_df[,2:93])
z <- 0.5 * log((1 + pearson_coeff) / (1 - pearson_coeff))

z.vec <- z[upper.tri(z)]
n <- dim(nasdaq_matrix)[1]
corr.pvals <- 2 * pnorm(abs(z.vec), 0,sqrt(1 / (n-3)), lower.tail=FALSE)
length(corr.pvals)      # 4186


#Applying the Benjamini-Hochberg adjustment to control for the false-discovery rate
pcorr.pvals.vec <- pcorr.pvals[lower.tri(pcorr.pvals)]
pcorr.pvals.adj <- p.adjust(pcorr.pvals.vec, "BH")

# Plotting for statistic significance level as 0.05
corr.edges <- (pcorr.pvals.adj < 0.05)
length(pcorr.pvals.adj[corr.edges])     #48

# Creating a matrix and then assigning names to its rows and columns
corr.A <- matrix(0, 92, 92)

rownames(corr.A) <- paste(c("ADBE", "ADP", "ADSK", "AKAM", "ALTR", "ALXN", "AMAT", "AMGN", "AMZN", "APOL", "ATVI", "BBBY", "BIDU", 
                            "BIIB", "BMC", "BRCM", "CA", "CELG", "CERN", "CHKP", "CHRW", "CMCSA", "COST", "CSCO", "CTRP", "CTSH", 
                            "CTXS", "DELL", "DLTR", "DTV", "EA", "EBAY", "ESRX", "EXPE", "FAST", "FFIV", "FISV", "FLEX", "FLIR", 
                            "FSLR", "GILD", "GMCR", "GRMN", "HSIC", "ILMN", "INFY", "INTU", "ISRG", "JOY", "KLAC", "LIFE", "LINTA", 
                            "LLTC", "LRCX", "MAT", "MCHP", "MRVL", "MU", "MXIM", "MYL", "NFLX", "NIHD", "NTAP", "NVDA", "NWSA", "ORCL",
                            "PAYX", "PCAR", "PCLN", "QCOM", "ROST", "SBUX", "SHLD", "SIAL", "SIRI", "SNDK", "SPLS", "SRCL", "STX",
                            "SYMC", "TEVA", "URBN", "VMED", "VOD", "VRSN", "VRTX", "WCRX", "WFM", "WYNN", "XLNX", "XRAY", "YHOO"))

colnames(corr.A) <- paste(c("ADBE", "ADP", "ADSK", "AKAM", "ALTR", "ALXN", "AMAT", "AMGN", "AMZN", "APOL", "ATVI", "BBBY", "BIDU", 
                            "BIIB", "BMC", "BRCM", "CA", "CELG", "CERN", "CHKP", "CHRW", "CMCSA", "COST", "CSCO", "CTRP", "CTSH", 
                            "CTXS", "DELL", "DLTR", "DTV", "EA", "EBAY", "ESRX", "EXPE", "FAST", "FFIV", "FISV", "FLEX", "FLIR", 
                            "FSLR", "GILD", "GMCR", "GRMN", "HSIC", "ILMN", "INFY", "INTU", "ISRG", "JOY", "KLAC", "LIFE", "LINTA", 
                            "LLTC", "LRCX", "MAT", "MCHP", "MRVL", "MU", "MXIM", "MYL", "NFLX", "NIHD", "NTAP", "NVDA", "NWSA", "ORCL",
                            "PAYX", "PCAR", "PCLN", "QCOM", "ROST", "SBUX", "SHLD", "SIAL", "SIRI", "SNDK", "SPLS", "SRCL", "STX",
                            "SYMC", "TEVA", "URBN", "VMED", "VOD", "VRSN", "VRTX", "WCRX", "WFM", "WYNN", "XLNX", "XRAY", "YHOO"))


# Creating a graph from the above obtained adjacency matrix after BH adjustment
corr.A[lower.tri(corr.A)] <- as.numeric(corr.edges)
g.corr <- graph.adjacency(corr.A, "undirected")
V(g.corr)$label.cex = 1
plot(g.corr,vertex.size=1)

# Deleting the nodes with degree centrality as 0 
g.corrdelete<-delete.vertices(g.corr, V(g.corr)[degree(g.corr) == 0])
V(g.corrdelete)$vertex_degree <-  degree(g.corrdelete)
plot(g.corrdelete,vertex.size=V(g.corrdelete)$vertex_degree)


# Plotting for statistic significance level as 0.01
corr.edges <- (pcorr.pvals.adj < 0.01)
length(pcorr.pvals.adj[corr.edges])     #32

# Creating a matrix and then assigning names to its rows and columns
corr.A <- matrix(0, 92, 92)

rownames(corr.A) <- paste(c("ADBE", "ADP", "ADSK", "AKAM", "ALTR", "ALXN", "AMAT", "AMGN", "AMZN", "APOL", "ATVI", "BBBY", "BIDU", 
                            "BIIB", "BMC", "BRCM", "CA", "CELG", "CERN", "CHKP", "CHRW", "CMCSA", "COST", "CSCO", "CTRP", "CTSH", 
                            "CTXS", "DELL", "DLTR", "DTV", "EA", "EBAY", "ESRX", "EXPE", "FAST", "FFIV", "FISV", "FLEX", "FLIR", 
                            "FSLR", "GILD", "GMCR", "GRMN", "HSIC", "ILMN", "INFY", "INTU", "ISRG", "JOY", "KLAC", "LIFE", "LINTA", 
                            "LLTC", "LRCX", "MAT", "MCHP", "MRVL", "MU", "MXIM", "MYL", "NFLX", "NIHD", "NTAP", "NVDA", "NWSA", "ORCL",
                            "PAYX", "PCAR", "PCLN", "QCOM", "ROST", "SBUX", "SHLD", "SIAL", "SIRI", "SNDK", "SPLS", "SRCL", "STX",
                            "SYMC", "TEVA", "URBN", "VMED", "VOD", "VRSN", "VRTX", "WCRX", "WFM", "WYNN", "XLNX", "XRAY", "YHOO"))

colnames(corr.A) <- paste(c("ADBE", "ADP", "ADSK", "AKAM", "ALTR", "ALXN", "AMAT", "AMGN", "AMZN", "APOL", "ATVI", "BBBY", "BIDU", 
                            "BIIB", "BMC", "BRCM", "CA", "CELG", "CERN", "CHKP", "CHRW", "CMCSA", "COST", "CSCO", "CTRP", "CTSH", 
                            "CTXS", "DELL", "DLTR", "DTV", "EA", "EBAY", "ESRX", "EXPE", "FAST", "FFIV", "FISV", "FLEX", "FLIR", 
                            "FSLR", "GILD", "GMCR", "GRMN", "HSIC", "ILMN", "INFY", "INTU", "ISRG", "JOY", "KLAC", "LIFE", "LINTA", 
                            "LLTC", "LRCX", "MAT", "MCHP", "MRVL", "MU", "MXIM", "MYL", "NFLX", "NIHD", "NTAP", "NVDA", "NWSA", "ORCL",
                            "PAYX", "PCAR", "PCLN", "QCOM", "ROST", "SBUX", "SHLD", "SIAL", "SIRI", "SNDK", "SPLS", "SRCL", "STX",
                            "SYMC", "TEVA", "URBN", "VMED", "VOD", "VRSN", "VRTX", "WCRX", "WFM", "WYNN", "XLNX", "XRAY", "YHOO"))


# Creating a graph from the above obtained adjacency matrix after BH adjustment
corr.A[lower.tri(corr.A)] <- as.numeric(corr.edges)
g.corr <- graph.adjacency(corr.A, "undirected")
V(g.corr)$label.cex = 1
plot(g.corr,vertex.size=1)

# Deleting the nodes with degree centrality as 0 
g.corrdelete<-delete.vertices(g.corr, V(g.corr)[degree(g.corr) == 0])
V(g.corrdelete)$vertex_degree <-  degree(g.corrdelete)
plot(g.corrdelete,vertex.size=V(g.corrdelete)$vertex_degree)
