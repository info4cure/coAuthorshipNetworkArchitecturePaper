#' --- 
#' title: "Influence of co-authorship network architecture on methodology quality of systematic reviews and meta-analysis on psoriasis" 
#' author: "Juan Ruano & Juan Luis Sanz-Cabanillas" 
#' date: "30 Nov 2016" 
#' institutions: Department of Dermatology, IMIBIC/Reina Sofia University Hospital/University of Cordoba, Cordoba, Spain
#' analysis: 1: NETWORK PLOT;2: NETWORK ARCHITECTURE ANALYSIS;3: NETWORK BY AMSTAR quality level  
#' --- 
#' 
#' R version 3.3.1 (2016-06-21)
#' Platform: x86_64-apple-darwin13.4.0 (64-bit)
#' Running under: OS X 10.9.5 (Mavericks)


########  R packages ----------------

# install.packages('igraph')
# install.packages('network') 
# install.packages('sna')
# install.packages('ndtv')
# install.packages('visNetwork')
 
library('igraph')
library('network') 
library('sna')
library('ndtv')
library('visNetwork')
library("reshape")
library("RColorBrewer")


######### 1: NETWORK PLOT ----------------------------------------
######## Read .csv files --------------------------------

####  edges -----

file10        <- read.csv2("edges.csv", 
                 sep = " ", 
                 dec= ".", 
                 stringsAsFactors = TRUE,
                 header = TRUE)
links         <- as.data.frame(file10)

####  nodes -----

file11        <- read.csv2("DB_network_1.csv", 
                 sep = ";", 
                 dec= ".", 
                 stringsAsFactors = TRUE,
                 header = TRUE)
DB_network    <- as.data.frame(file11)
nodes         <- DB_network
nodes         <- nodes[row.names(unique(nodes[,c("id", "class_node")])),]
nrow(nodes); length(unique(nodes$id))
nrow(links); nrow(unique(links[,c("from", "to")]))
links         <- links[order(links$from, links$to),]
rownames(links) <- NULL


### getting igraph object

net           <- graph_from_data_frame(d=links, vertices=nodes, directed=F)
net           <- simplify(net, remove.multiple = F, remove.loops = T) 

######## node colors ----------------

V(net)$color<-"gray50"
V(net)[nodes$country=="Australia"]$color<-"yellow4"
V(net)[nodes$country=="Austria"]$color<-"royalblue4"
V(net)[nodes$country=="Belgium"]$color<-"royalblue3"
V(net)[nodes$country=="Brazil"]$color<-"violetred4"
V(net)[nodes$country=="Canada"]$color<-"olivedrab4"
V(net)[nodes$country=="Chile"]$color<-"palegreen4"
V(net)[nodes$country=="China"]$color<-"brown2"
V(net)[nodes$country=="Colombia"]$color<-"chartreuse4"
V(net)[nodes$country=="Croatia"]$color<-"royalblue2"
V(net)[nodes$country=="Denmark"]$color<-"navyblue"
V(net)[nodes$country=="France"]$color<-"blue3"
V(net)[nodes$country=="Germany"]$color<-"dodgerblue2"
V(net)[nodes$country=="Greece"]$color<-"cadetblue3"
V(net)[nodes$country=="India"]$color<-"orchid4"
V(net)[nodes$country=="Ireland"]$color<-"cornflowerblue"
V(net)[nodes$country=="Israel"]$color<-"steelblue2"
V(net)[nodes$country=="Italy"]$color<-"slateblue3"
V(net)[nodes$country=="Kuwait"]$color<-"tan4"
V(net)[nodes$country=="Mexico"]$color<-"springgreen4"
V(net)[nodes$country=="Netherlands"]$color<-"darkblue"
V(net)[nodes$country=="Norway"]$color<-"cyan4"
V(net)[nodes$country=="Portugal"]$color<-"blue1"
V(net)[nodes$country=="Qatar"]$color<-"lightsalmon4"
V(net)[nodes$country=="Russian Federation"]$color<-"orangered3"
V(net)[nodes$country=="Saudi Arabia"]$color<-"goldenrod2"
V(net)[nodes$country=="Serbia"]$color<-"lightblue4"
V(net)[nodes$country=="South Korea"]$color<-"lightcoral"
V(net)[nodes$country=="Spain"]$color<-"royalblue2"
V(net)[nodes$country=="Switzerland"]$color<-"turquoise3"
V(net)[nodes$country=="Thailand"]$color<-"purple3"
V(net)[nodes$country=="United Kingdom"]$color<-"deepskyblue4"
V(net)[nodes$country=="United States"]$color<-"limegreen"
V(net)[nodes$country=="paper"]$color<-"gray80"

######## node sizes ----------------

V(net)$size <- V(net)$log_node.size*1.2

######## node label size ----------------

# V(net)$label <- NA in case you do not want node labels
V(net)$label.cex <- seq(0.5,0.5)

######## network plot ----------------

set.seed(120)
plot(net,edge.color="gray50", edge.arrow.size=0,vertex.label=V(net)$country.label, vertex.label.font=1, vertex.label.color="black", vertex.label.family = "sans",vertex.label.dist=0,layout=layout_nicely) 

#  plot.new()


######## network color legend ----------------

### colours of legend
# colours   <- unique(V(net)$color)

#### labels of legend
# subdf     <- subset(nodes, country!="paper")
# subdf$country <- factor(subdf$country)
# labels    <- levels(subdf$country)
# legend("bottom",legend=labels, col=colours[-33], pch=19,pt.cex=1, cex=0.6,ncol=4, title="Countries of authors's institutions")


######### 2: NETWORK ARCHITECTURE ANALYSIS ----------------------------------------
######### 2.1. Network and node descriptives --------------------
# 2.1.1 Density
# The proportion of present edges from all possible edges in the network.
# edge_density(net, loops=F)
edge_density(net, loops=F)

# 2.1.2 Reciprocity
# The proportion of reciprocated ties (for a directed network).
# NA

# 2.1.3 Transitivity
# • global - ratio of triangles (direction disregarded) to connected triples.
# • local - ratio of triangles to connected triples each vertex is part of.
transitivity(net, type="global") # net is treated as an undirected network
transitivity(as.undirected(net, mode="collapse")) # same as above
transitivity(net, type="local")

# 2.1.4 Diameter
# A network diameter is the longest geodesic distance (length of the shortest path between two nodes)
# in the network. In igraph, diameter() returns the distance, while get_diameter() returns the
# nodes along the first found path of that distance.
# Note that edge weights are used by default, unless set to NA
diameter(net, directed=F, weights=NA)
diam<-get_diameter(net, directed=F, weights=NA)
diameter(net, directed=F)

#Color nodes along the diameter:
vcol <- rep("gray40", vcount(net))
vcol[diam] <- "gold"
ecol <- rep("gray80", ecount(net))
ecol[E(net, path=diam)] <- "orange"
# E(net, path=diam) finds edges along a path, here 'diam'
plot(net, vertex.color=vcol, edge.color=ecol, edge.arrow.mode=0, vertex.label=V(net)$country.label, vertex.label.font=1, vertex.label.color="black", vertex.label.family = "sans", vertex.label.dist=0)

# 2.1.5 Centrality & centralization
# Centrality functions (vertex level) and centralization functions (graph level). The centralization
# functions return res - vertex centrality, centralization, and theoretical_max - maximum
# centralization score for a graph of that size. The centrality function can run on a subset of nodes
# (set with the vids parameter). This is helpful for large graphs where calculating all centralities may
# be a resource-intensive and time-consuming task.
# Degree (number of ties)
centr_degree(net, mode="in", normalized=T)

# Closeness (centrality based on distance to others in the graph)
# Inverse of the node’s average geodesic distance to others in the network.

centr_clo(net, mode="all", normalized=T)

# Eigenvector (centrality proportional to the sum of connection centralities)
# Values of the first eigenvector of the graph matrix.

eigen_centrality(net, directed=T, weights=NA)
centr_eigen(net, directed=T, normalized=T)

# Betweenness (centrality based on a broker position connecting others)
# Number of geodesics that pass through the node or the edge.
betweenness(net, directed=T, weights=NA)  ## NO VA
edge_betweenness(net, directed=T, weights=NA)
centr_betw(net, directed=T, normalized=T)


# 2.1.6 Hubs and authorities
# The hubs and authorities algorithm developed by Jon Kleinberg was initially used to examine
# web pages. Hubs were expected to contain catalogs with a large number of outgoing links; while
# authorities would get many incoming links from hubs, presumably because of their high-quality
# relevant information

hs <- hub_score(net, weights=NA)$vector
as <- authority_score(net, weights=NA)$vector

par(mfrow=c(1,2))
plot(net,edge.color="gray50", edge.arrow.size=0,vertex.label=V(net)$country.label, vertex.label.font=1, vertex.label.color="black", vertex.label.family = "sans",vertex.label.dist=0,layout=layout_nicely, vertex.size=hs*50, main="Hubs") 
plot(net,edge.color="gray50", edge.arrow.size=0,vertex.label=V(net)$country.label, vertex.label.font=1, vertex.label.color="black", vertex.label.family = "sans",vertex.label.dist=0,layout=layout_nicely, vertex.size=as*30, main="Authorities") 

######### 2.2. Distances and paths --------------------
# 2.2.1 Average path length
# Average path length: the mean of the shortest distance between each pair of nodes in the network
# (in both directions for directed graphs).
mean_distance(net, directed=F)

# 2.2.2 Length of all shortest paths
# We can also find the length of all shortest paths in the graph:
distances(net) # with edge weights
distances(net, weights=NA) # ignore weights

######### 2.3. Cliques --------------------
# Find cliques (complete subgraphs of an undirected graph)
# Before we start, we will make our network undirected. There are several ways to do that conversion:
#     • We can create an undirected link between any pair of connected nodes (mode="collapse" )
#     • Create undirected link for each directed one in the network, potentially ending up with a
#     multiplex graph (mode="each" )
#     • Create undirected link for each symmetric link in the graph (mode="mutual" ).
# 
# In cases when we may have ties A -> B and B -> A ties collapsed into a single undirected link, we
# need to specify what to do with their edge attributes using the parameter ‘edge.attr.comb’ as we
# did earlier with simplify(). Here we have said that the ‘weight’ of the links should be summed,
# and all other edge attributes ignored and dropped.

net.sym <- as.undirected(net, mode= "collapse",edge.attr.comb=list(weight="sum", "ignore"))

cliques(net.sym) # list of cliques
sapply(cliques(net.sym), length) # clique sizes
largest_cliques(net.sym) # cliques with max number of nodes

vcol <- rep("grey80", vcount(net.sym))
vcol[unlist(largest_cliques(net.sym))] <- "gold"
plot(as.undirected(net.sym), vertex.label=V(net.sym)$name, vertex.color=vcol)
#### al ser tan modular, todo son cliques máximos

######### 2.4. Community detection --------------------
# 2.4.1: Community detection based on edge betweenness (Newman-Girvan)  ------------------
# High-betweenness edges are removed sequentially (recalculating at each step) and the best partitioning
# of the network is selected.
ceb <- cluster_edge_betweenness(net)
dendPlot(ceb, mode="hclust")
plot(ceb,net,edge.color="gray50", edge.arrow.size=0,vertex.label=V(net)$country.label, vertex.label.font=1, vertex.label.color="black", vertex.label.family = "sans",vertex.label.dist=0,layout=layout_nicely) 
length(ceb) ### da 90 comunidades
membership(ceb)
modularity(ceb) # how modular the graph partitioning is  0.90 High modularity for a partitioning reflects dense connections within communities and sparse
# connections across communities
crossing(ceb, net) 

# 2.4.2: Community detection based on based on propagating labels  ------------------
# Assigns node labels, randomizes, than replaces each vertex’s label with the label that appears most
# frequently among neighbors. Those steps are repeated until each vertex has the most common label
# of its neighbors.

clp <- cluster_label_prop(net)
plot(clp,net,edge.color="gray50", edge.arrow.size=0,vertex.label=V(net)$country.label, vertex.label.font=1, vertex.label.color="black", vertex.label.family = "sans",vertex.label.dist=0,layout=layout_nicely) 

# 2.4.3: Community detection based on greedy optimization of modularity
# NO SE PUEDE: fast-greedy community finding works only on graphs without multiple edges
cfg <- cluster_fast_greedy(net)
plot(cfg, as.undirected(net))

######### 2.5. K-core decomposition --------------------
# The k-core is the maximal subgraph in which every node has degree of at least k. This also means
# that the (k+1)-core will be a subgraph of the k-core.
# The result here gives the coreness of each vertex in the network. A node has coreness D if it belongs
# to a D-core but not to (D+1)-core.
kc <- coreness(net, mode="all")
plot(net, vertex.size=kc*6, vertex.label=kc, vertex.color=colrs[kc])

######### 2.6. Assortativity and Homophily --------------------
# Homophily: the tendency of nodes to connect to others who are similar on some variable.
# • assortativity_nominal() is for categorical variables (labels)
# • assortativity() is for ordinal and above variables
# • assortativity_degree() checks assortativity in node degrees

assortativity_nominal(net, V(net)$country, directed=F)  ## no funciona
assortativity(net, V(net)$log_node.size, directed=F) ### resultado -0.1729667  ###==Correlation of attributes across connected nodes
assortativity_degree(net, directed=F)  ### resultado 0.4419482  ###== Correlation of focal attribute being the node degree D-1



###
# 5. K-CORES
###
# The graph.coreness() function in igraph returns a vector containing 
# the degree of the highest-degree k-core to which each vertex 
# belongs. 
# Note that the output of graph.coreness refers simply to the *degree*
# of the k-core, not to the k-core itself; thus, two vertices both with 
# coreness of 3 may not be connected at all and thus may be in separate
# k-cores.
coreness = graph.coreness(net)
coreness


# One way to get a sense of the actual k-core structure is to simply 
# plot the graph and color-code by k-core:
make_k_core_plot <- function (g) {
    lay1 <- layout.fruchterman.reingold(g)
    plot(g, 
         vertex.color = graph.coreness(g), 
         layout=lay1, 
         vertex.label=V(net)$country.label,
         vertex.label.font=1, 
         vertex.label.color="black", 
         vertex.label.family = "sans",
         edge.arrow.size = .5)
} 
 
make_k_core_plot(net)

deg.dist <- degree_distribution(net, cumulative=T, mode="all")
plot( x=0:max(deg), y=1-deg.dist, pch=19, cex=1.2, col="orange", xlab="Degree", ylab="Cumulative Frequency")



### calculo degree
deg.net <- degree(net, mode='total', loops=FALSE) 
net.low<-delete_vertices(net.low,which(degree(net.low)<1))




######### 3: NETWORK BY AMSTAR quality level ----------------------------------------
#####  split plot ----------
net.low      <- net - (E(net)[E(net)$amstar_level=="high quality" | E(net)$amstar_level=="moderate quality"])
net.moderate <- net - (E(net)[E(net)$amstar_level=="high quality" | E(net)$amstar_level=="low quality"])
net.high     <- net - (E(net)[E(net)$amstar_level=="moderate quality" | E(net)$amstar_level=="low quality"])

### low
V(net.low)$color<-"gray50"
V(net.low)[nodes$country=="Australia"]$color<-"yellow4"
V(net.low)[nodes$country=="Austria"]$color<-"royalblue4"
V(net.low)[nodes$country=="Belgium"]$color<-"royalblue3"
V(net.low)[nodes$country=="Brazil"]$color<-"violetred4"
V(net.low)[nodes$country=="Canada"]$color<-"olivedrab4"
V(net.low)[nodes$country=="Chile"]$color<-"palegreen4"
V(net.low)[nodes$country=="China"]$color<-"brown2"
V(net.low)[nodes$country=="Colombia"]$color<-"chartreuse4"
V(net.low)[nodes$country=="Croatia"]$color<-"royalblue2"
V(net.low)[nodes$country=="Denmark"]$color<-"navyblue"
V(net.low)[nodes$country=="France"]$color<-"blue3"
V(net.low)[nodes$country=="Germany"]$color<-"dodgerblue2"
V(net.low)[nodes$country=="Greece"]$color<-"cadetblue3"
V(net.low)[nodes$country=="India"]$color<-"orchid4"
V(net.low)[nodes$country=="Ireland"]$color<-"cornflowerblue"
V(net.low)[nodes$country=="Israel"]$color<-"steelblue2"
V(net.low)[nodes$country=="Italy"]$color<-"slateblue3"
V(net.low)[nodes$country=="Kuwait"]$color<-"tan4"
V(net.low)[nodes$country=="Mexico"]$color<-"springgreen4"
V(net.low)[nodes$country=="Netherlands"]$color<-"darkblue"
V(net.low)[nodes$country=="Norway"]$color<-"cyan4"
V(net.low)[nodes$country=="Portugal"]$color<-"blue1"
V(net.low)[nodes$country=="Qatar"]$color<-"lightsalmon4"
V(net.low)[nodes$country=="Russian Federation"]$color<-"orangered3"
V(net.low)[nodes$country=="Saudi Arabia"]$color<-"goldenrod2"
V(net.low)[nodes$country=="Serbia"]$color<-"lightblue3"
V(net.low)[nodes$country=="South Korea"]$color<-"lightcoral"
V(net.low)[nodes$country=="Spain"]$color<-"royalblue2"
V(net.low)[nodes$country=="Switzerland"]$color<-"turquoise3"
V(net.low)[nodes$country=="Thailand"]$color<-"purple3"
V(net.low)[nodes$country=="United Kingdom"]$color<-"deepskyblue4"
V(net.low)[nodes$country=="United States"]$color<-"limegreen"
V(net.low)[nodes$country=="paper"]$color<-"gray80"

V(net.low)$size <- V(net.low)$log_node.size+1.2
#delete.vertices(net.low, V(net.low)[ degree(net.low) == 0] )
set.seed(123)
plot(net.low,edge.color="gray50", edge.arrow.size=0) 


### moderate
V(net.moderate)$color<-"gray50"
V(net.moderate)[nodes$country=="Australia"]$color<-"yellow4"
V(net.moderate)[nodes$country=="Austria"]$color<-"royalblue4"
V(net.moderate)[nodes$country=="Belgium"]$color<-"royalblue3"
V(net.moderate)[nodes$country=="Brazil"]$color<-"violetred4"
V(net.moderate)[nodes$country=="Canada"]$color<-"olivedrab4"
V(net.moderate)[nodes$country=="Chile"]$color<-"palegreen4"
V(net.moderate)[nodes$country=="China"]$color<-"brown2"
V(net.moderate)[nodes$country=="Colombia"]$color<-"chartreuse4"
V(net.moderate)[nodes$country=="Croatia"]$color<-"royalblue2"
V(net.moderate)[nodes$country=="Denmark"]$color<-"navyblue"
V(net.moderate)[nodes$country=="France"]$color<-"blue3"
V(net.moderate)[nodes$country=="Germany"]$color<-"dodgerblue2"
V(net.moderate)[nodes$country=="Greece"]$color<-"cadetblue3"
V(net.moderate)[nodes$country=="India"]$color<-"orchid4"
V(net.moderate)[nodes$country=="Ireland"]$color<-"cornflowerblue"
V(net.moderate)[nodes$country=="Israel"]$color<-"steelblue2"
V(net.moderate)[nodes$country=="Italy"]$color<-"slateblue3"
V(net.moderate)[nodes$country=="Kuwait"]$color<-"tan4"
V(net.moderate)[nodes$country=="Mexico"]$color<-"springgreen4"
V(net.moderate)[nodes$country=="Netherlands"]$color<-"darkblue"
V(net.moderate)[nodes$country=="Norway"]$color<-"cyan4"
V(net.moderate)[nodes$country=="Portugal"]$color<-"blue1"
V(net.moderate)[nodes$country=="Qatar"]$color<-"lightsalmon4"
V(net.moderate)[nodes$country=="Russian Federation"]$color<-"orangered3"
V(net.moderate)[nodes$country=="Saudi Arabia"]$color<-"goldenrod2"
V(net.moderate)[nodes$country=="Serbia"]$color<-"lightblue3"
V(net.moderate)[nodes$country=="South Korea"]$color<-"lightcoral"
V(net.moderate)[nodes$country=="Spain"]$color<-"royalblue2"
V(net.moderate)[nodes$country=="Switzerland"]$color<-"turquoise3"
V(net.moderate)[nodes$country=="Thailand"]$color<-"purple3"
V(net.moderate)[nodes$country=="United Kingdom"]$color<-"deepskyblue4"
V(net.moderate)[nodes$country=="United States"]$color<-"limegreen"
V(net.moderate)[nodes$country=="paper"]$color<-"gray80"

V(net.moderate)$size <- V(net.moderate)$log_node.size+1.2

set.seed(123)
plot(net.moderate,edge.color="gray50", edge.arrow.size=0) 

### high
V(net.high)$color<-"gray50"
V(net.high)[nodes$country=="Australia"]$color<-"yellow4"
V(net.high)[nodes$country=="Austria"]$color<-"royalblue4"
V(net.high)[nodes$country=="Belgium"]$color<-"royalblue3"
V(net.high)[nodes$country=="Brazil"]$color<-"violetred4"
V(net.high)[nodes$country=="Canada"]$color<-"olivedrab4"
V(net.high)[nodes$country=="Chile"]$color<-"palegreen4"
V(net.high)[nodes$country=="China"]$color<-"brown2"
V(net.high)[nodes$country=="Colombia"]$color<-"chartreuse4"
V(net.high)[nodes$country=="Croatia"]$color<-"royalblue2"
V(net.high)[nodes$country=="Denmark"]$color<-"navyblue"
V(net.high)[nodes$country=="France"]$color<-"blue3"
V(net.high)[nodes$country=="Germany"]$color<-"dodgerblue2"
V(net.high)[nodes$country=="Greece"]$color<-"cadetblue3"
V(net.high)[nodes$country=="India"]$color<-"orchid4"
V(net.high)[nodes$country=="Ireland"]$color<-"cornflowerblue"
V(net.high)[nodes$country=="Israel"]$color<-"steelblue2"
V(net.high)[nodes$country=="Italy"]$color<-"slateblue3"
V(net.high)[nodes$country=="Kuwait"]$color<-"tan4"
V(net.high)[nodes$country=="Mexico"]$color<-"springgreen4"
V(net.high)[nodes$country=="Netherlands"]$color<-"darkblue"
V(net.high)[nodes$country=="Norway"]$color<-"cyan4"
V(net.high)[nodes$country=="Portugal"]$color<-"blue1"
V(net.high)[nodes$country=="Qatar"]$color<-"lightsalmon4"
V(net.high)[nodes$country=="Russian Federation"]$color<-"orangered3"
V(net.high)[nodes$country=="Saudi Arabia"]$color<-"goldenrod2"
V(net.high)[nodes$country=="Serbia"]$color<-"lightblue3"
V(net.high)[nodes$country=="South Korea"]$color<-"lightcoral"
V(net.high)[nodes$country=="Spain"]$color<-"royalblue2"
V(net.high)[nodes$country=="Switzerland"]$color<-"turquoise3"
V(net.high)[nodes$country=="Thailand"]$color<-"purple3"
V(net.high)[nodes$country=="United Kingdom"]$color<-"deepskyblue4"
V(net.high)[nodes$country=="United States"]$color<-"limegreen"
V(net.high)[nodes$country=="paper"]$color<-"gray80"

V(net.high)$size <- V(net.high)$log_node.size+1.2

set.seed(123)
plot(net.high,edge.color="gray50", edge.arrow.size=0) 

set.seed(123)
l <- layout_with_fr(net)
par(mfrow=c(1,3))
plot(net.low,edge.color="gray50", edge.arrow.size=0) 
plot(net.moderate,edge.color="gray50", edge.arrow.size=0) 
plot(net.high,edge.color="gray50", edge.arrow.size=0) 

dev.off()

