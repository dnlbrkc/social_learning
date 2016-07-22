library(igraph)
source("all_functions.R")

Nodes<-16
degree=3

r <- as.integer( commandArgs(TRUE)[1])

#MAX-MEAN-BETWEENNESS
networks <- list()
for(i in 1:1000){
  network<-generate_network(Nodes,degree)
  original<-network
  g<-graph.edgelist(network,directed=T)
  networks[[i]]<-RW.degreeFIXmax.betwenness(igraph::betweenness,g,1e+06,mean) #MAX AVG BETWEENNESS
}
e<-vector()
for(i in 1:1000){
  e[i] <- max(igraph::betweenness(graph.edgelist(networks[[i]])))
}
network <- networks[[which.max(e)]]
plot(graph.edgelist(network,directed=T), vertex.size=5,vertex.label=NA,edge.arrow.size=0,vertex.color="black")
save(network, file="max-mean-betweenness.Rdata")



#MIN-MEAN-BETWEENNESS
networks <- list()
for(i in 1:1000){
  network<-generate_network(Nodes,degree)
  original<-network
  g<-graph.edgelist(network,directed=T)
  networks[[i]]<-RW.degreeFIXmin.betweenness(igraph::betweenness,g,1e+06,mean) #MIN AVG BETWEENNESS
}
e<-vector()
for(i in 1:1000){
  e[i] <- min(igraph::betweenness(graph.edgelist(networks[[i]])))
}
network <- networks[[which.min(e)]]
plot(graph.edgelist(network,directed=T), vertex.size=5,vertex.label=NA,edge.arrow.size=0,vertex.color="black")
save(network, file="min-mean-betweenness.Rdata")




#MAX-MAX-BETWEENNESS
networks <- list()
for(i in 1:1000){
  network<-generate_network(Nodes,degree)
  original<-network
  g<-graph.edgelist(network,directed=T)
  networks[[i]]<-RW.degreeFIXmax.betwenness(igraph::betweenness,g,1e+06,max)
}
e<-vector()
for(i in 1:1000){
  e[i] <- max(igraph::betweenness(graph.edgelist(networks[[i]])))
}
network <- networks[[which.max(e)]]
plot(graph.edgelist(network,directed=T), vertex.size=5,vertex.label=NA,edge.arrow.size=0,vertex.color="black")
save(network, file="max-max-betweenness.Rdata")



#MAX-MAX-CLOSENESS
networks <- list()
for(i in 1:1000){
  network<-generate_network(Nodes,degree)
  original<-network
  g<-graph.edgelist(network,directed=T)
  networks[[i]]<-RW.degreeFIXmax.closeness(centralization.closeness,g,1e+06,max)
}
e<-vector()
for(i in 1:1000){
  e[i] <- max(centralization.closeness(graph.edgelist(networks[[i]]))$res)
}
network <- networks[[which.max(e)]]
plot(graph.edgelist(network,directed=T), vertex.size=5,vertex.label=NA,edge.arrow.size=0,vertex.color="black")
save(network, file="max-max-closeness.Rdata")



#MIN-MAX-CLOSENESS
networks <- list()
for(i in 1:1000){
  network<-generate_network(Nodes,degree)
  original<-network
  g<-graph.edgelist(network,directed=T)
  networks[[i]]<-RW.degreeFIXmin.closeness(centralization.closeness,g,1e+06,max)
}
e<-vector()
for(i in 1:1000){
  e[i] <- max(centralization.closeness(graph.edgelist(networks[[i]]))$res)
}
network <- networks[[which.min(e)]]
plot(graph.edgelist(network,directed=T), vertex.size=5,vertex.label=NA,edge.arrow.size=0,vertex.color="black")
save(network, file="min-max-closeness.Rdata")




#MAX-AVG-CLUSTERING
networks <- list()
for(i in 1:1000){
  network<-generate_network(Nodes,degree)
  original<-network
  g<-graph.edgelist(network,directed=T)
  networks[[i]]<-RW.degreeFIXmax.clustering(transitivity,g,1e+06,mean)
}
e<-vector()
for(i in 1:1000){
  e[i] <- mean(transitivity(graph.edgelist(networks[[i]])))
}
network <- networks[[which.max(e)]]
plot(graph.edgelist(network,directed=T), vertex.size=5,vertex.label=NA,edge.arrow.size=0,vertex.color="black")
save(network, file="max-mean-clustering.Rdata")



#MIN AVG CLUSTERING
networks <- list()
for(i in 1:1000){
  network<-generate_network(Nodes,degree)
  original<-network
  g<-graph.edgelist(network,directed=T)
  networks[[i]]<-RW.degreeFIXmin.clustering(transitivity,g,1e+06,mean)
}
e<-vector()
for(i in 1:1000){
  e[i] <- mean(transitivity(graph.edgelist(networks[[i]])))
}
network <- networks[[which.min(e)]]
plot(graph.edgelist(network,directed=T), vertex.size=5,vertex.label=NA,edge.arrow.size=0,vertex.color="black")
save(network, file="min-mean-clustering.Rdata")


#MAX-VAR-CONSTRAINT
networks <- list()
for(i in 1:1000){
  network<-generate_network(Nodes,degree)
  original<-network
  g<-graph.edgelist(network,directed=T)
  networks[[i]]<-RW.degreeFIXmax.constraint(constraint,g,1e+06,var)
}
e<-vector()
for(i in 1:1000){
  e[i] <- var(constraint(graph.edgelist(networks[[i]])))
}
network <- networks[[which.max(e)]]
plot(graph.edgelist(network,directed=T), vertex.size=5,vertex.label=NA,edge.arrow.size=0,vertex.color="black")
save(network, file="max-var-constraint.Rdata")







#
#
#
#
#
# #calculate network properties
# properties <- cbind(radius(orig2),
# diameter(orig2),
# round(mean(centralization.closeness(orig2)$res),digit=2),
# round(mean(betweenness(orig2,normalized=T)),digit=2),
# round(mean(transitivity(orig2)),digit=2),
# round(mean(constraint(orig2)),digit=2))
# colnames(properties)<-c("radius","diameter","closeness","betweenness","clustering","constraint")
# print(properties)
#
#
# network <- get.edgelist(orig2)
# print(properties)
#
#
# #lattice:
#
# Nodes <- 16
# degree <- 2
# d <- graph(sapply(1:Nodes, function(i) {
#   rbind(i, ((i+1):(i+degree)-1) %% Nodes + 1)
# }))
#
# network <- get.edgelist(d)
#
# pdf(file="~/Desktop/9.pdf",heigh=10,width=10)
# plot(graph.edgelist(network,directed=T), vertex.size=5,vertex.label=NA,edge.arrow.size=0,vertex.color="blue",edge.color='blue',main="Locally connected lattice")
# dev.off()
#
#
# #fully connected:
#
# Nodes <- 16
# degree <- 15
# d <- graph(sapply(1:Nodes, function(i) {
#   rbind(i, ((i+1):(i+degree)-1) %% Nodes + 1)
# }))
#
# network <- get.edgelist(d)
# pdf(file="~/Desktop/10.pdf",heigh=10,width=10)
# plot(graph.edgelist(network,directed=T), vertex.size=5,vertex.label=NA,edge.arrow.size=0,vertex.color="red",edge.color='red',main="Fully connected")
# dev.off()
#
#
