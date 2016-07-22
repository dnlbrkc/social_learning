#all functions
#network generation, degree preserving rewiring replicator
rewire<-function(network){
  node2.2<-integer(0)
  while(!length(node2.2)){
    node1.1<-sample(1:Nodes,1) #sample a node randomly
    node1.2<-sample(network[network[,1]==node1.1,2],1) #sample one of its contacts randomly

    node2.1<-sample(setdiff(1:Nodes,network[network[,2]==node1.1,1]),1) #sample node that is not connected to original node
    candidates<-network[network[,1]==node2.1,2] #sample contacts of unconnected node
    diff<-c(network[network[,1]==node1.1,2],network[network[,1]==node1.2,2]) #select node that is not connected to node or its
    node2.2<-setdiff(candidates,diff)
    if(length(node2.2)>1){node2.2<-sample(node2.2,1)}
  }

  pos1<-which(network[network[,1]==node1.1,2]==node1.2)
  network[network[,1]==node1.1,2][pos1]<-node2.1
  pos2<-which(network[network[,1]==node1.2,2]==node1.1)
  network[network[,1]==node1.2,2][pos2]<-node2.2
  pos3<-which(network[network[,1]==node2.1,2]==node2.2)
  network[network[,1]==node2.1,2][pos3]<-node1.1
  pos4<-which(network[network[,1]==node2.2,2]==node2.1)
  network[network[,1]==node2.2,2][pos4]<-node1.2
  return(network)
}

generate_network<-function(Nodes,degree){
  a<-1:Nodes
  temp<-matrix(NA,Nodes,degree)
  #generate network
  while(any(is.na(temp)))
  {

    #create matrices
    temp<-matrix(NA,Nodes,degree)
    collect<-matrix(0,Nodes,1)
    network<-matrix(NA,ncol=2,nrow=Nodes*degree)





    #generate network
    for (i in 1:Nodes){


      if(i==1){contacts<-sample(setdiff(1:Nodes,a[i]),degree)
      collect[i,]<-degree
      collect[c(contacts),]<-collect[c(contacts),]+1
      temp[i,]<-contacts

      for(k in 1:length(contacts)){

        temp[contacts[k],which.min(is.na(temp[contacts[k],]))]<-i
      }


      } else {

        tryCatch({

          contacts<-sample(setdiff(which(collect<=(degree-1)),i),degree-collect[i,1])
          if(length(collect[c(contacts),])>0){collect[c(contacts),]<-collect[setdiff(contacts,i),]+1}
          if(length(collect[c(contacts),])>0){for(k in 1:length(contacts)){
            temp[contacts[k],min(which(is.na(temp[contacts[k],])))]<-i
          }}
          if(is.logical(temp[i,which(is.na(temp[i,]))])==FALSE){
            temp[i,which(is.na(temp[i,]))]<-contacts} else{}




          collect[i,]<-degree

        },warning = function(q){
        },error = function(w){
        },finally = {})

      }}
  }
  network[,1]<-rep(1:Nodes,each=degree)
  for(i in 1:Nodes) network[network[,1]==i,2]<-temp[i,]
  return(network)
}


RW.degreeFIXmax.betwenness<-function(metric,g,reps,stat){
  measure<-vector()
  measure[1]<-stat(metric(g,directed=T))

  for (i in 2:reps){
    net<-rewire(network)
    g<-graph.edgelist(net,directed=T)
    new<-stat(metric(g,directed=T))
    if(new>measure[i-1] && clusters(g)$no==1){ network<-net;measure[i]<-new} else {measure[i]<-measure[i-1]
    }}
  return(network)
}

RW.degreeFIXmin.betweenness<-function(metric,g,reps,stat){
  measure<-vector()
  measure[1]<-stat(metric(g,directed=TRUE))

  for (i in 2:reps){
    net<-rewire(network)
    g<-graph.edgelist(net,directed=T)
    new<-stat(metric(g,directed=T))
    if(new<measure[i-1] && clusters(g)$no==1){ network<-net;measure[i]<-new} else {measure[i]<-measure[i-1]
    }}
  return(network)
}




RW.degreeFIXmax.closeness<-function(metric,g,reps,stat){
  measure<-vector()
  measure[1]<-stat(metric(g)$res)

  for (i in 2:reps){
    net<-rewire(network)
    g<-graph.edgelist(net,directed=T)
    new<-stat(metric(g)$res)
    if(new>measure[i-1] && clusters(g)$no==1){ network<-net;measure[i]<-new} else {measure[i]<-measure[i-1]
    }}
  return(network)
}

RW.degreeFIXmin.closeness<-function(metric,g,reps,stat){
  measure<-vector()
  measure[1]<-stat(metric(g)$res)

  for (i in 2:reps){
    net<-rewire(network)
    g<-graph.edgelist(net,directed=T)
    new<-stat(metric(g)$res)
    if(new<measure[i-1] && clusters(g)$no==1){ network<-net;measure[i]<-new} else {measure[i]<-measure[i-1]
    }}
  return(network)
}



RW.degreeFIXmax.clustering<-function(metric,g,reps,stat){
  measure<-vector()
  measure[1]<-stat(metric(g))

  for (i in 2:reps){
    net<-rewire(network)
    g<-graph.edgelist(net,directed=T)
    new<-stat(metric(g))
    if(new>measure[i-1] && clusters(g)$no==1){ network<-net;measure[i]<-new} else {measure[i]<-measure[i-1]
    }}
  return(network)
}

RW.degreeFIXmin.clustering<-function(metric,g,reps,stat){
  measure<-vector()
  measure[1]<-stat(metric(g))

  for (i in 2:reps){
    net<-rewire(network)
    g<-graph.edgelist(net,directed=T)
    new<-stat(metric(g))
    if(new<measure[i-1] && clusters(g)$no==1){ network<-net;measure[i]<-new} else {measure[i]<-measure[i-1]
    }}
  return(network)
}



RW.degreeFIXmax.constraint<-function(metric,g,reps,stat){
  measure<-vector()
  measure[1]<-stat(metric(g))

  for (i in 2:reps){
    net<-rewire(network)
    g<-graph.edgelist(net,directed=T)
    new<-stat(metric(g))
    if(new>measure[i-1] && clusters(g)$no==1 ){ network<-net;measure[i]<-new} else {measure[i]<-measure[i-1]
    }}
  return(network)
}
