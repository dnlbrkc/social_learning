rm(list=ls())

#function storing code for generating Mason and Watts landscape
source("landscapes/generate_MasonWatts.R")

#range of input values for the landscape
range <- seq(1,100,1)

#generate landscape
environment <- MasonWatts(100)

#number of agents and trials
n.agents=100; tsteps=200

networks <- c("lattice.Rdata",
              "max-max-betweenness.Rdata",
              "max-mean-betweenness.Rdata",
              "max-mean-clustering.Rdata",
              "max-var-constraint.Rdata",
              "fully-connected.Rdata",
              "max-max-closeness.Rdata",
              "min-max-closeness.Rdata",
              "min-mean-betweenness.Rdata",
              "min-mean-clustering.Rdata")

#repetition ID for cluster computing
r <- as.integer( commandArgs(TRUE)[1])

#for each network
for(netname in networks){
setwd("networks")
load(netname)
contacts <- network

#matrix storing results
perf.time <- matrix(0,ncol=2,nrow=tsteps)

#sample size and search radius
samplesize <- 3


#allocate a random option to each agent and a corresponding payoff
agents<-list()
choices <- t(replicate(n.agents,sample(range,2))) #random choice
payoffs <- sapply(1:n.agents, function(x) environment[choices[x,1],choices[x,2]  ]) #determine payoff
agents[[1]] <- cbind(choices,payoffs)


count=0
for(strat in c(1,2)){ #for each strategy
count=count+1

  #loop through time-steps
  for (i in 2:tsteps){

    agents[[i]]<-agents[[i-1]] #carry over previous choices
    samples<-sapply(1:n.agents, function(x) sample(contacts[contacts[,1]==x,2],samplesize)) #determine samples


      if(strat==1){ #best member rule
        ind.learn<-vector(length=n.agents)
        agents2 <- agents[[i]]
        for (j in 1:n.agents){
        al<-agents[[i]][samples[,j],3]
        best<-which(al==max(al))
        if(length(best)>1){best<-sample(best,1)}
        agents2[j,] <- agents[[i]][samples[best,j],] #adopt choice of best member
          if(agents2[j,3]<=agents[[i-1]][j,3]){ #if payoff worse or equal, learn individually
          ind.learn[j] <- j
          }
        }
        ind.learn<-ind.learn[ind.learn!=0]
        no.learn <- setdiff(1:100,ind.learn)
        if(length(no.learn)>0){
          agents[[i]][no.learn,] <- agents2[no.learn,]
        }
      } else { #conformity rule
        ind.learn<-vector(length=n.agents)
        agents2 <- agents[[i]]
        for (j in 1:n.agents){
          al<-agents[[i]][samples[,j],1:2]
          freq <- vector()
          combs <- t(combn(nrow(al),2))
          for(u in 1:nrow(combs)){
          freq[u] <- al[combs[u,1],1] == al[combs[u,2],1]  && al[combs[u,1],2] == al[combs[u,2],2]
          }
          if(length(freq[freq==TRUE])<1){ind.learn[j]<-j
          } else {
          majority <- which(freq==TRUE)
          if(length(majority)>1) majority<-sample(majority,1)
          choose <- al[combs[majority,1],]
          agents2[j,1:2]<-choose
          agents2[j,3] <- environment[choose[1],choose[2]]
          }
          if(agents2[j,3]<=agents[[i-1]][j,3]){
            ind.learn[j]<-j
          }
        }
          ind.learn<-ind.learn[ind.learn!=0]
          no.learn <- setdiff(1:100,ind.learn)
          if(length(no.learn)>0){
            agents[[i]][no.learn,] <- agents2[no.learn,]
          }
      }
      temp <- agents[[i]]
      if(length(ind.learn)>=1){
        for (n in c(ind.learn)){
          #RADIUS SEARCH
          radius <- sample(1:100,1)
          which.digit <- sample(c(1:2),1)
          radius <- as.numeric(temp[n,c(which.digit)])
          vec_radius <- 1:radius
          values <- c(radius + vec_radius,radius - vec_radius)
          values[values>max(range)] <- min(range)
          values[values< min(range)] <- max(range)
          temp[n,which.digit] <- sample(values,1)
          temp[n,3] <- environment[temp[n,1],temp[n,2] ]
        }
      agents[[i]][ind.learn,] <- temp[ind.learn,]
      }

      #determine who's switching to the new option and who is keeping the old option
      switching<-ifelse(agents[[i-1]][,3]<agents[[i]][,3],1,0)*1:n.agents
      not.switching<-setdiff(1:n.agents,switching[switching!=0])
      #those who are not switching carry their option from the previous round
      agents[[i]][not.switching,]<-agents[[i-1]][not.switching,]
  }

#calculate mean performance over time and normalize from 0 to 1
perf.time[,count]<-sapply(1:tsteps, function(x) mean(agents[[x]][,3]))/100
}

#save results for each repetition and network
setwd("..")
setwd("results")
name<-paste0("Results",netname,r,'.Rdata',sep="",collapse=NULL)
res <- list(perf.time)
save(res, file=name)
setwd("..")
}
