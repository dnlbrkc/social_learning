rm(list=ls())

library(gtools)
library(fGarch)

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

source("landscapes/generate_NK.R")
#this file contains all 2^N binary numbers
#in decimal format and of each number all neighbors
#that differ in exactly 1 digit from a given number.
#this information is given in decimal format so that
#the numbers can be directly used to select rows in
#the matrix containing the landscape.
load("landscapes/bin_neighbors.Rdata")


#PROPERTIES OF LANDSCAPE, AGENTS, AND NUMBER OF TRIALS

n.strat=6 #NO. STRATEGIES
N=15; K=7 #NK PARAMETERS
n.agents=100 #NO. AGENTS
tsteps=200 #NO. TIME STEPS

#REPLICATION ID
r <- as.integer( commandArgs(TRUE)[1])

#STORE RESULTS
diversity<-matrix(NA,ncol=6,nrow=tsteps)
perf.time<-matrix(NA,ncol=6,nrow=tsteps)

#GENERATE LANDSCAPE
LS<-permutations(2,N,v=c(0,1),repeats.allowed=TRUE)
LS<-as.data.frame(LS)
if (K==0){
  depends <- as.vector(1:N)
  values <- replicate(N,round(runif(2,0,1),1))
  fitness <- values
} else {
  depends <- rbind(1:N,replicate(N,sample(c(1:N),K,replace=F)))
  combinations <- permutations(2,K+1,v=c(0,1),repeats.allowed=TRUE)
  values <- replicate(N,round(runif(nrow(combinations),0,1),1))
  fitness <- cbind(combinations,values)
}
landscape <- generate_landscape(N,K,LS,fitness,depends)
landscape[,N+1] <- (landscape[,N+1]/max(landscape[,N+1]))^8
landscape <- cbind(0:(nrow(landscape)-1),landscape[,N+1])

for (net in networks){
  setwd("networks/")
  load(net)
  contacts <- network

#AVERAGE OVER 100 LANDSCAPES
total.perftime <- matrix(0,ncol=n.strat,nrow=tsteps)
total.diversity <- matrix(0,ncol=n.strat,nrow=tsteps)
reps = 1
for (reps in 1:reps){

#FOR EACH STRATEGY
for (s in c(1:6)){

#DETERMINE SAMPLE SIZE
if (s == 1 || s == 3) samplesize<-3 else if (s == 2 || s == 4) samplesize<-9 else samplesize<-1

agents<-list()
#ALLOCATE RANDOM STARTING POSITIONS
agents[[1]] <- landscape[sample(1:2^N,100,replace=F),]

#FOR EACH TIME STEP
for (i in 2:tsteps){

#CARRY OVER OPTION FROM PREVIOUS TIME STEP
agents[[i]] <- agents[[i-1]]
#SAMPLE 's' OTHER PEOPLE
#samples <- replicate(n.agents,sample(1:n.agents,samplesize,replace=F))
samples <- sapply(1:100, function(x) sample(contacts[contacts[,1]==x,2],samplesize))

if (s == 1 || s == 2 ){ #IMPLEMENT BEST MEMBER STRATEGY

  ind.learn<-vector(length=n.agents)

  agents2 <- agents[[i]]
  for (j in 1:n.agents){

    payoffs <- agents[[i]][samples[,j],2] #look up payoffs of samples
    best<-which(payoffs==max(payoffs)) #select highest payoff
    if(length(best)>1) best <- sample(best,1) #if there's a tie in highest payoff, sample randomly

	  agents2[j,] <- agents[[i]][samples[best,j],] #adopt choice of best member
    if(agents2[j,2]<=agents[[i-1]][j,2]){ #if payoff worse or equal, learn individually
    ind.learn[j] <- j
    }

  }
  ind.learn<-ind.learn[ind.learn!=0]

  #those who do not learn individually adopt the socially acquired option
  no.learn <- setdiff(1:100,ind.learn)
  if(length(no.learn)>0){
  agents[[i]][no.learn,] <- agents2[no.learn,]
  }

} else if (s == 3 || s == 4 ) { #IMPLEMENT CONFORMITY STRATEGY

  ind.learn<-vector(length=n.agents)
  agents2 <- agents[[i]]
  for (j in 1:n.agents){


    a<-agents[[i]][samples[,j],1]
    b<-tabulate(a)
    freq <- b[b==max(b)]

      if(length(freq)>1 & length(freq)<samplesize){
          CH<-sample(which(b==freq[1]),1)
      } else if (length(freq)==samplesize) {
          ind.learn[j]<-j #if all options equally frequent, learn individually
        } else {
          CH <- which(b==freq[1])
          }

      if(length(freq)!=samplesize){

        agents2[j,1] <- CH
        agents2[j,2] <- landscape[(CH+1),2]
      }


      if(agents2[j,2]<=agents[[i-1]][j,2]){
        ind.learn[j]<-j
      }

  }
  ind.learn<-ind.learn[ind.learn!=0]

  #those who do not learn individually adopt the socially acquired option
  no.learn <- setdiff(1:100,ind.learn)
  if(length(no.learn)>0){
    agents[[i]][no.learn,] <- agents2[no.learn,]
  }

} else if (s==5) { #IMPLEMENT RANDOM COPYING


  ind.learn<-vector(length=n.agents)
  agents2 <- agents[[i]]
  for (j in 1:n.agents) {
    agents2[j,] <- agents[[i]][samples[j],]
  }
  ind.learn<-ind.learn[ind.learn!=0]

  #those who do not learn individually adopt the socially acquired option
  no.learn <- setdiff(1:100,ind.learn)
  if(length(no.learn)>0){
    agents[[i]][no.learn,] <- agents2[no.learn,]
  }

} else { # NO SOCIAL LEARNING CONDITION

agents[[i]]<-agents[[i-1]]
ind.learn<-1:100

}

#IMPLEMENT INDIVIDUAL LEARNING

if(length(ind.learn)>=1){
  h=0
  choose<-vector()
  for (n in c(ind.learn)){
    h=h+1
    choose[h] <- sample(bin_neighbors[,agents[[i]][n,1]+1],1)
  }
  choose1 <- choose+1

  pay<-sapply(1:length(choose1), function(x) landscape[choose1[x],2])
  new<-cbind(choose,pay)

  l=0
  for (n in c(ind.learn)){
    l=l+1
    agents[[i]][n,] <- new[l,]
  }

}

#COMPARE PAYOFFS AFTER LEARNING AND DECIDE WHETHER OR NOT TO SWITCH

switching <- ifelse(agents[[i-1]][,2] < agents[[i]][,2],1,0)*1:n.agents
not.switching <- setdiff(1:100,switching[switching!=0])

agents[[i]][not.switching,] <- agents[[i-1]][not.switching,]

}

perf.time[,s] <- sapply(1:tsteps, function(x) mean(agents[[x]][,2]))
diversity[,s] <- sapply(1:tsteps, function(x) nrow(unique(agents[[x]])))


}

total.perftime <- total.perftime + perf.time
total.diversity <- total.diversity + diversity
}

total.perftime <- total.perftime / reps
total.diversity <- total.diversity / reps
setwd("..")
setwd("results")
name<-paste0("Results",net,N,'_',K,r,'.Rdata',sep="",collapse=NULL)
res <- list(total.perftime,total.diversity)
save(res, file=name)
setwd("..")
}
