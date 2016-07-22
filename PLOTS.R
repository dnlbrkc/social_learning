library(ggplot2)
library(dplyr)
library(reshape)
library(cowplot)
library(gridExtra)


rm(list=ls())
###################################################################
#PLOT 1: SIMPLE VS COMPLEX ENVIRONMENTS#
###################################################################
#panel A
a <- rnorm(1e+06)
d <- density(a)

#panel B
x<-seq(-5,5,0.001)
b<-sin(2*x) + cos(x)

setwd("~/Desktop/")
pdf("problemspace.pdf",width=6,height=3)

par(mfrow=c(1,2),mar=c(1,1,3,1))
plot(d,main="A: Simple environment",ylab="",xaxt='n',yaxt='n',xlab="",cex.main=1)
plot(b,type='l',main="B: Complex environment",ylab="",xaxt='n',yaxt='n',xlab="",cex.main=1)

dev.off()



##############################################################
#PLOT 2: PERFORMANCE OVER TIME FOR DIFFERENT STRATEGIES#
##############################################################
setwd("results/results_NK_k=0-14/k0/")
data <- list.files(pattern="Rdata")
ls <- matrix(0,ncol=6,nrow=200)
for (i in 1:length(data)){
  load(data[i])
  ls <- ls + res[[1]]
}
strats1 <- ls/length(data)

setwd("..")
setwd("..")
setwd("results_NK/")
data <- list.files(pattern="Resultsfully")
ls <- matrix(0,ncol=6,nrow=200)
for (i in 1:length(data)){
  load(data[i])
  ls <- ls + res[[1]]
}
strats2 <- ls/length(data)



colnames(strats1) <- colnames(strats2) <- c("Best member (s=3)",
                    "Best member (s=9)",
                    "Conformity (s=3)",
                    "Conformity (s=9)",
                    "Random copying",
                    "Individual learning")
melt_1 <- melt(strats1)
melt_2 <- melt(strats2)

colnames(melt_1) <- colnames(melt_2) <- c("x","Strategy","value")

plot1 <- ggplot(melt_1,aes(x=x,y=value,color=Strategy,linetype=Strategy,size=Strategy))+
  geom_line()+
  xlab("Time")+
  ylab("Average performance")+
  theme(strip.background = element_blank())+
  scale_linetype_manual(values=c(3,2,1,1,9,2))+
  scale_size_manual(values=c(0.5,0.7,0.5,1,0.7,0.7))+
  scale_color_manual(values=c("#F8766D","#F8766D","#00BFC4","#00BFC4","#000000",'#999999'))+
  theme(legend.position="bottom",
        legend.direction="horizontal",
        legend.title=element_blank(),
        legend.key.width=unit(0.6,"cm"),
        plot.title=element_text(size=12,face="plain")) + ylim(0,1)+
  ggtitle("Simple environment (N=15, K=0)")

plot2 <- ggplot(melt_2,aes(x=x,y=value,color=Strategy,linetype=Strategy,size=Strategy))+
  geom_line()+
  xlab("Time")+
  ylab("Average performance")+
  theme(strip.background = element_blank())+
  scale_linetype_manual(values=c(3,2,1,1,9,2))+
  scale_size_manual(values=c(0.5,0.7,0.5,1,0.7,0.7))+
  scale_color_manual(values=c("#F8766D","#F8766D","#00BFC4","#00BFC4","#000000",'#999999'))+
  theme(legend.position="bottom",
        legend.direction="horizontal",
        legend.title=element_blank(),
        legend.key.width=unit(0.6,"cm"),
        plot.title=element_text(size=12,face="plain")) + ylim(0,1)+
  ggtitle("Complex environment (N=15, K=7)")


p <- plot_grid( plot1 + theme(legend.position="none"),
                   plot2 + theme(legend.position="none"),
                   align = 'vh',
                   labels = c("A", "B"),
                   hjust = -1,
                   nrow = 1
)
grobs <- ggplotGrob(plot1)$grobs
legend <- grobs[[which(sapply(grobs, function(x) x$name) == "guide-box")]]

p2 <- plot_grid( p, legend, ncol=1,rel_heights=c(1,0.2))

save_plot("~/Desktop/plot1.pdf",p2,base_height=4,base_width=8)
setwd("..")
setwd("..")

##############################################################
#PLOT: UNIQUE SOLUTIONS OVER TIME FOR DIFFERENT STRATEGIES#
##############################################################
setwd("results/results_NK_k=0-14/k0/")
data <- list.files(pattern="Rdata")
ls <- matrix(0,ncol=6,nrow=200)
for (i in 1:length(data)){
  load(data[i])
  ls <- ls + res[[2]]
}
strats1 <- ls/length(data)

setwd("..")
setwd("..")
setwd("results_NK/")
data <- list.files(pattern="Resultsfully")
ls <- matrix(0,ncol=6,nrow=200)
for (i in 1:length(data)){
  load(data[i])
  ls <- ls + res[[2]]
}
strats2 <- ls/length(data)



colnames(strats1) <- colnames(strats2) <- c("Best member (s=3)",
                                            "Best member (s=9)",
                                            "Conformity (s=3)",
                                            "Conformity (s=9)",
                                            "Random copying",
                                            "Individual learning")
melt_1 <- melt(strats1)
melt_2 <- melt(strats2)

colnames(melt_1) <- colnames(melt_2) <- c("x","Strategy","value")

plot1 <- ggplot(melt_1,aes(x=x,y=value,color=Strategy,linetype=Strategy,size=Strategy))+
  geom_line()+
  xlab("Time")+
  ylab("Number of unique solutions")+
  theme(strip.background = element_blank())+
  scale_linetype_manual(values=c(3,2,1,1,9,2))+
  scale_size_manual(values=c(0.5,0.7,0.5,1,0.7,0.7))+
  scale_color_manual(values=c("#F8766D","#F8766D","#00BFC4","#00BFC4","#000000",'#999999'))+
  theme(legend.position="bottom",
        legend.direction="horizontal",
        legend.title=element_blank(),
        legend.key.width=unit(0.6,"cm"),
        plot.title=element_text(size=12,face="plain")) + ylim(0,100)+
  ggtitle("Simple environment (N=15, K=0)")

plot2 <- ggplot(melt_2,aes(x=x,y=value,color=Strategy,linetype=Strategy,size=Strategy))+
  geom_line()+
  xlab("Time")+
  ylab("Number of unique solutions")+
  theme(strip.background = element_blank())+
  scale_linetype_manual(values=c(3,2,1,1,9,2))+
  scale_size_manual(values=c(0.5,0.7,0.5,1,0.7,0.7))+
  scale_color_manual(values=c("#F8766D","#F8766D","#00BFC4","#00BFC4","#000000",'#999999'))+
  theme(legend.position="bottom",
        legend.direction="horizontal",
        legend.title=element_blank(),
        legend.key.width=unit(0.6,"cm"),
        plot.title=element_text(size=12,face="plain")) + ylim(0,100)+
  ggtitle("Complex environment (N=15, K=7)")


p <- plot_grid( plot1 + theme(legend.position="none"),
                plot2 + theme(legend.position="none"),
                align = 'vh',
                labels = c("A", "B"),
                hjust = -1,
                nrow = 1
)
grobs <- ggplotGrob(plot1)$grobs
legend <- grobs[[which(sapply(grobs, function(x) x$name) == "guide-box")]]

p2 <- plot_grid( p, legend, ncol=1,rel_heights=c(1,0.2))

save_plot("~/Desktop/unique.pdf",p2,base_height=4,base_width=8)
setwd("..")
setwd("..")




#####################################################
#PLOT 3: NETWORKS
#####################################################
rm(list=ls())
setwd("results/results_NK/")

data <- list.files(pattern="Resultslattice")
ls <- matrix(0,ncol=2,nrow=200)
for (i in 1:length(data)){
  load(data[i])
  ls <- ls + res[[1]][,c(1,3)]
}
pz1 <- ls/length(data)

data <- list.files(pattern="Resultsmax-max-betweenness")
ls <- matrix(0,ncol=2,nrow=200)
for (i in 1:length(data)){
  load(data[i])
  ls <- ls + res[[1]][,c(1,3)]
}
pz2 <- ls/length(data)


data <- list.files(pattern="Resultsmax-mean-betweenness")
ls <- matrix(0,ncol=2,nrow=200)
for (i in 1:length(data)){
  load(data[i])
  ls <- ls + res[[1]][,c(1,3)]
}
pz3 <- ls/length(data)

data <- list.files(pattern="Resultsmax-mean-clustering")
ls <- matrix(0,ncol=2,nrow=200)
for (i in 1:length(data)){
  load(data[i])
  ls <- ls + res[[1]][,c(1,3)]
}
pz4 <- ls/length(data)


data <- list.files(pattern="Resultsmax-var-constraint")
ls <- matrix(0,ncol=2,nrow=200)
for (i in 1:length(data)){
  load(data[i])
  ls <- ls + res[[1]][,c(1,3)]
}
pz5 <- ls/length(data)

data <- list.files(pattern="Resultsfully")
ls <- matrix(0,ncol=2,nrow=200)
for (i in 1:length(data)){
  load(data[i])
  ls <- ls + res[[1]][,c(1,3)]
}
pz6 <- ls/length(data)

data <- list.files(pattern="Resultsmax-max-closeness")
ls <- matrix(0,ncol=2,nrow=200)
for (i in 1:length(data)){
  load(data[i])
  ls <- ls + res[[1]][,c(1,3)]
}
pz7 <- ls/length(data)

data <- list.files(pattern="Resultsmin-max-closeness")
ls <- matrix(0,ncol=2,nrow=200)

for (i in 1:length(data)){
  load(data[i])
  ls <- ls + res[[1]][,c(1,3)]
}
pz8 <- ls/length(data)


data <- list.files(pattern="Resultsmin-mean-betweenness")
ls <- matrix(0,ncol=2,nrow=200)
for (i in 1:length(data)){
  load(data[i])
  ls <- ls + res[[1]][,c(1,3)]
}
pz9 <- ls/length(data)


data <- list.files(pattern="Resultsmin-mean-clustering")
ls <- matrix(0,ncol=2,nrow=200)

for (i in 1:length(data)){

  load(data[i])
  ls <- ls + res[[1]][,c(1,3)]
}
pz10 <- ls/length(data)


bestnet <- cbind(pz1[,1],
                 pz2[,1],
                 pz3[,1],
                 pz4[,1],
                 pz5[,1],
                 pz6[,1],
                 pz7[,1],
                 pz8[,1],
                 pz9[,1],
                 pz10[,1])
majnet <- cbind(pz1[,2],
                pz2[,2],
                pz3[,2],
                pz4[,2],
                pz5[,2],
                pz6[,2],
                pz7[,2],
                pz8[,2],
                pz9[,2],
                pz10[,2])

colnames(bestnet) <- colnames(majnet) <- c("lattice",
                                           "max_max_betweenness",
                                           "max_mean_betweenness",
                                           "max_mean_clustering",
                                           "max_var_constraint",
                                           "fully_connected",
                                           "max_max_closeness",
                                           "min_max_closeness",
                                           "min_mean_betweenness",
                                           "min_mean_clustering")


bestnet <- as.data.frame(bestnet)
majnet <- as.data.frame(majnet)

best_ineff <- bestnet %>% select(lattice:max_var_constraint) %>% rowMeans()
best_eff <- bestnet %>% select(fully_connected:min_mean_clustering) %>% rowMeans()
maj_ineff <- majnet %>% select(lattice:max_var_constraint) %>% rowMeans()
maj_eff <- majnet %>% select(fully_connected:min_mean_clustering) %>% rowMeans()

results <- cbind(best_ineff,best_eff,maj_ineff,maj_eff)

#best
ineff_up <- bestnet %>% mutate(x=1:200) %>% filter(x==200) %>% select(lattice:max_var_constraint)  %>% which.max()
ineff_low <- bestnet %>% mutate(x=1:200) %>% filter(x==200) %>% select(lattice:max_var_constraint)  %>% which.min()

eff_up <- bestnet %>% mutate(x=1:200) %>% filter(x==200) %>% select(fully_connected:min_mean_clustering)  %>% which.max()
eff_low <- bestnet %>% mutate(x=1:200) %>% filter(x==200) %>% select(fully_connected:min_mean_clustering)  %>% which.min()

beub <-  bestnet %>% select(fully_connected:min_mean_clustering) %>% select(eff_up)
belb <- bestnet %>% select(fully_connected:min_mean_clustering) %>% select(eff_low)

biub<-  bestnet  %>% select(lattice:max_var_constraint) %>% select(ineff_up)

bilb <- bestnet  %>% select(lattice:max_var_constraint) %>% select(ineff_low)

#majority
ineff_up <- majnet %>% mutate(x=1:200) %>% filter(x==200) %>% select(lattice:max_var_constraint)  %>% which.max()
ineff_low <- majnet %>% mutate(x=1:200) %>% filter(x==200) %>% select(lattice:max_var_constraint)  %>% which.min()

eff_up <- majnet %>% mutate(x=1:200) %>% filter(x==200) %>% select(fully_connected:min_mean_clustering)  %>% which.max()
eff_low <- majnet %>% mutate(x=1:200) %>% filter(x==200) %>% select(fully_connected:min_mean_clustering)  %>% which.min()

meub <-  majnet %>% select(fully_connected:min_mean_clustering) %>% select(eff_up)
melb <- majnet %>% select(fully_connected:min_mean_clustering) %>% select(eff_low)

miub <-  majnet %>% select(lattice:max_var_constraint) %>% select(ineff_up)
milb <-  majnet %>% select(lattice:max_var_constraint) %>% select(ineff_low)


#create plot
melt_df <- melt(results)
melt2 <- cbind(melt_df, as.vector(c(as.numeric(unlist(belb)),
                                    as.numeric(unlist(bilb)),
                                    as.numeric(unlist(melb)),
                                    as.numeric(unlist(milb)))),
               as.vector(c(as.numeric(unlist(beub)),
                           as.numeric(unlist(biub)),
                           as.numeric(unlist(meub)),
                           as.numeric(unlist(miub)))))
colnames(melt2) <- c("Time",
                     "Strategy",
                     "value",
                     "LB",
                     "UB")

p1 <- melt2 %>%
  mutate(x=rep(1:200,4),strat=rep(c("Strategy: best member (s=3)",
                                    "Strategy: conformity (s=3)"),each=400 ) )

df1 <- p1[p1$Strategy=="best_eff",]
df2 <- p1[p1$Strategy=="best_ineff",]
df3 <- p1[p1$Strategy=="maj_eff",]
df4 <- p1[p1$Strategy=="maj_ineff",]

p1$Strategy <- rep(rep(c("Inefficient networks","Efficient networks"),2),each=200)



#sds

data <- list.files(pattern="Resultslattice")
ls <- matrix(0,ncol=2,nrow=length(data))
for (i in 1:length(data)){
  load(data[i])
  ls[i,] <- res[[1]][200,c(1,3)]
}
sd1 <- ls

data <- list.files(pattern="Resultsmax-max-betweenness")
ls <- matrix(0,ncol=2,nrow=length(data))
for (i in 1:length(data)){
  load(data[i])
  ls[i,] <- res[[1]][200,c(1,3)]
}
sd2 <- ls

data <- list.files(pattern="Resultsmax-mean-betweenness")
ls <- matrix(0,ncol=2,nrow=length(data))
for (i in 1:length(data)){
  load(data[i])
  ls[i,] <- res[[1]][200,c(1,3)]
}
sd3 <- ls

data <- list.files(pattern="Resultsmax-mean-clustering")
ls <- matrix(0,ncol=2,nrow=length(data))
for (i in 1:length(data)){
  load(data[i])
  ls[i,] <- res[[1]][200,c(1,3)]
}
sd4 <- ls

data <- list.files(pattern="Resultsmax-var-constraint")
ls <- matrix(0,ncol=2,nrow=length(data))
for (i in 1:length(data)){
  load(data[i])
  ls[i,] <- res[[1]][200,c(1,3)]
}
sd5 <- ls

data <- list.files(pattern="Resultsfully")
ls <- matrix(0,ncol=2,nrow=length(data))
for (i in 1:length(data)){
  load(data[i])
  ls[i,] <- res[[1]][200,c(1,3)]
}
sd6 <- ls

data <- list.files(pattern="Resultsmax-max-closeness")
ls <- matrix(0,ncol=2,nrow=length(data))
for (i in 1:length(data)){
  load(data[i])
  ls[i,] <- res[[1]][200,c(1,3)]
}
sd7 <- ls

data <- list.files(pattern="Resultsmin-max-closeness")
ls <- matrix(0,ncol=2,nrow=length(data))
for (i in 1:length(data)){
  load(data[i])
  ls[i,] <- res[[1]][200,c(1,3)]
}
sd8 <- ls

data <- list.files(pattern="Resultsmin-mean-betweenness")
ls <- matrix(0,ncol=2,nrow=length(data))
for (i in 1:length(data)){
  load(data[i])
  ls[i,] <- res[[1]][200,c(1,3)]
}
sd9 <- ls

data <- list.files(pattern="Resultsmin-mean-clustering")
ls <- matrix(0,ncol=2,nrow=length(data))
for (i in 1:length(data)){
  load(data[i])
  ls[i,] <- res[[1]][200,c(1,3)]
}
sd10 <- ls

sdZ <-  rbind(apply(sd1,2,sd),
              apply(sd2,2,sd),
              apply(sd3,2,sd),
              apply(sd4,2,sd),
              apply(sd5,2,sd),
              apply(sd6,2,sd),
              apply(sd7,2,sd),
              apply(sd8,2,sd),
              apply(sd9,2,sd),
              apply(sd10,2,sd))


avg_best <- bestnet
avg_maj <- majnet
last_best <- avg_best[200,]
last_maj <- avg_maj[200,]
sd_best <- apply(bestnet,2,sd)
sd_maj <- apply(majnet,2,sd)


df <- cbind(as.numeric(last_best),as.numeric(last_maj))
mdf <- melt(df)
mdf <- cbind(mdf,rep(c("Strategy: best member (s=3)","Strategy: conformity (s=3)"),each=10),
             rep(c("a","b","a","b"),each=5))

SE_best <- sdZ[,1]/sqrt(1000)
SE_conf <- sdZ[,2]/sqrt(1000)

mdf <- cbind(mdf,c(SE_best,SE_conf))

colnames(mdf) <- c("variable","condition","value","network","net_type","SE")

mdf$variable <- factor(mdf$variable)
levels(mdf$variable)  <- c("Lattice",
                           "Max max betweenness",
                           "Max mean betweenness",
                           "Max mean clustering",
                           "Max var constraint",
                           "Fully connected",
                           "Max max closeness",
                           "Min max closeness",
                           "Min mean betweenness",
                           "Min mean clustering")



plot1 <- ggplot(p1,aes(x=Time,y=value,color=Strategy,linetype=Strategy))+
  geom_ribbon(data=df1,aes(x = Time,ymin = LB, ymax = UB), inherit.aes = FALSE,fill = "#00BFC4",alpha=0.3)+
  geom_ribbon(data=df2,aes(x = Time,ymin = LB, ymax = UB), inherit.aes = FALSE,fill = "#F8766D",alpha=0.3)+
  geom_ribbon(data=df3,aes(x = Time,ymin = LB, ymax = UB), inherit.aes = FALSE,fill = "#00BFC4",alpha=0.3)+
  geom_ribbon(data=df4,aes(x = Time,ymin = LB, ymax = UB), inherit.aes = FALSE,fill = "#F8766D",alpha=0.3)+
  geom_line(size=1.5)+ facet_wrap(~strat,scales="free_y")+
  scale_linetype_manual(values=c(1,2))+
  scale_y_continuous(breaks=seq(0,0.9,0.1),labels=seq(0,0.9,0.1),limits=c(0,0.9)) +
  theme(strip.background = element_blank()) +
  scale_color_manual(values=c("#F8766D","#00BFC4"))+
  xlab("Time") + ylab("Average performance")+
  theme(legend.position = "bottom",
        legend.title=element_blank(),
        legend.text=element_text(size=15),
        legend.key.size=unit(1.5,"cm"))

levels(mdf$net_type) <- c("Inefficient networks","Efficient networks")
mdf$net_type <- factor(mdf$net_type, levels=c("Efficient networks","Inefficient networks"))

plot2 <- ggplot(mdf,aes(x=variable,y=value,color=net_type,shape=net_type)) + facet_wrap(~network,scale="free_y") +
  geom_point(size=2)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(legend.position="bottom",
        legend.title=element_blank())+
  theme(strip.background = element_blank())+
  ylab("Performance at t=200")+
  xlab("")+
  geom_errorbar(aes(ymin=value-2*SE, ymax=value+2*SE), width=.2,show.legend=FALSE)+
  scale_y_continuous(breaks=seq(0.5,0.85,0.1),labels=seq(0.5,0.85,0.1),limits=c(0.5,0.86))+
  scale_color_manual(values=c("#F8766D","#00BFC4"))+
  scale_shape_manual(values=c(19,17))


svg("~/Desktop/network_plot.svg",height=10,width=8)
grid.arrange(plot1,plot2,nrow=2)
dev.off()

####################scatterplot

avg_best <- colMeans(bestnet)
avg_maj <- colMeans(majnet)
diam <- c(6,4,6,7,5,1,3,3,2,3)

mdf <- cbind(avg_best,avg_maj)
rownames(mdf) <- NULL

mdf <- melt(mdf)
mdf <- cbind(mdf,rep(diam,2))

colnames(mdf) <- c("x","strat","value","diameter")

one <- filter(mdf,strat=="avg_best") %>% arrange(diameter)
two <- filter(mdf,strat=="avg_maj") %>% arrange(diameter)

b <- rbind(one,two)
b <- cbind(b,rep(rep(c("Efficient networks","Inefficient networks"),each=5),2))
colnames(b)[ncol(b)] <- "net_type"

cor_best <- one %>% summarize(cor(value,diameter))
cor_maj <- two %>% summarize(cor(value,diameter))

levels(b$strat) <-  c(paste0("Strategy: best member (s=3), r=",as.numeric(round(cor_best,2))), paste0("Strategy: conformity (s=3), r=",as.numeric(round(cor_maj,2))))

p1 <-  ggplot(b[1:10,],aes(x=diameter,y=value,color=net_type))+
  geom_point(aes(shape=net_type),size=2)+
  geom_smooth(method = "lm", se = FALSE,size=0.4,col="black")+
  xlab("Diameter of network")+
  ylab("Average performance")+
  theme(strip.background = element_blank())+
  scale_x_continuous(breaks=1:10,labels=1:10)+
  theme(legend.position="bottom")+
  scale_y_continuous(breaks=seq(0.45,0.7,0.05),labels=seq(0.45,0.7,0.05),limits=c(0.44,0.7))+
  theme(legend.title=element_blank(),
        plot.title=element_text(size=12,face="plain"))+
  annotate("text", x = 2, y = 0.5, label = paste0("r = ",round(cor(b[1:10,]$diameter,b[1:10,]$value),2)))+
  ggtitle("Strategy: best member (s=3)")


p2 <-  ggplot(b[11:20,],aes(x=diameter,y=value,color=net_type))+
  geom_point(aes(shape=net_type),size=2)+
  geom_smooth(method = "lm", se = FALSE,size=0.4,col="black")+
  xlab("Diameter of network")+
  ylab("Average performance")+
  theme(strip.background = element_blank())+
  scale_x_continuous(breaks=1:10,labels=1:10)+
  theme(legend.position="bottom")+
  scale_y_continuous(breaks=seq(0.45,0.7,0.05),labels=seq(0.45,0.7,0.05),limits=c(0.44,0.71))+
  theme(legend.title=element_blank(),
        plot.title=element_text(size=12,face="plain"))+
  annotate("text", x = 2, y = 0.5, label = paste0("r = ",round(cor(b[11:20,]$diameter,b[11:20,]$value),2)))+
  ggtitle("Strategy: conformity (s=3)")


p <- plot_grid( p1 + theme(legend.position="none"),
                p2 + theme(legend.position="none"),
                align = 'vh',
                labels = c("A", "B"),
                hjust = -1,
                nrow = 1
)
grobs <- ggplotGrob(p1)$grobs
legend <- grobs[[which(sapply(grobs, function(x) x$name) == "guide-box")]]

p2 <- plot_grid( p, legend, ncol=1,rel_heights=c(1,0.2))

save_plot("~/Desktop/diameter.pdf",p2,base_height=4,base_width=8)
setwd("..")
setwd("..")
#################################
##Boxplot of strategy variability
#################################


setwd("results/results_NK/")
data <- list.files(pattern="Resultsfully")
ls <- matrix(0,ncol=6,nrow=1000)
for (i in 1:length(data)){
  load(data[i])
  ls[i,] <- res[[1]][200,]
}

lab<-c("Best member (s=3)","Best member (s=9)","Conformity (s=3)","Conformity (s=9)","Random copying", "Individual learning")

colnames(ls) <- lab

ls2 <- melt(ls)

box <- ls2 %>% ggplot(aes(x=X2,y=value)) +
         geom_boxplot(width=0.5)+
         theme(axis.text.x = element_text(angle = 45, hjust = 1))+
         ylab("Performance at t=200") + xlab("")

save_plot("~/Desktop/boxplot.pdf",box,base_height=5,base_width=7)



#####################################################
#PLOT: Mason Watts
#####################################################
rm(list=ls())
setwd("results/results_MW/")

data <- list.files(pattern="Resultslattice")
ls <- matrix(0,ncol=2,nrow=200)
for (i in 1:length(data)){
  load(data[i])
  ls <- ls + res[[1]]
}
pz1 <- ls/length(data)

data <- list.files(pattern="Resultsmax-max-betweenness")
ls <- matrix(0,ncol=2,nrow=200)
for (i in 1:length(data)){
  load(data[i])
  ls <- ls + res[[1]]
}
pz2 <- ls/length(data)


data <- list.files(pattern="Resultsmax-mean-betweenness")
ls <- matrix(0,ncol=2,nrow=200)
for (i in 1:length(data)){
  load(data[i])
  ls <- ls + res[[1]]
}
pz3 <- ls/length(data)

data <- list.files(pattern="Resultsmax-mean-clustering")
ls <- matrix(0,ncol=2,nrow=200)
for (i in 1:length(data)){
  load(data[i])
  ls <- ls + res[[1]]
}
pz4 <- ls/length(data)


data <- list.files(pattern="Resultsmax-var-constraint")
ls <- matrix(0,ncol=2,nrow=200)
for (i in 1:length(data)){
  load(data[i])
  ls <- ls + res[[1]]
}
pz5 <- ls/length(data)

data <- list.files(pattern="Resultsfully")
ls <- matrix(0,ncol=2,nrow=200)
for (i in 1:length(data)){
  load(data[i])
  ls <- ls + res[[1]]
}
pz6 <- ls/length(data)

data <- list.files(pattern="Resultsmax-max-closeness")
ls <- matrix(0,ncol=2,nrow=200)
for (i in 1:length(data)){
  load(data[i])
  ls <- ls + res[[1]]
}
pz7 <- ls/length(data)

data <- list.files(pattern="Resultsmin-max-closeness")
ls <- matrix(0,ncol=2,nrow=200)

for (i in 1:length(data)){
  load(data[i])
  ls <- ls + res[[1]]
}
pz8 <- ls/length(data)


data <- list.files(pattern="Resultsmin-mean-betweenness")
ls <- matrix(0,ncol=2,nrow=200)
for (i in 1:length(data)){
  load(data[i])
  ls <- ls + res[[1]]
}
pz9 <- ls/length(data)


data <- list.files(pattern="Resultsmin-mean-clustering")
ls <- matrix(0,ncol=2,nrow=200)

for (i in 1:length(data)){

  load(data[i])
  ls <- ls + res[[1]]
}
pz10 <- ls/length(data)


bestnet <- cbind(pz1[,1],
                 pz2[,1],
                 pz3[,1],
                 pz4[,1],
                 pz5[,1],
                 pz6[,1],
                 pz7[,1],
                 pz8[,1],
                 pz9[,1],
                 pz10[,1])
majnet <- cbind(pz1[,2],
                pz2[,2],
                pz3[,2],
                pz4[,2],
                pz5[,2],
                pz6[,2],
                pz7[,2],
                pz8[,2],
                pz9[,2],
                pz10[,2])

colnames(bestnet) <- colnames(majnet) <- c("lattice",
                                           "max_max_betweenness",
                                           "max_mean_betweenness",
                                           "max_mean_clustering",
                                           "max_var_constraint",
                                           "fully_connected",
                                           "max_max_closeness",
                                           "min_max_closeness",
                                           "min_mean_betweenness",
                                           "min_mean_clustering")


bestnet <- as.data.frame(bestnet)
majnet <- as.data.frame(majnet)

best_ineff <- bestnet %>% select(lattice:max_var_constraint) %>% rowMeans()
best_eff <- bestnet %>% select(fully_connected:min_mean_clustering) %>% rowMeans()
maj_ineff <- majnet %>% select(lattice:max_var_constraint) %>% rowMeans()
maj_eff <- majnet %>% select(fully_connected:min_mean_clustering) %>% rowMeans()

results <- cbind(best_ineff,best_eff,maj_ineff,maj_eff)

#best
ineff_up <- bestnet %>% mutate(x=1:200) %>% filter(x==200) %>% select(lattice:max_var_constraint)  %>% which.max()
ineff_low <- bestnet %>% mutate(x=1:200) %>% filter(x==200) %>% select(lattice:max_var_constraint)  %>% which.min()

eff_up <- bestnet %>% mutate(x=1:200) %>% filter(x==200) %>% select(fully_connected:min_mean_clustering)  %>% which.max()
eff_low <- bestnet %>% mutate(x=1:200) %>% filter(x==200) %>% select(fully_connected:min_mean_clustering)  %>% which.min()

beub <-  bestnet %>% select(fully_connected:min_mean_clustering) %>% select(eff_up)
belb <- bestnet %>% select(fully_connected:min_mean_clustering) %>% select(eff_low)

biub<-  bestnet  %>% select(lattice:max_var_constraint) %>% select(ineff_up)

bilb <- bestnet  %>% select(lattice:max_var_constraint) %>% select(ineff_low)

#majority
ineff_up <- majnet %>% mutate(x=1:200) %>% filter(x==200) %>% select(lattice:max_var_constraint)  %>% which.max()
ineff_low <- majnet %>% mutate(x=1:200) %>% filter(x==200) %>% select(lattice:max_var_constraint)  %>% which.min()

eff_up <- majnet %>% mutate(x=1:200) %>% filter(x==200) %>% select(fully_connected:min_mean_clustering)  %>% which.max()
eff_low <- majnet %>% mutate(x=1:200) %>% filter(x==200) %>% select(fully_connected:min_mean_clustering)  %>% which.min()

meub <-  majnet %>% select(fully_connected:min_mean_clustering) %>% select(eff_up)
melb <- majnet %>% select(fully_connected:min_mean_clustering) %>% select(eff_low)

miub <-  majnet %>% select(lattice:max_var_constraint) %>% select(ineff_up)
milb <-  majnet %>% select(lattice:max_var_constraint) %>% select(ineff_low)


#create plot
melt_df <- melt(results)
melt2 <- cbind(melt_df, as.vector(c(as.numeric(unlist(belb)),
                                    as.numeric(unlist(bilb)),
                                    as.numeric(unlist(melb)),
                                    as.numeric(unlist(milb)))),
               as.vector(c(as.numeric(unlist(beub)),
                           as.numeric(unlist(biub)),
                           as.numeric(unlist(meub)),
                           as.numeric(unlist(miub)))))
colnames(melt2) <- c("Time",
                     "Strategy",
                     "value",
                     "LB",
                     "UB")

p1 <- melt2 %>%
  mutate(x=rep(1:200,4),strat=rep(c("Strategy: best member (s=3)",
                                    "Strategy: conformity (s=3)"),each=400 ) )

df1 <- p1[p1$Strategy=="best_eff",]
df2 <- p1[p1$Strategy=="best_ineff",]
df3 <- p1[p1$Strategy=="maj_eff",]
df4 <- p1[p1$Strategy=="maj_ineff",]

p1$Strategy <- rep(rep(c("Inefficient networks","Efficient networks"),2),each=200)



#sds

data <- list.files(pattern="Resultslattice")
ls <- matrix(0,ncol=2,nrow=length(data))
for (i in 1:length(data)){
  load(data[i])
  ls[i,] <- res[[1]][200,]
}
sd1 <- ls

data <- list.files(pattern="Resultsmax-max-betweenness")
ls <- matrix(0,ncol=2,nrow=length(data))
for (i in 1:length(data)){
  load(data[i])
  ls[i,] <- res[[1]][200,]
}
sd2 <- ls

data <- list.files(pattern="Resultsmax-mean-betweenness")
ls <- matrix(0,ncol=2,nrow=length(data))
for (i in 1:length(data)){
  load(data[i])
  ls[i,] <- res[[1]][200,]
}
sd3 <- ls

data <- list.files(pattern="Resultsmax-mean-clustering")
ls <- matrix(0,ncol=2,nrow=length(data))
for (i in 1:length(data)){
  load(data[i])
  ls[i,] <- res[[1]][200,]
}
sd4 <- ls

data <- list.files(pattern="Resultsmax-var-constraint")
ls <- matrix(0,ncol=2,nrow=length(data))
for (i in 1:length(data)){
  load(data[i])
  ls[i,] <- res[[1]][200,]
}
sd5 <- ls

data <- list.files(pattern="Resultsfully")
ls <- matrix(0,ncol=2,nrow=length(data))
for (i in 1:length(data)){
  load(data[i])
  ls[i,] <- res[[1]][200,]
}
sd6 <- ls

data <- list.files(pattern="Resultsmax-max-closeness")
ls <- matrix(0,ncol=2,nrow=length(data))
for (i in 1:length(data)){
  load(data[i])
  ls[i,] <- res[[1]][200,]
}
sd7 <- ls

data <- list.files(pattern="Resultsmin-max-closeness")
ls <- matrix(0,ncol=2,nrow=length(data))
for (i in 1:length(data)){
  load(data[i])
  ls[i,] <- res[[1]][200,]
}
sd8 <- ls

data <- list.files(pattern="Resultsmin-mean-betweenness")
ls <- matrix(0,ncol=2,nrow=length(data))
for (i in 1:length(data)){
  load(data[i])
  ls[i,] <- res[[1]][200,]
}
sd9 <- ls

data <- list.files(pattern="Resultsmin-mean-clustering")
ls <- matrix(0,ncol=2,nrow=length(data))
for (i in 1:length(data)){
  load(data[i])
  ls[i,] <- res[[1]][200,]
}
sd10 <- ls

sdZ <-  rbind(apply(sd1,2,sd),
              apply(sd2,2,sd),
              apply(sd3,2,sd),
              apply(sd4,2,sd),
              apply(sd5,2,sd),
              apply(sd6,2,sd),
              apply(sd7,2,sd),
              apply(sd8,2,sd),
              apply(sd9,2,sd),
              apply(sd10,2,sd))


avg_best <- bestnet
avg_maj <- majnet
last_best <- avg_best[200,]
last_maj <- avg_maj[200,]
sd_best <- apply(bestnet,2,sd)
sd_maj <- apply(majnet,2,sd)


df <- cbind(as.numeric(last_best),as.numeric(last_maj))
mdf <- melt(df)
mdf <- cbind(mdf,rep(c("Strategy: best member (s=3)","Strategy: conformity (s=3)"),each=10),
             rep(c("a","b","a","b"),each=5))

SE_best <- sdZ[,1]/sqrt(1000)
SE_conf <- sdZ[,2]/sqrt(1000)

mdf <- cbind(mdf,c(SE_best,SE_conf))

colnames(mdf) <- c("variable","condition","value","network","net_type","SE")

mdf$variable <- factor(mdf$variable)
levels(mdf$variable)  <- c("Lattice",
                           "Max max betweenness",
                           "Max mean betweenness",
                           "Max mean clustering",
                           "Max var constraint",
                           "Fully connected",
                           "Max max closeness",
                           "Min max closeness",
                           "Min mean betweenness",
                           "Min mean clustering")



plot1 <- ggplot(p1,aes(x=Time,y=value,color=Strategy,linetype=Strategy))+
  geom_ribbon(data=df1,aes(x = Time,ymin = LB, ymax = UB), inherit.aes = FALSE,fill = "#00BFC4",alpha=0.3)+
  geom_ribbon(data=df2,aes(x = Time,ymin = LB, ymax = UB), inherit.aes = FALSE,fill = "#F8766D",alpha=0.3)+
  geom_ribbon(data=df3,aes(x = Time,ymin = LB, ymax = UB), inherit.aes = FALSE,fill = "#00BFC4",alpha=0.3)+
  geom_ribbon(data=df4,aes(x = Time,ymin = LB, ymax = UB), inherit.aes = FALSE,fill = "#F8766D",alpha=0.3)+
  geom_line(size=1.5)+ facet_wrap(~strat,scales="free_y")+
  scale_linetype_manual(values=c(1,2))+
  scale_y_continuous(breaks=seq(0,0.9,0.1),labels=seq(0,0.9,0.1),limits=c(0,0.9)) +
  theme(strip.background = element_blank()) +
  scale_color_manual(values=c("#F8766D","#00BFC4"))+
  xlab("Time") + ylab("Average performance")+
  theme(legend.position = "bottom",
        legend.title=element_blank(),
        legend.text=element_text(size=15),
        legend.key.size=unit(1.5,"cm"))

levels(mdf$net_type) <- c("Inefficient networks","Efficient networks")
mdf$net_type <- factor(mdf$net_type, levels=c("Efficient networks","Inefficient networks"))

plot2 <- ggplot(mdf,aes(x=variable,y=value,color=net_type,shape=net_type)) + facet_wrap(~network,scale="free_y") +
  geom_point(size=2)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(legend.position="bottom",
        legend.title=element_blank())+
  theme(strip.background = element_blank())+
  ylab("Performance at t=200")+
  xlab("")+
  geom_errorbar(aes(ymin=value-2*SE, ymax=value+2*SE), width=.2)+
  scale_y_continuous(breaks=seq(0.6,0.85,0.1),labels=seq(0.6,0.85,0.1),limits=c(0.6,0.86))+
  scale_color_manual(values=c("#F8766D","#00BFC4"))+
  scale_shape_manual(values=c(19,17))


svg("~/Desktop/MW_plot.svg",height=10,width=8)
grid.arrange(plot1,plot2,nrow=2)
dev.off()
setwd("..")