ml<-readRDS("ml.rds")
datasetg<-readRDS("datasetg.rds")


#GoF
#End of run all functions
j=1
q=7
#10 - Check outdegree GoF
MHDlist<-list()
MHD<-list()
resultslist<-list()
results<-list()
for(q in 1:length(ml)){  #for each model I want to compare
    print(q)
    Model<-ml[[q]]
    for(j in 1:3){ #for each statistic I care about
      print(paste(q,j))
        #a - Because GoF only shows GoF by group, run on each group.
            registerDoParallel(makeCluster(length(datasetg)))
            if(j==1){
                FIT <- foreach(i=1:length(datasetg), .packages="RSiena") %dopar% {
                  sienaGOF(Model, verbose=TRUE, varName="friendship", cumulative=FALSE, groupName=i,OutdegreeDistribution) #levls=0:3                }
                }
            }
            if(j==2){
                FIT <- foreach(i=1:length(datasetg), .packages="RSiena") %dopar% {
                  sienaGOF(Model, verbose=TRUE, varName="friendship", cumulative=FALSE, groupName=i,IndegreeDistribution) 
                }
            }
            if(j==3){
                FIT <- foreach(i=1:length(datasetg), .packages="RSiena") %dopar% {
                  sienaGOF(Model, verbose=TRUE, varName="DV", cumulative=FALSE,groupName=i,BehaviorDistribution)
                }
            }
            closeAllConnections()

        #b - plot GoF for first group
            plot(FIT[[8]])
        #c - append all GoF stats. The significance test is wrong, but this visualizes where GoF isn't good.
            #i - Get key GoF stats for each group
                a<-lapply(FIT, "[[", c("Joint"))
                a1<-lapply(a, "[[", c("Simulations"))
                a2<-lapply(a, "[[", c("Observations"))
                
            #ii - merge stats
                g1<-Reduce("+",a2)
                g<-Reduce("+",a1)
                    #the above suggest that I might want to adjust the model so that it doesn't predict any people with 4 friends...
                #temp cheat for visualization
                    if(j==1){
                        g[,4]<-g[,4]+g[,5]
                        g[,5]<-0
                    }
            #iii = put stats df
                a<-FIT[[1]]
                MHD[[j]]<-sapply(FIT,function(x) x$Joint$ObservedTestStat)
                # FIT[[1]]$Joint$ObservedTestStat # this is the MHD value
                a$Joint$SimulatedTestStat<-NULL
                a$Joint$ObservedTestStat<-NULL
                a$Joint$TwoTailed<-NULL
                a$Joint$Rank<-NULL
                a$Joint$InvCovSimStats<-NULL
                a$Joint$Simulations<-g
                a$Joint$Observations<-g1
            #iv - plot
                # plot(a)
            #v - save results to a single object
                results[[j]]<-a
}
resultslist[[q]]<-results
MHDlist[[q]]<-MHD
}


#get a sense of the standard deviations of estimates (the means should be all about the same)
    for(i in 1:length(resultslist)) print(paste(i,round(100*sd(resultslist[[i]][[3]]$Joint$Simulations[,2]/sum(g1)),2)))
    for(i in 1:length(resultslist)) print(paste(i,round(100*mean(resultslist[[i]][[3]]$Joint$Simulations[,2]/sum(g1)),2)))

#prep data for plotting
    worst<-resultslist[[1]][[3]]$Joint$Simulations[,2]
    best<-resultslist[[3]][[3]]$Joint$Simulations[,2]
    worst<-worst/sum(g1)
    best<-best/sum(g1)
    df<-data.frame(estimate=c(worst,best),
                   group=c(rep("Without Norms",length(worst)),rep("With Injunctive Norms",length(worst)))
    )
    
#conduct an f-test of difference in variances. Significance = one model is actually better than the other...?
    var.test(resultslist[[3]][[3]]$Joint$Simulations[,2],resultslist[[1]][[3]]$Joint$Simulations[,2])

#plot distribution of simulated predictions data
    # png(file="C:/Users/bda13/Desktop/Racial Disparities in Net Worth.png", height=4, width=7,units = "in",res = 300)
      ggplot()+
        geom_density(data = df, aes(x = estimate,fill=group),alpha=1,color="white")+
        scale_fill_grey(start = .8, end = 0)+
        geom_density(data=df[df$group=="With Injunctive Norms",],aes(x=estimate),alpha=1,fill="grey90")+
        geom_density(data=df[df$group=="Without Norms",],aes(x=estimate),alpha=.6,fill="black")+
        geom_density(data=df[df$group=="With Injunctive Norms",],aes(x=estimate),alpha=.5,fill="grey90")+
        theme_bw()+
        geom_vline(aes(xintercept=g1[2]/sum(g1)),color="red")+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black"),
              legend.text=element_text(size=12),
              text=element_text(family="A"),
              legend.title=element_text(size=12,face="bold"),
              title = element_text(size=12,face="bold"),
              legend.position="bottom",
              axis.text.x = element_text(size=11.5, hjust = 1),
              # axis.text.y = element_blank(),
              axis.text.y = element_text(size=11.5),
              axis.title.x= element_text(size=12,face="bold",margin = margin(t = 10, r = 0, b = 0, l = 0)),
              axis.title.y= element_text(size=13.5,face="bold",margin = margin(t = 0, r = 15, b = 0, l = 0)),
              strip.text.x = element_text(size = 14))+
              labs(x="Proportion of Sample Tested for HIV", y="Density",fill="")
        
      var.test(df[df$group=="With Injunctive Norms",]$estimate, df[df$group=="Without Norms",]$estimate, alternative = "two.sided")
      
#MHDs
  MHDlist[[7]] #the 8th camp has a bad outdegree distribution 
  a<-sapply(MHDlist,function(x) (x[[3]]))
  tdf<-data.frame(MHD=c(a),Model=rep(c("Baseline","Alter-Tested","Alter-Should","Alter-Advertised","Camp-Tested","Camp-Should","Camp-Advertised","Both-Tested","Both-Should","Both-Advertised"),each=length(MHDlist[[1]][[3]])))
  a<-sapply(MHDlist,function(x) sum(x[[3]]))
  b<-1.96*sapply(MHDlist,function(x) sd(x[[3]]))
  tdf<-data.frame(MHD=c(a),Model=(c("Baseline","Alter-Tested","Alter-Should","Alter-Advertised","Camp-Tested","Camp-Should","Camp-Advertised","Both-Tested","Both-Should","Both-Advertised")),error=b)
  tdf$Model<-factor(tdf$Model,levels=rev(c("Baseline","Alter-Tested","Alter-Should","Alter-Advertised","Camp-Tested","Camp-Should","Camp-Advertised","Both-Tested","Both-Should","Both-Advertised")))
  
  tdf$CIupper=tdf$MHD+tdf$error
  tdf$CIlower=tdf$MHD-tdf$error
  
png(file="MHD by Model.png", height=5, width=7,units = "in",res = 300)
  ggplot(tdf,aes(y=MHD,x=Model)) + 
  coord_flip()+
  geom_pointrange(aes(ymin=CIlower,ymax=CIupper),size=1.5,color="grey")+
  geom_point(size=6)+
  theme_bw()+
    scale_y_continuous(limits = c(9,21),breaks = c(10,12,14,16,18,20))+
    # ylim(c(9,20))+
    ggtitle("Joint Mahalanobis Distance by Model")+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"),
          legend.text=element_text(size=12),
          text=element_text(family="Times"),
          legend.title=element_text(size=12,face="bold"),
          title = element_text(size=12,face="bold"),
          # legend.position="bottom",
          axis.text.x = element_text(size=11.5,angle = 30, hjust = 1),
          # axis.text.y = element_blank(),
          axis.text.y = element_text(size=11.5),
          axis.title.x= element_text(size=12,face="bold",margin = margin(t = 10, r = 0, b = 0, l = 0)),
          axis.title.y= element_text(size=13.5,face="bold",margin = margin(t = 0, r = 15, b = 0, l = 0)),
          strip.text.x = element_text(size = 14))
   dev.off()
  
  # loadfonts(device = "win")
  # 
  # windowsFonts(A = windowsFont("Times New Roman"))
  