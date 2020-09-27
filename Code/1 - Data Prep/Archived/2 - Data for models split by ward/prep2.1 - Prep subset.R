    sumstats2 <- readRDS("sumstats2.rds")
#Create a backup
    sumstats3 <- sumstats2
#decent N
    sumstats3 <- sumstats3[sumstats3$N > 10, ]
#decent jaccard
    sumstats3 <- sumstats3[sumstats3$jaccard > .1, ]
    if (options$DVname == "ipvoutcome_cat") {
      sumstats3 <- sumstats3[sumstats3$d.ipv > .1, ]
    }
    if (options$DVname == "gem_r15_avg") {
      sumstats3 <- sumstats3[sumstats3$d.gem > .1, ]
    }
    if (options$DVname == "testhiv_12") {
      sumstats3 <- sumstats3[sumstats3$d.hiv > .1, ]
    }
    if (options$DVname == "alc_freq") {
      sumstats3 <- sumstats3[sumstats3$d.alc > .1, ]
    }

    
    keeps<-sumstats3[,c("Camp","Wave")]
    saveRDS(keeps,"keeps.rds")
    
    
    
   # table(df$ward[df$camp%in%sumstats3$Camp[sumstats3$Wave==1]])
   # table(df$ward[df$camp%in%sumstats3$Camp[sumstats3$Wave==3]])
    # sumstats3<-sumstats3[sumstats3$d.alc>.15,]
  
    # sum(sumstats3$N1)  
    
##decent coverage
#   dataset2<-dataset2[sumstats3$missing<.2]
#   sumstats3<-sumstats3[sumstats3$missing<.2,]
#   dataset2<-dataset2[!sapply(dataset2,is.null)]
##decent behavior heterogeneity
#   dataset2<-dataset2[sumstats3$d.ipv>0]
#   sumstats3<-sumstats3[sumstats3$d.ipv>0,]
#   dataset2<-dataset2[!sapply(dataset2,is.null)]
    
