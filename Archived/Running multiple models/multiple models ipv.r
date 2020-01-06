#1 - run
    Model <- siena07(myalgorithm, data = datasetg, effects = myeff, nbrNodes=25, useCluster=T, initC=T, returnDeps=TRUE, batch=T, verbose = F)
#2 - save
    SaveResults()
#3 - Close connections to save VM memory and cpu
    gc()
    closeAllConnections()
#Model <- siena07(myalgorithm, data = datasetg, effects = myeff, nbrNodes=16, useCluster=T, initC=T, returnDeps=TRUE, batch=T, verbose = F,prevAns=Model)
    
    dataset2<-dataset
    conv<-F

    while(conv==F){
    Model <- siena07(myalgorithm, data = datasetg, effects = myeff, nbrNodes=25, useCluster=T, initC=T, returnDeps=TRUE, batch=T, verbose = F)
    SaveResults()
    effects<-Model$effects$effectName[!(grepl("wave",Model$effects$effectName) & !grepl("x",Model$effects$effectName))]
    effects<-effects[!(grepl("condition",effects) & !grepl("x",effects))]
    rates<-Model$theta[grepl("^rate DV",effects)]
    highest<-tail(sort(rates),3)
    rates<-ifelse(rates %in% highest, rates, 3)
    toohigh<-(rates>15)
    conv<-!any(toohigh)
    if(conv==F){
        dataset2 <- dataset2[!toohigh]
        datasetg <- sienaGroupCreate(dataset2)
        source("mod.3 - Create effects for algorithm.R")
    }
    }
    camps<-sapply(dataset2,function(x)(x$cCovars$campn[1]))
    waves<-sapply(dataset2,function(x)attr(x$cCovars$wave,"mean"))
    
#stepwise method to get good model    
    # ipvconvbeh<-sumstats2[paste(sumstats2$Wave,sumstats2$Camp)  %in% paste(waves,camps),]
    # saveRDS(ipvconvbeh,"ipvconvbeh.rds")
    # 
    
    #determine which camps to keep
        effects<-Model$effects$effectName[!(grepl("wave",Model$effects$effectName) & !grepl("x",Model$effects$effectName))]
        effects<-effects[!(grepl("condition",effects) & !grepl("x",effects))]
        rates<-Model$theta[grepl("^rate DV",effects)]
        rates<-abs(Model$tstat[grepl("^rate DV",effects)])
        
    #i. remove problems
        toohigh<-(rates>30)
        dataset2 <- dataset[!toohigh]
        datasetg <- sienaGroupCreate(dataset2)
        
        toohigh2<-(rates>15)
        dataset3 <- dataset2[!toohigh2]
        datasetg <- sienaGroupCreate(dataset3)
        
        toohigh3<-(rates>.4)
        dataset4 <- dataset3[!toohigh3]
        datasetg <- sienaGroupCreate(dataset4)

  hivcamps<-sumstats2[!toohigh,]
  saveRDS(hivcamps,"ipvcamps.rds")
  dataset[toohigh]

  
#dyadic covariates
m1 <- includeEffects(myeff, avWAlt, interaction1 = "friendship", interaction2 = "dy.hivtstf", name = "DV")
m2 <- includeEffects(myeff, avWAlt, interaction1 = "friendship", interaction2 = "dy.hivinjf", name = "DV")
m3 <- includeEffects(myeff, avWAlt, interaction1 = "friendship", interaction2 = "dy.hivadvf", name = "DV")

#
m9 <- includeEffects(myeff, effFrom, interaction1 = "hivinjf.ego", name = "DV") #yes
m10 <- includeEffects(myeff, avAlt, interaction1 = "friendship", name = "DV")



lms<-list(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16)


for(i in c(25)){
  Model <- siena07(myalgorithm, data = datasetg, effects = lms[[i]], nbrNodes=16, useCluster=T, initC=T, returnDeps=TRUE, batch=T, verbose = F)
  SaveResults()
}

Model

Model <- siena07(myalgorithm, data = datasetg, effects = m25, nbrNodes=16, useCluster=T, initC=T, returnDeps=TRUE, batch=T, verbose = F)
SaveResults()





