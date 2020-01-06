sbadconvcamps<-vector()
sbadconvwaves<-vector()
badconvcamps<-vector()
badconvwaves<-vector()

#1 - Run model
    conv=0
    while (conv==0) {
    Model <- siena07( myalgorithm, data = datasetg, effects = myeff, nbrNodes=25, useCluster=T, initC=T, returnDeps=TRUE,batch=T, verbose = F)
    SaveResults()
#2 - find where rate effects don't work
    effects<-Model$effects$effectName[!(grepl("wave",Model$effects$effectName) & !grepl("x",Model$effects$effectName))]
    effects<-effects[!(grepl("condition",effects) & !grepl("x",effects))]
    rates2<-Model$theta[grepl("^constant friendship rate",effects)]
    #i.if some places don't converge at all, remove and redo
        superhigh<-(rates2>15)
        if(any(superhigh)){
            sbadconvcamps<-c(sapply(dataset2[superhigh],function(x) x$cCovars$campn[1]),sbadconvcamps)
            sbadconvwaves<-c(sapply(dataset2[superhigh],function(x) attr(x$cCovars$wave,"mean")),sbadconvwaves)
            dataset2<-dataset2[setdiff(1:length(dataset2),which(superhigh))]
            datasetg <- sienaGroupCreate(dataset2)
            source("modf.3 - Create effects for algorithm.R")
            next()
        }
        conv=1
    }
    sumstats3<-sumstats3[!(paste(sumstats3$Wave,sumstats3$Camp) %in% paste(sbadconvwaves,sbadconvcamps)),]
    goodcamps<-(sumstats3[,c("Wave","Camp")])
    saveRDS(goodcamps,"goodcamps.rds")
  