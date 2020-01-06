#1 - Create effects
    lmyeff<-list()
    lmyeff[[1]] <- includeEffects(myeff, avAlt, interaction1 = "friendship", name = "DV")
    lmyeff[[1]] <- includeInteraction(lmyeff[[1]], avAlt, effFrom, interaction1 = c("friendship","fam.available"), name = "DV")
      
#2 - Run models
    for(i in 1:length(lmyeff)){
        if(is.null(lmyeff[[i]])){
          next()
        }
      tryCatch(Model <- siena07(myalgorithm, data = datasetg, effects = lmyeff[[i]], nbrNodes=16, useCluster=T, initC=T, returnDeps=TRUE, batch=T, verbose = F), error=function(e){})
      SaveResults()    
    }
#3 - Save VM memory
    gc()
    closeAllConnections()