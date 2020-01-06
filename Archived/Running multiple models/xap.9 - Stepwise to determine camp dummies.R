#1 - intial run
    Model <- siena07(myalgorithm, data = datasetg, effects = myeff, nbrNodes=16, useCluster=T, initC=T, returnDeps=TRUE, batch=T, verbose = F)
#2 - if failed to converge, kill linear combinations and run again
    if(is.na(Model$tconv.max)){
      conv<-0
      counter<-0
        while(conv==0 & counter<20){
        #determine names of coefficients,
            temp<-Model$effects$effectName
            temp<-temp[!grepl("^DV: effect from get",Model$effects$effectName)]
        #find names of camp effects
            campeff<-grepl("dfname | linear",temp)
        #find 3 smallest effect sizes
            small<-head(Model$theta[campeff][order(abs(Model$theta[campeff]))],3)
            index<-which(Model$theta %in% small)
        #remove them from model
            myeff[myeff$include==T & myeff$fix==F,][index,]$include<-F
        #model again
            Model <- tryCatch(siena07(myalgorithm, data = datasetg, effects = myeff, nbrNodes=16, useCluster=T, initC=T, returnDeps=TRUE, batch=T, verbose = F))
        #check if converged; if converged, break loop.
            conv<-ifelse(is.na(Model$tconv.max),0,1)
        #add to counter; break after 20 tries
            counter<-counter+1
        }
    }
#3 - save results
    SaveResults()