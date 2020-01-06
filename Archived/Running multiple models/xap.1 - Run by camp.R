results<-list()
for(i in 1:length(dataset)){
    datasetg<-dataset[[i]]
    #1 - Create effects
        source("mod.3 - Create effects for algorithm.R")
    #4 - run model
        tryCatch({
          Model <- siena07( myalgorithm, data = datasetg, effects = myeff, nbrNodes=16, useCluster=T, initC=T, returnDeps=F,batch=T, verbose = F)
    #5 - append results
          results[[i]]<-data.frame(Effects=Model$requestedEffects$effectName,Coefficient=Model$theta)
          }, error=function(e){})
        print(i)
}

#6 - Format results
for(i in 1:length(results)){
  tryCatch({
    names(results[[i]])[2]<-i
  }, error=function(e){})
}
saveRDS(results,"sienabyward.rds")
results<-readRDS("sienabyward.rds")

results2<-results[[1]]
for(i in 2:length(results)){
  tryCatch({
    results2<-merge(results2,results[[i]],by="Effects",all=T,sort = F)
  }, error=function(e){})
}

View(results2)
#From further digging, it seems that Camp 29 simply breaks because there's no variation in age and sex within it.
    sumstats2[sapply(results,is.null),]

#Explore each effect
    library(stringr)
    temp<-data.frame(1,2,3,4,5,6,7)
    names(temp)<-names(summary(results2[,2]))
    for(i in 1:nrow(results2)){
      a<-as.numeric(unlist(summary(unlist(results2[i,-1]))))
      if(length(a)==6) a<-c(a,0)
      temp[i,]<-a
    }
    temp<-as.data.frame(sapply(temp,function(x) round(x,2)))
    row.names(temp)<-results2$a
    temp2<-temp
    temp2[temp2>0]<-1
    temp2[temp2<0]<-(-1)
    temp2$Mean<-NULL
    temp2$`NA's`<-NULL
    #Why do some camps have huge negative coefficients?
        tempdf<-results2[,c(-100,rec)<(-10)]

#Explore correlations among effects
    posres<-results2
    #convert to 1s and -1s
        #make numeric
            posres[,2:ncol(posres)]<-apply(posres[,2:ncol(posres)],2, as.numeric)
        #convert positives and negatives    
            posres[,2:ncol(posres)][posres[,2:ncol(posres)]>0]<-1
            posres[,2:ncol(posres)][posres[,2:ncol(posres)]<0]<-(-1)
        #convert to long format
            posres2<-t(posres)
            posres2<-as.data.frame(posres2)
            names(posres2)<-posres[,1]
            posres2<-posres2[-1,]
            posres2<-as.data.frame(apply(posres2,2, as.numeric))
        #split by friendship selection and behavior
            friends<-posres2[2:14]
            behaves<-posres2[17:26]
        #Run correlations
            cormat<-cor(friends,use="complete")        
    
#
    cormat<-cor((posres2[,1]),(posres2[,3]))
    
    posres[posres>1,]

    