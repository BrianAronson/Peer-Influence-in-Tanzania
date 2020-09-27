# source("mod.3 - Create effects for algorithm.R")
#1 - run
    Model <- siena07(myalgorithm, data = datasetg, effects = myeff, nbrNodes=25, useCluster=T, initC=T, returnDeps=TRUE, batch=T, verbose = F)
#2 - save
    # SaveResults()
#3 - Close connections to save VM memory and cpu
    gc()
    closeAllConnections()

    printfun(Model)

    library("xtable")
    printfun<-function(Model,name=""){
        pval <- round(2*pnorm(abs(Model$theta/Model$se),lower.tail=FALSE),4) #Not sure this is correct, but should be close.
        pval <-ifelse(grepl("^constant friendship rate",Model$requestedEffects$effectName) | grepl("^rate DV",Model$requestedEffects$effectName),1,pval)
        htmldf<-data.frame(
          Name=Model$requestedEffects$effectName,
          Estimate=round(Model$theta,3),
          SE=round(Model$se,3),
          pval=ifelse(pval<.001,"***",ifelse(pval<.01,"**",ifelse(pval<.05,"*","")))
        )
        number<-suppressWarnings(max(as.numeric(gsub("[[:alpha:]]", "", list.files(path="/data/data1/bda13/Data/Shared/Tanzania/Results/Results html",pattern='*\\.html', recursive=TRUE))),na.rm=T)+1)
        number<-ifelse(is.infinite(number),1,number)
        htmlfilename<-paste("/data/data1/bda13/Data/Shared/Tanzania/Results/Results html/Model ",number,name,".html",sep="")
        print(xtable(htmldf), type="html", file=htmlfilename)
        browseURL(htmlfilename)
    }

    printfun(Model)
View(Model)[10]
# 
# #Model <- siena07(myalgorithm, data = datasetg, effects = myeff, nbrNodes=16, useCluster=T, initC=T, returnDeps=TRUE, batch=T, verbose = F,prevAns=Model)
# 
# # getwd()
# # 
# # sum(attr(datasetg$Data2$compositionChange$changes,"activeStart")[,1])
# # 
# # sapply(datasetg,function(x) sum(attr(x$compositionChange$changes,"activeStart")[,1]))
# # 
# # as.matrix(datasetg$Data31$depvars$friendship)[1:16,]
# # datasetg$Data1$compositionChange
# 
# (timetest<-sienaTimeTest(Model,effects = c(42)))
#     timetest$IndividualTest
#     Model$effects$effectName
# 
#     
#     #subset to positively correlated camps
#         datasetg <- sienaGroupCreate(dataset)
#         temp1<-(sapply(datasetg, function(x) (x$depvars$DV[,,1])))
#         temp2<-(sapply(datasetg, function(x) (x$depvars$DV[,,2])))
#         cors<-mapply(function(x,y) round(cor(x,y,use = "complete"),2), x=temp1,y=temp2)
#         a<-!is.na(cors) & cors>.05
#         sum(a)
#         dataset2<-dataset[a]
#         datasetg <- sienaGroupCreate(dataset2)
#     #subset to places that fit the model
#         effects<-Model$effects$effectName[!(grepl("wave",Model$effects$effectName) & !grepl("x",Model$effects$effectName))]
#         effects<-effects[!(grepl("condition",effects) & !grepl("x",effects))]
#         rates<-Model$theta[grepl("^rate DV",effects)]
#         toohigh<-rates>15 |rates==0
#         dataset2<-dataset2[!toohigh]
#         datasetg <- sienaGroupCreate(dataset2)
#     #camp size
#         sum1<-(sapply(datasetg, function(x) sum(!is.na(x$depvars$DV[,,1]))))
#         sum2<-(sapply(datasetg, function(x) sum(!is.na(x$depvars$DV[,,2]))))
#     #DV freq
#         freq1<-(sapply(datasetg, function(x) sum(x$depvars$DV[,,1],na.rm=T)))
#         freq2<-(sapply(datasetg, function(x) sum(x$depvars$DV[,,2],na.rm=T)))
#         size<-mapply(function(x,y) round(cor(x,y,use = "complete"),2), x=temp1,y=temp2)
#         
#         
#         
#     mean(datasetg$Data61$depvars$DV[,,2],na.rm = T)
#     
#     
#     #problem camp stats
#         summary(cors[toohigh]) #behaviors less correlated across waves
#         summary(sum1[toohigh])
#         summary(sum2[toohigh]) #barely any smaller
#         summary(freq1[toohigh])
#         summary(freq2[toohigh]) #no difference in frequency
# 
#         summary(cors)    
#         sort(cors)
#         sort(cors[!toohigh])
#         sort(cors[toohigh])
# 
# 
#         cors[a][rates>15]
#         
        
      