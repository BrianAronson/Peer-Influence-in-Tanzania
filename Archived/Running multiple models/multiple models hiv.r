# #1 - run
#     Model <- siena07(myalgorithm, data = datasetg, effects = myeff, nbrNodes=25, useCluster=T, initC=T, returnDeps=TRUE, batch=T, verbose = F)
# #2 - save
#     SaveResults()
# #3 - Close connections to save VM memory and cpu
#     gc()
#     closeAllConnections()
# #Model <- siena07(myalgorithm, data = datasetg, effects = myeff, nbrNodes=16, useCluster=T, initC=T, returnDeps=TRUE, batch=T, verbose = F,prevAns=Model)
# 
#     #determine which camps to keep
#         effects<-Model$effects$effectName[!(grepl("wave",Model$effects$effectName) & !grepl("x",Model$effects$effectName))]
#         effects<-effects[!(grepl("condition",effects) & !grepl("x",effects))]
#         rates<-Model$theta[grepl("^rate DV",effects)]
#     #i. remove problems
#         toohigh<-(rates>12)
#         dataset2 <- dataset[!toohigh]
#         datasetg <- sienaGroupCreate(dataset2)
#   hivcamps<-sumstats2[!toohigh,]
#   saveRDS(hivcamps,"hivcamps.rds")
# 
#   dataset[toohigh]
# 
# #dyadic covariates
# m1 <- includeEffects(myeff, avWAlt, interaction1 = "friendship", interaction2 = "dy.hivtstf", name = "DV")
# m2 <- includeEffects(myeff, avWAlt, interaction1 = "friendship", interaction2 = "dy.hivinjf", name = "DV")
# m3 <- includeEffects(myeff, avWAlt, interaction1 = "friendship", interaction2 = "dy.hivadvf", name = "DV")
# 
# #average alter version of dyadic covariates
# m4 <- includeEffects(myeff, effFrom, interaction1 = "hivtst.alter", name = "DV") #yes
# m5 <- includeEffects(myeff, effFrom, interaction1 = "hivadvf.alter", name = "DV") #yes
# m6 <- includeEffects(myeff, effFrom, interaction1 = "hivinjf.alter", name = "DV") #yes
# 
# #
# m4 <- includeInteraction(m4, effFrom,effFrom, interaction1 = c("hivtst.alter","sex"), name = "DV") #yes
# m5 <- includeInteraction(m5, effFrom,effFrom, interaction1 = c("hivadvf.alter","sex"), name = "DV") #yes
# m6 <- includeInteraction(m6, effFrom,effFrom, interaction1 = c("hivinjf.alter","sex"), name = "DV") #yes
# # myeff <- includeInteraction(myeff, avAlt, effFrom, interaction1 = c("friendship","sex"), name = "DV")
# 
# #ego version of dyadic covariates (i.e. do people think ego engaged in behavior)
# m7 <- includeEffects(myeff, effFrom, interaction1 = "hivtst.ego", name = "DV") #no
# m8 <- includeEffects(myeff, effFrom, interaction1 = "hivadvf.ego", name = "DV") #?
# m9 <- includeEffects(myeff, effFrom, interaction1 = "hivinjf.ego", name = "DV") #yes
# 
# #group level covariates
# m10 <- includeInteraction(myeff, linear, effFrom, interaction1 = c('','r.hiv'),name="DV") #suggestive
# m11 <- includeInteraction(myeff, avSim, effFrom, interaction1 = c('friendship','r.hiv'),name="DV") #doesn't work with interaction alone, so no
# m12 <- includeEffects(myeff, effFrom, interaction1 = "hivtst.group", name = "DV") #reverse coded, so also suggestive
# m13 <- includeInteraction(myeff, linear, effFrom, interaction1 = c('','density'),name="DV") #suggestive that dense networks are more likely to increase rates hiv testing
# m14 <- includeInteraction(myeff, avSim, effFrom, interaction1 = c('friendship','density'),name="DV") # #doesn't work with interaction alone, so no
# m15 <- includeInteraction(myeff, effFrom, effFrom, interaction1 = c('condition','density'),name="DV") #oddly no
# 
# 
# m16 <- includeInteraction(myeff, linear, effFrom, interaction1 = c('','r.hiv'),name="DV")
# m16 <- includeInteraction(m16, avSim, effFrom, interaction1 = c('friendship','r.hiv'),name="DV")
# m16 <- includeEffects(m16, avSim, interaction1 = c('friendship'),name="DV") #no
# 
# m17 <- includeInteraction(myeff, linear, effFrom, interaction1 = c('','density'),name="DV")
# m17 <- includeInteraction(m17, avSim, effFrom, interaction1 = c('friendship','density'),name="DV")
# m17 <- includeEffects(m17, avSim, interaction1 = c('friendship'),name="DV") #no
# 
# m18 <- includeEffects(myeff, effFrom, interaction1 = "hivtst.group", name = "DV")
# m18 <- includeEffects(m18, effFrom, interaction1 = "hivtst.alter", name = "DV") #this works in conjunction with the group level variable
# 
# m19 <- includeInteraction(myeff, linear, effFrom, interaction1 = c('','r.hivtst'),name="DV")
# m20 <- includeInteraction(myeff, linear, effFrom, interaction1 = c('','r.hivinj'),name="DV")
# m21 <- includeInteraction(myeff, linear, effFrom, interaction1 = c('','r.hivadv'),name="DV")
# m22 <- includeEffects(myeff, avSim, interaction1 = c('friendship'),name="DV") #no
# 
# 
# m23 <- includeEffects(myeff, effFrom, interaction1 = "hivtst.alter", name = "DV") #yes
# m23 <- includeInteraction(m23, linear, effFrom, interaction1 = c('','r.hivtst'),name="DV")
# m24 <- includeEffects(myeff, effFrom, interaction1 = "hivadvf.alter", name = "DV") #yes
# m24 <- includeInteraction(m24, linear, effFrom, interaction1 = c('','r.hivadv'),name="DV")
# m25 <- includeEffects(myeff, effFrom, interaction1 = "hivinjf.alter", name = "DV") #yes
# m25 <- includeInteraction(m25, linear, effFrom, interaction1 = c('','r.hivinj'),name="DV")
# 
# m25 <- includeEffects(m25, egoX, altX, simX, interaction1 = "hivinjf.alter")
# m25 <- includeInteraction(m25, effFrom, effFrom, interaction1 = c('hivinjf.alter','r.hivinj'),name="DV")
# 
# m25 <- includeInteraction(m25, linear, effFrom, interaction1 = c('','r.hivinj'),name="DV")
# m25 <- includeEffects(m25, egoX, altX, simX, interaction1 = "hivinjf.alter")
# 
# m25 <- includeEffects(myeff, avAlt, interaction1 = "friendship", name = "DV")
# 
# lms<-list(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16,m17,m18,m19,m20,m21,m22,m23,m24,m25)

m0 <- myeff
m1 <- includeEffects(myeff, avSim, interaction1 = c('friendship'),name="DV") #no
m2 <- includeEffects(myeff, effFrom, interaction1 = "hivtst.alter", name = "DV") #yes
m3 <- includeEffects(myeff, effFrom, interaction1 = "hivadvf.alter", name = "DV") #yes
m4 <- includeEffects(myeff, effFrom, interaction1 = "hivinjf.alter", name = "DV") #yes
m5 <- includeInteraction(myeff, linear, effFrom, interaction1 = c('','r.hivtst'),name="DV")
m6 <- includeInteraction(myeff, linear, effFrom, interaction1 = c('','r.hivinj'),name="DV")
m7 <- includeInteraction(myeff, linear, effFrom, interaction1 = c('','r.hivadv'),name="DV")
m8 <- includeEffects(myeff, effFrom, interaction1 = "hivtst.alter", name = "DV") #yes
m8 <- includeInteraction(m8, linear, effFrom, interaction1 = c('','r.hivtst'),name="DV")
m9 <- includeEffects(myeff, effFrom, interaction1 = "hivadvf.alter", name = "DV") #yes
m9 <- includeInteraction(m9, linear, effFrom, interaction1 = c('','r.hivadv'),name="DV")
m10 <- includeEffects(myeff, effFrom, interaction1 = "hivinjf.alter", name = "DV") #yes
m10 <- includeInteraction(m10, linear, effFrom, interaction1 = c('','r.hivinj'),name="DV")

lms<-list(m0,m1,m2,m3,m4,m5,m6,m7,m8,m9,m10)

ml<-list()
for(i in 1:c(length(lms))){
  ml[[i]] <- siena07(myalgorithm, data = datasetg, effects = lms[[i]], nbrNodes=16, useCluster=T, initC=T, returnDeps=TRUE, batch=T, verbose = F)
  Model<-ml[[i]]
  SaveResults()
  gc()
  closeAllConnections()
}

ml[[11]]


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




i=2
for(i in 1:c(length(lms))){
  ml[[i]] <- siena07(myalgorithm, data = datasetg, effects = lms[[i]], nbrNodes=16, useCluster=T, initC=T, returnDeps=TRUE, batch=T, verbose = F)
  Model<-ml[[i]]
  printfun(Model)
  gc()
  closeAllConnections()
}


m0 <- myeff
m0 <- includeEffects(m0, avSim, interaction1 = c('friendship'),name="DV") #no
m1 <- includeEffects(myeff, avWAlt, interaction1 = "friendship", interaction2 = "dy.hivtstf", name = "DV")
m2 <- includeEffects(myeff, avWAlt, interaction1 = "friendship", interaction2 = "dy.hivinjf", name = "DV")
m3 <- includeEffects(myeff, avWAlt, interaction1 = "friendship", interaction2 = "dy.hivadvf", name = "DV")
