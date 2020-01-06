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
            sbadconvwaves<-c(sapply(dataset2[superhigh],function(x) attr(x$cCovars$wave,"mean")),sbadconvcamps)
            dataset2<-dataset2[setdiff(1:length(dataset2),which(superhigh))]
            datasetg <- sienaGroupCreate(dataset2)
            source("modf.3 - Create effects for algorithm.R")
            next()
        }
        
    # #ii.otherwise, identify places that don't converge well
    #     if(any(rates2>median(rates2)*1.5)){
    #       toohigh<-(rates2>median(rates2)*1.5)
    #       badconvcamps<-c(sapply(dataset2[toohigh],function(x) x$cCovars$campn[1]))
    #       badconvwaves<-c(sapply(dataset2[toohigh],function(x) attr(x$cCovars$wave,"mean")))
          conv=1
    # }
    }    
    
    
    
    
    
#3 - add identifiers of badconv to sumstats3
    sumstats2$badconv<-ifelse(paste(sumstats2$Wave,paste(sumstats2$Camp)) %in% paste(badconvwaves,badconvcamps),1,0)
    sumstats2$excluded<-ifelse(paste(sumstats2$Wave,paste(sumstats2$Camp)) %in% paste(sapply(dataset2,function(x) attr(x$cCovars$wave,"mean")),sapply(dataset2,function(x) x$cCovars$campn[1])),0,1)
    removedids<-which(sumstats2$excluded==1)
    sumstats4<-sumstats2[sumstats2$excluded==0,]
    
#4 - add these identifiers as a new siena object 
    source("modf.5.5 - Add Siena Vars.R")

#5 - create interaction terms for all network vars and camps
    #find included effects
        effects<-myeff[myeff$include==T,]
    #find behavior effects
        effects<-effects[!grepl("^constant friendship rate",effects$effectName),]
        effects<-effects[!grepl("^unspecified",effects$effectName),]
    #insert interactions for all problems
        #rename interaction1 to include quotation marks
            effects$interaction1<-paste("'",effects$interaction1,"'",sep = "")
        #create new interactions for each var
            for(i in 1:nrow(effects)){
                myeff <-eval(parse(text=paste("includeInteraction(myeff, ",effects$shortName[i],", egoX, interaction1 = c(",(effects$interaction1[i]),",'conv'),name='friendship')",sep="")))
            }
                
#7 - run the model
    Model <- siena07( myalgorithm, data = datasetg, effects = myeff, nbrNodes=7, useCluster=T, initC=T, returnDeps=TRUE,batch=T, verbose = F)
            #for behaviors
                #myeff <-eval(parse(text=paste("includeInteraction(myeff, ",effects$shortName[i],", effFrom, interaction1 = c(",(effects$interaction1[i]),",'prob'),name='DV')",sep="")))
#8 - kill weak effects
    effects<-myeff[myeff$include==T,]
    effects<-effects[!grepl("^constant friendship rate",effects$effectName),]
    myeff[myeff$include==T,][!grepl("^constant friendship rate",effects$effectName),][abs(Model$theta[!grepl("^constant friendship rate",effects$effectName)])<abs(Model$se[!grepl("^constant friendship rate",effects$effectName)]),]$include<-F
#9 - run the model
    Model <- siena07( myalgorithm, data = datasetg, effects = myeff, nbrNodes=7, useCluster=T, initC=T, returnDeps=TRUE,batch=T, verbose = F)
#10 - slice out some more non-convergers
    effects<-Model$effects$effectName[!(grepl("wave",Model$effects$effectName) & !grepl("x",Model$effects$effectName))]
    effects<-effects[!(grepl("condition",effects) & !grepl("x",effects))]
    rates2<-Model$theta[grepl("^constant friendship rate",effects)]
    superhigh<-(rates2>15)
    if(any(superhigh)){
      sbadconvcamps<-c(sapply(dataset2[superhigh],function(x) x$cCovars$campn[1]),sbadconvcamps)
      sbadconvwaves<-c(sapply(dataset2[superhigh],function(x) attr(x$cCovars$wave,"mean")),sbadconvwaves)
      dataset2<-dataset2[setdiff(1:length(dataset2),which(superhigh))]
      datasetg <- sienaGroupCreate(dataset2)
      source("modf.3 - Create effects for algorithm.R")
      #find included effects
          effects<-myeff[myeff$include==T,]
      #find behavior effects
          effects<-effects[!grepl("^constant friendship rate",effects$effectName),]
          effects<-effects[!grepl("^unspecified",effects$effectName),]
      #insert interactions for all problems
          #rename interaction1 to include quotation marks
              effects$interaction1<-paste("'",effects$interaction1,"'",sep = "")
          #create new interactions for each var
              for(i in 1:nrow(effects)){
                  myeff <-eval(parse(text=paste("includeInteraction(myeff, ",effects$shortName[i],", egoX, interaction1 = c(",(effects$interaction1[i]),",'conv'),name='friendship')",sep="")))
              }
    }
#11 - run the model
    Model <- siena07( myalgorithm, data = datasetg, effects = myeff, nbrNodes=7, useCluster=T, initC=T, returnDeps=TRUE,batch=T, verbose = F)
            #for behaviors
                #myeff <-eval(parse(text=paste("includeInteraction(myeff, ",effects$shortName[i],", effFrom, interaction1 = c(",(effects$interaction1[i]),",'prob'),name='DV')",sep="")))
#12 - kill weak effects
    effects<-myeff[myeff$include==T,]
    #effects<-effects[!grepl("^constant friendship rate",effects$effectName),]
    myeff[myeff$include==T,][!grepl("^constant friendship rate",effects$effectName),][abs(Model$theta[!grepl("^constant friendship rate",effects$effectName)])<(2.5*abs(Model$se[!grepl("^constant friendship rate",effects$effectName)])),]$include<-F
#13 - run the model
    Model <- siena07( myalgorithm, data = datasetg, effects = myeff, nbrNodes=7, useCluster=T, initC=T, returnDeps=TRUE,batch=T, verbose = F)
#    Model <- siena07( myalgorithm, data = datasetg, effects = myeff, nbrNodes=7, useCluster=T, initC=T, returnDeps=TRUE,batch=T, verbose = F,prevAns=Model)

    dv.ipvpsych_cat<-list()
    alc_evr<-list()
    alc_frq<-list()
    alc_bing<-list()
    drnk_frq<-list()
    
    table(df$b_ipvpsych_cat)
    table(df$m_ipvpsych_cat)
    table(df$e_ipvpsych_cat)
    table(df$b_alc_evr)
    table(df$b_alc_frq)
    table(df$b_drnk_frq)
    table(df$b_alc_bing)
    
    # myeff <- includeInteraction(myeff, linear, effFrom, interaction1 = c('',"prob"),name="DV")
    # myeff <- includeInteraction(myeff, quad, effFrom, interaction1 = c('',paste("camp",sumstats2$Wave[1],sumstats2$campn[1],sep="")),name="DV")
    # myeff <- includeInteraction(myeff, avAlt, effFrom, interaction1 = c('friendship',paste("camp",sumstats2$Wave[1],sumstats2$campn[1],sep="")),name="DV")
    # myeff <- includeInteraction(myeff, outdeg, effFrom, interaction1 = c('friendship',paste("camp",sumstats2$Wave[1],sumstats2$campn[1],sep="")),name="DV")
    # myeff <- includeInteraction(myeff, effFrom, effFrom, interaction1 = c('sex',paste("camp",sumstats2$Wave[1],sumstats2$campn[1],sep="")),name="DV")
    # myeff <- includeInteraction(myeff, effFrom, effFrom, interaction1 = c('sex.active',paste("camp",sumstats2$Wave[1],sumstats2$campn[1],sep="")),name="DV")
    # myeff <- includeInteraction(myeff, effFrom, effFrom, interaction1 = c('child.violence',paste("camp",sumstats2$Wave[1],sumstats2$campn[1],sep="")),name="DV")
    # myeff <- includeInteraction(myeff, linear, effFrom, effFrom, interaction1 = c('','wave',paste("camp",sumstats2$Wave[1],sumstats2$campn[1],sep="")),name="DV")
                