#0 - Prep vars
    problemcamps<-vector()
    problemwaves<-vector()
    badconvcamps<-vector()
    badconvwaves<-vector()
    altident<-list()
#1 - Run models
    conv<-0
    run<-0
    while(conv==0 & run<20){
        #a - run model
            tryCatch(Model <- siena07( myalgorithm, data = datasetg, effects = myeff, nbrNodes=16, useCluster=T, initC=T, returnDeps=TRUE,batch=T, verbose = F))
            run<-run+1
        #b - determine model's rate effects
            effects<-Model$effects$effectName[!(grepl("wave",Model$effects$effectName) & !grepl("x",Model$effects$effectName))]
            effects<-effects[!(grepl("condition",effects) & !grepl("x",effects))]
            rates<-Model$theta[grepl("^rate DV",effects)]
            rates2<-Model$theta[grepl("^constant friendship rate",effects)]
          #for first run, just get rid of places that don't converge well on the friendship side of things
            if(run==1){
              if(any(rates2>12)){
                #i. identify problems
                    toohigh<-(rates>12)
                    badconvcamps<-c(badconvcamps,sapply(dataset[toohigh],function(x) x$cCovars$campn[1]))
                    badconvwaves<-c(badconvwaves,sapply(dataset[toohigh],function(x) attr(x$cCovars$wave,"mean")))
                    altident[[run]]<-which(toohigh)
                #ii. remove problems
                    dataset2 <- dataset2[!toohigh]
                    datasetg <- sienaGroupCreate(dataset2)
                    source("mod.3 - Create effects for algorithm.R")
                    next()
              }
            }
        #c - if rates are too high, identify problem camps, remove them, and try again.
            if(any(rates>10)){
                #i. identify problems
                    toohigh<-(rates>10 | rates2>20)
                    problemcamps<-c(problemcamps,sapply(dataset[toohigh],function(x) x$cCovars$campn[1]))
                    problemwaves<-c(problemwaves,sapply(dataset[toohigh],function(x) attr(x$cCovars$wave,"mean")))
                    altident[[run]]<-which(toohigh)
                #ii. remove problems
                    dataset2 <- dataset2[!toohigh]
                    datasetg <- sienaGroupCreate(dataset2)
                    source("mod.3 - Create effects for algorithm.R")
            }else{
                conv<-1
            }
        SaveResults()
    }

#problem camps
    # much more likely to occur in earlier waves;
    # slightly smaller,
    # slightly worse jaccards,
    # slightly higher missingness,
    # much less correlated

#try to fix problem camps with interaction terms
    sumstats3$problem<-ifelse(paste(sumstats3$Wave,paste(sumstats3$Camp)) %in% paste(problemwaves,problemcamps),1,0)
    sumstats3$noconv<-ifelse(paste(sumstats3$Wave,paste(sumstats3$Camp)) %in% paste(badconvwaves,badconvcamps),1,0)
    
#1 - load data to make siena objects
    varlist<-readRDS("varlist.rds")
    sumvars<-readRDS("sumvars.rds")
    #NEW STEP: remove objects that were removed in mod.2
        for(i in 1:length(varlist)){
          varlist[[i]][removedids]<-NULL
        }
        for(i in 1:length(sumvars)){
          sumvars[[i]][removedids]<-NULL
        }
    list2env(varlist,globalenv())
    list2env(sumvars,globalenv())
#2 - Choose DV
    DV<-eval(parse(text=paste("dv.",options$DVname,sep="")))
#3 - NEW STEP: Create siena object that identifies problem camps
    prob<-campn
    conv<-campn
    for(i in 1:length(campn)){
      prob[[i]][]<-sumstats3$problem[i]
      conv[[i]][]<-sumstats3$noconv[i]
    }
#4 - Create RSiena objects
      dataset2<-list()
      for(i in 1:length(friendship)){ #camp 3 wave 1 generates warning
        dataset2[[i]] <- sienaDataCreate(friendship = friendship[[i]], ffriendship = ffriendship[[i]], DV = DV[[i]], gem = gem[[i]], sex = sex[[i]], age = age[[i]], edu = edu[[i]], married = married[[i]], ses = ses[[i]],campn=campn[[i]], ward1=ward1[[i]], ward2=ward2[[i]],ward3=ward3[[i]],ward4=ward4[[i]], condition=condition[[i]], leader=leader[[i]], sex.active=sex.active[[i]], sex.year=sex.year[[i]], child.violence=child.violence[[i]], child.sex.violence=child.sex.violence[[i]], camp.duration=camp.duration[[i]],wave=wave[[i]], has.children = has.children[[i]], fam.available = fam.available[[i]], hivtst.ego = hivtst.ego[[i]], hivinjf.ego = hivinjf.ego[[i]], hivadvf.ego = hivadvf.ego[[i]], knowf.ego = knowf.ego[[i]], closef.ego = closef.ego[[i]], dy.hivtstf = hivtstf[[i]], dy.hivinjf = hivinjf[[i]], dy.hivadvf = hivadvf[[i]], dy.knowf = knowf[[i]], dy.closef = closef[[i]], ever.test=ever.test[[i]],ever.ipv=ever.ipv[[i]],prob=prob[[i]])
      }
      datasetg <- sienaGroupCreate(dataset2)
#5 - create effects like usual
      source("mod.3 - Create effects for algorithm.R")
      
#6 - Time to get creative!
    #create interaction terms for all network vars and camps
        #find included effects
            effects<-myeff[myeff$include==T,]
        #find behavior effects
            effects<-effects[(max(which(grepl("^rate DV",effects$effectName)))+1):nrow(effects),]
            effects<-effects[!grepl("^rate DV",effects)]
            effects<-effects[!grepl("^constant friendship rate",effects)]
            effects$interaction1<-paste("'",effects$interaction1,"'",sep = "")
        #insert interactions for all problems
            for(i in 1:nrow(effects)){
              myeff <-eval(parse(text=paste("includeInteraction(myeff, ",effects$shortName[i],", effFrom, interaction1 = c(",(effects$interaction1[i]),",'prob'),name='DV')",sep="")))
            }
#7 - run the model
      
    # myeff <- includeInteraction(myeff, linear, effFrom, interaction1 = c('',"prob"),name="DV")
    # myeff <- includeInteraction(myeff, quad, effFrom, interaction1 = c('',paste("camp",sumstats2$Wave[1],sumstats2$campn[1],sep="")),name="DV")
    # myeff <- includeInteraction(myeff, avAlt, effFrom, interaction1 = c('friendship',paste("camp",sumstats2$Wave[1],sumstats2$campn[1],sep="")),name="DV")
    # myeff <- includeInteraction(myeff, outdeg, effFrom, interaction1 = c('friendship',paste("camp",sumstats2$Wave[1],sumstats2$campn[1],sep="")),name="DV")
    # myeff <- includeInteraction(myeff, effFrom, effFrom, interaction1 = c('sex',paste("camp",sumstats2$Wave[1],sumstats2$campn[1],sep="")),name="DV")
    # myeff <- includeInteraction(myeff, effFrom, effFrom, interaction1 = c('sex.active',paste("camp",sumstats2$Wave[1],sumstats2$campn[1],sep="")),name="DV")
    # myeff <- includeInteraction(myeff, effFrom, effFrom, interaction1 = c('child.violence',paste("camp",sumstats2$Wave[1],sumstats2$campn[1],sep="")),name="DV")
    # myeff <- includeInteraction(myeff, linear, effFrom, effFrom, interaction1 = c('','wave',paste("camp",sumstats2$Wave[1],sumstats2$campn[1],sep="")),name="DV")