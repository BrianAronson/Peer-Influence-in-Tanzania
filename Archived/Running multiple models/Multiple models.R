#ipv
    #set DV
      options$DVname<-"ipvoutcome_cat"
    #prep data
    {
      source("prep.1 - Load libraries.R")
      source("prep2.1 - Prep subset.R")#need to manually edit
      source("prep.2 - Load data.R")  
      source("prep2.3 - Prep network data.R")
      source("prep2.4 - Prep all other dyadic covariates.R")
      source("prep2.5 - Prep non-network data.R")
      source("prep2.6 - Prep full friendship networks.R")
      source("prep2.7 - Remove individuals from waves 2-3.R")
      source("prep2.9 - Create summary info and save.R")
      source("prep2.10 - Create more camp level dummies.R")
    }
      source("mod.1 - Load and create RSiena objects.R")
      source("mod.3 - Create effects for algorithm.R")
    #choose effects to always include
      myeff <- includeInteraction(myeff, linear, effFrom, interaction1 = c('','ward2'),name="DV")
      myeff <- includeInteraction(myeff, linear, effFrom, interaction1 = c('','ward3'),name="DV")
      myeff <- includeInteraction(myeff, linear, effFrom, interaction1 = c('','ward4'),name="DV")
      myeff <- includeInteraction(myeff, linear, effFrom, interaction1 = c('','wave'),name="DV")
      myeff <- includeEffects(myeff, effFrom, interaction1 = "sex", name = "DV")
      myeff <- includeEffects(myeff, effFrom, interaction1 = "sex.active", name = "DV")
      myeff <- includeEffects(myeff, effFrom, interaction1 = "alc_frq", name = "DV")
      myeff <- includeEffects(myeff, effFrom, interaction1 = "edu", name = "DV")
    #choose effects to test
      m0<-NA;m1<-NA;m2<-NA;m3<-NA;m4<-NA;m5<-NA;m6<-NA;m7<-NA;m8<-NA;m9<-NA;m10
      # m0 <- includeEffects(myeff, avSim, interaction1 = "friendship", name = "DV")
      m1 <- includeEffects(myeff, avAlt, interaction1 = "friendship", name = "DV")
      m2 <- includeEffects(myeff, avAltDist2, interaction1='friendship', name='DV')
      m3 <- includeEffects(myeff, avSimPopAlt, interaction1='friendship', name='DV')
      m4 <- includeInteraction(myeff, avSim, effFrom, interaction1 = c("friendship","sex"), name = "DV")
      m4 <- includeEffects(m4, avSim, interaction1 = "friendship", name = "DV")
      m5 <- includeInteraction(myeff, avSim, effFrom, interaction1 = c("friendship","sex.year"), name = "DV")
      m5 <- includeEffects(m5, avSim, interaction1 = "friendship", name = "DV")
      m6 <- includeEffects(myeff, avXAlt, interaction1='sex', interaction2='friendship', name='DV')
      m7 <- includeEffects(myeff, avXAlt, interaction1='sex', interaction2='friendship', name='DV')
      m7 <- includeEffects(m7, avAlt, interaction1 = "friendship", name = "DV")
      m8 <- includeEffects(myeff, avXAlt, interaction1='sex', interaction2='friendship', name='DV')
      m8 <- includeEffects(m8, avSim, interaction1 = "friendship", name = "DV")
      m9 <- includeEffects(myeff, avAlt, interaction1 = "friendship", name = "DV")
      m9 <- includeInteraction(m9, avAlt, effFrom, interaction1 = c("friendship","sex"), name = "DV")
      m9 <- includeInteraction(m9, avAlt, effFrom, interaction1 = c("friendship","sex.active"), name = "DV")
      m10 <- includeEffects(myeff, avSim, interaction1 = "friendship", name = "DV")
      m10 <- includeInteraction(m10, avSim, effFrom, interaction1 = c("friendship","sex"), name = "DV")
      m10 <- includeInteraction(m10, avSim, effFrom, interaction1 = c("friendship","sex.active"), name = "DV")
      # m6 <- includeInteraction(myeff, avAlt, effFrom, interaction1 = c("friendship","sex.active"), name = "DV")
      # m8 <- includeEffects(myeff, avXAlt, interaction1='leader', interaction2='friendship', name='DV')
      # m9 <- includeEffects(myeff, effFrom, interaction1 = "gem", name = "DV")
      # m9 <- includeEffects(m9, avAlt, interaction1 = "friendship", name = "DV")
      # m10 <- includeEffects(myeff, avAltPop, interaction1 = "friendship", name = "DV")
      lms<-list(m0,m1,m2,m3,m4,m5,m6,m7,m8,m9,m10)
      ml<-list()
    #set file names
      names<-paste("ipv",c("avsim","avalt","avaltdist2","avsimpopalt","avsim*sex","avsim*sexyear","sexalt","sexalt+avalt","sexalt+avsim","avalt*int","avsim*int"))
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
      kill<-sapply(lms,function(x) class(x[[1]][1]))=="character"
      lms<-lms[kill]
      names<-names[kill]
    #run
      for(i in 1:c(length(lms))){
        ml[[i]] <- siena07(myalgorithm, data = datasetg, effects = lms[[i]], nbrNodes=16, useCluster=T, initC=T, returnDeps=TRUE, batch=T, verbose = F)
        Model<-ml[[i]]
        printfun(Model,name=names[i])
        gc()
        closeAllConnections()
      }
      
      
#gem
    #set DV
      options$DVname<-"gem_r15_avg"
    #prep data
    {
      source("prep.1 - Load libraries.R")
      source("prep2.1 - Prep subset.R")#need to manually edit
      source("prep.2 - Load data.R")  
      source("prep2.3 - Prep network data.R")
      source("prep2.4 - Prep all other dyadic covariates.R")
      source("prep2.5 - Prep non-network data.R")
      source("prep2.6 - Prep full friendship networks.R")
      source("prep2.7 - Remove individuals from waves 2-3.R")
      source("prep2.9 - Create summary info and save.R")
      source("prep2.10 - Create more camp level dummies.R")
    }
      source("mod.1 - Load and create RSiena objects.R")
      source("mod.3 - Create effects for algorithm.R")
    #choose effects to always include
      myeff <- includeInteraction(myeff, linear, effFrom, interaction1 = c('','ward2'),name="DV")
      myeff <- includeInteraction(myeff, linear, effFrom, interaction1 = c('','ward3'),name="DV")
      myeff <- includeInteraction(myeff, linear, effFrom, interaction1 = c('','ward4'),name="DV")
      myeff <- includeInteraction(myeff, linear, effFrom, interaction1 = c('','wave'),name="DV")
      myeff <- includeEffects(myeff, effFrom, interaction1 = "sex", name = "DV")
      # myeff <- includeEffects(myeff, effFrom, interaction1 = "sex.active", name = "DV")
      myeff <- includeEffects(myeff, effFrom, interaction1 = "alc_frq", name = "DV")
      myeff <- includeEffects(myeff, effFrom, interaction1 = "edu", name = "DV")
    #choose effects to test
      m0<-NA;m1<-NA;m2<-NA;m3<-NA;m4<-NA;m5<-NA;m6<-NA;m7<-NA;m8<-NA;m9<-NA;m10
      # m0 <- includeEffects(myeff, avSim, interaction1 = "friendship", name = "DV")
      m1 <- includeEffects(myeff, avAlt, interaction1 = "friendship", name = "DV")
      # m2 <- includeEffects(myeff, avAltDist2, interaction1='friendship', name='DV')
      # m3 <- includeEffects(myeff, avSimPopAlt, interaction1='friendship', name='DV')
      m4 <- includeInteraction(myeff, avSim, effFrom, interaction1 = c("friendship","sex"), name = "DV")
      m4 <- includeEffects(m4, avSim, interaction1 = "friendship", name = "DV")
      m5 <- includeInteraction(myeff, avSim, effFrom, interaction1 = c("friendship","sex.year"), name = "DV")
      m5 <- includeEffects(m5, avSim, interaction1 = "friendship", name = "DV")
      m6 <- includeEffects(myeff, avXAlt, interaction1='sex', interaction2='friendship', name='DV')
      m7 <- includeEffects(myeff, avXAlt, interaction1='sex', interaction2='friendship', name='DV')
      m7 <- includeEffects(m7, avAlt, interaction1 = "friendship", name = "DV")
      m8 <- includeEffects(myeff, avXAlt, interaction1='sex', interaction2='friendship', name='DV')
      m8 <- includeEffects(m8, avSim, interaction1 = "friendship", name = "DV")
      m9 <- includeEffects(myeff, avAlt, interaction1 = "friendship", name = "DV")
      m9 <- includeInteraction(m9, avAlt, effFrom, interaction1 = c("friendship","sex"), name = "DV")
      m9 <- includeInteraction(m9, avAlt, effFrom, interaction1 = c("friendship","sex.active"), name = "DV")
      m10 <- includeEffects(myeff, avSim, interaction1 = "friendship", name = "DV")
      m10 <- includeInteraction(m10, avSim, effFrom, interaction1 = c("friendship","sex"), name = "DV")
      m10 <- includeInteraction(m10, avSim, effFrom, interaction1 = c("friendship","sex.active"), name = "DV")
      # m6 <- includeInteraction(myeff, avAlt, effFrom, interaction1 = c("friendship","sex.active"), name = "DV")
      # m8 <- includeEffects(myeff, avXAlt, interaction1='leader', interaction2='friendship', name='DV')
      # m9 <- includeEffects(myeff, effFrom, interaction1 = "gem", name = "DV")
      # m9 <- includeEffects(m9, avAlt, interaction1 = "friendship", name = "DV")
      # m10 <- includeEffects(myeff, avAltPop, interaction1 = "friendship", name = "DV")
      
      lms<-list(m0,m1,m2,m3,m4,m5,m6,m7,m8,m9,m10)
      ml<-list()
    #set file names
      names<-paste("gem",c("avsim","avalt","avaltdist2","avsimpopalt","avsim*sex","avsim*sexyear","sexalt","sexalt+avalt","sexalt+avsim","avalt*int","avsim*int"))
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
      kill<-sapply(lms,function(x) class(x[[1]][1]))=="character"
      lms<-lms[kill]
      names<-names[kill]
      
    #run
      for(i in 1:c(length(lms))){
        ml[[i]] <- siena07(myalgorithm, data = datasetg, effects = lms[[i]], nbrNodes=16, useCluster=T, initC=T, returnDeps=TRUE, batch=T, verbose = F)
        Model<-ml[[i]]
        printfun(Model,name=names[i])
        gc()
        closeAllConnections()
      }
#hiv
    #set DV
      options$DVname<-"testhiv_12"
    #prep data
    {
      source("prep.1 - Load libraries.R")
      source("prep2.1 - Prep subset.R")#need to manually edit
      source("prep.2 - Load data.R")  
      source("prep2.3 - Prep network data.R")
      source("prep2.4 - Prep all other dyadic covariates.R")
      source("prep2.5 - Prep non-network data.R")
      source("prep2.6 - Prep full friendship networks.R")
      source("prep2.7 - Remove individuals from waves 2-3.R")
      source("prep2.9 - Create summary info and save.R")
      source("prep2.10 - Create more camp level dummies.R")
    }
      source("mod.1 - Load and create RSiena objects.R")
      temp<-sapply(datasetg,function(x) x$vCovars$wave[1])
      wave<-ifelse(temp<mean(temp),1,3)
      dataset<-dataset[wave==3]
      datasetg <- sienaGroupCreate(dataset)
      source("mod.3 - Create effects for algorithm.R")
      myeff <- includeEffects(myeff, outdeg, interaction1 = "friendship", name = "DV")
      myeff <- includeEffects(myeff, effFrom, interaction1 = "sex", name = "DV")
      myeff <- includeEffects(myeff, effFrom, interaction1 = "sex.year", name = "DV")
      myeff <- includeEffects(myeff, effFrom, interaction1 = "alc_evr", name = "DV")
      myeff <- includeEffects(myeff, effFrom, interaction1 = "ses", name = "DV")
      m0<-NA;m1<-NA;m2<-NA;m3<-NA;m4<-NA;m5<-NA;m6<-NA;m7<-NA;m8<-NA;m9<-NA;m10<-NA;m11<-NA;m12<-NA;m13<-NA;m14<-NA
      # m0 <- myeff
      m1 <- includeEffects(myeff, avSim, interaction1 = c('friendship'),name="DV") #no
      m2 <- includeEffects(myeff, effFrom, interaction1 = "hivtst.alter", name = "DV") #yes
      m3 <- includeEffects(myeff, effFrom, interaction1 = "hivadvf.alter", name = "DV") #yes
      m4 <- includeEffects(myeff, effFrom, interaction1 = "hivinjf.alter", name = "DV") #yes
      m5 <- includeInteraction(myeff, linear, effFrom, interaction1 = c('','r.hivtst'),name="DV")
      m6 <- includeInteraction(myeff, linear, effFrom, interaction1 = c('','r.hivinj'),name="DV")
      
      
      m6 <- includeEffects(m6, effFrom, interaction1 = "r.hivinj", name = "DV") #yes

      m7 <- includeInteraction(myeff, linear, effFrom, interaction1 = c('','r.hivadv'),name="DV")
      m8 <- includeEffects(myeff, effFrom, interaction1 = "hivtst.alter", name = "DV") #yes
      m8 <- includeInteraction(m8, linear, effFrom, interaction1 = c('','r.hivtst'),name="DV")
      m9 <- includeEffects(myeff, effFrom, interaction1 = "hivadvf.alter", name = "DV") #yes
      m9 <- includeInteraction(m9, linear, effFrom, interaction1 = c('','r.hivadv'),name="DV")
      m10 <- includeEffects(myeff, effFrom, interaction1 = "hivinjf.alter", name = "DV") #yes
      m10 <- includeInteraction(m10, linear, effFrom, interaction1 = c('','r.hivinj'),name="DV")
      
      m11 <- includeEffects(myeff, effFrom, interaction1 = "hivtst.alter", name = "DV") #yes
      m11 <- includeInteraction(m11, linear, effFrom, interaction1 = c('','r.hivtst'),name="DV")
      m11 <- includeEffects(m11, avSim, interaction1 = c('friendship'),name="DV") #no
      m12 <- includeEffects(myeff, effFrom, interaction1 = "hivadvf.alter", name = "DV") #yes
      m12 <- includeInteraction(m12, linear, effFrom, interaction1 = c('','r.hivadv'),name="DV")
      m12 <- includeEffects(m12, avSim, interaction1 = c('friendship'),name="DV") #no
      m13 <- includeEffects(myeff, effFrom, interaction1 = "hivinjf.alter", name = "DV") #yes
      m13 <- includeInteraction(m13, linear, effFrom, interaction1 = c('','r.hivinj'),name="DV")
      m13 <- includeEffects(m13, avSim, interaction1 = c('friendship'),name="DV") #no
      
      m14 <- includeEffects(myeff, avSim, interaction1 = c('friendship'),name="DV") #no
      m14 <- includeEffects(m14, effFrom, interaction1 = "hivtst.alter", name = "DV") #yes
      m14 <- includeInteraction(m14, linear, effFrom, interaction1 = c('','r.hivtst'),name="DV")
      m14 <- includeEffects(m14, effFrom, interaction1 = "hivadvf.alter", name = "DV") #yes
      m14 <- includeInteraction(m14, linear, effFrom, interaction1 = c('','r.hivadv'),name="DV")
      m14 <- includeEffects(m14, effFrom, interaction1 = "hivinjf.alter", name = "DV") #yes
      m14 <- includeInteraction(m14, linear, effFrom, interaction1 = c('','r.hivinj'),name="DV")
      
            
      lms<-list(m0,m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14)
      ml<-list()
    #set file names
      names<-paste("hiv",c("standard","avsim","ihivtst","ihivadv","ihivinj","rhivtst","rhivadv","rhivinj","bothhivtst","bothrhivadv","bothrhivinj","avsimbothhivtst","avsimbothrhivadv","avsimbothrhivinj","kitchensink"))
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
      kill<-sapply(lms,function(x) class(x[[1]][1]))=="character"
      lms<-lms[kill]
      names<-names[kill]
      
    #run
      for(i in 1:c(length(lms))){
        ml[[i]] <- siena07(myalgorithm, data = datasetg, effects = lms[[i]], nbrNodes=16, useCluster=T, initC=T, returnDeps=TRUE, batch=T, verbose = F)
        Model<-ml[[i]]
        printfun(Model,name=names[i])
        gc()
        closeAllConnections()
      }
      
      
      
      
      
      
      
      
      
      
      
      
      
      





      #alc_freq
          #set DV
            options$DVname<-"alc_freq"
          #prep data
          {
            source("prep.1 - Load libraries.R")
            source("prep2.1 - Prep subset.R")#need to manually edit
            source("prep.2 - Load data.R")  
            source("prep2.3 - Prep network data.R")
            source("prep2.4 - Prep all other dyadic covariates.R")
            source("prep2.5 - Prep non-network data.R")
            source("prep2.6 - Prep full friendship networks.R")
            source("prep2.7 - Remove individuals from waves 2-3.R")
            source("prep2.9 - Create summary info and save.R")
            source("prep2.10 - Create more camp level dummies.R")
          }
            source("mod.1 - Load and create RSiena objects.R")
            source("mod.3 - Create effects for algorithm.R")
          #choose effects to always include
            myeff <- includeInteraction(myeff, linear, effFrom, interaction1 = c('','ward2'),name="DV")
            myeff <- includeInteraction(myeff, linear, effFrom, interaction1 = c('','ward3'),name="DV")
            myeff <- includeInteraction(myeff, linear, effFrom, interaction1 = c('','ward4'),name="DV")
            myeff <- includeInteraction(myeff, linear, effFrom, interaction1 = c('','wave'),name="DV")
            myeff <- includeEffects(myeff, effFrom, interaction1 = "sex", name = "DV")
            myeff <- includeEffects(myeff, effFrom, interaction1 = "sex.active", name = "DV")
            myeff <- includeEffects(myeff, effFrom, interaction1 = "edu", name = "DV")
          #choose effects to test
            m0<-NA;m1<-NA;m2<-NA;m3<-NA;m4<-NA;m5<-NA;m6<-NA;m7<-NA;m8<-NA;m9<-NA;m10
            # m0 <- includeEffects(myeff, avSim, interaction1 = "friendship", name = "DV")
            m1 <- includeEffects(myeff, avAlt, interaction1 = "friendship", name = "DV")
            m2 <- includeEffects(myeff, avAltDist2, interaction1='friendship', name='DV')
            m3 <- includeEffects(myeff, avSimPopAlt, interaction1='friendship', name='DV')
            m4 <- includeInteraction(myeff, avSim, effFrom, interaction1 = c("friendship","sex"), name = "DV")
            m4 <- includeEffects(m4, avSim, interaction1 = "friendship", name = "DV")
            m5 <- includeInteraction(myeff, avSim, effFrom, interaction1 = c("friendship","sex.year"), name = "DV")
            m5 <- includeEffects(m5, avSim, interaction1 = "friendship", name = "DV")
            m6 <- includeEffects(myeff, avXAlt, interaction1='sex', interaction2='friendship', name='DV')
            m7 <- includeEffects(myeff, avXAlt, interaction1='sex', interaction2='friendship', name='DV')
            m7 <- includeEffects(m7, avAlt, interaction1 = "friendship", name = "DV")
            m8 <- includeEffects(myeff, avXAlt, interaction1='sex', interaction2='friendship', name='DV')
            m8 <- includeEffects(m8, avSim, interaction1 = "friendship", name = "DV")
            m9 <- includeEffects(myeff, avAlt, interaction1 = "friendship", name = "DV")
            m9 <- includeInteraction(m9, avAlt, effFrom, interaction1 = c("friendship","sex"), name = "DV")
            m9 <- includeInteraction(m9, avAlt, effFrom, interaction1 = c("friendship","sex.active"), name = "DV")
            m10 <- includeEffects(myeff, avSim, interaction1 = "friendship", name = "DV")
            m10 <- includeInteraction(m10, avSim, effFrom, interaction1 = c("friendship","sex"), name = "DV")
            m10 <- includeInteraction(m10, avSim, effFrom, interaction1 = c("friendship","sex.active"), name = "DV")
            # m6 <- includeInteraction(myeff, avAlt, effFrom, interaction1 = c("friendship","sex.active"), name = "DV")
            # m8 <- includeEffects(myeff, avXAlt, interaction1='leader', interaction2='friendship', name='DV')
            # m9 <- includeEffects(myeff, effFrom, interaction1 = "gem", name = "DV")
            # m9 <- includeEffects(m9, avAlt, interaction1 = "friendship", name = "DV")
            # m10 <- includeEffects(myeff, avAltPop, interaction1 = "friendship", name = "DV")
            lms<-list(m0,m1,m2,m3,m4,m5,m6,m7,m8,m9,m10)
            ml<-list()
          #set file names
            names<-paste("alc",c("avsim","avalt","avaltdist2","avsimpopalt","avsim*sex","avsim*sexyear","sexalt","sexalt+avalt","sexalt+avsim","avalt*int","avsim*int"))
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
            kill<-sapply(lms,function(x) class(x[[1]][1]))=="character"
            lms<-lms[kill]
            names<-names[kill]
          #run
            for(i in 1:c(length(lms))){
              ml[[i]] <- siena07(myalgorithm, data = datasetg, effects = lms[[i]], nbrNodes=16, useCluster=T, initC=T, returnDeps=TRUE, batch=T, verbose = F)
              Model<-ml[[i]]
              printfun(Model,name=names[i])
              gc()
              closeAllConnections()
            }
