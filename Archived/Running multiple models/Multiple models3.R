{
#ipv
    #set DV
      options$DVname<-"ipvoutcome_cat" #gem_r15_avg ipvoutcome_cat alc_freq testhiv_12
    #prep data
      source("prep.1 - Load libraries.R")
      source("mod.1 - Load and create RSiena objects.R")
      #subset data
          sumstats2<-readRDS("sumstats2.rds")
          #Create a backup
              sumstats3<-sumstats2
          # decent N
              sumstats3<-sumstats3[sumstats3$N>30,]
              #sum(sumstats3$N)
          # decent missing
              sumstats3<-sumstats3[sumstats3$missing<.2,]
          #decent jaccard
              sumstats3<-sumstats3[sumstats3$ffjaccard>.15,]
                # if(options$DVname=="ipvoutcome_cat"){
                #   sumstats3<-sumstats3[sumstats3$d.ipv>.15,]
                # }
                  sumstats3<-sumstats3[sumstats3$d.ipvall>.15,]
                # if(options$DVname=="all_ipv"){
                #   sumstats3<-sumstats3[sumstats3$d.ipvall>.15,]
                # }
                # if(options$DVname=="all_ipv_bin"){
                #   sumstats3<-sumstats3[sumstats3$d.ipvallbin>.15,]
                # }
                # if(options$DVname=="gem_r15_avg"){
                #   sumstats3<-sumstats3[sumstats3$d.gem>.15,]
                # }
                # if(options$DVname=="testhiv_12"){
                #   sumstats3<-sumstats3[sumstats3$d.hiv>.15,]
                # }
                # if(options$DVname=="alc_freq"){
                #   sumstats3<-sumstats3[sumstats3$d.alc>.15,]
                # }
  
      
              # sumstats3<-sumstats3[setdiff(c(1:nrow(sumstats3)),c(7)),]
              # sumstats3<-sumstats3[setdiff(c(1:nrow(sumstats3)),c(1,4)),]
              # sumstats3<-sumstats3[setdiff(c(1:nrow(sumstats3)),c(2)),]
              # sumstats3<-sumstats3[setdiff(c(1:nrow(sumstats3)),c(4)),]
              
                  
                  
              sumstats3<-sumstats3[setdiff(c(1:nrow(sumstats3)),c(6)),]
              sumstats3<-sumstats3[setdiff(c(1:nrow(sumstats3)),c(3)),]
              sumstats3<-sumstats3[setdiff(c(1:nrow(sumstats3)),c(3)),]
              sumstats3<-sumstats3[setdiff(c(1:nrow(sumstats3)),c(3)),]
              sumstats3<-sumstats3[setdiff(c(1:nrow(sumstats3)),c(3)),]
              
              #these camps are good too
              sumstats3$Wave
              sumstats3$Camp
              dataset2<-dataset[which(paste(sumstats2$Wave,sumstats2$Camp) %in% paste(sumstats3$Wave,sumstats3$Camp))]
              
              # dataset3<-dataset2[1:10]
              # dataset4<-dataset2[11:19]
              # #If there's an outlier and want to make interaction term
              #     outlie<-c(9,15)
              #     for(i in 1:length(dataset2)){
              #       if(i %in% outlie){
              #         dataset2[[i]]$cCovars$campn[1:length(dataset2[[i]]$cCovars$campn)]<-1
              #       }else{
              #         dataset2[[i]]$cCovars$campn[1:length(dataset2[[i]]$cCovars$campn)]<-0
              #       }
              #     }
              #SAVE ADDING THE ABOVE FOR LATER, PREP NUMEROUS CAMPN VARIABLES FOR THIS.
              #SAVE ADDING MORE CAMPS FOR THAT TIME AS WELL
              
              datasetg <- sienaGroupCreate(dataset2)
              
#maybe dont remove missing from network

              # sumstats3$ffJaccard.12
              # sumstats3$d.ipv.12
              
              # sapply(dataset2, function(x) x$cCovars$campn[1])
              # datasetg<-sienaGroupCreate(dataset2[c(1,2,4:8,10:length(dataset2))])
                # datasetg<-sienaGroupCreate(dataset2[setdiff(c(1:length(dataset2)),c(4,5,6,20))])
                # datasetg<-sienaGroupCreate(dataset2[setdiff(c(1:length(dataset2)),c(4,5,11,26))])
              
        # file.edit("mod.3 - Create effects for algorithm.R")
        source("mod.3ff - Create effects for algorithm.R")
    #choose effects to always include
      # # myeff <- includeInteraction(myeff, linear, effFrom, interaction1 = c('','ward2'),name="DV")
      # # myeff <- includeInteraction(myeff, linear, effFrom, interaction1 = c('','ward3'),name="DV")
      # # myeff <- includeInteraction(myeff, linear, effFrom, interaction1 = c('','ward4'),name="DV")
      # myeff <- includeInteraction(myeff, linear, effFrom, interaction1 = c('','wave'),name="DV")
      # myeff <- includeEffects(myeff, effFrom, interaction1 = "sex", name = "DV")
      # myeff <- includeEffects(myeff, effFrom, interaction1 = "sex.active", name = "DV")
      # myeff <- includeEffects(myeff, effFrom, interaction1 = "alc_frq", name = "DV")
      # # myeff <- includeEffects(myeff, effFrom, interaction1 = "edu", name = "DV")
    #choose effects to test
      m0<-NA;m1<-NA;m2<-NA;m3<-NA;m4<-NA;m5<-NA;m6<-NA;m7<-NA;m8<-NA;m9<-NA;m10<-NA;m11<-NA;m12<-NA;m13<-NA;m14<-NA;m15<-NA;m16<-NA;m17<-NA;m18<-NA;m19<-NA;m20<-NA
      
      # m0 <- includeEffects(myeff, effFrom, interaction1 = "ffhiv", name = "DV")
      # m1 <- includeEffects(myeff, transTrip)
      # m2 <- includeEffects(myeff, transRecTrip) #maybe
      # m3 <- includeEffects(myeff, cycle3)
      # m5 <- includeEffects(myeff, transTies)
      # m9 <- includeEffects(myeff, denseTriads)
      # m10 <- includeEffects(myeff, outPop) #sqrt better
      # m12 <- includeEffects(myeff, reciPop) #sqrt better 
      # m14 <- includeEffects(myeff, inAct) #sqrt better
      # m16 <- includeEffects(myeff, inIsDegree)
      # m17 <- includeEffects(myeff, outAct)
      # m18 <- includeEffects(myeff, outActSqrt)
      # m19 <- includeEffects(myeff, reciAct)
      # m7 <- includeEffects(myeff, Jin) #yes  #no more trunc and gwesp; interact with camp 6
      # m8 <- includeEffects(myeff, nbrDist2) #yes

      
      myalgorithm <- sienaAlgorithmCreate( projname = 'SingleGroups', useStdInits = FALSE, nsub=3, n3=2000,cond = F,MaxDegree = c(friendship=8))
      
      m0<-myeff
      m0[m0$include==T & m0$shortName=="outTrunc",]$include<-F
      m0[m0$include==T & m0$shortName=="gwespFF",]$include<-F
      m0[m0$include==T & m0$shortName=="outInAss",]$include<-F
      m0 <- includeEffects(m0, Jout) #yes  #no more trunc and gwesp; interact with camp 6
      m0 <- includeEffects(m0, outPopSqrt)
      # m0 <- includeEffects(m0, egoX, altX, simX, interaction1 = "DV")
      m0 <- includeInteraction(m0, X, altX, interaction1 = c("bothmale","DV"),name="friendship")
      m0 <- includeInteraction(m0, X, simX, interaction1 = c("bothmale","DV"),name="friendship")
      m0 <- includeInteraction(m0, X, egoX, interaction1 = c("bothmale","DV"),name="friendship")
      m0 <- includeEffects(m0, X, interaction1='bothmale')
      
      
      # m0 <- includeEffects(m0, altX, interaction1 = "alc_frq")
      # m0 <- includeInteraction(m0, sameX, egoX, interaction1 = c("male","male"),name="friendship")

      #interactions
      # m0 <- includeInteraction(m0, Jout, egoX, interaction1 = c('','campn'),name="friendship")
      
      
      
      #behavior
      # m0 <- includeInteraction(m0, linear, effFrom, interaction1 = c('','ward2'),name="DV")
      # m0 <- includeInteraction(m0, linear, effFrom, interaction1 = c('','ward3'),name="DV")
      # m0 <- includeInteraction(m0, linear, effFrom, interaction1 = c('','ward4'),name="DV")
      # m0 <- includeInteraction(m0, linear, effFrom, interaction1 = c('','wave'),name="DV")
      m0 <- includeEffects(m0, effFrom, interaction1 = "male", name = "DV")
      m0 <- includeEffects(m0, effFrom, interaction1 = "sex.active", name = "DV")
      # m0 <- includeEffects(m0, effFrom, interaction1 = "alc_frq", name = "DV")
      # m0 <- includeEffects(m0, effFrom, interaction1 = "edu", name = "DV")

      # m1 <- includeEffects(m0, avAlt, interaction1 = "friendship", name = "DV")
      # m2 <- includeEffects(m0, avSim, interaction1 = "friendship", name = "DV")
      # m3 <- includeEffects(m0, avXAlt, interaction1='sex', interaction2='friendship', name='DV')
      # m4 <- includeEffects(m0, avXAlt, interaction1='gem', interaction2='friendship', name='DV')
      
      # m5 <- includeEffects(m0, avAltW, interaction1='friendship', interaction2='bothmale', name='DV')
      m0 <- includeEffects(m0, avSimW, interaction1='friendship', interaction2='bothmale', name='DV')
      # m7 <- includeInteraction(m0, avXAlt, effFrom, interaction1=c('sex','male'), interaction2=c('friendship',''), name='DV')
      
      
      
          #this is wrong; should be avxAlt where ego sex ==1
      # m8 <- includeInteraction(m0, avXAlt,avWAlt, interaction1=c('gem','friendship'), interaction2=c('friendship','bothmale'), name='DV')
      
      # m9 <- includeInteraction(m0, avSimPopAlt,avWAlt, interaction1=c('friendship','friendship'), interaction2=c('','bothmale'), name='DV')
      # m10 <- includeInteraction(m0, avAltPop,avWAlt, interaction1=c('friendship','friendship'), interaction2=c('','bothmale'), name='DV')
      # m11 <- includeInteraction(m0, avAltDist2,avWAlt, interaction1=c('friendship','friendship'), interaction2=c('','bothmale'), name='DV')
      # m12 <- includeInteraction(m0, avSimPopEgo,avWAlt, interaction1=c('friendship','friendship'), interaction2=c('','bothmale'), name='DV')
      # 
      # m1 <- includeEffects(m0, outdeg, interaction1 = "friendship", name = "DV")
      # m2 <- includeEffects(m0, indeg, interaction1 = "friendship", name = "DV")
      
      #should try outdeg and indeg effect again

            #
      # m1 <- includeEffects(m0, transTrip)
      # m2 <- includeEffects(m0, transRecTrip)
      # m3 <- includeEffects(m0, cycle3)
      # m4 <- includeEffects(m0, transTies)
      # m5 <- includeEffects(m0, nbrDist2)
      # m6 <- includeEffects(m0, gwespFF)
      
      # m1 <- includeEffects(m0, inOutAss) #yes; cancels out-in
      # m6 <- includeEffects(m3, outOutAss) #yes
      # m2 <- includeEffects(m1, cycle4) #yes
      # m4 <- includeEffects(m3, reciPopSqrt) #yes
      # m5 <- includeEffects(m4, inActSqrt) #yes
      # m3 <- includeEffects(myeff, antiIso)
      # m4 <- includeEffects(myeff, Jout) #too correlated
      # m4 <- includeEffects(m4, Jin) 
      # m5 <- includeEffects(myeff, cycle4) #too correlated
      # m5 <- includeEffects(m5, nbrDist2) #yes
      
      # m0 <- includeEffects(myeff, effFrom, interaction1 = "ffipv", name = "DV")
      # m1 <- includeEffects(myeff, effFrom, interaction1 = "ffgem", name = "DV")
      # m2 <- includeEffects(myeff, effFrom, interaction1 = "ffalc", name = "DV")
      # m3 <- includeEffects(myeff, effFrom, interaction1 = "ffgend", name = "DV")
      # m4 <- includeEffects(myeff, effFrom, interaction1 = "ffnum", name = "DV")
      # m5 <- includeEffects(myeff, effFrom, interaction1 = "clipv", name = "DV")
      # m6 <- includeEffects(myeff, effFrom, interaction1 = "clgem", name = "DV")
      # m7 <- includeEffects(myeff, effFrom, interaction1 = "clalc", name = "DV")
      # m8 <- includeEffects(myeff, effFrom, interaction1 = "clgend", name = "DV")
      # m9 <- includeEffects(myeff, effFrom, interaction1 = "clnum", name = "DV")
      # m10 <- includeInteraction(myeff, linear, effFrom, interaction1 = c('','r.ipv'),name="DV")
      # m11 <- includeInteraction(myeff, linear, effFrom, interaction1 = c('','r.gem'),name="DV")
      # m12 <- includeInteraction(myeff, linear, effFrom, interaction1 = c('','r.alc'),name="DV")
      # m13 <- includeInteraction(myeff, linear, effFrom, interaction1 = c('','r.gend'),name="DV")
      # m14 <- includeInteraction(myeff, linear, effFrom, interaction1 = c('','N1'),name="DV")
      ##WILL NEED SELECTION EFFECTS
      
      # m0 <- includeEffects(myeff, avSim, interaction1 = "friendship", name = "DV")
      # m1 <- includeEffects(myeff, avAlt, interaction1 = "friendship", name = "DV")
      # m2 <- includeEffects(myeff, avAltDist2, interaction1='friendship', name='DV')
      # m3 <- includeEffects(myeff, avSimPopAlt, interaction1='friendship', name='DV')
      # m4 <- includeInteraction(myeff, avSim, effFrom, interaction1 = c("friendship","sex"), name = "DV")
      # m4 <- includeEffects(m4, avSim, interaction1 = "friendship", name = "DV")
      # m5 <- includeInteraction(myeff, avSim, effFrom, interaction1 = c("friendship","sex.year"), name = "DV")
      # m5 <- includeEffects(m5, avSim, interaction1 = "friendship", name = "DV")
      # m6 <- includeEffects(myeff, avXAlt, interaction1='sex', interaction2='friendship', name='DV')
      # m7 <- includeEffects(myeff, avXAlt, interaction1='sex', interaction2='friendship', name='DV')
      # m7 <- includeEffects(m7, avAlt, interaction1 = "friendship", name = "DV")
      # m8 <- includeEffects(myeff, avXAlt, interaction1='sex', interaction2='friendship', name='DV')
      # m8 <- includeEffects(m8, avSim, interaction1 = "friendship", name = "DV")
      # m9 <- includeEffects(myeff, avAlt, interaction1 = "friendship", name = "DV")
      # m9 <- includeInteraction(m9, avAlt, effFrom, interaction1 = c("friendship","sex"), name = "DV")
      # m9 <- includeInteraction(m9, avAlt, effFrom, interaction1 = c("friendship","sex.active"), name = "DV")
      # m10 <- includeEffects(myeff, avSim, interaction1 = "friendship", name = "DV")
      # m10 <- includeInteraction(m10, avSim, effFrom, interaction1 = c("friendship","sex"), name = "DV")
      # m10 <- includeInteraction(m10, avSim, effFrom, interaction1 = c("friendship","sex.active"), name = "DV")
      # 
      # m6 <- includeInteraction(myeff, avAlt, effFrom, interaction1 = c("friendship","sex.active"), name = "DV")
      # m8 <- includeEffects(myeff, avXAlt, interaction1='leader', interaction2='friendship', name='DV')
      # m9 <- includeEffects(myeff, effFrom, interaction1 = "gem", name = "DV")
      # m9 <- includeEffects(m9, avAlt, interaction1 = "friendship", name = "DV")
      # m10 <- includeEffects(myeff, avAltPop, interaction1 = "friendship", name = "DV")
      lms<-list(m0,m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16,m17,m18,m19)
      ml<-list()
    #set file names
      # names<-paste("ipv",c("ffipv","ffgem","ffalc","ffgend","ffnum","clipv","clgem","clalc","clgend","clnum","r.ipv","r.gem","r.alc","r.gend","N1"))
      names<-rep("ff",length(lms))
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
        number<-suppressWarnings(max(as.numeric(sub('\\..*', '',gsub("[[:alpha:]]", "", list.files(path="/data/data1/bda13/Data/Shared/Tanzania/Results/Results html",pattern='*\\.html', recursive=TRUE)))),na.rm=T)+1)
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
      }
      Model<-ml[[i-1]]
      

      # time test
      Model$effects$effectName
      (timetest<-sienaTimeTest(Model,effects = c(35:37)))
          a<-as.data.frame(timetest$IndividualTest)
          a<-a[a$`p-Value`<.05,]
          # a<-a[abs(a$`One Step Est.`)<1,]
          a$`Initial Est.`<-NULL
          outlie<-c(9,15) #manually assign
          tsum<-sumstats3[outlie,]
          sumstats3<-sumstats3[,c("N","Wave","density","ffjaccard","missing")]
          for(i in 1:ncol(sumstats3)){
            na<-names(sumstats3)[i]
            tr<-round(mean(sumstats3[outlie,i]),2)
            co<-round(mean(sumstats3[,i]),2)
            print(paste(na,"- Tr =",tr,"vs Co =",co))
          }
          #explore stat that seems screwed up in case other camps should also be assign an interaction for this
          # sumstats3[outlie,]
          b<-which(sumstats3$ffjaccard<.2)
          a<-as.data.frame(timetest$IndividualTest)
          a$`Initial Est.`<-NULL
          a<-a[grepl("out-Jaccard",row.names(a)),]
          a[b,]
      #create dummy variable for outliers
          #alter campn to become interactino term for outliers

      ml[[6]]
      # #determine if model will crash
      # crash<-function(x,var){
      #     temp<-x$vCovars[var][[1]]
      #     temp<-temp[!is.na(temp)]
      #     temp<-length(unique(temp))
      #     return(temp)
      # }
      # vars<-c("ffipv","ffgem","ffalc","ffgend","ffnum","clipv","clgem","clalc","clgend","clnum","r.ipv","r.gem","r.alc","r.gend","N1")
      # for(i in 1:length(vars)){
      #     print(sum(sapply(datasetg,function(x) crash(x,vars[i]))==1))  
      # }
      # # sapply(datasetg,function(x) crash(x,vars[i]))
      # # sapply(datasetg,function(x) crash(x,"alc_frq"))
      # # datasetg<-datasetg2[!sapply(datasetg,function(x) crash(x,"ffipv"))==1]
      
      
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

            
            
            
            a<-
            gsub("(\\w+)", "",a)
            gsub("/.*", '', a)
            gsub(" ", "", a)
            nchar(2)
            gsub("\\..$","", a)
                       
            
            
            
            sapply
            as.numeric(dataset[[1]]$cCovars$gem)
            t1<-unlist(sapply(dataset,function(x) as.numeric(x$cCovars$gem+attr(x$cCovars$gem,"mean"))))
            t2<-unlist(sapply(dataset,function(x) as.numeric(x$cCovars$sex+attr(x$cCovars$sex,"mean"))))
            t3<-unlist(sapply(dataset,function(x) as.numeric(x$cCovars$edu+attr(x$cCovars$edu,"mean"))))
            cor(t1,t2,use="complete")
            cor(t1,t3,use="complete")
            as.numeric(x$cCovars$gem+attr(x$cCovars$gem,"mean"))
            as.numeric(x$cCovars$sex+attr(x$cCovars$sex,"mean"))
            as.numeric(x$cCovars$edu+attr(x$cCovars$edu,"mean"))
            
            head(df$b_gem_r15_avg)
            cor(df$m_ipvoutcome_cat,df$m_gem_r15_avg,use="complete")
            
            Model
            