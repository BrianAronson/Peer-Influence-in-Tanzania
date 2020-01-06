#0 - Set work directory
     # setwd("/misc/utopia3/bda13/lanhome/Data/Shared/Tanzania/Code") #Alternatives: setwd("C:/Users/bda13/Desktop/Sociology/Papers in Progress/Tanzania/Code")
    # setwd("C:/Users/bda13/Desktop/Tanzania/Code")
setwd("/home/rstudio/Dropbox/AWS/Tanzania/Code")

#1) load data
    source("Ward 1 - load data.R")

#2) kill camps you don't like
    #identify camps
        sumstats2<-readRDS("sumstats2.rds")
        sumstats3<-sumstats2
        sumstats3<-sumstats3[sumstats3$N>15,]
        sumstats3<-sumstats3[sumstats3$missing<.2,]
        sumstats3<-sumstats3[sumstats3$ffjaccard>.15,]
        # sumstats3<-sumstats3[sumstats3$d.hiv>.1,]
        # sumstats3<-sumstats3[sumstats3$Wave==3,]
        sumstats3<-sumstats3[sumstats3$Camp!=15,]  #Camp 15 is problematic

        {
        # #identify condition camps
        # dir<-getwd(); tdir<-'..';setwd(paste(tdir,"/Dropbox files/Tanzania R01 Network Data - Peer Influence/SAS Data",sep=""));df <- read.sas7bdat("widebehavioraldata_20180614.sas7bdat");setwd(dir); names(df)<-tolower(names(df)); df<-df[df$b_respond!=0,]
        # cond1<-unique(df$camp[df$condition==1])
        # cond2<-unique(df$camp[df$condition==2])
        # #remove a camp from ward 3
        #     w3camps<-unique(sapply(campn,function(x)x[1])[sapply(ward3,function(x)x[1])==1 & sapply(wave,function(x)x[1])==1])
        #     rmcamps<-sumstats3$Camp[sumstats3$Camp%in%w3camps & sumstats3$Wave==1]
        #     # sumstats3[sumstats3$Camp %in% rmcamps & sumstats3$Wave==1,]
        #     sumstats4<-sumstats3[!(sumstats3$Camp %in% rmcamps & sumstats3$Wave==1),]
        #
        #     # sumstats3[sumstats3$Camp==40 & sumstats3$Wave==1,]
        #     # sumstats3$N
        #     # dataset[[3]]
        #     # dataset[[3]]$cCovars$campn
        #     nrow(sumstats4)
        #     length(unique(sumstats4$Camp))
        #     weighted.mean(sumstats4$missing[sumstats4$Wave==1],sumstats4$N[sumstats4$Wave==1])
        #     weighted.mean(sumstats4$missing[sumstats4$Wave==3],sumstats4$N[sumstats4$Wave==3])
        }

    #kill them
        kill<-paste(sumstats2$Camp,sumstats2$Wave) %in% paste(sumstats3$Camp,sumstats3$Wave)
        a<-c(varlist,sumvars)
        for(i in 1:length(a)){
          a[[i]]<-a[[i]][kill]
        }
        list2env(a,globalenv())

    #TEMPORARY - kill DV
        noDV<-1

#3) aggregate ward data
    source("Ward 2 - aggregate objects to wards.R")

#4) prep siena data for HIV (default = close friends for hiv)
    source("Ward 3 - prep siena objects.R")

#5) customize models
    conv<-vector()
    {
    # dataset[4:7]
    # x<-dataset[[4]]
    # x$cCovars$condition>0
    # x$cCovars$wave
    #
    #   a1<-(unlist(sapply(dataset[5:8],function(x) x$depvars$DV[,,1])))
    #   a2<-(unlist(sapply(dataset[5:8],function(x) x$depvars$DV[,,2])))
    #   cond<-(unlist(sapply(dataset[5:8],function(x) x$cCovars$condition))>0)
    #   men<-(unlist(sapply(dataset[5:8],function(x) x$cCovars$male))>0)
    #   anyna<-is.nan(a1)|is.nan(a2)
    #   b1<-a1[!anyna]
    #   b2<-a2[!anyna]
    #   bcond<-cond[!anyna]
    #   bmen<-men[!anyna]
    #   table(b1)
    #   table(b2)
    #   prop.table(table(b1[bcond & bmen]))
    #   prop.table(table(b2[bcond & bmen]))
    #   prop.table(table(b1[!bcond & bmen]))
    #   prop.table(table(b2[!bcond & bmen]))
    #   a1[cond]
    #   a2[cond]
    #   a1[!cond]
    #   a2[!cond]
    # sum(sapply(datasetg,function(x) length(x$nodeSets$Actors))[4:7])
    # dataset2<-dataset[-3]
    # length(unique(unlist(sapply(dataset2,function(x) c(x$cCovars$pid)))))
    # a<-unlist(sapply(dataset2,function(x) c(x$cCovars$pid)))
    # b<-unlist(sapply(dataset2,function(x) c(x$cCovars$sex>0)))
    # table(b[!duplicated(a)])
    # prop.table(table(unlist(sapply(dataset2,(function(x)x$depvars$DV[,,1])))))
    # prop.table(table(unlist(sapply(dataset2,(function(x)x$depvars$DV[,,2])))))
    # a0<-as.numeric(unlist(sapply(dataset2[4:7],(function(x)x$cCovars$ever.test)))>0)
    # a1<-unlist(sapply(dataset2[1:7],(function(x)x$depvars$DV[,,1])))
    # a2<-unlist(sapply(dataset2[1:7],(function(x)x$depvars$DV[,,2])))
    # prop.table(table(a>0))
    # prop.table(table(b>0))
    # prop.table(table(a2,a1))
    }
    # datasetg <- sienaGroupCreate(dataset[c(-3,-5)])
    # datasetg <- sienaGroupCreate(dataset[c(-3,-5,-6,-7,-8)])
    datasetg <- sienaGroupCreate(dataset[c(-1,-2,-3,-4)])
    datasetg <- sienaGroupCreate(dataset[c(-3,-5,-6,-7,-8)])
    # datasetg<-sienaGroupCreate(dataset)


#selection
    #Selection
        myeff <- getEffects(datasetg,behNintn =10)
        myeff <- includeEffects(myeff, simX, interaction1 = "age")
        myeff <- includeEffects(myeff, sameX, interaction1 = "sex" )
        myeff <- includeEffects(myeff, inPopSqrt)
        # myeff <- includeInteraction(myeff, egoX, inPopSqrt, interaction1 = c('ward1',''),name="friendship")
        myeff <- includeInteraction(myeff, density, egoX, interaction1 = c('','wave'),name="friendship")
        myeff <- includeEffects(myeff, altX,simX,egoX, interaction1='DV',name='friendship')
        myeff <- includeEffects(myeff, X, interaction1='ffriendship')
        
        # myeff <- includeEffects(myeff, egoX,interaction1='ward1')
        myeff <- includeEffects(myeff, egoX,interaction1='ward2')
        myeff <- includeEffects(myeff, egoX,interaction1='ward3')
        myeff <- includeEffects(myeff, egoX,interaction1='ward4')
        #Better for close friends
            # myeff <- setEffect(myeff, outTrunc, parameter = 1)
            # myeff <- includeEffects(myeff, gwespFF, parameter=69)
            myeff <- includeEffects(myeff, outInAss)
        #Better for full friends
            # myeff <- includeEffects(myeff, outPopSqrt)
            myeff <- includeEffects(myeff, Jout)
            {
        #new efforts
            # myeff <- includeEffects(myeff, egoX,interaction1='ward3')
            # myeff <- includeInteraction(myeff, egoX, egoX, interaction1 = c('ward3','wave'),name="friendship")
            # myeff <- includeInteraction(myeff, density, egoX, interaction1 = c('','wardwave3'),name="friendship")
            # myeff <- includeInteraction(myeff, recip, egoX, interaction1 = c('','wardwave3'),name="friendship")
            # myeff <- includeInteraction(myeff, inPopSqrt, egoX, interaction1 = c('','wardwave3'),name="friendship")
            # myeff <- includeInteraction(myeff, simX, egoX, interaction1 = c('age','wardwave3'),name="friendship")
            # myeff <- includeInteraction(myeff, sameX, egoX, interaction1 = c('sex','wardwave3'),name="friendship")
            # myeff <- includeInteraction(myeff, X, egoX, interaction1 = c('ffriendship','wardwave3'),name="friendship")
            # myeff <- includeInteraction(myeff, gwespFF, egoX, interaction1 = c('','wardwave3'),name="friendship",parameter=69)
            # myeff <- includeInteraction(myeff, outInAss, egoX, interaction1 = c('','wardwave3'),name="friendship")
            # myeff <- includeInteraction(myeff, outTrunc, egoX, interaction1 = c('','wardwave3'),name="friendship", parameter = 1)
            # myeff <- includeInteraction(myeff, simX, egoX, interaction1 = c('DV','wardwave3'),name="friendship")
            # myeff <- includeInteraction(myeff, altX, egoX, interaction1 = c('DV','wardwave3'),name="friendship")
            # myeff <- includeInteraction(myeff, egoX, egoX, interaction1 = c('DV','wardwave3'),name="friendship")
            }

#behavior
    # myeff <- includeEffects(myeff, outdeg, interaction1 = "friendship", name = "DV")
    myeff <- includeEffects(myeff, effFrom, interaction1 = "male", name = "DV")
    myeff <- includeEffects(myeff, effFrom, interaction1 = "sex.num2", name = "DV")
    myeff <- includeEffects(myeff, effFrom, interaction1 = "ses", name = "DV")
    # myeff <- includeEffects(myeff, effFrom, interaction1 = "wave", name = "DV")
    # myeff <- includeEffects(myeff, effFrom, interaction1 = "condition", name = "DV")

    myeff <- includeInteraction(myeff, effFrom, effFrom, interaction1 = c('leader','wave1'),name="DV")
    # myeff <- includeInteraction(myeff, effFrom, effFrom, interaction1 = c('fam.available','wave2'),name="DV")
{
    # myeff <- includeEffects(myeff, effFrom, interaction1 = "leader", name = "DV")
    # myeff <- includeEffects(myeff, effFrom, interaction1 = "camp.duration", name = "DV")
    # myeff <- includeEffects(myeff, effFrom, interaction1 = "has.children", name = "DV")
    # myeff <- includeEffects(myeff, effFrom, interaction1 = "fam.available", name = "DV")
    # myeff <- includeEffects(myeff, effFrom, interaction1 = "child.sex.violence", name = "DV")
    # myeff <- includeInteraction(myeff, linear, effFrom, effFrom, interaction1 = c("",'wave','ward4'),name="DV")
    # myeff <- includeInteraction(myeff, linear, effFrom, interaction1 = c("",'male'),name="DV")
    # myeff <- includeEffects(myeff, effFrom, interaction1 = "alc_evr", name = "DV")
    # myeff <- includeInteraction(myeff, linear, effFrom, interaction1 = c('','ward2'),name="DV")
    # myeff <- includeInteraction(myeff, linear, effFrom, interaction1 = c('','ward4'),name="DV")
    # myeff <- includeInteraction(myeff, linear, effFrom, interaction1 = c('','ward3'),name="DV")
    }

    myalgorithm <- sienaAlgorithmCreate( projname = 'SingleGroups', useStdInits = FALSE,
                                         nsub=4, n3=2000,MaxDegree = c(friendship=3),cond = F, firstg=.1, doubleAveraging=1) #,


#6) multiple model
    m0 <- myeff
    m0 <- includeEffects(m0, avSim, interaction1 = c('friendship'),name="DV")
    # m0 <- includeEffects(m0, avAltDist2, interaction1 = c('friendship'),name="DV")
    # m0 <- includeEffects(m0, avRecAlt, interaction1 = c('friendship'),name="DV")
    # m0 <- includeEffects(m0, maxAlt, interaction1 = c('friendship'),name="DV")

    parallel::detectCores() 

#7) run models
    lms<-list(m0,m1,m2,m3,m4,m5,m6,m7,m8,m9,m10);kill<-sapply(lms,function(x) class(x[[1]][1]))=="character"; lms<-lms[kill];ml<-list()
    baseMod <- siena07(myalgorithm, data = datasetg, effects = lms[[1]], nbrNodes=36, useCluster=T, initC=T, returnDeps=TRUE, batch=T, verbose = F)
    closeAllConnections()
    Model<-baseMod
    htmlfilename<-printfun(Model)
    max(abs(Model$tconv))
    Model$tconv.max
    (conv<-c(conv,Model$tconv.max))

    Model <- siena07(myalgorithm, data = datasetg, effects = lms[[1]], nbrNodes=36, useCluster=T, initC=T, returnDeps=TRUE, batch=T, verbose = F,prevAns = Model)
    htmlfilename<-printfun(Model)
    max(abs(Model$tconv))
    Model$tconv.max
    (conv<-c(conv,Model$tconv.max))
    
    Model <- siena07(myalgorithm, data = datasetg, effects = lms[[1]], nbrNodes=36, useCluster=T, initC=T, returnDeps=TRUE, batch=T, verbose = F,prevAns = Model)
    htmlfilename<-printfun(Model)
    max(abs(Model$tconv))
    Model$tconv.max
    (conv<-c(conv,Model$tconv.max))
    