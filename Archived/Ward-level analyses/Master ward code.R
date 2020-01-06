# install.packages("rJava")
# install.packages("xlsxjars")

#0 - Set work directory
     setwd("/misc/utopia3/bda13/lanhome/Data/Shared/Tanzania/Code") #Alternatives: setwd("C:/Users/bda13/Desktop/Sociology/Papers in Progress/Tanzania/Code")
    # setwd("C:/Users/bda13/Desktop/Tanzania/Code")

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
        nrow(sumstats3)
        kill<-paste(sumstats2$Camp,sumstats2$Wave) %in% paste(sumstats3$Camp,sumstats3$Wave)
    #kill them
        a<-c(varlist,sumvars)
        for(i in 1:length(a)){
          a[[i]]<-a[[i]][kill]
        }
        list2env(a,globalenv())



#3) aggregate ward data
    source("Ward 2 - aggregate objects to wards.R")

#4) prep siena data for HIV (default = close friends for hiv)
    source("Ward 3 - prep siena objects.R")

#5) customize models
    # datasetg <- sienaGroupCreate(dataset)
    # datasetg <- sienaGroupCreate(dataset[c(-1,-3)])
    # datasetg <- sienaGroupCreate(dataset[c(-3)])
    # datasetg <- sienaGroupCreate(dataset[c(-3,-5)])


#selection
    myeff <- getEffects(datasetg,behNintn =10)
    myeff <- includeEffects(myeff, simX, interaction1 = "age")
    myeff <- includeEffects(myeff, sameX, interaction1 = "sex" )
    myeff <- includeEffects(myeff, inPopSqrt)

    #Better for full friends
        # myeff <- includeEffects(myeff, outPopSqrt)
        # myeff <- includeEffects(myeff, Jout)
    #Better for close friends
        myeff <- setEffect(myeff, outTrunc, parameter = 1)
        myeff <- includeEffects(myeff, gwespFF, parameter=69)
        myeff <- includeEffects(myeff, outInAss)

    #the rest
        myeff <- includeInteraction(myeff, density, egoX, interaction1 = c('','wave'),name="friendship")
        myeff <- includeEffects(myeff, altX,simX,egoX, interaction1='DV',name='friendship')
        myeff <- includeEffects(myeff, X, interaction1='ffriendship')
        myeff <- includeEffects(myeff, egoX,interaction1='ward2')
        myeff <- includeEffects(myeff, egoX,interaction1='ward4')

#behavior
    # myeff <- includeEffects(myeff, outdeg, interaction1 = "friendship", name = "DV")
    myeff <- includeEffects(myeff, effFrom, interaction1 = "male", name = "DV")
    myeff <- includeEffects(myeff, effFrom, interaction1 = "sex.num2", name = "DV")
    myeff <- includeEffects(myeff, effFrom, interaction1 = "ses", name = "DV")
    myeff <- includeEffects(myeff, effFrom, interaction1 = "wave", name = "DV")
    # myeff <- includeInteraction(myeff, linear, effFrom, effFrom, interaction1 = c("",'wave','ward4'),name="DV")


    myeff <- includeEffects(myeff, effFrom, interaction1 = "condition", name = "DV")
    myeff <- includeEffects(myeff, effFrom, interaction1 = "leader", name = "DV")
    myeff <- includeEffects(myeff, effFrom, interaction1 = "child.sex.violence", name = "DV")
    myeff <- includeEffects(myeff, effFrom, interaction1 = "camp.duration", name = "DV")
    myeff <- includeEffects(myeff, effFrom, interaction1 = "has.children", name = "DV")
    myeff <- includeEffects(myeff, effFrom, interaction1 = "fam.available", name = "DV")



    # myeff <- includeInteraction(myeff, linear, effFrom, interaction1 = c("",'male'),name="DV")
    # myeff <- includeEffects(myeff, effFrom, interaction1 = "alc_evr", name = "DV")
    # myeff <- includeInteraction(myeff, linear, effFrom, interaction1 = c('','ward2'),name="DV")
    # myeff <- includeInteraction(myeff, linear, effFrom, interaction1 = c('','ward4'),name="DV")
    # myeff <- includeInteraction(myeff, linear, effFrom, interaction1 = c('','ward3'),name="DV")


    myalgorithm <- sienaAlgorithmCreate( projname = 'SingleGroups', useStdInits = FALSE,
                                         nsub=4, n3=2000,MaxDegree = c(friendship=3),cond = F, firstg=.1, doubleAveraging=1) #,


    # #If friendship = full friends
    #     myalgorithm <- sienaAlgorithmCreate( projname = 'SingleGroups', useStdInits = FALSE,
    #         nsub=4, n3=2000,cond = F, firstg=.05, doubleAveraging=1,MaxDegree = c(friendship=8)) #,

#6) multiple model
    m0 <- myeff
    m0 <- includeEffects(m0, avSim, interaction1 = c('friendship'),name="DV")
    # m0 <- includeEffects(m0, avAltDist2, interaction1 = c('friendship'),name="DV")
    # m0 <- includeEffects(m0, avRecAlt, interaction1 = c('friendship'),name="DV")
    # m0 <- includeEffects(m0, maxAlt, interaction1 = c('friendship'),name="DV")



    # m0 <- includeEffects(m0, avAlt, interaction1 = c('friendship'),name="DV")
        #slightly weaker effect
    # m0 <- includeEffects(m0, avSimW, interaction1='friendship', interaction2='bothmale', name='DV')
        #slightly weaker effect
    # m0 <- includeEffects(m0, effFrom, interaction1 = "ever.test", name = "DV")
        #kills convergence
    # m0 <- includeInteraction(m0, linear, effFrom, interaction1 = c('','condition'),name="DV")
        #no effect
    
    ncores<-detectCores()
    
#7) run models
    lms<-list(m0,m1,m2,m3,m4,m5,m6,m7,m8,m9,m10);kill<-sapply(lms,function(x) class(x[[1]][1]))=="character"; lms<-lms[kill];ml<-list()
    baseMod <- siena07(myalgorithm, data = datasetg, effects = lms[[1]], nbrNodes=ncores, useCluster=T, initC=T, returnDeps=TRUE, batch=T, verbose = F)
    Model<-baseMod
    htmlfilename<-printfun(Model)

    # Model <- siena07(myalgorithm, data = datasetg, effects = lms[[1]], nbrNodes=4, useCluster=T, initC=T, returnDeps=TRUE, batch=T, verbose = F,prevAns = Model)
    # htmlfilename<-printfun(Model)

#8) append results to excel file
    #initiate excel file (do this only once!)
        # source("Ward 4.1 - save results to excel (1st time).R")
    #append results to excel
        # source("Ward 4.2 - save results to excel (2 or more).R")

#view html
    browseURL(htmlfilename)
#download xlsx
    browseURL(filename)


#MOST RECENT CHANGE:
    #1)allowed for more missingness - didn't change much
    #2)remove outdeg just to see - was successful
    #3)reduced core count and changed selection effects ; Runs better.
    #4)reintroduce wards 1 and 3 from first wave ; made model worse
    #5)decreased missingness and increased jaccard requirements ; decreased significance
    #6)reintroduce wards 1 and 3 from first wave after step 5. ; no good.
    #7)keep ward 1 but remove ward 3; better
    #8)run 7 again; no difference
    #9)Try without ward 3 in wave 1; a little better, but starting to seem arbitrary
    #10)bring back ward 3 and change wave-interaction to that specific time period ; not useful
    #11)remove wave interaction ; worse
    #12)try modeling male as an interaction ; no difference
    #13)put condition, leader, and sexual violence in model ; no effects
    #14)try camp duration, has kids, family available ; no effects
    #15)try full distance 2 ; nope
    #16)try reciprocated ties ; nope
    #17)try maxAlt ; nope
    #18)try full friendship (already reverted all steps in code for this) ; nope
    #19)reset all, include ward 3, and save output. ; bad. wave 4 screws things up
    #20)remove ward 3 again; yep just as good as before
    #21)try a version with a smaller N; much worse
    #22)try another version with a smaller N;
    #23)try another version with a smaller N and without ward-wave 1; nope
    #24)try kitchen sink model again (keep if still significant), and switch outdegree trunc with outjaccard


    #To do
    #x1)run old version with fewer camps and camp-level interactions; no good
    #x2)run old version without outdeg and decreasing number of camps; better but still bad
    #x3)run old version with current specification; nope. Not enough power


    # for(i in 2:c(length(lms))){ #run each model twice
    #     ml[[i]] <- siena07(myalgorithm, data = datasetg, effects = lms[[i]], nbrNodes=24, useCluster=T, initC=T, returnDeps=TRUE, batch=T, verbose = F,prevAns = baseMod)
    #     Model<-ml[[i]]
    #     ml[[i]] <- siena07(myalgorithm, data = datasetg, effects = lms[[i]], nbrNodes=24, useCluster=T, initC=T, returnDeps=TRUE, batch=T, verbose = F,prevAns = Model)
    #     Model<-ml[[i]]
    #     printfun(Model)
    #     gc()
    #     closeAllConnections()
    # }
    # saveRDS(ml,"ml.rds")
    # saveRDS(datasetg,"datasetg.rds")
