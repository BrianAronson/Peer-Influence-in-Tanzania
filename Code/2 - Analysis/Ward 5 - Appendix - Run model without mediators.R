data.dir
datasetg <- readRDS(file.path(data.dir, "sienadata.rds"))

#1 - specify model
    myeff <- getEffects(datasetg,behNintn =10)
    #Selection
        myeff <- includeEffects(myeff, inPopSqrt)
        myeff <- includeInteraction(myeff, egoX, egoX, inPopSqrt, interaction1 = c('ward3','wave1',''),name="friendship")
        myeff <- includeInteraction(myeff, egoX, egoX, inPop, interaction1 = c('ward3','wave1',''),name="friendship")
        myeff <- includeEffects(myeff, antiInIso )
        myeff <- getEffects(datasetg,behNintn =10)
        myeff <- includeEffects(myeff, simX, interaction1 = "age")
        myeff <- includeEffects(myeff, sameX, interaction1 = "sex" )
        myeff <- includeEffects(myeff, inPopSqrt)
        myeff <- includeInteraction(myeff, density, egoX, interaction1 = c('','wave'),name="friendship")
        myeff <- includeEffects(myeff, altX,simX,egoX, interaction1='DV',name='friendship')
        myeff <- includeEffects(myeff, X, interaction1='ffriendship')
        myeff <- includeEffects(myeff, egoX,interaction1='ward2')
        myeff <- includeEffects(myeff, egoX,interaction1='ward4')
        myeff <- setEffect(myeff, outTrunc, parameter = 1)
        myeff <- includeEffects(myeff, gwespFF, parameter=69)
        myeff <- includeEffects(myeff, outInAss)

    #behavior
        myeff <- includeEffects(myeff, effFrom, interaction1 = "male", name = "DV")
        myeff <- includeEffects(myeff, effFrom, interaction1 = "sex.num2", name = "DV")
        myeff <- includeEffects(myeff, effFrom, interaction1 = "ses", name = "DV")
        myeff <- includeEffects(myeff, effFrom, interaction1 = "condition", name = "DV")
        
        
    #Alternate versions of model
        myeff1 <- myeff
        myeff2 <- myeff
        myeff3 <- myeff
        myeff4 <- myeff
        myeff2 <- includeInteraction(myeff2, effFrom, effFrom, interaction1 = c('leader','wave1'),name="DV")
        myeff3 <- includeEffects(myeff3, avSim, interaction1 = c('friendship'),name="DV")
        myeff4 <- includeEffects(myeff4, avSim, interaction1 = c('friendship'),name="DV")
        myeff4 <- includeInteraction(myeff4, effFrom, effFrom, interaction1 = c('leader','wave1'),name="DV")
        
#2 - specify algorithm
    myalgorithm <- sienaAlgorithmCreate( projname = 'SingleGroups', useStdInits = FALSE,
                                         nsub=4, n3=2000, MaxDegree = c(friendship=3),cond = F, firstg=.1, doubleAveraging=1) #,

#3 - run each model
    run_model <- function(myeff, file_name){
        Model <- siena07(myalgorithm, data = datasetg, effects = myeff, nbrNodes=7, useCluster=T, initC=T, returnDeps=TRUE, batch=T, verbose = F)
        Model <- siena07(myalgorithm, data = datasetg, effects = myeff, nbrNodes=7, useCluster=T, initC=T, returnDeps=TRUE, batch=T, verbose = F, prevAns = Model)
        print(Model)
        print(tconv <- max(abs(Model$tconv)))
        print(tmaxconv <- Model$tconv.max)
        saveRDS(Model, file.path(data.dir, file_name))
        return(list(Model, tconv, tmaxconv))
    }
    Model1 <- run_model(myeff1, "Appendix Model 1.rds")
    Model2 <- run_model(myeff2, "Appendix Model 2.rds")
    Model3 <- run_model(myeff3, "Appendix Model 3.rds")

