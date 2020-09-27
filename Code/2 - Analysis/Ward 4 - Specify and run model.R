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
        myeff <- includeInteraction(myeff, effFrom, effFrom, interaction1 = c('leader','wave1'),name="DV")
        myeff <- includeEffects(myeff, avSim, interaction1 = c('friendship'),name="DV")
        
#2 - specify algorithm
    myalgorithm <- sienaAlgorithmCreate( projname = 'SingleGroups', useStdInits = FALSE,
                                         nsub=4, n3=2000,MaxDegree = c(friendship=3),cond = F, firstg=.1, doubleAveraging=1) #,


#3 - run models
    #run first time
        baseMod <- siena07(myalgorithm, data = datasetg, effects = myeff, nbrNodes=8, useCluster=T, initC=T, returnDeps=TRUE, batch=T, verbose = F)
    #output effects and convergence stats
        Model <- baseMod
        htmlfilename <- printfun(Model)
        max(abs(Model$tconv))
        Model$tconv.max
    #continue running model
        Model <- siena07(myalgorithm, data = datasetg, effects = myeff, nbrNodes=8, useCluster=T, initC=T, returnDeps=TRUE, batch=T, verbose = F, prevAns = Model)
    #output new effects and convergence stats
        htmlfilename<-printfun(Model)
        Model$tconv.max
    
