#0 - #Prevent from printing in the console
    capture.output({  
#1 - Base effects
    myeff <- getEffects(datasetg,nintn=20)
#2 - Friendship
    #i - Structural effects
        myeff <- includeEffects(myeff, inPopSqrt)
        myeff <- setEffect(myeff, outTrunc, parameter = 1)
        myeff <- includeEffects(myeff, gwespFF, parameter=69)
        myeff <- includeEffects(myeff, outInAss)
    #ii - Full network effects
        myeff <- includeEffects(myeff, X, interaction1='ffriendship')
    #iii - Selection effects
        myeff <- includeEffects(myeff, simX, interaction1 = "age")
        myeff <- includeEffects(myeff, sameX, interaction1 = "sex" )
        myeff <- includeEffects(myeff, egoX, altX, simX, interaction1 = "DV")
        myeff <- includeInteraction(myeff, density, egoX, interaction1 = c('','wave'),name="friendship")
        myeff <- includeEffects(myeff, egoX,interaction1='ward2')
        myeff <- includeEffects(myeff, egoX,interaction1='ward4')
        # myeff <- includeEffects(myeff, egoX,interaction1='ward3')
        # myeff <- includeInteraction(myeff, egoX, egoX, interaction1 = c('ward2','wave'),name="friendship")
        # myeff <- includeInteraction(myeff, egoX, egoX, interaction1 = c('ward3','wave'),name="friendship")
        # myeff <- includeInteraction(myeff, egoX, egoX, interaction1 = c('ward4','wave'),name="friendship")
        # 
        # myeff <- includeEffects(myeff, sameX, interaction1 = "has.children" )

#        myeff <- includeEffects(myeff, sameX, interaction1 = "alc_evr" )
#        myeff <- includeEffects(myeff, sameX, interaction1 = "alc_frq" )
#        myeff <- includeEffects(myeff, sameX, interaction1 = "alc_bing" )
#        myeff <- includeEffects(myeff, sameX, interaction1 = "drnk_frq" )

#6 - alternatives:
    #Friendship
        #dyadic
            # myeff <- includeEffects(myeff, XRecip, interaction1='ffriendship')
        #selection
            # myeff <- includeEffects(myeff, altDist2, simDist2, interaction1 = "DV")
            # myeff <- includeEffects(myeff, altX, interaction1 = "leader")
            # myeff <- includeEffects(myeff, egoX,interaction1='ward2')
            # myeff <- includeEffects(myeff, egoX,interaction1='ward3')
            # myeff <- includeEffects(myeff, egoX,interaction1='ward4')
            # myeff <- includeEffects(myeff, sameX, interaction1 = "fam.available" )
            # myeff <- includeEffects(myeff, simRecipX, interaction1 = "DV")
            # myeff <- includeEffects(myeff, inPopX, interaction1 = "DV")
            # myeff <- includeEffects(myeff, simXTransTrip, interaction1 = "DV")
#Create algorithm
    myalgorithm <- sienaAlgorithmCreate( projname = 'SingleGroups', useStdInits = FALSE, nsub=4, n3=2000, MaxDegree = c(friendship=4),cond = F) #, MaxDegree = c(friendship=3)
})
