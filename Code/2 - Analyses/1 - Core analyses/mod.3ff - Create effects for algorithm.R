#0 - #Prevent from printing in the console
    capture.output({  
#1 - Base effects
    myeff <- getEffects(datasetg,behNintn =10)
    #if camp dummies
        # if(options$campdums==T){
        #     myeff <- getEffects(datasetg,behNintn = length(dfnames)+10) #allows more intereactions when interacting with many camps
        # }
#2 - Friendship
    #i - Structural effects
        myeff <- includeEffects(myeff, inPopSqrt)
        myeff <- setEffect(myeff, outTrunc, parameter = 1)
        myeff <- includeEffects(myeff, gwespFF, parameter=69)
        myeff <- includeEffects(myeff, outInAss)
    #ii - Full network effects
        # myeff <- includeEffects(myeff, X, interaction1='ffriendship')
    #iii - Selection effects
        myeff <- includeEffects(myeff, simX, interaction1 = "age")
        myeff <- includeEffects(myeff, sameX, interaction1 = "sex" )
        # myeff <- includeEffects(myeff, egoX, altX, simX, interaction1 = "gem") 
        #myeff <- includeInteraction(myeff, density, egoX, interaction1 = c('','wave'),name="friendship")
        #myeff <- includeEffects(myeff, egoX,interaction1='ward2')
        #myeff <- includeEffects(myeff, egoX,interaction1='ward4')
        
#3 - Behavior  
    #i - Peer influence
         
    #ii - Behavior controls
        # myeff <- includeInteraction(myeff, linear, effFrom, interaction1 = c('','ward2'),name="DV")
        # myeff <- includeInteraction(myeff, linear, effFrom, interaction1 = c('','ward4'),name="DV")
        # myeff <- includeInteraction(myeff, linear, effFrom, interaction1 = c('','wave'),name="DV")
        # myeff <- includeEffects(myeff, effFrom, interaction1 = "sex", name = "DV")
        # myeff <- includeEffects(myeff, effFrom, interaction1 = "sex.active", name = "DV")
        # 
        # myeff <- includeEffects(myeff, avSim, interaction1 = "friendship", name = "DV")
        # myeff <- includeEffects(myeff, avAlt, interaction1 = "friendship", name = "DV")
        # myeff <- includeInteraction(myeff, avAlt, effFrom, interaction1 = c("friendship","sex"), name = "DV")
        # myeff <- includeInteraction(myeff, avAlt, effFrom, interaction1 = c("friendship","sex.year"), name = "DV")
        # myeff <- includeInteraction(myeff, avAlt, effFrom, interaction1 = c("friendship","sex.active"), name = "DV")
        # myeff <- includeEffects(myeff, effFrom, interaction1 = "alc_frq", name = "DV")
        # myeff <- includeEffects(myeff, avXAlt, interaction1='sex', interaction2='friendship', name='DV')
        # myeff <- includeEffects(myeff, avAltDist2, interaction1='friendship', name='DV')
        # myeff <- includeEffects(myeff, avXAlt, interaction1='leader', interaction2='friendship', name='DV')
        # myeff <- includeEffects(myeff, effFrom, interaction1 = "gem", name = "DV")

        
        # myeff <- includeEffects(myeff, effFrom, interaction1 = "ever.ipv", name = "DV")
        # myeff <- includeInteraction(myeff, linear, effFrom, interaction1 = c('','ward2'),name="DV")
        # myeff <- includeInteraction(myeff, linear, effFrom, interaction1 = c('','ward4'),name="DV")
        # myeff <- includeInteraction(myeff, linear, effFrom, interaction1 = c('','wave'),name="DV")

        # myeff <- includeEffects(myeff, avXAlt, interaction1='alc_frq', interaction2='friendship', name='DV')
        # myeff <- includeInteraction(myeff, linear, effFrom, interaction1 = c('','ward3'),name="DV")
        # myeff <- includeEffects(myeff, outdeg, interaction1 = "friendship", name = "DV")
        
        # myeff <- includeEffects(myeff, effFrom, interaction1 = "child.violence", name = "DV")
        # myeff <- includeInteraction(myeff, linear, effFrom, interaction1 = c('','ward2'),name="DV")
        # myeff <- includeInteraction(myeff, linear, effFrom, interaction1 = c('','ward3'),name="DV")
        # myeff <- includeInteraction(myeff, linear, effFrom, interaction1 = c('','ward4'),name="DV")
        # myeff <- includeInteraction(myeff, linear, effFrom, interaction1 = c('','wave'),name="DV")
        # myeff <- includeEffects(myeff, effFrom, interaction1 = "age", name = "DV")
        # myeff <- includeEffects(myeff, effFrom, interaction1 = "sex.year", name = "DV")
        # myeff <- includeInteraction(myeff, linear, effFrom, interaction1 = c('','N1'),name="DV")
        # myeff <- includeEffects(myeff, effFrom, interaction1 = "alc_evr", name = "DV")
        # myeff <- includeEffects(myeff, effFrom, interaction1 = "alc_bing", name = "DV")
        # myeff <- includeEffects(myeff, effFrom, interaction1 = "drnk_frq", name = "DV")
        # myeff <- includeInteraction(myeff, linear, effFrom, interaction1 = c('','N1'),name="DV")
        # myeff <- includeInteraction(myeff, linear, effFrom, interaction1 = c('','jaccard'),name="DV")
        # myeff <- includeInteraction(myeff, linear, effFrom, interaction1 = c('','missing'),name="DV")
        # myeff <- includeInteraction(myeff, linear, effFrom, interaction1 = c('','d.ipv'),name="DV")
        # myeff <- includeInteraction(myeff, linear, effFrom, interaction1 = c('','ward1'),name="DV")
        # myeff <- includeInteraction(myeff, linear, effFrom, interaction1 = c('','ward2'),name="DV")
        # myeff <- includeInteraction(myeff, linear, effFrom, interaction1 = c('','ward3'),name="DV")
        # myeff <- includeInteraction(myeff, linear, effFrom, interaction1 = c('','ward4'),name="DV")
        #interaction syntax
            # myeff <- includeInteraction(myeff, linear, effFrom, interaction1 = c('',paste("camp",sumstats2$Wave[1],sumstats2$campn[1],sep="")),name="DV")
            # myeff <- includeInteraction(myeff, quad, effFrom, interaction1 = c('',paste("camp",sumstats2$Wave[1],sumstats2$campn[1],sep="")),name="DV")
            # myeff <- includeInteraction(myeff, avAlt, effFrom, interaction1 = c('friendship',paste("camp",sumstats2$Wave[1],sumstats2$campn[1],sep="")),name="DV")
            # myeff <- includeInteraction(myeff, outdeg, effFrom, interaction1 = c('friendship',paste("camp",sumstats2$Wave[1],sumstats2$campn[1],sep="")),name="DV")
            # myeff <- includeInteraction(myeff, effFrom, effFrom, interaction1 = c('sex',paste("camp",sumstats2$Wave[1],sumstats2$campn[1],sep="")),name="DV")
            # myeff <- includeInteraction(myeff, effFrom, effFrom, interaction1 = c('sex.active',paste("camp",sumstats2$Wave[1],sumstats2$campn[1],sep="")),name="DV")
            # myeff <- includeInteraction(myeff, effFrom, effFrom, interaction1 = c('child.violence',paste("camp",sumstats2$Wave[1],sumstats2$campn[1],sep="")),name="DV")
            # myeff <- includeInteraction(myeff, linear, effFrom, effFrom, interaction1 = c('','wave',paste("camp",sumstats2$Wave[1],sumstats2$campn[1],sep="")),name="DV")
            
        #Create algorithm
        # myalgorithm <- sienaAlgorithmCreate( projname = 'SingleGroups', useStdInits = FALSE, nsub=4, n3=2000, MaxDegree = c(friendship=4),cond = F) #, MaxDegree = c(friendship=3)
        myalgorithm <- sienaAlgorithmCreate( projname = 'SingleGroups', useStdInits = FALSE, nsub=4, n3=2000,cond = F) #, MaxDegree = c(friendship=3)
        
        
        #4 - Create algorithm
        #        myalgorithm <- sienaAlgorithmCreate( projname = 'SingleGroups', useStdInits = FALSE, nsub=4, n3=2000, MaxDegree = c(friendship=4),cond = T)#, MaxDegree = c(friendship=3)
        #        myalgorithm <- sienaAlgorithmCreate( projname = 'SingleGroups', useStdInits = FALSE, nsub=1, n3=500, MaxDegree = c(friendship=4),cond = T, firstg=.01, doubleAveraging=2)#, MaxDegree = c(friendship=3)
                 #myalgorithm <- sienaAlgorithmCreate( projname = 'SingleGroups', useStdInits = FALSE, nsub=5, n3=2000, MaxDegree = c(friendship=4),cond = F, firstg=.05, doubleAveraging=1)#, MaxDegree = c(friendship=3)
    }, file='NUL')
    
        
        
        
        
        
        
        
    #ffipv,ffgem,ffalc,ffgend,ffnum,ffhiv,clipv,clgem,clalc,clgend,clnum,clhiv
        
        
        
        
            
#5 - Optionals
    # #i - Turn off DV side of function entirely
        # a<-myeff[myeff$include==T,]$effectName
        # myeff[myeff$include==T,][grep("rate DV",a),]$fix<-T
        # myeff[myeff$include==T,][grep("rate DV",a),]$initialValue<-1
        # myeff[myeff$effectName=="DV average similarity",]$include<-F
        # myeff[myeff$effectName=="DV alter",]$include<-F
        # myeff[myeff$effectName=="DV ego",]$include<-F
        # myeff[myeff$effectName=="DV similarity",]$include<-F
    
    #ii - fix behavior function to speed things up
    # a<-myeff[myeff$include==T,]$effectName
    # myeff[myeff$include==T,][grep("rate DV",a),]$fix<-T
    # myeff[myeff$include==T,][grep("rate DV",a),]$initialValue<-3
    
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
    #Behavior
        #peer influence (DV)
            # myeff <- includeEffects(myeff, avSim, interaction1 = "friendship", name = "DV")
            # myeff <- includeEffects(myeff, avSimPopAlt, interaction1 = "friendship", name = "DV")
            # myeff <- includeEffects(myeff, avAltPop, interaction1 = "friendship", name = "DV")
            # myeff <- includeEffects(myeff, avAltDist2, interaction1='friendship', name='DV')
            # myeff <- includeEffects(myeff, avSimPopEgo, interaction1='friendship', name='DV')
            # myeff <- includeEffects(myeff, avInAltDist2, interaction1='friendship', name='DV')
            # myeff <- includeEffects(myeff, avXAlt, interaction1='leader', interaction2='friendship', name='DV')
            # myeff <- includeEffects(myeff, totXInAltDist2, interaction1='leader', interaction2='friendship', name='DV')
            # myeff <- includeInteraction(myeff, avAlt, effFrom, interaction1 = c("friendship","age"), name = "DV")
            # myeff <- includeInteraction(myeff, avAlt, effFrom, interaction1 = c("friendship","sex"), name = "DV")
            # myeff <- includeInteraction(myeff, avAlt, effFrom, interaction1 = c("friendship","sex.year"), name = "DV")
            # myeff <- includeInteraction(myeff, avAlt, effFrom, interaction1 = c("friendship","sex.active"), name = "DV")
            # myeff <- includeInteraction(myeff, avSim, effFrom, interaction1 = c("friendship","sex.active"), name = "DV")
            # myeff <- includeInteraction(myeff, avAlt, effFrom, interaction1 = c("friendship","child.violence"), name = "DV")
            # myeff <- includeInteraction(myeff, linear, effFrom, avAlt, interaction1 = c('','wave','friendship'),name="DV")
            # myeff <- includeInteraction(myeff, avSim, effFrom, interaction1 = c("friendship","has.children"), name = "DV")
            # myeff <- includeInteraction(myeff, avAlt, effFrom, interaction1 = c("friendship","fam.available"), name = "DV")
        #peer influence (covariates)
        #controls
            # myeff <- includeEffects(myeff, outdeg, interaction1 = "friendship", name = "DV")
            # myeff <- includeEffects(myeff, effFrom, interaction1 = "sex", name = "DV")
            # myeff <- includeInteraction(myeff, linear, effFrom, interaction1 = c('','wave'),name="DV")
            # myeff <- includeEffects(myeff, effFrom, interaction1 = "age", name = "DV")
            # myeff <- includeEffects(myeff, effFrom, interaction1 = "edu", name = "DV")
            # myeff <- includeEffects(myeff, effFrom, interaction1 = "sex.active", name = "DV")
            # myeff <- includeEffects(myeff, effFrom, interaction1 = "fam.available", name = "DV")
            # myeff <- includeEffects(myeff, effFrom, interaction1 = "has.children", name = "DV")
            # myeff <- includeEffects(myeff, effFrom, interaction1 = "married", name = "DV")
            # myeff <- includeEffects(myeff, effFrom, interaction1 = "sex.year", name = "DV")
            # myeff <- includeEffects(myeff, effFrom, interaction1 = "child.violence", name = "DV")
            # myeff <- includeEffects(myeff, effFrom, interaction1 = "child.sex.violence", name = "DV")
            # myeff <- includeEffects(myeff, effFrom, interaction1 = "camp.duration", name = "DV")
            # myeff <- includeEffects(myeff, effFrom, interaction1 = "leader", name = "DV")
            # myeff <- includeEffects(myeff, effFrom, interaction1 = "ses", name = "DV")
            # myeff <- includeEffects(myeff, effFrom, interaction1 = "ever.test", name = "DV")
            # myeff <- includeInteraction(myeff, linear, effFrom, interaction1 = c('','condition'),name="DV")
            # myeff <- includeEffects(myeff, effFrom, interaction1 = "ever.ipv", name = "DV")
            # myeff <- includeInteraction(myeff, linear, effFrom, interaction1 = c('','ward2'),name="DV")
            # myeff <- includeInteraction(myeff, linear, effFrom, interaction1 = c('','ward3'),name="DV")
            # myeff <- includeInteraction(myeff, linear, effFrom, interaction1 = c('','ward4'),name="DV")
            # myeff <- includeEffects(myeff, effFrom, interaction1 = "gem", name = "DV")
