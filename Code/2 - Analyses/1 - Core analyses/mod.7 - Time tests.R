#1) time test
    #a) determine which effects to time test
        Model$requestedEffects$effectName
        (timetest<-sienaTimeTest(Model,effects = c(48)))
    #b) grab useful info; delete useless info
        a<-as.data.frame(timetest$IndividualTest)
        a$`Initial Est.`<-NULL
    #c) optional: remove those with low p values or step estimates
        a<-a[a$`p-Value`<.05,]
        # a<-a[abs(a$`One Step Est.`)<1,]
    #d) explore individual camps
        outlie<-c(6,7,10) #manually assign which to look at
        tsum<-sumstats3[outlie,]
        sumstats5<-sumstats3[,c("N","Wave","density","ffjaccard","missing")]
        for(i in 1:ncol(sumstats5)){
          na<-names(sumstats5)[i]
          tr<-round(mean(sumstats5[outlie,i]),2)
          co<-round(mean(sumstats5[,i]),2)
          print(paste(na,"- Sample =",tr,"vs Camp =",co))
        }
        sumstats5[outlie,]$d.hiv
    #e) explore stat that seems screwed up
        b<-which(sumstats3$ffjaccard<.2)
        a<-as.data.frame(timetest$IndividualTest)
        a$`Initial Est.`<-NULL
        a<-a[grepl("out-Jaccard",row.names(a)),] #manually change effect name
        a[b,]
