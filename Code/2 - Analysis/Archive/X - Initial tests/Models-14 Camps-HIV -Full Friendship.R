source("prep.1 - Load libraries.R")
#I - Load data
  #1 - load data
      sumstats<-readRDS("sumstats.rds")
      sumstats2<-readRDS("sumstats2.rds")
      varlist<-readRDS("varlist.rds")
      list2env(varlist,globalenv())
      sumvars<-readRDS("sumvars.rds")
      list2env(sumvars,globalenv())
      goodcamps<-readRDS("goodcamps.rds")
      
  #2 - Choose DV
      options$DVname<-"testhiv_12" #gem_r15_avg ipvoutcome_cat alc_freq testhiv_12 all_ipv dv.ipvphysical_cat
      DV<-eval(parse(text=paste("dv.",options$DVname,sep="")))

  #2.5 - Temporary steps
          sex.num2<-sex.num
          for(i in 1:length(friendship)){
            #Convert DV to binary
              # DV[[i]][]<-ifelse(DV[[i]][]>0,1,0)
            #create a different sexually active variable
              sex.num2[[i]][]<-ifelse(sex.num2[[i]][]>0,1,0)
          }

  #3 - Create RSiena objects
      dataset<-list()
      #If using full friendship
          # #convert closest friends into coDyadCovar
          #     friendship[[i]]<-coDyadCovar(friendship[[i]][,,1])
          # for(i in 1:length(friendship)){ #camp 3 wave 1 generates warning
          #     suppressWarnings(dataset[[i]] <- sienaDataCreate( friendship = dv.ffriendship[[i]], DV = DV[[i]],closefriend=friendship[[i]], ffriendship = ffriendship[[i]], gem = gem[[i]], sex = sex[[i]], age = age[[i]], edu = edu[[i]], married = married[[i]], ses = ses[[i]],campn=campn[[i]], ward1=ward1[[i]], ward2=ward2[[i]],ward3=ward3[[i]],ward4=ward4[[i]], condition=condition[[i]], leader=leader[[i]], sex.active=sex.active[[i]], sex.year=sex.year[[i]], child.violence=child.violence[[i]], child.sex.violence=child.sex.violence[[i]], camp.duration=camp.duration[[i]],wave=wave[[i]], has.children = has.children[[i]], fam.available = fam.available[[i]], hivtst.ego = hivtst.ego[[i]], hivinjf.ego = hivinjf.ego[[i]], hivadvf.ego = hivadvf.ego[[i]], knowf.ego = knowf.ego[[i]], closef.ego = closef.ego[[i]], dy.hivtstf = hivtstf[[i]], dy.hivinjf = hivinjf[[i]], dy.hivadvf = hivadvf[[i]], dy.knowf = knowf[[i]], dy.closef = closef[[i]], ever.test=ever.test[[i]],ever.ipv=ever.ipv[[i]], N1=N1[[i]],jaccard=jaccard[[i]],d.ipv=d.ipv[[i]],d.hiv=d.hiv[[i]],r.hiv=r.hiv[[i]],density=density[[i]],r.hivtst=r.hivtst[[i]],r.hivinj=r.hivinj[[i]],r.hivadv=r.hivadv[[i]],alc_evr=alc_evr[[i]],alc_frq=alc_frq[[i]],alc_bing=alc_bing[[i]],drnk_frq=drnk_frq[[i]],hivtst.alter = hivtst.alter[[i]],hivinjf.alter = hivinjf.alter[[i]],hivadvf.alter = hivadvf.alter[[i]],hivtst.group = hivtst.group[[i]], sex.num=sex.num[[i]], ffipv=ffipv[[i]],ffgem=ffgem[[i]],ffalc=ffalc[[i]],ffgend=ffgend[[i]],ffnum=ffnum[[i]],ffhiv=ffhiv[[i]],clipv=clipv[[i]],clgem=clgem[[i]],clalc=clalc[[i]],clgend=clgend[[i]],clnum=clnum[[i]],clhiv=clhiv[[i]],r.ipv=r.ipv[[i]],r.gem=r.gem[[i]],r.alc=r.alc[[i]],r.gend=r.gend[[i]],bothmale=bothmale[[i]],sex.num2=sex.num2[[i]],male=male[[i]]))
          # }
      #If using close friendship
          for(i in 1:length(friendship)){ #camp 3 wave 1 generates warning
             suppressWarnings(dataset[[i]] <- sienaDataCreate( friendship = friendship[[i]], DV = DV[[i]],ffriendship = ffriendship[[i]], gem = gem[[i]], sex = sex[[i]], age = age[[i]], edu = edu[[i]], married = married[[i]], ses = ses[[i]],campn=campn[[i]], ward1=ward1[[i]], ward2=ward2[[i]],ward3=ward3[[i]],ward4=ward4[[i]], condition=condition[[i]], leader=leader[[i]], sex.active=sex.active[[i]], sex.year=sex.year[[i]], child.violence=child.violence[[i]], child.sex.violence=child.sex.violence[[i]], camp.duration=camp.duration[[i]],wave=wave[[i]], has.children = has.children[[i]], fam.available = fam.available[[i]], hivtst.ego = hivtst.ego[[i]], hivinjf.ego = hivinjf.ego[[i]], hivadvf.ego = hivadvf.ego[[i]], knowf.ego = knowf.ego[[i]], closef.ego = closef.ego[[i]], dy.hivtstf = hivtstf[[i]], dy.hivinjf = hivinjf[[i]], dy.hivadvf = hivadvf[[i]], dy.knowf = knowf[[i]], dy.closef = closef[[i]], ever.test=ever.test[[i]],ever.ipv=ever.ipv[[i]], N1=N1[[i]],jaccard=jaccard[[i]],d.ipv=d.ipv[[i]],d.hiv=d.hiv[[i]],r.hiv=r.hiv[[i]],density=density[[i]],r.hivtst=r.hivtst[[i]],r.hivinj=r.hivinj[[i]],r.hivadv=r.hivadv[[i]],alc_evr=alc_evr[[i]],alc_frq=alc_frq[[i]],alc_bing=alc_bing[[i]],drnk_frq=drnk_frq[[i]],hivtst.alter = hivtst.alter[[i]],hivinjf.alter = hivinjf.alter[[i]],hivadvf.alter = hivadvf.alter[[i]],hivtst.group = hivtst.group[[i]], sex.num=sex.num[[i]], ffipv=ffipv[[i]],ffgem=ffgem[[i]],ffalc=ffalc[[i]],ffgend=ffgend[[i]],ffnum=ffnum[[i]],ffhiv=ffhiv[[i]],clipv=clipv[[i]],clgem=clgem[[i]],clalc=clalc[[i]],clgend=clgend[[i]],clnum=clnum[[i]],clhiv=clhiv[[i]],r.ipv=r.ipv[[i]],r.gem=r.gem[[i]],r.alc=r.alc[[i]],r.gend=r.gend[[i]],bothmale=bothmale[[i]],sex.num2=sex.num2[[i]],male=male[[i]]))
          }

  #4 - Remove unecessary stuff from environment
      rm(list=setdiff(ls(), c("dataset","options","sumstats","sumstats2","goodcamps","SaveResults","ml","q")))
      printfun<-function(Model,name=""){
        #grab useful info from model
            pval <- round(2*pnorm(abs(Model$theta/Model$se),lower.tail=FALSE),4) #Not sure this is correct, but should be close.
            pval <-ifelse(grepl("^constant friendship rate",Model$requestedEffects$effectName) | grepl("^rate DV",Model$requestedEffects$effectName),1,pval)
            htmldf<-data.frame(
              Name=Model$requestedEffects$effectName,
              Estimate=round(Model$theta,3),
              SE=round(Model$se,3),
              pval=ifelse(pval<.001,"***",ifelse(pval<.01,"**",ifelse(pval<.05,"*","")))
            )
        #save as html
            number<-suppressWarnings(max(as.numeric(sub('\\..*', '',gsub("[[:alpha:]]", "", list.files(path="/data/data1/bda13/Data/Shared/Tanzania/Results/Results html",pattern='*\\.html', recursive=TRUE)))),na.rm=T)+1)
            number<-ifelse(is.infinite(number),1,number)
            htmlfilename<-paste("/data/data1/bda13/Data/Shared/Tanzania/Results/Results html/Model ",number,name,".html",sep="")
            print(xtable(htmldf), type="html", file=htmlfilename)
            browseURL(htmlfilename)
      }
      
      
      
#II - subset data to 15 camps that worked in IPV data
  {#If full friends
    #   sumstats2<-readRDS("sumstats2.rds")
    #   sumstats3<-sumstats2
    #   sumstats3<-sumstats3[sumstats3$N>15,]
    #   sumstats3<-sumstats3[sumstats3$missing<.2,]
    #   sumstats3<-sumstats3[sumstats3$ffjaccard>.15,]
    #   sumstats3<-sumstats3[sumstats3$d.ipv>.1,]
    #   sumstats3<-sumstats3[sumstats3$r.ipv>.1,]
    # #11 good camps
    #   sumstats3<- sumstats3[c(1:5,7:9,11:20,22:23),][c(-9,-19),][c(-4,-8,-9),][-4,][-4,][-7,][-4,]
    # #6 good camps
    #   base.info<-data.frame(Camp=c(12,47,35,36,53,60),
    #                         Wave=c(1,1,3,3,3,3))
    #   sumstats4<-sumstats2[which(paste(sumstats2$Wave,sumstats2$Camp) %in% paste(base.info$Wave,base.info$Camp)),]
    # #7 good camps from August
    #   base.info2<-data.frame(Camp=c(9,12,35,36,48,53,60),
    #                         Wave=c(1,1,3,3,3,3,3))
    #   sumstats5<-sumstats2[which(paste(sumstats2$Wave,sumstats2$Camp) %in% paste(base.info2$Wave,base.info2$Camp)),]
    # #merged
    #   sumstats3<-rbind(sumstats3,sumstats4)
    #   sumstats3<-rbind(sumstats3,sumstats5)
    #   sumstats3<-sumstats3[!duplicated(paste(sumstats3$Wave,sumstats3$Camp)),]
    # #prep 
    #   dataset2<-dataset[which(paste(sumstats2$Wave,sumstats2$Camp) %in% paste(sumstats3$Wave,sumstats3$Camp))]
    #   # dataset2<-dataset2[-14][-13]
    #   datasetg <- sienaGroupCreate(dataset2)
}
  #if close friends  
      sumstats2<-readRDS("sumstats2.rds")
      sumstats3<-sumstats2
      # sumstats3<-sumstats3[sumstats3$N>30,]
      sumstats3<-sumstats3[sumstats3$N>15,]
      sumstats3<-sumstats3[sumstats3$missing<.2,]
      sumstats3<-sumstats3[sumstats3$ffjaccard>.15,]
      sumstats3<-sumstats3[sumstats3$d.hiv>.1,]
      sumstats3<-sumstats3[sumstats3$Wave==3,]
            #Camp 15 is problematic 
            sumstats3<-sumstats3[sumstats3$Camp!=15,]
            nrow(sumstats3)

  #TEMPORARY ALTERNATIVE - Increase minimum N
      sumstats2<-readRDS("sumstats2.rds")
      sumstats3<-sumstats2
      sumstats3<-sumstats3[sumstats3$Wave==3,]
      #Camp 15 is problematic
          sumstats3<-sumstats3[sumstats3$Camp!=15,]
      sumstats3<-sumstats3[sumstats3$N>20,]
      # sumstats3<-sumstats3[sumstats3$N<=25 & sumstats3$N>15,]
      sumstats3<-sumstats3[sumstats3$missing<.2,]
      sumstats3<-sumstats3[sumstats3$ffjaccard>.15 | sumstats3$jaccard>.15,]
      sumstats3<-sumstats3[sumstats3$d.hiv>0,]
      # sumstats3<-sumstats3[-2,]
      # sumstats3[sumstats3$missing>.2,c("N","missing")]

    #prep
    dataset2<-dataset[which(paste(sumstats2$Wave,sumstats2$Camp) %in% paste(sumstats3$Wave,sumstats3$Camp))]
    # dataset2<-dataset2[-14][-13]
    datasetg <- sienaGroupCreate(dataset2)

# #temporary alternative:
#     #try using bad camps
#         goodcamps<-goodcamps[goodcamps$Wave==3,]
#         sumstats4<-sumstats2
#         sumstats4<-sumstats4[paste(sumstats4$Wave,sumstats4$Camp) %in% paste(goodcamps$Wave,goodcamps$Camp),]
#         sumstats4<-sumstats4[sumstats4$d.hiv>0,]
#         sumstats4<-sumstats4[!(paste(sumstats4$Wave,sumstats4$Camp) %in% paste(sumstats3$Wave,sumstats3$Camp)),]
#         sumstats3<-sumstats4
#         #prep
#         dataset2<-dataset[which(paste(sumstats2$Wave,sumstats2$Camp) %in% paste(sumstats3$Wave,sumstats3$Camp))]
#         dataset2<-dataset2[c(-4,-22)]
#         datasetg <- sienaGroupCreate(dataset2)
        
#III - Prep Effects
#   #When friendship = full friends
#       myeff <- getEffects(datasetg,behNintn =10)
#       myeff <- includeEffects(myeff, inPopSqrt)
#       myeff <- includeEffects(myeff, simX, interaction1 = "age")
#       myeff <- includeEffects(myeff, sameX, interaction1 = "sex" )
#       myeff <- includeEffects(myeff, Jout)
#       myeff <- includeEffects(myeff, outPopSqrt)
#       myeff <- includeInteraction(myeff, density, egoX, interaction1 = c('','wave'),name="friendship")
#       myeff <- includeEffects(myeff, altX,simX,egoX, interaction1='DV',name='friendship')
#       # myalgorithm <- sienaAlgorithmCreate( projname = 'SingleGroups', useStdInits = FALSE,
#       #                                      nsub=4, n3=2000,cond = F, firstg=.05, doubleAveraging=1,MaxDegree = c(friendship=8)) #,
# 
#       
# #TEMPORARY
#       myeff <- includeEffects(myeff, X, interaction1='ffriendship')
#       myeff <- includeEffects(myeff, egoX,interaction1='ward2')
#       myeff <- includeEffects(myeff, egoX,interaction1='ward4')
    
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
    
    
      myalgorithm <- sienaAlgorithmCreate( projname = 'SingleGroups', useStdInits = FALSE,
                                           nsub=4, n3=2000,cond = F, firstg=.1, doubleAveraging=1,MaxDegree = c(friendship=3)) #,

      
      
      
  # #When friendship = close friends
  #     myeff <- getEffects(datasetg,behNintn =10)
  #         myeff <- includeEffects(myeff, inPopSqrt)
  #         myeff <- setEffect(myeff, outTrunc, parameter = 1)
  #         myeff <- includeEffects(myeff, gwespFF, parameter=69)
  #         myeff <- includeEffects(myeff, outInAss)
  #         myeff <- includeEffects(myeff, X, interaction1='ffriendship')
  #         myeff <- includeEffects(myeff, simX, interaction1 = "age")
  #         myeff <- includeEffects(myeff, sameX, interaction1 = "sex" )
  #         myeff <- includeEffects(myeff, egoX, altX, simX, interaction1 = "DV")
  #         myeff <- includeEffects(myeff, egoX,interaction1='ward2')
  #         myeff <- includeEffects(myeff, egoX,interaction1='ward4')
  #         myalgorithm <- sienaAlgorithmCreate( projname = 'SingleGroups', useStdInits = FALSE, 
  #                                           nsub=4, n3=2000,cond = F, firstg=.05, doubleAveraging=1,MaxDegree = c(friendship=4)) #,
  # #DV effects
  #     myeff <- includeEffects(myeff, outdeg, interaction1 = "friendship", name = "DV")
  #     myeff <- includeEffects(myeff, effFrom, interaction1 = "male", name = "DV")
  #     myeff <- includeEffects(myeff, effFrom, interaction1 = "sex.num2", name = "DV")
  #     myeff <- includeInteraction(myeff, linear, effFrom, interaction1 = c('','condition'),name="DV")
  #     # myeff <- includeEffects(myeff, effFrom, interaction1 = "ever.test", name = "DV")
  #     
  #     # myeff <- includeEffects(myeff, effFrom, interaction1 = "alc_evr", name = "DV")
  #     # myeff <- includeEffects(myeff, effFrom, interaction1 = "ses", name = "DV")
  #     # myeff <- includeEffects(myeff, effFrom, interaction1 = "sex.year", name = "DV")
  #     # myeff <- includeEffects(myeff, effFrom, interaction1 = "alc_evr", name = "DV")
  #     # myeff <- includeEffects(myeff, effFrom, interaction1 = "ses", name = "DV")
      

            
      
  #Monadic peer influence effects
      m0<-NA;m1<-NA;m2<-NA;m3<-NA;m4<-NA;m5<-NA;m6<-NA;m7<-NA;m8<-NA;m9<-NA;m10<-NA;m11<-NA;m12<-NA;m13<-NA;m14<-NA;m15<-NA;m16<-NA;m17<-NA;m18<-NA;m19<-NA;m20<-NA
      m0 <- myeff
      m0 <- includeEffects(m0, avSim, interaction1 = c('friendship'),name="DV") #no
      
      
      
      #7) run models
          lms<-list(m0,m1,m2,m3,m4,m5,m6,m7,m8,m9,m10);kill<-sapply(lms,function(x) class(x[[1]][1]))=="character"; lms<-lms[kill];ml<-list()
          baseMod <- siena07(myalgorithm, data = datasetg, effects = lms[[1]], nbrNodes=4, useCluster=T, initC=T, returnDeps=TRUE, batch=T, verbose = F)
          Model<-baseMod
          htmlfilename<-printfun(Model)

          # Model <- siena07(myalgorithm, data = datasetg, effects = lms[[1]], nbrNodes=4, useCluster=T, initC=T, returnDeps=TRUE, batch=T, verbose = F,prevAns = Model)
          # htmlfilename<-printfun(Model)

      #8) append results to excel file
          #initiate excel file (do this only once!)
              # source("Ward 4.1 - save results to excel (1st time).R")
          #append results to excel
              source("Ward 4.2 - save results to excel (2 or more).R")

      #view html
          browseURL(htmlfilename)
      #download xlsx
          browseURL(filename)
      
      # m1 <- includeEffects(myeff, avWAlt, interaction1 = "friendship", interaction2 = "dy.hivtstf", name = "DV")
      # m1 <- includeEffects(m1, X, interaction1='dy.hivtstf')
      # m2 <- includeEffects(myeff, avWAlt, interaction1 = "friendship", interaction2 = "dy.hivinjf", name = "DV")
      # m2 <- includeEffects(m2, X, interaction1='dy.hivinjf')
      # m3 <- includeEffects(myeff, avWAlt, interaction1 = "friendship", interaction2 = "dy.hivadvf", name = "DV")
      # m3 <- includeEffects(m3, X, interaction1='dy.hivadvf')
      # m5 <- includeInteraction(myeff, linear, effFrom, interaction1 = c('','r.hivtst'),name="DV")
      # m6 <- includeInteraction(myeff, linear, effFrom, interaction1 = c('','r.hivinj'),name="DV")
      # m7 <- includeInteraction(myeff, linear, effFrom, interaction1 = c('','r.hivadv'),name="DV")
      # 
      # m8 <- includeEffects(myeff, effFrom, interaction1 = "hivtst.alter", name = "DV") #yes
      # m8 <- includeInteraction(m8, linear, effFrom, interaction1 = c('','r.hivtst'),name="DV")
      # m8 <- includeEffects(m8, X, interaction1='dy.hivtstf')
      # 
      # m9 <- includeEffects(myeff, effFrom, interaction1 = "hivinjf.alter", name = "DV") #yes
      # m9 <- includeInteraction(m9, linear, effFrom, interaction1 = c('','r.hivinj'),name="DV")
      # m9 <- includeEffects(m9, X, interaction1='dy.hivadvf')
      # 
      #       
      # m10 <- includeEffects(myeff, effFrom, interaction1 = "hivadvf.alter", name = "DV") #yes
      # m10 <- includeInteraction(m10, linear, effFrom, interaction1 = c('','r.hivadv'),name="DV")
      # m10 <- includeEffects(m10, X, interaction1='dy.hivinjf')
      
      
  #thoughts:
      #I have no controls for selecting friends based on norms
      #I don't test whether other people think one's alters have higher rates of HIV
      #It's not clear where people get thoughts about alters' norms
          #I'm not sure whether I need full friendship info
      #If I switch from monadic effects, It might make more sense to model this as total alters
      #I should probably try some random effects models first.

# lms<-list(m0,m1,m2,m3,m4,m5,m6,m7,m8,m9,m10);kill<-sapply(lms,function(x) class(x[[1]][1]))=="character"; lms<-lms[kill];ml<-list()
# 
# baseMod <- siena07(myalgorithm, data = datasetg, effects = lms[[1]], nbrNodes=24, useCluster=T, initC=T, returnDeps=TRUE, batch=T, verbose = F)
# Model<-baseMod
# printfun(Model)
# ml[[1]]<-baseMod
# for(i in 2:c(length(lms))){
#   ml[[i]] <- siena07(myalgorithm, data = datasetg, effects = lms[[i]], nbrNodes=24, useCluster=T, initC=T, returnDeps=TRUE, batch=T, verbose = F,prevAns = baseMod)
#   Model<-ml[[i]]
#   ml[[i]] <- siena07(myalgorithm, data = datasetg, effects = lms[[i]], nbrNodes=24, useCluster=T, initC=T, returnDeps=TRUE, batch=T, verbose = F,prevAns = Model)
#   Model<-ml[[i]]
#   printfun(Model)
#   gc()
#   closeAllConnections()
# }
# ml[[3]]
# i=3
# saveRDS(ml,"ml.rds")
# saveRDS(datasetg,"datasetg.rds")



table(df$condition)
table(df$m_testhiv_12)
table(df$condition,df$m_testhiv_12)
table(df$condition,df$e_testhiv_12)


prop.table(table(df$condition,df$b_testhiv))
prop.table(table(df$condition,df$m_testhiv_12))
prop.table(table(df$condition,df$e_testhiv_12))
