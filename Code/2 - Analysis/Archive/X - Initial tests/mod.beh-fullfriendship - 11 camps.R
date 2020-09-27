q=1
ml<-list()

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
      
      
      options$DVname<-"ipvoutcome_cat" #gem_r15_avg ipvoutcome_cat alc_freq testhiv_12 all_ipv dv.ipvphysical_cat
      # options$DVname<-"ipvphysical_cat" 
      DV<-eval(parse(text=paste("dv.",options$DVname,sep="")))
      
  #2.5 - Temporary steps
          sex.num2<-sex.num    
          for(i in 1:length(friendship)){
            #convert closest friends into coDyadCovar
              friendship[[i]]<-coDyadCovar(friendship[[i]][,,1])
            #Convert DV to binary
              # DV[[i]][]<-ifelse(DV[[i]][]>0,1,0)
            #create a different sexually active variable
              sex.num2[[i]][]<-ifelse(sex.num2[[i]][]>0,1,0)
          }
  
  #3 - Create RSiena objects
      dataset<-list()
      for(i in 1:length(friendship)){ #camp 3 wave 1 generates warning 
        suppressWarnings(dataset[[i]] <- sienaDataCreate( friendship = dv.ffriendship[[i]], DV = DV[[i]],closefriend=friendship[[i]], ffriendship = ffriendship[[i]], gem = gem[[i]], sex = sex[[i]], age = age[[i]], edu = edu[[i]], married = married[[i]], ses = ses[[i]],campn=campn[[i]], ward1=ward1[[i]], ward2=ward2[[i]],ward3=ward3[[i]],ward4=ward4[[i]], condition=condition[[i]], leader=leader[[i]], sex.active=sex.active[[i]], sex.year=sex.year[[i]], child.violence=child.violence[[i]], child.sex.violence=child.sex.violence[[i]], camp.duration=camp.duration[[i]],wave=wave[[i]], has.children = has.children[[i]], fam.available = fam.available[[i]], hivtst.ego = hivtst.ego[[i]], hivinjf.ego = hivinjf.ego[[i]], hivadvf.ego = hivadvf.ego[[i]], knowf.ego = knowf.ego[[i]], closef.ego = closef.ego[[i]], dy.hivtstf = hivtstf[[i]], dy.hivinjf = hivinjf[[i]], dy.hivadvf = hivadvf[[i]], dy.knowf = knowf[[i]], dy.closef = closef[[i]], ever.test=ever.test[[i]],ever.ipv=ever.ipv[[i]], N1=N1[[i]],jaccard=jaccard[[i]],d.ipv=d.ipv[[i]],d.hiv=d.hiv[[i]],r.hiv=r.hiv[[i]],density=density[[i]],r.hivtst=r.hivtst[[i]],r.hivinj=r.hivinj[[i]],r.hivadv=r.hivadv[[i]],alc_evr=alc_evr[[i]],alc_frq=alc_frq[[i]],alc_bing=alc_bing[[i]],drnk_frq=drnk_frq[[i]],hivtst.alter = hivtst.alter[[i]],hivinjf.alter = hivinjf.alter[[i]],hivadvf.alter = hivadvf.alter[[i]],hivtst.group = hivtst.group[[i]], sex.num=sex.num[[i]], ffipv=ffipv[[i]],ffgem=ffgem[[i]],ffalc=ffalc[[i]],ffgend=ffgend[[i]],ffnum=ffnum[[i]],ffhiv=ffhiv[[i]],clipv=clipv[[i]],clgem=clgem[[i]],clalc=clalc[[i]],clgend=clgend[[i]],clnum=clnum[[i]],clhiv=clhiv[[i]],r.ipv=r.ipv[[i]],r.gem=r.gem[[i]],r.alc=r.alc[[i]],r.gend=r.gend[[i]],bothmale=bothmale[[i]],sex.num2=sex.num2[[i]],male=male[[i]]))
      }

  #4 - Remove unecessary stuff from environment
      rm(list=setdiff(ls(), c("dataset","options","sumstats","sumstats2","goodcamps","SaveResults","ml","q")))

#II - subset data
      sumstats2<-readRDS("sumstats2.rds")
      sumstats3<-sumstats2
      sumstats3<-sumstats3[sumstats3$N>15,]
      sumstats3<-sumstats3[sumstats3$missing<.2,]
      sumstats3<-sumstats3[sumstats3$ffjaccard>.15,]
      sumstats3<-sumstats3[sumstats3$d.ipv>.1,]
      sumstats3<-sumstats3[sumstats3$r.ipv>.1,]

      
      
    #11 good camps
      sumstats3<- sumstats3[c(1:5,7:9,11:20,22:23),][c(-9,-19),][c(-4,-8,-9),][-4,][-4,][-7,][-4,]
      
    #6 good camps from the past
      base.info<-data.frame(Camp=c(12,47,35,36,53,60),
                            Wave=c(1,1,3,3,3,3))
      sumstats4<-sumstats2[which(paste(sumstats2$Wave,sumstats2$Camp) %in% paste(base.info$Wave,base.info$Camp)),]
    
    #7 good camps from August
      base.info2<-data.frame(Camp=c(9,12,35,36,48,53,60),
                            Wave=c(1,1,3,3,3,3,3))
      sumstats5<-sumstats2[which(paste(sumstats2$Wave,sumstats2$Camp) %in% paste(base.info2$Wave,base.info2$Camp)),]
      
    #merge these
      sumstats3<-rbind(sumstats3,sumstats4)
      sumstats3<-rbind(sumstats3,sumstats5)
      sumstats3<-sumstats3[!duplicated(paste(sumstats3$Wave,sumstats3$Camp)),]
    
    #prep data
      dataset2<-dataset[which(paste(sumstats2$Wave,sumstats2$Camp) %in% paste(sumstats3$Wave,sumstats3$Camp))]
      dataset2<-dataset2[-14][-13]
      datasetg <- sienaGroupCreate(dataset2)
      
      # sapply(dataset2, function(x) sum(x$depvars$DV[,1,1],na.rm=T))
      # sapply(dataset2, function(x) sum(x$depvars$DV[,1,2],na.rm=T))
      # sapply(dataset2, function(x) sum(x$depvars$DV[,1,1],na.rm=T)/length(x$depvars$DV[,1,1]))
      # sapply(dataset2, function(x) sum(x$depvars$DV[,1,2],na.rm=T)/length(x$depvars$DV[,1,2]))
      # 

#III - prep model
      myeff <- getEffects(datasetg,behNintn =10)
      myeff <- includeEffects(myeff, inPopSqrt)
      #myeff <- setEffect(myeff, outTrunc, parameter = 1)
      #myeff <- includeEffects(myeff, gwespFF, parameter=69)
      #myeff <- includeEffects(myeff, outInAss)
      myeff <- includeEffects(myeff, simX, interaction1 = "age")
      myeff <- includeEffects(myeff, sameX, interaction1 = "sex" )
      myeff <- includeEffects(myeff, Jout)
      myeff <- includeEffects(myeff, outPopSqrt)
      myeff <- includeInteraction(myeff, density, egoX, interaction1 = c('','wave'),name="friendship")
      # myeff <- includeInteraction(myeff, sameX, egoX, interaction1 = c('sex','r.gend'),name="friendship")
      # myeff <- includeEffects(myeff, X, interaction1='closefriend')
      
    #DV  
      myeff <- includeInteraction(myeff, X, altX, interaction1 = c("bothmale","DV"),name="friendship")
      myeff <- includeInteraction(myeff, X, simX, interaction1 = c("bothmale","DV"),name="friendship")
      myeff <- includeInteraction(myeff, X, egoX, interaction1 = c("bothmale","DV"),name="friendship")
      myeff <- includeEffects(myeff, X, interaction1='bothmale')
    #behav
      myeff <- includeEffects(myeff, effFrom, interaction1 = "male", name = "DV")
      # myeff <- includeInteraction(myeff, effFrom, effFrom, interaction1=c('male','sex.active'), name='DV') #doesn't matter
      # myeff <- includeInteraction(myeff, effFrom, effFrom, interaction1=c('male','sex.num2'), name='DV')
      myeff <- includeInteraction(myeff, effFrom, effFrom, interaction1=c('male','sex.num'), name='DV')
      # myeff <- includeInteraction(myeff, effFrom, effFrom, interaction1=c('male','child.violence'), name='DV')
      # myeff <- includeInteraction(myeff, effFrom, effFrom, interaction1=c('male','camp.duration'), name='DV') #doesn't matter      


      # myeff <- includeEffects(myeff, avSimW, interaction1='friendship', interaction2='bothmale', name='DV')
      
      # myeff <- includeInteraction(myeff, avSimW,avSimW, interaction1=c('friendship','friendship'), interaction2=c('bothmale','closefriend'), name='DV')
      myeff <- includeInteraction(myeff, avSimW,effFrom, interaction1=c('friendship','sex.num2'), interaction2=c('bothmale',""), name='DV')
      
      
                
      # myeff <- includeInteraction(myeff, effFrom, effFrom, interaction1=c('male','ses'), name='DV')
      # myeff <- includeInteraction(myeff, effFrom, effFrom, interaction1=c('male','fam.available'), name='DV')
      

      # myeff <- includeEffects(myeff, effFrom, interaction1 = "gem", name = "DV")  #doesn't matter at all
      # myeff <- includeInteraction(myeff, effFrom, effFrom, interaction1=c('male','ever.ipv'), name='DV') #might matter a lot


      # myeff <- includeInteraction(myeff, effFrom, effFrom, interaction1=c('male','has.children'), name='DV') #might matter
      
      #   myeff <- includeEffects(myeff, avSimW, interaction1='friendship', interaction2='bothmale', interaction3='closefriend', name='DV')
      
    
        # myeff <- includeEffects(myeff, avWAlt, interaction1='friendship', interaction2='bothmale', name='DV')
        # myeff <- includeInteraction(myeff, avWAlt, avXAlt, interaction1=c('friendship','alc_frq'),
        #                           interaction2=c('bothmale','friendship'), name='DV')

      
      # myeff <- includeInteraction(myeff, effFrom, effFrom, interaction1=c('male','drnk_frq'), name='DV')
      # myeff <- includeInteraction(myeff, effFrom, effFrom, interaction1=c('male','sex.num'), name='DV')
      # myeff <- includeInteraction(myeff, effFrom, avSim, interaction1=c('male','friendship'), name='DV')

      # if(q==1){
      # myeff <- includeEffects(myeff, avWAlt, interaction1='friendship', interaction2='bothmale', name='DV')
      
      #   myeff <- includeEffects(myeff, avWAlt, interaction1='friendship', interaction2='bothmale', interaction3='closefriend', name='DV')
      # }
      # if(q==2){
      #   myeff <- includeEffects(myeff, avWAlt, interaction1='friendship', interaction2='bothmale', name='DV')
      #   myeff <- includeInteraction(myeff, avWAlt, avXAlt, interaction1=c('friendship','alc_frq'),
      #                             interaction2=c('bothmale','friendship'), name='DV')
      # }
      # if(q==3){
      #   myeff <- includeEffects(myeff, avWAlt, interaction1='friendship', interaction2='bothmale', name='DV')
      #   myeff <- includeInteraction(myeff, avWAlt, avXAlt, interaction1=c('friendship','gem'),
      #                             interaction2=c('bothmale','friendship'), name='DV')
      # }
      # if(q==4){
      #   myeff <- includeEffects(myeff, avWAlt, interaction1='friendship', interaction2='bothmale', name='DV')
      #   myeff <- includeInteraction(myeff, avWAlt, avXAlt, interaction1=c('friendship','alc_frq'),
      #                             interaction2=c('bothmale','friendship'), name='DV')
      # }
      
      myalgorithm <- sienaAlgorithmCreate( projname = 'SingleGroups', useStdInits = FALSE, 
                     nsub=4, n3=2000,cond = F, firstg=.05, doubleAveraging=1,MaxDegree = c(friendship=8)) #,
      
#IV - print function
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

            
#V - run model
      ml[[q]] <- siena07(myalgorithm, data = datasetg, effects = myeff, nbrNodes=24, useCluster=T, initC=T, returnDeps=TRUE, batch=T, verbose = F)
      Model<-ml[[q]]
      printfun(Model)
      
      ml[[q]] <- siena07(myalgorithm, data = datasetg, effects = myeff, nbrNodes=24, useCluster=T, initC=T, returnDeps=TRUE, batch=T, verbose = F,prevAns = Model)
      Model<-ml[[q]]
      printfun(Model)

      ml[[q]] <- siena07(myalgorithm, data = datasetg, effects = myeff, nbrNodes=24, useCluster=T, initC=T, returnDeps=TRUE, batch=T, verbose = F,prevAns = Model)
      Model<-ml[[q]]
      printfun(Model)
      
#Last thing done: changed requirements so that more people have to have committed ipv and ipv correlation needs to be higher.
      
# }
      

#vi - time test
    #manually determine which effects to test      
        Model$requestedEffects$functionName
        (timetest<-sienaTimeTest(Model,effects = c(43)))
    #limit data to just significant outliers
        a<-as.data.frame(timetest$IndividualTest)
        a<-a[a$`p-Value`<.2,]
        a$`Initial Est.`<-NULL
        row.names(a)<-NULL
        a

        for(i in 43:48){
          (timetest<-sienaTimeTest(Model,effects = i))
          print(timetest$ToTest[13,][c(10,12,13)])
        }
        
    #look at a couple of outliers in the data
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
        
        
        (sumstats3$r.gend[c(1,2,4:10)])
        (sumstats3$r.gend[c(3,7,8,11,14,17)])

        for(i in 1:ncol(df)){
          print(paste(names(df)[i],round(cor(df$b_ipv_all_cat,df[,i],use="complete"),2) ))
        }

        dataset2[[1]]$cCovars$child.violence
                
df$b_partnered
df$b_ssavlff
df$b_ssavlmf
df$b_childvio2


