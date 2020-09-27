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
      options$DVname<-"ipvoutcome_cat" #gem_r15_avg ipvoutcome_cat alc_freq testhiv_12 all_ipv
      DV<-eval(parse(text=paste("dv.",options$DVname,sep="")))
      
  #2.5 - Temporary steps
          for(i in 1:length(friendship)){
            #convert closest friends into coDyadCovar
              friendship[[i]]<-coDyadCovar(friendship[[i]][,,1])
            #Convert DV to binary
              # DV[[i]][]<-ifelse(DV[[i]][]>0,1,0)
          }
  
  #3 - Create RSiena objects
      dataset<-list()
      for(i in 1:length(friendship)){ #camp 3 wave 1 generates warning 
        suppressWarnings(dataset[[i]] <- sienaDataCreate( friendship = dv.ffriendship[[i]], DV = DV[[i]],closefriend=friendship[[i]], ffriendship = ffriendship[[i]], gem = gem[[i]], sex = sex[[i]], age = age[[i]], edu = edu[[i]], married = married[[i]], ses = ses[[i]],campn=campn[[i]], ward1=ward1[[i]], ward2=ward2[[i]],ward3=ward3[[i]],ward4=ward4[[i]], condition=condition[[i]], leader=leader[[i]], sex.active=sex.active[[i]], sex.year=sex.year[[i]], child.violence=child.violence[[i]], child.sex.violence=child.sex.violence[[i]], camp.duration=camp.duration[[i]],wave=wave[[i]], has.children = has.children[[i]], fam.available = fam.available[[i]], hivtst.ego = hivtst.ego[[i]], hivinjf.ego = hivinjf.ego[[i]], hivadvf.ego = hivadvf.ego[[i]], knowf.ego = knowf.ego[[i]], closef.ego = closef.ego[[i]], dy.hivtstf = hivtstf[[i]], dy.hivinjf = hivinjf[[i]], dy.hivadvf = hivadvf[[i]], dy.knowf = knowf[[i]], dy.closef = closef[[i]], ever.test=ever.test[[i]],ever.ipv=ever.ipv[[i]], N1=N1[[i]],jaccard=jaccard[[i]],d.ipv=d.ipv[[i]],d.hiv=d.hiv[[i]],r.hiv=r.hiv[[i]],density=density[[i]],r.hivtst=r.hivtst[[i]],r.hivinj=r.hivinj[[i]],r.hivadv=r.hivadv[[i]],alc_evr=alc_evr[[i]],alc_frq=alc_frq[[i]],alc_bing=alc_bing[[i]],drnk_frq=drnk_frq[[i]],hivtst.alter = hivtst.alter[[i]],hivinjf.alter = hivinjf.alter[[i]],hivadvf.alter = hivadvf.alter[[i]],hivtst.group = hivtst.group[[i]], sex.num=sex.num[[i]], ffipv=ffipv[[i]],ffgem=ffgem[[i]],ffalc=ffalc[[i]],ffgend=ffgend[[i]],ffnum=ffnum[[i]],ffhiv=ffhiv[[i]],clipv=clipv[[i]],clgem=clgem[[i]],clalc=clalc[[i]],clgend=clgend[[i]],clnum=clnum[[i]],clhiv=clhiv[[i]],r.ipv=r.ipv[[i]],r.gem=r.gem[[i]],r.alc=r.alc[[i]],r.gend=r.gend[[i]],bothmale=bothmale[[i]],male=male[[i]]))
      }
      
  #4 - Remove unecessary stuff from environment
      rm(list=setdiff(ls(), c("dataset","options","sumstats","sumstats2","goodcamps","SaveResults","ml","q")))

#II - subset data
      sumstats2<-readRDS("sumstats2.rds")
      sumstats3<-sumstats2
      sumstats3<-sumstats3[sumstats3$N>25,]
      sumstats3<-sumstats3[sumstats3$missing<.2,]
      sumstats3<-sumstats3[sumstats3$ffjaccard>.15,]
      sumstats3<-sumstats3[sumstats3$d.ipvall>.15,]
      dataset2<-dataset[which(paste(sumstats2$Wave,sumstats2$Camp) %in% paste(sumstats3$Wave,sumstats3$Camp))]
      # if(q==1){
      #   datasetg <- sienaGroupCreate(dataset2[c(2,4,6:10)])  
      # }else{
      #   datasetg <- sienaGroupCreate(dataset2[11:19])  
      # }
      base.info<-data.frame(Camp=c(12,47,35,36,53,60),
                            Wave=c(1,1,3,3,3,3))
      dataset3<-dataset2[which(paste(sumstats3$Wave,sumstats3$Camp) %in% paste(base.info$Wave,base.info$Camp))]
      dataset4<-dataset2[which(!(paste(sumstats3$Wave,sumstats3$Camp) %in% paste(base.info$Wave,base.info$Camp)))]
      
#for each camp not included in base info, run a model of base info + that camp
      for(q in 1:length(dataset4)){
        datasetg <- sienaGroupCreate(c(dataset3,dataset4[q]))

# for(q in 1:4){
      # q=1
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
      myeff <- includeEffects(myeff, X, interaction1='closefriend')
      
      
    #DV  
      myeff <- includeInteraction(myeff, X, altX, interaction1 = c("bothmale","DV"),name="friendship")
      myeff <- includeInteraction(myeff, X, simX, interaction1 = c("bothmale","DV"),name="friendship")
      myeff <- includeInteraction(myeff, X, egoX, interaction1 = c("bothmale","DV"),name="friendship")
      myeff <- includeEffects(myeff, X, interaction1='bothmale')
    #behav
      myeff <- includeEffects(myeff, effFrom, interaction1 = "male", name = "DV")
      myeff <- includeEffects(myeff, effFrom, interaction1 = "sex.active", name = "DV")
      
      # myeff <- includeInteraction(myeff, effFrom, effFrom, interaction1=c('male','drnk_frq'), name='DV')
      # myeff <- includeInteraction(myeff, effFrom, effFrom, interaction1=c('male','sex.num'), name='DV')
      # myeff <- includeInteraction(myeff, effFrom, effFrom, interaction1=c('male','camp.duration'), name='DV')
      # myeff <- includeInteraction(myeff, effFrom, effFrom, interaction1=c('male','has.children'), name='DV')
      
      # myeff <- includeInteraction(myeff, effFrom, effFrom, interaction1=c('male','ever.ipv'), name='DV')
      # myeff <- includeInteraction(myeff, effFrom, avSim, interaction1=c('male','friendship'), name='DV')

      # if(q==1){
      # myeff <- includeEffects(myeff, avWAlt, interaction1='friendship', interaction2='bothmale', name='DV')
        myeff <- includeEffects(myeff, avSimW, interaction1='friendship', interaction2='bothmale', name='DV')
      # 
      #   myeff <- includeEffects(myeff, avWAlt, interaction1='friendship', interaction2='bothmale', interaction3='closefriend', name='DV')
      #   myeff <- includeEffects(myeff, avSimW, interaction1='friendship', interaction2='bothmale', interaction3='closefriend', name='DV')
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

}
      
  #after modeling both groups above, the results look similar except for the 3rd context (bad convergence) in q1 and the coefficient for same sex.
  #let's see if removing the 3rd camp results in more similar coefficients
      q=3
      datasetg <- sienaGroupCreate(dataset2[c(1:2,4:10)])  
  #No coefficients changed... What if we group camp 3 with q2?
      q=4
      datasetg <- sienaGroupCreate(dataset2[c(3,11:19)])  
  #This is even worse for this camp. Let's put everything together, then see if we can figure out how to deal with outliers.
      q=5
      datasetg <- sienaGroupCreate(dataset2)
  #What about everything but camp 3?
      q=6
      datasetg <- sienaGroupCreate(dataset2[c(1:2,4:19)]) #This is going to have to do  
    #group 3 and 5 are a bit outliers on most things; convergence for 15 isn't great. 
      
      Model$tconv
      Model$tmax
#vi - time test
    #manually determine which effects to test      
        Model$requestedEffects$functionName
        (timetest<-sienaTimeTest(Model,effects = c(58)))
    #limit data to just significant outliers
        a<-as.data.frame(timetest$IndividualTest)
        a<-a[a$`p-Value`<.05,]
        a$`Initial Est.`<-NULL
        a
        

        for(i in 31:40){
          (timetest<-sienaTimeTest(Model,effects = i))
          print(timetest$ToTest[4,][c(10,12,13)])
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

        