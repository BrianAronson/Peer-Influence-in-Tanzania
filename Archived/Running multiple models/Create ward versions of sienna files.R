source("prep.1 - Load libraries.R")
#1 - load data
    sumstats<-readRDS("sumstats.rds")
    sumstats2<-readRDS("sumstats2.rds")
    varlist<-readRDS("varlist.rds")
    list2env(varlist,globalenv())
    sumvars<-readRDS("sumvars.rds")
    list2env(sumvars,globalenv())

#1.5 - kill camps you don't like
    #identify camps
        sumstats2<-readRDS("sumstats2.rds")
        sumstats3<-sumstats2
        sumstats3<-sumstats3[sumstats3$N>15,]
        sumstats3<-sumstats3[sumstats3$missing<.2,]
        sumstats3<-sumstats3[sumstats3$ffjaccard>.15,]
        sumstats3<-sumstats3[sumstats3$d.hiv>.1,]
        # sumstats3<-sumstats3[sumstats3$Wave==3,]
        sumstats3<-sumstats3[sumstats3$Camp!=15,]  #Camp 15 is problematic
        kill<-paste(sumstats2$Camp,sumstats2$Wave) %in% paste(sumstats3$Camp,sumstats3$Wave)
              
    #kill them
        a<-c(varlist,sumvars)
        for(i in 1:length(a)){
          a[[i]]<-a[[i]][kill]
        }
        list2env(a,globalenv())

#2 - identify ward and time of each camp
    ward<-1:length(dv.ipvoutcome_cat)
    tward1<-sapply(ward1, function(x) x[1])
    tward2<-sapply(ward2, function(x) x[1])
    tward3<-sapply(ward3, function(x) x[1])
    tward4<-sapply(ward4, function(x) x[1])
    ward[tward1==1]<-1
    ward[tward2==1]<-2
    ward[tward3==1]<-3
    ward[tward4==1]<-4
  # two waves?
    # len<-length(ward)
    # ward[(len/2+1):len]<-ward[(len/2+1):len]+4
    
#3 - sienna dependent variables:
    combine.depvar<-function(x){
        x1<-x
        x2<-split(x1,ward) #split by ward
        x3<-lapply(x2,function(y){#within each ward
          y1<-y
          y2<-sapply(y1, function(x) x[,,])
          y3<-do.call(rbind,y2)
          y4<-sienaDependent(y3, type ="behavior",allowOnly = F)
        })
        return(x3)
    }
    dv.ipvoutcome_cat<-combine.depvar(dv.ipvoutcome_cat)
    dv.ipvoutcome_bin<-combine.depvar(dv.ipvoutcome_bin)
    dv.gem_r15_avg<-combine.depvar(dv.gem_r15_avg)
    dv.ipvphysical_cat<-combine.depvar(dv.ipvphysical_cat)
    dv.ipvpsych_cat<-combine.depvar(dv.ipvpsych_cat)
    dv.testhiv_12<-combine.depvar(dv.testhiv_12)
    dv.all_ipv<-combine.depvar(dv.all_ipv)
    dv.alc_freq<-combine.depvar(dv.alc_freq)

#4 - siena network variables
    combine.netvar<-function(x,AO=T){
        x1<-x
        x2<-split(x1,ward) #split by ward
    #within each ward:
        x3<-sapply(x2,function(x){
        #for each camp, break down siena objects into arrays
            x4<-sapply(x, function(x) x[,,])
        #create an empty array to fit all camps
            dims<-sapply(x4, function(x) dim(x)[1])
            rows<-sum(dims)
            tarray<-array(10,dim=c(rows,rows,2))
        #for each camp, put data in array
            #identify rows to input data into
                stops=cumsum(dims)
                starts<-stops+1
                starts<-c(1,starts[-length(starts)])
                df<-data.frame(start=starts,stop=stops)
            for(i in 1:length(x4)){
              a<-x4[[i]][,,1]
              b<-x4[[i]][,,2]
              tarray[df$start[i]:df$stop[i],df$start[i]:df$stop[i],1]<-a
              tarray[df$start[i]:df$stop[i],df$start[i]:df$stop[i],2]<-b
            }

            return(sienaNet(tarray,allowOnly = AO))
          })
          return(x3)
        }
    friendship<-combine.netvar(friendship)
    dv.ffriendship<-combine.netvar(dv.ffriendship,AO = F)
    
#5 - siena coDyadCovars
    combine.coDyadCovars<-function(x,CT=T){
        x1<-x
        x2<-split(x1,ward) #split by ward
    #within each ward:
        x3<-sapply(x2,function(x){
        #for each camp, break down siena objects into arrays
            x4<-sapply(x, function(x) x[,])
        #create an empty array to fit all camps
            dims<-sapply(x4, function(x) dim(x)[1])
            rows<-sum(dims)
            tarray<-matrix(10,nrow=rows,ncol=rows)
        #for each camp, put data in array
            #identify rows to input data into
                stops=cumsum(dims)
                starts<-stops+1
                starts<-c(1,starts[-length(starts)])
                df<-data.frame(start=starts,stop=stops)
            for(i in 1:length(x4)){
              a<-x4[[i]]
              tarray[df$start[i]:df$stop[i],df$start[i]:df$stop[i]]<-a
            }
            return(coDyadCovar(tarray,centered = CT))
          })
          return(x3)
    }
    bothmale<-combine.coDyadCovars(bothmale,CT=F)
    ffriendship<-combine.coDyadCovars(ffriendship)        
    hivtstf<-combine.coDyadCovars(hivtstf)        
    hivinjf<-combine.coDyadCovars(hivinjf)        
    hivadvf<-combine.coDyadCovars(hivadvf)        
    knowf<-combine.coDyadCovars(knowf)        
    closef<-combine.coDyadCovars(closef)        
    
    x3<-x2[[1]]
    
    
#6 - sienna covars:
    combine.covvar<-function(x,CT=T){
        x1<-x
        x2<-split(x1,ward) #split by ward
        x3<-lapply(x2,function(y){#within each ward
          y3<-unlist(sapply(y,function(x) c(x)))
          y4<-coCovar(y3, centered=CT)
        })
        return(x3)
    }
    sex<-combine.covvar(sex)
    age<-combine.covvar(age)
    edu<-combine.covvar(edu)
    married<-combine.covvar(married)
    ses<-combine.covvar(ses)
    campn<-combine.covvar(campn)
    ward1<-combine.covvar(ward1)
    ward2<-combine.covvar(ward2)
    ward3<-combine.covvar(ward3)
    ward4<-combine.covvar(ward4)
    condition<-combine.covvar(condition)
    leader<-combine.covvar(leader)
    gem<-combine.covvar(gem)
    wave<-combine.covvar(wave)
    sex.active<-combine.covvar(sex.active)
    sex.year<-combine.covvar(sex.year)
    child.violence<-combine.covvar(child.violence)
    child.sex.violence<-combine.covvar(child.sex.violence)
    camp.duration<-combine.covvar(camp.duration)
    has.children<-combine.covvar(has.children)
    fam.available<-combine.covvar(fam.available)
    hivtst.ego<-combine.covvar(hivtst.ego)
    hivinjf.ego<-combine.covvar(hivinjf.ego)
    hivadvf.ego<-combine.covvar(hivadvf.ego)
    knowf.ego<-combine.covvar(knowf.ego)
    closef.ego<-combine.covvar(closef.ego)
    ever.test<-combine.covvar(ever.test)
    ever.ipv<-combine.covvar(ever.ipv)
    alc_evr<-combine.covvar(alc_evr)
    alc_frq<-combine.covvar(alc_frq)
    drnk_frq<-combine.covvar(drnk_frq)
    alc_bing<-combine.covvar(alc_bing)
    hivtst.alter<-combine.covvar(hivtst.alter)
    hivinjf.alter<-combine.covvar(hivinjf.alter)
    hivadvf.alter<-combine.covvar(hivadvf.alter)
    hivtst.group<-combine.covvar(hivtst.group)
    sex.num<-combine.covvar(sex.num)
    ffipv<-combine.covvar(ffipv)
    ffgem<-combine.covvar(ffgem)
    ffalc<-combine.covvar(ffalc)
    ffgend<-combine.covvar(ffgend)
    ffnum<-combine.covvar(ffnum)
    ffhiv<-combine.covvar(ffhiv)
    clipv<-combine.covvar(clipv)
    clgem<-combine.covvar(clgem)
    clalc<-combine.covvar(clalc)
    clgend<-combine.covvar(clgend)
    clnum<-combine.covvar(clnum)
    clhiv<-combine.covvar(clhiv)
    male<-combine.covvar(male)
    
#7 - save
    varlist<-list(dv.all_ipv, dv.ipvoutcome_cat,dv.ipvoutcome_bin,dv.gem_r15_avg,dv.ipvphysical_cat,dv.testhiv_12,friendship,ffriendship,hivtstf,hivinjf,hivadvf,knowf,closef,sex,age,edu,married,ses,campn,ward1,ward2,ward3,ward4,condition,leader,gem,wave,sex.active,sex.year,child.violence,child.sex.violence,camp.duration,has.children,fam.available,hivtst.ego,hivinjf.ego,hivadvf.ego,knowf.ego,closef.ego,ever.test,ever.ipv,dv.ipvpsych_cat,alc_evr,alc_frq,alc_bing,drnk_frq,hivtst.alter,hivinjf.alter,hivadvf.alter,hivtst.group,sex.num,dv.alc_freq,ffipv,ffgem,ffalc,ffgend,ffnum,ffhiv,clipv,clgem,clalc,clgend,clnum,clhiv,dv.ffriendship,bothmale,male)
    names(varlist)<-c("dv.all_ipv","dv.ipvoutcome_cat","dv.ipvoutcome_bin","dv.gem_r15_avg","dv.ipvphysical_cat","dv.testhiv_12","friendship","ffriendship","hivtstf","hivinjf","hivadvf","knowf","closef","sex","age","edu","married","ses","campn","ward1","ward2","ward3","ward4","condition","leader","gem","wave","sex.active","sex.year","child.violence","child.sex.violence","camp.duration","has.children","fam.available","hivtst.ego","hivinjf.ego","hivadvf.ego","knowf.ego","closef.ego","ever.test","ever.ipv","dv.ipvpsych_cat","alc_evr","alc_frq","alc_bing","drnk_frq","hivtst.alter","hivinjf.alter","hivadvf.alter","hivtst.group","sex.num","dv.alc_freq","ffipv","ffgem","ffalc","ffgend","ffnum","ffhiv","clipv","clgem","clalc","clgend","clnum","clhiv","dv.ffriendship","bothmale","male")
    saveRDS(varlist,"varlist2.rds")
    

        
    #2.5 - Temporary steps
            sex.num2<-sex.num
            for(i in 1:length(friendship)){
              #Convert DV to binary
                # DV[[i]][]<-ifelse(DV[[i]][]>0,1,0)
              #create a different sexually active variable
                sex.num2[[i]][]<-ifelse(sex.num2[[i]][]>0,1,0)
            }

    #2 - Choose DV
    options$DVname<-"testhiv_12" #gem_r15_avg ipvoutcome_cat alc_freq testhiv_12 all_ipv dv.ipvphysical_cat
    DV<-eval(parse(text=paste("dv.",options$DVname,sep="")))
    
    dataset<-list()
    #Camp level variables are broken for ward level DVs
        for(i in 1:length(friendship)){ #camp 3 wave 1 generates warning
          suppressWarnings(dataset[[i]] <- sienaDataCreate( friendship = friendship[[i]], DV = DV[[i]],ffriendship = ffriendship[[i]], gem = gem[[i]], sex = sex[[i]], age = age[[i]], edu = edu[[i]], married = married[[i]], ses = ses[[i]],campn=campn[[i]], ward1=ward1[[i]], ward2=ward2[[i]],ward3=ward3[[i]],ward4=ward4[[i]], condition=condition[[i]], leader=leader[[i]], sex.active=sex.active[[i]], sex.year=sex.year[[i]], child.violence=child.violence[[i]], child.sex.violence=child.sex.violence[[i]], camp.duration=camp.duration[[i]],wave=wave[[i]], has.children = has.children[[i]], fam.available = fam.available[[i]], hivtst.ego = hivtst.ego[[i]], hivinjf.ego = hivinjf.ego[[i]], hivadvf.ego = hivadvf.ego[[i]], knowf.ego = knowf.ego[[i]], closef.ego = closef.ego[[i]], dy.hivtstf = hivtstf[[i]], dy.hivinjf = hivinjf[[i]], dy.hivadvf = hivadvf[[i]], dy.knowf = knowf[[i]], dy.closef = closef[[i]], ever.test=ever.test[[i]],ever.ipv=ever.ipv[[i]],alc_evr=alc_evr[[i]],alc_frq=alc_frq[[i]],alc_bing=alc_bing[[i]],drnk_frq=drnk_frq[[i]],hivtst.alter = hivtst.alter[[i]],hivinjf.alter = hivinjf.alter[[i]],hivadvf.alter = hivadvf.alter[[i]],hivtst.group = hivtst.group[[i]], sex.num=sex.num[[i]], ffipv=ffipv[[i]],ffgem=ffgem[[i]],ffalc=ffalc[[i]],ffgend=ffgend[[i]],ffnum=ffnum[[i]],ffhiv=ffhiv[[i]],clipv=clipv[[i]],clgem=clgem[[i]],clalc=clalc[[i]],clgend=clgend[[i]],clnum=clnum[[i]],clhiv=clhiv[[i]],bothmale=bothmale[[i]],sex.num2=sex.num2[[i]],male=male[[i]]))
        }
    
    #4 - Remove unecessary stuff from environment
        rm(list=setdiff(ls(), c("dataset","options","sumstats","sumstats2","goodcamps","SaveResults","ml","q")))
    
    #5 - Create print function
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
    
    datasetg <- sienaGroupCreate(dataset)
    
    
    
    #When friendship = full friends
        myeff <- getEffects(datasetg,behNintn =10)
        myeff <- includeEffects(myeff, inPopSqrt)
        myeff <- includeEffects(myeff, simX, interaction1 = "age")
        myeff <- includeEffects(myeff, sameX, interaction1 = "sex" )
        myeff <- includeEffects(myeff, Jout)
        myeff <- includeEffects(myeff, outPopSqrt)
        myeff <- includeInteraction(myeff, density, egoX, interaction1 = c('','wave'),name="friendship")
        myeff <- includeEffects(myeff, altX,simX,egoX, interaction1='DV',name='friendship')
        # myalgorithm <- sienaAlgorithmCreate( projname = 'SingleGroups', useStdInits = FALSE,
        #                                      nsub=4, n3=2000,cond = F, firstg=.05, doubleAveraging=1,MaxDegree = c(friendship=8)) #,
    
    #TEMPORARY
        myeff <- includeEffects(myeff, X, interaction1='ffriendship')
        myeff <- includeEffects(myeff, egoX,interaction1='ward2')
        myeff <- includeEffects(myeff, egoX,interaction1='ward4')
        myalgorithm <- sienaAlgorithmCreate( projname = 'SingleGroups', useStdInits = FALSE,
                                         nsub=4, n3=2000,cond = F, firstg=.05, doubleAveraging=1,MaxDegree = c(friendship=3)) #,
    
    
    #models
        m0<-NA;m1<-NA;m2<-NA;m3<-NA;m4<-NA;m5<-NA;m6<-NA;m7<-NA;m8<-NA;m9<-NA;m10<-NA;m11<-NA;m12<-NA;m13<-NA;m14<-NA;m15<-NA;m16<-NA;m17<-NA;m18<-NA;m19<-NA;m20<-NA
        m0 <- myeff
        m0 <- includeEffects(m0, avSim, interaction1 = c('friendship'),name="DV") #no
        
        
        lms<-list(m0,m1,m2,m3,m4,m5,m6,m7,m8,m9,m10);kill<-sapply(lms,function(x) class(x[[1]][1]))=="character"; lms<-lms[kill];ml<-list()
        baseMod <- siena07(myalgorithm, data = datasetg, effects = lms[[1]], nbrNodes=24, useCluster=T, initC=T, returnDeps=TRUE, batch=T, verbose = F)
        Model<-baseMod
        printfun(Model)
        