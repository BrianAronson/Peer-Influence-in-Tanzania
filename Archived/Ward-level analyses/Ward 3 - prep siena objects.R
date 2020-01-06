#a - create sex num2
        sex.num2<-sex.num
        for(i in 1:length(friendship)){
          #Convert DV to binary
            # DV[[i]][]<-ifelse(DV[[i]][]>0,1,0)
          #create a different sexually active variable
          sex.num2[[i]][]<-ifelse(sex.num2[[i]][]>0,1,0)
        }
        
        #b - Choose DV
        options$DVname<-"testhiv_12" #gem_r15_avg ipvoutcome_cat alc_freq testhiv_12 all_ipv dv.ipvphysical_cat
    DV<-eval(parse(text=paste("dv.",options$DVname,sep="")))

    #include wave1 dummy
        wave1<-wave
        for(i in 1:length(wave1)){
            if(wave1[[i]][1]==3){
              wave1[[i]][]<-0
            }
            attr(wave1[[i]],"centered")<-F
        }
        wave2<-wave
        for(i in 1:length(wave2)){
          if(wave2[[i]][1]==3){
            wave2[[i]][]<-1
          }else{
            wave2[[i]][]<-0
          }
          attr(wave2[[i]],"centered")<-F
        }
        
    #convert condtion
        for(i in 1:length(condition)){
          if(condition[[i]][1]==2){
            condition[[i]][]<-0
          }
          attr(condition[[i]],"centered")<-F
        }
        
#c - create siena objects
    dataset<-list()
        for(i in 1:length(friendship)){ #camp 3 wave 1 generates warning
          suppressWarnings(dataset[[i]] <- sienaDataCreate( friendship = friendship[[i]], DV = DV[[i]],ffriendship = ffriendship[[i]], gem = gem[[i]], sex = sex[[i]], age = age[[i]], edu = edu[[i]], married = married[[i]], ses = ses[[i]],campn=campn[[i]], ward1=ward1[[i]], ward2=ward2[[i]],ward3=ward3[[i]],ward4=ward4[[i]], condition=condition[[i]], leader=leader[[i]], sex.active=sex.active[[i]], sex.year=sex.year[[i]], child.violence=child.violence[[i]], child.sex.violence=child.sex.violence[[i]], camp.duration=camp.duration[[i]],wave=wave[[i]], has.children = has.children[[i]], fam.available = fam.available[[i]], hivtst.ego = hivtst.ego[[i]], hivinjf.ego = hivinjf.ego[[i]], hivadvf.ego = hivadvf.ego[[i]], knowf.ego = knowf.ego[[i]], closef.ego = closef.ego[[i]], dy.hivtstf = hivtstf[[i]], dy.hivinjf = hivinjf[[i]], dy.hivadvf = hivadvf[[i]], dy.knowf = knowf[[i]], dy.closef = closef[[i]], ever.test=ever.test[[i]],ever.ipv=ever.ipv[[i]],alc_evr=alc_evr[[i]],alc_frq=alc_frq[[i]],alc_bing=alc_bing[[i]],drnk_frq=drnk_frq[[i]],hivtst.alter = hivtst.alter[[i]],hivinjf.alter = hivinjf.alter[[i]],hivadvf.alter = hivadvf.alter[[i]],hivtst.group = hivtst.group[[i]], sex.num=sex.num[[i]], ffipv=ffipv[[i]],ffgem=ffgem[[i]],ffalc=ffalc[[i]],ffgend=ffgend[[i]],ffnum=ffnum[[i]],ffhiv=ffhiv[[i]],clipv=clipv[[i]],clgem=clgem[[i]],clalc=clalc[[i]],clgend=clgend[[i]],clnum=clnum[[i]],clhiv=clhiv[[i]],bothmale=bothmale[[i]],sex.num2=sex.num2[[i]],male=male[[i]],wardwave1=wardwave1[[i]],wardwave2=wardwave2[[i]],wardwave3=wardwave3[[i]],wardwave4=wardwave4[[i]],wardwave5=wardwave5[[i]],wardwave6=wardwave6[[i]],wardwave7=wardwave7[[i]],wardwave8=wardwave8[[i]],pid=pid[[i]],wave1=wave1[[i]],wave2=wave2[[i]]))
        } #full friendship requires dv.ffriendship
        if(noDV==1){
          for(i in 1:length(friendship)){
              suppressWarnings(dataset[[i]] <- sienaDataCreate(friendship = friendship[[i]],ffriendship = ffriendship[[i]], gem = gem[[i]], sex = sex[[i]], age = age[[i]], edu = edu[[i]], married = married[[i]], ses = ses[[i]],campn=campn[[i]], ward1=ward1[[i]], ward2=ward2[[i]],ward3=ward3[[i]],ward4=ward4[[i]], condition=condition[[i]], leader=leader[[i]], sex.active=sex.active[[i]], sex.year=sex.year[[i]], child.violence=child.violence[[i]], child.sex.violence=child.sex.violence[[i]], camp.duration=camp.duration[[i]],wave=wave[[i]], has.children = has.children[[i]], fam.available = fam.available[[i]], hivtst.ego = hivtst.ego[[i]], hivinjf.ego = hivinjf.ego[[i]], hivadvf.ego = hivadvf.ego[[i]], knowf.ego = knowf.ego[[i]], closef.ego = closef.ego[[i]], dy.hivtstf = hivtstf[[i]], dy.hivinjf = hivinjf[[i]], dy.hivadvf = hivadvf[[i]], dy.knowf = knowf[[i]], dy.closef = closef[[i]], ever.test=ever.test[[i]],ever.ipv=ever.ipv[[i]],alc_evr=alc_evr[[i]],alc_frq=alc_frq[[i]],alc_bing=alc_bing[[i]],drnk_frq=drnk_frq[[i]],hivtst.alter = hivtst.alter[[i]],hivinjf.alter = hivinjf.alter[[i]],hivadvf.alter = hivadvf.alter[[i]],hivtst.group = hivtst.group[[i]], sex.num=sex.num[[i]], ffipv=ffipv[[i]],ffgem=ffgem[[i]],ffalc=ffalc[[i]],ffgend=ffgend[[i]],ffnum=ffnum[[i]],ffhiv=ffhiv[[i]],clipv=clipv[[i]],clgem=clgem[[i]],clalc=clalc[[i]],clgend=clgend[[i]],clnum=clnum[[i]],clhiv=clhiv[[i]],bothmale=bothmale[[i]],sex.num2=sex.num2[[i]],male=male[[i]],wardwave1=wardwave1[[i]],wardwave2=wardwave2[[i]],wardwave3=wardwave3[[i]],wardwave4=wardwave4[[i]],wardwave5=wardwave5[[i]],wardwave6=wardwave6[[i]],wardwave7=wardwave7[[i]],wardwave8=wardwave8[[i]],pid=pid[[i]],wave1=wave1[[i]],wave2=wave2[[i]]))          
          }
        }
    
    #d - Remove unecessary stuff from environment
    rm(list=setdiff(ls(), c("dataset","options","sumstats","sumstats2","sumstats3","sumstats4","goodcamps","SaveResults","ml","q")))
    
    #e - Create print function
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
      html<-getwd()
      html<-gsub("Code","/Results/Results html/Model ",html)
      htmlfilename<-paste(html,number,name,".html",sep="")
      print(xtable(htmldf), type="html", file=htmlfilename)
      browseURL(htmlfilename)
      return(htmlfilename)
    }

#f - make final siena dataset
    datasetg <- sienaGroupCreate(dataset)
    
#g - prep effects
    m0<-NA;m1<-NA;m2<-NA;m3<-NA;m4<-NA;m5<-NA;m6<-NA;m7<-NA;m8<-NA;m9<-NA;m10<-NA;m11<-NA;m12<-NA;m13<-NA;m14<-NA;m15<-NA;m16<-NA;m17<-NA;m18<-NA;m19<-NA;m20<-NA
    