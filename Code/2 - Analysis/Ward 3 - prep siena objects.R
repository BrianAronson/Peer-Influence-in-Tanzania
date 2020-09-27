#a - Create a binary sex number variable
        sex.num2<-sex.num
        for(i in 1:length(friendship)){
          sex.num2[[i]][] <- ifelse(sex.num2[[i]][] > 0, 1, 0)
        }
        
#b - Choose DV
    DV <- dv.testhiv_12

#c - Create wave dummies
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
    
#d - Fix condition variable
    for(i in 1:length(condition)){
      if(condition[[i]][1]==2){
        condition[[i]][]<-0
      }
      attr(condition[[i]],"centered")<-F
    }

#e - Create siena objects
    dataset<-list()
    for(i in 1:length(friendship)){ #camp 3 wave 1 generates warning
      suppressWarnings(dataset[[i]] <- sienaDataCreate(friendship = friendship[[i]], DV = DV[[i]],ffriendship = ffriendship[[i]], gem = gem[[i]], sex = sex[[i]], age = age[[i]], edu = edu[[i]], married = married[[i]], ses = ses[[i]],campn=campn[[i]], ward1=ward1[[i]], ward2=ward2[[i]],ward3=ward3[[i]],ward4=ward4[[i]], condition=condition[[i]], leader=leader[[i]], sex.active=sex.active[[i]], sex.year=sex.year[[i]], child.violence=child.violence[[i]], child.sex.violence=child.sex.violence[[i]], camp.duration=camp.duration[[i]],wave=wave[[i]], has.children = has.children[[i]], fam.available = fam.available[[i]], hivtst.ego = hivtst.ego[[i]], hivinjf.ego = hivinjf.ego[[i]], hivadvf.ego = hivadvf.ego[[i]], knowf.ego = knowf.ego[[i]], closef.ego = closef.ego[[i]], dy.hivtstf = hivtstf[[i]], dy.hivinjf = hivinjf[[i]], dy.hivadvf = hivadvf[[i]], dy.knowf = knowf[[i]], dy.closef = closef[[i]], ever.test=ever.test[[i]],ever.ipv=ever.ipv[[i]],alc_evr=alc_evr[[i]],alc_frq=alc_frq[[i]],alc_bing=alc_bing[[i]],drnk_frq=drnk_frq[[i]],hivtst.alter = hivtst.alter[[i]],hivinjf.alter = hivinjf.alter[[i]],hivadvf.alter = hivadvf.alter[[i]],hivtst.group = hivtst.group[[i]], sex.num=sex.num[[i]], ffipv=ffipv[[i]],ffgem=ffgem[[i]],ffalc=ffalc[[i]],ffgend=ffgend[[i]],ffnum=ffnum[[i]],ffhiv=ffhiv[[i]],clipv=clipv[[i]],clgem=clgem[[i]],clalc=clalc[[i]],clgend=clgend[[i]],clnum=clnum[[i]],clhiv=clhiv[[i]],bothmale=bothmale[[i]],sex.num2=sex.num2[[i]],male=male[[i]],wardwave1=wardwave1[[i]],wardwave2=wardwave2[[i]],wardwave3=wardwave3[[i]],wardwave4=wardwave4[[i]],wardwave5=wardwave5[[i]],wardwave6=wardwave6[[i]],wardwave7=wardwave7[[i]],wardwave8=wardwave8[[i]],pid=pid[[i]],wave1=wave1[[i]],wave2=wave2[[i]]))
    }
        
#f - Remove unecessary stuff from environment
    rm(list=setdiff(ls(), c("dataset","options","sumstats","sumstats2","sumstats3","sumstats4","goodcamps","SaveResults","ml","q", "data.dir", "raw.data.dir", "der.data.dir", "code.prep.dir", "code.analysis.dir", "code.figures.dir")))
    
#g - make final siena dataset
    datasetg <- sienaGroupCreate(dataset)
    
