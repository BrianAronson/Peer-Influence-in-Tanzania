#1 - load data
    sumstats<-readRDS("sumstats.rds")
    sumstats2<-readRDS("sumstats2.rds")
    varlist<-readRDS("varlist.rds")
    sumvars<-readRDS("sumvars.rds")
    list2env(varlist,globalenv())
    list2env(sumvars,globalenv())
    
#2 - Create RSiena objects
    dataset<-list()
    suppressWarnings({
      for(i in 1:length(friendship)){ #camp 3 wave 1 generates warning 
        dataset[[i]] <- sienaDataCreate(friendship = friendship[[i]], ffriendship = ffriendship[[i]], gem = gem[[i]], sex = sex[[i]], age = age[[i]], edu = edu[[i]], married = married[[i]], ses = ses[[i]],campn=campn[[i]], ward1=ward1[[i]], ward2=ward2[[i]],ward3=ward3[[i]],ward4=ward4[[i]], condition=condition[[i]], leader=leader[[i]], sex.active=sex.active[[i]], sex.year=sex.year[[i]], child.violence=child.violence[[i]], child.sex.violence=child.sex.violence[[i]], camp.duration=camp.duration[[i]],wave=wave[[i]], has.children = has.children[[i]], fam.available = fam.available[[i]], hivtst.ego = hivtst.ego[[i]], hivinjf.ego = hivinjf.ego[[i]], hivadvf.ego = hivadvf.ego[[i]], knowf.ego = knowf.ego[[i]], closef.ego = closef.ego[[i]], dy.hivtstf = hivtstf[[i]], dy.hivinjf = hivinjf[[i]], dy.hivadvf = hivadvf[[i]], dy.knowf = knowf[[i]], dy.closef = closef[[i]], ever.test=ever.test[[i]],ever.ipv=ever.ipv[[i]], alc_evr=alc_evr[[i]],alc_frq=alc_frq[[i]],alc_bing=alc_bing[[i]],drnk_frq=drnk_frq[[i]],hivtst.alter=hivtst.alter[[i]],hivinjf.alter=hivinjf.alter[[i]],hivadvf.alter=hivadvf.alter[[i]],hivtst.group=hivtst.group[[i]])
      }
    })  
#3 - Remove unecessary stuff from environment
    rm(list=setdiff(ls(), c("dataset","options","sumstats","sumstats2")))
    

    
    # alc_evr=alc_evr[[i]],
    # alc_frq=alc_frq[[i]],
    # alc_bing=alc_bing[[i]],
    # drnk_frq=drnk_frq[[i]],
    # hivtst.alter=hivtst.alter[[i]],
    # hivinjf.alter=hivinjf.alter[[i]],
    # hivadvf.alter=hivadvf.alter[[i]],
    # hivtst.group=hivtst.group[[i]]