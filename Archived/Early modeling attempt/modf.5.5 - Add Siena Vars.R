#1 - load data to make siena objects
    varlist<-readRDS("varlist.rds")
    sumvars<-readRDS("sumvars.rds")
    #NEW STEP: remove objects that were removed in mod.2
        for(i in 1:length(varlist)){
          varlist[[i]][removedids]<-NULL
        }
        for(i in 1:length(sumvars)){
          sumvars[[i]][removedids]<-NULL
        }
    
    list2env(varlist,globalenv())
    list2env(sumvars,globalenv())
#2 - NEW STEP: Create siena object that identifies problem camps
    conv<-campn
    for(i in 1:length(campn)){
      conv[[i]][]<-sumstats4$noconv[i]
    }
#3 - Create RSiena objects
    dataset2<-list()
    for(i in 1:length(friendship)){ #camp 3 wave 1 generates warning
      dataset2[[i]] <- sienaDataCreate(friendship = friendship[[i]], ffriendship = ffriendship[[i]], gem = gem[[i]], sex = sex[[i]], age = age[[i]], edu = edu[[i]], married = married[[i]], ses = ses[[i]],campn=campn[[i]], ward1=ward1[[i]], ward2=ward2[[i]],ward3=ward3[[i]],ward4=ward4[[i]], condition=condition[[i]], leader=leader[[i]], sex.active=sex.active[[i]], sex.year=sex.year[[i]], child.violence=child.violence[[i]], child.sex.violence=child.sex.violence[[i]], camp.duration=camp.duration[[i]],wave=wave[[i]], has.children = has.children[[i]], fam.available = fam.available[[i]], hivtst.ego = hivtst.ego[[i]], hivinjf.ego = hivinjf.ego[[i]], hivadvf.ego = hivadvf.ego[[i]], knowf.ego = knowf.ego[[i]], closef.ego = closef.ego[[i]], dy.hivtstf = hivtstf[[i]], dy.hivinjf = hivinjf[[i]], dy.hivadvf = hivadvf[[i]], dy.knowf = knowf[[i]], dy.closef = closef[[i]], ever.test=ever.test[[i]],ever.ipv=ever.ipv[[i]],conv=conv[[i]])
    }
    datasetg <- sienaGroupCreate(dataset2)
#4 - create effects like usual
    source("modf.3 - Create effects for algorithm.R")
