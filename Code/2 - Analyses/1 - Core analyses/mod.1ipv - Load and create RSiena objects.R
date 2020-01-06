#1 - load data
    sumstats<-readRDS("sumstats.rds")
    sumstats2<-readRDS("sumstats2.rds")
    varlist<-readRDS("varlist.rds")
    list2env(varlist,globalenv())
    sumvars<-readRDS("sumvars.rds")
    list2env(sumvars,globalenv())
    goodcamps<-readRDS("goodcamps.rds")

#2 - Choose DV
    DV<-eval(parse(text=paste("dv.",options$DVname,sep="")))
    
#3 - Create RSiena objects
    if(options$campdums==F){
      dataset<-list()
      for(i in 1:length(friendship)){ #camp 3 wave 1 generates warning 
        # suppressWarnings(dataset[[i]] <- sienaDataCreate(friendship = friendship[[i]], ffriendship = ffriendship[[i]], DV = DV[[i]], gem = gem[[i]], sex = sex[[i]], age = age[[i]], edu = edu[[i]], married = married[[i]], ses = ses[[i]],campn=campn[[i]], ward1=ward1[[i]], ward2=ward2[[i]],ward3=ward3[[i]],ward4=ward4[[i]], condition=condition[[i]], leader=leader[[i]], sex.active=sex.active[[i]], sex.year=sex.year[[i]], child.violence=child.violence[[i]], child.sex.violence=child.sex.violence[[i]], camp.duration=camp.duration[[i]],wave=wave[[i]], has.children = has.children[[i]], fam.available = fam.available[[i]], hivtst.ego = hivtst.ego[[i]], hivinjf.ego = hivinjf.ego[[i]], hivadvf.ego = hivadvf.ego[[i]], knowf.ego = knowf.ego[[i]], closef.ego = closef.ego[[i]], dy.hivtstf = hivtstf[[i]], dy.hivinjf = hivinjf[[i]], dy.hivadvf = hivadvf[[i]], dy.knowf = knowf[[i]], dy.closef = closef[[i]], ever.test=ever.test[[i]],ever.ipv=ever.ipv[[i]], N1=N1[[i]],jaccard=jaccard[[i]],d.ipv=d.ipv[[i]],d.hiv=d.hiv[[i]],r.hiv=r.hiv[[i]],density=density[[i]],r.hivtst=r.hivtst[[i]],r.hivinj=r.hivinj[[i]],r.hivadv=r.hivadv[[i]],alc_evr=alc_evr[[i]],alc_frq=alc_frq[[i]],alc_bing=alc_bing[[i]],drnk_frq=drnk_frq[[i]],hivtst.alter = hivtst.alter[[i]],hivinjf.alter = hivinjf.alter[[i]],hivadvf.alter = hivadvf.alter[[i]],hivtst.group = hivtst.group[[i]], sex.num=sex.num[[i]], ffipv=ffipv[[i]],ffgem=ffgem[[i]],ffalc=ffalc[[i]],ffgend=ffgend[[i]],ffnum=ffnum[[i]],ffhiv=ffhiv[[i]],clipv=clipv[[i]],clgem=clgem[[i]],clalc=clalc[[i]],clgend=clgend[[i]],clnum=clnum[[i]],clhiv=clhiv[[i]],r.ipv=r.ipv[[i]],r.gem=r.gem[[i]],r.alc=r.alc[[i]],r.gend=r.gend[[i]]))
        suppressWarnings(dataset[[i]] <- sienaDataCreate(DV = DV[[i]], friendship = dv.ffriendship[[i]], ffriendship = ffriendship[[i]], gem = gem[[i]], sex = sex[[i]], age = age[[i]], edu = edu[[i]], married = married[[i]], ses = ses[[i]],campn=campn[[i]], ward1=ward1[[i]], ward2=ward2[[i]],ward3=ward3[[i]],ward4=ward4[[i]], condition=condition[[i]], leader=leader[[i]], sex.active=sex.active[[i]], sex.year=sex.year[[i]], child.violence=child.violence[[i]], child.sex.violence=child.sex.violence[[i]], camp.duration=camp.duration[[i]],wave=wave[[i]], has.children = has.children[[i]], fam.available = fam.available[[i]], hivtst.ego = hivtst.ego[[i]], hivinjf.ego = hivinjf.ego[[i]], hivadvf.ego = hivadvf.ego[[i]], knowf.ego = knowf.ego[[i]], closef.ego = closef.ego[[i]], dy.hivtstf = hivtstf[[i]], dy.hivinjf = hivinjf[[i]], dy.hivadvf = hivadvf[[i]], dy.knowf = knowf[[i]], dy.closef = closef[[i]], ever.test=ever.test[[i]],ever.ipv=ever.ipv[[i]], N1=N1[[i]],jaccard=jaccard[[i]],d.ipv=d.ipv[[i]],d.hiv=d.hiv[[i]],r.hiv=r.hiv[[i]],density=density[[i]],r.hivtst=r.hivtst[[i]],r.hivinj=r.hivinj[[i]],r.hivadv=r.hivadv[[i]],alc_evr=alc_evr[[i]],alc_frq=alc_frq[[i]],alc_bing=alc_bing[[i]],drnk_frq=drnk_frq[[i]],hivtst.alter = hivtst.alter[[i]],hivinjf.alter = hivinjf.alter[[i]],hivadvf.alter = hivadvf.alter[[i]],hivtst.group = hivtst.group[[i]], sex.num=sex.num[[i]], ffipv=ffipv[[i]],ffgem=ffgem[[i]],ffalc=ffalc[[i]],ffgend=ffgend[[i]],ffnum=ffnum[[i]],ffhiv=ffhiv[[i]],clipv=clipv[[i]],clgem=clgem[[i]],clalc=clalc[[i]],clgend=clgend[[i]],clnum=clnum[[i]],clhiv=clhiv[[i]],r.ipv=r.ipv[[i]],r.gem=r.gem[[i]],r.alc=r.alc[[i]],r.gend=r.gend[[i]],bothmale=bothmale[[i]],male=male[[i]]))
      }
      
    }else{
      camplist<-readRDS("camplist.rds")
      dataset<-list()
      for(i in 1:length(friendship)){
          # dataset[[i]] <- sienaDataCreate(friendship = friendship[[i]], ffriendship = ffriendship[[i]], DV = DV[[i]], gem = gem[[i]], sex = sex[[i]], age = age[[i]], edu = edu[[i]], married = married[[i]], ses = ses[[i]],campn=campn[[i]], ward1=ward1[[i]], ward2=ward2[[i]],ward3=ward3[[i]],ward4=ward4[[i]], condition=condition[[i]], leader=leader[[i]], sex.active=sex.active[[i]], sex.year=sex.year[[i]], child.violence=child.violence[[i]], child.sex.violence=child.sex.violence[[i]], camp.duration=camp.duration[[i]],wave=wave[[i]], has.children = has.children[[i]], fam.available = fam.available[[i]], hivtst.ego = hivtst.ego[[i]], hivinjf.ego = hivinjf.ego[[i]], hivadvf.ego = hivadvf.ego[[i]], knowf.ego = knowf.ego[[i]], closef.ego = closef.ego[[i]], dy.hivtstf = hivtstf[[i]], dy.hivinjf = hivinjf[[i]], dy.hivadvf = hivadvf[[i]], dy.knowf = knowf[[i]], dy.closef = closef[[i]], ever.test=ever.test[[i]],ever.ipv=ever.ipv[[i]],N1=N1[[i]],jaccard=jaccard[[i]],missing=missing[[i]],d.ipv=d.ipv[[i]],d.hiv=d.hiv[[i]], alc_evr=alc_evr[[i]],alc_frq=alc_frq[[i]],alc_bing=alc_bing[[i]],drnk_frq=drnk_frq[[i]],
          #                 camp11=camplist[[1]][[i]],camp21=camplist[[2]][[i]],camp31=camplist[[3]][[i]],camp41=camplist[[4]][[i]],camp51=camplist[[5]][[i]],camp61=camplist[[6]][[i]],camp71=camplist[[7]][[i]],camp81=camplist[[8]][[i]],camp91=camplist[[9]][[i]],camp101=camplist[[10]][[i]],camp111=camplist[[11]][[i]],camp121=camplist[[12]][[i]],camp131=camplist[[13]][[i]],camp141=camplist[[14]][[i]],camp151=camplist[[15]][[i]],camp161=camplist[[16]][[i]],camp171=camplist[[17]][[i]],camp181=camplist[[18]][[i]],camp191=camplist[[19]][[i]],camp201=camplist[[20]][[i]],camp211=camplist[[21]][[i]],camp221=camplist[[22]][[i]],camp231=camplist[[23]][[i]],camp241=camplist[[24]][[i]],camp251=camplist[[25]][[i]],camp261=camplist[[26]][[i]],camp271=camplist[[27]][[i]],camp281=camplist[[28]][[i]],camp291=camplist[[29]][[i]],camp301=camplist[[30]][[i]],camp311=camplist[[31]][[i]],camp321=camplist[[32]][[i]],camp331=camplist[[33]][[i]],camp341=camplist[[34]][[i]],camp351=camplist[[35]][[i]],camp361=camplist[[36]][[i]],camp371=camplist[[37]][[i]],camp381=camplist[[38]][[i]],camp391=camplist[[39]][[i]],camp401=camplist[[40]][[i]],camp411=camplist[[41]][[i]],camp421=camplist[[42]][[i]],camp431=camplist[[43]][[i]],camp441=camplist[[44]][[i]],camp451=camplist[[45]][[i]],camp461=camplist[[46]][[i]],camp471=camplist[[47]][[i]],camp481=camplist[[48]][[i]],camp491=camplist[[49]][[i]],camp501=camplist[[50]][[i]],camp511=camplist[[51]][[i]],camp521=camplist[[52]][[i]],camp531=camplist[[53]][[i]],camp541=camplist[[54]][[i]],camp551=camplist[[55]][[i]],camp561=camplist[[56]][[i]],camp571=camplist[[57]][[i]],camp581=camplist[[58]][[i]],camp591=camplist[[59]][[i]],camp13=camplist[[60]][[i]],camp23=camplist[[61]][[i]],camp33=camplist[[62]][[i]],camp43=camplist[[63]][[i]],camp53=camplist[[64]][[i]],camp63=camplist[[65]][[i]],camp73=camplist[[66]][[i]],camp83=camplist[[67]][[i]],camp93=camplist[[68]][[i]],camp103=camplist[[69]][[i]],camp113=camplist[[70]][[i]],camp123=camplist[[71]][[i]],camp133=camplist[[72]][[i]],camp143=camplist[[73]][[i]],camp153=camplist[[74]][[i]],camp163=camplist[[75]][[i]],camp173=camplist[[76]][[i]],camp183=camplist[[77]][[i]],camp193=camplist[[78]][[i]],camp203=camplist[[79]][[i]],camp213=camplist[[80]][[i]],camp223=camplist[[81]][[i]],camp233=camplist[[82]][[i]],camp243=camplist[[83]][[i]],camp253=camplist[[84]][[i]],camp263=camplist[[85]][[i]],camp273=camplist[[86]][[i]],camp283=camplist[[87]][[i]],camp293=camplist[[88]][[i]],camp303=camplist[[89]][[i]],camp313=camplist[[90]][[i]],camp323=camplist[[91]][[i]],camp333=camplist[[92]][[i]],camp343=camplist[[93]][[i]],camp353=camplist[[94]][[i]],camp363=camplist[[95]][[i]],camp373=camplist[[96]][[i]],camp383=camplist[[97]][[i]],camp393=camplist[[98]][[i]],camp403=camplist[[99]][[i]],camp413=camplist[[100]][[i]],camp423=camplist[[101]][[i]],camp433=camplist[[102]][[i]],camp443=camplist[[103]][[i]],camp453=camplist[[104]][[i]],camp463=camplist[[105]][[i]],camp473=camplist[[106]][[i]],camp483=camplist[[107]][[i]],camp493=camplist[[108]][[i]],camp503=camplist[[109]][[i]],camp513=camplist[[110]][[i]],camp523=camplist[[111]][[i]],camp533=camplist[[112]][[i]],camp543=camplist[[113]][[i]],camp553=camplist[[114]][[i]],camp563=camplist[[115]][[i]],camp573=camplist[[116]][[i]],camp583=camplist[[117]][[i]],camp593=camplist[[118]][[i]])
        suppressWarnings(dataset[[i]] <- sienaDataCreate(friendship = dv.ffriendship[[i]], ffriendship = ffriendship[[i]], gem = gem[[i]], sex = sex[[i]], age = age[[i]], edu = edu[[i]], married = married[[i]], ses = ses[[i]],campn=campn[[i]], ward1=ward1[[i]], ward2=ward2[[i]],ward3=ward3[[i]],ward4=ward4[[i]], condition=condition[[i]], leader=leader[[i]], sex.active=sex.active[[i]], sex.year=sex.year[[i]], child.violence=child.violence[[i]], child.sex.violence=child.sex.violence[[i]], camp.duration=camp.duration[[i]],wave=wave[[i]], has.children = has.children[[i]], fam.available = fam.available[[i]], hivtst.ego = hivtst.ego[[i]], hivinjf.ego = hivinjf.ego[[i]], hivadvf.ego = hivadvf.ego[[i]], knowf.ego = knowf.ego[[i]], closef.ego = closef.ego[[i]], dy.hivtstf = hivtstf[[i]], dy.hivinjf = hivinjf[[i]], dy.hivadvf = hivadvf[[i]], dy.knowf = knowf[[i]], dy.closef = closef[[i]], ever.test=ever.test[[i]],ever.ipv=ever.ipv[[i]], N1=N1[[i]],jaccard=jaccard[[i]],d.ipv=d.ipv[[i]],d.hiv=d.hiv[[i]],r.hiv=r.hiv[[i]],density=density[[i]],r.hivtst=r.hivtst[[i]],r.hivinj=r.hivinj[[i]],r.hivadv=r.hivadv[[i]],alc_evr=alc_evr[[i]],alc_frq=alc_frq[[i]],alc_bing=alc_bing[[i]],drnk_frq=drnk_frq[[i]],hivtst.alter = hivtst.alter[[i]],hivinjf.alter = hivinjf.alter[[i]],hivadvf.alter = hivadvf.alter[[i]],hivtst.group = hivtst.group[[i]], sex.num=sex.num[[i]], ffipv=ffipv[[i]],ffgem=ffgem[[i]],ffalc=ffalc[[i]],ffgend=ffgend[[i]],ffnum=ffnum[[i]],ffhiv=ffhiv[[i]],clipv=clipv[[i]],clgem=clgem[[i]],clalc=clalc[[i]],clgend=clgend[[i]],clnum=clnum[[i]],clhiv=clhiv[[i]],r.ipv=r.ipv[[i]],r.gem=r.gem[[i]],r.alc=r.alc[[i]],r.gend=r.gend[[i]],
            camp11=camplist[[1]][[i]],camp21=camplist[[2]][[i]],camp31=camplist[[3]][[i]],camp41=camplist[[4]][[i]],camp51=camplist[[5]][[i]],camp61=camplist[[6]][[i]],camp71=camplist[[7]][[i]],camp81=camplist[[8]][[i]],camp91=camplist[[9]][[i]],camp101=camplist[[10]][[i]],camp111=camplist[[11]][[i]],camp121=camplist[[12]][[i]],camp131=camplist[[13]][[i]],camp141=camplist[[14]][[i]],camp151=camplist[[15]][[i]],camp161=camplist[[16]][[i]],camp171=camplist[[17]][[i]],camp181=camplist[[18]][[i]],camp191=camplist[[19]][[i]],camp201=camplist[[20]][[i]],camp211=camplist[[21]][[i]],camp221=camplist[[22]][[i]],camp231=camplist[[23]][[i]],camp241=camplist[[24]][[i]],camp251=camplist[[25]][[i]],camp261=camplist[[26]][[i]],camp271=camplist[[27]][[i]],camp281=camplist[[28]][[i]],camp291=camplist[[29]][[i]],camp301=camplist[[30]][[i]],camp311=camplist[[31]][[i]],camp321=camplist[[32]][[i]],camp331=camplist[[33]][[i]],camp341=camplist[[34]][[i]],camp351=camplist[[35]][[i]],camp361=camplist[[36]][[i]],camp371=camplist[[37]][[i]],camp381=camplist[[38]][[i]],camp391=camplist[[39]][[i]],camp401=camplist[[40]][[i]],camp411=camplist[[41]][[i]],camp421=camplist[[42]][[i]],camp431=camplist[[43]][[i]],camp441=camplist[[44]][[i]],camp451=camplist[[45]][[i]],camp461=camplist[[46]][[i]],camp471=camplist[[47]][[i]],camp481=camplist[[48]][[i]],camp491=camplist[[49]][[i]],camp501=camplist[[50]][[i]],camp511=camplist[[51]][[i]],camp521=camplist[[52]][[i]],camp531=camplist[[53]][[i]],camp541=camplist[[54]][[i]],camp551=camplist[[55]][[i]],camp561=camplist[[56]][[i]],camp571=camplist[[57]][[i]],camp581=camplist[[58]][[i]],camp591=camplist[[59]][[i]],camp13=camplist[[60]][[i]],camp23=camplist[[61]][[i]],camp33=camplist[[62]][[i]],camp43=camplist[[63]][[i]],camp53=camplist[[64]][[i]],camp63=camplist[[65]][[i]],camp73=camplist[[66]][[i]],camp83=camplist[[67]][[i]],camp93=camplist[[68]][[i]],camp103=camplist[[69]][[i]],camp113=camplist[[70]][[i]],camp123=camplist[[71]][[i]],camp133=camplist[[72]][[i]],camp143=camplist[[73]][[i]],camp153=camplist[[74]][[i]],camp163=camplist[[75]][[i]],camp173=camplist[[76]][[i]],camp183=camplist[[77]][[i]],camp193=camplist[[78]][[i]],camp203=camplist[[79]][[i]],camp213=camplist[[80]][[i]],camp223=camplist[[81]][[i]],camp233=camplist[[82]][[i]],camp243=camplist[[83]][[i]],camp253=camplist[[84]][[i]],camp263=camplist[[85]][[i]],camp273=camplist[[86]][[i]],camp283=camplist[[87]][[i]],camp293=camplist[[88]][[i]],camp303=camplist[[89]][[i]],camp313=camplist[[90]][[i]],camp323=camplist[[91]][[i]],camp333=camplist[[92]][[i]],camp343=camplist[[93]][[i]],camp353=camplist[[94]][[i]],camp363=camplist[[95]][[i]],camp373=camplist[[96]][[i]],camp383=camplist[[97]][[i]],camp393=camplist[[98]][[i]],camp403=camplist[[99]][[i]],camp413=camplist[[100]][[i]],camp423=camplist[[101]][[i]],camp433=camplist[[102]][[i]],camp443=camplist[[103]][[i]],camp453=camplist[[104]][[i]],camp463=camplist[[105]][[i]],camp473=camplist[[106]][[i]],camp483=camplist[[107]][[i]],camp493=camplist[[108]][[i]],camp503=camplist[[109]][[i]],camp513=camplist[[110]][[i]],camp523=camplist[[111]][[i]],camp533=camplist[[112]][[i]],camp543=camplist[[113]][[i]],camp553=camplist[[114]][[i]],camp563=camplist[[115]][[i]],camp573=camplist[[116]][[i]],camp583=camplist[[117]][[i]],camp593=camplist[[118]][[i]]))

      }
    }
#4 - Remove unecessary stuff from environment
    rm(list=setdiff(ls(), c("dataset","options","sumstats","sumstats2","goodcamps","SaveResults")))

    datasetg <- sienaGroupCreate(dataset)
    
#5 - subset to good camps    
#     dataset<-dataset[which(paste(sumstats2$Wave,sumstats2$Camp) %in% paste(goodcamps$Wave,goodcamps$Camp))]
#     sumstats2<-sumstats2[which(paste(sumstats2$Wave,sumstats2$Camp) %in% paste(goodcamps$Wave,goodcamps$Camp)),]
#     datasetg <- sienaGroupCreate(dataset)
# 
#     # ipvconvbeh<-readRDS("ipvconvbeh.rds")
#     # dataset<-dataset[which(!(paste(sumstats2$Wave,sumstats2$Camp) %in% paste(ipvconvbeh$Wave,ipvconvbeh$Camp)))]
#     # sumstats2<-sumstats2[which(!(paste(sumstats2$Wave,sumstats2$Camp) %in% paste(ipvconvbeh$Wave,ipvconvbeh$Camp))),]
#     # datasetg <- sienaGroupCreate(dataset)
#
#     ipvconvbeh<-readRDS("ipvconvbeh.rds")
#     dataset<-dataset[which((paste(sumstats2$Wave,sumstats2$Camp) %in% paste(ipvconvbeh$Wave,ipvconvbeh$Camp)))]
#     sumstats2<-sumstats2[which((paste(sumstats2$Wave,sumstats2$Camp) %in% paste(ipvconvbeh$Wave,ipvconvbeh$Camp))),]
#     datasetg <- sienaGroupCreate(dataset)
#     
#     
#     # sum(duplicated(sumstats2$Camp))
#     # table(sumstats2$Wave)
#     # #34 camps are good across 2 waves
#     # #17 camps are only good in 1 wave
#     # #Only 2 camps are good in wave 1 but not in wave 2.
#     
#   
#     #subset to positively correlated camps
#         datasetg <- sienaGroupCreate(dataset)
#         temp1<-(sapply(datasetg, function(x) (x$depvars$DV[,,1])))
#         temp2<-(sapply(datasetg, function(x) (x$depvars$DV[,,2])))
#         cors<-mapply(function(x,y) round(cor(x,y,use = "complete"),2), x=temp1,y=temp2)
#         a<-!is.na(cors) & cors>.15
#         sum(a)
#         dataset2<-dataset[a]
#         datasetg <- sienaGroupCreate(dataset2)
#     
#     #Subset to just one large camp
#         datasetg <- sienaGroupCreate(dataset)
#         sum1<-(sapply(datasetg, function(x) sum(!is.na(x$depvars$DV[,,1]))))
#         sum2<-(sapply(datasetg, function(x) sum(!is.na(x$depvars$DV[,,2]))))
#         sum3<-(sum1+sum2)/2
#         b<-sort(sum3,decreasing=T)[1]
#         a<-which(sum3==b)
#         datasetg<-dataset[[a]]
#         
#         b<-sort(sum3,decreasing=T)[(1:15)]
#         a<-which(sum3%in%b)
#         datasetg<-sienaGroupCreate(dataset[a])
#         
#     # #subset to places that fit the model
#     #     effects<-Model$effects$effectName[!(grepl("wave",Model$effects$effectName) & !grepl("x",Model$effects$effectName))]
#     #     effects<-effects[!(grepl("condition",effects) & !grepl("x",effects))]
#     #     rates<-Model$theta[grepl("^rate DV",effects)]
#     #     toohigh<-rates>15 |rates==0
#     #     dataset2<-dataset2[!toohigh]
#     #     datasetg <- sienaGroupCreate(dataset2)
#     
#     # #DV freq
#     #     freq1<-(sapply(datasetg, function(x) sum(x$depvars$DV[,,1],na.rm=T)))
#     #     freq2<-(sapply(datasetg, function(x) sum(x$depvars$DV[,,2],na.rm=T)))
#     #     size<-mapply(function(x,y) round(cor(x,y,use = "complete"),2), x=temp1,y=temp2)
#         
#         
#         
#         
#         
#         
#         
#         
#         
#         
