#6 - Remove individuals from waves 2-3 who were not present during either wave, as they are non-informative
tmdftesthiv_12<-list()
tmdf1<-df
tmdf2<-df
tmdf1<-tmdf1[tmdf1$camp %in% camps1,]
tmdf2<-tmdf2[tmdf2$camp %in% camps2,]
tmdfl<-split(tmdf1,tmdf1$ward)
tmdfl2<-split(tmdf2,tmdf2$ward)
for(i in 1:nrow(waveindex)){
  camp<-waveindex$camp[i]
  Wave<-waveindex$Wave[i]
  if(Wave==1){
    tmdftesthiv_12[[i]]<-as.matrix(tmdfl[[camp]][,c("b_testhiv_12","m_testhiv_12")])
    tmdftesthiv_12[[i]]<-sienaDependent(tmdftesthiv_12[[i]], type ="behavior",allowOnly = F)
  }else{
    tmdftesthiv_12[[i]]<-as.matrix(tmdfl2[[camp]][,c("m_testhiv_12","e_testhiv_12")])
    tmdftesthiv_12[[i]]<-sienaDependent(tmdftesthiv_12[[i]], type ="behavior",allowOnly = F)
  }
}
  for(i in 1:length(friendship)){
    #determine if individual missing in both waves
      mDV<-matrix(tmdftesthiv_12[[i]],ncol=2)
      missing<-is.na(mDV[,1])&is.na(mDV[,2])
    #if respondent is missing in both waves, remove him from all siena objects
      if(sum(missing)>0){
        index<-which(missing)
        # dv.ipvoutcome_cat[[i]]<-sienaDependent(dv.ipvoutcome_cat[[i]][-index,1,], type ="behavior",allowOnly = F)
        # dv.ipvoutcome_bin[[i]]<-sienaDependent(dv.ipvoutcome_bin[[i]][-index,1,], type ="behavior",allowOnly = F)
        # dv.gem_r15_avg[[i]]<-sienaDependent(dv.gem_r15_avg[[i]][-index,1,], type ="behavior",allowOnly = F)
        # dv.ipvphysical_cat[[i]]<-sienaDependent(dv.ipvphysical_cat[[i]][-index,1,], type ="behavior",allowOnly = F)
        # dv.ipvpsych_cat[[i]]<-sienaDependent(dv.ipvpsych_cat[[i]][-index,1,], type ="behavior",allowOnly = F)
        # dv.testhiv_12[[i]]<-sienaDependent(dv.testhiv_12[[i]][-index,1,], type ="behavior",allowOnly = F)
        friendship[[i]]<-sienaNet(friendship[[i]][-index,-index,])
        ffriendship[[i]]<-coDyadCovar(ffriendship[[i]][-index,-index])
        hivtstf[[i]]<-coDyadCovar(hivtstf[[i]][-index,-index])
        hivinjf[[i]]<-coDyadCovar(hivinjf[[i]][-index,-index])
        hivadvf[[i]]<-coDyadCovar(hivadvf[[i]][-index,-index])
        knowf[[i]]<-coDyadCovar(knowf[[i]][-index,-index])
        closef[[i]]<-coDyadCovar(closef[[i]][-index,-index])
        # sex[[i]] <- coCovar(sex[[i]][-index])
        # age[[i]] <- coCovar(age[[i]][-index])
        # edu[[i]] <- coCovar(edu[[i]][-index])
        # married[[i]] <- coCovar(married[[i]][-index])
        # ses[[i]] <- coCovar(ses[[i]][-index])
        # campn[[i]] <- coCovar(campn[[i]][-index], centered=FALSE)
        # ward1[[i]] <- coCovar(ward1[[i]][-index])
        # ward2[[i]] <- coCovar(ward2[[i]][-index])
        # ward3[[i]] <- coCovar(ward3[[i]][-index])
        # ward4[[i]] <- coCovar(ward4[[i]][-index])
        # condition[[i]] <- coCovar(condition[[i]][-index])
        # leader[[i]] <- coCovar(leader[[i]][-index])
        # gem[[i]] <- coCovar(gem[[i]][-index])
        # wave[[i]] <- coCovar(wave[[i]][-index])
        # sex.active[[i]] <- coCovar(sex.active[[i]][-index])
        # sex.year[[i]] <- coCovar(sex.year[[i]][-index])
        # child.violence[[i]] <- coCovar(child.violence[[i]][-index])
        # child.sex.violence[[i]] <- coCovar(child.sex.violence[[i]][-index])
        # camp.duration[[i]] <- coCovar(camp.duration[[i]][-index])
        # has.children[[i]] <- coCovar(has.children[[i]][-index])
        # fam.available[[i]] <- coCovar(fam.available[[i]][-index])
        # hivtst.ego[[i]] <- coCovar(hivtst.ego[[i]][-index])
        # hivinjf.ego[[i]] <- coCovar(hivinjf.ego[[i]][-index])
        # hivadvf.ego[[i]] <- coCovar(hivadvf.ego[[i]][-index])
        # knowf.ego[[i]] <- coCovar(knowf.ego[[i]][-index])
        # closef.ego[[i]] <- coCovar(closef.ego[[i]][-index])
        # ever.test[[i]] <- coCovar(ever.test[[i]][-index])
        # ever.ipv[[i]] <- coCovar(ever.ipv[[i]][-index])
        # alc_evr[[i]] <- coCovar(alc_evr[[i]][-index])
        # alc_frq[[i]] <- coCovar(alc_frq[[i]][-index])
        # drnk_frq[[i]] <- coCovar(drnk_frq[[i]][-index])
        # alc_bing[[i]] <- coCovar(alc_bing[[i]][-index])
        # hivtst.alter[[i]] <- coCovar(hivtst.alter[[i]][-index])
        # hivinjf.alter[[i]] <- coCovar(hivinjf.alter[[i]][-index])
        # hivadvf.alter[[i]] <- coCovar(hivadvf.alter[[i]][-index])
        # hivtst.group[[i]] <- coCovar(hivtst.group[[i]][-index])
        # sex.num[[i]] <- coCovar(sex.num[[i]][-index])
        # dv.all_ipv[[i]]<-sienaDependent(dv.all_ipv[[i]][-index,1,], type ="behavior",allowOnly = F)
        # dv.alc_freq[[i]]<-sienaDependent(dv.alc_freq[[i]][-index,1,], type ="behavior",allowOnly = F)
        # 
        # 
        # N1[[i]] <- coCovar(N1[[i]][-index])
        # jaccard[[i]] <- coCovar(jaccard[[i]][-index])
        # missing[[i]] <- coCovar(missing[[i]][-index])
        # d.ipv[[i]] <- coCovar(d.ipv[[i]][-index])
        # d.hiv[[i]] <- coCovar(d.hiv[[i]][-index])
        # r.hiv[[i]] <- coCovar(r.hiv[[i]][-index])
        # density[[i]] <- coCovar(density[[i]][-index])
        # r.hivtst[[i]] <- coCovar(r.hivtst[[i]][-index])
        # r.hivinj[[i]] <- coCovar(r.hivinj[[i]][-index])
        # r.hivadv[[i]] <- coCovar(r.hivadv[[i]][-index])

      }
  }
