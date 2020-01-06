  #4 - Prep non-network data
{
    #a - prep variables
        #i.create function for filling missing entries with 0s when appropriate
        fillmiss<-function(x){
          y<-x
          wave<-substr(deparse(substitute(x)),4,4)
          if(wave=="b"){wave<-df$b_respond}else if(wave=="m"){wave<-df$m_respond}else if(wave=="e"){wave<-df$e_respond}
          y[is.na(y) & wave==1]<-0
          return(y)
        }
        #ii. alter variables
        df$b_testhiv_12<-df$b_testhiv
        
        df$b_sxp12_cont<-df$b_sxp12_num
        df$m_sxp12_cont<-df$m_sxp12_num
        df$e_sxp12_cont<-df$e_sxp12_num
        
        df$b_sxp12_cont<-ifelse(df$b_sxp12_cont>2,3,df$b_sxp12_cont)
        df$m_sxp12_cont<-ifelse(df$m_sxp12_cont>2,3,df$m_sxp12_cont)
        df$e_sxp12_cont<-ifelse(df$e_sxp12_cont>2,3,df$e_sxp12_cont)
        
        
        df$b_sxp12_num<-ifelse(df$b_sxp12_num>0,1,0)
        df$m_sxp12_num<-ifelse(df$m_sxp12_num>0,1,0)
        df$e_sxp12_num<-ifelse(df$e_sxp12_num>0,1,0)
        df$b_elig5_d<-ifelse(df$b_elig5_d>3,3,df$b_elig5_d)
        df$b_child_ev[df$b_child_ev>1]<-1
        df$b_sxp12_num<-fillmiss(df$b_sxp12_num)
        df$m_sxp12_num<-fillmiss(df$m_sxp12_num)
        df$e_sxp12_num<-fillmiss(df$e_sxp12_num)
        df$m_partnered<-fillmiss(df$m_partnered)
        df$e_partnered<-fillmiss(df$e_partnered)
        df$b_alc_frq<-fillmiss(df$b_alc_frq)
        df$m_alc_frq<-fillmiss(df$m_alc_frq)
        df$e_alc_frq<-fillmiss(df$e_alc_frq)
        df$b_drnk_frq<-fillmiss(df$b_drnk_frq)
        df$m_drnk_frq<-fillmiss(df$m_drnk_frq)
        df$e_drnk_frq<-fillmiss(df$e_drnk_frq)
        df$b_alc_bing<-fillmiss(df$b_alc_bing)
        df$m_alc_bing<-fillmiss(df$m_alc_bing)
        df$e_alc_bing<-fillmiss(df$e_alc_bing)
        
        df$b_all_ipv<-ifelse(df$b_ipvoutcome_bin|df$b_ipvpsych_bin,1,0)
        df$m_all_ipv<-ifelse(df$m_ipvoutcome_bin|df$m_ipvpsych_bin,1,0)
        df$e_all_ipv<-ifelse(df$e_ipvoutcome_bin|df$e_ipvpsych_bin,1,0)
    
    #b - Separate df into lists based on camp
        df1<-df
        df2<-df
        #bring in sumvars
            sumvars<-readRDS("sumvars.rds")
            list2env(sumvars,globalenv())
            tdf<-matrix(nrow=length(unlist(sumvars[[i]])),ncol=length(sumvars))
            for(i in 1:length(sumvars)){
              tdf[,i]<-unlist(sumvars[[i]])
            }
            tdf<-data.frame(tdf)
            names(tdf)<-names(sumvars)
            tdf1<-tdf[1:nrow(df),]
            tdf2<-tdf[(nrow(df)+1):nrow(tdf),]
        #subset df2 to individuals who are not missing dv info in both waves
            df2<-df2[!(is.na(df$m_testhiv_12) & is.na(df$e_testhiv_12)),]
        #merge in sumvars
            df1<-cbind(df1,tdf1)
            df2<-cbind(df2,tdf2)
        #subset data and list it
        df1<-df1[df1$camp %in% camps1,]
        df2<-df2[df2$camp %in% camps2,]
        dfl<-split(df1,df1$ward)
        dfl2<-split(df2,df2$ward)
    #c - Create RSiena objects for each variable
        dv.ipvoutcome_cat<-list()
        dv.ipvoutcome_bin<-list()
        dv.gem_r15_avg<-list()
        dv.ipvphysical_cat<-list()
        dv.testhiv_12<-list()
        dv.ipvpsych_cat<-list()
        dv.all_ipv<-list()
        gem<-list()
        sex<-list()
        age<-list()
        edu<-list()
        married<-list()
        ses<-list()
        ward1<-list()
        ward2<-list()
        ward3<-list()
        ward4<-list()
        condition<-list()
        leader<-list()
        wave<-list()
        sex.active<-list()
        sex.year<- list()
        child.violence<-list()
        child.sex.violence<-list()
        camp.duration<-list()
        campn<-list()
        has.children<-list()
        fam.available<-list()
        hivtst.ego<-list()
        hivinjf.ego<-list()
        hivadvf.ego<-list()
        knowf.ego<-list()
        closef.ego<-list()
        ever.test<-list()
        ever.ipv<-list()
        alc_evr<-list()
        alc_frq<-list()
        alc_bing<-list()
        drnk_frq<-list()
        hivtst.alter<-list()
        hivinjf.alter<-list()
        hivadvf.alter<-list()
        hivtst.group<-list()
        sex.num<-list()
        dv.alc_freq<-list()
        N1<-list()
        jaccard<-list()
        missing<-list()
        d.ipv<-list()
        d.hiv<-list()
        r.hiv<-list()
        density<-list()
        r.hivtst<-list()
        r.hivinj<-list()
        r.hivadv<-list()
    #d - create an index to create covariates by; note: camp number does not indicate camp name, just where it occurs in the list of camps
        waveindex<-data.frame(Wave=c(rep(1,length(unique(tnet1$Ward1))),rep(3,length(unique(tnet2$Ward1)))),camp=c(unique(tnet1$Ward1),unique(tnet2$Ward1)))
        
    #e - create covariates
        for(i in 1:nrow(waveindex)){
            camp<-waveindex$camp[i]
            Wave<-waveindex$Wave[i]
                if(Wave==1){
                  sex[[i]] <- coCovar(dfl[[camp]]$gender)
                  age[[i]] <- coCovar(dfl[[camp]]$age_cat)
                  edu[[i]] <- coCovar(dfl[[camp]]$b_edu_cat)
                  married[[i]] <- coCovar(dfl[[camp]]$b_married)
                  ses[[i]] <- coCovar(dfl[[camp]]$b_ses)
                  campn[[i]] <- coCovar(dfl[[camp]]$camp, centered=FALSE)
                  ward1[[i]] <- coCovar(as.numeric(dfl[[camp]]$ward==1))
                  ward2[[i]] <- coCovar(as.numeric(dfl[[camp]]$ward==2))
                  ward3[[i]] <- coCovar(as.numeric(dfl[[camp]]$ward==3))
                  ward4[[i]] <- coCovar(as.numeric(dfl[[camp]]$ward==4))
                  condition[[i]] <- coCovar(dfl[[camp]]$condition)
                  leader[[i]] <- coCovar(dfl[[camp]]$chl)
                  child.violence[[i]]<-coCovar(dfl[[camp]]$b_childvio1)
                  child.sex.violence[[i]]<-coCovar(dfl[[camp]]$b_childvio2)
                  camp.duration[[i]]<-coCovar(dfl[[camp]]$b_elig5_d)
                  has.children[[i]] <- coCovar(dfl[[camp]]$b_child_ev)
                  fam.available[[i]] <- coCovar(dfl[[camp]]$b_ssfamavl)
                  ever.test[[i]] <- coCovar(dfl[[camp]]$b_testhiv)
                  ever.ipv[[i]] <- coCovar(dfl[[camp]]$b_ipvever_bin)
                  alc_evr[[i]] <- coCovar(dfl[[camp]]$b_alc_evr)
                  hivtst.group[[i]] <- coCovar(dfl[[camp]]$e_hivtst_c)
                  dv.ipvoutcome_cat[[i]]<-as.matrix(dfl[[camp]][,c("b_ipvoutcome_cat","m_ipvoutcome_cat")])
                  dv.ipvoutcome_cat[[i]]<-sienaDependent(dv.ipvoutcome_cat[[i]], type ="behavior",allowOnly = F)
                  dv.ipvoutcome_bin[[i]]<-as.matrix(dfl[[camp]][,c("b_ipvoutcome_bin","m_ipvoutcome_bin")])
                  dv.ipvoutcome_bin[[i]]<-sienaDependent(dv.ipvoutcome_bin[[i]], type ="behavior",allowOnly = F)
                  dv.gem_r15_avg[[i]]<-as.matrix(dfl[[camp]][,c("b_gem_r15_avg","m_gem_r15_avg")])
                  dv.gem_r15_avg[[i]]<-sienaDependent(dv.gem_r15_avg[[i]], type ="behavior",allowOnly = F)
                  dv.testhiv_12[[i]]<-as.matrix(dfl[[camp]][,c("b_testhiv_12","m_testhiv_12")])
                  dv.testhiv_12[[i]]<-sienaDependent(dv.testhiv_12[[i]], type ="behavior",allowOnly = F)
                  dv.ipvphysical_cat[[i]]<-as.matrix(dfl[[camp]][,c("b_ipvphysical_cat","m_ipvphysical_cat")])
                  dv.ipvphysical_cat[[i]]<-sienaDependent(dv.ipvphysical_cat[[i]], type ="behavior",allowOnly = F)
                  dv.ipvpsych_cat[[i]]<-as.matrix(dfl[[camp]][,c("b_ipvpsych_cat","m_ipvpsych_cat")])
                  dv.ipvpsych_cat[[i]]<-sienaDependent(dv.ipvpsych_cat[[i]], type ="behavior",allowOnly = F)
                  dv.all_ipv[[i]]<-as.matrix(dfl[[camp]][,c("b_all_ipv","m_all_ipv")])
                  dv.all_ipv[[i]]<-sienaDependent(dv.all_ipv[[i]], type ="behavior",allowOnly = F)
                  
                  dv.alc_freq[[i]]<-as.matrix(dfl[[camp]][,c("b_alc_frq","m_alc_frq")])
                  dv.alc_freq[[i]]<-sienaDependent(dv.alc_freq[[i]], type ="behavior",allowOnly = F)
                  
                  gem[[i]] <- coCovar(as.vector(dfl[[camp]][,c("b_gem_r15_avg")]))
                  wave[[i]] <- coCovar(rep(1,nrow(dfl[[camp]])))
                  sex.active[[i]] <- coCovar(as.vector(dfl[[camp]][,c("b_partnered")]))
                  sex.year[[i]] <- coCovar(as.vector(dfl[[camp]][,c("b_sxp12_num")]))
                  sex.num[[i]] <- coCovar(as.vector(dfl[[camp]][,c("b_sxp12_cont")]))
                  hivtst.ego[[i]] <- coCovar(as.vector(dfl[[camp]][,c("b_hivtstfego")]))
                  hivinjf.ego[[i]] <- coCovar(as.vector(dfl[[camp]][,c("b_hivinjfego")]))
                  hivadvf.ego[[i]] <- coCovar(as.vector(dfl[[camp]][,c("b_hivadvfego")]))
                  knowf.ego[[i]] <- coCovar(as.vector(dfl[[camp]][,c("b_knowfego")]))
                  closef.ego[[i]] <- coCovar(as.vector(dfl[[camp]][,c("b_closefego")]))
                  alc_frq[[i]] <- coCovar(as.vector(dfl[[camp]][,c("b_alc_frq")]))
                  drnk_frq[[i]] <- coCovar(as.vector(dfl[[camp]][,c("b_drnk_frq")]))
                  alc_bing[[i]] <- coCovar(as.vector(dfl[[camp]][,c("b_alc_bing")]))
                  hivtst.alter[[i]] <- coCovar(as.vector(dfl[[camp]][,c("b_hivtstfalter")]))
                  hivinjf.alter[[i]] <- coCovar(as.vector(dfl[[camp]][,c("b_hivinjfalter")]))
                  hivadvf.alter[[i]] <- coCovar(as.vector(dfl[[camp]][,c("b_hivadvfalter")]))
                  
                  N1[[i]] <- coCovar(as.vector(dfl[[camp]][,c("N1")]))
                  jaccard[[i]] <- coCovar(as.vector(dfl[[camp]][,c("jaccard")]))
                  missing[[i]] <- coCovar(as.vector(dfl[[camp]][,c("missing")]))
                  d.ipv[[i]] <- coCovar(as.vector(dfl[[camp]][,c("d.ipv")]))
                  d.hiv[[i]] <- coCovar(as.vector(dfl[[camp]][,c("d.hiv")]))
                  r.hiv[[i]] <- coCovar(as.vector(dfl[[camp]][,c("r.hiv")]))
                  density[[i]] <- coCovar(as.vector(dfl[[camp]][,c("density")]))
                  r.hivtst[[i]] <- coCovar(as.vector(dfl[[camp]][,c("r.hivtst")]))
                  r.hivinj[[i]] <- coCovar(as.vector(dfl[[camp]][,c("r.hivinj")]))
                  r.hivadv[[i]] <- coCovar(as.vector(dfl[[camp]][,c("r.hivadv")]))
                  
                  
                }else{
                  sex[[i]] <- coCovar(dfl2[[camp]]$gender)
                  age[[i]] <- coCovar(dfl2[[camp]]$age_cat)
                  edu[[i]] <- coCovar(dfl2[[camp]]$b_edu_cat)
                  married[[i]] <- coCovar(dfl2[[camp]]$b_married)
                  ses[[i]] <- coCovar(dfl2[[camp]]$b_ses)
                  campn[[i]] <- coCovar(dfl2[[camp]]$camp, centered=FALSE)
                  ward1[[i]] <- coCovar(as.numeric(dfl2[[camp]]$ward==1))
                  ward2[[i]] <- coCovar(as.numeric(dfl2[[camp]]$ward==2))
                  ward3[[i]] <- coCovar(as.numeric(dfl2[[camp]]$ward==3))
                  ward4[[i]] <- coCovar(as.numeric(dfl2[[camp]]$ward==4))
                  condition[[i]] <- coCovar(dfl2[[camp]]$condition)
                  leader[[i]] <- coCovar(dfl2[[camp]]$chl)
                  child.violence[[i]]<-coCovar(dfl2[[camp]]$b_childvio1)
                  child.sex.violence[[i]]<-coCovar(dfl2[[camp]]$b_childvio2)
                  camp.duration[[i]]<-coCovar(dfl2[[camp]]$b_elig5_d)
                  has.children[[i]] <- coCovar(dfl2[[camp]]$b_child_ev)
                  fam.available[[i]] <- coCovar(dfl2[[camp]]$b_ssfamavl)
                  ever.test[[i]] <- coCovar(dfl2[[camp]]$b_testhiv)
                  ever.ipv[[i]] <- coCovar(dfl2[[camp]]$b_ipvever_bin)
                  alc_evr[[i]] <- coCovar(dfl2[[camp]]$b_alc_evr)
                  hivtst.group[[i]] <- coCovar(dfl2[[camp]]$e_hivtst_c)
                  dv.ipvoutcome_cat[[i]]<-as.matrix(dfl2[[camp]][,c("m_ipvoutcome_cat","e_ipvoutcome_cat")])
                  dv.ipvoutcome_cat[[i]]<-sienaDependent(dv.ipvoutcome_cat[[i]], type ="behavior",allowOnly = F)
                  dv.ipvoutcome_bin[[i]]<-as.matrix(dfl2[[camp]][,c("m_ipvoutcome_bin","e_ipvoutcome_bin")])
                  dv.ipvoutcome_bin[[i]]<-sienaDependent(dv.ipvoutcome_bin[[i]], type ="behavior",allowOnly = F)
                  dv.gem_r15_avg[[i]]<-as.matrix(dfl2[[camp]][,c("m_gem_r15_avg","e_gem_r15_avg")])
                  dv.gem_r15_avg[[i]]<-sienaDependent(dv.gem_r15_avg[[i]], type ="behavior",allowOnly = F)
                  dv.ipvphysical_cat[[i]]<-as.matrix(dfl2[[camp]][,c("m_ipvphysical_cat","e_ipvphysical_cat")])
                  dv.ipvphysical_cat[[i]]<-sienaDependent(dv.ipvphysical_cat[[i]], type ="behavior",allowOnly = F)
                  dv.ipvpsych_cat[[i]]<-as.matrix(dfl2[[camp]][,c("m_ipvpsych_cat","e_ipvpsych_cat")])
                  dv.ipvpsych_cat[[i]]<-sienaDependent(dv.ipvpsych_cat[[i]], type ="behavior",allowOnly = F)
                  dv.testhiv_12[[i]]<-as.matrix(dfl2[[camp]][,c("m_testhiv_12","e_testhiv_12")])
                  dv.testhiv_12[[i]]<-sienaDependent(dv.testhiv_12[[i]], type ="behavior",allowOnly = F)
                  dv.all_ipv[[i]]<-as.matrix(dfl2[[camp]][,c("m_all_ipv","e_all_ipv")])
                  dv.all_ipv[[i]]<-sienaDependent(dv.all_ipv[[i]], type ="behavior",allowOnly = F)
                  gem[[i]] <- coCovar(as.vector(dfl2[[camp]][,c("m_gem_r15_avg")]))
                  wave[[i]] <- coCovar(rep(3,nrow(dfl2[[camp]])))
                  sex.active[[i]] <- coCovar(as.vector(dfl2[[camp]][,c("m_partnered")]))
                  sex.year[[i]] <- coCovar(as.vector(dfl2[[camp]][,c("m_sxp12_num")]))
                  sex.num[[i]] <- coCovar(as.vector(dfl2[[camp]][,c("m_sxp12_cont")]))
                  hivtst.ego[[i]] <- coCovar(as.vector(dfl2[[camp]][,c("m_hivtstfego")]))
                  hivinjf.ego[[i]] <- coCovar(as.vector(dfl2[[camp]][,c("m_hivinjfego")]))
                  hivadvf.ego[[i]] <- coCovar(as.vector(dfl2[[camp]][,c("m_hivadvfego")]))
                  knowf.ego[[i]] <- coCovar(as.vector(dfl2[[camp]][,c("m_knowfego")]))
                  closef.ego[[i]] <- coCovar(as.vector(dfl2[[camp]][,c("m_closefego")]))
                  alc_frq[[i]] <- coCovar(as.vector(dfl2[[camp]][,c("m_alc_frq")]))
                  drnk_frq[[i]] <- coCovar(as.vector(dfl2[[camp]][,c("m_drnk_frq")]))
                  alc_bing[[i]] <- coCovar(as.vector(dfl2[[camp]][,c("m_alc_bing")]))
                  hivtst.alter[[i]] <- coCovar(as.vector(dfl2[[camp]][,c("m_hivtstfalter")]))
                  hivinjf.alter[[i]] <- coCovar(as.vector(dfl2[[camp]][,c("m_hivinjfalter")]))
                  hivadvf.alter[[i]] <- coCovar(as.vector(dfl2[[camp]][,c("m_hivadvfalter")]))
                  
                  dv.alc_freq[[i]]<-as.matrix(dfl2[[camp]][,c("m_alc_frq","e_alc_frq")])
                  dv.alc_freq[[i]]<-sienaDependent(dv.alc_freq[[i]], type ="behavior",allowOnly = F)
                  
                  N1[[i]] <- coCovar(as.vector(dfl2[[camp]][,c("N1")]))
                  jaccard[[i]] <- coCovar(as.vector(dfl2[[camp]][,c("jaccard")]))
                  missing[[i]] <- coCovar(as.vector(dfl2[[camp]][,c("missing")]))
                  d.ipv[[i]] <- coCovar(as.vector(dfl2[[camp]][,c("d.ipv")]))
                  d.hiv[[i]] <- coCovar(as.vector(dfl2[[camp]][,c("d.hiv")]))
                  r.hiv[[i]] <- coCovar(as.vector(dfl2[[camp]][,c("r.hiv")]))
                  density[[i]] <- coCovar(as.vector(dfl2[[camp]][,c("density")]))
                  r.hivtst[[i]] <- coCovar(as.vector(dfl2[[camp]][,c("r.hivtst")]))
                  r.hivinj[[i]] <- coCovar(as.vector(dfl2[[camp]][,c("r.hivinj")]))
                  r.hivadv[[i]] <- coCovar(as.vector(dfl2[[camp]][,c("r.hivadv")]))
                }
            }
}



