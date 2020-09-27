#Stats and ols models for IPV
    source("prep.2 - Load data.R")
    source("prep.3 - Prep network data.R")
    tfnet1<-fnet1
    tfnet2<-fnet2
    tfnet3<-fnet3
    #create function to get mean friend info
        fmean<-function(x,y,z){
            #create dataframe of just pid and variable of interest
                df2<-data.table(x[,c("pid",z)])
                names(df2)<-c("ID2","a")
            #subset net1 to just individuals with ties
                a<-y[y$tie==1,]
            #merge ties and covariates
                b<-merge(a,df2,by="ID2")
            #get average alter stats by covariate
                c<-b[,.(means=mean(a,na.rm=T)),by=.(ID1)]
            #put covariate info into dataframe (to order correctly)
                names(df2)[1]<-"ID1"
                df2<-merge(df2,c,by="ID1",all=T)
            #return covariates
                return(df2$means)
        }

    #prep fnet for this (for convenience, using original data rather than subset)
        {
        fnet1<-data.table(ID1=tfnet1[,1],ID2=tfnet1[,2],tie=tfnet1[,3])
        fnet2<-data.table(ID1=tfnet2[,2],ID2=tfnet2[,3],tie=tfnet2[,4])
        fnet3<-data.table(ID1=tfnet3[,2],ID2=tfnet3[,3],tie=tfnet3[,4])
        fnet1<-fnet1[fnet1$ID1 %in% df$pid & fnet1$ID2 %in% df$pid]
        fnet2<-fnet2[fnet2$ID1 %in% df$pid & fnet2$ID2 %in% df$pid]
        fnet3<-fnet3[fnet3$ID1 %in% df$pid & fnet3$ID2 %in% df$pid]
    #create male versions of these
        df2<-df[df$gender==1,]
        mnet1<-net1[net1$ID1 %in% df2$pid & net1$ID2 %in% df2$pid]
        mnet2<-net2[net2$ID1 %in% df2$pid & net2$ID2 %in% df2$pid]
        mnet3<-net3[net3$ID1 %in% df2$pid & net3$ID2 %in% df2$pid]
        mfnet1<-fnet1[fnet1$ID1 %in% df2$pid & fnet1$ID2 %in% df2$pid]
        mfnet2<-fnet2[fnet2$ID1 %in% df2$pid & fnet2$ID2 %in% df2$pid]
        mfnet3<-fnet3[fnet3$ID1 %in% df2$pid & fnet3$ID2 %in% df2$pid]

    #IPV
        df$cfIPV1<-fmean(df,net1,"b_ipvphysical_bin")
        df$cfIPV2<-fmean(df,net2,"m_ipvphysical_bin")
        df$cfIPV3<-fmean(df,net3,"e_ipvphysical_bin")
        df$ffIPV1<-fmean(df,fnet1,"b_ipvphysical_bin")
        df$ffIPV2<-fmean(df,fnet2,"m_ipvphysical_bin")
        df$ffIPV3<-fmean(df,fnet3,"e_ipvphysical_bin")
        df$mcfIPV1<-fmean(df,mnet1,"b_ipvphysical_bin")
        df$mcfIPV2<-fmean(df,mnet2,"m_ipvphysical_bin")
        df$mcfIPV3<-fmean(df,mnet3,"e_ipvphysical_bin")
        df$mffIPV1<-fmean(df,mfnet1,"b_ipvphysical_bin")
        df$mffIPV2<-fmean(df,mfnet2,"m_ipvphysical_bin")
        df$mffIPV3<-fmean(df,mfnet3,"e_ipvphysical_bin")

        # df$cfIPV1<-fmean(df,net1,"b_ipvoutcome_cat")
        # df$cfIPV2<-fmean(df,net2,"m_ipvoutcome_cat")
        # df$cfIPV3<-fmean(df,net3,"e_ipvoutcome_cat")
        # df$ffIPV1<-fmean(df,fnet1,"b_ipvoutcome_cat")
        # df$ffIPV2<-fmean(df,fnet2,"m_ipvoutcome_cat")
        # df$ffIPV3<-fmean(df,fnet3,"e_ipvoutcome_cat")
        # df$mcfIPV1<-fmean(df,mnet1,"b_ipvoutcome_cat")
        # df$mcfIPV2<-fmean(df,mnet2,"m_ipvoutcome_cat")
        # df$mcfIPV3<-fmean(df,mnet3,"e_ipvoutcome_cat")
        # df$mffIPV1<-fmean(df,mfnet1,"b_ipvoutcome_cat")
        # df$mffIPV2<-fmean(df,mfnet2,"m_ipvoutcome_cat")
        # df$mffIPV3<-fmean(df,mfnet3,"e_ipvoutcome_cat")

    #gem
        df$cfgem1<-fmean(df,net1,"b_gem_r15_avg")
        df$cfgem2<-fmean(df,net2,"m_gem_r15_avg")
        df$cfgem3<-fmean(df,net3,"e_gem_r15_avg")
        df$ffgem1<-fmean(df,fnet1,"b_gem_r15_avg")
        df$ffgem2<-fmean(df,fnet2,"m_gem_r15_avg")
        df$ffgem3<-fmean(df,fnet3,"e_gem_r15_avg")
        df$mcfgem1<-fmean(df,mnet1,"b_gem_r15_avg")
        df$mcfgem2<-fmean(df,mnet2,"m_gem_r15_avg")
        df$mcfgem3<-fmean(df,mnet3,"e_gem_r15_avg")
        df$mffgem1<-fmean(df,mfnet1,"b_gem_r15_avg")
        df$mffgem2<-fmean(df,mfnet2,"m_gem_r15_avg")
        df$mffgem3<-fmean(df,mfnet3,"e_gem_r15_avg")

    #gender
        df$gender<-1-(df$gender-1)
        df$cfgender1<-fmean(df,net1,"gender")
        df$cfgender2<-fmean(df,net2,"gender")
        df$cfgender3<-fmean(df,net3,"gender")
        df$ffgender1<-fmean(df,fnet1,"gender")
        df$ffgender2<-fmean(df,fnet2,"gender")
        df$ffgender3<-fmean(df,fnet3,"gender")

        #df<-df[df$CONDITION==2,]
    #other
        df$cfb_testhiv_122<-fmean(df,net2,"b_testhiv")
        df$cfm_testhiv_122<-fmean(df,net2,"m_testhiv_12")
        df$cfe_testhiv_123<-fmean(df,net3,"e_testhiv_12")
        df$ffb_testhiv_122<-fmean(df,fnet2,"b_testhiv")
        df$ffm_testhiv_122<-fmean(df,fnet2,"m_testhiv_12")
        df$ffe_testhiv_123<-fmean(df,fnet3,"e_testhiv_12")
        df$mcfb_testhiv_122<-fmean(df,mnet2,"b_testhiv")
        df$mcfm_testhiv_122<-fmean(df,mnet2,"m_testhiv_12")
        df$mcfe_testhiv_123<-fmean(df,mnet3,"e_testhiv_12")
        df$mffb_testhiv_122<-fmean(df,mfnet2,"b_testhiv")
        df$mffm_testhiv_122<-fmean(df,mfnet2,"m_testhiv_12")
        df$mffe_testhiv_123<-fmean(df,mfnet3,"e_testhiv_12")

    #Alter hiv
        df$hivadv1=rowMeans(df[,c("b_hivadvf1","b_hivtstf2")],na.rm = T)
        df$hivadv2=rowMeans(df[,c("m_hivadvf1","m_hivtstf2")],na.rm = T)
        df$hivadv3=rowMeans(df[,c("e_hivadvf1","e_hivtstf2")],na.rm = T)
        df$hivinj1=rowMeans(df[,c("b_hivinjf1","b_hivtstf2")],na.rm = T)
        df$hivinj2=rowMeans(df[,c("m_hivinjf1","m_hivtstf2")],na.rm = T)
        df$hivinj3=rowMeans(df[,c("e_hivinjf1","e_hivtstf2")],na.rm = T)
        df$hivtst1=rowMeans(df[,c("b_hivtstf1","b_hivtstf2")],na.rm = T)
        df$hivtst2=rowMeans(df[,c("m_hivtstf1","m_hivtstf2")],na.rm = T)
        df$hivtst3=rowMeans(df[,c("e_hivtstf1","e_hivtstf2")],na.rm = T)

    #camp rateshiv
        df$r.hiv1<-NA
        df$r.hiv2<-NA
        df$r.hiv3<-NA
        df$r.hivtst1<-NA
        df$r.hivtst2<-NA
        df$r.hivtst3<-NA
        df$r.hivinj1<-NA
        df$r.hivinj2<-NA
        df$r.hivinj3<-NA
        df$r.hivadv1<-NA
        df$r.hivadv2<-NA
        df$r.hivadv3<-NA
        for(i in 1:nrow(df)){
          df$r.hiv1[i]<-mean(df$b_testhiv[df$camp==df$camp[i]],na.rm=T)
          df$r.hiv2[i]<-mean(df$m_testhiv_12[df$camp==df$camp[i]],na.rm=T)
          df$r.hiv3[i]<-mean(df$e_testhiv_12[df$camp==df$camp[i]],na.rm=T)
          df$r.hivtst1[i]<-mean(df$hivtst1[df$camp==df$camp[i]],na.rm=T)
          df$r.hivtst2[i]<-mean(df$hivtst2[df$camp==df$camp[i]],na.rm=T)
          df$r.hivtst3[i]<-mean(df$hivtst3[df$camp==df$camp[i]],na.rm=T)
          df$r.hivinj1[i]<-mean(df$hivinj1[df$camp==df$camp[i]],na.rm=T)
          df$r.hivinj2[i]<-mean(df$hivinj2[df$camp==df$camp[i]],na.rm=T)
          df$r.hivinj3[i]<-mean(df$hivinj3[df$camp==df$camp[i]],na.rm=T)
          df$r.hivadv1[i]<-mean(df$hivadv1[df$camp==df$camp[i]],na.rm=T)
          df$r.hivadv2[i]<-mean(df$hivadv2[df$camp==df$camp[i]],na.rm=T)
          df$r.hivadv3[i]<-mean(df$hivadv3[df$camp==df$camp[i]],na.rm=T)
        }

    #long format
        ldf<-data.frame(
          ID=rep(df$pid,3),
          respond=c(df$b_respond,df$m_respond,df$e_respond),
          wave=rep(c(1,2,3),each=nrow(df)),
          camp=rep(df$camp,3),
          ipv=c(df$b_ipvoutcome_cat,df$m_ipvoutcome_cat,df$e_ipvoutcome_cat),
          ipvt1=c(df$m_ipvoutcome_cat,df$e_ipvoutcome_cat,rep(NA,nrow(df))),
          ipvp=c(df$b_ipvphysical_bin,df$m_ipvphysical_bin,df$e_ipvphysical_bin),
          ipvpt1=c(df$m_ipvphysical_bin,df$e_ipvphysical_bin,rep(NA,nrow(df))),
          male=c(df$gender,df$gender,df$gender),
          gem=c(df$b_gem_r15_avg,df$m_gem_r15_avg,df$e_gem_r15_avg),
          gemt1=c(df$m_gem_r15_avg,df$e_gem_r15_avg,rep(NA,nrow(df))),
          drnk_frq=c(df$b_drnk_frq,df$m_drnk_frq,df$e_drnk_frq),
          alc_frq=c(df$b_alc_frq,df$m_alc_frq,df$e_alc_frq),
          sex.num=c(df$b_sxp12_num,df$m_sxp12_num,df$e_sxp12_num),
          sex.active=c(df$b_partnered,df$m_partnered,df$e_partnered),
          religious=c(df$m_relig,df$m_relig,df$e_relig),
          hiv=c(df$b_testhiv,df$m_testhiv_12,df$e_testhiv_12),
          hivt1=c(df$m_testhiv,df$e_testhiv_12,rep(NA,nrow(df))),
          AltIPV=c(df$cfIPV1,df$cfIPV2,df$cfIPV3),
          AltGEM=c(df$cfgem1,df$cfgem2,df$cfgem3),
          AltGender=c(df$cfgender1,df$cfgender2,df$cfgender3),
          AltHIV=c(df$cfb_testhiv_122,df$cfm_testhiv_122,df$cfe_testhiv_123),
          fAltIPV=c(df$ffIPV1,df$ffIPV2,df$ffIPV3),
          fAltGEM=c(df$ffgem1,df$ffgem2,df$ffgem3),
          fAltGender=c(df$ffgender1,df$ffgender2,df$ffgender3),
          fAltHIV=c(df$ffb_testhiv_122,df$ffm_testhiv_122,df$ffe_testhiv_123),
          mAltIPV=c(df$mcfIPV1,df$mcfIPV2,df$mcfIPV3),
          mAltGEM=c(df$mcfgem1,df$mcfgem2,df$mcfgem3),
          mAltHIV=c(df$mcfb_testhiv_122,df$mcfm_testhiv_122,df$mcfe_testhiv_123),
          mfAltIPV=c(df$mffIPV1,df$mffIPV2,df$mffIPV3),
          mfAltGEM=c(df$mffgem1,df$mffgem2,df$mffgem3),
          mfAltHIV=c(df$mffb_testhiv_122,df$mffm_testhiv_122,df$mffe_testhiv_123),

          hivadv=c(df$hivadv1,df$hivadv2,df$hivadv3),
          hivinj=c(df$hivinj1,df$hivinj2,df$hivinj3),
          hivtst=c(df$hivtst1,df$hivtst2,df$hivtst3),
          r.hiv=c(df$r.hiv1,df$r.hiv2,df$r.hiv3),
          r.hivtst=c(df$r.hivtst1,df$r.hivtst2,df$r.hivtst3),
          r.hivinj=c(df$r.hivinj1,df$r.hivinj2,df$r.hivinj3),
          r.hivadv=c(df$r.hivadv1,df$r.hivadv2,df$r.hivadv3),

          # hivadv=c(df$m_hivadvfalter,df$m_hivadvfalter,df$e_hivadvfalter),
          # hivstf=c(df$m_hivtstfalter,df$m_hivtstfalter,df$e_hivtstfalter),
          # hivinj=c(df$m_hivinjfalter,df$m_hivinjfalter,df$e_hivinjfalter),
          # hivstfego=c(df$m_hivtstfego,df$m_hivtstfego,df$e_hivtstfego)
          condition=rep(df$condition,3),
          ses=rep(df$b_ses,3),
          leader=rep(df$chl,3),
          married=rep(df$b_married,3),
          age=rep(df$age,3),
          fam.available=rep(df$b_ssfamavl,3),
          ward=rep(df$ward,3),
          camp.duration=rep(df$b_elig5_d,3),
          num.children=rep(df$b_child_ev,3),
          has.children=rep(ifelse(df$b_child_ev>1,1,df$b_child_ev),3),
          child.violence=rep(df$b_childvio1,3),
          educ=rep(df$b_edu_cat,3),
          child.sex.violence=rep(df$b_childvio2,3)

        )
    #More variable edits
        ldf$camp.duration2<-ifelse(ldf$camp.duration>15,15,ldf$camp.duration)
        ldf$ward<-factor(ldf$ward)
        ldf$ID<-factor(ldf$ID)
        ldf$camp<-factor(ldf$camp)
        ldf$sex.num2<-ifelse(ldf$sex.num>2,3,ldf$sex.num)
        ldf$religious[ldf$religious==0]<-1
        ldf$religious[ldf$religious>2]<-2
    }
    #descriptives
        descriptives=data.frame(Variable=names(ldf))
        descriptives$mean=0
        descriptives$median=0
        descriptives$sd=0
        descriptives$min=0
        descriptives$max=0
        for(i in 1:length(ldf)){
          if(class(ldf[,i])!="factor"){
            descriptives$mean[i]<-round(mean(ldf[,i],na.rm=T),3)
            descriptives$median[i]<-round(median(ldf[,i],na.rm=T),3)
            descriptives$sd[i]<-round(sd(ldf[,i],na.rm=T),3)
            descriptives$min[i]<-min(ldf[,i],na.rm=T)
            descriptives$max[i]<-max(ldf[,i],na.rm=T)
          }
        }

    #model prep
        library(lme4)
        library(nlme)
        library(lmerTest)
        init.model<- function(form) lmer(formula=form,data=ldf[ldf$respond==1 &ldf$male==1 & ldf$sex.num>0,])
        prstars<-function(x){
          x[,2]<-ifelse(x[,2]<.001,"***",ifelse(x[,2]<.01,"**",ifelse(x[,2]<.05,"*","")))
          return(x)
        }
        sum.model<-function(model) {
          x<-as.data.frame(round(summary(model)$coefficients,10))
          x[,c(1,ncol(x))]
        }
        allf<-function(x) prstars(sum.model(init.model(x)))

        table(ldf$sex.num==0)

    #models
        names(ldf)
        (m1<-allf(init.model(ipv~(1|ID)+(1 | camp) + gem+drnk_frq+wave+sex.active+sex.num2+married+fam.available+child.violence+AltIPV:AltGender)))
        (m1<-allf(init.model(ipv~(1|ID)+(1 | camp) + drnk_frq+sex.active+sex.num2+child.violence+AltIPV:AltGender))) #this is close friend
        (m1<-allf(init.model(ipv~(1|ID)+(1 | camp) + drnk_frq+sex.active+sex.num2+child.violence+mAltIPV))) #this is close friend
        (m1<-allf(init.model(ipv~(1|ID)+(1 | camp) + drnk_frq+sex.active+sex.num2+child.violence+mfAltIPV))) #this is close friend

        (m1<-allf(init.model(ipvt1~(1|ID)+(1 | camp) + gem+drnk_frq+wave+sex.active+sex.num2+married+fam.available+child.violence)))
        (m1<-allf(init.model(ipvt1~(1|ID)+(1 | camp) + drnk_frq+sex.active+sex.num2+child.violence+mfAltIPV))) #this is close friend


        (m1<-allf(init.model(ipvp~ (1|ID) + (1|wave) +(1 | camp) + age + educ + married + sex.num2 + child.violence + child.sex.violence+alc_frq+gem)))
        (m1<-allf(init.model(ipvpt1~ (1|ID) + (1|wave) +(1 | camp) + age + educ + married + sex.num2 + child.violence + child.sex.violence+alc_frq+gem)))

        (m1<-allf(init.model(ipvp~ (1|ID) + (1|wave) +(1 | camp) + age + educ + married + sex.num2 + child.violence + child.sex.violence+alc_frq+gem + mfAltIPV)))
        (m1<-allf(init.model(ipvp~ (1|ID) + (1|wave) +(1 | camp) + age + educ + married + sex.num2 + child.violence + child.sex.violence+alc_frq+gem + mAltIPV)))
        (m1<-allf(init.model(ipvpt1~ (1|ID) + (1|wave) +(1 | camp) + age + educ + married + sex.num2 + child.violence + child.sex.violence+alc_frq+gem + mfAltIPV)))
        (m1<-allf(init.model(ipvpt1~ (1|ID) + (1|wave) +(1 | camp) + age + educ + married + sex.num2 + child.violence + child.sex.violence+alc_frq+gem + mAltIPV)))


        (m1<-allf(init.model(ipv~ (1|ID) + (1|wave) +(1 | camp) + age + educ + married + sex.num2 + child.violence + child.sex.violence+alc_frq+gem + mfAltIPV)))
        (m1<-allf(init.model(ipv~ (1|ID) + (1|wave) +(1 | camp) + age + educ + married + sex.num2 + child.violence + child.sex.violence+alc_frq+gem + mAltIPV)))
        (m1<-allf(init.model(ipvt1~ (1|ID) + (1|wave) +(1 | camp) + age + educ + married + sex.num2 + child.violence + child.sex.violence+alc_frq+gem + mfAltIPV)))
        (m1<-allf(init.model(ipvt1~ (1|ID) + (1|wave) +(1 | camp) + age + educ + married + sex.num2 + child.violence + child.sex.violence+alc_frq+gem + mAltIPV)))


        # (m1<-allf(init.model(gem~ (1|ID) + (1|wave) +(1 | camp) + age + educ + married + sex.num2 + child.violence + child.sex.violence+alc_frq+gem + mfAltGEM)))
        # (m1<-allf(init.model(gemt1~ (1|ID) + (1|wave) +(1 | camp) + age + educ + married + sex.num2 + child.violence + child.sex.violence+alc_frq + mfAltGEM)))

        ldf$gem

        # (m1<-allf(init.model(gem~(1|ID)+(1 | camp) + gem+drnk_frq+wave+sex.active+sex.num2+married+fam.available+child.violence+AltGEM:AltGender))) #NOPE



    #mimic marta's models
        init.model<- function(form) lmer(formula=form,data=ldf[ldf$respond==1 &ldf$male==1 & ldf$wave==1  & ldf$sex.num>0,])
        ldf$ipv
        summary(glm(ipvp~ + age + educ + married + sex.num2 + child.violence + child.sex.violence+alc_frq+gem,
            data=ldf[ldf$respond==1 &ldf$male==1 & ldf$wave==1  & ldf$sex.num>0,],family=binomial(link = "logit")))

        (m1<-allf(init.model(ipvp~ (1 | camp) + age + educ + married + sex.num2 + child.violence + child.sex.violence+alc_frq+gem)))

        (m1<-allf(init.model(ipv~ + age + educ + married + sex.num2 + child.violence + child.sex.violence+alc_frq+gem)))

        ldf$child.violence
        ldf$alc_frq
        df$alc

        nrow(ldf[ldf$respond==1 &ldf$male==1 & ldf$wave==1 & ldf$sex.num>0,])
        table(ldf$sex.num>0)




    #HIV
        (m1<-allf(init.model(hiv~ (1|ID) + (1|wave) +(1 | camp) + age + educ + married + sex.num2 + child.violence + child.sex.violence+alc_frq+gem +AltHIV)))
        (m1<-allf(init.model(hivt1~ (1|ID) + (1|wave) +(1 | camp) + age + educ + married + sex.num2 + child.violence + child.sex.violence+alc_frq+gem +AltHIV)))

        (m1<-allf(init.model(hiv~ (1|ID) + (1|wave) +(1 | camp) + age + educ + married + sex.num2 + child.violence + child.sex.violence+alc_frq+gem +hivtst)))
        (m1<-allf(init.model(hivt1~ (1|ID) + (1|wave) +(1 | camp) + age + educ + married + sex.num2 + child.violence + child.sex.violence+alc_frq+gem +hivtst)))

        (m1<-allf(init.model(hiv~ (1|ID) + (1|wave) +(1 | camp) + age + educ + married + sex.num2 + child.violence + child.sex.violence+alc_frq+gem +hivinj)))
        (m1<-allf(init.model(hivt1~ (1|ID) + (1|wave) +(1 | camp) + age + educ + married + sex.num2 + child.violence + child.sex.violence+alc_frq+gem +hivinj)))

        (m1<-allf(init.model(hiv~ (1|ID) + (1|wave) +(1 | camp) + age + educ + married + sex.num2 + child.violence + child.sex.violence+alc_frq+gem +hivadv)))
        (m1<-allf(init.model(hivt1~ (1|ID) + (1|wave) +(1 | camp) + age + educ + married + sex.num2 + child.violence + child.sex.violence+alc_frq+gem +hivadv)))

        (m1<-allf(init.model(hiv~ (1|ID) + (1|wave) +(1 | camp) + age + educ + married + sex.num2 + child.violence + child.sex.violence+alc_frq+gem +r.hivtst)))
        (m1<-allf(init.model(hivt1~ (1|ID) + (1|wave) +(1 | camp) + age + educ + married + sex.num2 + child.violence + child.sex.violence+alc_frq+gem +r.hivtst)))

        (m1<-allf(init.model(hiv~ (1|ID) + (1|wave) +(1 | camp) + age + educ + married + sex.num2 + child.violence + child.sex.violence+alc_frq+gem +r.hivinj)))
        (m1<-allf(init.model(hivt1~ (1|ID) + (1|wave) +(1 | camp) + age + educ + married + sex.num2 + child.violence + child.sex.violence+alc_frq+gem +r.hivinj)))

        (m1<-allf(init.model(hiv~ (1|ID) + (1|wave) +(1 | camp) + age + educ + married + sex.num2 + child.violence + child.sex.violence+alc_frq+gem +r.hivadv)))
        (m1<-allf(init.model(hivt1~ (1|ID) + (1|wave) +(1 | camp) + age + educ + married + sex.num2 + child.violence + child.sex.violence+alc_frq+gem +r.hivadv)))



        (m1<-allf(init.model(hiv~ (1|ID) + (1|wave) +(1 | camp) + age + educ + married + sex.num2 + child.violence + child.sex.violence+alc_frq+gem +hivtst+hivinj+hivadv)))
        (m1<-allf(init.model(hivt1~ (1|ID) + (1|wave) +(1 | camp) + age + educ + married + sex.num2 + child.violence + child.sex.violence+alc_frq+gem +hivtst+hivinj+hivadv)))
        (m1<-allf(init.model(hiv~ (1|ID) + (1|wave) +(1 | camp) + age + educ + married + sex.num2 + child.violence + child.sex.violence+alc_frq+gem +r.hivtst+r.hivinj+r.hivadv)))
        (m1<-allf(init.model(hivt1~ (1|ID) + (1|wave) +(1 | camp) + age + educ + married + sex.num2 + child.violence + child.sex.violence+alc_frq+gem +r.hivtst+r.hivinj+r.hivadv)))

        (m1<-allf(init.model(hiv~ (1|ID) + (1|wave) +(1 | camp) + age + educ + married + sex.num2 + child.violence + child.sex.violence+alc_frq+gem +hivtst+hivinj+hivadv+r.hivtst+r.hivinj+r.hivadv)))
        (m1<-allf(init.model(hivt1~ (1|ID) + (1|wave) +(1 | camp) + age + educ + married + sex.num2 + child.violence + child.sex.violence+alc_frq+gem +hivtst+hivinj+hivadv+r.hivtst+r.hivinj+r.hivadv)))

