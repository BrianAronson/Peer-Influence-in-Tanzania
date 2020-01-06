#Appendices
    #ap.1 - Search for Outliers
        #Sender distribution
            a<-as.data.frame(table(net1$ID1[net1$tie==1]))
            summary(a$Freq)
            c<-as.data.frame(table(net2$ID1[net2$tie==1]))
            summary(c$Freq)
            e<-as.data.frame(table(net3$ID1[net3$tie==1]))
            summary(e$Freq)
        #Receiver distribution
            b<-as.data.frame(table(net1$ID2[net1$tie==1]))
            summary(b$Freq)
            d<-as.data.frame(table(net2$ID2[net2$tie==1]))
            summary(d$Freq)
            f<-as.data.frame(table(net3$ID2[net3$tie==1]))
            summary(f$Freq)
        #reciprocity distribution
            a<-net1[net1$tie==1,]
            b<-a
            names(b)[c(1,2)]<-c("ID2","ID1")
            a<-rbind(a,b)
            c<-paste(a$ID1,a$ID2)
            d<-duplicated(c)
            a<-as.data.frame(a)
            a<-a[d,]
            b<-as.data.frame(table(a$ID1[a$tie==1]))
            summary(b$Freq)

    #ap.2 - Get report about data
        print01Report(data = dataset[[1]], modelname = 'stuff')

    #ap.3 - Estimate jaccard indices
        #a - create a function for measuring jaccard
            jaccard <- function(df) {
                M_00 <- apply(df, 1, sum) == 0
                M_11 <- apply(df, 1, sum) == 2
                df <- df[!M_00,]
                JSim <- sum(M_11) / nrow(df)
                return(c(JSim = JSim))
            }
        #b - get jaccards for each network
            jdf<-as.data.frame(matrix(nrow=length(net1),ncol=4))
            for(i in 1:length(net1)){
              #create a df containing two columns, one for matrix 1, another for matrix 2
                  temp<-data.frame(v1=c(net1[[i]]),v2=c(net2[[i]]))
                  temp2<-data.frame(v1=c(net2[[i]]),v2=c(net3[[i]]))
              #set 10s to 0
                  temp[,1][temp[,1]==10]<-0
                  temp[,2][temp[,2]==10]<-0
                  temp2[,1][temp2[,1]==10]<-0
                  temp2[,2][temp2[,2]==10]<-0
              #calculate jaccards
                  jdf[i,1]<-jaccard(temp)
                  jdf[i,2]<-jaccard(temp2)
              #grab network sizes too
                  jdf[i,3]<-nrow(net1[[i]])
              #and info about missingness
                  jdf[i,4]<-sum(is.na(df[[i]]$B_IPVOutcome_CAT))
            }
            mean(jdf$V1)
            sd(jdf$V1)
            mean(jdf$V2)
            sd(jdf$V2)
            jdf$V5<-rowMeans(jdf[,c(1,2)])
            jdf$V6<-ifelse(jdf$V1>.2 & jdf$V2>.2,1,0)

      #ap.4 - Estimate density
          density<-as.data.frame(matrix(nrow=length(net1),ncol=3))
            for(i in 1:length(net1)){
              temp<-c(net1[[i]])
              temp[temp==10]<-0
              density[i,1]<-mean(temp)
              temp<-c(net2[[i]])
              temp[temp==10]<-0
              density[i,2]<-mean(temp)
              temp<-c(net3[[i]])
              temp[temp==10]<-0
              density[i,3]<-mean(temp)
            }
            mean(density$V1)
            sd(density$V1)
            mean(density$V2)
            sd(density$V2)
            mean(density$V3)
            sd(density$V3)

      #ap.5 - Determine overall maximum convergence ratio and maximum t-ratio
            Model1$tconv.max
            Model1$tmax

      #ap.6 - Estimate GoF
            #Test Goodness of Fit
            {
            (indegreefit <- sienaGOF(Model1, verbose=TRUE, varName="friendship", IndegreeDistribution)) #MHD= p=
              plot(indegreefit)
            (outdegreefit <- sienaGOF(Model1, verbose=TRUE, varName="friendship", OutdegreeDistribution)) #MHD= p=0
              plot(outdegreefit)
            }
            (GeodesicFit <- sienaGOF(Model1, verbose=TRUE, varName="friendship", GeodesicDistribution)) #MHD= p=

      #ap.7 - Degree by missingness
            a<-as.data.frame(table(net2$ID1))
            b<-as.data.frame(table(net2$ID2))
            names(a)<-c("PID","outdegree")
            names(b)<-c("PID","indegree")
            df2<-merge(df,a,all = T)
            df2<-merge(df2,b,all = T)
            df2$B_Respond[is.na(df2$B_Respond)]<-0
            df2$outdegree[is.na(df2$outdegree)]<-0
            df2$indegree[is.na(df2$indegree)]<-0
            #non respondents send very few ties
                summary(df2$outdegree[df2$B_Respond==0])
                summary(df2$outdegree[df2$B_Respond==1])
            #non respondents received fewer ties
                summary(df2$indegree[df2$B_Respond==0])
                summary(df2$indegree[df2$B_Respond==1])
            #for later waves...
                #non respondents send very few ties
                    summary(df2$outdegree[df2$B_Respond==0])
                    summary(df2$outdegree[df2$M_Respond==1])
                #non respondents received fewer ties
                    summary(df2$indegree[df2$B_Respond==0])
                    summary(df2$indegree[df2$M_Respond==1])



      #ap.8 - Fixed effects for IPV
          source("prep.2 - Load data.R")
          source("prep.3 - Prep network data.R")
          #create function to get mean friend info
              fmean<-function(x,y,z){
                  df2<-data.table(x[,c("pid",z)])
                  names(df2)<-c("ID2","a")
                  a<-y[y$tie==1,]
                  b<-merge(a,df2,by="ID2")
                  c<-b[,.(means=mean(a)),by=.(ID1)]
                  names(df2)[1]<-"ID1"
                  df2<-merge(df2,c,by="ID1",all=T)
                  return(df2$means)
              }
          #IPV
              df$cfIPV1<-fmean(df,net1,"b_ipvoutcome_bin")
              df$cfIPV2<-fmean(df,net2,"m_ipvoutcome_bin")
              df$cfIPV3<-fmean(df,net3,"e_ipvoutcome_bin")
          #gem
              df$cfgem1<-fmean(df,net1,"b_gem_r15_avg")
              df$cfgem2<-fmean(df,net2,"m_gem_r15_avg")
              df$cfgem3<-fmean(df,net3,"e_gem_r15_avg")
          #gender
              df$gender<-1-(df$gender-1)
              df$cfgender1<-fmean(df,net1,"gender")
              df$cfgender2<-fmean(df,net2,"gender")
              df$cfgender3<-fmean(df,net3,"gender")
              #df<-df[df$CONDITION==2,]
          # #other
              df$cfb_testhiv_122<-fmean(df,net2,"b_testhiv")
              df$cfm_testhiv_122<-fmean(df,net2,"m_testhiv_12")
              df$cfe_testhiv_123<-fmean(df,net3,"e_testhiv_12")
          
          #full friendship variables
              dv.ffriendship[[i]]
              
          
          #long format
              ldf<-data.frame(
                ID=rep(df$pid,3),
                respond=c(df$b_respond,df$m_respond,df$e_respond),
                wave=rep(c(1,2,3),each=nrow(df)),
                camp=rep(df$camp,3),
                ipv=c(df$b_ipvoutcome_cat,df$m_ipvoutcome_cat,df$e_ipvoutcome_cat),
                male=c(df$gender,df$gender,df$gender),
                gem=c(df$b_gem_r15_avg,df$m_gem_r15_avg,df$e_gem_r15_avg),
                drnk_frq=c(df$b_drnk_frq,df$m_drnk_frq,df$e_drnk_frq),
                alc_frq=c(df$b_alc_frq,df$m_alc_frq,df$e_alc_frq),
                sex.num=c(df$b_sxp12_num,df$m_sxp12_num,df$e_sxp12_num),
                sex.active=c(df$b_partnered,df$m_partnered,df$e_partnered),
                religious=c(df$m_relig,df$m_relig,df$e_relig),
                hiv=c(df$b_testhiv,df$m_testhiv_12,df$e_testhiv_12),
                AltIPV=c(df$cfIPV1,df$cfIPV2,df$cfIPV3),
                AltGEM=c(df$cfgem1,df$cfgem2,df$cfgem3),
                AltGender=c(df$cfgender1,df$cfgender2,df$cfgender3),
                AltHIV=c(df$cfb_testhiv_122,df$cfm_testhiv_122,df$cfe_testhiv_123),
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
                child.violence=rep(df$b_childvio1,3)
              )
          #More variable edits
              ldf$camp.duration2<-ifelse(ldf$camp.duration>15,15,ldf$camp.duration)
              ldf$ward<-factor(ldf$ward)
              ldf$ID<-factor(ldf$ID)
              ldf$camp<-factor(ldf$camp)
              ldf$sex.num2<-ifelse(ldf$sex.num>3,4,ldf$sex.num)
              ldf$religious[ldf$religious==0]<-1
              ldf$religious[ldf$religious>2]<-2
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
              init.model<- function(form) lmer(formula=form,data=ldf[ldf$respond==1 &ldf$male==1,])
              prstars<-function(x){
                x[,2]<-ifelse(x[,2]<.001,"***",ifelse(x[,2]<.01,"**",ifelse(x[,2]<.05,"*","")))
                return(x)
              }
              sum.model<-function(model) {
                x<-as.data.frame(round(summary(model)$coefficients,10))
                x[,c(1,ncol(x))]
              }
              allf<-function(x) prstars(sum.model(init.model(x)))


          #models
              names(ldf)
              (m1<-allf(init.model(ipv~(1|ID)+(1 | camp) + gem+drnk_frq+wave+sex.active+sex.num2+married+fam.available+child.violence+AltIPV:AltGender)))
              (m1<-allf(init.model(ipv~(1|ID)+(1 | camp) + drnk_frq+sex.active+sex.num2+child.violence+AltIPV:AltGender)))
              (m5<-allf(init.model(ipv~(1 | ID)+female )))
              summary(ldf$respond)

          names(ldf)
              a$coefficients
              (m1<-summary(plm(hiv~AltMarried+AltAge+AltGender+AltHIV, data=ldf, index=c("ID", "wave"), model="random")))
              (m2<-summary(plm(hiv~AltMarried+AltAge+AltGender+hivadv, data=ldf, index=c("ID", "wave"), model="within")))
              (m3<-summary(plm(hiv~AltMarried+AltAge+AltGender+hivstf, data=ldf, index=c("ID", "wave"), model="within")))
              (m4<-summary(plm(hiv~AltMarried+AltAge+AltGender+hivinj, data=ldf, index=c("ID", "wave"), model="within")))
              (m5<-summary(plm(hiv~AltMarried+AltAge+AltGender+hivstfego, data=ldf, index=c("ID", "wave"), model="within")))
              (m6<-summary(plm(hiv~AltMarried+AltAge+AltGender+AltHIV+hivadv+hivstf+hivinj, data=ldf, index=c("ID", "wave"), model="within")))
              model(m1$coefficients)
              model(m2$coefficients)
              model(m3$coefficients)
              model(m4$coefficients)
              model(m5$coefficients)
              model(m6$coefficients)
              
              library(plm)    
          #create a long format df
              # ldf<-data.frame(
              #     ID=rep(df$PID,3),
              #     condition=rep(df$CONDITION,3),
              #     IPV=c(df$B_IPVOutcome_Bin,df$M_IPVOutcome_Bin,df$E_IPVOutcome_Bin),
              #     IPV2=c(df$M_IPVOutcome_Bin,df$E_IPVOutcome_Bin,df$E_IPVOutcome_Bin),
              #     Camp=rep(df$CAMP,3),
              #     wave=rep(c(1,2,3),each=nrow(df)),
              #     gem=c(df$B_GEM_R15_AVG,df$M_GEM_R15_AVG,df$E_GEM_R15_AVG),
              #     gem2=c(df$M_GEM_R15_AVG,df$E_GEM_R15_AVG,df$E_GEM_R15_AVG),
              #     AltIPV=c(df$fIPV1,df$fIPV2,df$fIPV3),
              #     AltGEM=c(df$fgem1,df$fgem2,df$fgem3),
              #     AltGender=c(df$fgender1,df$fgender2,df$fgender3))
              

          library(plm)
              # summary(plm(IPV~AltIPV, data=ldf, index=c("ID", "wave"), model="random"))
              # summary(plm(IPV~AltGender, data=ldf, index=c("ID", "wave"), model="random"))
              # summary(plm(IPV~AltGEM, data=ldf, index=c("ID", "wave"), model="random")) #hmmmm...
              # summary(plm(gem~AltGEM, data=ldf, index=c("ID", "wave"), model="random"))
              # summary(plm(gem~AltGender, data=ldf, index=c("ID", "wave"), model="random")) #
              # summary(glm(IPV2~IPV+AltIPV,data=ldf[ldf$wave!=3,])) #
              # summary(glm(IPV2~IPV+AltGender,data=ldf[ldf$wave!=3,]))
              # summary(glm(IPV2~IPV+AltGEM,data=ldf[ldf$wave!=3,])) #
              # summary(glm(gem2~gem+AltGender,data=ldf[ldf$wave!=3,])) #
              # summary(glm(gem2~gem+AltGEM,data=ldf[ldf$wave!=3,]))
              
              
#Check jaccards in DV

#ii - Estimate average and sd change in behavior DV; requires df not transformed yet
    df <- read.sas7bdat("widebehavioraldata_20180206.sas7bdat")
    df<-df[!is.na(df$B_Respond),]
    twaveindex<-waveindex
    twaveindex$campname<-sapply(camp, "[[", 1)
    bdf<-twaveindex
    bdf$mean<-NA
    bdf$absmean<-NA
    bdf$w1mean<-NA
    bdf$w2mean<-NA
    bdf$sd<-NA
    bdf$N<-NA
    bdf$pmiss<-NA
    for(i in 1:nrow(twaveindex)){
        if(twaveindex[i,1]==1){
          #grab DV for those waves and camps
              temp<-data.frame(v1=c(df$B_GEM_R15_AVG[df$CAMP==twaveindex[i,3]]),v2=c(df$M_GEM_R15_AVG[df$CAMP==twaveindex[i,3]]))
        }else{
              temp<-data.frame(v1=c(df$M_GEM_R15_AVG[df$CAMP==twaveindex[i,3]]),v2=c(df$E_GEM_R15_AVG[df$CAMP==twaveindex[i,3]]))
    }
    #estimate change in behaviors
        temp$v3<-temp$v1-temp$v2
        bdf[i,4]<-mean(temp$v3,na.rm = T)
        bdf[i,5]<-mean(abs(temp$v3),na.rm = T)
        bdf[i,6]<-mean(temp$v1,na.rm = T)
        bdf[i,7]<-mean(temp$v2,na.rm = T)
        bdf[i,8]<-sd(temp$v3,na.rm = T)
    #get N
        bdf[i,9]<-sum(df$CAMP==twaveindex[i,3])
    #get % missing
        bdf[i,10]<-sum(is.na(temp$v3))/sum(df$CAMP==twaveindex[i,3])
  }
    #bring in parameter rate estimates
    bdf<-bdf[-c(3,21),]
        bdf$estimates<-Model$theta[grepl("^rate DV",Model$effects$effectName)]
        bdf$whacko<-ifelse(bdf$estimates>50,1,0)
        View(bdf)

        a<-vector()
        for(i in 1:ncol(bdf)-2){
            a[i]<-paste(names(bdf)[i],mean(bdf[bdf$whacko==1,][,i],na.rm=T),mean(bdf[bdf$whacko==0,][,i],na.rm=T),sep=", ")
        }





              