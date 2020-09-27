#merge camp level info with individual level
    sumstats$camp<-sumstats$Camp
    tdf<-merge(df,sumstats,by="camp")

#observe some correlations
    cor(tdf$r.hiv.2,tdf$m_testhiv_12,use="complete")
    cor(tdf$r.hivtst.2,tdf$m_hivtstfalter,use="complete")
    cor(tdf$r.hivinj.2,tdf$m_hivinjfalter,use="complete")
    cor(tdf$r.hivadv.2,tdf$m_hivadvfalter,use="complete")

#does anything mediate these correlations?
    cor(tdf$r.hivinj.2[tdf$density.2<.05],tdf$m_hivinjfalter[tdf$density.2<.05],use="complete")

#what is correlated with changes in hiv testing?
    tdf$d.testhiv_12<-tdf$e_testhiv_12-tdf$m_testhiv_12
    for(i in 1:ncol(tdf)){
      if(substr(names(tdf)[i],1,1)=="m"){
      print(paste(round(cor(tdf$d.testhiv_12,tdf[,i],use="complete"),2),names(tdf)[i]))  
      }
    }

#what in wave 2 is correlated with hiv testing in wave 3?
    for(i in 1:ncol(tdf)){
      if(substr(names(tdf)[i],1,1)=="m"){
        print(paste(round(cor(tdf$e_testhiv_12,tdf[,i],use="complete"),2),names(tdf)[i]))  
      }
    }
    
    mean(tdf$b_hivadvfalter,na.rm = T)
    mean(tdf$m_hivadvfalter,na.rm = T)
    mean(tdf$e_hivadvfalter,na.rm = T)
    cor(tdf$e_testhiv_12,tdf$b_testhiv,use="complete")
    summary(tdf$e_hivadvfalter[tdf$condition==2])


#density slightly moderates grou/alter correlations
    tdf2<-data.frame(hivadvf=c(tdf$b_hivadvfalter,tdf$m_hivadvfalter,tdf$e_hivadvfalter),id=c(tdf$pid,tdf$pid,tdf$pid),condition=c(tdf$condition,tdf$condition,tdf$condition))
    tdf2$wave=rep(c(1,2,3),each=nrow(tdf))
    tdf2$condition<-2-tdf2$condition
    tdf2$condition[tdf2$wave==1]<-0
