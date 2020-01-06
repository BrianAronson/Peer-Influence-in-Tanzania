#Create subset criteria
# {
# #1 - Get Ns and jaccard indices
#     #i - Jaccard function
#         jaccard <- function(df) {
#             M_00 <- apply(df, 1, sum) == 0
#             M_11 <- apply(df, 1, sum) == 2
#             df <- df[!M_00,]
#             JSim <- sum(M_11) / nrow(df)
#             return(c(JSim = JSim))
#         }
#     #ii - Get Jaccards
#         sumstats<-as.data.frame(matrix(nrow=max(length(nnet1),length(nnet3)),ncol=3))
#         for(i in 1:length(nnet1)){
#           #create a df containing two columns, one for matrix 1, another for matrix 2
#               temp<-data.frame(v1=c(nnet1[[i]]),v2=c(nnet2[[i]]))
#               temp2<-data.frame(v1=c(nnet2.5[[i]]),v2=c(nnet3[[i]]))
#           #set 10s to 0
#               temp[,1][temp[,1]==10]<-0
#               temp[,2][temp[,2]==10]<-0
#               temp2[,1][temp2[,1]==10]<-0
#               temp2[,2][temp2[,2]==10]<-0
#           #get campname
#               sumstats[i,1]<-dfl[[i]]$ward[1]
#           #get N
#               sumstats[i,2]<-nrow(nnet1[[i]])
#               sumstats[i,3]<-nrow(nnet3[[i]])
#           #calculate jaccards
#               sumstats[i,4]<-jaccard(temp)
#               sumstats[i,5]<-jaccard(temp2)
#         }
#         names(sumstats)<-c("Ward","N","N2","Jaccard.12","Jaccard.23")
#         
# #2 - Get rates of missingness, rates of DV change, assortativity, rates of DV occurrence
#     #prep ego alter correlation table with df of ego alter traits
#         tdf1<-sumnet1[sumnet1$tie==1,]
#         tdf2<-sumnet2[sumnet2$tie==1,]
#         tdf3<-sumnet3[sumnet3$tie==1,]
#         tempdf<-data.table(df)
#         names(tempdf)<-paste("s_",names(df),sep="")
#         names(tempdf)[1]<-"ID1"
#         tdf1<-merge(tdf1,tempdf,by="ID1",all.x=T)
#         tdf2<-merge(tdf2,tempdf,by="ID1",all.x=T)
#         tdf3<-merge(tdf3,tempdf,by="ID1",all.x=T)
#         tempdf<-data.table(df)
#         names(tempdf)<-paste("r_",names(df),sep="")
#         names(tempdf)[1]<-"ID2"
#         tdf1<-merge(tdf1,tempdf,by="ID2",all.x=T)
#         tdf2<-merge(tdf2,tempdf,by="ID2",all.x=T)
#         tdf3<-merge(tdf3,tempdf,by="ID2",all.x=T)
#         tdf1<-split(tdf1,by="Ward1")
#         tdf2<-split(tdf2,by="Ward1")
#         tdf3<-split(tdf3,by="Ward1")
#     #grab stats
#     tryCatch({
#     capture.output({
#         sumstats$missing.2<-1-sapply(dfl,function(x) sum(x$m_respond))/sumstats$N
#         sumstats$missing.3<-1-sapply(dfl,function(x) sum(x$e_respond))/sumstats$N
#         sumstats$d.ipv.12<-sapply(dfl,function(x) cor(x$b_ipvoutcome_cat,x$m_ipvoutcome_cat,use="complete"))
#         sumstats$d.ipv.23<-sapply(dfl,function(x) cor(x$m_ipvoutcome_cat,x$e_ipvoutcome_cat,use="complete"))
#         sumstats$r.ipv.1<-sapply(dfl,function(x) sum(x$b_ipvoutcome_cat>0,na.rm=T))/sumstats$N
#         sumstats$r.ipv.2<-sapply(dfl,function(x) sum(x$m_ipvoutcome_cat>0,na.rm=T))/sumstats$N
#         sumstats$r.ipv.3<-sapply(dfl,function(x) sum(x$e_ipvoutcome_cat>0,na.rm=T))/sumstats$N
#         sumstats$alt.ipv.1<-sapply(tdf1,function(x) cor(x$r_b_ipvoutcome_cat,x$s_b_ipvoutcome_cat,use="complete"))
#         sumstats$alt.ipv.2<-sapply(tdf2,function(x) cor(x$r_m_ipvoutcome_cat,x$s_m_ipvoutcome_cat,use="complete"))
#         sumstats$alt.ipv.3<-sapply(tdf3,function(x) cor(x$r_e_ipvoutcome_cat,x$s_e_ipvoutcome_cat,use="complete"))
#         sumstats$d.gem.12<-sapply(dfl,function(x) cor(x$b_gem_r15_avg,x$m_gem_r15_avg,use="complete"))
#         sumstats$d.gem.23<-sapply(dfl,function(x) cor(x$m_gem_r15_avg,x$e_gem_r15_avg,use="complete"))
#         sumstats$r.gem.1<-sapply(dfl,function(x) mean(x$b_gem_r15_avg,na.rm=T))
#         sumstats$r.gem.2<-sapply(dfl,function(x) mean(x$m_gem_r15_avg,na.rm=T))
#         sumstats$r.gem.3<-sapply(dfl,function(x) mean(x$e_gem_r15_avg,na.rm=T))
#         sumstats$alt.gem.1<-sapply(tdf1,function(x) cor(x$r_b_gem_r15_avg,x$s_b_gem_r15_avg,use="complete"))
#         sumstats$alt.gem.2<-sapply(tdf2,function(x) cor(x$r_m_gem_r15_avg,x$s_m_gem_r15_avg,use="complete"))
#         sumstats$alt.gem.3<-sapply(tdf3,function(x) cor(x$r_e_gem_r15_avg,x$s_e_gem_r15_avg,use="complete"))
#         sumstats$d.hiv.12<-sapply(dfl,function(x) cor(x$b_testhiv_12,x$m_testhiv_12,use="complete"))
#         sumstats$d.hiv.23<-sapply(dfl,function(x) cor(x$m_testhiv_12,x$e_testhiv_12,use="complete"))
# 
#         sumstats$d.alc.12<-sapply(dfl,function(x) cor(x$b_alc_frq,x$m_alc_frq,use="complete"))
#         sumstats$d.alc.23<-sapply(dfl,function(x) cor(x$m_alc_frq,x$e_alc_frq,use="complete"))
# 
#         sumstats$r.hiv.1<-sapply(dfl,function(x) sum(x$b_testhiv_12>0,na.rm=T))/sumstats$N
#         sumstats$r.hiv.2<-sapply(dfl,function(x) sum(x$m_testhiv_12>0,na.rm=T))/sumstats$N
#         sumstats$r.hiv.3<-sapply(dfl,function(x) sum(x$e_testhiv_12>0,na.rm=T))/sumstats$N
#         sumstats$r.hivtst.1<-sapply(dfl,function(x) mean(x$b_hivtstfalter,na.rm=T))
#         sumstats$r.hivtst.2<-sapply(dfl,function(x) mean(x$m_hivtstfalter,na.rm=T))
#         sumstats$r.hivtst.3<-sapply(dfl,function(x) mean(x$e_hivtstfalter,na.rm=T))
#         sumstats$r.hivinj.1<-sapply(dfl,function(x) mean(x$b_hivinjfalter,na.rm=T))
#         sumstats$r.hivinj.2<-sapply(dfl,function(x) mean(x$m_hivinjfalter,na.rm=T))
#         sumstats$r.hivinj.3<-sapply(dfl,function(x) mean(x$e_hivinjfalter,na.rm=T))
#         sumstats$r.hivadv.1<-sapply(dfl,function(x) mean(x$b_hivadvfalter,na.rm=T))
#         sumstats$r.hivadv.2<-sapply(dfl,function(x) mean(x$m_hivadvfalter,na.rm=T))
#         sumstats$r.hivadv.3<-sapply(dfl,function(x) mean(x$e_hivadvfalter,na.rm=T))
#         sumstats$alt.hiv.1<-sapply(tdf1,function(x) cor(x$r_b_testhiv_12,x$s_b_testhiv_12,use="complete"))
#         sumstats$alt.hiv.2<-sapply(tdf2,function(x) cor(x$r_m_testhiv_12,x$s_m_testhiv_12,use="complete"))
#         sumstats$alt.hiv.3<-sapply(tdf3,function(x) cor(x$r_e_testhiv_12,x$s_e_testhiv_12,use="complete"))
#         sumstats$density.1<-sapply(nnet1,function(x) sum(x==1)/(nrow(x)*(nrow(x)-1))/2)
#         sumstats$density.2<-sapply(nnet2,function(x) sum(x==1)/(nrow(x)*(nrow(x)-1))/2)
#         sumstats$density.3<-sapply(nnet3,function(x) sum(x==1)/(nrow(x)*(nrow(x)-1))/2)
#         sumstats<-apply(sumstats,2,function(x) round(x,2))
#         sumstats[is.na(sumstats)]<-0
# 
#     })
# }, error=function(e){})
# }
# #3. Fit sumstat criteria into data better format
#     sumstats<-as.data.frame(sumstats)
# tryCatch({
#     sumstats2<-rbind(sumstats,sumstats)
#     sumstats2$d.ipv<-c(sumstats$d.ipv.12,sumstats$d.ipv.23)
#     sumstats2$d.gem<-c(sumstats$d.gem.12,sumstats$d.gem.23)
#     sumstats2$d.hiv<-c(sumstats$d.hiv.12,sumstats$d.hiv.23)
#     sumstats2$d.alc<-c(sumstats$d.alc.12,sumstats$d.alc.23)
#     sumstats2$missing<-c(sumstats$missing.2,sumstats$missing.3)
#     sumstats2$jaccard<-c(sumstats$Jaccard.12,sumstats$Jaccard.23)
#     sumstats2$N1<-sumstats2$N
#     sumstats2$Wave<-c(rep(1,4),rep(3,4))
#     sumstats2$density<-c(sumstats$density.1,sumstats$density.2)
#     sumstats2$r.hiv<-c(sumstats$r.hiv.1,sumstats$r.hiv.2)
#     sumstats2$r.hivtst<-c(sumstats$r.hivtst.1,sumstats$r.hivtst.2)
#     sumstats2$r.hivinj<-c(sumstats$r.hivinj.1,sumstats$r.hivinj.2)
#     sumstats2$r.hivadv<-c(sumstats$r.hivadv.1,sumstats$r.hivadv.2)
#     sumstats2<-sumstats2[,c(ncol(sumstats2)-(1:ncol(sumstats2))+1)]
# }, error=function(e){})

#Create objects to save
    varlist<-list(dv.all_ipv, dv.ipvoutcome_cat,dv.ipvoutcome_bin,dv.gem_r15_avg,dv.ipvphysical_cat,dv.testhiv_12,friendship,ffriendship,hivtstf,hivinjf,hivadvf,knowf,closef,sex,age,edu,married,ses,campn,ward1,ward2,ward3,ward4,condition,leader,gem,wave,sex.active,sex.year,child.violence,child.sex.violence,camp.duration,has.children,fam.available,hivtst.ego,hivinjf.ego,hivadvf.ego,knowf.ego,closef.ego,ever.test,ever.ipv,dv.ipvpsych_cat,alc_evr,alc_frq,alc_bing,drnk_frq,hivtst.alter,hivinjf.alter,hivadvf.alter,hivtst.group,sex.num,dv.alc_freq)
    names(varlist)<-c("dv.all_ipv","dv.ipvoutcome_cat","dv.ipvoutcome_bin","dv.gem_r15_avg","dv.ipvphysical_cat","dv.testhiv_12","friendship","ffriendship","hivtstf","hivinjf","hivadvf","knowf","closef","sex","age","edu","married","ses","campn","ward1","ward2","ward3","ward4","condition","leader","gem","wave","sex.active","sex.year","child.violence","child.sex.violence","camp.duration","has.children","fam.available","hivtst.ego","hivinjf.ego","hivadvf.ego","knowf.ego","closef.ego","ever.test","ever.ipv","dv.ipvpsych_cat","alc_evr","alc_frq","alc_bing","drnk_frq","hivtst.alter","hivinjf.alter","hivadvf.alter","hivtst.group","sex.num","dv.alc_freq")
    # saveRDS(sumstats,"2sumstats.rds")
    # saveRDS(sumstats2,"2sumstats2.rds")
    saveRDS(varlist,"2varlist.rds")
    