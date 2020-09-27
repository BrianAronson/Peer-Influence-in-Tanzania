#10 - create more camp level objects,
#create new camp objects based on campn object for convenience
# N1<-campn
# jaccard<-campn
# missing<-campn
# d.ipv<-campn
# d.hiv<-campn
# r.hiv<-campn
# density<-campn
# r.hivtst<-campn
# r.hivinj<-campn
# r.hivadv<-campn
# #For each element in list
# for(i in 1:length(campn)){
#   N1[[i]][]<-sumstats2$N1[i]
#   jaccard[[i]][]<-sumstats2$jaccard[i]
#   missing[[i]][]<-sumstats2$missing[i]
#   d.ipv[[i]][]<-sumstats2$d.ipv[i]
#   d.hiv[[i]][]<-sumstats2$d.hiv[i]
#   r.hiv[[i]][]<-sumstats2$r.hiv[i]
#   density[[i]][]<-sumstats2$density[i]
#   r.hivtst[[i]][]<-sumstats2$r.hivtst[i]
#   r.hivinj[[i]][]<-sumstats2$r.hivinj[i]
#   r.hivadv[[i]][]<-sumstats2$r.hivadv[i]
#   
# }
#assign new vars to list and same
sumvars<-list(N1,jaccard,missing,d.ipv,d.hiv,r.hiv,density,r.hivtst,r.hivinj,r.hivadv)
names(sumvars)<-c("N1","jaccard","missing","d.ipv","d.hiv","r.hiv","density","r.hivtst","r.hivinj","r.hivadv")
saveRDS(sumvars,"2sumvars.rds")
#saveRDS(camplist,"camplist.rds")
