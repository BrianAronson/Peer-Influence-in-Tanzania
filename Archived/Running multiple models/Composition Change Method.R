changes<-list()
lcomp1<-list()
lcomp2<-list()
for(j in 1:length(friendship)){
    a<-array(as.matrix(friendship[[j]]),dim = dim(friendship[[j]]))
    b<-ifelse(diag(a[,,1])==10,2,1)
    c<-ifelse(diag(a[,,2])==10,1,2)
    kill<-ifelse(b==2 & c==1,T,F)
    b<-b[!kill]
    c<-c[!kill]
    comp<-vector(length = length(b),mode="list")
    for(i in 1:length(comp)){
      comp[[i]]<-c(b[i],c[i])
    }
    lcomp1[[j]]<-b
    lcomp2[[j]]<-c
    changes[[j]] <- sienaCompositionChange(comp)
}

