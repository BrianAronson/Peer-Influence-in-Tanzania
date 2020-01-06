{
#3.5 - prep all other dyadic covariates
  # df$b_hivtstf1 #think friend has been tested
  # df$b_hivinjf1 #thinks friend thinks that he should have a test
  # df$b_hivadvf1 #friend encouraged ego to get tested
  # df$b_knowf1 #friend known length (inverse)
  # df$b_closef1 #friend close (inverse)
  #a - create list of dyadic variables (results include phony 3rd alter average effect)

      dyadicv1<-list()
      dyadicv2<-list()
      tarrayl<-list()
      tfriendship<-list()
      dyvars<-c("hivtstf","hivinjf","hivadvf","knowf","closef")
  #b - create ego and alter variables based on dyads
    for(q in 1:length(dyvars)){
        vnames<-c("b_","m_","e_")
        for(i in 1:length(vnames)){
            temp1<-data.frame(ID1=rep(df$pid,3),ID2=c(df[,paste(vnames[i],"frnd1pid",sep="")],df[,paste(vnames[i],"frnd2pid",sep="")],df[,paste(vnames[i],"frnd3pid",sep="")]),tie=c(df[,paste(vnames[i],dyvars[q],"1",sep="")],df[,paste(vnames[i],dyvars[q],"2",sep="")],rowMeans(df[,c(paste(vnames[i],dyvars[q],"1",sep=""),paste(vnames[i],dyvars[q],"2",sep=""))])))
            temp1<-temp1[!is.na(temp1$ID2),]
            a<-duplicated(paste(temp1$ID1,temp1$ID2))
            temp1<-temp1[!a,]
            temp1<-data.table(temp1)
            temp2<-data.table(ID1=df$pid,ID2=df$pid)
            meanalt<-temp1[,.(means=mean(tie)),by=.(ID1)]
            meanego<-temp1[,.(means=mean(tie)),by=.(ID2)]
            meanalt<-merge(temp2,meanalt,all.x = T)
            meanego<-merge(temp2,meanego,all.x = T)
            df[,paste(vnames[i],dyvars[q],"alter",sep="")]<-meanalt$means #monadic=behavior of outdegrees; dyadic=alter's perceived behavior
            df[,paste(vnames[i],dyvars[q],"ego",sep="")]<-meanego$means #monadic=behavior of indegrees; dyadic=ego's perceived behavior
        }
  #c - create edgelist based on data in df
      #i - temporarily impute missing info
          tempdf<-df
          #find NAs
              NA1<-is.na(tempdf[,c(paste("b_",dyvars[q],"1",sep=""))])
              NA2<-is.na(tempdf[,c(paste("b_",dyvars[q],"2",sep=""))])
              NA21<-is.na(tempdf[,c(paste("m_",dyvars[q],"1",sep=""))])
              NA22<-is.na(tempdf[,c(paste("m_",dyvars[q],"2",sep=""))])
          #determine row means
              rmeans1<-rowMeans(tempdf[,c(paste("b_",dyvars[q],"1",sep=""),paste("b_",dyvars[q],"2",sep=""))],na.rm=T)
              rmeans2<-rowMeans(tempdf[,c(paste("m_",dyvars[q],"1",sep=""),paste("m_",dyvars[q],"2",sep=""))],na.rm=T)
          #impute missing info with row means
              tempdf[,c(paste("b_",dyvars[q],"1",sep=""))][NA1]<-rmeans1[NA1]
              tempdf[,c(paste("b_",dyvars[q],"2",sep=""))][NA2]<-rmeans1[NA2]
              tempdf[,c(paste("b_",dyvars[q],"3",sep=""))]<-rmeans1
              tempdf[,c(paste("m_",dyvars[q],"1",sep=""))][NA21]<-rmeans1[NA21]
              tempdf[,c(paste("m_",dyvars[q],"2",sep=""))][NA22]<-rmeans1[NA22]
              tempdf[,c(paste("m_",dyvars[q],"3",sep=""))]<-rmeans2
          #impute remaining missing info with means
              means1<-mean(unlist(tempdf[,c(paste("b_",dyvars[q],"1",sep=""),paste("b_",dyvars[q],"2",sep=""),paste("b_",dyvars[q],"3",sep=""))]),na.rm=T)
              means2<-mean(unlist(tempdf[,c(paste("m_",dyvars[q],"1",sep=""),paste("m_",dyvars[q],"2",sep=""),paste("m_",dyvars[q],"3",sep=""))]),na.rm=T)
              tempdf[,c(paste("b_",dyvars[q],"1",sep=""),paste("b_",dyvars[q],"2",sep=""),paste("b_",dyvars[q],"3",sep=""))][is.na(tempdf[,c(paste("b_",dyvars[q],"1",sep=""),paste("b_",dyvars[q],"2",sep=""),paste("b_",dyvars[q],"3",sep=""))])]<-means1
              tempdf[,c(paste("m_",dyvars[q],"1",sep=""),paste("m_",dyvars[q],"2",sep=""),paste("m_",dyvars[q],"3",sep=""))][is.na(tempdf[,c(paste("m_",dyvars[q],"1",sep=""),paste("m_",dyvars[q],"2",sep=""),paste("m_",dyvars[q],"3",sep=""))])]<-means2
      #i - Create df of tie traits
          tnet1<-data.frame(ID1=rep(tempdf$pid,3),ID2=c(tempdf$b_frnd1pid,tempdf$b_frnd2pid,tempdf$b_frnd3pid),tie=c(tempdf[,paste("b_",dyvars[q],"1",sep="")],tempdf[,paste("b_",dyvars[q],"2",sep="")],tempdf[,paste("b_",dyvars[q],"3",sep="")]))
          tnet2<-data.frame(ID1=rep(tempdf$pid,3),ID2=c(tempdf$m_frnd1pid,tempdf$m_frnd2pid,tempdf$m_frnd3pid),tie=c(tempdf[,paste("m_",dyvars[q],"1",sep="")],tempdf[,paste("m_",dyvars[q],"2",sep="")],tempdf[,paste("m_",dyvars[q],"3",sep="")]))
      #ii - Remove ties that don't exist
          exist1<-net1[net1$tie==1,]
          exist1<-paste(exist1$ID1,exist1$ID2)
          keep1<-paste(tnet1$ID1,tnet1$ID2)
          exist2<-net2[net2$tie==1,]
          exist2<-paste(exist2$ID1,exist2$ID2)
          keep2<-paste(tnet2$ID1,tnet2$ID2)
          tnet1<-tnet1[keep1%in%exist1,]
          tnet2<-tnet2[keep2%in%exist2,]
      #iii - remove duplicate edges (a few people nominated the same person as their first and second closest friend)
          a<-duplicated(paste(tnet1$ID1,tnet1$ID2))
          tnet1<-tnet1[!a,]
          a<-duplicated(paste(tnet2$ID1,tnet2$ID2))
          tnet2<-tnet2[!a,]
  #d - create empty edges for non-ties
      #i - create data.table
          dt2<-data.table(ID1=rep(df$pid,each=nrow(df)),ID2=rep(df$pid,nrow(df)),Camp1=rep(df$camp,each=nrow(df)),Camp2=rep(df$camp,nrow(df)))
      #ii - convert tnets to data table
          tnet1<-data.table(tnet1)
          tnet2<-data.table(tnet2)
      #iii - merge
          tnet1<-merge(tnet1,dt2,all.y=T)
          tnet2<-merge(tnet2,dt2,all.y=T)
      #iv - set null ties to 0
          tnet1$tie<-ifelse(is.na(tnet1$tie),0,tnet1$tie)
          tnet2$tie<-ifelse(is.na(tnet2$tie),0,tnet2$tie)

  #f - Create separate matrices and arrays for each camp
      #i - remove cross camp ties from data.tables
          tnet1<-tnet1[tnet1$Camp1==tnet1$Camp2,]
          tnet2<-tnet2[tnet2$Camp1==tnet2$Camp2,]
      #i2 - save as temporary object for summary stats
          dyadicv1[[q]]<-tnet1
          dyadicv2[[q]]<-tnet2
      #ii - split tnetworks into lists by camps
          ntnet1<-split(tnet1,tnet1$Camp1)
          ntnet2<-split(tnet2,tnet2$Camp1)
      #iii - convert each camp edge list to a matrix
          for(i in 1:length(ntnet1)){
            ntnet1[[i]]<-matrix(ntnet1[[i]]$tie,nrow = sqrt(nrow(ntnet1[[i]])),ncol = sqrt(nrow(ntnet1[[i]])),byrow=T)
            ntnet2[[i]]<-matrix(ntnet2[[i]]$tie,nrow = sqrt(nrow(ntnet2[[i]])),ncol = sqrt(nrow(ntnet2[[i]])),byrow=T)
          }
      #iv - To keep syntax consistant with closest friends, rename as arrays
          tarrayl1<-ntnet1
          tarrayl2<-ntnet2
      #vi - Append the remaining arrays
          tarrayl<-c(tarrayl1,tarrayl2)
      #vii - convert to siena objects
          for(i in 1:length(tarrayl)){
            tarrayl[[i]]<-coDyadCovar(tarrayl[[i]])
          }
          tfriendship[[q]]<-tarrayl
    }
      hivtstf<-tfriendship[[1]]
      hivinjf<-tfriendship[[2]]
      hivadvf<-tfriendship[[3]]
      knowf<-tfriendship[[4]]
      closef<-tfriendship[[5]]
}