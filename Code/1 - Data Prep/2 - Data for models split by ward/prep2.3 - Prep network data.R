#3 - Prep network data
{
    #b - create edgelist based on data in df
        #i - Convert friend covariates to edgelists
            net1<-data.frame(ID1=rep(df$pid,3),ID2=c(df$b_frnd1pid,df$b_frnd2pid,df$b_frnd3pid),tie=1)
            net2<-data.frame(ID1=rep(df$pid,3),ID2=c(df$m_frnd1pid,df$m_frnd2pid,df$m_frnd3pid),tie=1)
            net3<-data.frame(ID1=rep(df$pid,3),ID2=c(df$e_frnd1pid,df$e_frnd2pid,df$e_frnd3pid),tie=1)
        #ii - remove NAs
            net1<-net1[!is.na(net1$ID2),]
            net2<-net2[!is.na(net2$ID2),]
            net3<-net3[!is.na(net3$ID2),]
        #iii - remove duplicate edges (a few people nominated the same person as their first and second closest friend)
            a<-duplicated(paste(net1$ID1,net1$ID2))
            net1<-net1[!a,]
            a<-duplicated(paste(net2$ID1,net2$ID2))
            net2<-net2[!a,]
            a<-duplicated(paste(net3$ID1,net3$ID2))
            net3<-net3[!a,]
    #c - have empty edges for non-ties
        #i - Create an empty edge list with all possible (and impossible) ties
            dt<-data.table(ID1=rep(df$pid,each=nrow(df)),ID2=rep(df$pid,nrow(df)),Camp1=rep(df$camp,each=nrow(df)),Camp2=rep(df$camp,nrow(df)),Ward1=rep(df$ward,each=nrow(df)),Ward2=rep(df$ward,nrow(df)))
        #ii - convert nets to data table
            net1<-data.table(net1)
            net2<-data.table(net2)
            net3<-data.table(net3)
        #iii - merge
            net1<-merge(net1,dt,all.y=T)
            net2<-merge(net2,dt,all.y=T)
            net3<-merge(net3,dt,all.y=T)
        #iv - set null ties to 0
            net1$tie<-ifelse(is.na(net1$tie),0,net1$tie)
            net2$tie<-ifelse(is.na(net2$tie),0,net2$tie)
            net3$tie<-ifelse(is.na(net3$tie),0,net3$tie)
    #d - set impossible ties to structural 0s (10)
        #i - cross camp ties
            net1$tie<-ifelse(net1$Camp1!=net1$Camp2,10,net1$tie)
            net2$tie<-ifelse(net2$Camp1!=net2$Camp2,10,net2$tie)
            net3$tie<-ifelse(net3$Camp1!=net3$Camp2,10,net3$tie)
        #ii - non responders
            nonresponders<-df$pid[df$b_respond==0]
            net1$tie<-ifelse(net1$ID1 %in% nonresponders,10,net1$tie)
            net2$tie<-ifelse(net2$ID1 %in% nonresponders,10,net2$tie)
            net3$tie<-ifelse(net3$ID1 %in% nonresponders,10,net3$tie)
        #iii - leavers
            leavers2<-df$pid[df$m_respond==0]
            net2$tie<-ifelse(net2$ID1 %in% leavers2,10,net2$tie)
            leavers3<-df$pid[df$e_respond==0]
            net3$tie<-ifelse(net3$ID1 %in% leavers3,10,net3$tie)
        #iv - self loops
            net1$tie[net1$ID1==net1$ID2]<-0
            net2$tie[net2$ID1==net2$ID2]<-0
            net3$tie[net3$ID1==net3$ID2]<-0
        #(optional) - pretend non responders are not in the network
            net1<-net1[!(net1$ID1 %in% nonresponders) & !(net1$ID2 %in% nonresponders),]
            net2<-net2[!(net2$ID1 %in% nonresponders) & !(net2$ID2 %in% nonresponders),]
            net3<-net3[!(net3$ID1 %in% nonresponders) & !(net3$ID2 %in% nonresponders),]
        
        #######OPTIONAL##########   
        # pretend leavers could not be nominated
            net2$tie<-ifelse(net2$ID2 %in% leavers2,10,net2$tie)
            net3$tie<-ifelse(net3$ID2 %in% leavers3,10,net3$tie)
        #########################
            
    #e - Create separate matrices and arrays for each camp
        #i - remove cross ward ties from data.tables
            net1<-net1[net1$Ward1==net1$Ward2,]
            net2<-net2[net2$Ward1==net2$Ward2,]
            net2.5<-net2
            net3<-net3[net3$Ward1==net3$Ward2,]
        #ia - for summary stats; save net info as separate name
            sumnet1<-net1
            sumnet2<-net2
            sumnet2<-net2.5
            sumnet3<-net3
        #kill bad data
            # keeps<-sumstats2[sumstats2$N>10 & sumstats2$jaccard>.15,c("Camp","Wave")]
            camps1<-keeps$Camp[keeps$Wave==1]
            camps2<-keeps$Camp[keeps$Wave==3]
            net1<-net1[net1$Camp1 %in% camps1 & net1$Camp2 %in% camps1,]
            net2<-net2[net2$Camp1 %in% camps1 & net2$Camp2 %in% camps1,]
            net2.5<-net2.5[net2.5$Camp1 %in% camps2 & net2.5$Camp2 %in% camps2,]
            net3<-net3[net3$Camp1 %in% camps2 & net3$Camp2 %in% camps2,]
          
        #ii - split networks into lists by camps
            nnet1<-split(net1,net1$Ward1)
            nnet2<-split(net2,net2$Ward1)
            nnet2.5<-split(net2.5,net2.5$Ward1)
            nnet3<-split(net3,net3$Ward1)
        #iii - convert each camp edge list to a matrix
            for(i in 1:length(nnet1)){
              nnet1[[i]]<-matrix(nnet1[[i]]$tie,nrow = sqrt(nrow(nnet1[[i]])),ncol = sqrt(nrow(nnet1[[i]])),byrow=T)
              nnet2[[i]]<-matrix(nnet2[[i]]$tie,nrow = sqrt(nrow(nnet2[[i]])),ncol = sqrt(nrow(nnet2[[i]])),byrow=T)
            }
            for(i in 1:length(nnet2.5)){
                nnet2.5[[i]]<-matrix(nnet2.5[[i]]$tie,nrow = sqrt(nrow(nnet2.5[[i]])),ncol = sqrt(nrow(nnet2.5[[i]])),byrow=T)
                nnet3[[i]]<-matrix(nnet3[[i]]$tie,nrow = sqrt(nrow(nnet3[[i]])),ncol = sqrt(nrow(nnet3[[i]])),byrow=T)
            }
              
                        
    #g - create data for siena model; remove waves with high heterogeneity
        #i - Treat each two-wave pair as a separate array.
            arrayl1<-list()
            arrayl2<-list()
            for(i in 1:length(nnet1)){
              arrayl1[[i]]<-array(c(nnet1[[i]],nnet2[[i]]), dim = c(nrow(nnet1[[i]]),ncol(nnet1[[i]]), 2))
            }
            for(i in 1:length(nnet2.5)){
              arrayl2[[i]]<-array(c(nnet2.5[[i]],nnet3[[i]]), dim = c(nrow(nnet3[[i]]),ncol(nnet3[[i]]), 2))
            }
        #iii - Append the remaining arrays
            arrayl<-c(arrayl1,arrayl2)
        #iv - convert to siena objects
            for(i in 1:length(arrayl)){
              arrayl[[i]]<-sienaNet(arrayl[[i]])
            }
            friendship<-arrayl
}
