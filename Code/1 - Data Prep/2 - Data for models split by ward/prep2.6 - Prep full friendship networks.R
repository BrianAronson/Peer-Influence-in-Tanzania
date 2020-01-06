#5 - prep Full friendship networks
{
    #a - merge each network with that edge list so that we have 0s for non ties
        #i - no longer any need to merge in camp info
            fnet1$camp<-NULL
            fnet2$CAMP<-NULL
        #ii - rename to match names in dt
            names(fnet1)<-c("ID1","ID2","RELP")
            names(fnet2)<-c("ID1","ID2","RELP")
        #iii - Due to error in raw data, make sure to remove duplicate edges, and use shortest RELP
            #fnet1
                fnet1<-fnet1[order(fnet1$RELP),]
                a<-paste(fnet1$ID1,fnet1$ID2)
                b<-duplicated(a)
                fnet1<-fnet1[!b,]
            #fnet2
                fnet2<-fnet2[order(fnet2$RELP),]
                a<-paste(fnet2$ID1,fnet2$ID2)
                b<-duplicated(a)
                fnet2<-fnet2[!b,]
        #iV - convert fnets to datatables to speed up merges
            fnet1<-data.table(fnet1)
            fnet2<-data.table(fnet2)
        #v - merge with full edge list and remove ties that aren't in df
            fnet1<-merge(fnet1,dt,all.y=T)
            fnet2<-merge(fnet2,dt,all.y=T)
    #b - just focus on RELP = 1; convert the rest to 0s.
        fnet1$RELP<-ifelse(is.na(fnet1$RELP),0,ifelse(fnet1$RELP==1,1,0))
        fnet2$RELP<-ifelse(is.na(fnet2$RELP),0,ifelse(fnet2$RELP==1,1,0))
    #c - set impossible ties to 0s
        #i - cross camp ties
            fnet1$RELP<-ifelse(fnet1$Camp1!=fnet1$Camp2,0,fnet1$RELP)
            fnet2$RELP<-ifelse(fnet2$Camp1!=fnet2$Camp2,0,fnet2$RELP)
        #ii - leavers
            fnet2$RELP<-ifelse(fnet2$ID1 %in% leavers2 | fnet2$ID2 %in% leavers2,0,fnet2$RELP)
        #iii - pretend non responders are not in the fnetwork
            fnet1<-fnet1[!(fnet1$ID1 %in% nonresponders) & !(fnet1$ID2 %in% nonresponders),]
            fnet2<-fnet2[!(fnet2$ID1 %in% nonresponders) & !(fnet2$ID2 %in% nonresponders),]
        #iv - pretend leavers could not be nominated
            fnet2$RELP<-ifelse(fnet2$ID2 %in% leavers2,0,fnet2$RELP)
        #v - remove cross camp ties from data.tables
            fnet1<-fnet1[fnet1$Ward1==fnet1$Ward2,]
            fnet2<-fnet2[fnet2$Ward1==fnet2$Ward2,]
    #d - divide ties by total outdegree so that individuals who nominate too many alters are still useful and so that they don't screw things up for the rest.
            divides<-function(x){
                temp1<-as.data.table(table(x$ID1[x$RELP==1]))
                names(temp1)[1]<-"ID1"
                temp1$ID1<-as.numeric(temp1$ID1)
                x<-merge(x,temp1,by="ID1",all=T)
                x$N[is.na(x$N)]<-1
                x$N[x$N<3]<-3
                x$RELP<-x$RELP/x$N
                return(x)
            }
            fnet1<-divides(fnet1)
            fnet2<-divides(fnet2)
    #e - set actual ties to within-person mean to remove collinearity - this isn't quite right... the mean shouldn't include within person ties...
            temp<-fnet1[,.(means=mean(RELP)),by=.(ID1)]
            fnet1<-merge(temp,fnet1)
            temp<-fnet2[,.(means=mean(RELP)),by=.(ID1)]
            fnet2<-merge(temp,fnet2)
            fnet1$RELP[net1$tie==1]<-fnet1$means[net1$tie==1]
            fnet2$RELP[net2$tie==1]<-fnet2$means[net2$tie==1]
    #f - Create separate matrices and arrays for each camp (FOR META-ANALYSIS METHOD ONLY)
        
            #kill bad data
            fnet1<-fnet1[fnet1$Camp1 %in% camps1 & fnet1$Camp2 %in% camps1,]
            fnet2<-fnet2[fnet2$Camp1 %in% camps2 & fnet2$Camp2 %in% camps2,]
            
        #i - split fnetworks into lists by camps
            fnet1<-split(fnet1,fnet1$Ward1)
            fnet2<-split(fnet2,fnet2$Ward1)
        #ii - convert each camp edge list to a matrix
            for(i in 1:length(fnet1)){
              fnet1[[i]]<-matrix(fnet1[[i]]$RELP,nrow = sqrt(nrow(fnet1[[i]])),ncol = sqrt(nrow(fnet1[[i]])),byrow=T)
            }
            for(i in 1:length(fnet2)){
              fnet2[[i]]<-matrix(fnet2[[i]]$RELP,nrow = sqrt(nrow(fnet2[[i]])),ncol = sqrt(nrow(fnet2[[i]])),byrow=T)
            }
        #iii - To keep syntax consistant with closest friends, rename as arrays
            farrayl1<-fnet1
            farrayl2<-fnet2
        #v - Append the remaining arrays
            farrayl<-c(farrayl1,farrayl2)
        #vi - convert to siena objects
            for(i in 1:length(farrayl)){
              farrayl[[i]]<-coDyadCovar(farrayl[[i]])
            }
            ffriendship<-farrayl
}
