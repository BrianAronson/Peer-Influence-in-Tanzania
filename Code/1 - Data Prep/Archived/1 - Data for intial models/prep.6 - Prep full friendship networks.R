source(file.path(code.prep.dir, "prep.2 - Load data.R"))

#5 - prep Full friendship networks
{
    #a - merge each network with that edge list so that we have 0s for non ties
        #i - no longer any need to merge in camp info
            fnet1$camp <- NULL
            fnet2$CAMP <- NULL
            fnet3$CAMP <- NULL
        #ii - rename to match names in dt
            names(fnet1) <- c("ID1","ID2","RELP")
            names(fnet2) <- c("ID1","ID2","RELP")
            names(fnet3) <- c("ID1","ID2","RELP")
        #iii - Due to error in raw data, make sure to remove duplicate edges, and use shortest RELP
            #fnet1
                fnet1 <- fnet1[order(fnet1$RELP),]
                a <- paste(fnet1$ID1,fnet1$ID2)
                b <- duplicated(a)
                fnet1 <- fnet1[!b,]
            #fnet2
                fnet2 <- fnet2[order(fnet2$RELP),]
                a <- paste(fnet2$ID1,fnet2$ID2)
                b <- duplicated(a)
                fnet2 <- fnet2[!b,]
            #fnet3
                fnet3 <- fnet3[order(fnet3$RELP),]
                a <- paste(fnet3$ID1,fnet3$ID2)
                b <- duplicated(a)
                fnet3 <- fnet3[!b,]
        #iV - convert fnets to datatables to speed up merges
            fnet1 <- data.table(fnet1)
            fnet2 <- data.table(fnet2)
            fnet3 <- data.table(fnet3)
        #v - create a full edge list consisting of all possible ties
            #Create full list of respondents
            dt <- data.table(ID1=rep(df2$pid,each=nrow(df2)),ID2=rep(df2$pid,nrow(df2)),Camp1=rep(df2$camp,each=nrow(df2)),Camp2=rep(df2$camp,nrow(df2)))
            fnet1 <- merge(fnet1,dt,all.y=T)
            fnet2 <- merge(fnet2,dt,all.y=T)
            fnet3 <- merge(fnet3,dt,all.y=T)
    #b - just focus on RELP = 1; convert the rest to 0s.
        fnet1$RELP <- ifelse(is.na(fnet1$RELP),0,ifelse(fnet1$RELP==1,1,0))
        fnet2$RELP <- ifelse(is.na(fnet2$RELP),0,ifelse(fnet2$RELP==1,1,0))
        fnet3$RELP <- ifelse(is.na(fnet3$RELP),0,ifelse(fnet3$RELP==1,1,0))
    #c - eliminate cross camp ties
            fnet1 <- fnet1[fnet1$Camp1==fnet1$Camp2,]
            fnet2 <- fnet2[fnet2$Camp1==fnet2$Camp2,]
            fnet3 <- fnet3[fnet3$Camp1==fnet3$Camp2,]    
#create different versions of fnet for when fnet = dv
    dv.fnet1 <- fnet1
    dv.fnet2 <- fnet2
    dv.fnet3 <- fnet3
# For fnet (not dv)
    #a - remove leavers and nonrespondents
        #ii - leavers
            fnet2$RELP <- ifelse(fnet2$ID1 %in% leavers2 | fnet2$ID2 %in% leavers2,0,fnet2$RELP)
            fnet3$RELP <- ifelse(fnet3$ID1 %in% leavers3 | fnet3$ID2 %in% leavers3,0,fnet3$RELP)
        #iii - pretend non responders are not in the fnetwork
            fnet1 <- fnet1[!(fnet1$ID1 %in% nonresponders) & !(fnet1$ID2 %in% nonresponders),]
            fnet2 <- fnet2[!(fnet2$ID1 %in% nonresponders) & !(fnet2$ID2 %in% nonresponders),]
            fnet3 <- fnet3[!(fnet3$ID1 %in% nonresponders) & !(fnet3$ID2 %in% nonresponders),]
        #iv - pretend leavers could not be nominated
            fnet2$RELP <- ifelse(fnet2$ID2 %in% leavers2,0,fnet2$RELP)
            fnet3$RELP <- ifelse(fnet3$ID2 %in% leavers3,0,fnet3$RELP)
    #b - divide ties by total outdegree so that individuals who nominate too many alters are still useful and so that they don't screw things up for the rest.
            divides <- function(x){
                temp1 <- as.data.table(table(x$ID1[x$RELP==1]))
                names(temp1)[1] <- "ID1"
                temp1$ID1 <- as.numeric(temp1$ID1)
                x <- merge(x,temp1,by="ID1",all=T)
                x$N[is.na(x$N)] <- 1
                x$N[x$N<3] <- 3
                x$RELP <- x$RELP/x$N
                return(x)
            }
            fnet1 <- divides(fnet1)
            fnet2 <- divides(fnet2)
            fnet3 <- divides(fnet3)
    #c - set actual ties to within-person mean to remove collinearity - this isn't quite right... the mean shouldn't include within person ties...
            temp <- fnet1[,.(means=mean(RELP)),by=.(ID1)]
            fnet1 <- merge(temp,fnet1)
            temp <- fnet2[,.(means=mean(RELP)),by=.(ID1)]
            fnet2 <- merge(temp,fnet2)
            temp <- fnet3[,.(means=mean(RELP)),by=.(ID1)]
            fnet3 <- merge(temp,fnet3)
            fnet1$RELP[net1$tie==1] <- fnet1$means[net1$tie==1]
            fnet2$RELP[net2$tie==1] <- fnet2$means[net2$tie==1]
            fnet3$RELP[net3$tie==1] <- fnet3$means[net3$tie==1]
            
# For both fnet and dv            
    #a - Create separate matrices and arrays for each camp
        #i - split fnetworks into lists by camps
            fnet1 <- split(fnet1,fnet1$Camp1)
            fnet2 <- split(fnet2,fnet2$Camp1)
            fnet3 <- split(fnet3,fnet3$Camp1)
            dv.fnet1 <- split(dv.fnet1,dv.fnet1$Camp1)
            dv.fnet2 <- split(dv.fnet2,dv.fnet2$Camp1)
            dv.fnet3 <- split(dv.fnet3,dv.fnet3$Camp1)
            
        #ii - convert each camp edge list to a matrix
            for(i in 1:length(fnet1)){
              fnet1[[i]] <- matrix(fnet1[[i]]$RELP,nrow = sqrt(nrow(fnet1[[i]])),ncol = sqrt(nrow(fnet1[[i]])),byrow=T)
              fnet2[[i]] <- matrix(fnet2[[i]]$RELP,nrow = sqrt(nrow(fnet2[[i]])),ncol = sqrt(nrow(fnet2[[i]])),byrow=T)
              fnet3[[i]] <- matrix(fnet3[[i]]$RELP,nrow = sqrt(nrow(fnet3[[i]])),ncol = sqrt(nrow(fnet3[[i]])),byrow=T)
              dv.fnet1[[i]] <- matrix(dv.fnet1[[i]]$RELP,nrow = sqrt(nrow(dv.fnet1[[i]])),ncol = sqrt(nrow(dv.fnet1[[i]])),byrow=T)
              dv.fnet2[[i]] <- matrix(dv.fnet2[[i]]$RELP,nrow = sqrt(nrow(dv.fnet2[[i]])),ncol = sqrt(nrow(dv.fnet2[[i]])),byrow=T)
              dv.fnet3[[i]] <- matrix(dv.fnet3[[i]]$RELP,nrow = sqrt(nrow(dv.fnet3[[i]])),ncol = sqrt(nrow(dv.fnet3[[i]])),byrow=T)
            }
            
# For fnet
    #iii - To keep syntax consistant with closest friends, rename as arrays
        farrayl1 <- fnet1
        farrayl2 <- fnet2
        farrayl3 <- fnet3
    #v - Append the remaining arrays
        farrayl <- c(farrayl1,farrayl2)
    #vi - convert to siena objects
        for(i in 1:length(farrayl)){
          farrayl[[i]] <- coDyadCovar(farrayl[[i]])
        }
        ffriendship <- farrayl
            
# For dv
    #i - Treat each two-wave pair as a separate array.
        dv.farrayl1 <- list()
        dv.farrayl2 <- list()
        for(i in 1:length(dv.fnet1)){
          dv.farrayl1[[i]] <- array(c(dv.fnet1[[i]],dv.fnet2[[i]]), dim = c(nrow(dv.fnet1[[i]]),ncol(dv.fnet1[[i]]), 2))
          dv.farrayl2[[i]] <- array(c(dv.fnet2[[i]],dv.fnet3[[i]]), dim = c(nrow(dv.fnet1[[i]]),ncol(dv.fnet1[[i]]), 2))
        }
    #iii - Append the remaining arrays
        dv.farrayl <- c(dv.farrayl1,dv.farrayl2)
    #iv - convert to siena objects
        for(i in 1:length(arrayl)){
          dv.farrayl[[i]] <- sienaNet(dv.farrayl[[i]],allowOnly = F)
        }
        dv.ffriendship <- dv.farrayl
        
}

