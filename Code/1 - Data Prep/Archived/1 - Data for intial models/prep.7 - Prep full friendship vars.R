#7 - Find averages for all friendship clusters on dvs
    library(igraph)
    library(cluster)

    
#1 - create versions of nnet and dfl with all possible ties (including nonrespondents)
    #a - nnet
          #b - create edgelist based on data in df
              #i - Convert friend covariates to edgelists
                  altnet1 <- data.frame(ID1=rep(df2$pid,3),ID2=c(df2$b_frnd1pid,df2$b_frnd2pid,df2$b_frnd3pid),tie=1)
                  altnet2 <- data.frame(ID1=rep(df2$pid,3),ID2=c(df2$m_frnd1pid,df2$m_frnd2pid,df2$m_frnd3pid),tie=1)
                  altnet3 <- data.frame(ID1=rep(df2$pid,3),ID2=c(df2$e_frnd1pid,df2$e_frnd2pid,df2$e_frnd3pid),tie=1)
              #ii - remove NAs
                  altnet1 <- altnet1[!is.na(altnet1$ID2),]
                  altnet2 <- altnet2[!is.na(altnet2$ID2),]
                  altnet3 <- altnet3[!is.na(altnet3$ID2),]
              #iii - remove duplicate edges (a few people nominated the same person as their first and second closest friend)
                  a <- duplicated(paste(altnet1$ID1,altnet1$ID2))
                  altnet1 <- altnet1[!a,]
                  a <- duplicated(paste(altnet2$ID1,altnet2$ID2))
                  altnet2 <- altnet2[!a,]
                  a <- duplicated(paste(altnet3$ID1,altnet3$ID2))
                  altnet3 <- altnet3[!a,]
          #c - have empty edges for non-ties
              #i - Create an empty edge list with all possible (and impossible) ties
                  dt <- data.table(ID1=rep(df2$pid,each=nrow(df2)),ID2=rep(df2$pid,nrow(df2)),Camp1=rep(df2$camp,each=nrow(df2)),Camp2=rep(df2$camp,nrow(df2)))
              #ii - convert altnets to data table
                  altnet1 <- data.table(altnet1)
                  altnet2 <- data.table(altnet2)
                  altnet3 <- data.table(altnet3)
              #iii - merge
                  altnet1 <- merge(altnet1,dt,all.y=T)
                  altnet2 <- merge(altnet2,dt,all.y=T)
                  altnet3 <- merge(altnet3,dt,all.y=T)
              #iv - set null ties to 0
                  altnet1$tie <- ifelse(is.na(altnet1$tie),0,altnet1$tie)
                  altnet2$tie <- ifelse(is.na(altnet2$tie),0,altnet2$tie)
                  altnet3$tie <- ifelse(is.na(altnet3$tie),0,altnet3$tie)
          #d - set impossible ties to 0
              #i - cross camp ties
                  altnet1$tie <- ifelse(altnet1$Camp1!=altnet1$Camp2,0,altnet1$tie)
                  altnet2$tie <- ifelse(altnet2$Camp1!=altnet2$Camp2,0,altnet2$tie)
                  altnet3$tie <- ifelse(altnet3$Camp1!=altnet3$Camp2,0,altnet3$tie)
              #iv - self loops
                  altnet1$tie[altnet1$ID1==altnet1$ID2] <- 0
                  altnet2$tie[altnet2$ID1==altnet2$ID2] <- 0
                  altnet3$tie[altnet3$ID1==altnet3$ID2] <- 0
              
          #e - Create separate matrices and arrays for each camp
              #i - remove cross camp ties from data.tables
                  altnet1 <- altnet1[altnet1$Camp1==altnet1$Camp2,]
                  altnet2 <- altnet2[altnet2$Camp1==altnet2$Camp2,]
                  altnet3 <- altnet3[altnet3$Camp1==altnet3$Camp2,]
              #ia - for summary stats; save altnet info as separate name
                  sumaltnet1 <- altnet1
                  sumaltnet2 <- altnet2
                  sumaltnet3 <- altnet3
              #ii - split altnetworks into lists by camps
                  naltnet1 <- split(altnet1,altnet1$Camp1)
                  naltnet2 <- split(altnet2,altnet2$Camp1)
                  naltnet3 <- split(altnet3,altnet3$Camp1)
              #iii - convert each camp edge list to a matrix
                  for(i in 1:length(naltnet1)){
                    naltnet1[[i]] <- matrix(naltnet1[[i]]$tie,nrow = sqrt(nrow(naltnet1[[i]])),ncol = sqrt(nrow(naltnet1[[i]])),byrow=T)
                    naltnet2[[i]] <- matrix(naltnet2[[i]]$tie,nrow = sqrt(nrow(naltnet2[[i]])),ncol = sqrt(nrow(naltnet2[[i]])),byrow=T)
                    naltnet3[[i]] <- matrix(naltnet3[[i]]$tie,nrow = sqrt(nrow(naltnet3[[i]])),ncol = sqrt(nrow(naltnet3[[i]])),byrow=T)
                  }
    #b - for dfl
        dfl2 <- dfl
        for(i in 1:length(dfl2)){
        #identify missing respondents in each camp
            tcamp <- dfl2[[i]]$camp[1]
            tpid <- dfl2[[i]]$pid
            tmiss <- df2$pid[df2$camp==tcamp & !(df2$pid %in% tpid)]
        #add rows for missing respondents
            emptydf <- as.data.frame(matrix(NA,nrow=length(tmiss),ncol=ncol(dfl2[[i]])))
            names(emptydf) <- names(dfl2[[i]])
            emptydf$pid <- tmiss
            dfl2[[i]] <- rbind(dfl2[[i]],emptydf)
        }

        
#create list of networks from all waves
    allnet <- c(naltnet1,naltnet2,naltnet3)
    fullnet <- c(dv.fnet1,dv.fnet2,dv.fnet3)
    twave <- rep(c(1:3),each=59)
    alldfl <- c(dfl2,dfl2,dfl2)

#make diagonals = 0
    for(i in 1:length(fullnet)){
      diag(fullnet[[i]]) <- 0
    } 
        
#create similarity function
    simmat <- function(x){
      outer(x, x, function(a, b) as.integer(a == b))
    }
#prep variables
    tmp <- c(edu,edu[1:59])
    ffipv <- tmp
    ffgem <- tmp
    ffalc <- tmp
    ffgend <- tmp
    ffnum <- tmp
    ffhiv <- tmp
    clusters <- list()
    modul <- list()
    clipv <- tmp
    clgem <- tmp
    clalc <- tmp
    clgend <- tmp
    clnum <- tmp
    clhiv <- tmp

    
#start loop 
    for(j in 1:length(twave)){
    #create temporary network objects
        tfnet <- fullnet[[j]]
        diag(tfnet) <- 0
        cfnet <- allnet[[j]]
        g1 <- graph_from_adjacency_matrix(tfnet,weighted = T)
    #friend variables
        for(i in 1:nrow(tfnet)){ #for each ego, find average friend ipv
            #skip if ego not a responder network
                if(alldfl[[j]]$pid[i]%in%nonresponders){
                  next()
                }
            ff <- tfnet[i,]==1 #find ego's friends
            if(sum(ff)==0){ #if no friends
              ffipv[[j]][i] <- NA
              ffgem[[j]][i] <- NA
              ffalc[[j]][i] <- NA
              ffgend[[j]][i] <- NA
              ffnum[[j]][i] <- 0
              ffhiv[[j]][i] <- NA
            }else{
              ffgend[[j]][i] <- mean(alldfl[[j]]$gender[ff])
              ffnum[[j]][i] <- sum(ff)
              if(twave[j]==1){
                ffipv[[j]][i] <- mean(alldfl[[j]]$b_ipvoutcome_cat[ff],na.rm=T)
                ffgem[[j]][i] <- mean(alldfl[[j]]$b_gem_r15_avg[ff],na.rm=T)
                ffalc[[j]][i] <- mean(alldfl[[j]]$b_alc_frq[ff],na.rm=T)
                ffhiv[[j]][i] <- mean(alldfl[[j]]$b_testhiv_12[ff],na.rm=T)
              }
              if(twave[j]==2){
                ffipv[[j]][i] <- mean(alldfl[[j]]$m_ipvoutcome_cat[ff],na.rm=T)
                ffgem[[j]][i] <- mean(alldfl[[j]]$m_gem_r15_avg[ff],na.rm=T)
                ffalc[[j]][i] <- mean(alldfl[[j]]$m_alc_frq[ff],na.rm=T)
                ffhiv[[j]][i] <- mean(alldfl[[j]]$m_testhiv_12[ff],na.rm=T)
              }
              if(twave[j]==3){
                ffipv[[j]][i] <- mean(alldfl[[j]]$e_ipvoutcome_cat[ff],na.rm=T)
                ffgem[[j]][i] <- mean(alldfl[[j]]$e_gem_r15_avg[ff],na.rm=T)
                ffalc[[j]][i] <- mean(alldfl[[j]]$e_alc_frq[ff],na.rm=T)
                ffhiv[[j]][i] <- mean(alldfl[[j]]$e_testhiv_12[ff],na.rm=T)
              }
            }
          }
    #identify clusters
        #weight full network so that those with high outdegree have fewer friends
            wgt <- sqrt(rowSums(tfnet))
            for(i in 1:nrow(tfnet)){
              if(wgt[i]==0){
                next()
              }
              tfnet[i,] <- tfnet[i,]/wgt[i]
            }
        #also weight full friendship network so that close friends matter
            rowSums(tfnet)
            tfnet <- tfnet+cfnet
            tfnet <- ifelse(tfnet>.5,.5,tfnet)
        #cluster
            for(q in 1:3){
              #turn to graph object
                if(q==1){
                  g <- graph_from_adjacency_matrix(tfnet,weighted = T)
                  stfnet <- tfnet
                }else{
                  g <- graph_from_adjacency_matrix(stfnet,weighted = T)
                }
              #run clustering algorithms
                  #prep data
                      cl <- as.data.frame(matrix(nrow=nrow(tfnet)))[,-1]
                  #run main cluster algorithms
                      cl$t1 <- walktrap.community(g)$membership
                      tryCatch(cl$t2 <- spinglass.community(g)$membership,error=function(e) NULL)
                      cl$t3 <- fastgreedy.community(as.undirected(g))$membership
                      cl$t4 <- leading.eigenvector.community(as.undirected(g))$membership
                      cl$t5 <- cluster_louvain(as.undirected(g))$membership
                      a <- sapply(cl,function(x)length(unique(x)))
                      if(sum(a>1)>1){
                        cl <- cl[,a!=1]
                      }
                      hc <- hclust(as.dist(1-stfnet),method="ward.D")
                      km=round(mean(sapply(cl,function(x) sum(table(x)!=1)+ ifelse(any(table(x)==1),1,0)+.00001)))
                      cl$t6  <-  cutree(hc, k = km)
                  #check similarity of clusters
                      # mat <- matrix(nrow=6,ncol=6)
                      # for(i in 1:6){
                      #   for(j in 1:6){
                      #       mat[i,j] <- (cor(c(simmat(cl[,i])),c(simmat(cl[,j]))))
                      #   }
                      # }
                  #create a matrix of overlaps
                      simnet <- simmat(cl[,1])
                      for(r in 2:length(cl)){
                          simnet <- simnet+simmat(cl[,r])
                      }
                      diag(simnet) <- 0
                  #weight by full friends too
                      stfnet <- (simnet+tfnet*2)
                  #run clusters on this object
            }
            #extract cluster memberships
                cluster <- cl[,length(cl)]
            #calculate modularity
                modul[[j]] <- modularity(as.undirected(g),cluster)
            #Save cluster memberships as matrix of ties
                tmpnet <- simmat(cluster)
                diag(tmpnet) <- 0
                clusters[[j]] <- tmpnet
    #Get friend info based on clusters
        for(i in 1:nrow(tmpnet)){
          if(alldfl[[j]]$pid[i]%in%nonresponders){
            next()
          }
          cl <- tmpnet[i,]==1
          if(sum(cl)==0){ #if no friends
            clipv[[j]][i] <- NA
            clgem[[j]][i] <- NA
            clalc[[j]][i] <- NA
            clgend[[j]][i] <- NA
            clnum[[j]][i] <- 0
            clhiv[[j]][i] <- NA
          }else{
            clgend[[j]][i] <- mean(alldfl[[j]]$gender[cl])
            clnum[[j]][i] <- sum(cl)
            if(twave[j]==1){
              clipv[[j]][i] <- mean(alldfl[[j]]$b_ipvoutcome_cat[cl])
              clgem[[j]][i] <- mean(alldfl[[j]]$b_gem_r15_avg[cl])
              clalc[[j]][i] <- mean(alldfl[[j]]$b_alc_frq[cl])
              clhiv[[j]][i] <- mean(alldfl[[j]]$b_testhiv_12[cl])
            }
            if(twave[j]==2){
              clipv[[j]][i] <- mean(alldfl[[j]]$m_ipvoutcome_cat[cl])
              clgem[[j]][i] <- mean(alldfl[[j]]$m_gem_r15_avg[cl])
              clalc[[j]][i] <- mean(alldfl[[j]]$m_alc_frq[cl])
              clhiv[[j]][i] <- mean(alldfl[[j]]$m_testhiv_12[cl])
            }
            if(twave[j]==3){
              clipv[[j]][i] <- mean(alldfl[[j]]$e_ipvoutcome_cat[cl])
              clgem[[j]][i] <- mean(alldfl[[j]]$e_gem_r15_avg[cl])
              clalc[[j]][i] <- mean(alldfl[[j]]$e_alc_frq[cl])
              clhiv[[j]][i] <- mean(alldfl[[j]]$e_testhiv_12[cl])
            }
          }
          # #check quality of clusters graphically
          #        set.seed(1);plot.igraph(g1,vertex.label=NA,edge.arrow.size=.00001, vertex.size=5,vertex.color=cluster)
          #        g1 <- graph_from_adjacency_matrix(stfnet,weighted = T)
        }
        print(j)
    }

#1 - get close received ties
    recnet <- allnet
    for(i in 1:length(recnet)){
      recnet[[i]] <- t(allnet[[i]])
    }

#2 - get close transitive ties (outdegrees of outdegrees)
    trnetoo <- allnet
    for(i in 1:length(trnetoo)){
    for(j in 1:nrow(trnetoo[[i]])){ #for each individual in each camp
        a <- allnet[[i]][j,]==1 #identify egos' ties
        if(sum(a)==0){ #skip if ego has no ties
          trnetoo[[i]][j,] <- 0
          next()
        }
        if(sum(a)==1){ #grab friends of ego's ties if ego has one tie
          b <- allnet[[i]][a,]
        }
        if(sum(a)>1){ #grab all ties of ties if ego has more than one tie
          b <- colSums(allnet[[i]][a,])
        }
        trnetoo[[i]][j,] <- ifelse(b>0,1,0) #assign these ties to ego
    }
    }

#3 - get close transitive ties (indegrees of outdegrees)
    trnetio <- allnet
    for(i in 1:length(trnetio)){
    for(j in 1:nrow(trnetio[[i]])){ #for each individual in each camp
        a <- allnet[[i]][j,]==1 #identify ties
        d <- ncol(allnet[[i]][,a])
        d <- ifelse(is.null(d),1,d)
        if(sum(a)==0){ #skip if no ties
          trnetio[[i]][j,] <- 0
          next()
        }
        if(d==1){ #grab friends of ties indegree=1
          b <- allnet[[i]][,a]
        }
        if(d>1){ #grab all ties of ties if more than one tie
          b <- rowSums(allnet[[i]][,a])
        }
        trnetio[[i]][j,] <- ifelse(b>0,1,0) #assign any of these ties to ego as T/F
    }
      print(i)
    }


#4 - get close transitive ties (outdegrees of indegrees)
    trnetoi <- allnet
    for(i in 1:length(trnetoi)){
    for(j in 1:nrow(trnetoi[[i]])){ #for each individual in each camp
        a <- allnet[[i]][,j]==1 #identify egos' ties
        if(sum(a)==0){ #skip if ego has no ties
          trnetoi[[i]][,j] <- 0
          next()
        }
        if(sum(a)==1){ #grab friends of ego's ties if ego has one tie
          b <- allnet[[i]][a,]
        }
        if(sum(a)>1){ #grab all ties of ties if ego has more than one tie
          b <- colSums(allnet[[i]][a,])
        }
        trnetoi[[i]][j,] <- ifelse(b>0,1,0) #assign these ties to ego
    }
      print(i)
    }

    
#5 - get close transitive ties (indegrees of indegrees)
    trnetii <- allnet
    for(i in 1:length(trnetii)){
    for(j in 1:nrow(trnetii[[i]])){ #for each individual in each camp
        a <- allnet[[i]][,j]==1 #identify ties
        d <- ncol(allnet[[i]][,a])
        d <- ifelse(is.null(d),1,d)
        if(sum(a)==0){ #skip if no ties
          trnetii[[i]][,j] <- 0
          next()
        }
        if(d==1){ #grab friends of tie if one ties
          b <- allnet[[i]][,a]
        }
        if(d>1){ #grab all ties of ties if more than one tie
          b <- rowSums(allnet[[i]][,a])
        }
        trnetii[[i]][j,] <- ifelse(b>0,1,0) #assign any of these ties to ego as T/F
    }
      print(i)
    }
    
#6 - get full received ties
    frecnet <- fullnet
    for(i in 1:length(frecnet)){
      #transpose df
          frecnet[[i]] <- t(frecnet[[i]])
      #exclude reciprocal ties where ego and alter have high outdegrees
          for(j in 1:nrow(fullnet[[i]])){
            for(k in 1:nrow(fullnet[[i]])){
              if((sum(fullnet[[i]][j,])>15 | sum(fullnet[[i]][j,]>0)>length(fullnet[[i]][j,])/3) & (sum(fullnet[[i]][k,])>15 | sum(fullnet[[i]][k,]>0)>length(fullnet[[i]][k,])/3)){
                frecnet[[i]][j,k] <- 0
              }
            }
          }
    }
    
#7 - create network for finding transitive ties that excludes outdegrees of egos with high outdegrees
    tempnet <- fullnet
    for(j in 1:nrow(fullnet[[i]])){
      if(sum(fullnet[[i]][j,])>15){
        tempnet[[i]][j,] <- 0
      }
    }
    
#8 - get close transitive ties (outdegrees of outdegrees)
    ftrnetoo <- tempnet
    for(i in 1:length(ftrnetoo)){
    for(j in 1:nrow(ftrnetoo[[i]])){ #for each individual in each camp
        a <- tempnet[[i]][j,]==1 #identify egos' ties
        if(sum(a)==0){ #skip if ego has no ties
          ftrnetoo[[i]][j,] <- 0
          next()
        }
        if(sum(a)==1){ #grab friends of ego's ties if ego has one tie
          b <- tempnet[[i]][a,]
        }
        if(sum(a)>1){ #grab all ties of ties if ego has more than one tie
          b <- colSums(tempnet[[i]][a,])
        }
        ftrnetoo[[i]][j,] <- ifelse(b>0,1,0) #assign these ties to ego
    }
      print(i)
    }

#9 - get close transitive ties (indegrees of outdegrees)
    ftrnetio <- tempnet
    for(i in 1:length(ftrnetio)){
    for(j in 1:nrow(ftrnetio[[i]])){ #for each individual in each camp
        a <- tempnet[[i]][j,]==1 #identify ties
        d <- ncol(tempnet[[i]][,a])
        d <- ifelse(is.null(d),1,d)
        if(sum(a)==0){ #skip if no ties
          ftrnetio[[i]][j,] <- 0
          next()
        }
        if(d==1){ #grab friends of tie if one ties
          b <- tempnet[[i]][,a]
        }
        if(d>1){ #grab all ties of ties if more than one tie
          b <- rowSums(tempnet[[i]][,a])
        }
        ftrnetio[[i]][j,] <- ifelse(b>0,1,0) #assign any of these ties to ego as T/F
    }
      print(i)
    }


#10 - get close transitive ties (outdegrees of indegrees)
    ftrnetoi <- tempnet
    for(i in 1:length(ftrnetoi)){
    for(j in 1:nrow(ftrnetoi[[i]])){ #for each individual in each camp
        a <- tempnet[[i]][,j]==1 #identify egos' ties
        if(sum(a)==0){ #skip if ego has no ties
          ftrnetoi[[i]][,j] <- 0
          next()
        }
        if(sum(a)==1){ #grab friends of ego's ties if ego has one tie
          b <- tempnet[[i]][a,]
        }
        if(sum(a)>1){ #grab all ties of ties if ego has more than one tie
          b <- colSums(tempnet[[i]][a,])
        }
        ftrnetoi[[i]][j,] <- ifelse(b>0,1,0) #assign these ties to ego
    }
      print(i)
    }

#11 - get close transitive ties (indegrees of indegrees)
    ftrnetii <- tempnet
    for(i in 1:length(ftrnetii)){
    for(j in 1:nrow(ftrnetii[[i]])){ #for each individual in each camp
        a <- tempnet[[i]][,j]==1 #identify ties
        d <- ncol(tempnet[[i]][,a])
        d <- ifelse(is.null(d),1,d)
        if(sum(a)==0){ #skip if no ties
          ftrnetii[[i]][,j] <- 0
          next()
        }
        if(d==1){ #grab friends of tie if one ties
          b <- tempnet[[i]][,a]
        }
        if(d>1){ #grab all ties of ties if more than one tie
          b <- rowSums(tempnet[[i]][,a])
        }
        ftrnetii[[i]][j,] <- ifelse(b>0,1,0) #assign any of these ties to ego as T/F
    }
      print(i)
    }


#Instead of below, create a weighted network based on the networks we created above; ties method max means only keep friends we know are better than others.    
    wnet <- allnet
    for(i in 1:length(wnet)){
      wnet[[i]] <- 
      allnet[[i]]*1000+
      fullnet[[i]]*100+
      recnet[[i]]*5+
      frecnet[[i]]*3+
      trnetoi[[i]]*2+
      trnetoo[[i]]*2+
      trnetio[[i]]*1+
      trnetii[[i]]*1+
      ftrnetoo[[i]]*.5+
      ftrnetio[[i]]*.5+
      ftrnetoi[[i]]*.33+
      ftrnetii[[i]]*.2+
      clusters[[i]]*.1
    }
    
#now choose top 10 friends of all with over 10 nominations; top 3 friends for any with less than 3 nominations
    tl <- fullnet
    for(i in 1:length(wnet)){
      for(j in 1:nrow(wnet[[i]])){
        if(sum(fullnet[[i]][j,])>8){
          tl[[i]][j,] <- ifelse(9>rank(10000-wnet[[i]][j,],ties.method = "max"),1,0)
        }
        # if(sum(fullnet[[i]][j,])<3){
        #   tl[[i]][j,] <- ifelse(4>rank(10000-wnet[[i]][j,],ties.method = "max"),1,0)
        # }
        # if(sum(fullnet[[i]][j,])<3){
        #   tl[[i]][j,] <- ifelse(4>rank(10000-wnet[[i]][j,],ties.method = "max"),1,0)
        # }
        # if(sum(fullnet[[i]][j,])<3){
        #   tl[[i]][j,] <- ifelse(4>rank(10000-wnet[[i]][j,],ties.method = "max"),1,0)
        # }
        # if(sum(fullnet[[i]][j,])<1){
        #   tl[[i]][j,] <- ifelse(2>rank(10000-wnet[[i]][j,],ties.method = "max"),1,0)
        # }
        # if(sum(fullnet[[i]][j,])<1){
        #   tl[[i]][j,] <- ifelse(2>rank(10000-wnet[[i]][j,],ties.method = "max"),1,0)
        # }
      }
    }
    # allnet[[i]][j,]
    # cor(unlist(fullnet)==1,unlist(allnet)==1)
    # cor(unlist(tl)==1,unlist(allnet)==1)
    # 
    # table(unlist(tl)[unlist(allnet)==1])
    # 
    # length(unlist(fullnet))
    # length(unlist(allnet))
    # tl[[4]][1,]
    # fullnet[[4]][1,]
    
    # #trim full friendship list based on above info
    #     {
    #     #Prep data
    #         fullnett <- fullnet
    #         tl <- fullnet
    #     #Create subset requirements
    #         toomuchf <- function(x){
    #           sum(x==1)>8 | sum(x>0)>length(x)/3
    #         }
    #         toolittlef <- function(x){
    #           sum(x==1)<4 & length(x)>20
    #         }
    #         toomuchf2 <- function(x){
    #           sum(x)>5 | sum(x)>(length(x)/4)
    #         }
    #     }
    # 
    # for(i in 1:length(fullnet)){
    #   for(j in 1:nrow(fullnet[[i]])){
    #     #Create subset criteria
    #       a <-  tl[[i]][j,]==1 & (recnet[[i]][j,]==1 | allnet[[i]][j,]==1)
    #       b <-  tl[[i]][j,]==1 & (recnet[[i]][j,]==1 | allnet[[i]][j,]==1 | frecnet[[i]][j,]==1)
    #       c <-  tl[[i]][j,]==1 & (recnet[[i]][j,]==1 | allnet[[i]][j,]==1 | frecnet[[i]][j,]==1 | trnetoi[[i]][j,]==1)
    #       d <-  tl[[i]][j,]==1 & (recnet[[i]][j,]==1 | allnet[[i]][j,]==1 | frecnet[[i]][j,]==1 | trnetoi[[i]][j,]==1 | trnetoo[[i]][j,]==1)
    #       e <-  tl[[i]][j,]==1 & (recnet[[i]][j,]==1 | allnet[[i]][j,]==1 | frecnet[[i]][j,]==1 | trnetoi[[i]][j,]==1 | trnetoo[[i]][j,]==1 |  trnetio[[i]][j,]==1)
    #       f <-  tl[[i]][j,]==1 & (recnet[[i]][j,]==1 | allnet[[i]][j,]==1 | frecnet[[i]][j,]==1 | trnetoi[[i]][j,]==1 | trnetoo[[i]][j,]==1 |  trnetio[[i]][j,]==1 | trnetii[[i]][j,]==1)
    #       g <-  tl[[i]][j,]==1 & (recnet[[i]][j,]==1 | allnet[[i]][j,]==1 | frecnet[[i]][j,]==1 | trnetoi[[i]][j,]==1 | trnetoo[[i]][j,]==1 |  trnetio[[i]][j,]==1 | trnetii[[i]][j,]==1 | ftrnetoo[[i]][j,]==1)
    #       h <-  tl[[i]][j,]==1 & (recnet[[i]][j,]==1 | allnet[[i]][j,]==1 | frecnet[[i]][j,]==1 | trnetoi[[i]][j,]==1 | trnetoo[[i]][j,]==1 |  trnetio[[i]][j,]==1 | trnetii[[i]][j,]==1 | ftrnetoo[[i]][j,]==1 | ftrnetio[[i]][j,]==1)
    #       i0 <-  tl[[i]][j,]==1 & (recnet[[i]][j,]==1 | allnet[[i]][j,]==1 | frecnet[[i]][j,]==1 | trnetoi[[i]][j,]==1 | trnetoo[[i]][j,]==1 |  trnetio[[i]][j,]==1 | trnetii[[i]][j,]==1 | ftrnetoo[[i]][j,]==1 | ftrnetio[[i]][j,]==1 | ftrnetii[[i]][j,]==1)
    #       j0 <-  tl[[i]][j,]==1 & (recnet[[i]][j,]==1 | allnet[[i]][j,]==1 | frecnet[[i]][j,]==1 | trnetoi[[i]][j,]==1 | trnetoo[[i]][j,]==1 |  trnetio[[i]][j,]==1 | trnetii[[i]][j,]==1 | ftrnetoo[[i]][j,]==1 | ftrnetio[[i]][j,]==1 | ftrnetii[[i]][j,]==1 | ftrnetoi[[i]][j,]==1)
    #       k <-  tl[[i]][j,]==1 & (recnet[[i]][j,]==1 | allnet[[i]][j,]==1 | frecnet[[i]][j,]==1 | trnetoi[[i]][j,]==1 | trnetoo[[i]][j,]==1 |  trnetio[[i]][j,]==1 | trnetii[[i]][j,]==1 | ftrnetoo[[i]][j,]==1 | ftrnetio[[i]][j,]==1 | ftrnetii[[i]][j,]==1 |  ftrnetoi[[i]][j,]==1 | clusters[[i]][j,])
    # 
    #       a1 <-  tl[[i]][j,]==1 | (recnet[[i]][j,]==1 | allnet[[i]][j,]==1)
    #       b1 <-  tl[[i]][j,]==1 | (recnet[[i]][j,]==1 | allnet[[i]][j,]==1 | frecnet[[i]][j,]==1)
    #       c1 <-  tl[[i]][j,]==1 | (recnet[[i]][j,]==1 | allnet[[i]][j,]==1 | frecnet[[i]][j,]==1 | trnetoi[[i]][j,]==1)
    #       d1 <-  tl[[i]][j,]==1 | (recnet[[i]][j,]==1 | allnet[[i]][j,]==1 | frecnet[[i]][j,]==1 | trnetoi[[i]][j,]==1 | trnetoo[[i]][j,]==1)
    #       e1 <-  tl[[i]][j,]==1 | (recnet[[i]][j,]==1 | allnet[[i]][j,]==1 | frecnet[[i]][j,]==1 | trnetoi[[i]][j,]==1 | trnetoo[[i]][j,]==1 |  trnetio[[i]][j,]==1)
    #       f1 <-  tl[[i]][j,]==1 | (recnet[[i]][j,]==1 | allnet[[i]][j,]==1 | frecnet[[i]][j,]==1 | trnetoi[[i]][j,]==1 | trnetoo[[i]][j,]==1 |  trnetio[[i]][j,]==1 | trnetii[[i]][j,]==1)
    #       g1 <-  tl[[i]][j,]==1 | (recnet[[i]][j,]==1 | allnet[[i]][j,]==1 | frecnet[[i]][j,]==1 | trnetoi[[i]][j,]==1 | trnetoo[[i]][j,]==1 |  trnetio[[i]][j,]==1 | trnetii[[i]][j,]==1 | ftrnetoo[[i]][j,]==1)
    #       h1 <-  tl[[i]][j,]==1 | (recnet[[i]][j,]==1 | allnet[[i]][j,]==1 | frecnet[[i]][j,]==1 | trnetoi[[i]][j,]==1 | trnetoo[[i]][j,]==1 |  trnetio[[i]][j,]==1 | trnetii[[i]][j,]==1 | ftrnetoo[[i]][j,]==1 | ftrnetio[[i]][j,]==1)
    #       i1 <-  tl[[i]][j,]==1 | (recnet[[i]][j,]==1 | allnet[[i]][j,]==1 | frecnet[[i]][j,]==1 | trnetoi[[i]][j,]==1 | trnetoo[[i]][j,]==1 |  trnetio[[i]][j,]==1 | trnetii[[i]][j,]==1 | ftrnetoo[[i]][j,]==1 | ftrnetio[[i]][j,]==1 | ftrnetii[[i]][j,]==1)
    #       j1 <-  tl[[i]][j,]==1 | (recnet[[i]][j,]==1 | allnet[[i]][j,]==1 | frecnet[[i]][j,]==1 | trnetoi[[i]][j,]==1 | trnetoo[[i]][j,]==1 |  trnetio[[i]][j,]==1 | trnetii[[i]][j,]==1 | ftrnetoo[[i]][j,]==1 | ftrnetio[[i]][j,]==1 | ftrnetii[[i]][j,]==1 | ftrnetoi[[i]][j,]==1)
    #       k1 <-  tl[[i]][j,]==1 | (recnet[[i]][j,]==1 | allnet[[i]][j,]==1 | frecnet[[i]][j,]==1 | trnetoi[[i]][j,]==1 | trnetoo[[i]][j,]==1 |  trnetio[[i]][j,]==1 | trnetii[[i]][j,]==1 | ftrnetoo[[i]][j,]==1 | ftrnetio[[i]][j,]==1 | ftrnetii[[i]][j,]==1 | ftrnetoi[[i]][j,]==1 | clusters[[i]][j,])
    #       
    #       
    #       # fullnet[[i]][j,]
    #       # tl[[i]][j,]
    #     #subset
    #       if(toomuchf(tl[[i]][j,])){
    #         tl[[i]][j,] <- k
    #       }
    #       if(toomuchf(tl[[i]][j,])){
    #         tl[[i]][j,] <- j0
    #       }
    #       if(toomuchf(tl[[i]][j,])){
    #         tl[[i]][j,] <- i0
    #       }
    #       if(toomuchf(tl[[i]][j,])){
    #         tl[[i]][j,] <- h
    #       }
    #       if(toomuchf(tl[[i]][j,])){
    #         tl[[i]][j,] <- g
    #       }
    #       if(toomuchf(tl[[i]][j,])){
    #           tl[[i]][j,] <- f
    #       }
    #       if(toomuchf(tl[[i]][j,])){
    #           tl[[i]][j,] <- e
    #       }
    #       if(toomuchf(tl[[i]][j,])){
    #           tl[[i]][j,] <- d
    #       }
    #       if(toomuchf(tl[[i]][j,])){
    #           tl[[i]][j,] <- c
    #       }
    #       if(toomuchf(tl[[i]][j,])){
    #           tl[[i]][j,] <- b
    #       }
    #       if(toomuchf(tl[[i]][j,])){
    #           tl[[i]][j,] <- a
    #       }
    #       if(toolittlef(tl[[i]][j,])){
    #         if(!toomuchf2(a1)){
    #            tl[[i]][j,] <- a1  
    #         }
    #       }
    #       if(toolittlef(tl[[i]][j,])){
    #         if(!toomuchf2(b1)){
    #           tl[[i]][j,] <- b1  
    #         }
    #       }
    #       if(toolittlef(tl[[i]][j,])){
    #         if(!toomuchf2(c1)){
    #           tl[[i]][j,] <- c1  
    #         }
    #       }
    #       if(toolittlef(tl[[i]][j,])){
    #         if(!toomuchf2(d1)){
    #           tl[[i]][j,] <- d1  
    #         }
    #       } 
    #       if(toolittlef(tl[[i]][j,])){
    #         if(!toomuchf2(e1)){
    #           tl[[i]][j,] <- e1  
    #         }
    #       }
    #       if(toolittlef(tl[[i]][j,])){
    #         if(!toomuchf2(f1)){
    #           tl[[i]][j,] <- f1
    #         }
    #       }
    #       if(toolittlef(tl[[i]][j,])){
    #         if(!toomuchf2(g1)){
    #           tl[[i]][j,] <- g1
    #         }
    #       }
    #       if(toolittlef(tl[[i]][j,])){
    #         if(!toomuchf2(h1)){
    #           tl[[i]][j,] <- h1
    #         }
    #       }
    #       if(toolittlef(tl[[i]][j,])){
    #         if(!toomuchf2(i1)){
    #           tl[[i]][j,] <- i1
    #         }
    #       }
    #       if(toolittlef(tl[[i]][j,])){
    #         if(!toomuchf2(k1)){
    #           tl[[i]][j,] <- k1
    #         }
    #       }
    #   }
    # }
    # summary(unlist(sapply(fullnet,function(x) rowSums(x>0))))
    # summary(unlist(sapply(tl,function(x) rowSums(x>0))))
    # table(unlist(sapply(fullnet,function(x) rowSums(x>0))))
    # table(unlist(sapply(tl,function(x) rowSums(x>0))))
    # (sapply(fullnet,function(x) rowSums(x>0)))
    # (sapply(tl,function(x) rowSums(x>0)))
    # table(unlist(sapply(fullnet,function(x) colSums(x>0))))
    # table(unlist(sapply(tl,function(x) colSums(x>0))))
    # (sapply(tl,function(x) colSums(x>0)))
    # cor(unlist(frecnet),unlist(tl))
    # cor(unlist(frecnet),unlist(fullnet))
    # cor(unlist(frecnet),unlist(allnet))
    # cor(c(tl[[1]]),c(fullnet[[1]]))
    # cor(c(tl[[1]]),c(fullnet[[1]]))
    # tl[[117]]

#Derive friendship variables with new friendship info
    for(j in 1:length(twave)){
      #create temporary network objects
          tfnet <- tl[[j]]
          diag(tfnet) <- 0
      for(i in 1:nrow(tfnet)){ #for each ego, find average friend ipv
        if(alldfl[[j]]$pid[i]%in%nonresponders){
          next()
        }
        ff <- tfnet[i,]==1 #find ego's friends
        if(sum(ff)==0){ #if no friends
          ffipv[[j]][i] <- NA
          ffgem[[j]][i] <- NA
          ffalc[[j]][i] <- NA
          ffgend[[j]][i] <- NA
          ffnum[[j]][i] <- 0
          ffhiv[[j]][i] <- NA
        }else{
          ffgend[[j]][i] <- mean(alldfl[[j]]$gender[ff])
          ffnum[[j]][i] <- sum(ff)
          if(twave[j]==1){
            ffipv[[j]][i] <- mean(alldfl[[j]]$b_ipvoutcome_cat[ff])
            ffgem[[j]][i] <- mean(alldfl[[j]]$b_gem_r15_avg[ff])
            ffalc[[j]][i] <- mean(alldfl[[j]]$b_alc_frq[ff])
            ffhiv[[j]][i] <- mean(alldfl[[j]]$b_testhiv_12[ff])
          }
          if(twave[j]==2){
            ffipv[[j]][i] <- mean(alldfl[[j]]$m_ipvoutcome_cat[ff])
            ffgem[[j]][i] <- mean(alldfl[[j]]$m_gem_r15_avg[ff])
            ffalc[[j]][i] <- mean(alldfl[[j]]$m_alc_frq[ff])
            ffhiv[[j]][i] <- mean(alldfl[[j]]$m_testhiv_12[ff])
          }
          if(twave[j]==3){
            ffipv[[j]][i] <- mean(alldfl[[j]]$e_ipvoutcome_cat[ff])
            ffgem[[j]][i] <- mean(alldfl[[j]]$e_gem_r15_avg[ff])
            ffalc[[j]][i] <- mean(alldfl[[j]]$e_alc_frq[ff])
            ffhiv[[j]][i] <- mean(alldfl[[j]]$e_testhiv_12[ff])
          }
        }
      }
    }
#Subset variables to first two waves
    ffipv <- ffipv[1:118]
    ffgem <- ffgem[1:118]
    ffalc <- ffalc[1:118]
    ffgend <- ffgend[1:118]
    ffnum <- ffnum[1:118]
    ffhiv <- ffhiv[1:118]
    clipv <- clipv[1:118]
    clgem <- clgem[1:118]
    clalc <- clalc[1:118]
    clgend <- clgend[1:118]
    clnum <- clnum[1:118]
    clhiv <- clhiv[1:118]
        
#Convert to siena object for dvs. 
    #remove missing respondents from fullnet    
        temp <- split(df2$b_respond,df2$camp)
        temp <- c(temp,temp,temp)
        for(i in 1:length(tl)){
          tl[[i]] <- tl[[i]][temp[[i]]==1,temp[[i]]==1]  
        }
    #Make structural 0s = 10.
        allnet <- c(nnet1,nnet2,nnet3)
        for(i in 1:length(tl)){
          tl[[i]][allnet[[i]]==10] <- 10
        }
        
        
        dv.fnet1 <- tl[1:59]
        dv.fnet2 <- tl[60:118]
        dv.fnet3 <- tl[119:177]
        
        
    #for dv versions
        #i - Treat each two-twave pair as a separate array.
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
              
              
