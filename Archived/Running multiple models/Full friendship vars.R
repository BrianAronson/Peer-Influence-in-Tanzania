#1 - recreate full friendship network, but without screwing up close friends
    dir<-getwd()
    tdir<-"/data/data1/bda13/Data/Shared/Tanzania/"
    setwd(paste(tdir,"/Dropbox files/Tanzania R01 Network Data - Peer Influence/SAS Data",sep=""))
    fnet1 <- read.sas7bdat("baselinesocialnetwork_20180206.sas7bdat")
    fnet2 <- read.sas7bdat("midpointsocialnetwork_20180206.sas7bdat")
    setwd(dir)
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
              fnet1<-fnet1[fnet1$Camp1==fnet1$Camp2,]
              fnet2<-fnet2[fnet2$Camp1==fnet2$Camp2,]
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
              # temp<-fnet1[,.(means=mean(RELP)),by=.(ID1)]
              # fnet1<-merge(temp,fnet1)
              # temp<-fnet2[,.(means=mean(RELP)),by=.(ID1)]
              # fnet2<-merge(temp,fnet2)
              # fnet1$RELP[net1$tie==1]<-fnet1$means[net1$tie==1]
              # fnet2$RELP[net2$tie==1]<-fnet2$means[net2$tie==1]
      #f - Create separate matrices and arrays for each camp (FOR META-ANALYSIS METHOD ONLY)
          #i - split fnetworks into lists by camps
              fnet1<-split(fnet1,fnet1$Camp1)
              fnet2<-split(fnet2,fnet2$Camp1)
          #ii - convert each camp edge list to a matrix
              for(i in 1:length(fnet1)){
                fnet1[[i]]<-matrix(fnet1[[i]]$RELP,nrow = sqrt(nrow(fnet1[[i]])),ncol = sqrt(nrow(fnet1[[i]])),byrow=T)
                fnet2[[i]]<-matrix(fnet2[[i]]$RELP,nrow = sqrt(nrow(fnet2[[i]])),ncol = sqrt(nrow(fnet2[[i]])),byrow=T)
              }
  }
  

#2 - Find averages for all friends on dvs
    #prep variables
      ffipv<-edu
      ffgem<-edu
      ffalc<-edu
      ffgend<-edu
      ffnum<-edu
      ffhiv<-edu
      
    #for each camp in wave 1
      for(j in 1:length(fnet1)){
      #create temporary full friend network
          tfnet<-fnet1[[j]]
      #change ties back to binary
          tfnet<-ifelse(tfnet>0,1,0)
      #for each ego, find average friend ipv
          for(i in 1:nrow(tfnet)){
            ff<-tfnet[i,]==1
            if(sum(ff)==0){ #if no friends
              ffipv[[j]][i]<-NA
              ffgem[[j]][i]<-NA
              ffalc[[j]][i]<-NA
              ffgend[[j]][i]<-NA
              ffnum[[j]][i]<-0
              ffhiv[[j]][i]<-NA
            }else{
              ffipv[[j]][i]<-mean(dfl[[j]]$b_ipvoutcome_cat[ff])
              ffgem[[j]][i]<-mean(dfl[[j]]$b_gem_r15_avg[ff])
              ffalc[[j]][i]<-mean(dfl[[j]]$b_alc_frq[ff])
              ffgend[[j]][i]<-mean(dfl[[j]]$gender[ff])
              ffnum[[j]][i]<-sum(ff)
              ffhiv[[j]][i]<-mean(dfl[[j]]$b_testhiv_12[ff])
            }
          }
      }
      
    #for each camp in wave 2
      for(k in 1:length(fnet2)){
        j<-k+length(fnet1)
      #create temporary full friend network
          tfnet<-fnet2[[k]]
      #change ties back to binary
          tfnet<-ifelse(tfnet>0,1,0)
      #for each ego, find average friend ipv
          for(i in 1:nrow(tfnet)){
            ff<-tfnet[i,]==1
            if(sum(ff)==0){ #if no friends
              ffipv[[j]][i]<-NA
              ffgem[[j]][i]<-NA
              ffalc[[j]][i]<-NA
              ffgend[[j]][i]<-NA
              ffnum[[j]][i]<-0
              ffhiv[[j]][i]<-NA
            }else{
              ffipv[[j]][i]<-mean(dfl[[k]]$m_ipvoutcome_cat[ff])
              ffgem[[j]][i]<-mean(dfl[[k]]$m_gem_r15_avg[ff])
              ffalc[[j]][i]<-mean(dfl[[k]]$m_alc_frq[ff])
              ffgend[[j]][i]<-mean(dfl[[k]]$gender[ff])
              ffnum[[j]][i]<-sum(ff)
              ffhiv[[j]][i]<-mean(dfl[[k]]$m_testhiv_12[ff])
            }
          }
      }
 
      
      
#3 - Find averages for all friendship clusters on dvs
      library(igraph)
      library(cluster)
    #create similarity function
        simmat<-function(x){
          outer(x, x, function(a, b) as.integer(a == b))
        }
    #prep lists
        clusters<-list()
        modul<-list()
        clipv<-edu
        clgem<-edu
        clalc<-edu
        clgend<-edu
        clnum<-edu
        clhiv<-edu
        
    #for each camp in wave 1
      for(j in 1:length(fnet1)){
      #find friendship clusters
          #create temporary full friend network
              tfnet<-fnet1[[j]]
          #turn to graph
              g<-graph_from_adjacency_matrix(tfnet,weighted = T)
          #run clustering algorithms
            cl<-as.data.frame(matrix(nrow=nrow(tfnet)))[,-1]
            cl$t1<-walktrap.community(g)$membership
            tryCatch(cl$t2<-spinglass.community(g)$membership,error=function(e) NULL)
            cl$t3<-fastgreedy.community(as.undirected(g))$membership
            cl$t4<-leading.eigenvector.community(as.undirected(g))$membership
            cl$t5<-cluster_louvain(as.undirected(g))$membership
            a<-sapply(cl,function(x)length(unique(x)))
            cl<-cl[,a!=1]
            cl[]<-lapply(cl,as.factor)
          #create dissimilarity matrix from clusters
            gower_mat <- as.matrix(daisy(cl))
            gower_mat<-1-gower_mat
            diag(gower_mat)<-0
          #run final cluster algorithm based on dissimilarity matrix
            g1<-graph_from_adjacency_matrix(gower_mat,weighted = T)
            cluster<-cluster_louvain(as.undirected(g1))$membership
            clusters[[j]]<-cluster
            modul[[j]]<-modularity(as.undirected(g1),cluster)
      #get friendship attributes
          #create ties based on clusters
              tfnet<-simmat(cluster)
              diag(tfnet)<-0
              #for each ego, find average cluster friend info
                  for(i in 1:nrow(tfnet)){
                    cl<-tfnet[i,]==1
                    if(sum(cl)==0){ #if no friends
                      clipv[[j]][i]<-NA
                      clgem[[j]][i]<-NA
                      clalc[[j]][i]<-NA
                      clgend[[j]][i]<-NA
                      clnum[[j]][i]<-0
                      clhiv[[j]][i]<-NA
                    }else{
                      clipv[[j]][i]<-mean(dfl[[j]]$b_ipvoutcome_cat[cl])
                      clgem[[j]][i]<-mean(dfl[[j]]$b_gem_r15_avg[cl])
                      clalc[[j]][i]<-mean(dfl[[j]]$b_alc_frq[cl])
                      clgend[[j]][i]<-mean(dfl[[j]]$gender[cl])
                      clnum[[j]][i]<-sum(cl)
                      clhiv[[j]][i]<-mean(dfl[[j]]$b_testhiv_12[cl])
                    }
                  }
              print(j)
        }

    #for each camp in wave 2
      for(k in 1:length(fnet2)){
        j<-k+length(fnet1)
      #create temporary full friend network
          tfnet<-fnet2[[k]]
          #turn to graph
              g<-graph_from_adjacency_matrix(tfnet,weighted = T)
          #run clustering algorithms
            cl<-as.data.frame(matrix(nrow=nrow(tfnet)))[,-1]
            cl$t1<-walktrap.community(g)$membership
            tryCatch(cl$t2<-spinglass.community(g)$membership,error=function(e) NULL)
            cl$t3<-fastgreedy.community(as.undirected(g))$membership
            cl$t4<-leading.eigenvector.community(as.undirected(g))$membership
            cl$t5<-cluster_louvain(as.undirected(g))$membership
            a<-sapply(cl,function(x)length(unique(x)))
            cl<-cl[,a!=1]
            cl[]<-lapply(cl,as.factor)
          #create dissimilarity matrix from clusters
            gower_mat <- as.matrix(daisy(cl))
            gower_mat<-1-gower_mat
            diag(gower_mat)<-0
          #run final cluster algorithm based on dissimilarity matrix
            g1<-graph_from_adjacency_matrix(gower_mat,weighted = T)
            cluster<-cluster_louvain(as.undirected(g1))$membership
            clusters[[j]]<-cluster
            modul[[j]]<-modularity(as.undirected(g1),cluster)
    #get friendship attributes
        #create ties based on clusters
            tfnet<-simmat(cluster)
            diag(tfnet)<-0
            #for each ego, find average cluster friend info
                for(i in 1:nrow(tfnet)){
                  cl<-tfnet[i,]==1
                  if(sum(cl)==0){ #if no friends
                    clipv[[j]][i]<-NA
                    clgem[[j]][i]<-NA
                    clalc[[j]][i]<-NA
                    clgend[[j]][i]<-NA
                    clnum[[j]][i]<-0
                    clhiv[[j]][i]<-NA
                  }else{
                    clipv[[j]][i]<-mean(dfl[[k]]$b_ipvoutcome_cat[cl])
                    clgem[[j]][i]<-mean(dfl[[k]]$b_gem_r15_avg[cl])
                    clalc[[j]][i]<-mean(dfl[[k]]$b_alc_frq[cl])
                    clgend[[j]][i]<-mean(dfl[[k]]$gender[cl])
                    clnum[[j]][i]<-sum(cl)
                    clhiv[[j]][i]<-mean(dfl[[k]]$b_testhiv_12[cl])
                  }
                }
            print(j)
        }
        
        # # ffipv
        # # ffgem
        # # ffalc
        # # ffgend
        # # ffnum
        # # ffhiv
        # # clipv
        # # clgem
        # # clalc
        # # clgend
        # # clnum
        # # clhiv
        # 
        # summary(unlist(clipv))
        # summary(unlist(ffipv))
        # summary(unlist(r.ipv))
        # 
        # cor(unlist(r.ipv),unlist(ffipv),use="complete")
        # cor(unlist(r.ipv),unlist(clipv),use="complete")
        # cor(unlist(clipv),unlist(ffipv),use="complete")        
        # 