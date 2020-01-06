#Perform after running step 3 in initial code

#1 - create igraph objects for each network within each wave
    unicamps<-unique(df$camp)
    #wave1
        #subset network
            sumnet1<-sumnet1[sumnet1$tie==1,]
        #convert to igraph
            g1 <- graph_from_data_frame(d = sumnet1, vertices = df[!is.na(df$b_respond),],directed = T)
        #fix nas
            V(g1)$b_gem_r15_avg[is.na(V(g1)$b_gem_r15_avg)]<-mean(V(g1)$b_gem_r15_avg,na.rm=T)
            V(g1)$b_testhiv[is.na(V(g1)$b_testhiv)]<-mean(V(g1)$b_testhiv,na.rm=T)
            V(g1)$b_testhiv_12<-V(g1)$b_testhiv
        #split into list of components
            dg1 <- decompose.graph(g1)
            dg1 <- list()
            for(i in 1:length(unicamps)){
              dg1[[i]]<-induced_subgraph(g1, which(V(g1)$camp==unicamps[i]))
            }

    #wave2
        #subset sumnetwork
            sumnet2<-sumnet2[sumnet2$tie==1,]
        #convert to igraph
            g2 <- graph_from_data_frame(d = sumnet2, vertices = df[!is.na(df$m_respond) & df$m_respond==1,],directed = T)
        #fix nas
            V(g2)$m_gem_r15_avg[is.na(V(g2)$m_gem_r15_avg)]<-mean(V(g2)$m_gem_r15_avg,na.rm=T)
            V(g2)$m_testhiv_12[is.na(V(g2)$m_testhiv_12)]<-mean(V(g2)$m_testhiv_12,na.rm=T)
        #split into list of components
            dg2 <- decompose.graph(g2)
            dg2 <- list()
            for(i in 1:length(unicamps)){
              dg2[[i]]<-induced_subgraph(g2, which(V(g2)$camp==unicamps[i]))
            }

    #wave 3
        #subset sumnetwork
            sumnet3<-sumnet3[sumnet3$tie==1,]
        #convert to igraph
            g3 <- graph_from_data_frame(d = sumnet3, vertices = df[!is.na(df$e_respond) & df$e_respond==1,],directed = T)
        #split into list of components
            dg3 <- decompose.graph(g3)
            dg3 <- list()
            for(i in 1:length(unicamps)){
              dg3[[i]]<-induced_subgraph(g3, which(V(g3)$camp==unicamps[i]))
            }

#2 - grab key stats for each camp, wave, and network
    #a - All camps
        #i - create list of graphs
            gl<-list(g1,g2,g3)
        #ii - prep data frame
            stats<-data.frame(Wave=1:3,Density=0,Reciprocity=0,Transitivity=0,MaxDiameter=0,IPVAssortativity=0,GEMAssortativity=0,HIVAssortativity=0)
        #iii - grab stats
            vnames<-c("b_","m_","e_")
            for(i in 1:nrow(stats)){
                stats$Density[i]<-edge_density(gl[[i]])
                stats$Reciprocity[i]<-reciprocity(gl[[i]])
                stats$Transitivity[i]<-transitivity(gl[[i]])
                stats$MaxDiameter[i]<-diameter(gl[[i]])
                name<-paste(vnames[i],"ipvoutcome_cat",sep="")
                stats$IPVAssortativity[i]<-eval(parse(text=paste("assortativity(gl[[i]],V(gl[[i]])$",name,")",sep="")))
                name<-paste(vnames[i],"gem_r15_avg",sep="")
                stats$GEMAssortativity[i]<-eval(parse(text=paste("assortativity(gl[[i]],V(gl[[i]])$",name,")",sep="")))
                name<-paste(vnames[i],"testhiv_12",sep="")
                stats$HIVAssortativity[i]<-eval(parse(text=paste("assortativity(gl[[i]],V(gl[[i]])$",name,")",sep="")))
            }

      #a - All camps
          #i - create list of graphs
              gll<-list(dg1,dg2,dg3)
          #ii - prep data frame
              stats<-data.frame(Wave=1:1000,Density=0,Reciprocity=0,Transitivity=0,MaxDiameter=0,IPVAssortativity=0,GEMAssortativity=0,HIVAssortativity=0)
              stats2<-list()
          #iii - grab stats
              vnames<-c("b_","m_","e_")
              for(j in 1:length(gll)){
                gl<-gll[[j]]
                stats<-data.frame(Wave=1:1000,Density=0,Reciprocity=0,Transitivity=0,N=0,IPVAssortativity=0,GEMAssortativity=0,HIVAssortativity=0)
                for(i in 1:length(gl)){
                  #get network stats
                      stats$Density[i]<-edge_density(gl[[i]])
                      stats$Reciprocity[i]<-reciprocity(gl[[i]])
                      stats$Transitivity[i]<-transitivity(gl[[i]])
                      stats$N[i]<-length(V(gl[[i]]))
                  #get assortativity stats
                      name<-paste(vnames[j],"ipvoutcome_cat",sep="")
                      stats$IPVAssortativity[i]<-eval(parse(text=paste("assortativity(gl[[i]],V(gl[[i]])$",name,")",sep="")))
                      name<-paste(vnames[j],"gem_r15_avg",sep="")
                      stats$GEMAssortativity[i]<-eval(parse(text=paste("assortativity(gl[[i]],V(gl[[i]])$",name,")",sep="")))
                      name<-paste(vnames[j],"testhiv_12",sep="")
                      V(gl[[i]])$b_testhiv_12<-V(gl[[i]])$b_testhiv
                      stats$HIVAssortativity[i]<-eval(parse(text=paste("assortativity(gl[[i]],V(gl[[i]])$",name,")",sep="")))
                  #get ipv count
                      name<-paste(vnames[j],"ipvoutcome_cat",sep="")
                      stats$IPVsum[i]<-eval(parse(text=paste("sum(V(gl[[i]])$",name,")",sep="")))
                }
                
                stats<-stats[1:i,]
                stats2[[j]]<-stats
              }
            gstats<-data.frame(camp=unicamps,
                               ipvsum1=stats2[[1]]$IPVsum,
                               ipvsum2=stats2[[2]]$IPVsum,
                               ipvsum3=stats2[[3]]$IPVsum,
                               wave1N=stats2[[1]]$N,
                               wave2N=stats2[[2]]$N,
                               wave3N=stats2[[3]]$N,
                               wave1assort=stats2[[1]]$IPVAssortativity,
                               wave2assort=stats2[[2]]$IPVAssortativity,
                               wave3assort=stats2[[3]]$IPVAssortativity)

