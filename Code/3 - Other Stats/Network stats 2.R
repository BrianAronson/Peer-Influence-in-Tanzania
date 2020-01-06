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
                }
                stats<-stats[1:i,]
                stats2[[j]]<-stats
              }
            gstats<-data.frame(camp=unicamps,
                               wave1N=stats2[[1]]$N,
                               wave2N=stats2[[2]]$N,
                               wave3N=stats2[[3]]$N,
                               wave1assort=stats2[[1]]$IPVAssortativity,
                               wave2assort=stats2[[2]]$IPVAssortativity,
                               wave3assort=stats2[[3]]$IPVAssortativity)

#3 - estimate tie propensity across variables
    #prep variables to adjust
        vnames<-c("b_","m_","e_")
        temp1l<-list()
        netl<-list(net1,net2,net3)
    for(i in 1:3){
    #create edge list
        temp1<-netl[[i]][netl[[i]]$tie==1,]
    #merge in all covariates
        temp1<-data.table(temp1)
        temp2<-data.table(df)
        temp3<-data.table(df)
        names(temp2)<-paste("s_",names(temp2),sep="")
        names(temp3)<-paste("r_",names(temp3),sep="")
        names(temp2)[1]<-"ID1"
        names(temp3)[1]<-"ID2"
        temp1<-merge(temp1,temp2,by="ID1",all.x=T)
        temp1<-merge(temp1,temp3,by="ID2",all.x=T)
        temp1l[[i]]<-as.data.frame(temp1)
    }
    #bring in dyadic variables
        #create dyadic var dfs
            a<-do.call(cbind,dyadicv1)
            a<-as.data.frame(a)
            b<-a[,names(a)=="tie"]
            b$ID1<-a[,1]
            b$ID2<-a[,2]
            keep<-paste(b$ID1,b$ID2) %in% paste(temp1l[[1]]$ID1,temp1l[[1]]$ID2)
            tempdyad1<-b[keep,]
            a<-do.call(cbind,dyadicv2)
            a<-as.data.frame(a)
            b<-a[,names(a)=="tie"]
            b$ID1<-a[,1]
            b$ID2<-a[,2]
            keep<-paste(b$ID1,b$ID2) %in% paste(temp1l[[2]]$ID1,temp1l[[2]]$ID2)
            tempdyad2<-b[keep,]
        #rename/remove info
            tempdyad1<-tempdyad1[,1:5]
            tempdyad2<-tempdyad2[,1:5]
            names(tempdyad1)<-c("hivtstf","hivinjf","hivadvf","knowf","closef")
            names(tempdyad2)<-c("hivtstf","hivinjf","hivadvf","knowf","closef")
        #bind with tie-based dataframes
            temp1l[[1]]<-cbind(temp1l[[1]],tempdyad1)
            temp1l[[2]]<-cbind(temp1l[[2]],tempdyad2)
    #test correlations
        #assortativity for monadic variables
            wav=1
            var<-"ssavlm" #ipvoutcome_bin gem_r15_avg testhiv testhiv_12
            cor(temp1l[[wav]][,paste("s_",vnames[wav],var,sep="")],temp1l[[wav]][,paste("r_",vnames[wav],var,sep="")],use="complete")
        #correlating two variables variables
            wav=1
            var1<-"ssavlm"
            var2<-"ssavlm"
            cor(temp1l[[wav]][,paste("s_",vnames[wav],var1,sep="")],temp1l[[wav]][,paste("r_",vnames[wav],var2,sep="")],use="complete")
        #correlating dyadic variables
            wav=2
            var1<-"hivadvf" #hivtstf hivinjf hivadvf knowf closef
            var2<-"testhiv_12"
            cor(temp1l[[wav]][,var1],temp1l[[wav]][,paste("r_",vnames[wav],var2,sep="")],use="complete")


#4 - Graph IPV
    #find assortativity in each graph
        adf<-data.frame(Assortativity=0)
        for(i in 1:length(dg1)){
          adf[i]<-assortativity(dg1[[i]],V(dg1[[i]])$b_ipvoutcome_bin)
        }
    
    #cherry pick best option
        sort(adf)
        gorder(dg1[[15]])
        table(V(dg1[[15]])$b_ipvoutcome_bin)
    #graph
        par(mar=c(0,0.1,1.1,0)+.1)
        plot.igraph(dg1[[15]], vertex.label=NA, edge.arrow.size=.01, vertex.size=10, vertex.color=ifelse(V(dg1[[15]])$b_ipvoutcome_bin == 1, "red", "blue"),  main = "Camp Network by Interpersonal Violence")
        legend("left",
               legend = c("Not Violent", "Violent"),
               col = c("blue",
                       "red"),
               pch = c(19,19),
               bty = "n",
               pt.cex = 2.1,
               cex = 1.5,
               text.col = "black")
          
#5 - Graph HIV
    #find assortativity in each graph
        adf<-data.frame(Assortativity=0,n=0,hiv=0)
        for(i in 1:length(dg1)){
          adf[i,1]<-assortativity(dg1[[i]],V(dg1[[i]])$b_testhiv)
          adf[i,2]<-gorder(dg1[[i]])
          adf[i,3]<-sum(V(dg1[[i]])$b_testhiv)
        }
    #cherry pick best option
        View(adf)
    #graph
        par(mar=c(0,5.1,2.1,0)+.1)
        i<-54
        set.seed(2)
        plot.igraph(dg1[[i]], vertex.label=NA, edge.arrow.size=.01, vertex.size=10, vertex.color=ifelse(V(dg1[[i]])$b_testhiv == 1, "red", "blue"))
        title("Camp Network by HIV Testing",cex.main=2)
        legend("left",
               legend = c("Not HIV Tested", "HIV Tested"),
               col = c("blue",
                       "red"),
               pch = c(19,19),
               bty = "n",
               pt.cex = 2.1,
               cex = 1.5,
               text.col = "black")

#6 - check if popularity is related to HIV/IPV
      V(g1)$outd<-degree(g1, mode = c("out"))
      V(g1)$ind<-degree(g1, mode = c("in"))
      cor(V(g1)$b_testhiv,V(g1)$outd)
      cor(V(g1)$b_testhiv[V(g1)$CONDITION==2],V(g1)$outd[V(g1)$CONDITION==2])
      cor(V(g1)$b_testhiv[V(g1)$CONDITION==1],V(g1)$outd[V(g1)$CONDITION==1])
      
      V(g2)$outd<-degree(g2, mode = c("out"))
      V(g2)$ind<-degree(g2, mode = c("in"))
      cor(V(g2)$m_testhiv_12,V(g2)$outd)
      cor(V(g2)$m_testhiv_12[V(g2)$CONDITION==2],V(g2)$outd[V(g2)$CONDITION==2])
      cor(V(g2)$m_testhiv_12[V(g2)$CONDITION==1],V(g2)$outd[V(g2)$CONDITION==1])
      
            
      V(g3)$outd<-degree(g3, mode = c("out"))
      V(g3)$ind<-degree(g3, mode = c("in"))
      cor(V(g3)$e_testhiv_12,V(g3)$outd)
      cor(V(g3)$e_testhiv_12[V(g3)$CONDITION==2],V(g3)$outd[V(g3)$CONDITION==2])
      cor(V(g3)$e_testhiv_12[V(g3)$CONDITION==1],V(g3)$outd[V(g3)$CONDITION==1])
      
      
            


          
        
#         V(gl[[i]])$b_married[is.na(V(gl[[i]])$b_married)]<-mean(V(gl[[i]])$b_married,na.rm=T)
#         V(gl[[i]])$b_gem_r15_avg[is.na(V(gl[[i]])$b_gem_r15_avg)]<-mean(V(gl[[i]])$b_gem_r15_avg,na.rm=T)
#         V(gl[[i]])$e_hivtstfego[is.na(V(gl[[i]])$e_hivtstfego)]<-mean(V(gl[[i]])$e_hivtstfego,na.rm=T)
#         V(gl[[i]])$e_hivtstfalter[is.na(V(gl[[i]])$e_hivtstfalter)]<-mean(V(gl[[i]])$e_hivtstfalter,na.rm=T)
#         
#         i=3
#         assortativity(gl[[i]],V(gl[[i]])$e_ipvoutcome_bin)
#         assortativity(gl[[i]],V(gl[[i]])$gender)
#         assortativity(gl[[i]],V(gl[[i]])$e_ipvoutcome_bin,V(gl[[i]])$gender)
# 
#         assortativity(gl[[i]],V(gl[[i]])$e_testhiv_12)
#         assortativity(gl[[i]],V(gl[[i]])$e_hivtstfalter)
#         assortativity(gl[[i]],V(gl[[i]])$e_hivtstfego)
#         assortativity(gl[[i]],V(gl[[i]])$e_testhiv_12,V(gl[[i]])$e_hivtstfalter)
#         assortativity(gl[[i]],V(gl[[i]])$e_hivtstfalter,V(gl[[i]])$e_testhiv_12)
#         assortativity(gl[[i]],V(gl[[i]])$e_testhiv_12,V(gl[[i]])$e_hivtstfego)
#         assortativity(gl[[i]],V(gl[[i]])$e_hivtstfego,V(gl[[i]])$e_testhiv_12)
#         
#         cor(df$b_hivtstfalter,df$b_hivtstfego,use="complete")
#         
#         df$e_hivtstfalter
#         #degree; centrality; 
#         

# #look at assortativity
#     assortativity(g1,V(g1)$b_ipvoutcome_bin)
#     assortativity(g1,V(g1)$b_gem_r15_avg)
#     assortativity(g1,V(g1)$b_testhiv)
# #look at assortativity
#     assortativity(g2,V(g2)$m_ipvoutcome_bin)
#     assortativity(g2,V(g2)$m_gem_r15_avg)
#     assortativity(g2,V(g2)$m_testhiv_12)
# #what about condition 1 vs 2
#     g22<-g2
#     g22<-delete.vertices(g22,names(V(g22))[V(g22)$CONDITION==2])
#     assortativity(g22,V(g22)$m_ipvoutcome_bin)
#     assortativity(g22,V(g22)$m_gem_r15_avg)
#     assortativity(g22,V(g22)$m_testhiv_12)
# #condition 1 vs 2
#     g32<-g3
#     g32<-delete.vertices(g32,names(V(g32))[V(g32)$CONDITION==1])
#     assortativity(g32,V(g32)$e_ipvoutcome_bin)
#     assortativity(g32,V(g32)$e_gem_r15_avg)
#     assortativity(g32,V(g32)$e_testhiv_12)





# #plot gem
#     range1.100 <- function(x){1 + 75*(x-min(x))/(max(x)-min(x))}
#     palf <- colorRampPalette(c("white", "blue"))
#     colr <- palf(100);
#     V(g1)$color<-V(g1)$b_gem_r15_avg
#     V(g1)$color[is.nan(V(g1)$color)]<-1
#     V(g1)$color[V(g1)$color>2.5]<-2.5
#     V(g1)$color<-(max(V(g1)$color))-(V(g1)$color)
#     V(g1)$color2 <- colr[round(range1.100(V(g1)$color))]
#     {set.seed(4)
#       plot.igraph(g1, vertex.label=NA, edge.arrow.size=.01, vertex.size=10, vertex.color=V(g1)$color2,  main = "Camp Network by Negative Gender Norms")}    
#     assortativity(g1,V(g1)$color)



# df$B_HIVTSTF1 #think friend has been tested
# df$B_HIVINJF1 #thinks friend thinks that he should have a test
# df$B_HIVADVF1 #friend encouraged ego to get tested
# df$B_KNOWF1 #friend known length
# df$B_CLOSEF1 #friend close
# df$b_testhiv #had an hiv test
# df$e_hivtst_c #thinks most camp members got tested
