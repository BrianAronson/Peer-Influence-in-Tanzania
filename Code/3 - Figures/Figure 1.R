#pull data
#0 - Set work directory
    data.dir <- "C:/Users/admin/Desktop/IDrive-Sync/Academic - Writing/Tanzania/Data"
    raw.data.dir <- file.path(data.dir, "Raw Data")
    der.data.dir <- file.path(data.dir, "Derived Data")
    code.prep.dir <- "C:/Users/admin/Desktop/github files/ZPosted/Tanzania Peer Influence/Code/1 - Data Prep"
    code.analysis.dir <- "C:/Users/admin/Desktop/github files/ZPosted/Tanzania Peer Influence/Code/2 - Analysis"
    code.figures.dir <- "C:/Users/admin/Desktop/github files/ZPosted/Tanzania Peer Influence/Code/1 - Data Prep/3 - Other Stats"


#1) load data
    source(file.path(code.prep.dir, "prep.1 - Load libraries.R"))
    source(file.path(code.analysis.dir, "Ward 1 - Load data and subset camps.R"))


#2) kill camps you don't like
    sumstats2 <- readRDS("sumstats2.rds")
    kill <- paste(sumstats2$Camp, sumstats2$Wave) %in% paste(sumstats3$Camp, sumstats3$Wave)
    #get real N
        sumstats3$N1 <- sapply(wave, length)
    #get real ward
        ward <- 1:length(dv.ipvoutcome_cat)
        tward1 <- sapply(ward1, function(x) x[1])
        tward2 <- sapply(ward2, function(x) x[1])
        tward3 <- sapply(ward3, function(x) x[1])
        tward4 <- sapply(ward4, function(x) x[1])
        ward[tward1 == 1] <- 1
        ward[tward2 == 1] <- 2
        ward[tward3 == 1] <- 3
        ward[tward4 == 1] <- 4
        sumstats3$ward <- ward
    #subset data
        a <- c(varlist, sumvars)
        for (i in 1:length(a)) {
            a[[i]] <- a[[i]][kill]
        }
        list2env(a, globalenv())
        noDV <- 0


#3) prepare dataset
    source(file.path(code.analysis.dir, "Ward 2 - Aggregate objects to wards.R"))
    source(file.path(code.analysis.dir, "Ward 3 - prep siena objects.R"))


#4) Create graph of all sociograms
    library(cowplot)
    library(ggplot2)
    library(extrafont)
    loadfonts(device = "win")
    #a) Pull camps, networks, and hiv status from dataset object
        #a) Create list of wave-wards
            dataset2<-dataset#[c(-1,-3)]
        #b) For each wave-ward
            lplots<-list()
            p<-list()
    for(j in 1:length(dataset2)){
            td<-dataset2[[j]]
        #c) Extract network
            n1<-td$depvars$friendship[,,1]
            c1<-td$cCovars$campn
            keeps<-!apply(n1,1,function(x) all(x==10))
            n1<-n1[keeps,keeps]
            c1<-c1[keeps]
            ln1<-list()
            uc1<-unique(c1)
            for(i in 1:length(uc1)){
              ln1[[i]]<-n1[c1==uc1[i],c1==uc1[i]]
            }
        #d) Extract hiv status
            hiv1<-td$depvars$DV[,,1]
            hiv1<-hiv1[keeps]
            lhiv1<-split(hiv1,c1)
            lg<-lapply(ln1,graph_from_adjacency_matrix)
            for(i in 1:length(lg)){
              V(lg[[i]])$hivtst<-lhiv1[[i]]
              V(lg[[i]])$colors<-V(lg[[i]])$hivtst
              V(lg[[i]])$camp<-uc1[i]
              V(lg[[i]])$camp2<-uc1[i]
              if(j==1){
                V(lg[[i]])$wardwave<-"Wave 1 (Ward 1)"
                V(lg[[i]])$camp2<-paste(V(lg[[i]])$camp2,"(Ward 1)",sep=" ")
              }
              if(j==2){
                V(lg[[i]])$wardwave<-"Wave 1 (Ward 2)"
                V(lg[[i]])$camp2<-paste(V(lg[[i]])$camp2,"(Ward 2)",sep=" ")
              }
              if(j==3){
                V(lg[[i]])$wardwave<-"Wave 1 (Ward 3)"
                V(lg[[i]])$camp2<-paste(V(lg[[i]])$camp2,"(Ward 3)",sep=" ")
              }
              if(j==4){
                V(lg[[i]])$wardwave<-"Wave 1 (Ward 4)"
                V(lg[[i]])$camp2<-paste(V(lg[[i]])$camp2,"(Ward 4)",sep=" ")
              }
              if(j==5){
                V(lg[[i]])$wardwave<-"Wave 2 (Ward 1)"
                V(lg[[i]])$camp2<-paste(V(lg[[i]])$camp2,"(Ward 1)",sep=" ")
              }
              if(j==6){
                V(lg[[i]])$wardwave<-"Wave 2 (Ward 2)"
                V(lg[[i]])$camp2<-paste(V(lg[[i]])$camp2,"(Ward 2)",sep=" ")
              }
              if(j==7){
                V(lg[[i]])$wardwave<-"Wave 2 (Ward 3)"
                V(lg[[i]])$camp2<-paste(V(lg[[i]])$camp2,"(Ward 3)",sep=" ")
              }
              if(j==8){
                V(lg[[i]])$wardwave<-"Wave 2 (Ward 4)"
                V(lg[[i]])$camp2<-paste(V(lg[[i]])$camp2,"(Ward 4)",sep=" ")
              }
            }
            lg<-lapply(lg, function(x) decompose.graph(x)[[1]])
        #e) Find assortativity
            for(i in 1:length(lg)){
              print(assortativity(lg[[i]],V(lg[[i]])$hivtst))
            }

    #b) Plot each graph
        #i) create graph function
            prettyplot<-function(x) {
              set.seed(1)
              df<-layout.fruchterman.reingold(x)
              df<-as.data.frame(df)
              df$colors<-V(x)$colors
              df$colors<-ifelse(df$colors==1,"grey50","white")
              nsize<-2.5 #125/nrow(df)
              temp.title <- paste("Camp", V(x)$camp2[1])
              #create edges
                  edg<-as.data.frame(as_edgelist(x))
                  edg2<-data.frame(xbeg=rep(0,nrow(edg)),ybeg=0,xend=0,yend=0)
                  edg2[,1]<-df[,1][edg[,1]]
                  edg2[,2]<-df[,2][edg[,1]]
                  edg2[,3]<-df[,1][edg[,2]]
                  edg2[,4]<-df[,2][edg[,2]]
              out<-ggplot()+
                geom_segment(data = edg2,aes(x = xbeg, y = ybeg, xend = xend, yend = yend),size=nsize/30)+
                geom_point(data=df,aes(x=V1,y=V2),size=nsize*1.25,color="black",fill=(df$colors),shape=21)+
                theme_void()+
                theme(
                    text=element_text(family="serif"),
                    plot.margin = unit(c(.2, .35, .5, .35), "cm"),
                    plot.title = element_text(hjust = 0.5,size=15,margin = margin(t = 0, r = 0, b = 15, l = 0)),
                    plot.subtitle = element_text(hjust = 0.5,size=12,margin = margin(t = 0, r = 0, b = 10, l = 0)),
                    plot.background = element_rect(color = "grey")
                    )+
                labs(
                  title = paste("Camp",V(x)$camp2[1])
                  # title=paste("Camp",V(x)$camp[1]),
                  # subtitle = paste(V(lg[[i]])$wardwave[1])
                ) 
                # annotate(geom = 'segment',
                #          y = Inf,
                #          yend = Inf,
                #          x = -Inf,
                #          xend = Inf,
                #          size = 1)
              return(out)
            }

            
        #ii) plot each graph
            lplots[[j]]<-lapply(lg,prettyplot)
        #iii) Plot to grid and end loop
            p[[j]]<-plot_grid(plotlist=lplots[[j]])
    }
    #c) put all plots in one big list, plot, title, and save
        lplots2<-unlist(lplots, recursive=FALSE)
        #remove duplicate camps
            a<-unlist(sapply(dataset,function(x) unique(x$cCovars$campn)))
            length(a)
            unique(a)
            a1<-!duplicated(a)
            lplots2<-lplots2[a1]
        #sort by camp
            a2<-a[a1]
            lplots2<-lplots2[order(a2)]
        # #sort by camp
        #     tmp<-(sapply(dataset,function(x) unique(x$cCovars$campn)))
        #     tmp2<-sapply(tmp,length)
        #     tward<-unlist(mapply(rep,c(1:4,1:4),tmp2))
        #     tward1<-tward[a1]
        #     lplots2<-lplots2[order(tward1)]
        p2 <- plot_grid(plotlist=lplots2)
        # title <- ggdraw() + draw_label("Figure 1 - Sociogram of All Camps in Sample", fontface='bold',size=22,fontfamily="serif")
        p2 <- plot_grid(title, p2,nrow=2,rel_heights=c(0.07, 1))
    #d) save
      save_plot("C:/Users/admin/Desktop/Figure1.png",p2,
                base_height=13, base_width=13 *.8)


