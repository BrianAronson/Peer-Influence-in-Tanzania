#1 - Heat map of correct guessing alter's testing behavior
    #create correlation matrix
        tdf2<-data.frame(id=df$pid, test=df$m_testhiv_12)
        tdf3<-data.frame(id=c(df$m_frnd1pid,df$m_frnd2pid),thinktest=c(df$b_hivtstf1,df$b_hivtstf2))
        tdf4<-merge(tdf2,tdf3,by="id")
        melted_cormat<-data.frame(value=
                c(prop.table(table(tdf4$test==1 & tdf4$thinktest==1))[2],
                prop.table(table(tdf4$test==1 & tdf4$thinktest==0))[2],
                prop.table(table(tdf4$test==0 & tdf4$thinktest==1))[2],
                prop.table(table(tdf4$test==0 & tdf4$thinktest==0))[2]),
                Var1=c("Tested","Tested","Not Tested","Not Tested"),
                Var2=c("Believed Tested","Not Believed Tested","Believed Tested","Not Believed Tested")  
                )
        melted_cormat$value2<-c(1,0,0,1)+melted_cormat$value
        melted_cormat$value<-round(melted_cormat$value,2)
        #row.names(cormat)<-c("Tested","Not Tested")
        #colnames(cormat)<-c("Considered Tested","Not Considered Tested")
    #create basic heatmap
        ggheatmap<-
        ggplot(data = melted_cormat, aes(Var2, Var1, fill = value2))+
          geom_tile(color = "white")+
          scale_fill_gradient2(low = "red4", high = "green4",
                               midpoint = .8, limit = c(0,2), space = "Lab",
                               name="Pearson\nCorrelation") +
          theme_minimal()+
          theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 30, hjust = 1),
                axis.text.y = element_text(size = 30))+
          coord_fixed()
    #finish heatmap
        png("C:/Users/bda13/Desktop/Fig 1 - AlterHIV Accuracy.png", width=900, height=700)
        ggheatmap +
          geom_text(aes(Var2, Var1, label = value), color = "black", size = 10) +
          theme(
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            panel.grid.major = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.ticks = element_blank(),
            plot.title = element_text(size=42, hjust=.53),
            plot.margin = margin(.5, 4, .5, .5, "cm"))+
          guides(fill = FALSE)+
          ggtitle("Accuracy of Alter HIV Testing Beliefs")
        #geom_vline(xintercept = 5.54,size=3)
        dev.off()

#2 - Descriptive statistics
    tdf5<-tdf[,c("m_testhiv_12","m_hivtstfalter","m_hivinjfalter","m_hivadvfalter","r.hivtst.2","r.hivinj.2","r.hivadv.2")]
    tdf6<-data.frame(Value=sapply(tdf5,function(x) mean(x,na.rm=T)))
    tdf6$sd<-sapply(tdf5,function(x) sd(x,na.rm=T))
    tdf6$Variable<-row.names(tdf6)
    vars<-c("Ego-Tested","Alters-Test","Alter-Should","Alters-Advertise", "Camp-Test","Camp-Should","Camp-Advertise")
    tdf6$Variable=factor(vars,levels=vars)
    
    png("C:/Users/bda13/Desktop/Fig 2 - Descriptive Stats.png", width=900, height=700)
    ggplot(tdf6, aes(x=Variable, y=Value)) + 
      geom_bar(color="black",fill="skyblue2",position=position_dodge(), width=.5, stat="identity")+
      geom_errorbar(aes(ymin=Value-sd, ymax=ifelse(Value+sd>1,1,Value+sd)),
                    width=.2,                    
                    position=position_dodge(.9))+
      theme_classic()+
      theme(axis.text.x=element_text(size=20,angle = 45,hjust=1),
            axis.text.y=element_text(size=20),
            axis.title = element_text(size=24),
            plot.title = element_text(size=30,hjust = 0.5),
            plot.margin = margin(1, 1, 1, 1, "cm"),
            axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
            axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0))
      )+
      ggtitle("Distribution of Key Variables")+
      scale_y_continuous(breaks=c(0,.25,.5,.75,1))
    dev.off()



#3 - Correlation of norms
    #create correlation matrix
        cormat<-cor(tdf[,c("m_hivtstfalter","m_hivinjfalter","m_hivadvfalter","r.hivtst.2","r.hivinj.2","r.hivadv.2")],use="complete")
        row.names(cormat)<-c("Alter-Tested","Alter-Should","Alter-Advertised","Camp-Tested","Camp-Should","Camp-Advertised")
        colnames(cormat)<-c("Alter-Tested","Alter-Should","Alter-Advertised","Camp-Tested","Camp-Should","Camp-Advertised")
    #reformat for graphing
        cormat<-round(cormat,2)
        get_upper_tri <- function(cormat){
          cormat[lower.tri(cormat)]<- NA
          return(cormat)
        }
        upper_tri <- get_upper_tri(cormat)
        library(reshape2)
        melted_cormat <- melt(upper_tri, na.rm = TRUE)
        melted_cormat[melted_cormat$Var1==melted_cormat$Var2,]$value<-0
        melted_cormat$val2<-melted_cormat$value
        melted_cormat$val2[melted_cormat$val2==0]<-""
    #create basic heatmap
        ggheatmap<-ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
          geom_tile(color = "white")+
          scale_fill_gradient2(low = "blue4", high = "red4",
                               midpoint = 0, limit = c(-1,1), space = "Lab",
                               name="Pearson\nCorrelation") +
          theme_minimal()+
          theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 20, hjust = 1),
                axis.text.y = element_text(size = 20))+
          coord_fixed()
    #finish heatmap
        png("C:/Users/bda13/Desktop/Fig 3 - Correlations.png", width=1050, height=650)
        ggheatmap +
          geom_text(aes(Var2, Var1, label = val2), color = "black", size = 10) +
          theme(
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            panel.grid.major = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.ticks = element_blank(),
            legend.text = element_text(size = 24),
            legend.title = element_text(size = 24),
            plot.title = element_text(size=42, hjust=.38),
            plot.margin = margin(.5, 4, 0, 0, "cm"))+
          guides(fill = guide_colorbar(barwidth = 4, barheight = 15))+
          ggtitle("Correlations Among HIV Norms")
          #geom_vline(xintercept = 5.54,size=3)
        dev.off()


#4 - Graph network parameter estimates
    #load packages
        library(XML)
        library(RCurl)
        library(rlist)
        library(stringr)
    #identify file names of results
        filedir<-"C:/Users/bda13/Desktop/Hiv models/"
        filenames<-list.files(filedir)
        filenames<-paste(filedir,filenames,sep="")
    #Read url
        tdf6 <- readHTMLTable(filenames[7])[[1]]
    #kill empty rows
        tdf6[]<-lapply(tdf6,as.character)
        remove<-rowSums(as.data.frame(ifelse(is.na(tdf6),1,0)))>0 | grepl("N.A",tdf6$par.)
        tdf6<-tdf6[!remove,]
    #convert to numbers
        regexp <- "[[:digit:]]+\\.[[:digit:]]+"
        tdf6$Estimate<-as.numeric(unlist(str_extract_all(tdf6$par., regexp)))
        tdf6$`Std. Error`<-as.numeric(unlist(str_extract_all(tdf6$`(s.e.)`, regexp)))
        tdf6$Variable<-sapply(tdf6$Effect, function(x) tail(strsplit(x,split=" ")[[1]],1) )
    #subset to relevant rows manually
        tdf7<-tdf6[41:54,]
        tdf7$Variable<-tdf7$Effect
    #graph it
        #create CI variable
            tdf7$CIlower<-tdf7$Estimate-tdf7$`Std. Error`*1.96
            tdf7$CIupper<-tdf7$Estimate+tdf7$`Std. Error`*1.96
        #create color variable
            tdf7$Significant<-factor(ifelse(sign(tdf7$CIlower)==sign(tdf7$CIupper),1,0))
        #keep variable order
            #tdf7$Variable<-factor(tdf7$Variable,levels=tdf7$Variable)
        #Visualize
           png(file="C:/Users/bda13/Desktop/Fig 4 - Friendship Selection Siena Estimates.png", height=800, width=1200)
            p <- ggplot(tdf7, aes(y=Estimate,x=reorder(Variable, Estimate),color=Significant))
            p + geom_pointrange(aes(ymin=CIlower,ymax=CIupper),size=3) + coord_flip()+
            labs(x="Parameters", y="Parameter Estimates")+
            scale_y_continuous(limits=c(-4.5,4.5))+
            geom_hline(yintercept = 0)+
            theme_bw() +
            theme(panel.border = element_blank(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                axis.line = element_line(colour = "black"),
                axis.text=element_text(size=24),
                title =element_text(size=32, face='bold'),
                plot.title = element_text(hjust=-1.80,margin = margin(t = 0, r = 0, b = 20, l = 0)),
                legend.position="none",
                plot.margin = margin(1, 1, 1, 1, "cm"),
                axis.title.y = element_text(size=30,face="bold", margin = margin(t = 0, r = 20, b = 0, l = 0)),
                axis.title.x = element_text(size=28,face="bold", margin = margin(t = 20, r = 0, b = 0, l = 0))
            )+
              ggtitle("Friendship Selection Siena Estimates")
             dev.off()
        
      #6 - Graph behavior parameter estimates           
             #subset to relevant rows manually
                 tdf7<-tdf6[c(96:100,102),]
                 #tdf7$Variable<-tdf7$Effect
             #graph it
                 #create CI variable
                     tdf7$CIlower<-tdf7$Estimate-tdf7$`Std. Error`*1.96
                     tdf7$CIupper<-tdf7$Estimate+tdf7$`Std. Error`*1.96
                 #create color variable
                     tdf7$Significant<-factor(ifelse(sign(tdf7$CIlower)==sign(tdf7$CIupper),1,0))
                 #keep variable order
                     #tdf7$Variable<-factor(tdf7$Variable,levels=tdf7$Variable)
                 #Visualize
                    png(file="C:/Users/bda13/Desktop/Fig 6 - Behavior Siena Estimates.png", height=800, width=1200)
                     p <- ggplot(tdf7, aes(y=Estimate,x=reorder(Variable, Estimate),color=Significant))
                     p + geom_pointrange(aes(ymin=CIlower,ymax=CIupper),size=3) + coord_flip()+
                     labs(x="Parameters", y="Parameter Estimates")+
                     scale_y_continuous(limits=c(-4.5,4.5))+
                     geom_hline(yintercept = 0)+
                     theme_bw() +
                     theme(panel.border = element_blank(),
                         panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank(),
                         axis.line = element_line(colour = "black"),
                         axis.text=element_text(size=24),
                         title =element_text(size=32, face='bold'),
                         plot.title = element_text(margin = margin(t = 0, r = 0, b = 20, l = 0)),
                         legend.position="none",
                         plot.margin = margin(1, 1, 1, 1, "cm"),
                         axis.title.y = element_text(size=30,face="bold", margin = margin(t = 0, r = 20, b = 0, l = 0)),
                         axis.title.x = element_text(size=28,face="bold", margin = margin(t = 20, r = 0, b = 0, l = 0))
                     )+
                       ggtitle("Behavior Control Estimates")
                      dev.off()

#7 - Graph outcome parameter estimates
    #Read url
        tdf8 <- readHTMLTable(filenames[7])[[1]]
    #kill empty rows
        tdf8[]<-lapply(tdf8,as.character)
        remove<-rowSums(as.data.frame(ifelse(is.na(tdf8),1,0)))>0 | grepl("N.A",tdf8$par.)
        tdf8<-tdf8[!remove,]
    #convert to numbers
        regexp <- "[[:digit:]]+\\.[[:digit:]]+"
        tdf8$Estimate<-as.numeric(unlist(str_extract_all(tdf8$par., regexp)))
        tdf8$`Std. Error`<-as.numeric(unlist(str_extract_all(tdf8$`(s.e.)`, regexp)))
        tdf8$Variable<-sapply(tdf8$Effect, function(x) tail(strsplit(x,split=" ")[[1]],1) )
    #identify variables to not keep
        nokeep<-tdf8$Effect[c(1:100,102)]
    #for each file in filename, just get the row with the outcome of interest
        parameters<-list()
        for(i in 1:length(filenames)){
            tdf8 <- readHTMLTable(filenames[i])[[1]]
        #kill empty rows
            tdf8[]<-lapply(tdf8,as.character)
            remove<-rowSums(as.data.frame(ifelse(is.na(tdf8),1,0)))>0 | grepl("N.A",tdf8$par.)
            tdf8<-tdf8[!remove,]
        #convert to numbers
            regexp <- "[[:digit:]]+\\.[[:digit:]]+"
            tdf8$Estimate<-as.numeric(unlist(str_extract_all(tdf8$par., regexp)))
            tdf8$`Std. Error`<-as.numeric(unlist(str_extract_all(tdf8$`(s.e.)`, regexp)))
            tdf8$Variable<-sapply(tdf8$Effect, function(x) tail(strsplit(x,split=" ")[[1]],1) )
        #identify variables to not keep
            parameters[[i]]<-tdf8[!(tdf8$Effect %in% nokeep),]}
    #bind these together
        tdf8<-do.call(rbind,parameters)
    #graph it
        #create CI variable
            tdf8$CIlower<-tdf8$Estimate-tdf8$`Std. Error`*1.96
            tdf8$CIupper<-tdf8$Estimate+tdf8$`Std. Error`*1.96
        #create color variable
            tdf8$Significant<-factor(ifelse(sign(tdf8$CIlower)==sign(tdf8$CIupper),1,0))
        #keep variable order
            tdf8<-tdf8[c(1:7,9:12),]
            tdf8$Variable<-c("Testing Similarity","Alter-Tested","Alter-Should","Alter-Advertised","Ego-Tested","Ego-Should","Ego-Advertised","Camp-Test-Rate","Camp-Tested","Camp-Should","Camp-Advertised")
            #tdf8$Variable<-factor(tdf8$Variable,levels=tdf8$Variable)
        #Visualize
           png(file="C:/Users/bda13/Desktop/Fig 7 - Independent Variable Estimates across Models.png", height=800, width=1200)
            p <- ggplot(tdf8, aes(y=Estimate,x=reorder(Variable, Estimate),color=Significant))
            p + geom_pointrange(aes(ymin=CIlower,ymax=CIupper),size=3) + coord_flip()+
            labs(x="Parameters", y="Parameter Estimates")+
            scale_y_continuous(limits=c(-4.5,4.5))+
            geom_hline(yintercept = 0)+
            theme_bw() +
            theme(panel.border = element_blank(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                axis.line = element_line(colour = "black"),
                axis.text=element_text(size=24),
                title =element_text(size=32, face='bold'),
                plot.title = element_text(hjust=-1.80,margin = margin(t = 0, r = 0, b = 20, l = 0)),
                legend.position="none",
                plot.margin = margin(1, 1, 1, 1, "cm"),
                axis.title.y = element_text(size=30,face="bold", margin = margin(t = 0, r = 20, b = 0, l = 0)),
                axis.title.x = element_text(size=28,face="bold", margin = margin(t = 20, r = 0, b = 0, l = 0))
            )+
              ggtitle("Independent Variable Estimates across Models")
             dev.off()
                      
             
             
#8 - Graph joint outcome parameter estimates
   #for each file in filename, just get the row with the outcome of interest
       filedir<-"C:/Users/bda13/Desktop/Hiv models2/"
       filenames<-list.files(filedir)
       filenames<-paste(filedir,filenames,sep="")
       parameters<-list()
       for(i in 1:length(filenames)){
           tdf8 <- readHTMLTable(filenames[i])[[1]]
       #kill empty rows
           tdf8[]<-lapply(tdf8,as.character)
           remove<-rowSums(as.data.frame(ifelse(is.na(tdf8),1,0)))>0 | grepl("N.A",tdf8$par.)
           tdf8<-tdf8[!remove,]
       #convert to numbers
           regexp <- "[[:digit:]]+\\.[[:digit:]]+"
           tdf8$Estimate<-as.numeric(unlist(str_extract_all(tdf8$par., regexp)))
           tdf8$`Std. Error`<-as.numeric(unlist(str_extract_all(tdf8$`(s.e.)`, regexp)))
           tdf8$Variable<-sapply(tdf8$Effect, function(x) tail(strsplit(x,split=" ")[[1]],1) )
       #identify variables to not keep
           parameters[[i]]<-tdf8[!(tdf8$Effect %in% nokeep),]}
   #bind these together
       tdf8<-do.call(rbind,parameters)
   #graph it
       #create CI variable
           tdf8$CIlower<-tdf8$Estimate-tdf8$`Std. Error`*1.96
           tdf8$CIupper<-tdf8$Estimate+tdf8$`Std. Error`*1.96
       #create color variable
           tdf8$Significant<-(ifelse(sign(tdf8$CIlower)==sign(tdf8$CIupper),1,0))
           tdf8$Significant<-factor(tdf8$Significant,levels=c(unique(as.character(tdf8$Significant)),""))
       #keep variable order
           #tdf8<-tdf8[c(1:7,9:12),]
           tdf8$Variable<-c("Alter-Should","Camp-Should","Alter-Advertised","Camp-Advertised","Alter-Tested","Camp-Tested")
           tdf8$Variable<-factor(tdf8$Variable,levels=c(tdf8$Variable,""))
           blank<-tdf8[1,]
           blank[1,]<-rep(1,9)
           tdf8<-rbind(tdf8[1:2,],blank,tdf8[3:4,],blank,tdf8[5:6,])
           tdf8$Estimate<-as.numeric(tdf8$Estimate)
           tdf8$CIlower<-as.numeric(tdf8$CIlower)
           tdf8$CIupper<-as.numeric(tdf8$CIupper)
           
           tdf8$Variable<-c("Alter-Should","Camp-Should"," ","Alter-Advertised","Camp-Advertised","  ","Alter-Tested","Camp-Tested")
           tdf8$Variable<-factor(tdf8$Variable,levels=c(rev(tdf8$Variable)))
           tdf8$Estimate[tdf8$Estimate==1]<-NA
        #Visualize
          png(file="C:/Users/bda13/Desktop/Fig 8 - Independent Variable Estimates within Models.png", height=800, width=1200)
           p <- ggplot(tdf8, aes(y=Estimate,x=Variable,color=Significant))
           p + geom_pointrange(aes(ymin=CIlower,ymax=CIupper),size=3) + coord_flip()+
           labs(x="Parameters", y="Parameter Estimates")+
#           scale_y_continuous(limits=c(-4.5,4.5))+
           geom_hline(yintercept = 0)+
           theme_bw() +
           theme(panel.border = element_blank(),
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               axis.line = element_line(colour = "black"),
               axis.text=element_text(size=24),
               title =element_text(size=32, face='bold'),
               plot.title = element_text(hjust=-1.80,margin = margin(t = 0, r = 0, b = 20, l = 0)),
               legend.position="none",
               plot.margin = margin(1, 1, 1, 1, "cm"),
               axis.title.y = element_text(size=30,face="bold", margin = margin(t = 0, r = 20, b = 0, l = 0)),
               axis.title.x = element_text(size=28,face="bold", margin = margin(t = 20, r = 0, b = 0, l = 0))
           )+
             ggtitle("Independent Variable Estimates within Models")
