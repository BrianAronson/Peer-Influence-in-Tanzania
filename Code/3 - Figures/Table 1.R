#Stats
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

        
#4) grab summary info on wards
    ward1w1camps<-unique(c(dataset[[1]]$cCovars$campn))
    ward2w1camps<-unique(c(dataset[[2]]$cCovars$campn))
    ward3w1camps<-unique(c(dataset[[3]]$cCovars$campn))
    ward4w1camps<-unique(c(dataset[[4]]$cCovars$campn))
    ward1w2camps<-unique(c(dataset[[5]]$cCovars$campn))
    ward2w2camps<-unique(c(dataset[[6]]$cCovars$campn))
    ward3w2camps<-unique(c(dataset[[7]]$cCovars$campn))
    ward4w2camps<-unique(c(dataset[[8]]$cCovars$campn))
    sumstats3$ward<-0
    sumstats3$ward[sumstats3$Camp%in%ward1w1camps | sumstats3$Camp%in%ward1w2camps]<-1
    sumstats3$ward[sumstats3$Camp%in%ward2w1camps | sumstats3$Camp%in%ward2w2camps]<-2
    sumstats3$ward[sumstats3$Camp%in%ward3w1camps | sumstats3$Camp%in%ward3w2camps]<-3
    sumstats3$ward[sumstats3$Camp%in%ward4w1camps | sumstats3$Camp%in%ward4w2camps]<-4

#4) Grab all relevant data
    #a) Create list of wave-wards
        dataset2<-dataset#[c(-1,-3)]
    #b) For each wave-ward
        lsdf<-list()
        l.df.analyses <- list()
        tmp <- list()
        
        sum(sapply(tmp, length))
        
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

            n2<-td$dycCovars$ffriendship
            n2<-n2[keeps,keeps]
            ln2<-list()
            for(i in 1:length(uc1)){
              ln2[[i]]<-n2[c1==uc1[i],c1==uc1[i]]
            }

        #d) Extract hiv status
            
            hiv1<-td$depvars$DV[,,1]
            hiv1<-hiv1[keeps]
            tmp[[j]] <- hiv1
            lhiv1<-split(hiv1,c1)
        #e) Extract covariate control variables
            male1<-as.numeric(td$cCovars$male>0)[keeps]
            sex.num1<-as.numeric(td$cCovars$sex.num2>0)[keeps]
            leader1<-as.numeric(td$cCovars$leader>0)[keeps]
            #ses
                temp<-c(td$cCovars$ses)
                vals<-sort(unique(temp))
                rng<-attr(td$cCovars$ses,"range2")
                rng<-rng[1]:rng[2]
                for(i in 1:length(vals)){
                  temp[temp==vals[i]]<-rng[i]
                }
                ses1<-temp[keeps]
                #break out
                    sesa<-ses1
                    ses0<-as.numeric(sesa==0)
                    ses1<-as.numeric(sesa==1)
                    ses2<-as.numeric(sesa==2)

            #age
                temp<-c(td$cCovars$age)
                vals<-sort(unique(temp))
                rng<-attr(td$cCovars$age,"range2")
                rng<-rng[1]:rng[2]
                for(i in 1:length(vals)){
                  temp[temp==vals[i]]<-rng[i]
                }
                age1<-temp[keeps]
                #break out
                    agea<-age1
                    age0<-as.numeric(agea==0)
                    age1<-as.numeric(agea==1)
                    age2<-as.numeric(agea==2)
                    age3<-as.numeric(agea==2)

        #f) Extract dyadic control variables
              outdegl<-list()
              indegl<-list()
              ffwoutdegl<-list()
              ffwindegl<-list()
              for(i in 1:length(ln1)){
                outdegl[[i]]<-apply(ln1[[i]],1,sum)
                indegl[[i]]<-apply(ln1[[i]],2,sum)
                ffwoutdegl[[i]]<-apply(ln2[[i]],1,sum)
                ffwindegl[[i]]<-apply(ln2[[i]],2,sum)
              }
              outdegl<-unlist(outdegl)[keeps]
              indegl<-unlist(indegl)[keeps]

                #outdegree
            #indegree
            #fullfriend ties
            #transitive ties

        #g) extract network stats
            lg<-lapply(ln1,function(x) graph_from_adjacency_matrix(x,diag=F,mode="directed"))
            assort<-vector()
            recip<-vector()
            dens<-vector()
            trans<-vector()
            for(i in 1:length(lg)){
              V(lg[[i]])$hivtst<-lhiv1[[i]]
              assort[i]<-assortativity(lg[[i]],V(lg[[i]])$hivtst)
              recip[i]<-reciprocity(lg[[i]])
              dens[i]<-edge_density(lg[[i]])
              trans[i]<-transitivity(lg[[i]])
            }
            ncamps<-length(uc1)
            N<-length(td$nodeSets$Actors)
            Npercamp<-N/ncamps
                if(j==1){
                  tward=1
                  twave=1
                }
                if(j==2){
                  tward=2
                  twave=1
                }
                if(j==3){
                  tward=3
                  twave=1
                }
                if(j==4){
                  tward=4
                  twave=1
                }
                if(j==5){
                  tward=1
                  twave=3
                }
                if(j==6){
                  tward=2
                  twave=3
                }
                if(j==7){
                  tward=3
                  twave=3
                }
                if(j==8){
                  tward=4
                  twave=3
                }
            
            tjac<-sumstats3$jaccard[sumstats3$ward==tward & sumstats3$Wave==twave]
            tmis<-sumstats3$missing[sumstats3$ward==tward & sumstats3$Wave==twave]
            jaccard<-mean(tjac)
            missing<-mean(tmis)
        #h) tally all stats
            lstats <-
              list(
                   0,
                   hiv1,
                   male1,
                   sex.num1,
                   leader1,
                   0,
                   ses0,
                   ses1,
                   ses2,
                   0,
                   age0,
                   age1,
                   age2,
                   age3,
                   N,
                   0,
                   0,
                   dens,
                   recip,
                   trans,
                   assort,
                   jaccard,
                   missing,
                   0,
                   ncamps,
                   Npercamp
              )

            sdf <-
              data.frame(
                name = c(
                "Individuals",
                  "Tested for HIV",
                  "Male",
                  "Any sex last year",
                  "Camp leader",
                  "SES",
                  "  Low",
                  "  Medium",
                  "  High",
                  "Age",
                  "  15-19",
                  "  20-24",
                  "  25-29",
                  "  30+",
                  "N",
                "Camps",
                  "Network Attributes",
                  "  Density",
                  "  Reciprocity",
                  "  Transitivity",
                  "  Assortativity (HIV)",
                  "  Jaccard index",
                  "  Missing (proportion)",
                  "Population",
                  "  Number camps",
                  "  N per Camp"
                ),
                mean = sapply(lstats, function(x)
                  mean(x, na.rm = T)),
                sd = sapply(lstats, function(x)
                    sd(x, na.rm = T)),
                stringsAsFactors = F
              )

            lsdf[[j]]<-sdf
            l.df.analyses[[j]] <- lstats
        }
            #bind all info
            out<-do.call(cbind,lsdf)
            names(out)[1]<-"aname"
            out<-out[,!names(out)=="name"]
            

            

#NEW - collapse to waves        
    out <- out[, !grepl("sd", names(out))]
    #count population in original
        out[24, 2:9] <- out[26, 2:9] * out[25, 2:9]
    #create new empty df
        out2 <- out[,1:3]
        out2[,2:3] <- 0
    #count population stats
        out2[24,2:3] <- c(sum(out[24, 2:5]),sum(out[24, 6:9]))
        out2[25,2:3] <- c(sum(out[25, 2:5]),sum(out[25, 6:9]))
        out2[26,2:3] <- out2[24, 2:3]/out2[25, 2:3]
    #get person-level wave averages
        for(i in 2:14){
            out2[i,2:3] <- c(sum(out[i,2:5]*out[24,2:5]) / out2[24,2], sum(out[i,6:9]*out[24,6:9]) / out2[24,3])
        }
    #get network-level wave averages
        for(i in 18:23){
            out2[i, 2:3] <- c(sum(out[i, 2:5] * out[25, 2:5]) / out2[25, 2], sum(out[i, 6:9] * out[25, 6:9]) / out2[25, 3])
        }
        out<-out2

            #format
            out[,2:ncol(out)]<-sapply(out[,2:ncol(out)],function(x) format(round(as.numeric(x), 2), nsmall = 2))
            out[,2:ncol(out)]<-sapply(out[,2:ncol(out)],function(x) gsub(" ","",x))
            out[,2:ncol(out)]<-sapply(out[,2:ncol(out)],function(x) paste("'",x,sep=""))
            trow<-out$aname %in% c("Individuals","SES","Age","Camps","Network Attributes","Population")
            out[trow,2:ncol(out)]<-""
            
            write.csv(out,"C:/Users/bda13/Desktop/Table 1 - Descriptives 7-9-19.csv")


            length(unique(unlist(sapply(dataset2,function(x) x$cCovars$pid))))
            
    
            
            
#            
    #bind results to a table
        l.df <- list()
        for(i in 1:length(l.df.analyses)){
            df <- l.df.analyses[[i]]
            tmp <- sapply(df, length)
            keep.vars <- tmp == max(tmp)
            df <- df[keep.vars]
            df <- data.table(matrix(unlist(df), ncol=length(df), byrow=F),stringsAsFactors=FALSE)
            names(df) <- sdf$name[keep.vars]
            l.df[[i]] <- df
        }
        df.w1 <- rbindlist(l.df[1:4])
        df.w2 <- rbindlist(l.df[5:8])
    
        t(df.w1[, lapply(.SD, function(x) c(mean(x, na.rm = T), sd(x, na.rm = T)))])
        t(df.w2[, lapply(.SD, function(x) c(mean(x, na.rm = T), sd(x, na.rm = T)))])
        

        
#different approach    
    covars <- list(list())
    for(i in 1:length(datasetg)){
        covars[[i]] <- list()
        for(j in 1:length(datasetg[[i]]$vCovars)){
            covars[[i]][[j]] <- datasetg[[i]]$vCovars[[j]][,1]
        }
    }
    
    l.df <- list()
    for(i in 1:length(covars)){
        l.df[[i]] <- data.table(matrix(unlist(covars[[i]]), ncol=length(covars[[i]]), byrow=F),stringsAsFactors=FALSE)    
    }
    df.w1 <- rbindlist(l.df[1:4])
    df.w2 <- rbindlist(l.df[5:8])
    names(df.w1) <- names(datasetg[[1]]$vCovars)
    names(df.w2) <- names(datasetg[[1]]$vCovars)
    
    df.m.w1 <- df.w1[df.w1$pid %in% df.w2$pid]
    df.m.w2 <- df.w2[df.w2$pid %in% df.w1$pid]


mapply(function(x,y) cor(x,y, use = "complete"), df.m.w1, df.m.w2)



depvars <- list()
for(i in 1:length(datasetg)){
    depvars[[i]] <- datasetg[[i]]$depvars$DV[,,]
}

l.df <- list()
for(i in 1:length(depvars)){
    l.df[[i]] <- data.table(depvars[[i]])    
}
dvf.w1 <- rbindlist(l.df[1:4])
dvf.w2 <- rbindlist(l.df[5:8])
cor(dvf.w1, use = "complete")
cor(dvf.w2, use = "complete")
