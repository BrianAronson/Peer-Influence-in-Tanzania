#Stats
#0 - Set work directory
    setwd("/misc/utopia3/bda13/lanhome/Data/Shared/Tanzania/Code") #Alternatives: setwd("C:/Users/bda13/Desktop/Sociology/Papers in Progress/Tanzania/Code")
    setwd("C:/Users/bda13/Desktop/Tanzania/Code")
    setwd("C:/Users/bda13/Dropbox/AWS/Tanzania/Code")

#1) load data
    source("Ward 1 - load data.R")

#2) kill camps you don't like
    sumstats2<-readRDS("sumstats2.rds")
    #get real N
        sumstats2$N1<-sapply(wave,length)
    #get real ward
        ward<-1:length(dv.ipvoutcome_cat)
        tward1<-sapply(ward1, function(x) x[1])
        tward2<-sapply(ward2, function(x) x[1])
        tward3<-sapply(ward3, function(x) x[1])
        tward4<-sapply(ward4, function(x) x[1])
        ward[tward1==1]<-1
        ward[tward2==1]<-2
        ward[tward3==1]<-3
        ward[tward4==1]<-4
        sumstats2$ward<-ward
    #subset data
        sumstats3<-sumstats2
        sumstats3<-sumstats3[sumstats3$N>19 & sumstats3$N<50,]
        sumstats3<-sumstats3[sumstats3$missing<.2,]
        sumstats3<-sumstats3[sumstats3$ffjaccard>.15,]
        sumstats3<-sumstats3[sumstats3$Camp!=15,]  #Camp 15 is problematic
        nrow(sumstats3)
        table(paste(sumstats3$Wave,sumstats3$ward))
        kill<-paste(sumstats2$Camp,sumstats2$Wave) %in% paste(sumstats3$Camp,sumstats3$Wave)
        a<-c(varlist,sumvars)
        for(i in 1:length(a)){
          a[[i]]<-a[[i]][kill]
        }
        list2env(a,globalenv())
        noDV<-0

    
#3) prepare dataset
    source("Ward 2 - aggregate objects to wards.R")
    source("Ward 3 - prep siena objects.R")

#3.5) grab summary info on wards
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
                stringsAsFactors = F
              )

            lsdf[[j]]<-sdf
        }
            #bind all info
            out<-do.call(cbind,lsdf)
            names(out)[1]<-"aname"
            out<-out[,!names(out)=="name"]
            out[,2:ncol(out)]<-sapply(out[,2:ncol(out)],function(x) format(round(as.numeric(x), 2), nsmall = 2))
            out[,2:ncol(out)]<-sapply(out[,2:ncol(out)],function(x) gsub(" ","",x))
            out[,2:ncol(out)]<-sapply(out[,2:ncol(out)],function(x) paste("'",x,sep=""))
            trow<-out$aname %in% c("Individuals","SES","Age","Camps","Network Attributes","Population")
            out[trow,2:ncol(out)]<-""
            
            write.csv(out,"C:/Users/bda13/Desktop/Table 1 - Descriptives 7-9-19.csv")


            length(unique(unlist(sapply(dataset2,function(x) x$cCovars$pid))))
            