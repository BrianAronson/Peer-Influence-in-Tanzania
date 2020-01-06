
#0 - Set work directory
     setwd("/misc/utopia3/bda13/lanhome/Data/Shared/Tanzania/Code") #Alternatives: setwd("C:/Users/bda13/Desktop/Sociology/Papers in Progress/Tanzania/Code")
    # setwd("C:/Users/bda13/Desktop/Tanzania/Code")

#1) load data
    source("Ward 1 - load data.R")

#2) kill camps you don't like
    #identify camps
        sumstats2<-readRDS("sumstats2.rds")

        #bring wards into sumstats
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

        sumstats3<-sumstats2
        sumstats3<-sumstats3[sumstats3$N>15 & sumstats3$N<50,]
        sumstats3<-sumstats3[sumstats3$missing<.2,]
        sumstats3<-sumstats3[sumstats3$jaccard>.2,]
        # sumstats3<-sumstats3[sumstats3$d.hiv>.1,]
        # sumstats3<-sumstats3[sumstats3$Wave==3,]
        sumstats3<-sumstats3[sumstats3$Camp!=15,]  #Camp 15 is problematic
        nrow(sumstats3)
        unique(paste(sumstats3$ward,sumstats3$Wave))
        sumstats3[sumstats3$ward==3 & sumstats3$Wave==1,]
        sumstats3[sumstats3$ward==3 & sumstats3$Wave==3,]
        sumstats3<-sumstats3[!sumstats3$Camp %in% c(35,39,42),]


    #kill them
        kill<-paste(sumstats2$Camp,sumstats2$Wave) %in% paste(sumstats3$Camp,sumstats3$Wave)
        a<-c(varlist,sumvars)
        for(i in 1:length(a)){
          a[[i]]<-a[[i]][kill]
        }
        list2env(a,globalenv())

    #TEMPORARY - kill DV
        noDV<-1

#3) aggregate ward data
    source("Ward 2 - aggregate objects to wards.R")

#4) prep siena data for HIV (default = close friends for hiv)
    source("Ward 3 - prep siena objects.R")

#5) cut data
    conv<-vector()
    datasetg<-dataset[[3]]

    a<-dataset[[7]]$depvars$friendship[]
    b<-dataset[[3]]$depvars$friendship[]

    b1<-a[,,1]
    b1[b1==10]<-0
    t1<-table(apply(b1,1,sum))
    b1<-a[,,2]
    b1[b1==10]<-0
    t2<-table(apply(b1,1,sum))
    b1<-b[,,1]
    b1[b1==10]<-0
    t3<-table(apply(b1,1,sum))
    b1<-b[,,2]
    b1[b1==10]<-0
    t4<-table(apply(b1,1,sum))

    t1
    t2
    t3
    t4
    
    
    datasetg<-dataset[[3]]
    x1<-1
    x2<-10
    x3<-2
    datasetg$depvars$friendship[x1:x2,,x3][datasetg$depvars$friendship[x1:x2,,x3]==1]<-0
    
    
    
# #randomly replace a few ties in datasetg, wave1
#     datasetg<-dataset[[3]]
#     for(i in 1:50){
#       tn<-datasetg$depvars$friendship[,,2]
#       tod<-apply(tn,1,function(x) sum(x==1))
#       #sample random row
#           trow<-sample(dim(tn)[1],1)
#       #coin flip
#           addrm<-rbinom(1,1,.5)
#       #remove a tie
#           if(addrm==0 & tod[trow]>0){
#             tcol<-sample(tod[trow])[1]
#             tn[trow,][tn[trow,]==1][tcol]<-0
#           }
#       #add a tie
#           if(addrm==1 & tod[trow]<3){
#             tcol<-sample(sum(tn[trow,]==0))[1]
#             tn[trow,][tn[trow,]==0][tcol]<-1
#           }
#       datasetg$depvars$friendship[,,2]<-tn
#     }
#     
#     
#     b1<-datasetg$depvars$friendship[,,2]
#     b1[b1==10]<-0
#     table(apply(b1,1,sum))
    
    sum(datasetg$depvars$friendship[,,2]==1)
    
  
    #randomly remove a few ties to egos with 0 outd in datasetg, wave2
      datasetg<-dataset[[3]]
      set.seed(1)
      for(i in 1:20){
        tn<-datasetg$depvars$friendship[,,2]
        tod<-apply(tn,1,function(x) sum(x==1))
        #sample random row where outd = 3
            trow<-sample(length(tod[tod==3])[1],1)
            trow<-which(tod==3)[trow]
        #subtract a tie
              tcol<-sample(3)[1]
              tn[trow,][tn[trow,]==1][tcol]<-0
        datasetg$depvars$friendship[,,2]<-tn
      }
      # for(i in 1:7){
      #   tn<-datasetg$depvars$friendship[,,2]
      #   tod<-apply(tn,1,function(x) sum(x==1))
      #   #sample random row where outd = 2
      #       trow<-sample(length(tod[tod==2])[1],1)
      #       trow<-which(tod==2)[trow]
      #   #subtract a tie
      #         tcol<-sample(2)[1]
      #         tn[trow,][tn[trow,]==1][tcol]<-0
      #   datasetg$depvars$friendship[,,2]<-tn
      # }
      # for(i in 1:3){
      #   tn<-datasetg$depvars$friendship[,,2]
      #   tod<-apply(tn,1,function(x) sum(x==1))
      #   #sample random row where outd = 2
      #       trow<-sample(length(tod[tod==1])[1],1)
      #       trow<-which(tod==1)[trow]
      #   #subtract a tie
      #         tcol<-sample(1)[1]
      #         tn[trow,][tn[trow,]==1][tcol]<-0
      #   datasetg$depvars$friendship[,,2]<-tn
      # }
      

      b1<-datasetg$depvars$friendship[,,2]
      table(apply(b1,1,function(x) sum(x==1)))

      #sum(datasetg$depvars$friendship[,,2]==1)
    
#6) specify model
    #Selection
        myeff <- getEffects(datasetg,behNintn =10)

    myalgorithm <- sienaAlgorithmCreate(projname = 'SingleGroups', useStdInits = FALSE,
                                        nsub=1, n3=2000,cond = F,MaxDegree = c(friendship=3)) #,


    Model <- siena07(myalgorithm, data = datasetg, effects = myeff, nbrNodes=16, useCluster=F, initC=T, returnDeps=TRUE, batch=T, verbose = F)
