#1 - Create a backup
    dataset2<-dataset
    sumstats3<-sumstats2
    removedids<-vector()
#2 - ideal subset
    #no camp 29 :'(
        removedids<-c(removedids,which(sumstats3$Camp==29))
    #decent N
        removedids<-c(removedids,which(sumstats3$N<=10))
    #decent jaccard
        removedids<-c(removedids,which(sumstats3$jaccard<=.10))
    #update remaining info
        removedids<-sort(unique(removedids))
        keeps<-setdiff(1:length(dataset2),removedids)
        dataset2[removedids]<-NULL
        sumstats3<-sumstats3[keeps,]
    #wave 1
        # dataset2<-dataset2[sumstats3$Wave==1]
        # sumstats3<-sumstats3[sumstats3$Wave==1,]
        # dataset2<-dataset2[!sapply(dataset2,is.null)]
      
#4 - Create group object
    datasetg <- sienaGroupCreate(dataset2)
