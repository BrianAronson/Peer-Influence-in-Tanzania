# #1. View data
#     if(is.null(options)){
#        View(sumstats)
#     }
# #3. Subset
#     #Create a backup
        dataset2<-dataset
        sumstats3<-sumstats2
        removedids<-vector()
#     #ideal subset
        #no camp 29 :'(
            removedids<-c(removedids,which(sumstats3$Camp==29))
        #decent N
            removedids<-c(removedids,which(sumstats3$N<=10))
        #decent jaccard
            removedids<-c(removedids,which(sumstats3$jaccard<=.15))
        #update remaining info
            removedids<-sort(unique(removedids))
            keeps<-setdiff(1:length(dataset2),removedids)
            dataset2[removedids]<-NULL
            sumstats3<-sumstats3[keeps,]

        #wave 1
            # dataset2<-dataset2[sumstats3$Wave==1]
            # sumstats3<-sumstats3[sumstats3$Wave==1,]
            # dataset2<-dataset2[!sapply(dataset2,is.null)]
             
#         #decent coverage
#             # dataset2<-dataset2[sumstats3$missing<.2]
#             # sumstats3<-sumstats3[sumstats3$missing<.2,]
#             # dataset2<-dataset2[!sapply(dataset2,is.null)]
#         #decent behavior heterogeneity
            #             # dataset2<-dataset2[sumstats3$d.ipv>0]
            #             # sumstats3<-sumstats3[sumstats3$d.ipv>0,]
            #             # dataset2<-dataset2[!sapply(dataset2,is.null)]
            # 
            #4 - Create group object
            datasetg <- sienaGroupCreate(dataset2)
            
            ## #example of people becoming violent who were not friends with violent ego
            #     dfl[[29]]$e_ipvoutcome_cat
            #     dfl[[29]]$pid[10]
            #     sumnet1[Camp1==29 & tie==1 & (ID1 %in% dfl[[29]]$pid[10] | ID2 %in% dfl[[29]]$pid[10]) ]$ID2
            #     dfl[[29]]$pid[dfl[[29]]$e_ipvoutcome_cat>0]
            
            # 
            #     sumstats3$Camp
            #     sumstats3$Wave
            #     temp<-sapply(dataset2, function(x) attr(x$cCovars$ward3,"mean")[1])
            #     temp2<-sapply(dataset2, function(x) attr(x$cCovars$ward4,"mean")[1])    
            #     
            #     dataset2<-dataset2[!(sumstats3$Wave==1 & (temp==1 | temp2==1))]
            #     sumstats3<-sumstats3[!(sumstats3$Wave==1 & (temp==1 | temp2==1)),]
            #     
            #     datasetg <- sienaGroupCreate(dataset2)
            #     