#1) load data
    sumstats <- readRDS(file.path(der.data.dir, "sumstats.rds"))
    sumstats2 <- readRDS(file.path(der.data.dir, "sumstats2.rds"))
    varlist <- readRDS(file.path(der.data.dir, "varlist.rds"))
    sumvars <- readRDS(file.path(der.data.dir, "sumvars.rds"))
    list2env(varlist, globalenv())
    list2env(sumvars, globalenv())

#2) remove camps that don't work
    #a) identify which camps to keep
        sumstats3 <- sumstats2
        sumstats3<-sumstats3[sumstats3$N>19 & sumstats3$N<50,]
        sumstats3<-sumstats3[sumstats3$missing<.2,]
        sumstats3<-sumstats3[sumstats3$ffjaccard>.15,]
        sumstats3<-sumstats3[sumstats3$Camp!=15,]  #Camp 15 is problematic
        
    #b) remove problem camps from data
        kill <- paste(sumstats2$Camp, sumstats2$Wave) %in% paste(sumstats3$Camp, sumstats3$Wave)
        a <- c(varlist, sumvars)
        for (i in 1:length(a)) {
            a[[i]] <- a[[i]][kill]
        }
        list2env(a, globalenv())

