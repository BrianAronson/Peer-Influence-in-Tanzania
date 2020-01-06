#1 - Set necessary custom options for analysis
    options<-data.frame(
    Camps="not 29",  #Alternatives = "not 29", "All", or "Control"
    DVname="testhiv_12", #Alternatives = "alc_freq" "ipvoutcome_cat" "ipvoutcome_bin" "gem_r15_avg" "ipvsexual_cat" "ipvphysical_cat" "testhiv_12" "ipvpsych_cat","all_ipv"
    nobehavior=F,
    jaccut=.15,
    campdums=F
    )
    source("prep.1 - Load libraries.R")
    
#2 - load data
    sumstats<-readRDS("sumstats.rds")
    sumstats2<-readRDS("sumstats2.rds")
    varlist<-readRDS("varlist.rds")
    list2env(varlist,globalenv())
    sumvars<-readRDS("sumvars.rds")
    list2env(sumvars,globalenv())