#0 - identify ward and time of each camp
    ward <- 1:length(dv.ipvoutcome_cat)
    tward1 <- sapply(ward1, function(x) x[1])
    tward2 <- sapply(ward2, function(x) x[1])
    tward3 <- sapply(ward3, function(x) x[1])
    tward4 <- sapply(ward4, function(x) x[1])
    ward[tward1==1] <- 1
    ward[tward2==1] <- 2
    ward[tward3==1] <- 3
    ward[tward4==1] <- 4
    # Specify if more than one wave
    if(length(unique(sumstats3$Wave))==2){
          twave <- sapply(wave, function(x) x[1])
          ward[twave==3] <- ward[twave==3]+4
        }

#1 - Sienna dependent variables:
    combine.depvar <- function(x){
        x1 <- x
        x2 <- split(x1,ward) #split by ward
        x3 <- lapply(x2,function(y){#within each ward
          y1 <- y
          if(length(y)>1){
              y2 <- sapply(y1, function(x) x[,,])
              y3 <- do.call(rbind,y2)
          }else{
            y2 <- y1[[1]][,,]
            y3 <- y2
          }
          y4 <- sienaDependent(y3, type ="behavior",allowOnly = F)
        })
        return(x3)
    }
    dv.ipvoutcome_cat <- combine.depvar(dv.ipvoutcome_cat)
    dv.ipvoutcome_bin <- combine.depvar(dv.ipvoutcome_bin)
    dv.gem_r15_avg <- combine.depvar(dv.gem_r15_avg)
    dv.ipvphysical_cat <- combine.depvar(dv.ipvphysical_cat)
    dv.ipvpsych_cat <- combine.depvar(dv.ipvpsych_cat)
    dv.testhiv_12 <- combine.depvar(dv.testhiv_12)
    dv.all_ipv <- combine.depvar(dv.all_ipv)
    dv.alc_freq <- combine.depvar(dv.alc_freq)

#2 - siena network variables
    combine.netvar <- function(x,AO=T){
        x1 <- x
        x2 <- split(x1,ward) #split by ward
    #within each ward:
        x3 <- sapply(x2,function(x){
            if(length(x)>1){
            #for each camp, break down siena objects into arrays
                x4 <- sapply(x, function(x) x[,,])
            #create an empty array to fit all camps
                dims <- sapply(x4, function(x) dim(x)[1])
                rows <- sum(dims)
                tarray <- array(10,dim=c(rows,rows,2))
            #for each camp, put data in array
                #identify rows to input data into
                    stops=cumsum(dims)
                    starts <- stops+1
                    starts <- c(1,starts[-length(starts)])
                    df <- data.frame(start=starts,stop=stops)
                for(i in 1:length(x4)){
                  a <- x4[[i]][,,1]
                  b <- x4[[i]][,,2]
                  tarray[df$start[i]:df$stop[i],df$start[i]:df$stop[i],1] <- a
                  tarray[df$start[i]:df$stop[i],df$start[i]:df$stop[i],2] <- b
                }
                return(sienaNet(tarray,allowOnly = AO))
            }else{
              tarray <- x[[1]][,,]
              return(sienaNet(tarray,allowOnly = AO))
            }
          })
          return(x3)
    }
    friendship <- combine.netvar(friendship)
    dv.ffriendship <- combine.netvar(dv.ffriendship,AO = F)

#3 - siena coDyadCovars
    combine.coDyadCovars <- function(x,CT=T){
        x1 <- x
        x2 <- split(x1,ward) #split by ward
    #within each ward:
        x3 <- sapply(x2,function(x){
            if(length(x)>1){
            #for each camp, break down siena objects into arrays
                x4 <- sapply(x, function(x) x[,])
            #create an empty array to fit all camps
                dims <- sapply(x4, function(x) dim(x)[1])
                rows <- sum(dims)
                tarray <- matrix(10,nrow=rows,ncol=rows)
            #for each camp, put data in array
                #identify rows to input data into
                    stops=cumsum(dims)
                    starts <- stops+1
                    starts <- c(1,starts[-length(starts)])
                    df <- data.frame(start=starts,stop=stops)
                for(i in 1:length(x4)){
                  a <- x4[[i]]
                  tarray[df$start[i]:df$stop[i],df$start[i]:df$stop[i]] <- a
                }
                return(coDyadCovar(tarray,centered = CT))
            }else{
              x4 <- x[[1]][,]
              tarray <- x4
              return(coDyadCovar(tarray,centered = CT))
            }
          })
          return(x3)
    }
    bothmale <- combine.coDyadCovars(bothmale,CT=F)
    ffriendship <- combine.coDyadCovars(ffriendship)
    hivtstf <- combine.coDyadCovars(hivtstf)
    hivinjf <- combine.coDyadCovars(hivinjf)
    hivadvf <- combine.coDyadCovars(hivadvf)
    knowf <- combine.coDyadCovars(knowf)
    closef <- combine.coDyadCovars(closef)

#4 - sienna covars:
    combine.covvar <- function(x,CT=T){
        x1 <- x
        x2 <- split(x1,ward) #split by ward
        x3 <- lapply(x2,function(y){#within each ward
          if(length(y)>1){
            y3 <- unlist(sapply(y,function(x) c(x))) #unlist objects
            y4 <- coCovar(y3, centered=CT) #turn back into a siena object
          }else{
            y3 <- unlist(y)
            coCovar(y3, centered=CT)
          }
        })
        return(x3)
    }

    #create ward-wave dummies
        wardwave <- combine.covvar(sex)
        for(i in 1:length(wardwave)){
          wardwave[[i]][] <- 0
        }
        wardwave1 <- wardwave
        wardwave2 <- wardwave
        wardwave3 <- wardwave
        wardwave4 <- wardwave
        wardwave5 <- wardwave
        wardwave6 <- wardwave
        wardwave7 <- wardwave
        wardwave8 <- wardwave
        wardwave1[[1]][] <- 1
        wardwave2[[2]][] <- 1
        wardwave3[[3]][] <- 1
        wardwave4[[4]][] <- 1
        wardwave5[[5]][] <- 1
        wardwave6[[6]][] <- 1
        wardwave7[[7]][] <- 1
        wardwave8[[8]][] <- 1

    #other variables
        sex <- combine.covvar(sex)
        age <- combine.covvar(age)
        edu <- combine.covvar(edu)
        married <- combine.covvar(married)
        ses <- combine.covvar(ses)
        campn <- combine.covvar(campn,CT=F)
        pid <- combine.covvar(pid,CT=F)
        ward1 <- combine.covvar(ward1)
        ward2 <- combine.covvar(ward2)
        ward3 <- combine.covvar(ward3)
        ward4 <- combine.covvar(ward4)
        condition <- combine.covvar(condition)
        leader <- combine.covvar(leader)
        gem <- combine.covvar(gem)
        wave <- combine.covvar(wave)
        sex.active <- combine.covvar(sex.active)
        sex.year <- combine.covvar(sex.year)
        child.violence <- combine.covvar(child.violence)
        child.sex.violence <- combine.covvar(child.sex.violence)
        camp.duration <- combine.covvar(camp.duration)
        has.children <- combine.covvar(has.children)
        fam.available <- combine.covvar(fam.available)
        hivtst.ego <- combine.covvar(hivtst.ego)
        hivinjf.ego <- combine.covvar(hivinjf.ego)
        hivadvf.ego <- combine.covvar(hivadvf.ego)
        knowf.ego <- combine.covvar(knowf.ego)
        closef.ego <- combine.covvar(closef.ego)
        ever.test <- combine.covvar(ever.test)
        ever.ipv <- combine.covvar(ever.ipv)
        alc_evr <- combine.covvar(alc_evr)
        alc_frq <- combine.covvar(alc_frq)
        drnk_frq <- combine.covvar(drnk_frq)
        alc_bing <- combine.covvar(alc_bing)
        hivtst.alter <- combine.covvar(hivtst.alter)
        hivinjf.alter <- combine.covvar(hivinjf.alter)
        hivadvf.alter <- combine.covvar(hivadvf.alter)
        hivtst.group <- combine.covvar(hivtst.group)
        sex.num <- combine.covvar(sex.num)
        ffipv <- combine.covvar(ffipv)
        ffgem <- combine.covvar(ffgem)
        ffalc <- combine.covvar(ffalc)
        ffgend <- combine.covvar(ffgend)
        ffnum <- combine.covvar(ffnum)
        ffhiv <- combine.covvar(ffhiv)
        clipv <- combine.covvar(clipv)
        clgem <- combine.covvar(clgem)
        clalc <- combine.covvar(clalc)
        clgend <- combine.covvar(clgend)
        clnum <- combine.covvar(clnum)
        clhiv <- combine.covvar(clhiv)
        male <- combine.covvar(male)

#5 - save
    varlist <- list(dv.all_ipv, dv.ipvoutcome_cat,dv.ipvoutcome_bin,dv.gem_r15_avg,dv.ipvphysical_cat,dv.testhiv_12,friendship,ffriendship,hivtstf,hivinjf,hivadvf,knowf,closef,sex,age,edu,married,ses,campn,ward1,ward2,ward3,ward4,condition,leader,gem,wave,sex.active,sex.year,child.violence,child.sex.violence,camp.duration,has.children,fam.available,hivtst.ego,hivinjf.ego,hivadvf.ego,knowf.ego,closef.ego,ever.test,ever.ipv,dv.ipvpsych_cat,alc_evr,alc_frq,alc_bing,drnk_frq,hivtst.alter,hivinjf.alter,hivadvf.alter,hivtst.group,sex.num,dv.alc_freq,ffipv,ffgem,ffalc,ffgend,ffnum,ffhiv,clipv,clgem,clalc,clgend,clnum,clhiv,dv.ffriendship,bothmale,male,wardwave1,wardwave2,wardwave3,wardwave4,wardwave5,wardwave6,wardwave7,wardwave8)
    names(varlist) <- c("dv.all_ipv","dv.ipvoutcome_cat","dv.ipvoutcome_bin","dv.gem_r15_avg","dv.ipvphysical_cat","dv.testhiv_12","friendship","ffriendship","hivtstf","hivinjf","hivadvf","knowf","closef","sex","age","edu","married","ses","campn","ward1","ward2","ward3","ward4","condition","leader","gem","wave","sex.active","sex.year","child.violence","child.sex.violence","camp.duration","has.children","fam.available","hivtst.ego","hivinjf.ego","hivadvf.ego","knowf.ego","closef.ego","ever.test","ever.ipv","dv.ipvpsych_cat","alc_evr","alc_frq","alc_bing","drnk_frq","hivtst.alter","hivinjf.alter","hivadvf.alter","hivtst.group","sex.num","dv.alc_freq","ffipv","ffgem","ffalc","ffgend","ffnum","ffhiv","clipv","clgem","clalc","clgend","clnum","clhiv","dv.ffriendship","bothmale","male","wardwave1","wardwave2","wardwave3","wardwave4","wardwave5","wardwave6","wardwave7","wardwave8")
    saveRDS(varlist, file.path(der.data.dir, "varlist2.rds"))

