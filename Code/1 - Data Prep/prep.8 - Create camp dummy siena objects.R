#8 - create camp dummy siena objects,
    #identify camp names
        campnames<-waveindex$camp
    #create list of camp object repeated an equivalent to number of camps in analysis
        camplist<-rep(list(campn),length(campnames))
    #For each element in large camp list
      for(i in 1:length(camplist)){
          #identify camp name
              tcamp<-camplist[[i]]
              tcampname<-campnames[i]
          #for each siena within smaller camp list
              for(j in 1:length(tcamp)){
                  #change to 1 if match camp name, or 0 if no match
                      tcampname2<-unique(tcamp[[j]])
                      if(tcampname2==tcampname){
                        tcamp[[j]][tcamp[[j]]==tcampname2]<-1
                      }else{
                        tcamp[[j]][tcamp[[j]]==tcampname2]<-0
                      }
              }
          #overwrite old camp info with new info
              camplist[[i]]<-tcamp
      }
      #name camp lists assuming each wave has different traits and put in environment
          names(camplist)<-paste("camp",campnames,waveindex$Wave,sep="")