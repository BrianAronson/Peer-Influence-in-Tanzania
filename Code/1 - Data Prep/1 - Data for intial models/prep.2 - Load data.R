{
    #b - read files in directory
        dir<-getwd()
        tdir<-'..'
        setwd(paste(tdir,"/Dropbox files/Tanzania R01 Network Data - Peer Influence/SAS Data",sep=""))
        df <- read.sas7bdat("widebehavioraldata_20180614.sas7bdat")
        df2 <- read.sas7bdat("widebehavioraldata_20180614.sas7bdat")
        fnet1 <- read.sas7bdat("baselinesocialnetwork_20180206.sas7bdat")
        fnet2 <- read.sas7bdat("midpointsocialnetwork_20180206.sas7bdat")
        fnet3 <- read.sas7bdat("endpointsocialnetwork_20180206.sas7bdat")
    #c - change all variable names to lower case
        names(df)<-tolower(names(df))
        names(df2)<-tolower(names(df2))
        setwd(dir)
    #d - remove nonresponders from data
        df<-df[df$b_respond!=0,]
        
}