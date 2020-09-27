{
    #b - read files in directory
        df <- read.sas7bdat(file.path(raw.data.dir, "widebehavioraldata_20180614.sas7bdat"))
        df2 <- read.sas7bdat(file.path(raw.data.dir, "widebehavioraldata_20180614.sas7bdat"))
        fnet1 <- read.sas7bdat(file.path(raw.data.dir, "baselinesocialnetwork_20180206.sas7bdat"))
        fnet2 <- read.sas7bdat(file.path(raw.data.dir, "midpointsocialnetwork_20180206.sas7bdat"))
        fnet3 <- read.sas7bdat(file.path(raw.data.dir, "endpointsocialnetwork_20180206.sas7bdat"))
    #c - change all variable names to lower case
        names(df) <- tolower(names(df))
        names(df2) <- tolower(names(df2))
    #d - remove nonresponders from data
        df <- df[df$b_respond != 0, ]
        
}
