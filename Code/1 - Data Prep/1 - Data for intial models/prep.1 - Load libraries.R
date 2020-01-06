{
    #a - Load libraries
        library(sas7bdat)
        library(data.table)
        library(RSienaTest)
        library(parallel)
        library(foreach)
        library(doParallel)
        library(rdrop2)
        library(XML)
        library(ggplot2)
        library(gridExtra)
        library(grid)
        library(igraph)
        library(xtable)
}


#dim(b)

# install.packages("RSienaTest",repos="http://r-forge.r-project.org")
# 
# detach("package:RSiena", unload=TRUE)
# library(RSienaTest)
# Model
# 
# remove.packages("RSienaTest")
# install.packages("RUnit")