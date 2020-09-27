rm(list=ls())
#1 - Set work directory
    data.dir <- "C:/Users/admin/Desktop/IDrive-Sync/Academic - Writing/Tanzania/Data"
    raw.data.dir <- file.path(data.dir, "Raw Data")
    der.data.dir <- file.path(data.dir, "Derived Data")
    code.prep.dir <- "C:/Users/admin/Desktop/IDrive-Sync/Analysis - Code/Tanzania/Peer-Influence-in-Tanzania/Code/1 - Data Prep"
    code.analysis.dir <- "C:/Users/admin/Desktop/IDrive-Sync/Analysis - Code/Tanzania/Peer-Influence-in-Tanzania/Code/2 - Analysis"
    code.figures.dir <- "C:/Users/admin/Desktop/IDrive-Sync/Analysis - Code/Tanzania/Peer-Influence-in-Tanzania/Code/3 - Other Stats"
    
    
#2 - Prepare the baseline data
    source(file.path(code.prep.dir, "prep.1 - Load libraries.R"))
    source(file.path(code.prep.dir, "prep.2 - Load data.R"))
    source(file.path(code.prep.dir, "prep.3 - Prep network data.R"))
    source(file.path(code.prep.dir, "prep.4 - Prep all other dyadic covariates.R"))
    source(file.path(code.prep.dir, "prep.5 - Prep non-network data.R"))
    source(file.path(code.prep.dir, "prep.6 - Prep full friendship networks.R"))
    source(file.path(code.prep.dir, "prep.7 - Prep full friendship vars.R"))
    source(file.path(code.prep.dir, "prep.8 - Remove individuals from waves 2-3.R"))
    source(file.path(code.prep.dir, "prep.9 - Create summary info and save.R"))
    source(file.path(code.prep.dir, "prep.10 - Create more camp level dummies.R"))
    
#3 - Prepare final analysis dataset and run Siena Model
    source(file.path(code.analysis.dir, "Ward 1 - Load data and subset camps.R"))
    source(file.path(code.analysis.dir, "Ward 2 - Aggregate objects to wards.R"))
    source(file.path(code.analysis.dir, "Ward 3 - prep siena objects.R"))
    file.edit(file.path(code.analysis.dir, "Ward 4 - Specify and run model.R"))
    file.edit(file.path(code.analysis.dir, "Ward 5 - Appendix - Run model without mediators.R"))
    
    
    
#4 - Generate figures for paper
    #source("mod.6 - Run GOF.R")
    file.edit("xap.1 - Run by camp.R")
    file.edit("xap.2 - Find Outliers.R")
    file.edit("xap.3 - Run Multiple Models.R")
    file.edit("xap.3 - Figures.R")
    file.edit("xap.4 - Other.R")
    file.edit("xap.5 - Other2.R")
    browseURL(htmlfilename)

