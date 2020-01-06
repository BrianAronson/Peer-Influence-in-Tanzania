#0 - Set work directory
    setwd("/misc/utopia3/bda13/lanhome/Data/Shared/Tanzania/Code") 
    #setwd("C:/Users/bda13/Desktop/Tanzania")
#1 - Set necessary custom options for analysis
    options<-data.frame(
    Camps="not 29",  #Alternatives = "not 29", "All", or "Control"
    DVname="gem_r15_avg", #Alternatives = "alc_freq" "ipvoutcome_cat" "ipvoutcome_bin" "gem_r15_avg" "ipvsexual_cat" "ipvphysical_cat" "testhiv_12" "ipvpsych_cat","all_ipv"
    nobehavior=F,
    jaccut=.15,
    campdums=F
    )

    
#2 - Prepare the data
  source("prep.1 - Load libraries.R")
    #1. Generate stats by camp to determine which camps must be removed from analysis
        #file.edit to open source code 
        #No need to run after first time; saves key info
      {
        source("prep.1 - Load libraries.R")
        source("prep.2 - Load data.R")
        source("prep.3 - Prep network data.R")
        source("prep.4 - Prep all other dyadic covariates.R")
        file.edit("prep.5 - Prep non-network data.R")
        source("prep.6 - Prep full friendship networks.R")
        source("prep.7 - Prep full friendship vars.R")
        source("prep.8 - Remove individuals from waves 2-3.R")
        source("prep.9 - Create summary info and save.R")
        source("prep.10 - Create more camp level dummies.R")
        # file.edit("prep.8 - Create camp dummy siena objects.R")
        #Full network variables
        #Friendship Clusters
    }
    
    #2. Use above information to subset data to good camps, then prep data by ward
        #file.edit to open source code
        #No need to run after first time; saves key info
        {
          source("prep.1 - Load libraries.R")
          file.edit("prep2.1 - Prep subset.R")#need to manually edit
          source("prep.2 - Load data.R")  
          source("prep2.3 - Prep network data.R")
          source("prep2.4 - Prep all other dyadic covariates.R")
          source("prep2.5 - Prep non-network data.R")
          source("prep2.6 - Prep full friendship networks.R")
          source("prep2.7 - Remove individuals from waves 2-3.R")
          source("prep2.9 - Create summary info and save.R")
          source("prep2.10 - Create more camp level dummies.R")
        }
    
    # #2. Determine camps to model
    #     #a. Find what the models would ideally look like
    #         source("prep.1 - Load libraries.R")
    #         source("modf.1 - Load and create RSiena objects.R")
    #         source("modf.2 - Subset data.R")    
    #         source("modf.3 - Create effects for algorithm.R")
    #         source("mod.4 - Create save results function.R")
    #         source("modf.5 - Find good camps.R")
     
    #3. Create full model for IPV
        source("prep.1 - Load libraries.R")
        file.edit("mod.1 - Load and create RSiena objects.R")
        file.edit("mod.3 - Create effects for algorithm.R")
        source("mod.4 - Create save results function.R")
        source("mod.5 - Run models.R")
    
    #4. Create full model for HIV
        source("prep.1 - Load libraries.R")
        source("mod.1hiv - Load and create RSiena objects.R")
        source("mod.3hiv - Create effects for algorithm.R")
        source("mod.4 - Create save results function.R")
        file.edit("mod.5 - Run models.R")
        
    #5. visualizations
        #source("mod.6 - Run GOF.R")
        file.edit("xap.1 - Run by camp.R")
        file.edit("xap.2 - Find Outliers.R")
        file.edit("xap.3 - Run Multiple Models.R")
        file.edit("xap.3 - Figures.R")
        file.edit("xap.4 - Other.R")
        file.edit("xap.5 - Other2.R")
        browseURL(htmlfilename)

