# add meta data -df for MLH1 data set
# input: MLH1 Rdata and metadat csv sheet
# output: stats of missing information for 'passing' mice
#eventually analysis of age and other effects.

library(knitr)
library(ggplot2)
library(pwr)
library(plyr)
library(lattice)
library(dplyr)

#orig cols
#mouse, DOB, mat DOB, pat DOB, stain, 

#calq cols
#sex, age, dissection date, mat age, pat age, time to stain (cell measure dependant)

# final cols
#Gough Line, (staining batch for cells)
#

#load meta data csv file 
#finalize the coding (NAs, ect)
#meta_data = read.csv("C:/Users/alpeterson7/Documents/MLH1data/data/mouseDOBs.csv")
setwd("C:/Users/alpeterson7/Documents/MLH1repo/")
full_meta_data = read.csv("C:/Users/alpeterson7/Documents/MLH1repo/ALP_MouseMetadata.csv")

#save.image("play_data_for_meta.RData")

### mat age pat age ###
full_meta_data$mouse <- as.character(full_meta_data$mouse)
#loop for calculating ages
for( t in 1:length(full_meta_data$mouse)){
  euth_date = strsplit(full_meta_data$mouse[t], split="_")[[1]][1]
  #full_meta_data$raw_euth_date[t] <- strsplit(full_meta_data$mouse[t], split="_")[[1]][1]
  
  fomt_euth <- as.Date(strsplit(full_meta_data$mouse[t], split="_")[[1]][1], format= '%d%b%y')
  fomt_euth_nrm <- as.Date(fomt_euth, "%Y-%m-%d")
  vv <- as.numeric(difftime(fomt_euth, as.Date(full_meta_data$DOB[t], '%m/%d/%Y')), units="weeks" )#whoo, this works
  hh <- as.numeric(difftime(fomt_euth, as.Date(full_meta_data$DOB[t], '%m/%d/%Y')), units="hours" )
  
 # full_meta_data$age_weeks[t] <- vv
  
  #mat age, DOB -Mat_dob
  mat_dob <- as.Date(full_meta_data$mat.DOB[t], format= '%m/%d/%Y')
  pat_dob <- as.Date(full_meta_data$pat.DOB[t], format= '%m/%d/%Y')
  
  mm.dob <- as.numeric(difftime(as.Date(full_meta_data$DOB[t], '%m/%d/%Y'),mat_dob), units="weeks" )
  pp.dob <- as.numeric(difftime(as.Date(full_meta_data$DOB[t], '%m/%d/%Y'),pat_dob), units="weeks" )
  
  full_meta_data$mat_age_wk[t] <- mm.dob
  full_meta_data$pat_age_wk[t] <- pp.dob
}

###SEX###
source("src/Func_addSex.R")
full_meta_data <- add_sex(full_meta_data)
#make two seperate DF's?

female_meta_data <- 
male_meta_data <- 

### female age ###

#if sex = female, euth date = DOB, age = neonate
# female age -- neonate v embryo? 
# what about embryo


###AGE, mat.age, pat.age### (weeks)





### EUTH Date ###


as.Date(first part of mouse,format='%d%b%y' )
euth_date <- as.Date(p[[1]][1],format='%d%b%y' )
as.Date('22JUN01',format='%d%b%y') ##this will read the mouse date format correctly.
#calculate age from dates
difftime()




# load previously made MLH1 data
setwd("C:/Users/alpeterson7/Documents/MLH1repo")
load(file="MLH1_data_setup.RData")

### write metadata-file ###


##############
# MERGE DATA #
##############
setwd("C:/Users/alpeterson7/Documents/MLH1repo/")
load("MLH1_data_setup.RData")
#add euth date and calc age
mergedDF = merge(MLH1_data, meta_data, by = "mouse") #4000 cells by table of 1178 mice

dissected_mice <- unique(meta_data$mouse)#180
imaged_mice <- unique(MLH1_data$mouse)#62
n.overlapping_mice <- length(imaged_mice)/length(dissected_mice)

############
#count the number of mice with missing meta-data
##############
dfn <- mergedDF[(mergedDF$DOB == '' ),]
dfm <- mergedDF[(is.na(mergedDF$DOB) ), ]

unique(dfn$mouse)

unique((dfm$mouse))


#### 
# Assess effects
#

F_merged_DF <- mergedDF[mergedDF$sex == "female",]
M_merged_DF <- mergedDF[mergedDF$sex == "male", ]

#calc mouse average, MLH1 and age

