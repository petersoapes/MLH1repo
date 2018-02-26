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

#age, mat and pat age, Gough Line, (staining batch for cells)

#load meta data csv file 
#finalize the coding (NAs, ect)
meta_data = read.csv("C:/Users/alpeterson7/Documents/MLH1data/data/mouseDOBs.csv")

save.image("play_data_for_meta.RData")
#data needs to be cleaned up (all NA's removed before calculating )

#mouse column already included,
#change category, sex -- there is no file name...
#using the functions will be a little tricky

meta_data$mouse <- as.character(meta_data$mouse)
#format euth date (from mouse)
#all NA's need to be removed

for( t in 1:length(meta_data$mouse)){
  euth_date = strsplit(meta_data$mouse[t], split="_")[[1]][1]
  meta_data$raw_euth_date[t] <- strsplit(meta_data$mouse[t], split="_")[[1]][1]
  fomt_euth <- as.Date(strsplit(meta_data$mouse[t], split="_")[[1]][1], format= '%d%b%y')
  fomt_euth_nrm <- as.Date(fomt_euth, "%Y-%m-%d")
  vv <- as.numeric(difftime(fomt_euth, as.Date(meta_data$DOB[t], '%m/%d/%Y')), units="weeks" )#whoo, this works
  hh <- as.numeric(difftime(fomt_euth, as.Date(meta_data$DOB[t], '%m/%d/%Y')), units="hours" )
  meta_data$age_weeks[t] <- vv
  meta_data$age_hours[t] <- hh

  #mat age, DOB -Mat_dob
  mat_dob <- as.Date(meta_data$maternal.age..DOB.[t], format= '%m/%d/%Y')
#  hh <- as.numeric(difftime(fomt_euth, as.Date(meta_data$DOB[t], '%m/%d/%Y')), units="hours" )
  gg <- as.numeric(difftime(as.Date(meta_data$DOB[t], '%m/%d/%Y'),mat_dob), units="weeks" )
  meta_data$mat_age_wk[t] <- gg
}


as.Date(first part of mouse,format='%d%b%y' )
euth_date <- as.Date(p[[1]][1],format='%d%b%y' )
as.Date('22JUN01',format='%d%b%y') ##this will read the mouse date format correctly.
#calculate age from dates
difftime()


for(i in MLH1_data$file.name){
  #print(i)
  templist= strsplit(i, split="_")[[1]]
  c = paste(templist[2], templist[3],templist[4], sep = "_")
  MLH1_data$mouse[count] <- c
  count= count +1
}

# load previously made MLH1 data
setwd("C:/Users/alpeterson7/Documents/MLH1repo")
load(file="MLH1_data_setup.RData")

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

