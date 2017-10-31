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


#age, mat and pat age, Gough Line, staining batch

#count the number of mice with missing meta-data


#load meta data csv file 
#finalize the coding (NAs, ect)
meta_data = read.csv("C:/Users/alpeterson7/Documents/MLH1data/data/ALP_MouseMetadata2.csv")
save.image("play_data_for_meta.RData")
#data needs to be cleaned up (all NA's removed before calculating )


#mouse column already included,
#change category, sex -- there is no file name...
#using the functions will be a little tricky



meta_data$mouse <- as.character(meta_data$mouse)
#format euth date (from mouse)
ply_set <- meta_data[5:9,]

#all NA's need to be removed

for( t in 1:length(meta_data$mouse)){
  euth_date = strsplit(meta_data$mouse[t], split="_")[[1]][1]
  meta_data$raw_euth_date[t] <- strsplit(meta_data$mouse[t], split="_")[[1]][1]
  fomt_euth <- as.Date(strsplit(meta_data$mouse[t], split="_")[[1]][1], format= '%d%b%y')
  fomt_euth_nrm <- as.Date(fomt_euth, "%Y-%m-%d")
  vv <- as.numeric(difftime(fomt_euth, as.Date(meta_data$DOB[t], '%m/%d/%Y')), units="weeks" )#whoo, this works
  hh <- as.numeric(difftime(fomt_euth, as.Date(meta_data$DOB[t], '%m/%d/%Y')), units="hours" )
  meta_data$diffy_weeks[t] <- vv
  meta_data$diffy_hours[t] <- hh
  meta_data$calq_age[t] <- fomt_euth

}

mouse[1]
strsplit(i, split="_")  #as.Date(first part of mouse,format='%d%b%y' )

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
#add euth date and calc age
merged = merge(MLH1_data, meta_data, by.x = "mouse")


#make table for counting data,
#for all mice in MLH1 table



# "passing an
#for 'passing mice' (figure out what table or list this will be)


#source("Func_addCategory.R")
#function would work for this csv file
#MLH1_data <- add_category(MLH1_data)
#set the order of categories (female, male) (cast, dom, musc)
#MLH1_data$category<- factor(MLH1_data$category,levels =c( "G female", "G male", 
#                       "WSB female", "WSB male", "LEWES female", 'LEWES male', 
#                     "PWD female", "PWD male", "MSM female", "MSM male",
#                                    "CAST female", "CAST male"), order=T )
#MLH1_data$category <- as.factor(MLH1_data$category)
