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
#meta_data = read.csv("C:/Users/alpeterson7/Documents/MLH1data/data/ALP_MouseMetadata.csv")

#set up data / format meta data sheet
meta_data$category <- ifelse(grepl("_WSB_m", meta_data$file.name), "WSB male", 
                   ifelse(grepl("_WSB_f", meta_data$file.name), "WSB female",
                      ifelse(grepl("_G_f", MLH1_data$file.name), "G female",
                   ifelse(grepl("_G_m", meta_data$file.name), "G male",
                    ifelse(grepl("_CAST_m", meta_data$file.name), "CAST male",
                       ifelse(grepl("_CAST_f", meta_data$file.name), "CAST female",
                      ifelse(grepl("_MSM_m", meta_data$file.name), "MSM male",
                                                                       ifelse(grepl("_MSM_f", meta_data$file.name), "MSM female",
                                                                              ifelse(grepl("_LEW_f", meta_data$file.name), "LEWES female", 
                                                                                     ifelse(grepl("_LEW_m", meta_data$file.name), "LEWES male",
                                                                                            ifelse(grepl("_LEWES_m", meta_data$file.name), "LEWES male",                                       
                                                                                                   ifelse(grepl("_PWD_m", meta_data$file.name), "PWD male",     
                                                                                                          ifelse(grepl("_PWD_f", meta_data$file.name), "PWD female", "other")))))))))))))

MLH1_data$category <- as.factor(MLH1_data$category)
MLH1_data$nMLH1.foci <- as.numeric(MLH1_data$nMLH1.foci) #make these numeric just in case there are other characters
MLH1_data$adj_nMLH1.foci <- as.numeric(MLH1_data$adj_nMLH1.foci)

count =1
for(i in MLH1_data$file.name){
  #print(i)
  templist= strsplit(i, split="_")[[1]]
  c = paste(templist[2], templist[3],templist[4], sep = "_")
  MLH1_data$mouse[count] <- c
  count= count +1
}


#add euth date and calc age
merged = merge(MLH1_data, meta_data, by.x = "mouse")
#format euth date (from mouse)
mouse (split "_", )
as.Date('1/15/2001',format='%m/%d/%Y')
mouse[1]
strsplit(i, split="_")
as.Date(first part of mouse,format='%d%b%y' )
euth_date <- as.Date(p[[1]][1],format='%d%b%y' )
as.Date('22JUN01',format='%d%b%y') ##this will read the mouse date format correctly.
#calculate age from dates
difftime()


# load previously made MLH1 data
setwd("C:/Users/alpeterson7/Documents/MLH1repo")
load(file="MLH1_data_setup.RData")

##############
# MERGE DATA #
##############



#make table for counting data,
#for all mice in MLH1 table



# "passing an
#for 'passing mice' (figure out what table or list this will be)