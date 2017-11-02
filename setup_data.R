##R script for making R.data files 
## read in data files and adjust with additional columns
## input: csv file of anon MLH1 counts
## output: .rdata saved workspace with dataframe and other stat tables (save tables)
## df of MLH1 should go from 12cols to 17cols
#import MLH1 measures across data sets; AP, BD's and Cast female Koehler
# 1. MLH1_data, tables: AP strains

library(plyr)
library(dplyr)

########################
# Setup main dataframe #
########################

#TODO: remove observations without quality scores.
#Write a make file that will merge all of the batch files and record the batch
setwd("C:/Users/alpeterson7/Documents/MLH1repo/")
MLH1_data = read.csv("data/AnonData.csv", header=TRUE )

original_DF = MLH1_data
original_length <- length(MLH1_data$Original.Name)

MLH1_data$n <- as.character(MLH1_data$n)
MLH1_data$nMLH1.foci<- as.character(MLH1_data$nMLH1.foci)
MLH1_data$quality<- as.character(MLH1_data$quality)

MLH1_data$n <- as.numeric(MLH1_data$n)
MLH1_data$nMLH1.foci<- as.numeric(MLH1_data$nMLH1.foci)
MLH1_data$quality<- as.numeric(MLH1_data$quality)

#make list of rows that need more data
missing_data_DF <- MLH1_data[(MLH1_data$nMLH1.foci == "" | MLH1_data$quality == ""),]

#remove X's and NAs
MLH1_data <- MLH1_data[ !grepl("X", MLH1_data$X) , ]
MLH1_data <- MLH1_data[!(is.na(MLH1_data$nMLH1.foci) | MLH1_data$nMLH1.foci==""), ]

MLH1_data <- MLH1_data[MLH1_data$nMLH1.foci != "X",]
MLH1_data <- MLH1_data[MLH1_data$nMLH1.foci != "x",]

source("Func_addCategory.R")
MLH1_data <- add_category(MLH1_data)
#set the order of categories (female, male) (cast, dom, musc)
MLH1_data$category<- factor(MLH1_data$category,levels =c( "G female", "G male", 
                      "WSB female", "WSB male", "LEWES female", 'LEWES male', "PERC male",
                      "PWD female", "PWD male", "MSM female", "MSM male",
                      "CAST female", "CAST male", "HMI female", "HMI male",
                      "SPRET female", "SPRET male"), order=T )

source("Func_addStrain.R")
MLH1_data <- add_strain(MLH1_data)

#the ordering factor below deletes all strain entries
#MLH1_data$strain<- factor(MLH1_data$category,levels =c( "G", "WSB", "LEWES", "PERC",
#                                                        "PWD", "MSM",
 #                                                       "CAST", "HMI",
#                                                        "SPRET"), order=T )

source("Func_addSex.R")
MLH1_data <- add_sex(MLH1_data)
#MLH1_data$sex<- factor(MLH1_data$category,levels =c( "female", "male"), order=T )

#add a column with male adjusted MLH1 values (+1 to all males)
MLH1_data$adj_nMLH1.foci <- ifelse(MLH1_data$sex=="male", MLH1_data$nMLH1.foci+1, MLH1_data$nMLH1.foci)

MLH1_data$adj_nMLH1.foci <- as.numeric(MLH1_data$adj_nMLH1.foci)

source("Func_addMouse.R")
MLH1_data <- add_mouse(MLH1_data)

#remove mice that had bad stains
#12sep16_MSM_f3(centromere signal bled into MLH1 signal)
MLH1_data <- MLH1_data[ !grepl("12sep16_MSM_f3", MLH1_data$mouse) , ]
#make a list of bad mice


#MLH1_by_M_mouse <- with(MLH1_by_M_mouse, MLH1_by_M_mouse[order(subsp),])
#load Lynn et al 2002 data. 12 CAST female MLH1 measurements (probably 1 female)
#Lynn_CASTf = c(20,21, 23, 25, 26, 26,26,27.5, 28, 28,28,33)


#load BD's data. Only PWD female and F1 females missing
fullBD_MLH1_data = read.csv("C:/Users/alpeterson7/Documents/MLH1data/data/BD_MLH1data/BD_RecombinationPhenotypes_input.csv")
#unique(fullBD_MLH1_data$Cross)
#subset P0s
BDMLH1_data <- subset(fullBD_MLH1_data, (Cross %in%  c("PANCEVO","RAT","CIM", "PWDFemale", "PWD","Peromyscus",
                      "CZECHI","PERA", "CAROLI", "CAST", "Microtus", "WSB") ))

#now that I have the mice of Beth's I want, remove the big BD df with F2s to save space.
rm(fullBD_MLH1_data)


############
# SAVE DFs #
############
print(c("initial data set of ", original_length, " cells, was slimed down to ", 
        length(MLH1_data$Original.Name)) )

print(c("The mean MLH1 foci number is  ", mean(MLH1_data$nMLH1.foci, na.rm =TRUE), 
"the distribution of quality scores is ", table(MLH1_data$quality)  ) )

save.image("MLH1_data_setup.RData")
#OutPut: big large MLH1_data (AP's) df, big DF of BD with just the mice I want.
# MLH1_data_table, means and variance of AP and BD MLH1 values. made from seperate tables from AP and BD data.
#  make sure decimal places are consistant
