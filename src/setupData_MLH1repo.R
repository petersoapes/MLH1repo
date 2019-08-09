##R script for making R.data files 
## read in data files and adjust with additional columns
## input: csv file of anon MLH1 counts
## output: .rdata saved workspace with dataframe and other stat tables (save tables)
## df of MLH1 should go from 12cols to 17cols
#import MLH1 measures across data sets; AP, BD's and Cast female Koehler
# 1. MLH1_data, tables: AP strains
#updated meta.data.file

#library(plyr)
library(dplyr)
library(raster)#for cV
library(ggplot2)

########################
# Setup main dataframe #
########################

#TODO: remove observations without quality scores (why?)
#TODO integrate newest batch

#images from slides
#12jan17_4jan17_LEW_f1_sp1_34_rev, 8feb17_4jan17_LEW_f1_sp1_3_rev, potential outliers

#Write a make file that will merge all of the batch files and record the batch
#setwd("C:/Users/alpeterson7/Documents/MLH1repo/")
setwd("~./MLH1repo/")

#forgot why I'm reading in this metadata file
meta.data = read.csv("data/clean.Meta.Data.txt",sep = "\t", header=TRUE )

MLH1_data = read.csv("data/MLH1/AnonData.csv", header=TRUE ) #7.30.19, 4382


source("src/CommonFunc_MLH1repo.R")
original_DF = MLH1_data
original_DF$fileName <- (original_DF$Original.Name)
original_DF <- add_mouse(original_DF)
#original_length <- length(MLH1_data$Original.Name)

#check that all batches are included
table(original_DF$Batch)


MLH1_data$Original.Name <- as.character(MLH1_data$Original.Name)
MLH1_data$fileName <- (MLH1_data$Original.Name)

#these introduce NAs
MLH1_data$n <- as.character(MLH1_data$n)
MLH1_data$nMLH1.foci<- as.character(MLH1_data$nMLH1.foci)
MLH1_data$quality<- as.character(MLH1_data$quality)

MLH1_data$n <- as.numeric(MLH1_data$n)
MLH1_data$nMLH1.foci<- as.numeric(MLH1_data$nMLH1.foci)
MLH1_data$quality<- as.numeric(MLH1_data$quality)

#remove X's and NAs
MLH1_data <- MLH1_data[ !grepl("X", MLH1_data$X) , ]
MLH1_data <- MLH1_data[!(is.na(MLH1_data$nMLH1.foci) | MLH1_data$nMLH1.foci==""), ]

MLH1_data <- MLH1_data[MLH1_data$nMLH1.foci != "X",]
MLH1_data <- MLH1_data[MLH1_data$nMLH1.foci != "x",]

source("src/CommonFunc_MLH1repo.R")
MLH1_data <- add_mouse(MLH1_data)
MLH1_data <- add_category(MLH1_data)
MLH1_data <- add_strain(MLH1_data)
MLH1_data <- add_sex(MLH1_data)
MLH1_data <- add_subsp(MLH1_data)

#add a column with male adjusted MLH1 values (+1 to all males)
MLH1_data$adj_nMLH1.foci <- ifelse(MLH1_data$sex=="male", MLH1_data$nMLH1.foci+1, MLH1_data$nMLH1.foci)
MLH1_data$adj_nMLH1.foci <- as.numeric(MLH1_data$adj_nMLH1.foci)



#reorder dataframe
MLH1_data <- MLH1_data %>%
  arrange(strain, sex, mouse) %>%
  mutate(Original.Name = factor(Original.Name) )



#Make a mouse level table
AP_mouse_table <- ddply(MLH1_data, c("mouse"), summarise,
                        Nmice = length(unique(mouse)),
                        Ncells  = length(adj_nMLH1.foci),
                        mean_co = as.numeric(format(round(  mean(adj_nMLH1.foci), 3 ), nsmall=3) ),
                        cV = cv(adj_nMLH1.foci),
                        var = format(round(   var(adj_nMLH1.foci),3), nsmall=3),
                        sd   = round(sd(adj_nMLH1.foci), 3),
                        se   = round(sd / sqrt(Ncells), 3)
                        #quality?
)

source("src/CommonFunc_MLH1repo.R")
AP_mouse_table <- add_strain(AP_mouse_table)
AP_mouse_table <- add_subsp(AP_mouse_table)
AP_mouse_table <- add_sex(AP_mouse_table)
AP_mouse_table <- add_category(AP_mouse_table)

#make sure they are all factors
AP_mouse_table$mouse <- as.factor(AP_mouse_table$mouse)
AP_mouse_table$sex <- as.factor(AP_mouse_table$sex)
AP_mouse_table$strain <- as.factor(AP_mouse_table$strain)
AP_mouse_table$subsp <-  as.factor(AP_mouse_table$subsp)






############
# SAVE DFs #
############

#I;m trying to think of a better way to deal with printing out the status
#print(c("The mean MLH1 foci number is  ", mean(MLH1_data$nMLH1.foci, na.rm =TRUE), 
#"the distribution of quality scores is ", table(MLH1_data$quality)  ) )


save.image("data/MLH1/MLH1_data_setup_8.9.19.RData")
#  OutPut: big large MLH1_data (AP's) df, big DF of BD with just the mice I want.
#  MLH1_data_table, means and variance of AP and BD MLH1 values. made from seperate tables from AP and BD data.
#  make sure decimal places are consistant