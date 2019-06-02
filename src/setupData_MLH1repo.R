##R script for making R.data files 
## read in data files and adjust with additional columns
## input: csv file of anon MLH1 counts
## output: .rdata saved workspace with dataframe and other stat tables (save tables)
## df of MLH1 should go from 12cols to 17cols
#import MLH1 measures across data sets; AP, BD's and Cast female Koehler
# 1. MLH1_data, tables: AP strains

library(plyr)
library(dplyr)
library(raster)#for cV
library(ggplot2)

########################
# Setup main dataframe #
########################

#TODO: remove observations without quality scores.
#12jan17_4jan17_LEW_f1_sp1_34_rev, 8feb17_4jan17_LEW_f1_sp1_3_rev, potential outliers

#Write a make file that will merge all of the batch files and record the batch
setwd("C:/Users/alpeterson7/Documents/MLH1repo/")
MLH1_data = read.csv("data/MLH1/AnonData.csv", header=TRUE )

original_DF = MLH1_data
#original_length <- length(MLH1_data$Original.Name)

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


#remove mice that had bad stains
#12sep16_MSM_f3(centromere signal bled into MLH1 signal), bad stain
MLH1_data <- MLH1_data[ !grepl("12sep16_MSM_f3", MLH1_data$mouse) , ]
MLH1_data <- MLH1_data[ !grepl("12sep16_MSM_f1", MLH1_data$mouse) , ]



#find a way to compare the list of AP_mice with Metadata, or list folders in Images
MouseMetaData = read.csv("data/ALP_MouseMetadata.csv", header=TRUE )

#full mouse list
Image_mice_dirs <- list.files(path = "C:/Users/alpeterson7/Documents/Images")


###########################
# Construct Lists of Mice #
###########################


#Dissected, but not imaged

#read in the dissection file
Dissection.File = read.csv("data/Disections_53019.csv", header=TRUE )

#cleaning up of Disection file
#standarize column names
#remove lines without mouse
Dissection.File <- Dissection.File[!(is.na(Dissection.File$mouse)|Dissection.File$mouse==""),]

#print the name with the date of the file somewhere

Dissection.list <- Dissection.File$mouse
#figure out a way to deal with the question marks, at least remove them temporarily

#remove the pero mice?

missing.mice <- unique(Dissection.File$mouse[(MLH1_data$mouse %in% Image_mice_dirs)])
#compare to the original list and also mark the number/enrichment of X's
#original_DF

list2 <- mice_image_folders[(mice_image_folders %in% AP_mouse_table$mouse)]



#Imaged, but not quantified

list <- unique(MLH1_data$mouse[(MLH1_data$mouse %in% Image_mice_dirs)])
list2 <- mice_image_folders[(mice_image_folders %in% AP_mouse_table$mouse)]
#length(list2) #124, 

#mice that are missing
missing_mice = subset(Image_mice_dirs, !(Image_mice_dirs %in% MLH1_data$mouse ) )
length(missing_mice)#136

missing_mice2 = subset(Image_mice_dirs, !(Image_mice_dirs %in% original_DF$mouse ) )
length(missing_mice2)#260

missing_mice.DF  <- as.data.frame(missing_mice)
colnames(missing_mice.DF) <- "mouse"

missing_mice.DF <- missing_mice.DF[missing_mice.DF$mouse != "__pycache__", ] #some reason this result is converted to chr
missing_mice.DF <- missing_mice.DF[missing_mice.DF$mouse != "misc", ]
missing_mice.DF <- missing_mice.DF[missing_mice.DF$mouse != "old", ]
missing_mice.DF <- missing_mice.DF[missing_mice.DF$mouse != "README.txt", ]

missing_mice.DF  <- as.data.frame(missing_mice.DF)
colnames(missing_mice.DF) <- "mouse"

missing_mice.DF$mouse  <- as.character(missing_mice.DF$mouse)

#str_split requires CHARAECTER!!

for( t in 1:length(missing_mice.DF$mouse)){
  euth.date <- as.Date(strsplit(missing_mice.DF$mouse[t], split="_")[[1]][1], format= '%d%b%y')
  missing_mice.DF$date[t] <- euth.date
}

#sort dataframe by date

#find the old mice, and investigate why, make a list of mice which weren't included due to bad staining, (but there are still folders)
# (check withoriginal datafram)
#having information for number of images might be helpful

#add strain and category to the file to make sorting easier.



#print out the list of mice, from 2016, and look at all image folders
#make notes of why the images haven't been quant'd
write.csv(missing_mice.DF, file = "checking_mice.csv")



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

#source("src/CommonFunc_MLH1repo.R")
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
print(c("The mean MLH1 foci number is  ", mean(MLH1_data$nMLH1.foci, na.rm =TRUE), 
"the distribution of quality scores is ", table(MLH1_data$quality)  ) )


save.image("data/MLH1/MLH1_data_setup.RData")
#  OutPut: big large MLH1_data (AP's) df, big DF of BD with just the mice I want.
#  MLH1_data_table, means and variance of AP and BD MLH1 values. made from seperate tables from AP and BD data.
#  make sure decimal places are consistant

