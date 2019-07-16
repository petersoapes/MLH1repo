##R script for making R.data files 
## read in data files and adjust with additional columns
## input: csv file of anon MLH1 counts
## output: .rdata saved workspace with dataframe and other stat tables (save tables)
## df of MLH1 should go from 12cols to 17cols
#import MLH1 measures across data sets; AP, BD's and Cast female Koehler
# 1. MLH1_data, tables: AP strains

#library(plyr)
library(dplyr)
library(raster)#for cV
library(ggplot2)

########################
# Setup main dataframe #
########################

#TODO: remove observations without quality scores.
#12jan17_4jan17_LEW_f1_sp1_34_rev, 8feb17_4jan17_LEW_f1_sp1_3_rev, potential outliers

#Write a make file that will merge all of the batch files and record the batch
#setwd("C:/Users/alpeterson7/Documents/MLH1repo/")
setwd("~./MLH1repo/")

MLH1_data = read.csv("data/MLH1/AnonData.csv", header=TRUE ) #~400 more than the Rdata

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

#add DOB
#(find a way to compare the list of AP_mice with Metadata, or list folders in Images)

#MouseMetaData = read.csv("~./MLH1repo/data/ALP_MouseMetadata.csv", header=TRUE )
#meta.data.file2 = read.csv("~./MLH1repo/data/MouseMetaData61119.csv", header = TRUE)
meta.data6.12.19 = read.csv("~./MLH1repo/data/Mouse_MetaData_6.12.19.csv", header = TRUE)

meta.data6.12.19$DOB.org <- meta.data6.12.19$DOB #there are still places where 'e16'
meta.data6.12.19$DOB <- as.Date(meta.data6.12.19$DOB, format= "%m/%d/%y") #the non-format matching DOB are converted to NAs
meta.data6.12.19 <- meta.data6.12.19[!(is.na(meta.data6.12.19$mouse)|meta.data6.12.19$mouse==""),]

#calq age for metadata
meta.data6.12.19 <- add_euth_date(meta.data6.12.19)#this isn't working because there are many mouse values which are not n the right format!

#must convert euth.date to date (remove the non dates)
#AS long as the first characters are in the right format it seems to work
meta.data6.12.19$euth.date <- as.Date(as.character(meta.data6.12.19$euth.date), format='%d%b%y')

meta.data6.12.19 <- add_age(meta.data6.12.19)


#reduce metadata down to 2 cols (mouse, DOB), merge this with MLH1
MouseMetaData.wrkin <- meta.data6.12.19[,c(1,3)]

colnames(MouseMetaData.wrkin) <- c("mouse", "DOB")
MouseMetaData.wrkin <- MouseMetaData.wrkin[!(is.na(MouseMetaData.wrkin$mouse)|MouseMetaData.wrkin$mouse==""),]

#check how many times there is a duplicate mouse!
length(which(duplicated(MouseMetaData.wrkin$mouse)))
#currently 6 for the downloaded G doc data frame)

#this is how you get the names of the duplicate mice
MouseMetaData.wrkin$mouse[which(duplicated(MouseMetaData.wrkin$mouse))]

#merge with MLH1 and BivData
impMLH1_data <- merge(MLH1_data, MouseMetaData.wrkin, by.x = "mouse")#don't apply the all parameters

length(which(duplicated(impMLH1_data$Random.Name)))#29 duplicated, 6 duplicated..

length(which(duplicated(impMLH1_data$fileName)))# 117 fileName, 107 duplicated
#some of these are true duplicates, but some are the same image files that were quantified across multiple anon batches
Quant2batches <- impMLH1_data[which(duplicated(impMLH1_data$fileName)),]#G, LEW, SPI females

true.duplicates <- impMLH1_data[(which(duplicated(impMLH1_data$Random.Name))),]#but now these aren't duplicated...


#integrating dates
MouseMetaData.wrkin <- meta.data.file2[,c(1,4)]
colnames(MouseMetaData.wrkin) <- c("mouse", "DOB")
MouseMetaData.wrkin <- MouseMetaData.wrkin[!(is.na(MouseMetaData.wrkin$mouse)|MouseMetaData.wrkin$mouse==""),]

MLH1.imprv <- merge(MLH1_data, MouseMetaData.wrkin)#don't apply the all parameters


#MLH1 - 2921,   #ll -- 2227, 
#when all.x 3418  (using all.x produces duplicate rows (in MLH1 ))

#add the euth date col
for( t in 1:length(impMLH1_data$mouse)){
  euth.date <- as.Date(strsplit(impMLH1_data$mouse[t], split="_")[[1]][1], format= '%d%b%y')
  impMLH1_data$date[t] <- euth.date
}

#add the age col (this might need to be a function to deal with the female exceptions)


#reorder dataframe
MLH1_data <- MLH1_data %>%
  arrange(strain, sex, mouse) %>%
  mutate(Original.Name = factor(Original.Name) )


#remove mice that had bad stains
#12sep16_MSM_f3(centromere signal bled into MLH1 signal), bad stain
MLH1_data <- MLH1_data[ !grepl("12sep16_MSM_f3", MLH1_data$mouse) , ]
MLH1_data <- MLH1_data[ !grepl("12sep16_MSM_f1", MLH1_data$mouse) , ]


#full mouse list
Image_mice_dirs <- list.files(path = "C:/Users/alpeterson7/Documents/Images")
#write this into a file

###########################
# Construct Lists of Mice #
###########################

#Dissected, but not imaged

#read in the dissection file
#Dissection.File = read.csv("data/Disections_53019.csv", header=TRUE )
#try to incorporate information from sp1 (the first stain)
Dissection.File = read.csv("~./MLH1repo/data/Mouse_MetaData_6.12.19.csv", header = TRUE)

#cleaning up of Disection file
#standarize column names
#remove lines without mouse
Dissection.File <- Dissection.File[!(is.na(Dissection.File$mouse)|Dissection.File$mouse==""),]

#add age to meta-data file, to save as reference
meta.data.DF <- Dissection.File
meta.data.DF <- meta.data.DF[!(is.na(meta.data.DF$mouse)|meta.data.DF$mouse==""),] #375
meta.data.DF$DOB <- as.Date(meta.data.DF$DOB, format= "%m/%d/%Y")#%Y 2000, %y '18,

meta.data.DF <- add_category(meta.data.DF)
source("~./MLH1repo/src/CommonFunc_MLH1repo.R")
meta.data.DF <- add_euth_date(meta.data.DF)
meta.data.DF <- add_age(meta.data.DF)

#write meta.data.DF to file
write.table(meta.data.DF, "~./MLH1repo/results/meta.data.DF.txt", 
            sep="\t", row.names = FALSE)


#print the name with the date of the file somewhere
Dissection.list <- Dissection.File$mouse
Dissection.list <- as.character(Dissection.list)
  
#figure out a way to incorporate the staining... to get a better idea of which slides 
#to do next

#remove the pero mice?

missing.mice <- unique(Dissection.File$mouse[(MLH1_data$mouse %in% Image_mice_dirs)])

missing.mice2 <- unique(Dissection.File$mouse[(MLH1_data$mouse %in% Dissection.list)])

missing_mice99 = subset(Dissection.File, !(Dissection.File$mouse %in% MLH1_data$mouse ) )
missing_mice101 = subset(Dissection.File, !(Dissection.File$mouse %in% original_DF$mouse ) )

missing_mice99$mouse

length(missing_mice99)#306   #

# mice in MLH1, 126, 151, 
# mice in dissection list, 431, 384...
# mice from dissection list not in MLH1, 306

#compare to the original list and also mark the number/enrichment of X's
#original_DF

list2 <- mice_image_folders[(mice_image_folders %in% AP_mouse_table$mouse)]

#Imaged, but not quantified
#not sure if this is right?
list <- unique(MLH1_data$mouse[(MLH1_data$mouse %in% Image_mice_dirs)])
imaged.not.quant <- unique(MLH1_data[(MLH1_data$mouse %in% Image_mice_dirs),])

list2 <- mice_image_folders[(mice_image_folders %in% AP_mouse_table$mouse)]
#length(list2) #124, 

#mice that are missing
missing_mice = subset(Image_mice_dirs, !(Image_mice_dirs %in% MLH1_data$mouse ) )

length(missing_mice)#136

#missing_mice2 = subset(Image_mice_dirs, !(Image_mice_dirs %in% original_DF$mouse ) )
#length(missing_mice2)#260

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
print(c("The mean MLH1 foci number is  ", mean(MLH1_data$nMLH1.foci, na.rm =TRUE), 
"the distribution of quality scores is ", table(MLH1_data$quality)  ) )


save.image("data/MLH1/MLH1_data_setup_62419.RData")
#  OutPut: big large MLH1_data (AP's) df, big DF of BD with just the mice I want.
#  MLH1_data_table, means and variance of AP and BD MLH1 values. made from seperate tables from AP and BD data.
#  make sure decimal places are consistant