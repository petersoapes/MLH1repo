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

########################
# Setup main dataframe #
########################

#TODO: remove observations without quality scores.
#12jan17_4jan17_LEW_f1_sp1_34_rev, 8feb17_4jan17_LEW_f1_sp1_3_rev, potential outliers

#Write a make file that will merge all of the batch files and record the batch
setwd("C:/Users/alpeterson7/Documents/MLH1repo/")
MLH1_data = read.csv("data/AnonData.csv", header=TRUE )

original_DF = MLH1_data
original_length <- length(MLH1_data$Original.Name)

MLH1_data$Original.Name <- as.character(MLH1_data$Original.Name)

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

# add pass or fail column.. this will be a large list..maybe write a function..
# highlight real passes? highlight non passes?
# calculate automatically, list manually?


source("src/Func_addCategory.R")
MLH1_data <- add_category(MLH1_data)
#set the order of categories (female, male) (cast, dom, musc)

source("src/Func_addStrain.R")
MLH1_data <- add_strain(MLH1_data)
source("src/Func_addSex.R")
MLH1_data <- add_sex(MLH1_data)

#add a column with male adjusted MLH1 values (+1 to all males)
MLH1_data$adj_nMLH1.foci <- ifelse(MLH1_data$sex=="male", MLH1_data$nMLH1.foci+1, MLH1_data$nMLH1.foci)

MLH1_data$adj_nMLH1.foci <- as.numeric(MLH1_data$adj_nMLH1.foci)

source("src/Func_addMouse.R")
MLH1_data <- add_mouse(MLH1_data)

#add subsp
source("src/Func_addSubsp.R")
MLH1_data <- add_subsp(MLH1_data)

#reorder dataframe
MLH1_data <- MLH1_data %>%
  arrange(strain, sex, mouse) %>%
  mutate(Original.Name = factor(Original.Name))


#remove mice that had bad stains
#12sep16_MSM_f3(centromere signal bled into MLH1 signal), bad stain
MLH1_data <- MLH1_data[ !grepl("12sep16_MSM_f3", MLH1_data$mouse) , ]
MLH1_data <- MLH1_data[ !grepl("12sep16_MSM_f1", MLH1_data$mouse) , ]

#Make a mouse table
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

#add strain, sex ect #function won't work
# format so that mean_co is numer
AP_mouse_table$subsp <-  ifelse(grepl("_WSB_", AP_mouse_table$mouse), "Dom", 
                               ifelse(grepl("_G_", AP_mouse_table$mouse), "Dom",
                                ifelse(grepl("_LEW", AP_mouse_table$mouse), "Dom",
                                  ifelse(grepl("PWD", AP_mouse_table$mouse), "Musc",
                               ifelse(grepl("MSM", AP_mouse_table$mouse), "Musc",  
                              ifelse(grepl("KAZ", AP_mouse_table$mouse), "Musc",        
                                      
                                      ifelse(grepl("CAST", AP_mouse_table$mouse), "Cast",         
                              ifelse(grepl("HMI", AP_mouse_table$mouse), "Cast",    
                               ifelse(grepl("SPI", AP_mouse_table$mouse), "outgroup",         
                                ifelse(grepl("CAROLI", AP_mouse_table$mouse), "outgroup",   
                                       ifelse(grepl("SPRET", AP_mouse_table$mouse), "outgroup",       
                                        "other")))))))))))

AP_mouse_table$strain <-  ifelse(grepl("_WSB_", AP_mouse_table$mouse), "WSB", 
                           ifelse(grepl("_G_", AP_mouse_table$mouse), "G",
                          ifelse(grepl("_LEW", AP_mouse_table$mouse), "LEW",
                          ifelse(grepl("PWD", AP_mouse_table$mouse), "PWD",
                          ifelse(grepl("MSM", AP_mouse_table$mouse), "MSM", 
                          ifelse(grepl("KAZ", AP_mouse_table$mouse), "KAZ",          
                          ifelse(grepl("CAST", AP_mouse_table$mouse), "CAST",         
                          ifelse(grepl("HMI", AP_mouse_table$mouse), "HMI",    
                         ifelse(grepl("SPI", AP_mouse_table$mouse), "SPIC",         
                          ifelse(grepl("CAROLI", AP_mouse_table$mouse), "CAROLI",   
                        ifelse(grepl("SPRET", AP_mouse_table$mouse), "SPRET",       
                                                "other")))))))))))


AP_mouse_table$sex <-  ifelse(grepl("_f", AP_mouse_table$mouse), "female", 
                       ifelse(grepl("_m", AP_mouse_table$mouse), "male",
                                                "other"))

AP_mouse_table$mouse <- as.factor(AP_mouse_table$mouse)
AP_mouse_table$sex <- as.factor(AP_mouse_table$sex)
AP_mouse_table$strain <- as.factor(AP_mouse_table$strain)
  AP_mouse_table$subsp <-  as.factor(AP_mouse_table$subsp)
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
