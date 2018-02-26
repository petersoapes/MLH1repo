# write a function foradding category

#use source(MLH1setupDataFuncs.R)

library(dplyr)
#meta_data = read.csv("C:/Users/alpeterson7/Documents/MLH1data/data/ALP_MouseMetadata.csv")
setwd("C:/Users/alpeterson7/Documents/MLH1repo/")
MLH1_data = read.csv("data/AnonData.csv", header=TRUE)


add_category <- function(oldframe){
  dframe <- oldframe
  dframe$category <- "other"
  
  dframe$category <- ifelse(grepl("_WSB_f", dframe$Original.Name), "WSB female", 
                    ifelse(grepl("_WSB_m",dframe$Original.Name), "WSB male",
                 ifelse(grepl("_G_f", dframe$Original.Name), "G female",
              ifelse(grepl("_G_m",dframe$Original.Name), "G male",
               ifelse(grepl("_LEW_f", dframe$Original.Name), "LEW female",
              ifelse(grepl("_LEW_m",dframe$Original.Name), "LEW male",          
              ifelse(grepl("_PERC_f", dframe$Original.Name), "PERC female",
            ifelse(grepl("_PERC_m",dframe$Original.Name), "PERC male",   
                            
               ifelse(grepl("_CAST_m", dframe$Original.Name), "CAST male",
              ifelse(grepl("_CAST_f", dframe$Original.Name), "CAST female",
             ifelse(grepl("_HMI_m", dframe$Original.Name), "HMI male",
          ifelse(grepl("_HMI_f", dframe$Original.Name), "HMI female",
                     
             ifelse(grepl("_MSM_f", dframe$Original.Name), "MSM female",       
            ifelse(grepl("_MSM_m", dframe$Original.Name), "MSM male",
              ifelse(grepl("_PWD_m", dframe$Original.Name), "PWD male",     
               ifelse(grepl("_PWD_f", dframe$Original.Name), "PWD female", 
     
        ifelse(grepl("_SPRET_f", dframe$Original.Name), "SPRET female",
        ifelse(grepl("_SPRET_m", dframe$Original.Name), "SPRET male",
        ifelse(grepl("_SPIC_m", dframe$Original.Name), "SPIC male",
        ifelse(grepl("_SPIC_f", dframe$Original.Name), "SPIC female",          
        ifelse(grepl("_CAROLI_m", dframe$Original.Name), "CAROLI male",
       ifelse(grepl("_CAROLI_f", dframe$Original.Name), "CAROLI female",
                            "other"))))))))))))))))))))))
       
  dframe$category <- as.character(dframe$category)
  return(dframe)
}
#tried writing this with mutate, couldn't get it to work

testdf <- add_category(MLH1_data)
table(test.df$category)

# add strain
add_strain <- function(dat){
  dframe <- dat
  dframe$strain <- "other"
  
  dframe$strain <- ifelse(grepl("_WSB", dframe$Original.Name), "WSB", 
              ifelse(grepl("_G_",dframe$Original.Name), "G",
              ifelse(grepl("_LEW_", dframe$Original.Name), "LEWES",

             ifelse(grepl("_MSM_", dframe$Original.Name), "MSM",           
              ifelse(grepl("_PWD_", dframe$Original.Name), "PWD",
                       
           ifelse(grepl("_CAST_", dframe$Original.Name), "CAST",           
          ifelse(grepl("_HMI_", dframe$Original.Name), "HMI",
         ifelse(grepl("_SPRET_", dframe$Original.Name), "SPRET",           
      ifelse(grepl("_SPIC_", dframe$Original.Name), "SPIC",
     ifelse(grepl("_CAROLI_", dframe$Original.Name), "CAROLI",
                   "other"))))))))))
  dframe$strain <- as.character(dframe$strain)
  return(dframe)
     }

#add sex
add_sex <- function(dat){
  dframe <- dat
  dframe$sex <- "other"
  dframe$sex <- ifelse(grepl("_f", dframe$Original.Name), "female", 
                     ifelse(grepl("_m",dframe$Original.Name), "male", "other"))

  dframe$sex <- as.character(dframe$sex)
  return(dframe)
}


#add mouse column
add_mouse <- function(dat){
  dframe <- dat
  count =1
  for(i in dframe$Original.Name){
    #print(i)
    templist= strsplit(i, split="_")[[1]]
    c = paste(templist[2], templist[3],templist[4], sep = "_")
    dframe$mouse[count] <- c
    count= count +1
    }
  return(dframe)
}

