# write a function foradding category

#use source(MLH1setupDataFuncs.R)

library(dplyr)
#meta_data = read.csv("C:/Users/alpeterson7/Documents/MLH1data/data/ALP_MouseMetadata.csv")
setwd("C:/Users/alpeterson7/Documents/MLH1repo/")
#MLH1_data = read.csv("data/AnonData.csv", header=TRUE)


add_category <- function(oldframe){
  dframe <- oldframe
  dframe$category <- "other"
  
  dframe$category <- ifelse(grepl("_WSB_f", dframe$Original.Name), "WSB female",
                    ifelse(grepl("_WSB_m",dframe$Original.Name), "WSB male",
                
                   ifelse(grepl("_G_f", dframe$Original.Name), "G female",
              ifelse(grepl("_G_m",dframe$Original.Name), "G male",
                     ifelse(grepl("_LEWES_m", dframe$Original.Name), "LEW male",
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
            ifelse(grepl("_KAZ_m", dframe$Original.Name), "KAZ male",     
            ifelse(grepl("_KAZ_f", dframe$Original.Name), "KAZ female",           
                      
     
        ifelse(grepl("_SPRET_f", dframe$Original.Name), "SPRET female",
        ifelse(grepl("_SPRET_m", dframe$Original.Name), "SPRET male",
        ifelse(grepl("_SPI_m", dframe$Original.Name), "SPIC male",
        ifelse(grepl("_SPI_f", dframe$Original.Name), "SPIC female",          
        ifelse(grepl("_CAROLI_m", dframe$Original.Name), "CAROLI male",
       ifelse(grepl("_CAROLI_f", dframe$Original.Name), "CAROLI female",
                            "other")))))))))))))))))))))))))
       
  dframe$category<- factor(dframe$category,levels =c( "WSB female", "WSB male","G female", "G male", 
                         "LEW female", 'LEW male', "PERC male",
                          "PWD female", "PWD male", "MSM female", "MSM male", "KAZ female","KAZ male",
                              "CAST female", "CAST male", "HMI female", "HMI male",
                                "SPRET female", "SPRET male", "SPIC female", "SPIC male", "other"), order=T )
  return(dframe)
}
#tried writing this with mutate, couldn't get it to work