

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
