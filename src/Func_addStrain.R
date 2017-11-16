

# add strain
add_strain <- function(dat){
  dframe <- dat
  dframe$strain <- "other"
  
  dframe$strain <- ifelse(grepl("_WSB", dframe$Original.Name), "WSB", 
              ifelse(grepl("_G_",dframe$Original.Name), "G",
           ifelse(grepl("_LEW", dframe$Original.Name), "LEWES",
                                  
            ifelse(grepl("_MSM_", dframe$Original.Name), "MSM",           
           ifelse(grepl("_PWD_", dframe$Original.Name), "PWD",
                                                      
                ifelse(grepl("_CAST_", dframe$Original.Name), "CAST",           
         ifelse(grepl("_HMI_", dframe$Original.Name), "HMI",
              ifelse(grepl("_SPRET_", dframe$Original.Name), "SPRET",           
             ifelse(grepl("_SPI_", dframe$Original.Name), "SPIC",
          ifelse(grepl("_CAROLI_", dframe$Original.Name), "CAROLI",
                                       "other"))))))))))
  
  #the ordering factor below deletes all strain entries
  dframe$strain<- factor(dframe$strain,levels =c( "WSB", "G", "LEWES", "PERC",
                                                        "PWD", "MSM",
                                                        "CAST", "HMI",
                                                        "SPRET", "SPIC", "other"), order=T )
  return(dframe)
}
