# Common MLH1repo functions
#
#


#add mouse for short mouse name
add_mouse <- function(dat){
  dframe <- dat
  count =1
  for(i in dframe$fileName){
    #print(i)
    templist= strsplit(i, split="_")[[1]]
    c = paste(templist[2],templist[3], templist[4], sep = "_")
    dframe$mouse[count] <- c
    count= count +1
  }
  return(dframe)
}



#add mouse column to MLH1 df
#add mouse column to MLH1 df
add_euth_date <- function(dat){
  dframe <- dat
  count =1
  for(i in dframe$Original.Name){
    #isolate the first part of Orignal name
    #print(i)
    templist= strsplit(i, split="_")[[1]]
    euth <- templist[1]
    #figure out date conversion
    dframe$euth.date[count] <- euth
    count= count +1
  }
  dframe$euth.date <- as.Date(dframe$euth.date,format='%d%b%y')
  return(dframe)
}

#format the dates into standard format
#mouse_list$DOB <- as.Date(mouse_list$DOB,format='%m/%d/%Y')
add_age <- function(dat){
  dframe <- dat
  count =1
  for(i in dframe$Original.Name){
    #after DOB and euth date are standardized #substract
    dframe$age.days <- dframe$euth.date-dframe$DOB
    dframe$age.weeks <- difftime(dframe$euth.date,dframe$DOB,units='weeks')
    #gives age in days
    count= count +1
  }
  return(dframe)
}



#add sex to dataframe
add_sex <- function(dat){
  dframe <- dat
  dframe$sex <- "other"
  dframe$sex <- ifelse(grepl("_f", dframe$mouse), "female", 
                       ifelse(grepl("_m",dframe$mouse), "male", "other"))
  dframe$sex<- factor(dframe$sex,levels =c( "female", "male"), order=T )
  return(dframe)
}



# add strain
add_strain <- function(dat){
  dframe <- dat
  dframe$strain <- "other"
  
  dframe$strain <- ifelse(grepl("_WSB_", dframe$mouse), "WSB",
                          ifelse(grepl("_G_",dframe$mouse), "G",
              ifelse(grepl("_LEW_", dframe$mouse), "LEWES",
               ifelse(grepl("PERC", dframe$mouse), "PERC",
                                        
             ifelse(grepl("_MSM_", dframe$mouse), "MSM",    
             ifelse(grepl("_PWD_", dframe$mouse), "PWD",
                ifelse(grepl("_KAZ_", dframe$mouse), "KAZ",
               ifelse(grepl("_CZECH_", dframe$mouse), "CZECH",
                                                             
                   ifelse(grepl("_CAST_", dframe$mouse), "CAST",
                      ifelse(grepl("_HMI_", dframe$mouse), "HMI",
                     ifelse(grepl("_SPRET_", dframe$mouse), "SPRET",
                             ifelse(grepl("_SPI_", dframe$mouse), "SPIC",
                         ifelse(grepl("_SPIC_", dframe$mouse), "SPIC",
                  ifelse(grepl("_CAROLI_", dframe$mouse), "CAROLI",
                                 "other"))))))))))))))
  
  #the ordering factor below deletes all strain entries
  dframe$strain<- factor(dframe$strain,levels =c( "WSB", "G", "LEWES", "PERC",
                                                  "PWD", "MSM","KAZ","CZECH",
                                                  "CAST", "HMI",
                                                  "SPRET", "SPIC", "CAROLI", "other"), order=T )
  return(dframe)
}



add_category <- function(oldframe){
  dframe <- oldframe
  dframe$category <- "other"
  
  dframe$category <- ifelse(grepl("_WSB_f", dframe$mouse), "WSB female",
                    ifelse(grepl("_WSB_m",dframe$mouse), "WSB male",
                                   
                    ifelse(grepl("_G_f", dframe$mouse), "G female",
                    ifelse(grepl("_G_m",dframe$mouse), "G male",
                         ifelse(grepl("_LEWES_m", dframe$mouse), "LEW male",
                        ifelse(grepl("_LEW_f", dframe$mouse), "LEW female",
                           ifelse(grepl("_LEW_m",dframe$mouse), "LEW male",        
                                                                      
                    ifelse(grepl("_PERC_f", dframe$mouse), "PERC female",
                     ifelse(grepl("_PERC_m",dframe$mouse), "PERC male",
                                                                          
                        ifelse(grepl("_CAST_m", dframe$mouse), "CAST male",
                     ifelse(grepl("_CAST_f", dframe$mouse), "CAST female",
                        ifelse(grepl("_HMI_m", dframe$mouse), "HMI male",
                     ifelse(grepl("_HMI_f", dframe$mouse), "HMI female",
                                                                                                                
                    ifelse(grepl("_MSM_f", dframe$mouse), "MSM female",       
                   ifelse(grepl("_MSM_m", dframe$mouse), "MSM male",
                    ifelse(grepl("_PWD_m", dframe$mouse), "PWD male",     
                       ifelse(grepl("_PWD_f", dframe$mouse), "PWD female",
                        ifelse(grepl("_KAZ_m", dframe$mouse), "KAZ male",     
                       ifelse(grepl("_KAZ_f", dframe$mouse), "KAZ female",  
                  ifelse(grepl("_CZECH_m", dframe$mouse), "CZECH male",     
                  ifelse(grepl("_CZECH_f", dframe$mouse), "CZECH female",           
      
                       ifelse(grepl("_SPRET_f", dframe$mouse), "SPRET female",
                       ifelse(grepl("_SPRET_m", dframe$mouse), "SPRET male",
                            ifelse(grepl("_SPI_m", dframe$mouse), "SPIC male",
                      ifelse(grepl("_SPI_f", dframe$mouse), "SPIC female",
                       ifelse(grepl("_SPIC_m", dframe$mouse), "SPIC male",
                      ifelse(grepl("_CAROLI_m", dframe$mouse), "CAROLI male",
                     ifelse(grepl("_CAROLI_f", dframe$mouse), "CAROLI female",
                                                      "other"))))))))))))))))))))))))))))))
  
  dframe$category<- factor(dframe$category,levels =c( "WSB female", "WSB male","G female", "G male", 
                                                      "LEW female", 'LEW male', "PERC male",
                                  "PWD female", "PWD male", "MSM female", "MSM male", "KAZ female","KAZ male","CZECH female","CZECH male",
                                                      "CAST female", "CAST male", "HMI female", "HMI male",
                                                      "SPRET female", "SPRET male", "SPIC female", "SPIC male","CAROLI female","CAROLI male",
                                                      "other"), order=T )
  return(dframe)
}
#tried writing this with mutate, couldn't get it to work



add_subsp <- function(oldframe) {
  #assign subspecies to tables
  dframe <- oldframe
  dframe$subsp <- "other"
  
  dframe$subsp <- ifelse(grepl("WSB", dframe$strain), "Dom", 
                         ifelse(grepl("G", dframe$strain), "Dom",
                                ifelse(grepl("LEW", dframe$strain), "Dom", 
                                       ifelse(grepl("LEWES", dframe$strain), "Dom",    
                       ifelse(grepl("PERA", dframe$strain), "Dom",
                                                     
                     ifelse(grepl("CAST", dframe$strain), "Cast",
                    ifelse(grepl("CIM", dframe$strain), "Cast",
                     ifelse(grepl("HMI", dframe$strain), "Cast",
                                                                          
                           ifelse(grepl("MSM", dframe$strain), "Musc",                                       
                         ifelse(grepl("PWD", dframe$strain), "Musc", 
        ifelse(grepl("CZECHI", dframe$strain), "Musc", 
                ifelse(grepl("PWDFemale", dframe$strain), "Musc",  
                      ifelse(grepl("KAZ", dframe$strain), "Musc",
                                                                                                             
                     ifelse(grepl("SPRET", dframe$strain), "Spretus",
               ifelse(grepl("SPIC", dframe$strain), "Spic",      
                                         ifelse(grepl("PANCEVO", dframe$strain), "Spic", 
                          ifelse(grepl("CAROLI", dframe$strain), "Caroli", 
                                                                                                                                         
                        ifelse(grepl("RAT", dframe$strain), "Outgroup", 
                    ifelse(grepl("Peromyscus", dframe$strain), "Outgroup", 
                      ifelse(grepl("Microtus", dframe$strain), "Outgroup", "other"))))))))))))))))))))
  return(dframe)
  
}

#
#

#this add mouse, is based on file names
add_mouse2 <- function(dat){
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

