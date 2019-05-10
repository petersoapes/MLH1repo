# Common MLH1repo functions
#
#


#add mouse for short mouse name
add_mouse1 <- function(dat){
  dframe <- dat
  count =1
  for(i in dframe$Original.Name){
    print(i)
    templist= strsplit(i, split="_")[[1]]
    c = paste(templist[1], templist[2],templist[3], sep = "_")
    dframe$mouse[count] <- c
    count= count +1
  }
  return(dframe)
}

#this add mouse, is based on file names
add_mouse2 <- function(dat){
  dframe <- dat
  count =1
  for(i in dframe$Original.Name){
    print(i)
    templist= strsplit(i, split="_")[[1]]
    c = paste(templist[2], templist[3],templist[4], sep = "_")
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
  dframe$sex <- ifelse(grepl("_f", dframe$Original.Name), "female", 
                       ifelse(grepl("_m",dframe$Original.Name), "male", "other"))
  
  dframe$sex<- factor(dframe$sex,levels =c( "female", "male"), order=T )
  return(dframe)
}


# add strain
add_strain <- function(dat){
  dframe <- dat
  dframe$strain <- "other"
  
  dframe$strain <- ifelse(grepl("_WSB", dframe$Original.Name), "WSB", 
                          ifelse(grepl("_G_",dframe$Original.Name), "G",
        ifelse(grepl("_LEW", dframe$Original.Name), "LEWES",
                                        
                                        
             ifelse(grepl("_MSM_", dframe$Original.Name), "MSM",           
             ifelse(grepl("_PWD_", dframe$Original.Name), "PWD",
                ifelse(grepl("_KAZ_", dframe$Original.Name), "KAZ",
               ifelse(grepl("CZECH", dframe$Original.Name), "CZECH",         
                       
                                                             
                   ifelse(grepl("_CAST_", dframe$Original.Name), "CAST",           
                      ifelse(grepl("_HMI_", dframe$Original.Name), "HMI",
                     ifelse(grepl("_SPRET_", dframe$Original.Name), "SPRET",           
                             ifelse(grepl("_SPI_", dframe$Original.Name), "SPIC",
                         ifelse(grepl("_SPIC_", dframe$Original.Name), "SPIC",
                  ifelse(grepl("_CAROLI_", dframe$Original.Name), "CAROLI",
                                 "other")))))))))))))
  
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
                  ifelse(grepl("_CZECH_m", dframe$Original.Name), "CZECH male",     
                  ifelse(grepl("_CZECH_f", dframe$Original.Name), "CZECH female",           
                              
      
                       ifelse(grepl("_SPRET_f", dframe$Original.Name), "SPRET female",
                       ifelse(grepl("_SPRET_m", dframe$Original.Name), "SPRET male",
                            ifelse(grepl("_SPI_m", dframe$Original.Name), "SPIC male",
                      ifelse(grepl("_SPI_f", dframe$Original.Name), "SPIC female",
                                                                                                                                            ifelse(grepl("_SPIC_m", dframe$Original.Name), "SPIC male",
                                                                                                                                                ifelse(grepl("_SPIC_f", dframe$Original.Name), "SPIC female",        
                                                                                                                                                                                                  
                      ifelse(grepl("_CAROLI_m", dframe$Original.Name), "CAROLI male",
                     ifelse(grepl("_CAROLI_f", dframe$Original.Name), "CAROLI female",
                                                      "other")))))))))))))))))))))))))))))
  
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

