# add subspe

#the column might need to be changed

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
        
      ifelse(grepl("SPRET", dframe$strain), "Spretus",
   ifelse(grepl("SPIC", dframe$strain), "Spic",      
        ifelse(grepl("PANCEVO", dframe$strain), "Spic", 
             ifelse(grepl("CAROLI", dframe$strain), "Caroli", 
                                                                                                    
          ifelse(grepl("RAT", dframe$strain), "Outgroup", 
       ifelse(grepl("Peromyscus", dframe$strain), "Outgroup", 
          ifelse(grepl("Microtus", dframe$strain), "Outgroup", "other")))))))))))))))))))
  return(dframe)

}