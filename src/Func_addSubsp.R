# add subspe

#the column might need to be changed

add_subsp <- function(dat) {
#assign subspecies to tables
subsp <- ifelse(grepl("WSB", Table_BD_strain$Cross), "Dom", 
           ifelse(grepl("G", Table_BD_strain$Cross), "Dom",
            ifelse(grepl("LEW", Table_BD_strain$Cross), "Dom", 
             ifelse(grepl("LEWES", Table_BD_strain$Cross), "Dom",    
            ifelse(grepl("PERA", Table_BD_strain$Cross), "Dom",
                                            
        ifelse(grepl("CAST", Table_BD_strain$Cross), "Cast",
           ifelse(grepl("CIM", Table_BD_strain$Cross), "Cast",
                                                          
        ifelse(grepl("MSM", Table_BD_strain$Cross), "Musc-Cast",                                       
           ifelse(grepl("PWD", Table_BD_strain$Cross), "Musc", 
            ifelse(grepl("CZECHI", Table_BD_strain$Cross), "Musc", 
         ifelse(grepl("PWDFemale", Table_BD_strain$Cross), "Musc",         
                                                                                      
        ifelse(grepl("PANCEVO", Table_BD_strain$Cross), "Spic", 
             ifelse(grepl("CAROLI", Table_BD_strain$Cross), "Caroli", 
                                                                                                    
          ifelse(grepl("RAT", Table_BD_strain$Cross), "Outgroup", 
       ifelse(grepl("Peromyscus", Table_BD_strain$Cross), "Outgroup", 
          ifelse(grepl("Microtus", Table_BD_strain$Cross), "Outgroup", "other"))))))))))))))))


}