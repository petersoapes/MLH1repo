

#add sex
add_sex <- function(dat){
  dframe <- dat
  dframe$sex <- "other"
  dframe$sex <- ifelse(grepl("_f", dframe$Original.Name), "female", 
                       ifelse(grepl("_m",dframe$Original.Name), "male", "other"))
  
  dframe$sex <- as.character(dframe$sex)
  return(dframe)
}
