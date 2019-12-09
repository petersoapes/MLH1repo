# Common MLH1repo functions
#
#

#make functions for cleaning up BivData
#-remove p_rev
#-check for multi-cell images



#make fileName
#paste together file name
add_fullFileName <- function(dat){
  count =1
  dat$fileName <- as.character(dat$fileName)
  dat$stain <- as.character(dat$stain)
  
  for(i in dat$fileName){  #i is file name!!!
    #print(i)
    templist1= strsplit(i, split="_")[[1]] #put the 
    
    j=dat$stain[count]
    templist2= strsplit(j, split="_")[[1]]
    c = paste(templist1[2], templist1[3],templist1[4], sep = "_")#mdate, strain, m#
    #test.old.master$mouse2[count] <- c #don't need to add or change mouse
    
    d = paste(templist1[1], templist1[2], templist1[3],templist1[4],templist2[3], dat$cell[count], 'rev.tif', sep = "_") 
    dat$file.name_full[count] <- d #should match file name
    count= count + 1
  }
  return(dat)
}

#format the dates into standard format
#mouse_list$DOB <- as.Date(mouse_list$DOB,format='%m/%d/%Y')
add_age <- function(dat){
  dframe <- dat
  count =1
  for(i in dframe$mouse){
    #after DOB and euth date are standardized #substract
    dframe$age.days[count] <- dframe$euth.date[count] - dframe$DOB[count]
    dframe$age.weeks[count] <- difftime(dframe$euth.date[count], dframe$DOB[count], units='weeks')
    #gives age in days
    count= count +1
  }
  return(dframe)
}
#running this blanks out all cells in euth.date col

#calq maternal age time of mouse birth
add_mat_age <- function(dat){
  dframe <- dat
  count =1
  for(i in dframe$mouse){
    #after DOB and euth date are standardized #substract  difftime(mat.age.dob - DOB)
    dframe$mat.age.days[count]<-difftime(dframe$DOB[count], dframe$maternal_DOB[count],units='days')
    
    #dframe$age.weeks[count] <- difftime(dframe$euth.date[count], dframe$DOB[count], units='weeks')
    #gives age in days
    count= count +1
  }
  return(dframe)
}


#add mouse column to MLH1 df
#this can be mess up when the columns aren't right type (charaecter)
add_euth_date <- function(dat){
  dframe <- dat
  count =1
  for(i in dframe$mouse){
    #isolate the first part of Orignal name
    #print(i)
    templist= strsplit(as.character(i), split="_")[[1]] #split must act of charaecter
    euth <- templist[1]
    #figure out date conversion
    dframe$euth.date[count] <- euth
    dframe$euth.date[count] <- as.character(dframe$euth.date[count])
    count= count +1
  }
  dframe$euth.date <- as.Date(as.character(dframe$euth.date), format='%d%b%y')
  return(dframe)
}


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



#add sex to dataframe
add_sex <- function(dat){
  dframe <- dat
  dframe$sex <- "other"
  dframe$sex <- ifelse(grepl("_f", dframe$mouse), "female", 
                       ifelse(grepl("_m",dframe$mouse), "male", "other"))
  dframe$sex<- factor(dframe$sex, ordered = TRUE, levels =c( "female", "male"))
  return(dframe)
}



# add strain
add_strain <- function(dat){
  dframe <- dat
  dframe$strain <- "other"
  
  dframe$strain <- ifelse(grepl("_WSB_", dframe$mouse), "WSB",
                          ifelse(grepl("_G_",dframe$mouse), "G",
              ifelse(grepl("_LEW_", dframe$mouse), "LEW",
            ifelse(grepl("_LEWES_", dframe$mouse), "LEW",
               ifelse(grepl("PERC", dframe$mouse), "PERC",
                                        
             ifelse(grepl("_MSM_", dframe$mouse), "MSM",
            ifelse(grepl("_MOLF_", dframe$mouse), "MOLF",
                   ifelse(grepl("_SKIVE_", dframe$mouse), "SKIVE",      
             ifelse(grepl("_PWD_", dframe$mouse), "PWD",
                ifelse(grepl("_KAZ_", dframe$mouse), "KAZ",
                   ifelse(grepl("_TOM_", dframe$mouse), "TOM",
                   ifelse(grepl("_AST_", dframe$mouse), "AST",
               ifelse(grepl("_CZECH_", dframe$mouse), "CZECH",
                      
                                                             
                   ifelse(grepl("_CAST_", dframe$mouse), "CAST",
                      ifelse(grepl("_HMI_", dframe$mouse), "HMI",
                     ifelse(grepl("_SPRET_", dframe$mouse), "SPRET",
                             ifelse(grepl("_SPI_", dframe$mouse), "SPIC",
                         ifelse(grepl("_SPIC_", dframe$mouse), "SPIC",
                  ifelse(grepl("_CAROLI_", dframe$mouse), "CAROLI",
                         
                 ifelse(grepl("F1_", dframe$mouse), "F1",         
                                 "other"))))))))))))))))))))
  
  #the ordering factor below deletes all strain entries
  dframe$strain<- factor(dframe$strain, ordered = TRUE, levels =c( "WSB", "G", "LEW", "PERC",
                                                  "PWD", "MSM", "MOLF","SKIVE", "KAZ", "TOM", "AST","CZECH",
                                                  "CAST", "HMI",
                                                  "SPRET", "SPIC", "CAROLI", "F1", "other") )
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
                 ifelse(grepl("_MOLF_f", dframe$mouse), "MOLF female",       
                 ifelse(grepl("_MOLF_m", dframe$mouse), "MOLF male",       
                          
                    ifelse(grepl("_PWD_m", dframe$mouse), "PWD male",
                       ifelse(grepl("_PWD_f", dframe$mouse), "PWD female",
                        ifelse(grepl("_KAZ_m", dframe$mouse), "KAZ male",     
                       ifelse(grepl("_KAZ_f", dframe$mouse), "KAZ female",
                              
                      ifelse(grepl("_AST_m", dframe$mouse), "AST male",     
                     ifelse(grepl("_TOM_m", dframe$mouse), "TOM male",
                              
                  ifelse(grepl("_CZECH_m", dframe$mouse), "CZECH male",     
                  ifelse(grepl("_CZECH_f", dframe$mouse), "CZECH female",  
          ifelse(grepl("_SKIVE_m", dframe$mouse), "SKIVE male",     
        ifelse(grepl("_SKIVE_f", dframe$mouse), "SKIVE female",            
                         
      
                       ifelse(grepl("_SPRET_f", dframe$mouse), "SPRET female",
                       ifelse(grepl("_SPRET_m", dframe$mouse), "SPRET male",
                            ifelse(grepl("_SPI_m", dframe$mouse), "SPIC male",
                     ifelse(grepl("_SPI_f", dframe$mouse), "SPIC female",
                      ifelse(grepl("_SPIC_f", dframe$mouse), "SPIC female",
                       ifelse(grepl("_SPIC_m", dframe$mouse), "SPIC male",
                      ifelse(grepl("_CAROLI_m", dframe$mouse), "CAROLI male",
                     ifelse(grepl("_CAROLI_f", dframe$mouse), "CAROLI female",
                                                     
                  ifelse(grepl("F1_m", dframe$mouse), "F1",     
                             "other"))))))))))))))))))))))))))))))))))))
  
  dframe$category<- factor(dframe$category, ordered=TRUE, levels =c( "WSB female", "WSB male","G female", "G male", 
                                                      "LEW female", 'LEW male', "PERC male",
                                  "PWD female", "PWD male", "MSM female", "MSM male", "MOLF female", 
                                  "MOLF male",  "SKIVE female", "SKIVE male", 
                                  "KAZ female","KAZ male","CZECH female","CZECH male", "AST male", "TOM male",
                                  "CAST female", "CAST male", "HMI female", "HMI male",
                                  "SPRET female", "SPRET male", "SPIC female", "SPIC male","CAROLI female","CAROLI male","F1",
                                                      "other"))
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
                      ifelse(grepl("PERC", dframe$strain), "Dom",
                                                            
                     ifelse(grepl("CAST", dframe$strain), "Cast",
                    ifelse(grepl("CIM", dframe$strain), "Cast",
                     ifelse(grepl("HMI", dframe$strain), "Cast",
                                                                          
                           ifelse(grepl("MSM", dframe$strain), "Musc", 
                        ifelse(grepl("MOLF", dframe$strain), "Musc",    
                
                  ifelse(grepl("PWD", dframe$strain), "Musc", 
      
                  ifelse(grepl("CZECH", dframe$strain), "Musc", 
               ifelse(grepl("AST", dframe$strain), "Musc",     
                      ifelse(grepl("TOM", dframe$strain), "Musc",  
                  ifelse(grepl("SKIVE", dframe$strain), "Musc", 
                             
                ifelse(grepl("PWDFemale", dframe$strain), "Musc",
                      ifelse(grepl("KAZ", dframe$strain), "Musc",
                             
                                                                                                             
                     ifelse(grepl("SPRET", dframe$strain), "Spretus",
               ifelse(grepl("SPIC", dframe$strain), "Spic",      
                                         ifelse(grepl("PANCEVO", dframe$strain), "Spic", 
                          ifelse(grepl("CAROLI", dframe$strain), "Caroli", 
                                                                                                                                         
                        ifelse(grepl("RAT", dframe$strain), "Outgroup", 
                    ifelse(grepl("Peromyscus", dframe$strain), "Outgroup", 
                      ifelse(grepl("Microtus", dframe$strain), "Outgroup", "other")))))))))))))))))))))))))
  
  dframe$subsp <- factor(dframe$subsp, ordered = TRUE,levels =c( "Dom",
                                                      "Cast",
                                                      "Musc",
                                                      "Spretus","Spic","Caroli", "Outgroup",
                                                      "other") )
  
  
  
  return(dframe)
  
}

#
#
add_species <- function(oldframe) {
  #assign subspecies to tables
  dframe <- oldframe
  dframe$species <- "other"
  
  dframe$species <- ifelse(grepl("WSB", dframe$strain), "M.musculus", 
                    ifelse(grepl("G", dframe$strain), "M.musculus",
                    ifelse(grepl("LEW", dframe$strain), "M.musculus", 
                    ifelse(grepl("LEWES", dframe$strain), "M.musculus",    
                    ifelse(grepl("PERA", dframe$strain), "M.musculus",
                    ifelse(grepl("PERC", dframe$strain), "M.musculus",
                           
                                                            
                        ifelse(grepl("CAST", dframe$strain), "M.musculus",
                       ifelse(grepl("CIM", dframe$strain), "M.musculus",
                     ifelse(grepl("HMI", dframe$strain), "M.musculus",
                                                                          
                     ifelse(grepl("MSM", dframe$strain), "M.musculus",                                       
                     ifelse(grepl("PWD", dframe$strain), "M.musculus", 
              ifelse(grepl("CZECHI", dframe$strain), "M.musculus", 
                     ifelse(grepl("CZECH", dframe$strain), "M.musculus", 
                           ifelse(grepl("PWDFemale", dframe$strain), "M.musculus",  
                         ifelse(grepl("KAZ", dframe$strain), "M.musculus",
                                
                      ifelse(grepl("SKIVE", dframe$strain), "M.musculus",            
                     ifelse(grepl("MOLF", dframe$strain), "M.musculus",  
                    ifelse(grepl("TOM", dframe$strain), "M.musculus",  
                   ifelse(grepl("AST", dframe$strain), "M.musculus",
                                
                                                                                                             
                                    ifelse(grepl("SPRET", dframe$strain), "M.spretus",
                                        ifelse(grepl("SPIC", dframe$strain), "M.spic",      
                                      ifelse(grepl("PANCEVO", dframe$strain), "M.spic", 
                                    ifelse(grepl("CAROLI", dframe$strain), "M.caroli", 
                                                                                                                                         
                                         ifelse(grepl("RAT", dframe$strain), "R.norv", 
                                           ifelse(grepl("Peromyscus", dframe$strain), "Peromyscus", 
                                        ifelse(grepl("Microtus", dframe$strain), "Microtus", "other"))))))))))))))))))))))))))
  
  dframe$species <- factor(dframe$species, ordered = TRUE,levels =c( "M.musculus",
                                                                 "M.spretus",
                                                                 "M.spic",
                                                                 "M.caroli","R.norv","Peromyscus", "Microtus",
                                                                 "other") )
  
  
  return(dframe)
  
}


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

#make sis-co-ten
#metric for calculating relative / predicted about of sister-cohesion tension at metaphase
#requires handfocicount, Foci positions, and IFD
#start with ABS measures

add_SisCoTen <- function(DF){
  
  #DF$SisCoTen <- ifelse(  as.numeric(DF$hand.foci.count) == 1 , print(c(DF$chromosomeLength - DF$Foci1) ),
  #          ifelse( as.numeric(DF$hand.foci.count) == 1, (as.integer(DF$chromosomeLength) - as.integer(DF$Foci1)),"" ))

  #this chain of ifelse works!
  #DF$SisCoTen2 <- ifelse(  as.numeric(DF$hand.foci.count) == 1, "mee",
  #                        #print(c(DF$chromosomeLength - DF$Foci1) ),
  #                  ifelse(  as.numeric(DF$hand.foci.count) == 2 , "meee2",
  #                                 #print(c(DF$chromosomeLength - DF$Foci1) ), "")
   #                       ifelse(  as.numeric(DF$hand.foci.count) == 3,  "meee3","")))

  DF$SisCoTen <- ifelse(  as.numeric(DF$hand.foci.count) == 1,DF$chromosomeLength - DF$Foci1,
                    ifelse(  as.numeric(DF$hand.foci.count) == 2, DF$IFD1,
      ifelse(  as.numeric(DF$hand.foci.count) == 3, as.numeric(DF$IFD1) + (as.numeric(DF$chromosomeLength)-as.numeric(DF$Foci3) ), "")))
  #adding the as.numeric around the pieces of expression above fixed the tiny bug

  DF$SisCoTen <- as.numeric(DF$SisCoTen)#not sure if numeric or integer is the best
  
  return(DF)
}


#make foci_PER



#Calculate InterFocal Distance (IFD) in Chrm1 dataframe
#requires Percent foci positions, for the sceiont part

add_IFD <- function(DF){
  
  DF$IFD1_ABS <- ifelse(  (DF$hand.foci.count >= 2),
                          #manual.verified.f1, 
                          as.numeric(DF$Foci2) - as.numeric(DF$Foci1),   "" )
  DF$IFD1_ABS <- as.numeric(DF$IFD1_ABS)
  
  
  DF$IFD1_PER <- ifelse(  (DF$hand.foci.count >= 2), 
                          as.numeric(DF$Foci2.PER) - as.numeric(DF$Foci1.PER),   "" )
  
  DF$IFD1_PER <- as.numeric(DF$IFD1_PER)
  
  return(DF)
}



#add second IFD for 3COs or more
#this function seems to f-up the hand.foci.count
#First form used colnames from DNA Crossover. With curation step -- I'm switching to the curation step colnames
#these have more reliable values

add_IFD.2 <- function(DF){
  
  DF$IFD1 <- ifelse(  as.numeric(DF$hand.foci.count >= 2), 
                      DF$Foci2 - DF$Foci1,   "" )
  
  DF$IFD2 <- ifelse(  as.numeric(DF$hand.foci.count >= 3), 
                      DF$Foci3 - DF$Foci2,   "" )
  
  DF$IFD3 <- ifelse(  as.numeric(DF$hand.foci.count >= 4), 
                      DF$Foci4 - DF$Foci3,   "" )
  
  DF$IFD4 <- ifelse(  as.numeric(DF$hand.foci.count >= 5), 
                      DF$Foci5 - DF$Foci4,   "" )
  
  
  return(DF)
}




#taken from wild mouse dir, 
#needs to be updated for the new colnames
#calculate Interchromsomal rbar
#tested with values from Veller et al 2018
calq.intra.rbar <- function(chrm_row){
  o = 1
  # rrbar <- c()
  for(o in 1:length(chrm_row$fileName) ){
    #print(chrm_row$fileName[o])
    if(grepl(0, chrm_row$hand.foci.count[o])){
      #print("fist has 0 CO")
      #rrbar[o] <- NA
      rrbar <- 0
    }
    if(grepl(1, chrm_row$hand.foci.count[o])){  #do the block here
      #print("fist has 1 CO")
      #rrbar[o] <- "1CO"
      portion1 <- chrm_row$Foci1[o]
      portion2  <- chrm_row$chromosomeLength[o] - portion1
      portion.sum <- portion1+portion2
      rel.portion1 <- (portion1 / portion.sum)
      rel.portion2 <- (portion2 / portion.sum)
      rel.por.sqrd1 <- rel.portion1^2
      rel.por.sqrd2 <- rel.portion2^2
      chrm.sq <- sum(rel.por.sqrd1, rel.por.sqrd2)
      
      #rbar[o] <- 0.5*(1-chrm.sq)
      #print(chrm_row$rbar[o])
      rrbar <- 0.5*(1-chrm.sq)
      #o = o +1
    }
    if(grepl(2, chrm_row$hand.foci.count[o])){  #do the block here
      #print("this has 2 CO")
      portion1 <- chrm_row$Foci1[o]
      portion2  <- chrm_row$Foci2[o] - chrm_row$Foci1[o]
      portion3 <- chrm_row$chromosomeLength[o] - chrm_row$Foci2[o]+1
      portion.sum <- portion1+portion2+portion3 #in 2CO portions 
      rel.portion1 <- (portion1 / portion.sum)
      rel.portion2 <- (portion2 / portion.sum)
      rel.portion3 <- (portion3 / portion.sum)
      rel.por.sqrd1 <- rel.portion1^2
      rel.por.sqrd2 <- rel.portion2^2
      rel.por.sqrd3 <- rel.portion3^2
      chrm.sq <- sum(rel.por.sqrd1,rel.por.sqrd2,rel.por.sqrd3)
      
      chrm_row$rbar[o] <- 0.5*(1-chrm.sq)
      #  rrbar[o] <- 0.5*(1-chrm.sq)
      rrbar <- 0.5*(1-chrm.sq)
      #print(chrm_row$rbar[o])
      #rbar[o] <- 0.5*(1-chrm.sq)
      #o = o +1
    }
    if(grepl(3, chrm_row$hand.foci.count)){
      portion1 <- chrm_row$Foci1
      portion2  <- chrm_row$Foci2 - chrm_row$Foci1
      portion3 <- chrm_row$Foci3 - chrm_row$Foci2
      portion4 <- chrm_row$chromosomeLength - chrm_row$Foci3
      portion.sum <- portion1+portion2+portion3+portion4
      rel.portion1 <- (portion1 / portion.sum)
      rel.portion2 <- (portion2 / portion.sum)
      rel.portion3 <- (portion3 / portion.sum)
      rel.portion4 <- (portion4 / portion.sum)
      
      rel.por.sqrd1 <- rel.portion1^2
      rel.por.sqrd2 <- rel.portion2^2
      rel.por.sqrd3 <- rel.portion3^2
      rel.por.sqrd4 <- rel.portion4^2
      
      chrm.sq <- sum(rel.por.sqrd1,rel.por.sqrd2,rel.por.sqrd3,rel.por.sqrd4)
      rrbar <- 0.5*(1-chrm.sq)
      #tot.rec <-  0.5*(1-sum(rel.por.sqrd1, rel.por.sqrd2, rel.por.sqrd3,rel.por.sqrd4 )) 
      #tot.rec =((rel.portion1 + rel.portion2 + rel.portion3 + rel.portion4) )^2
    }
    o = o +1
  }
  return(rrbar)
}


multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
