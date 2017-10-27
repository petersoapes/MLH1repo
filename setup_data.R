##R script for making R.data files 
## read in data files and adjust with additional columns
## input: csv file of anon MLH1 counts
## output: .rdata saved workspace with dataframe and other stat tables (save tables)
#import MLH1 measures across data sets; AP, BD's and Cast female Koehler
library(plyr)

########################
# Setup main dataframe #
########################

#TODO: remove observations without quality scores.
#Write a make file that will merge all of the batch files and record the batch
setwd("C:/Users/alpeterson7/Documents/MLH1repo/")
MLH1_data = read.csv("data/AnonData.csv", header=TRUE)

MLH1_data$category <- ifelse(grepl("_WSB_m", MLH1_data$Original.Name), "WSB male", 
                  ifelse(grepl("_WSB_f", MLH1_data$Original.Name), "WSB female",
                  ifelse(grepl("_G_f", MLH1_data$Original.Name), "G female",
                  ifelse(grepl("_G_m", MLH1_data$Original.Name), "G male",
                  ifelse(grepl("_CAST_m", MLH1_data$Original.Name), "CAST male",
                  ifelse(grepl("_CAST_f", MLH1_data$Original.Name), "CAST female",
                  ifelse(grepl("_MSM_m", MLH1_data$Original.Name), "MSM male",
                  ifelse(grepl("_MSM_f", MLH1_data$Original.Name), "MSM female",
                  ifelse(grepl("_LEW_f", MLH1_data$Original.Name), "LEWES female", 
                  ifelse(grepl("_LEW_m", MLH1_data$Original.Name), "LEWES male",
                  ifelse(grepl("_LEWES_m", MLH1_data$Original.Name), "LEWES male",                                       
                 ifelse(grepl("_PWD_m", MLH1_data$Original.Name), "PWD male",     
                  ifelse(grepl("_PWD_f", MLH1_data$Original.Name), "PWD female", "other")))))))))))))

#set the order of categories (female, male) (cast, dom, musc)
MLH1_data$category<- factor(MLH1_data$category,levels =c( "G female", "G male", 
                      "WSB female", "WSB male", "LEWES female", 'LEWES male', 
                      "PWD female", "PWD male", "MSM female", "MSM male",
                      "CAST female", "CAST male"), order=T )

MLH1_data$strain <- ifelse(grepl("_WSB", MLH1_data$Original.Name), "WSB", 
                    ifelse(grepl("_G", MLH1_data$Original.Name), "G",
                   ifelse(grepl("CAST", MLH1_data$Original.Name), "CAST",
                  ifelse(grepl("MSM", MLH1_data$Original.Name), "MSM",
                   ifelse(grepl("LEW", MLH1_data$Original.Name), "LEWES",                                         
                   ifelse(grepl("PWD", MLH1_data$Original.Name), "PWD", "other"))))))

MLH1_data$sex <- ifelse(grepl("_m", MLH1_data$Original.Name), "male", 
                      ifelse(grepl("_f", MLH1_data$Original.Name), "female", "other"
                      )) 
original_length = length(MLH1_data$Original.Name)
MLH1_data <- MLH1_data[MLH1_data$X != "X",]

change <- MLH1_data[MLH1_data$quality == "",]

MLH1_data$category <- as.factor(MLH1_data$category)

MLH1_data$nMLH1.foci <- as.numeric(MLH1_data$nMLH1.foci) #make these numeric just in case there are other characters

MLH1_data$adj_nMLH1.foci <- as.numeric(MLH1_data$adj_nMLH1.foci)

#add a column with male adjusted MLH1 values (+1 to all males)
MLH1_data$adj_nMLH1.foci <- ifelse(MLH1_data$sex=="male", MLH1_data$nMLH1.foci+1, MLH1_data$nMLH1.foci)

count =1
for(i in MLH1_data$Original.Name){
  #print(i)
  templist= strsplit(i, split="_")[[1]]
  c = paste(templist[2], templist[3],templist[4], sep = "_")
  MLH1_data$mouse[count] <- c
  count= count +1
}

#remove mice that had bad stains
#12sep16_MSM_f3(centromere signal bled into MLH1 signal)
MLH1_data <- MLH1_data[ !grepl("12sep16_MSM_f3", MLH1_data$mouse) , ]
#make a list of bad mice

##order data by subspeces (dom, musc, musc-cast(mol), cast, spret, spic...ect)! ordering is the worst.
#example. (variable needs to be a ordered factor)

#MLH1_by_M_mouse <- with(MLH1_by_M_mouse, MLH1_by_M_mouse[order(subsp),])
#load Lynn et al 2002 data. 12 CAST female MLH1 measurements (probably 1 female)
#Lynn_CASTf = c(20,21, 23, 25, 26, 26,26,27.5, 28, 28,28,33)


#load BD's data. Only PWD female and F1 females missing
fullBD_MLH1_data = read.csv("C:/Users/alpeterson7/Documents/MLH1data/data/BD_MLH1data/BD_RecombinationPhenotypes_input.csv")
#unique(fullBD_MLH1_data$Cross)
#subset P0s
BDMLH1_data <- subset(fullBD_MLH1_data, (Cross %in%  c("PANCEVO","RAT","CIM", "PWDFemale", "PWD","Peromyscus",
                      "CZECHI","PERA", "CAROLI", "CAST", "Microtus", "WSB") ))

#now that I have the mice of Beth's I want, remove the big BD df with F2s to save space.
rm(fullBD_MLH1_data)

Table_BD_mouse <- ddply(BDMLH1_data, c("ANIMAL_ID", "Cross"), summarise,
                       N  = length(nMLH1_foci),
                       mean_co = format(round( mean(nMLH1_foci), 3), nsmall = 3),
                       var = format(round( var(nMLH1_foci),3), nsmall = 3),
                       sd   = round(sd(nMLH1_foci),3),
                       se   = round(sd / sqrt(N),3)
                       #text=paste(Cross, collapse=""))
)

#strain averages
Table_BD_strain <- ddply(BDMLH1_data, c("Cross"), summarise,
                         Nmice = length(unique(ANIMAL_ID)),
                         Ncells  = length(nMLH1_foci),
                         mean_co = format(round (mean(nMLH1_foci), 3), nsmall=3),
                         var = format(round( var(nMLH1_foci), 3), nsmall=3),
                         sd   = round( sd(nMLH1_foci), 3),
                         se   = round(sd / sqrt(Ncells), 3)
                         #text=paste(Cross, collapse=""))
)#format(round(   var(nMLH1.foci),3), nsmall=3),

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

dataset <- rep("BD", length(Table_BD_strain$Cross) )

#everything should be male unless, 'female' listed in name
sex <- ifelse(grepl("Female", Table_BD_strain$Cross), "female", "male") 

Table_BD_strain <- cbind(Table_BD_strain, sex, subsp, dataset)
#change name
Table_BD_strain$Cross[Table_BD_strain$Cross== "PWDFemale"] <- "PWD"

######################
# Setup AP tables #
######################
################
# STRAIN TABLE #
################

#calculate strain averages from mouse averages
AP_strain_table <- ddply(MLH1_data, c("strain", "sex"), summarise,
                  Nmice = length(unique(mouse)),
                  Ncells  = length(nMLH1.foci),
                  mean_co = format(round(  mean(nMLH1.foci), 3 ), nsmall=3),
                  var = format(round(   var(nMLH1.foci),3), nsmall=3),
                  sd   = round(sd(nMLH1.foci), 3),
                  se   = round(sd / sqrt(Ncells), 3)
)

subsp <- ifelse(grepl("WSB", AP_strain_table$strain), "Dom", 
        ifelse(grepl("G", AP_strain_table$strain), "Dom",
         ifelse(grepl("LEW", AP_strain_table$strain), "Dom", 
        ifelse(grepl("LEWES", AP_strain_table$strain), "Dom",    
                                     
       ifelse(grepl("CAST", AP_strain_table$strain), "Cast",
      ifelse(grepl("MSM", AP_strain_table$strain), "Musc-Cast",                                       
       ifelse(grepl("PWD", AP_strain_table$strain), "Musc", "other")))))))

dataset <- rep("AP", length(AP_strain_table$strain) )

AP_strain_table <- cbind(AP_strain_table, subsp, dataset)
###################
# MERGE data sets # LOAD OTHER DATA
###################
#same number of cols, but in wrong order and named wrong
#change name of Cross
colnames(Table_BD_strain)[1] <- "strain"
#reorder
Table_BD_strain <- Table_BD_strain[c("strain", "sex","Nmice", "Ncells", "mean_co","var", "sd","se", "subsp", "dataset" )]

#add lynn data
Lynn_CASTf_foci = c(20,21, 23, 25, 26, 26,26,27.5, 28, 28,28,33)
cast_f = c("CAST", "female", 1, length(Lynn_CASTf_foci), round(mean(Lynn_CASTf_foci),3), round(var(Lynn_CASTf_foci),3), round(sd(Lynn_CASTf_foci),3), 
           round(sd(Lynn_CASTf_foci)/sqrt(length(Lynn_CASTf_foci)),3 ), "Cast", as.character("Ln") )

MLH1_data_table <- rbind(AP_strain_table, Table_BD_strain, cast_f)#error from dataset thing

#set the order
MLH1_data_table$strain <- factor(MLH1_data_table$strain,
                         levels =c("G", "LEWES","WSB","PERA",
                                  "PWD","CZECHI", "MSM", "CAST", "CIM",
                                "PANCEVO", "CAROLI", "RAT","Peromyscus","Microtus"), order=T )
MLH1_data_table$subsp <- factor(MLH1_data_table$subsp,
                        levels =c( "Dom", "Musc", "Musc-Cast", "Cast",
                                   "Spic", "Caroli","Outgroup", "other" ), order=T )
MLH1_data_table <- with(MLH1_data_table, MLH1_data_table[order(subsp, strain, sex),])

############
# SAVE DFs #
############
save.image("MLH1_data_setup.RData")
#OutPut: big large MLH1_data (AP's) df, big DF of BD with just the mice I want.
# MLH1_data_table, means and variance of AP and BD MLH1 values. made from seperate tables from AP and BD data.
#  make sure decimal places are consistant
