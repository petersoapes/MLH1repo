# tables for MLH1 data
# input:
# output: dplyr tables (strain/category) and mouse
#

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


dataset <- rep("BD", length(Table_BD_strain$Cross) )

#everything should be male unless, 'female' listed in name
sex <- ifelse(grepl("Female", Table_BD_strain$Cross), "female", "male") 

Table_BD_strain <- cbind(Table_BD_strain, sex, dataset)# subsp,
#change name
Table_BD_strain$Cross[Table_BD_strain$Cross== "PWDFemale"] <- "PWD"

######################
# Setup AP tables #
######################

################
# STRAIN TABLE #
################

#calculate strain averages from mouse averages
#(try making mean table by batch()

AP_strain_table <- ddply(MLH1_data, c("strain", "sex"), summarise,
                         Nmice = length(unique(mouse)),
                         Ncells  = length(nMLH1.foci),
                         mean_co = format(round(  mean(nMLH1.foci), 3 ), nsmall=3),
                         var = format(round(   var(nMLH1.foci),3), nsmall=3),
                         sd   = round(sd(nMLH1.foci), 3),
                         se   = round(sd / sqrt(Ncells), 3)
)#currently these are too low

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
# MERGE (tables) data sets # LOAD OTHER DATA
###################
#same number of cols, but in wrong order and named wrong
#change name of Cross
colnames(Table_BD_strain)[1] <- "strain"
#reorder
Table_BD_strain <- Table_BD_strain[c("strain", "sex","Nmice", "Ncells", "mean_co","var", "sd","se", "dataset" )]

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
#MLH1_data_table$subsp <- factor(MLH1_data_table$subsp,
#                        levels =c( "Dom", "Musc", "Musc-Cast", "Cast",
#                                  "Spic", "Caroli","Outgroup", "other" ), order=T )
MLH1_data_table <- with(MLH1_data_table, MLH1_data_table[order(strain, sex),])
