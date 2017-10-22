

library(knitr)
library(ggplot2)
library(pwr)
library(plyr)
library(lattice)
library(dplyr)

setwd("C:/Users/alpeterson7/Documents/MLH1repo")
load(file="MLH1_data_setup.RData")


q_cutoff_table <- ddply(MLH1_data, .(mouse), summarise,
                        total =  length(adj_nMLH1.foci),
                        q5 = sum(as.numeric(quality) >= 4, na.rm = TRUE ), 
                        above5 = sum(as.numeric(quality) <= 4, na.rm = TRUE )
                        # q_l3 = sum(as.numeric(as.numeric(quality)) <= 4, na.rm = TRUE )
)
#q_cutoff_table
passed_mice <- as.vector(q_cutoff_table[q_cutoff_table$above5 > 15,]$mouse)

not_passed <- as.vector(unique(MLH1_data[!(unique(MLH1_data$mouse) %in% passed_mice),]$mouse) )

#mnake dataframe with just passing mice -- so that it can be ploted
passed_mice_df <- MLH1_data[ (as.numeric(MLH1_data$quality) < 5), ]


#make a dataframe of many p values


