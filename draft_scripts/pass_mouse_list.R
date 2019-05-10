# list of passing mice
# input: current BivData.RData
# output: csv (or other formate) list of passing mice
# print out for hard copy and to be referenced by other scripts

library(plyr)
# initial number of cells 25.. (kinda abritrry)
# (high propotion of cells with quality less than 5) q.lt5 > .7

setwd("C:/Users/alpeterson7/Documents/MLH1repo")
#setwd("Documents/MLH1repo")

load(file="MLH1_data_setup.RData")


q_cutoff_table <- ddply(MLH1_data, .(mouse), summarise,
                        total =  length(adj_nMLH1.foci),
                        q5 = sum(as.numeric(quality) >= 4, na.rm = TRUE ), 
                        above4 = sum(as.numeric(quality) <= 3, na.rm = TRUE ),
                        #add percentages
                        prop.lt5 = above4/total
)
length(q_cutoff_table$mouse)#116

pass_mus_table <- q_cutoff_table[q_cutoff_table$above4 >= 25,] #30

pass_mus_table.2 <- pass_mus_table[pass_mus_table$prop.lt5 >= .6,]#29

OnEdge <-  q_cutoff_table[ ( q_cutoff_table$prop.lt5 >= .6) &
                               (q_cutoff_table$above4 <= 23 ),]

## write out list of mice


