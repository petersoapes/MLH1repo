#R script for making a minigithub repo for displaying MLH1 results
# input: 
# output: table of stats, show the distributions of mice,
# show what mice have passed/ the number of mice for each category

library(plyr)
library(gplots)
library(ggplot2)
#read in working environment that was already set
setwd("C:/Users/alpeterson7/Documents/MLH1repo")
load(file="MLH1_data_setup.RData")


#save a figure of boxplots in the current repo
ggplot(MLH1_data, aes(factor(mouse), nMLH1.foci)) + geom_boxplot(data = MLH1_data, aes(fill = factor(strain)))
ggsave("filename_boxplots.png")




#Quality / number of passing mice
q_cutoff_table <- ddply(MLH1_data, .(mouse), summarise,
                        total =  length(nMLH1.foci),
                        q5 = sum(as.numeric(quality) >= 4, na.rm = TRUE ), 
                        q_l3 = sum(as.numeric(as.numeric(quality)) <= 4, na.rm = TRUE )
)
q_cutoff_table

passed_mice <- as.data.frame(q_cutoff_table[q_cutoff_table$q_l3 >= 15,]$mouse, 
                             col.names= "mouse")
colnames(passed_mice) <- c("mouse")

passed_mice$strain <- ifelse(grepl("WSB", passed_mice$mouse), "WSB", 
                   ifelse(grepl("G", passed_mice$mouse), "G",
                    ifelse(grepl("CAST", passed_mice$mouse), "CAST",
                   ifelse(grepl("MSM", passed_mice$mouse), "MSM",
                  ifelse(grepl("LEW", passed_mice$mouse), "LEWES", 
                   ifelse(grepl("LEWES", passed_mice$mouse), "LEWES",                                          
                  ifelse(grepl("PWD", passed_mice$mouse), "PWD", "other")))))))

passed_mice$sex <- ifelse(grepl("_f", passed_mice$mouse), "female",                                         
                          ifelse(grepl("_m", passed_mice$mouse), "male", "other"))

#make a table of the number of mice passed for each category.
pass_table <- ddply(passed_mice, .(strain, sex), summarise,
                    passing.mice =  length(mouse)
)
#add non passed mice


setwd("C:/Users/alpeterson7/Documents/MLH1data/Results/")
#write.csv(passed_mice, file = "MLH1_data", fileEncoding = "UTF-16LE")
write.table(pass_table, "pass_table.txt", sep="\t", quote=FALSE, row.name=FALSE)
#this table would need to be put together via pandoc