#
# input: MLH1.RData
# output: png file of strain means by sex


library(car)
library(ggplot2)
library(gridExtra)
library(pwr)
library(grid)
library(gridBase)
library(asbio)
library(plyr)

setwd("C:/Users/alpeterson7/Documents/MLH1repo")
load(file="MLH1_data_setup.RData")

#create file to save image first
png("adj_MLH1_means.png")

Gough_f = MLH1_data[MLH1_data$category == "Gough female",]
Gough_m= MLH1_data[MLH1_data$category == "Gough male",]
WSB_f= MLH1_data[MLH1_data$category == "WSB female",]
WSB_m= MLH1_data[MLH1_data$category == "WSB male",]
PWD_f= MLH1_data[MLH1_data$category == "PWD female",]
PWD_m= MLH1_data[MLH1_data$category == "PWD male",]

##make a ddplry table instead with mean and se
AP_strain_table <- ddply(MLH1_data, c("strain", "sex"), summarise,
                         Nmice = length(unique(mouse)),
                         Ncells  = length(adj_nMLH1.foci),
                         mean_co = format(round(  mean(adj_nMLH1.foci), 3 ), nsmall=3),
                         var = format(round(   var(nMLH1.foci),3), nsmall=3),
                         sd   = round(sd(adj_nMLH1.foci), 3),
                         se   = round(sd / sqrt(Ncells), 3),
                         cV = round( (as.numeric(sd) / as.numeric(mean_co) ),3)
)

#add CAST female
Lynn_CASTf_foci = c(20,21, 23, 25, 26, 26,26,27.5, 28, 28,28,33)
cast_f_row = c("CAST", "female", 1, length(Lynn_CASTf_foci), round(mean(Lynn_CASTf_foci),3), 
               round(var(Lynn_CASTf_foci),3), round(sd(Lynn_CASTf_foci),3), 
               round(sd(Lynn_CASTf_foci)/sqrt(length(Lynn_CASTf_foci)),3 ), 
               round(sd(Lynn_CASTf_foci) / (mean(Lynn_CASTf_foci) ), 3)  )

AP_strain_table <- rbind(AP_strain_table, cast_f_row)

WSBm.MLH1.mean = (mean(MLH1_data[MLH1_data$category == 'WSB male', ]$adj_nMLH1.foci))
WSBm.MLH1.se = sd(MLH1_data[MLH1_data$category == 'WSB male', ]$adj_nMLH1.foci) / (sqrt(length(MLH1_data[MLH1_data$category == 'WSB male', ])))

#WSB female
WSBf.MLH1.mean = mean(MLH1_data[MLH1_data$category == 'WSB female', ]$adj_nMLH1.foci)
WSBf.MLH1.se = sd(MLH1_data[MLH1_data$category == 'WSB female', ]$adj_nMLH1.foci) / (sqrt(length(MLH1_data[MLH1_data$category == 'WSB female', ])))

#gough male
Gm.MLH1.mean = (mean(MLH1_data[MLH1_data$category == 'Gough male', ]$adj_nMLH1.foci))
Gm.MLH1.se = sd(MLH1_data[MLH1_data$category == 'Gough male', ]$adj_nMLH1.foci) / (sqrt(length(MLH1_data[MLH1_data$category == 'Gough male', ])))

#gough female
Gf.MLH1.mean = mean(MLH1_data[MLH1_data$category == 'Gough female', ]$adj_nMLH1.foci)
Gf.MLH1.se = sd(MLH1_data[MLH1_data$category == 'Gough female', ]$adj_nMLH1.foci) / (sqrt(length(MLH1_data[MLH1_data$category == 'Gough female', ])))

#PWD male
PWDm.MLH1.mean = (mean(MLH1_data[MLH1_data$category == 'PWD male', ]$adj_nMLH1.foci))
PWDm.MLH1.se = sd(MLH1_data[MLH1_data$category == 'PWD male', ]$adj_nMLH1.foci) / (sqrt(length(MLH1_data[MLH1_data$category == 'PWD male', ])))

#PWD female
PWDf.MLH1.mean = mean(MLH1_data[MLH1_data$category == 'PWD female', ]$adj_nMLH1.foci)
PWDf.MLH1.se = sd(MLH1_data[MLH1_data$category == 'PWD female', ]$adj_nMLH1.foci) / (sqrt(length(MLH1_data[MLH1_data$category == 'PWD female', ])))

dev.off()
