# R script for making plot for HetC file
#input: saved workspace with dataframe, strain and mouse summary tables.
#output: 2 figures displaying points of heterochiasmy. One with raw values, other with male adjusted

library(plyr)
library(gplots)
library(reshape2)
library(devEMF)
#############
# load data #
#############
setwd("C:/Users/alpeterson7/Documents/MLH1repo")
load(file="MLH1_data_setup.RData")

#create file to save image first
png("adj_HetC.png")

#emf(file="example.emf", bg="white", width=10, height=10, family="Calibri", pointsize=20)
#emf is not vectorized in emf file in powrpnt

#remake the table -- specific strains

AP_strain_table <- ddply(MLH1_data, c("strain", "sex"), summarise,
                         Nmice = length(unique(mouse)),
                         Ncells  = length(adj_nMLH1.foci),
                         mean_co = format(round(  mean(adj_nMLH1.foci), 3 ), nsmall=3),
                         var = format(round(   var(nMLH1.foci),3), nsmall=3),
                         sd   = round(sd(adj_nMLH1.foci), 3),
                         se   = round(sd / sqrt(Ncells), 3),
                         cV = round( (as.numeric(sd) / as.numeric(mean_co) ),3)
)

#remove single cast female, and HMI male
AP_strain_table <- AP_strain_table[-c(13,15),] #there is a line below this which accounts for this

#add Lynn Cast female data
Lynn_CASTf_foci = c(20,21, 23, 25, 26, 26,26,27.5, 28, 28,28,33)
cast_f_row = c("CAST", "female", 1, length(Lynn_CASTf_foci), round(mean(Lynn_CASTf_foci),3), 
           round(var(Lynn_CASTf_foci),3), round(sd(Lynn_CASTf_foci),3), 
           round(sd(Lynn_CASTf_foci)/sqrt(length(Lynn_CASTf_foci)),3 ), 
         round(sd(Lynn_CASTf_foci) / (mean(Lynn_CASTf_foci) ), 3)  )

AP_strain_table <- rbind(AP_strain_table, cast_f_row)

AP_strain_table$Nmice <- as.numeric(AP_strain_table$Nmice)
AP_strain_table$Nells <- as.numeric(AP_strain_table$Ncells)
AP_strain_table$mean_co <-  as.numeric(AP_strain_table$mean_co)
AP_strain_table$var <- as.numeric( AP_strain_table$var)
AP_strain_table$sd <- as.numeric( AP_strain_table$sd)
AP_strain_table$se <- as.numeric(  AP_strain_table$se)
AP_strain_table$cV <- as.numeric(   AP_strain_table$cV)

#make casted rows  --- why am I making cast rows?
casted_co <- dcast(data = AP_strain_table, formula= strain~sex, value.var="mean_co")
casted_var <- dcast(data = AP_strain_table, formula= strain~sex, value.var="var")
casted_sd <- dcast(data = AP_strain_table, formula= strain~sex, value.var="sd")
casted_se <- dcast(data = AP_strain_table, formula= strain~sex, value.var="se")
casted_cV <- dcast(data = AP_strain_table, formula= strain~sex, value.var="cV")

#strain variance table
HetC_table <- cbind(casted_co, casted_var[,2:3], casted_sd[,2:3], casted_se[,2:3], casted_cV[,2:3])

#rename the colnames
colnames(HetC_table) <- c("strain","f_CO_mean","m_CO_mean","f_CO_var","m_CO_var","f_CO_sd","m_CO_sd",
                  "f_CO_se","m_CO_se","f_CO_cV","m_CO_cV" )

#remove NA's (those without females)
HetC_table <- na.omit(HetC_table)

######################
# MAKE ADJUSTED PLOT #
######################

par(mfrow=c(1,1))
par(cex = 1)
#points ploted with means, F-y, M-x
plot(x =as.numeric(HetC_table[,3]), y = as.numeric(HetC_table[,2]), main = "",
     xlim=c(20,35), ylim=c(20,35),
     cex=.1,cex.lab=1.5,
      xlab ="Male MLH1 Foci", ylab = "Female MLH1 Foci", axes = F, type = "n", font.axis = 6)

axis(side = 1, at = c(0,21,23,25,27,29,31,33,35), labels = c("","21","23","25","27","29","31","33",""), lwd = 4, cex.axis=1.2)
axis(side = 2, at = c(0,21,23,25,27,29,31,33,35), labels = c("","21","23","25","27","29","31","33",""), lwd = 4, cex.axis=1.2)

#line through origin
x = c(1,5,10,15,20)
y = x
m = lm(x~y)
abline(m, lwd=1) 

#change labels
plotCI(x = as.numeric(HetC_table[,3]),   #male mean_co
       y = as.numeric(HetC_table[,2]),  #female mean_co
       uiw = as.numeric(HetC_table[,8])*2, #female se
       
#change the colors       
       col = c("#56B4E9","lightblue4", "cadetblue", #blues, W, Lew, G,
               "coral1", "#E69F00", "red", "yellowgreen", "black", "purple"),        #reds   "indianred",
       #   "seagreen3"           #green
       pch = 16, lwd = 3.5, gap = 0, sfrac = 0.01, add = TRUE, cex=1.4)

#WSB
#, row 1.  (coordinates, male mean x, female mean y  - female xse)
segments(  (as.numeric(HetC_table[1,3]) - as.numeric(HetC_table[1,9])*2 ), as.numeric(HetC_table[1,2]), 
           ( as.numeric(HetC_table[1,3]) + as.numeric(HetC_table[1,9])*2 ), as.numeric(HetC_table[1,2]),
           lwd=3.5, col="#56B4E9") #

#lew
segments(  (as.numeric(HetC_table[3,3]) - as.numeric(HetC_table[3,9])*2 ), as.numeric(HetC_table[3,2]), 
           ( as.numeric(HetC_table[3,3]) + as.numeric(HetC_table[3,9])*2 ), as.numeric(HetC_table[3,2]),
           lwd=3.5, col="cadetblue") 
#G
segments(  (as.numeric(HetC_table[2,3]) - as.numeric(HetC_table[2,9])*2 ), as.numeric(HetC_table[2,2]), 
           ( as.numeric(HetC_table[2,3]) + as.numeric(HetC_table[2,9])*2 ), as.numeric(HetC_table[2,2]),
           lwd=3.5, col="cadetblue4")

#pwd
segments(  (as.numeric(HetC_table[4,3]) - as.numeric(HetC_table[4,9])*2 ), as.numeric(HetC_table[4,2]), 
           ( as.numeric(HetC_table[4,3]) + as.numeric(HetC_table[4,9])*2 ), as.numeric(HetC_table[4,2]),
           lwd=3.5, col="coral1")
#msm
segments(  (as.numeric(HetC_table[5,3]) - as.numeric(HetC_table[5,9])*2 ), as.numeric(HetC_table[5,2]), 
           ( as.numeric(HetC_table[5,3]) + as.numeric(HetC_table[5,9])*2 ), as.numeric(HetC_table[5,2]),
           lwd=3.5, col="#E69F00")
#KAZ
segments(  (as.numeric(HetC_table[6,3]) - as.numeric(HetC_table[6,9])*2 ), as.numeric(HetC_table[6,2]), 
           ( as.numeric(HetC_table[6,3]) + as.numeric(HetC_table[6,9])*2 ), as.numeric(HetC_table[6,2]),
           lwd=3.5, col="#red")

#cast
segments(  (as.numeric(HetC_table[7,3]) - as.numeric(HetC_table[7,9])*2 ), as.numeric(HetC_table[7,2]), 
           ( as.numeric(HetC_table[7,3]) + as.numeric(HetC_table[7,9])*2 ), as.numeric(HetC_table[7,2]),
           lwd=3.5, col="yellowgreen")
#Spret
segments(  (as.numeric(HetC_table[8,3]) - as.numeric(HetC_table[8,9])*2 ), as.numeric(HetC_table[8,2]), 
           ( as.numeric(HetC_table[8,3]) + as.numeric(HetC_table[8,9])*2 ), as.numeric(HetC_table[8,2]),
           lwd=3.5, col="black")

#spic
segments(  (as.numeric(HetC_table[9,3]) - as.numeric(HetC_table[9,9])*2 ), as.numeric(HetC_table[9,2]), 
           ( as.numeric(HetC_table[9,3]) + as.numeric(HetC_table[9,9])*2 ), as.numeric(HetC_table[9,2]),
           lwd=3.5, col="purple")

dev.off()

#setwd("C:/Users/alpeterson7/Documents/MLH1repo/")
#dev.copy(png,'adj_HetC.png')
#library(devEMF)
#emf(file="example.emf", bg="white", width=12, height=8, family="Calibri", pointsize=20)
#dev.off()
#dev.off()#emf
#dev.copy(png,'Biv_scatter_plot_female.png')
#dev.off()
