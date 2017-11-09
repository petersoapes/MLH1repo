# R script for making plot for HetC file
#input: saved workspace with dataframe, strain and mouse summary tables.
#output: 2 figures displaying points of heterochiasmy. One with raw values, other with male adjusted

library(plyr)
library(gplots)
library(ggplot2)

#############
# load data #
#############
setwd("C:/Users/alpeterson7/Documents/MLH1data/Results/figures/")

load(file="MLH1_data_setup.RData")
#file for saving #png("HetC.png")

#remake the table -- specific strains

g_row <- c(subset(MLH1_data, strain == "G" & sex == "male" & dataset =="AP")$mean_co,
           subset(MLH1_data, strain == "G" & sex == "female" & dataset =="AP")$mean_co,
           subset(MLH1_data, strain == "G" & sex == "male" & dataset =="AP")$se,
           subset(MLH1_data, strain == "G" & sex == "female" & dataset =="AP")$se)

lew_row <- c(subset(MLH1_data, strain == "LEWES" & sex == "male" & dataset =="AP")$mean_co,
             subset(MLH1_data, strain == "LEWES" & sex == "female" & dataset =="AP")$mean_co,
             subset(MLH1_data, strain == "LEWES" & sex == "male" & dataset =="AP")$se,
             subset(MLH1_data, strain == "LEWES" & sex == "female" & dataset =="AP")$se)

wsb_row <- c(subset(MLH1_data, strain == "WSB" & sex == "male" & dataset =="AP")$mean_co,
             subset(MLH1_data, strain == "WSB" & sex == "female" & dataset =="AP")$mean_co,
             subset(MLH1_data, strain == "WSB" & sex == "male" & dataset =="AP")$se,
             subset(MLH1_data, strain == "WSB" & sex == "female" & dataset =="AP")$se)  

pwd_row <- c(subset(MLH1_data, strain == "PWD" & sex == "male" & dataset =="AP")$mean_co,
             subset(MLH1_data, strain == "PWD" & sex == "female" & dataset =="AP")$mean_co,
             subset(MLH1_data, strain == "PWD" & sex == "male" & dataset =="AP")$se,
             subset(MLH1_data, strain == "PWD" & sex == "female" & dataset =="AP")$se)  

msm_row <-c(subset(MLH1_data, strain == "MSM" & sex == "male" & dataset =="AP")$mean_co,
            subset(MLH1_data, strain == "MSM" & sex == "female" & dataset =="AP")$mean_co,
            subset(MLH1_data, strain == "MSM" & sex == "male" & dataset =="AP")$se,
            subset(MLH1_data, strain == "MSM" & sex == "female" & dataset =="AP")$se)  

cast_row <-c(subset(MLH1_data, strain == "CAST" & sex == "male" & dataset =="AP")$mean_co,
             subset(MLH1_data, strain == "CAST" & sex == "female")$mean_co,
            subset(MLH1_data, strain == "CAST" & sex == "male" & dataset =="AP")$se,
            subset(MLH1_data, strain == "CAST" & sex == "female")$se)

vals4plot <- rbind(g_row, lew_row, wsb_row, pwd_row, msm_row, cast_row)
#this turns everything to characters
colnames(vals4plot) <- c("male_CO", "female_CO", "male_se", "female_se")
vals4plot_adj <- vals4plot

#make an adjusted table, (just add 1 to the male means
vals4plot_adj[,1] <- as.numeric(vals4plot_adj[,1]) +1

######################
# MAKE ADJUSTED PLOT #
######################

par(mfrow=c(1,1))
par(cex = 1)
plot(x =  vals4plot_adj[,1], y = vals4plot_adj[,2], main = "",
     xlim=c(20,35), ylim=c(20,35),
     xlab ="Male MLH1 Foci", ylab = "Female MLH1 Foci", axes = F, type = "n", font.axis = 6)
axis(side = 1, at = c(0,21,23,25,27,29,31,33,35), labels = c("","21","23","25","27","29","31","33",""), lwd = 2)
axis(side = 2, at = c(0,21,23,25,27,29,31,33,35), labels = c("","21","23","25","27","29","31","33",""), lwd = 2)

#line through origin
x = c(1,5,10,15,20)
y = x
m = lm(x~y)
abline(m, lwd=2) 

#change labels
plotCI(x = as.numeric(vals4plot_adj[,1]),   #male mean_co
       y = as.numeric(vals4plot_adj[,2]),  #female mean_co
       uiw = as.numeric(vals4plot_adj[,4])*2, #female se
       
       col = c("#56B4E9","cadetblue4", "cadetblue", #blues, 
               "coral1", "#E69F00", "yellowgreen"),        #reds   "indianred",
               #   "seagreen3"           #green
       pch = 16, lwd = 2, gap = 0, sfrac = 0.01, add = TRUE, cex=1.4)


#g, row 1.  (coordinates, male mean x, female mean y  - female xse)
segments(  (as.numeric(vals4plot_adj[1,1]) - as.numeric(vals4plot_adj[1,3])*2 ), as.numeric(vals4plot_adj[1,2]), 
        ( as.numeric(vals4plot_adj[1,1]) + as.numeric(vals4plot_adj[1,3])*2 ), as.numeric(vals4plot_adj[1,2]),
           lwd=2, col="#56B4E9") 
#lew
segments(  (as.numeric(vals4plot_adj[2,1]) - as.numeric(vals4plot_adj[2,3])*2 ), as.numeric(vals4plot_adj[2,2]), 
           ( as.numeric(vals4plot_adj[2,1]) + as.numeric(vals4plot_adj[2,3])*2 ), as.numeric(vals4plot_adj[2,2]),
           lwd=2, col="cadetblue4") 
#wsb
segments(  (as.numeric(vals4plot_adj[3,1]) - as.numeric(vals4plot_adj[3,3])*2 ), as.numeric(vals4plot_adj[3,2]), 
           ( as.numeric(vals4plot_adj[3,1]) + as.numeric(vals4plot_adj[3,3])*2 ), as.numeric(vals4plot_adj[3,2]),
           lwd=2, col="cadetblue") 
#pwd
segments(  (as.numeric(vals4plot_adj[4,1]) - as.numeric(vals4plot_adj[4,3])*2 ), as.numeric(vals4plot_adj[4,2]), 
           ( as.numeric(vals4plot_adj[4,1]) + as.numeric(vals4plot_adj[4,3])*2 ), as.numeric(vals4plot_adj[4,2]),
           lwd=2, col="coral1")
#msm
segments(  (as.numeric(vals4plot_adj[5,1]) - as.numeric(vals4plot_adj[5,3])*2 ), as.numeric(vals4plot_adj[5,2]), 
           ( as.numeric(vals4plot_adj[5,1]) + as.numeric(vals4plot_adj[5,3])*2 ), as.numeric(vals4plot_adj[5,2]),
           lwd=2, col="#E69F00")
#cast
segments(  (as.numeric(vals4plot_adj[6,1]) - as.numeric(vals4plot_adj[6,3])*2 ), as.numeric(vals4plot_adj[6,2]), 
           ( as.numeric(vals4plot_adj[6,1]) + as.numeric(vals4plot_adj[6,3])*2 ), as.numeric(vals4plot_adj[6,2]),
           lwd=2, col="yellowgreen")

dev.copy(png,'HetC_adj.png')
dev.off()

dev.copy(win.metafile, "HetC_adj.wmf")
dev.off()


#####
#mus_tble <- ddply(MLH1_by_mouse, c("strain", "sex"), summarise,
  #                N  = length(mean_co),
 #                 meanCO = mean(mean_co),#means of mouse means
   #               #av_var = mean(var),
  #                sd   = sd(mean_co),  #sd of the means across mice (not average sd or var..that is dumb)
  #                se   = sd / sqrt(N)
#)

#mus_tble <- MLH1_by_strain