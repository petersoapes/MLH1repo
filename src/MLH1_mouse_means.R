#R script for making plots of the distributions of MLH1 foci numbers
#input: MLH1 data/ environment, mouse specific table
#plots: mouse specific, strain averages

#output: 2 sex-specific figures, 1 merged figure
#<don't adjust male data, think about including BD's data>

#..maybe there should be a R code for saving processing the data
#but for now, just construct the make file when dataframe is pasted into the dir
library(plyr)
library(gplots)
library(ggplot2)

#############
# LOAD DATA #
#############
#read in working environment that was already set
setwd("C:/Users/alpeterson7/Documents/MLH1repo")
load(file="MLH1_data_setup.RData")

###################
# DATA TREATMENTS #
###################

#make sure the levels are in the correct order for the graph plotting
MLH1_data <- with(MLH1_data, MLH1_data[order(subsp, strain),])

AP_strain_table <- ddply(MLH1_data, c("strain", "sex"), summarise,
                         Nmice = length(unique(mouse)),
                         Ncells  = length(adj_nMLH1.foci),
                         mean_co = format(round(  mean(adj_nMLH1.foci), 3 ), nsmall=3),
                         var = format(round(   var(nMLH1.foci),3), nsmall=3),
                         sd   = round(sd(adj_nMLH1.foci), 3),
                         se   = round(sd / sqrt(Ncells), 3),
                         cV = round( (as.numeric(sd) / as.numeric(mean_co) ),3)
)

Lynn_CASTf_foci = c(20,21, 23, 25, 26, 26,26,27.5, 28, 28,28,33)
cast_f_row = c("CAST", "female", 1, length(Lynn_CASTf_foci), round(mean(Lynn_CASTf_foci),3), 
               round(var(Lynn_CASTf_foci),3), round(sd(Lynn_CASTf_foci),3), 
               round(sd(Lynn_CASTf_foci)/sqrt(length(Lynn_CASTf_foci)),3 ), 
               round(sd(Lynn_CASTf_foci) / (mean(Lynn_CASTf_foci) ), 3)  )

AP_strain_table <- rbind(AP_strain_table, cast_f_row)

AP_strain_table$subsp  <- ifelse(grepl("WSB", AP_strain_table$strain), "M.m. domesticus", 
                      ifelse(grepl("LEW", AP_strain_table$strain), "M.m. domesticus",
                      ifelse(grepl("G", AP_strain_table$strain), "M.m. domesticus",
              ifelse(grepl("PWD", AP_strain_table$strain), "M.m. musculus",
             ifelse(grepl("MSM", AP_strain_table$strain), "M.m. musculus", 
        
                    ifelse(grepl("CAST", AP_strain_table$strain), "M.m. castaneus", 
                   ifelse(grepl("HMI", AP_strain_table$strain), "M.m. castaneus", 
            ifelse(grepl("SPRET", AP_strain_table$strain), "Mus spretus", ""))))))))

#adjust the order...

#set the order for a factor
AP_strain_table$subsp<- factor(AP_strain_table$subsp,levels =c("M.m. domesticus", "M.m. musculus",
                 "M.m. castaneus","Mus spretus"), order=T )

#puts table in the right order
AP_strain_table <- AP_strain_table %>%
  arrange(strain, sex, subsp) %>%               # sort your dataframe, by the focal categories
  mutate(image.title = factor(strain)) #another category that you want the order to match

#MLH1_data
MLH1_by_F_strain <- AP_strain_table[AP_strain_table$sex == "female", ]
MLH1_by_M_strain <- AP_strain_table[AP_strain_table$sex == "male", ]

################
# Strain Plots #
################
#text has to be changed outside of ggplot?
bold.italic.12.text <- element_text(face = "bold.italic", size = 12)

#sort by subspecies
dom_seq <- seq(length(MLH1_by_F_strain$strain[MLH1_by_F_strain$subsp == "M.m. domesticus"]))

#musc and space
musc_seq <- seq(length(dom_seq)+2, length(dom_seq)+length(MLH1_by_F_strain$strain[MLH1_by_F_strain$subsp == "M.m. musculus"])+1,1)

cast_seq <- seq(musc_seq[length(musc_seq)]+1, musc_seq[length(musc_seq)]+length(MLH1_by_F_strain$strain[MLH1_by_F_strain$subsp == "M.m. castaneus"]))

#length of MLH1_by_F_strain, dom vs musc, vs outgroup
x_space_scale = c(dom_seq, musc_seq, cast_seq)


## subspecies annotate texts
grob <- grobTree(textGrob(c("M.m. \n domesticus", "M.m. \n musculus", "M.m.\n castaneus"),x = c(2,5,7),y=22,
                  gp=gpar(col="red", fontsize=13, fontface="italic")))
# Plot
sp2 + annotation_custom(grob)


#okay .. plot is coming along
#not perfect...
try <- ggplot(MLH1_by_F_strain, aes(y = as.numeric(mean_co), x=x_space_scale, color=strain))+ geom_point(size = 4)+ylim(20, 32)
try <- try + geom_errorbar(aes(ymin = as.numeric(mean_co) - as.numeric(se), ymax = as.numeric(mean_co) + as.numeric(se)), size=1.5, width=0.15)+
  scale_color_manual(values=c("#56B4E9","cadetblue4","cadetblue","coral1","#E69F00", "yellowgreen") )

try <- try + labs(x="", y= "Female MLH1 Foci") + theme(axis.text = bold.italic.12.text)

try <- try + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      panel.background = element_blank(), axis.line = element_line(colour = "black"))

try <- try +annotate("text", x = c(2,2, 5,5,8, 8), y = c(22,21,22,21,22,21),
        label = c("M.m.","domesticus","M.m.","musculus","M.m.","castaneus") , fontface = 'italic' )
try


#below script prepares a file for saving in working dir
dev.copy(png,'NOV_female_plot.png')
dev.off()
#dev.off() #there's an extra layer?

####################
# Male strain plot #
####################

