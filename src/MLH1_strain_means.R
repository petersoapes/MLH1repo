#R script for making plots of the distributions of MLH1 foci numbers
#input: MLH1 data/ environment, mouse specific table
#plots: mouse specific, strain averages

#output: 2 sex-specific figures, 1 merged figure
#<don't adjust male data, think about including BD's data>

#..maybe there should be a R code for saving processing the data
#but for now, just construct the make file when dataframe is pasted into the dir
library(plyr)
library(dplyr)
library(ggplot2)

#############
# LOAD DATA #
#############
#read in working environment that was already set
#setwd("C:/Users/alpeterson7/Documents/MLH1repo")
setwd("C:/Users/April/Desktop/MLH1repo")
load(file="MLH1_data_setup.RData")

###################
# DATA TREATMENTS #
###################
#make sure the levels are in the correct order for the graph plotting
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
            ifelse(grepl("KAZ", AP_strain_table$strain), "M.m. musculus", 
        
                    ifelse(grepl("CAST", AP_strain_table$strain), "M.m. castaneus", 
                   ifelse(grepl("HMI", AP_strain_table$strain), "M.m. castaneus",
              ifelse(grepl("SPIC", AP_strain_table$strain), "Mus spicilegus",
            ifelse(grepl("SPRET", AP_strain_table$strain), "Mus spretus", ""))))))))))

#("black", "#56B4E9","cadetblue4","cadetblue","coral1","#E69F00", "yellowgreen") )

#set the order for a factor
AP_strain_table$subsp<- factor(AP_strain_table$subsp,levels =c("M.m. domesticus", "M.m. musculus",
                 "M.m. castaneus","Mus spretus", "Mus spicilegus"), order=T )

AP_strain_table$strain<- factor(AP_strain_table$strain,levels =c("G", "LEWES",
                        "WSB", "PWD","MSM","KAZ","CAST", "HMI","SPIC","SPRET"), order=T )

#puts table in the right order
AP_strain_table <- AP_strain_table %>%
  arrange(strain, sex, subsp) %>%               # sort your dataframe, by the focal categories
  mutate(image.title = factor(strain)) #another category that you want the order to match

#MLH1_data
MLH1_by_F_strain <- AP_strain_table[AP_strain_table$sex == "female", ]
MLH1_by_M_strain <- AP_strain_table[AP_strain_table$sex == "male", ]

####################
# Female strain plot #
####################
MLH1_by_F_strain <- MLH1_by_F_strain[-c(7,8),]

MLH1_by_F_strain <- MLH1_by_F_strain[MLH1_by_F_strain$strain != "KAZ", ]

png('femaleMLH1_plot2se.png')

#man_x_mspace <- c(.25, .5, .75, 
#1.65,1.85,
#2.8)
man_x_fspace <- c(.35, .5, .75,  #.25, .65, 1.1
         2,2.2,
          3.6)

ff_plot <- ggplot(MLH1_by_F_strain, aes(y = as.numeric(mean_co), x=man_x_fspace, color=strain))+ 
  geom_point(size = 4) +
  scale_y_continuous(breaks = seq(20,32, by=2)) +
  coord_cartesian(ylim = c(20,32))+
  xlim(0,5)+
  geom_errorbar(aes(ymin = as.numeric(mean_co)-(as.numeric(se)*2), 
                    ymax = as.numeric(mean_co)+ (as.numeric(se)*2)),
  size=1.2, width=0.1)+
  scale_color_manual(values=c("#56B4E9","cadetblue4","cadetblue","coral1","#E69F00", "yellowgreen") )+
   labs(x="", y= "Female MLH1 Foci") +
   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line.y = element_line(colour = "black", size = 1.5), legend.position="none",
        axis.ticks.y = element_line(colour = "black", size = 1.5),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0), size=20),
        
        axis.text.y = element_text(size=15, face="bold"),
      axis.title.x=element_blank(), axis.line.x = element_blank(), axis.text.x=element_blank(), axis.ticks.x = element_blank() ) +

    annotate("text", x = c(mean(man_x_fspace[1:3]), mean(man_x_fspace[1:3]), 
    mean(man_x_fspace[4:5]), mean(man_x_fspace[4:5]), 
    man_x_fspace[6], man_x_fspace[6]),
           y = c(21,20.5, 21,20.5,21,20.5),

        label = c("M.m.","domesticus","M.m.","musculus","M.m.","castaneus"),fontface = 'bold.italic',hjust = 0.5, size=7) +

  annotate("segment", x= mean(man_x_fspace[1:3])-.3, xend=mean(man_x_fspace[1:3])+.3, y=21.5, yend=21.5, colour="black",
         size=1.5) +

    annotate("segment", x=mean(man_x_fspace[4:5])-.3,xend=mean(man_x_fspace[4:5])+.3, y=21.5, yend=21.5, colour="black",
           size=1.5) +
  annotate("segment", x=man_x_fspace[6]-.3, xend=man_x_fspace[6]+.3, y=21.5, yend=21.5, colour="black",
           size=1.5)

ff_plot

dev.off()
####################
# Male strain plot #
####################
#nov 17, remove HMI mouse, 8 cells total
MLH1_by_M_strain <- MLH1_by_M_strain[MLH1_by_M_strain$strain != "HMI",]
MLH1_by_M_strain <- MLH1_by_M_strain[MLH1_by_M_strain$strain != "SPRET",]
MLH1_by_M_strain <- MLH1_by_M_strain[MLH1_by_M_strain$strain != "KAZ",]
MLH1_by_M_strain <- MLH1_by_M_strain[MLH1_by_M_strain$strain != "SPIC",]

png('maleMLH1_plot2se.png')
man_x_mspace <- c(.35, .5, .75, 
            2,2.2,
            3.6)
mm_plot <- ggplot(MLH1_by_M_strain, aes(y = as.numeric(mean_co), x=man_x_mspace, color=strain))+ 
  geom_point(size = 4)+ 
#  ylim(18,32) +  
#  scale_y_continuous(breaks = seq(18, 34, by = 2)) +
  scale_y_continuous(breaks = seq(20,35, by=2)) +
  coord_cartesian(ylim = c(20,32))+
  xlim(0,5)+
  geom_errorbar(aes(ymin = as.numeric(mean_co)-(as.numeric(se)*2), 
                    ymax = as.numeric(mean_co)+ (as.numeric(se)*2)),
                size=1.2, width=0.1)+
  
   #geom_errorbar(aes(ymin = as.numeric(mean_co) - as.numeric(se), 
  #                  ymax = as.numeric(mean_co)+ as.numeric(se),
   #   size=.5, width=.1))+
  scale_color_manual(values=c("#56B4E9","cadetblue4","cadetblue",
                "coral1","#E69F00", "yellowgreen") )+ #"olivedrab2", (HMI) ,"orchid" (SPRET)
  
  labs(x="", y= "Male MLH1 Foci") +
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line.y = element_line(colour = "black", size = 1.5), legend.position="none",
        axis.ticks.y = element_line(colour = "black", size = 1.5),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0), size=20),
        axis.text.y = element_text(size=15, face="bold"),
  axis.title.x=element_blank(), axis.line.x = element_blank(), axis.text.x=element_blank(), axis.ticks.x = element_blank()) +
   annotate("text", x = c(mean(man_x_mspace[1:3]), mean(man_x_mspace[1:3]), 
                         mean(man_x_mspace[4:5]), mean(man_x_mspace[4:5]),
                         mean(man_x_mspace[6]), mean(man_x_mspace[6])),
           y = c(21,20.5, 21,20.5,21,20.5),
      size=7,
      label = c("M.m.","domesticus","M.m.","musculus","M.m.","castaneus"),hjust = 0.5, fontface='bold.italic') +
# .75, 1.15, 1.45 
  annotate("segment", x= mean(man_x_mspace[1:3])-.3, xend=mean(man_x_mspace[1:3])+.3, y=21.5, yend=21.5, colour="black",
           size=1.5) +
  annotate("segment", x=mean(man_x_mspace[4:5])-.3,xend=mean(man_x_mspace[4:5])+.3, y=21.5, yend=21.5, colour="black",
           size=1.5) +
  annotate("segment", x=man_x_mspace[6]-.3, xend=man_x_mspace[6]+.3, y=21.5, yend=21.5, colour="black",
           size=1.5)
mm_plot


dev.off()
#make comparisons of these traits 
