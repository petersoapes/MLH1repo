# Distributions
# make plots that show the distributions of different categories
#

library(plyr)
library(dplyr)
library(ggplot2)
library(reshape2)
setwd("C:/Users/alpeterson7/Documents/MLH1repo")
load(file="MLH1_data_setup.RData")


# subsp
subsp_BP <- ggplot(MLH1_data, aes(x = as.factor(subsp), y = adj_nMLH1.foci, 
    fill= strain )) + geom_boxplot()  
+ ggtitle("Boxplots of MLH1 distributions by mouse") +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  scale_fill_manual(values=c("#56B4E9", "cadetblue", "lightblue",  "coral1", "#E69F00", "yellowgreen", "green", "grey" ))
subsp_BP <- subsp_BP   + facet_wrap(~ sex, scales="free")
subsp_BP


subsp_Den <- ggplot(MLH1_data, aes(x = adj_nMLH1.foci, fill=sex)) + 
  geom_density(alpha=.6)  +
  ggtitle("")
subsp_Den <- subsp_Den   + facet_wrap(~ subsp, scales="free")
subsp_Den

subsp_Hist <- ggplot(MLH1_data, aes(x = adj_nMLH1.foci, fill=sex)) + 
  geom_histogram(binwidth = 1, alpha=.6)  +
  ggtitle("")
subsp_Hist <- subsp_Hist   + facet_wrap(~ subsp, scales="free")
subsp_Hist



#strain
strain_Den <- ggplot(MLH1_data, aes(x = adj_nMLH1.foci, fill=sex)) + 
  geom_density(alpha=.6)  +
  ggtitle("")
strain_Den <- strain_Den   + facet_wrap(~ strain, scales="free")
strain_Den

strain_Hist <- ggplot(MLH1_data, aes(x = adj_nMLH1.foci, fill=sex)) + 
  geom_histogram(binwidth = 1, alpha=.6)  +
  ggtitle("")
strain_Hist <- strain_Hist   + facet_wrap(~ strain, scales="free")
strain_Hist
