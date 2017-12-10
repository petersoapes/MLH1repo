# Script for displaying p values from T-tests
# input: MLH1 data environment
# output: 
# 

#purpose, compare how different sample sizes of MLH1 counts drawn from the same mouse 
#(may appear to come from different distributions)
# (via pvalue of t-tests)
# Given that distributions of MLH1 counts have different variance across mice, 
# (different cell sample sizes from each mouse may not reflect the true distribution
#(there may be more error in testing the differences in distributions due to smaller sample
#sizes.

# Larger Goal, is to assess which mice have sufficient cells observations to acurately discribe the MLH1 count distribution.
# or observing more cells wouldn't change the distribution.


#LOAD data

setwd("C:/Users/alpeterson7/Documents/MLH1repo")
#setwd("Documents/MLH1repo")
#setwd("C:/Users/April/Desktop/MLH1repo")
load(file="MLH1_data_setup.RData")
library(ggplot2)
library(plyr)
library(dplyr)
library(raster)
library(reshape2)


#seperate large dataframe by how many cells are in the samples
twenty <- AP_mouse_table[AP_mouse_table$Ncells >= 20,]
lessTwenty <- AP_mouse_table[AP_mouse_table$Ncells < 20,]
#these will give the list of mice

#subset MLH1_data by these lists. (fixed line below, )
twenty_data <- MLH1_data[MLH1_data$mouse %in% twenty$mouse, ]
lessTwenty_data <- MLH1_data[MLH1_data$mouse %in% lessTwenty$mouse,]


# Create dataframes that iterate through different sample sizes of MLH1 counts 
# Outlists are dataframes, filled with pvalues from t-tests of different sample sizes.
# Q: How do the 20cells or greater mice observations compare with the lessthan 20 cell
# mouse observations.

sim_iterations <- 10000

outlist10 = list()
for(ten in 1:length(unique(twenty$mouse))){
  print(twenty$mouse[ten])
  mus_data <- twenty_data[twenty_data$mouse == twenty$mouse[ten],]
  outlist10[ten] <- list(replicate(sim_iterations, t.test(sample(mus_data$adj_nMLH1.foci, 10, replace = FALSE),
                                                          sample(mus_data$adj_nMLH1.foci, 10, replace = FALSE) )$p.value ) )
}
names(outlist10) <- twenty$mouse

outlist15 = list()
for(fif in 1:length(unique(twenty$mouse))){
  print(twenty$mouse[fif])
  mus_data <- twenty_data[twenty_data$mouse == twenty$mouse[fif],]
  outlist15[fif] <- list(replicate(sim_iterations, t.test(sample(mus_data$adj_nMLH1.foci, 15, replace = FALSE),
                                                          sample(mus_data$adj_nMLH1.foci, 15, replace = FALSE) )$p.value ) )
}
names(outlist15) <- twenty$mouse
#outlist is not a list of p-value list. ()

outlist20 = list()
for(tw in 1:length(unique(twenty$mouse))){
  print(twenty$mouse[tw])
  mus_data <- twenty_data[twenty_data$mouse == twenty$mouse[tw],]
  outlist20[tw] <- list(replicate(sim_iterations, t.test(sample(mus_data$adj_nMLH1.foci, 20, replace = FALSE),
                                                         sample(mus_data$adj_nMLH1.foci, 20, replace = FALSE) )$p.value ) )
}
names(outlist20) <- twenty$mouse


#make plots to display Pvalues
#1. histograms
#2. scatterplots

#plot lists of gg histograms
plot_list10 = list()
for (i in 1:length(twenty$mouse)){
  print(i)  
  try_df <- as.data.frame(outlist10[[i]])
  Title_mus_name <- paste("10000 iterations\n Test of 2 Samples of 10, \n", twenty$mouse[i] )
  colnames(try_df) <- "pvals"
  p10 <- ggplot(try_df) + ggtitle( Title_mus_name ) +xlim(0,1)+
    geom_histogram(aes(x=pvals) )
  plot_list10[[i]] <- p10
}

plot_list15 = list()
for (i in 1:length(twenty$mouse)){
  print(i)  
  try_df <- as.data.frame(outlist15[[i]])
  Title_mus_name <- paste("10000 iterations\n Test of 2 Samples of 15, \n", twenty$mouse[i] )
  colnames(try_df) <- "pvals"
  p15 <- ggplot(try_df) + ggtitle( Title_mus_name ) + xlim(0,1)+
    geom_histogram(aes(x=pvals) )#xlim
  #tryfiting 3 plots
  plot_list15[[i]] <- p15
}

plot_list20 = list()
for (i in 1:length(twenty$mouse)){
  print(i)  
  try_df <- as.data.frame(outlist20[[i]])
  colnames(try_df) <- "pvals"
  Title_mus_name <- paste("10000 iterations\n, Test of 2 Samples of 20, \n", twenty$mouse[i] )
  p20 <- ggplot(try_df) + ggtitle( Title_mus_name ) + xlim(0,1)+
    geom_histogram(aes(x=pvals) )
  plot_list20[[i]] <- p20
}


#make list of scatter plots
sctr_plot = list()
for (i in 1:length(twenty$mouse)){
  print(i)  
  df10 <- as.data.frame(outlist10[[i]])
  df15 <- as.data.frame(outlist15[[i]])
  df20 <-as.data.frame(outlist20[[i]])
  names(df10) = "pvals"
  names(df15)= "pvals"
  names(df20)= "pvals"
  allDF <- data.frame(pvals = rbind(df10, df15, df20),
                      DFsets = c (rep("samp10",length(df10[,1]) ), 
                                  rep("samp15",length(df10[,1])), 
                                  rep("samp20", length(df10[,1])) )
  )
  #the mouse name is not working
  Title_mus_name <- paste("10000 iterations\n Pvalues of T-tests \n", twenty$mouse[i] )
  colnames(try_df) <- "pvals"
  
  scplot <- ggplot(allDF) + ggtitle( Title_mus_name) +
    geom_jitter(aes(x=DFsets, y=pvals) )
  sctr_plot[[i]] <- scplot
}

#use Function Multiplot to display multiple ggplot objects
source("src/MultiPlot.R")

setwd("C:/Users/alpeterson7/Documents/MLH1repo")
# Saves multiple plots to a tiff file for each mouse
for(i in 1:15) {
  file_name = paste("Sample_sizes_p_", twenty$mouse[i], "_", ".tiff", sep="")#names(outlist15[i]
  tiff(file_name)
  #par( mfrow=c(1,2) )
  multiplot(sctr_plot[[i]], plot_list10[[i]], plot_list15[[i]], plot_list20[[i]],cols=2)
  dev.off()
}

#(the 1000 iterations, there is space between points. 10,000 iterations, there is no space between points)
# These figures should be judged; (if the scatter plots dont look different across the sample sizes,
# interpert that the additional cells didn't affect the permuted distributions).
# 

# The 'passing' and "more cells" designation of mice can be done automatically using a metric of these pvalues.
# 

#merge hudge DF of all mice's outlists
# OR make a table that calculated the means ect of each...
# add mouse col, and sample size col

#make a table / ddplyr on a merged df
