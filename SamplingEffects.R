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


#Todo: add mouse name, total cell number and average quality score to plots

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
twenty <- AP_mouse_table[AP_mouse_table$Ncells >= 20,] #53
#are these assumed to have passed?

lessTwenty <- AP_mouse_table[AP_mouse_table$Ncells < 20,] #28
#these will give the list of mice

#subset MLH1_data by these lists. (fixed line below, )
twenty_data <- MLH1_data[MLH1_data$mouse %in% twenty$mouse, ]
lessTwenty_data <- MLH1_data[MLH1_data$mouse %in% lessTwenty$mouse,]


# Create dataframes that iterate through different sample sizes of MLH1 counts 
# Outlists are dataframes, filled with pvalues from t-tests of different sample sizes.
# Q: How do the 20cells or greater mice observations compare with the lessthan 20 cell
# mouse observations.


#consider during this into function since it's so long, and then has to be done on the less than twenty list
sim_iterations <- 10000

#list, DF, ncells, sim_num

source("src/Func_sampleCells.R")
lessTwenty_pvals_5_10 <- sample_cells_pvals(lessTwenty, lessTwenty_data, 5, 10)#this returns a list (of vectors?) pvalues  for each mouse in a list

#nn[[4]] is the syntax to get just the pvalues
#names(nn[4]) is now you get the name of the vector / list


#sample >twenty mice
lessTwenty_pvals_10_1k <- sample_cells_pvals(lessTwenty, lessTwenty_data, 10, 1000)#this returns a list (of vectors?) pvalues  for each mouse in a list


Twenty_pvals_10_1k <- sample_cells_pvals(twenty, twenty_data, 10, 1000)#this returns a list (of vectors?) pvalues  for each mouse in a list

Twenty_pvals_15_1k <- sample_cells_pvals(twenty, twenty_data, 15, 1000)#this returns a list (of vectors?) pvalues  for each mouse in a list

Twenty_pvals_20_1k <- sample_cells_pvals(twenty, twenty_data, 20, 1000)#this returns a list (of vectors?) pvalues  for each mouse in a list


#make all these outlist of pvalues...

#
# figure out how to calq different of mean pvalue between sample sizes!!!
#



#think of saving these DF, as they take time 
save.image("SampleSizePvalues.RData")

#make plots to display Pvalues
#1. histograms
#2. scatterplots

#plot lists of gg histograms
plot_list10 = list()
for (i in 1:length(twenty$mouse)){
  print(i)  
  try_df <- as.data.frame(outlist10[[i]]) #turning a list into df
  Title_mus_name <- paste("10000 iterations\n Test of 2 Samples of 10, \n", twenty$mouse[i] )
  #add mouse name, total cells 
  
  colnames(try_df) <- "pvals"
  #create ggplot, and putting it into a list of ggplots
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

#after that code, there are 3 lists of ggplots histograms (by sample size)



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
#merging smaller dfs
    allDF <- data.frame(pvals = rbind(df10, df15, df20),
                      DFsets = c (rep("samp10",length(df10[,1]) ), 
                                  rep("samp15",length(df10[,1])), 
                                  rep("samp20", length(df10[,1])) )
  )
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

#make a table / ddplyr on a merged df. (outlist is a list)
#outlists are lists of lists -- 

#this merges all lists into cols of a df
dd  <-  as.data.frame(matrix(unlist(outlist10), 
                  nrow=length(unlist(outlist10[1]))))
names(dd) <- twenty$mouse

ff <- as.data.frame(matrix(unlist(outlist15), 
                 nrow=length(unlist(outlist15[1]))))
names(ff) <- twenty$mouse

tt <- as.data.frame(matrix(unlist(outlist20), 
                           nrow=length(unlist(outlist20[1]))))
names(tt) <- twenty$mouse

allouts <- rbind(dd,ff,tt)
allouts$SampleSize <- c(rep(10, sim_iterations), rep(15, sim_iterations), 
                        rep(20, sim_iterations))


#all of the means for the columns can be calulated with colMeans
Sample10_means <- colMeans(allouts[1:(sim_iterations),])
Sample15_means <- colMeans(allouts[(sim_iterations+1):((sim_iterations*2)),])
Sample20_means <- colMeans(allouts[((sim_iterations*2)+1):(sim_iterations*3),])
#this table shows the 
sumdiffs <- abs(Sample10_means-Sample15_means) + abs(Sample15_means-Sample20_means)
#list of mice where the difference in means between sample sizes is more than 0.2.
# Mice that should have more 
SampleSizeEffects <- sumdiffs[sumdiffs > 0.2]


## Assess mice with less than 20 cells, which ones have a sufficient amount
sim_its <- 100

#exclude mice with less than 10
TenCells <- lessTwenty[lessTwenty$Ncells >= 10,]

#subset MLH1_data by these lists. (fixed line below, )
TenCells_data <- MLH1_data[MLH1_data$mouse %in% TenCells$mouse,]


olist5 = list()
for(ten in 1:length(unique(TenCells$mouse))){
  print(TenCells$mouse[ten])
  mus_data <- TenCells_data[TenCells_data$mouse == TenCells$mouse[ten],]
  olist5[ten] <- list(replicate(sim_its, t.test(sample(mus_data$adj_nMLH1.foci, 5, replace = FALSE),
                                                          sample(mus_data$adj_nMLH1.foci, 5, replace = FALSE) )$p.value ) )
}
names(olist5) <- TenCells$mouse

olist10 = list()
for(fif in 1:length(unique(TenCells$mouse))){
  print(TenCells$mouse[fif])
  mus_data <- TenCells_data[TenCells_data$mouse == TenCells$mouse[fif],]
  olist10[fif] <- list(replicate(sim_its, t.test(sample(mus_data$adj_nMLH1.foci, 10, replace = FALSE),
                                                          sample(mus_data$adj_nMLH1.foci, 10, replace = FALSE) )$p.value ) )
}
names(outlist10) <- TenCells$mouse
#outlist is not a list of p-value list. ()


#
dd  <-  as.data.frame(matrix(unlist(olist5), 
                             nrow=length(unlist(olist5[1]))))
names(dd) <- TenCells$mouse

ff <- as.data.frame(matrix(unlist(olist10), 
                           nrow=length(unlist(olist10[1]))))
names(ff) <- TenCells$mouse

allouts <- rbind(dd,ff)
allouts$SampleSize <- c(rep(5, sim_its), rep(10, sim_its))

sample5_means <- colMeans(allouts[1:(sim_its),])
sample10_means <- colMeans(allouts[(sim_its+1):((sim_its*2)),])

#
somdiffs <- abs(sample5_means-sample10_means)

plot(somdiffs[1:20])

#list of mice where the difference in means between sample sizes is more than 0.2.
# Mice that should have more 
SampleSizeEffects <- somdiffs[somdiffs > 0.2]
