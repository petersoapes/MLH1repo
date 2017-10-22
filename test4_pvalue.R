

library(knitr)
library(ggplot2)
library(pwr)
library(plyr)
library(lattice)
library(dplyr)

setwd("C:/Users/alpeterson7/Documents/MLH1repo")
load(file="MLH1_data_setup.RData")


q_cutoff_table <- ddply(MLH1_data, .(mouse), summarise,
                        total =  length(adj_nMLH1.foci),
                        q5 = sum(as.numeric(quality) >= 4, na.rm = TRUE ), 
                        above5 = sum(as.numeric(quality) <= 4, na.rm = TRUE )
                        # q_l3 = sum(as.numeric(as.numeric(quality)) <= 4, na.rm = TRUE )
)
#q_cutoff_table
passed_mice <- as.vector(q_cutoff_table[q_cutoff_table$above5 > 15,]$mouse)
#
not_passed <- as.vector(unique(MLH1_data[!(unique(MLH1_data$mouse) %in% passed_mice),]$mouse) )
#mnake dataframe with just passing mice -- so that it can be ploted
passed_mice_df <- MLH1_data[ (as.numeric(MLH1_data$quality) < 5), ]

listOmice <- q_cutoff_table$mouse[ (q_cutoff_table$total >= 25) ]
#mke new df with from 
df.pasd.mice <- MLH1_data[MLH1_data$mouse %in% listOmice, ]


ply_data <- df.pasd.mice[df.pasd.mice$mouse == "8oct14_PWD_f8",]


#make a dataframe of many p values
#makes dfs, with the samples. // sampling from MLH1
resample_df <- function(x, samp_size){
#rep could always be two, since the test is a t.test  
  
  new.df = matrix(nrow=2, ncol=samp_size)
  for(i in 1:2) {
    new.df[i,] = sample(x, samp_size)
    
    }
  mmm = list(as.data.frame(new.df, colnames(c("mouse", "rep")) ),  t.test(new.df[,1], new.df[,2])$p.value)

  return(mmm )
  
  
  #return(as.data.frame(new.df, colnames(c("mouse", "rep")) ) )#colnms = 
  #return sampled
}


#this works. sampling x permutations, y sample size
vv <- resample_df(df.pasd.mice$nMLH1.foci,4)


## I think this does it! this makes a list of 23 dfs (for each mouse in the large df)
##this makes dfs for doing the t.tests
kk<-dlply(df.pasd.mice, .(mouse), function(x) resample_df(x$nMLH1.foci,2,5) )
#applies permut function over mouse -- then run a t.test and return pvalue

#two
jj<-ddply(df.pasd.mice, .(mouse), function(x) resample_df(x$nMLH1.foci,20) )
#run a t.test on -- isolate the rows for each mouse
#ddply(jj, .(mouse), function() t.test())


