## XX adjustment permutations

#XX issue, because XX is one of the longer chromomsomes, and not drawn from the middle of the SC length distribution
#in female data the XX acts as an extra long autosome, which will inflate the mean bivalent SC length, relative to males)
#even if the SC length distributions were equal

#with these permutaitons I hope to address, what the effect on the mean bivalent estimate is due to an extra autosome sample.
#compare bivalent summaries(mean) for permuted datasets,
# make 3 distinct DFs, female - 20 random biv draws, female - 19 random biv draws, male 19 random biv draws

#the script below mkes 1000 data sets 
WSBfemale.20.rand_BIG.DF.LIST <- replicate(1000,
                         #add subsample to a list, this works!!!
                         list( bind_rows(list(Curated_BivData[ sample( which(Curated_BivData$category=="WSB female"), 20), ]))
                         ))
WSBfemale.19.rand_BIG.DF.LIST <- replicate(1000,
                                        #add subsample to a list, this works!!!
                                        list( bind_rows(list(Curated_BivData[ sample( which(Curated_BivData$category=="WSB female"), 19), ]))
                                        ))
WSBmale.19.rand_BIG.DF.LIST <- replicate(1000, 
                                        #add subsample to a list, this works!!!
                                        list( bind_rows(list(Curated_BivData[ sample( which(Curated_BivData$category=="WSB male"), 19), ]))
                                        ))
#head(BIG.DF.LIST[[2]]) - the 2nd data set
#### 
### DFs above are dummy DF's for 1000 in silico cells
#head(BIG.DF.LIST[[2]]) - the 2nd data set

#summarize, calq the mean SC length across all the prelicates
#I want to end up with 1000 mean.SC observations, to plot as boxplots

#1. make an empty DF list
#Loop through the list of DFs and calq the average SC
Means.Here_WSB <- data.frame(
                    WSBmean_sc_female20 = rep(0, length(WSBfemale.20.rand_BIG.DF.LIST)),
                    WSBmean_sc_female19 = rep(0, length(WSBfemale.19.rand_BIG.DF.LIST)),
                    WSBmean_sc_male19 = rep(0, length(WSBmale.19.rand_BIG.DF.LIST))  )

#FILL in metrics
for(i in 1:length(Means.Here_WSB$WSBmean_sc_female20)){  #current data from
  print(i)
  #filling in metrics for Means.Here
  Means.Here_WSB$WSBmean_sc_female20[i] <- mean(WSBfemale.20.rand_BIG.DF.LIST[[i]]$chromosomeLength)#looks wacky, but this is right syntax
  Means.Here_WSB$WSBmean_sc_female19[i] <- mean(WSBfemale.19.rand_BIG.DF.LIST[[i]]$chromosomeLength)
  Means.Here_WSB$WSBmean_sc_male19[i] <- mean(WSBmale.19.rand_BIG.DF.LIST[[i]]$chromosomeLength)
}


#MELT, put all 100 means into 1 col and make notation of the dataset it came hence
Melted.SC.means <- melt(Means.Here, c( "mean_sc_female20","mean_sc_female19","mean_sc_male19"), 
                       id=c() )

#clean up dm1 (there are a bunch of rows without useful information)
Melted.SC.means <- Melted.SC.means[(!is.na(Melted.SC.means$value)),]
colnames(Melted.SC.means) <- c("DataSet", "mean")

#plot the stuff
looky.here <- ggplot(Melted.SC.means, aes(x= DataSet, y=mean))+geom_jitter()+geom_boxplot()+ggtitle("WSB auto-biv data")

#WHAT? It seems like the effect of sampling an extra autosome is significant... opps, mislabeled male and female :)
the.tree.means <- c(mean(Means.Here$mean_sc_female20), mean(Means.Here$mean_sc_female19), mean(Means.Here$mean_sc_male19))
t.test(Means.Here$mean_sc_female20, Means.Here$mean_sc_female19)#p-value = 0.4579


#REPEAT FOR ALL STRAINS?

#the script below mkes 1000 data sets 
Gfemale.20.rand_BIG.DF.LIST <- replicate(1000,
                                           #add subsample to a list, this works!!!
                        list( bind_rows(list(Curated_BivData[ sample( which(Curated_BivData$category=="G female"), 20), ]))
                                           ))
Gfemale.19.rand_BIG.DF.LIST <- replicate(1000,
                                           #add subsample to a list, this works!!!
                        list( bind_rows(list(Curated_BivData[ sample( which(Curated_BivData$category=="G female"), 19), ]))
                                           ))
Gmale.19.rand_BIG.DF.LIST <- replicate(1000, 
                                         #add subsample to a list, this works!!!
                        list( bind_rows(list(Curated_BivData[ sample( which(Curated_BivData$category=="G male"), 19), ]))
                                         ))

Means.Here_G <- data.frame(
  Gmean_sc_female20 = rep(0, length(Gfemale.20.rand_BIG.DF.LIST)),
  Gmean_sc_female19 = rep(0, length(Gfemale.19.rand_BIG.DF.LIST)),
  Gmean_sc_male19 = rep(0, length(Gmale.19.rand_BIG.DF.LIST))  )

#FILL in metrics
for(i in 1:length(Means.Here_G$Gmean_sc_female20)){  #current data from
  print(i)
  #filling in metrics for Means.Here
  
  Means.Here_G$Gmean_sc_female20[i] <- mean(Gfemale.20.rand_BIG.DF.LIST[[i]]$chromosomeLength)#looks wacky, but this is right syntax
  
  Means.Here_G$Gmean_sc_female19[i] <- mean(Gfemale.19.rand_BIG.DF.LIST[[i]]$chromosomeLength)
  
  Means.Here_G$Gmean_sc_male19[i] <- mean(Gmale.19.rand_BIG.DF.LIST[[i]]$chromosomeLength)
}
#MELT, put all 100 means into 1 col and make notation of the dataset it came hence
G.Melted.SC.means <- melt(Means.Here_G, c( "Gmean_sc_female20","Gmean_sc_female19","Gmean_sc_male19"), 
                        id=c() )

#clean up dm1 (there are a bunch of rows without useful information)
G.Melted.SC.means <- G.Melted.SC.means[(!is.na(G.Melted.SC.means$value)),]
colnames(G.Melted.SC.means) <- c("DataSet", "mean")

#plot the stuff
Gouphy.looky.here <- ggplot(G.Melted.SC.means, aes(x= DataSet, y=mean))+geom_jitter()+geom_boxplot()+ggtitle("G auto-biv data")

G.the.tree.means <- c(mean(Means.Here_G$Gmean_sc_female20), mean(Means.Here_G$Gmean_sc_female19), mean(Means.Here_G$Gmean_sc_male19))
t.test(Means.Here_G$Gmean_sc_female20, Means.Here_G$Gmean_sc_female19) #p-value = 0.9988



#PWD
#the script below mkes 1000 data sets 
PWDfemale.20.rand_BIG.DF.LIST <- replicate(1000,
                                           #add subsample to a list, this works!!!
                                           list( bind_rows(list(Curated_BivData[ sample( which(Curated_BivData$category=="PWD female"), 20), ]))
                                           ))
PWDfemale.19.rand_BIG.DF.LIST <- replicate(1000,
                                           #add subsample to a list, this works!!!
                                           list( bind_rows(list(Curated_BivData[ sample( which(Curated_BivData$category=="PWD female"), 19), ]))
                                           ))
PWDmale.19.rand_BIG.DF.LIST <- replicate(1000, 
                                         #add subsample to a list, this works!!!
                                         list( bind_rows(list(Curated_BivData[ sample( which(Curated_BivData$category=="PWD male"), 19), ]))
                                         ))
#head(BIG.DF.LIST[[2]]) - the 2nd data set
#### 
### DFs above are dummy DF's for 1000 in silico cells
#head(BIG.DF.LIST[[2]]) - the 2nd data set

#summarize, calq the mean SC length across all the prelicates
#I want to end up with 1000 mean.SC observations, to plot as boxplots

#1. make an empty DF list
#Loop through the list of DFs and calq the average SC
Means.Here_PWD <- data.frame(
  PWDmean_sc_female20 = rep(0, length(PWDfemale.20.rand_BIG.DF.LIST)),
  PWDmean_sc_female19 = rep(0, length(PWDfemale.19.rand_BIG.DF.LIST)),
  PWDmean_sc_male19 = rep(0, length(PWDmale.19.rand_BIG.DF.LIST))  )

#FILL in metrics
for(i in 1:length(Means.Here_PWD$PWDmean_sc_female20)){  #current data from
  print(i)
  #filling in metrics for Means.Here
  Means.Here_PWD$PWDmean_sc_female20[i] <- mean(PWDfemale.20.rand_BIG.DF.LIST[[i]]$chromosomeLength)#looks wacky, but this is right syntax
  Means.Here_PWD$PWDmean_sc_female19[i] <- mean(PWDfemale.19.rand_BIG.DF.LIST[[i]]$chromosomeLength)
  Means.Here_PWD$PWDmean_sc_male19[i] <- mean(PWDmale.19.rand_BIG.DF.LIST[[i]]$chromosomeLength)
}

#MELT, put all 100 means into 1 col and make notation of the dataset it came hence
PWDMelted.SC.means <- melt(Means.Here_PWD, c( "PWDmean_sc_female20","PWDmean_sc_female19","PWDmean_sc_male19"), 
                        id=c() )

#clean up dm1 (there are a bunch of rows without useful information)
PWDMelted.SC.means <- PWDMelted.SC.means[(!is.na(PWDMelted.SC.means$value)),]
colnames(PWDMelted.SC.means) <- c("DataSet", "mean")

#plot the stuff
looky.here <- ggplot(PWDMelted.SC.means, aes(x= DataSet, y=mean))+geom_jitter()+geom_boxplot()+ggtitle("PWD auto-biv data")

the.tree.means <- c(mean(PWDMelted.SC.means$PWDmean_sc_female20), mean(PWDMelted.SC.means$PWDmean_sc_female19), mean(PWDMelted.SC.means$PWDmean_sc_male19))
t.test(Means.Here$PWDmean_sc_female20, Means.Here$PWDmean_sc_female19)







#MSM
#the script below mkes 1000 data sets 
MSMfemale.20.rand_BIG.DF.LIST <- replicate(1000,
                                           #add subsample to a list, this works!!!
                                           list( bind_rows(list(Curated_BivData[ sample( which(Curated_BivData$category=="MSM female"), 20), ]))
                                           ))
MSMfemale.19.rand_BIG.DF.LIST <- replicate(1000,
                                           #add subsample to a list, this works!!!
                                           list( bind_rows(list(Curated_BivData[ sample( which(Curated_BivData$category=="MSM female"), 19), ]))
                                           ))
MSMmale.19.rand_BIG.DF.LIST <- replicate(1000, 
                                         #add subsample to a list, this works!!!
                                         list( bind_rows(list(Curated_BivData[ sample( which(Curated_BivData$category=="MSM male"), 19), ]))
                                         ))

Means.Here_MSM <- data.frame(
  MSMmean_sc_female20 = rep(0, length(MSMfemale.20.rand_BIG.DF.LIST)),
  MSMmean_sc_female19 = rep(0, length(MSMfemale.19.rand_BIG.DF.LIST)),
  MSMmean_sc_male19 = rep(0, length(MSMmale.19.rand_BIG.DF.LIST))  )

#FILL in metrics
for(i in 1:length(Means.Here_MSM$MSMmean_sc_female20)){  #current data from
  print(i)
  #filling in metrics for Means.Here
  Means.Here_MSM$MSMmean_sc_female20[i] <- mean(MSMfemale.20.rand_BIG.DF.LIST[[i]]$chromosomeLength)#looks wacky, but this is right syntax
  Means.Here_MSM$MSMmean_sc_female19[i] <- mean(MSMfemale.19.rand_BIG.DF.LIST[[i]]$chromosomeLength)
  Means.Here_MSM$MSMmean_sc_male19[i] <- mean(MSMmale.19.rand_BIG.DF.LIST[[i]]$chromosomeLength)
}

#MELT, put all 100 means into 1 col and make notation of the dataset it came hence
MSMMelted.SC.means <- melt(Means.Here_MSM, c( "MSMmean_sc_female20","MSMmean_sc_female19","MSMmean_sc_male19"), 
                           id=c() )

#clean up dm1 (there are a bunch of rows without useful information)
MSMMelted.SC.means <- MSMMelted.SC.means[(!is.na(MSMMelted.SC.means$value)),]
colnames(MSMMelted.SC.means) <- c("DataSet", "mean")

#plot the stuff
looky.here <- ggplot(MSMMelted.SC.means, aes(x= DataSet, y=mean))+geom_jitter()+geom_boxplot()+ggtitle("MSM auto-biv data")

the.tree.means <- c(mean(MSMMelted.SC.means$MSMmean_sc_female20), mean(MSMMelted.SC.means$MSMmean_sc_female19), mean(MSMMelted.SC.means$MSMmean_sc_male19))
t.test(Means.Here$MSMmean_sc_female20, Means.Here$MSMmean_sc_female19)



#LEW
#the script below mkes 1000 data sets 
LEWfemale.20.rand_BIG.DF.LIST <- replicate(1000,
                                           #add subsample to a list, this works!!!
                                           list( bind_rows(list(Curated_BivData[ sample( which(Curated_BivData$category=="LEW female"), 20), ]))
                                           ))
LEWfemale.19.rand_BIG.DF.LIST <- replicate(1000,
                                           #add subsample to a list, this works!!!
                                           list( bind_rows(list(Curated_BivData[ sample( which(Curated_BivData$category=="LEW female"), 19), ]))
                                           ))
LEWmale.19.rand_BIG.DF.LIST <- replicate(1000, 
                                         #add subsample to a list, this works!!!
                                         list( bind_rows(list(Curated_BivData[ sample( which(Curated_BivData$category=="LEW male"), 19), ]))
                                         ))

#1. make an empty DF list
#Loop through the list of DFs and calq the average SC
Means.Here_LEW <- data.frame(
  LEWmean_sc_female20 = rep(0, length(LEWfemale.20.rand_BIG.DF.LIST)),
  LEWmean_sc_female19 = rep(0, length(LEWfemale.19.rand_BIG.DF.LIST)),
  LEWmean_sc_male19 = rep(0, length(LEWmale.19.rand_BIG.DF.LIST))  )

#FILL in metrics
for(i in 1:length(Means.Here_LEW$LEWmean_sc_female20)){  #current data from
  print(i)
  #filling in metrics for Means.Here
  Means.Here_LEW$LEWmean_sc_female20[i] <- mean(LEWfemale.20.rand_BIG.DF.LIST[[i]]$chromosomeLength)#looks wacky, but this is right syntax
  Means.Here_LEW$LEWmean_sc_female19[i] <- mean(LEWfemale.19.rand_BIG.DF.LIST[[i]]$chromosomeLength)
  Means.Here_LEW$LEWmean_sc_male19[i] <- mean(LEWmale.19.rand_BIG.DF.LIST[[i]]$chromosomeLength)
}

#MELT, put all 100 means into 1 col and make notation of the dataset it came hence
LEWMelted.SC.means <- melt(Means.Here_LEW, c( "LEWmean_sc_female20","LEWmean_sc_female19","LEWmean_sc_male19"), 
                           id=c() )

#clean up dm1 (there are a bunch of rows without useful information)
LEWMelted.SC.means <- LEWMelted.SC.means[(!is.na(LEWMelted.SC.means$value)),]
colnames(LEWMelted.SC.means) <- c("DataSet", "mean")

#plot the stuff
looky.here <- ggplot(LEWMelted.SC.means, aes(x= DataSet, y=mean))+geom_jitter()+geom_boxplot()+ggtitle("LEW auto-biv data")

the.tree.means <- c(mean(LEWMelted.SC.means$LEWmean_sc_female20), mean(LEWMelted.SC.means$LEWmean_sc_female19), mean(LEWMelted.SC.means$LEWmean_sc_male19))
t.test(Means.Here$LEWmean_sc_female20, Means.Here$LEWmean_sc_female19)


#KAZ - > #LEW
#the script below mkes 1000 data sets 
KAZfemale.20.rand_BIG.DF.LIST <- replicate(1000,
                                           #add subsample to a list, this works!!!
                                           list( bind_rows(list(Curated_BivData[ sample( which(Curated_BivData$category=="KAZ female"), 20), ]))
                                           ))
KAZfemale.19.rand_BIG.DF.LIST <- replicate(1000,
                                           #add subsample to a list, this works!!!
                                           list( bind_rows(list(Curated_BivData[ sample( which(Curated_BivData$category=="KAZ female"), 19), ]))
                                           ))
KAZmale.19.rand_BIG.DF.LIST <- replicate(1000, 
                                         #add subsample to a list, this works!!!
                                         list( bind_rows(list(Curated_BivData[ sample( which(Curated_BivData$category=="KAZ male"), 19), ]))
                                         ))

#1. make an empty DF list, #Loop through the list of DFs and calq the average SC
Means.Here_KAZ <- data.frame(
  KAZmean_sc_female20 = rep(0, length(KAZfemale.20.rand_BIG.DF.LIST)),
  KAZmean_sc_female19 = rep(0, length(KAZfemale.19.rand_BIG.DF.LIST)),
  KAZmean_sc_male19 = rep(0, length(KAZmale.19.rand_BIG.DF.LIST))  )

#FILL in metrics
for(i in 1:length(Means.Here_KAZ$KAZmean_sc_female20)){  #current data from
  print(i)
  #filling in metrics for Means.Here
  Means.Here_KAZ$KAZmean_sc_female20[i] <- mean(KAZfemale.20.rand_BIG.DF.LIST[[i]]$chromosomeLength)#looks wacky, but this is right syntax
  Means.Here_KAZ$KAZmean_sc_female19[i] <- mean(KAZfemale.19.rand_BIG.DF.LIST[[i]]$chromosomeLength)
  Means.Here_KAZ$KAZmean_sc_male19[i] <- mean(KAZmale.19.rand_BIG.DF.LIST[[i]]$chromosomeLength)
}

#MELT, put all 100 means into 1 col and make notation of the dataset it came hence
KAZMelted.SC.means <- melt(Means.Here_KAZ, c( "KAZmean_sc_female20","KAZmean_sc_female19","KAZmean_sc_male19"), 
                           id=c() )

#clean up dm1 (there are a bunch of rows without useful information)
KAZMelted.SC.means <- KAZMelted.SC.means[(!is.na(KAZMelted.SC.means$value)),]
colnames(KAZMelted.SC.means) <- c("DataSet", "mean")

#plot the stuff
looky.here <- ggplot(KAZMelted.SC.means, aes(x= DataSet, y=mean))+geom_jitter()+geom_boxplot()+ggtitle("KAZ auto-biv data")

the.tree.means <- c(mean(KAZMelted.SC.means$KAZmean_sc_female20), mean(KAZMelted.SC.means$KAZmean_sc_female19), mean(KAZMelted.SC.means$KAZmean_sc_male19))
t.test(Means.Here$KAZmean_sc_female20, Means.Here$KAZmean_sc_female19)



#SKIVE  <- #LEW
#the script below mkes 1000 data sets 
SKIVEfemale.20.rand_BIG.DF.LIST <- replicate(1000,
                                           #add subsample to a list, this works!!!
                                           list( bind_rows(list(Curated_BivData[ sample( which(Curated_BivData$category=="SKIVE female"), 20), ]))
                                           ))
SKIVEfemale.19.rand_BIG.DF.LIST <- replicate(1000,
                                           #add subsample to a list, this works!!!
                                           list( bind_rows(list(Curated_BivData[ sample( which(Curated_BivData$category=="SKIVE female"), 19), ]))
                                           ))
SKIVEmale.19.rand_BIG.DF.LIST <- replicate(1000, 
                                         #add subsample to a list, this works!!!
                                         list( bind_rows(list(Curated_BivData[ sample( which(Curated_BivData$category=="SKIVE male"), 19), ]))
                                         ))

#1. make an empty DF list
#Loop through the list of DFs and calq the average SC
Means.Here_SKIVE <- data.frame(
  SKIVEmean_sc_female20 = rep(0, length(SKIVEfemale.20.rand_BIG.DF.LIST)),
  SKIVEmean_sc_female19 = rep(0, length(SKIVEfemale.19.rand_BIG.DF.LIST)),
  SKIVEmean_sc_male19 = rep(0, length(SKIVEmale.19.rand_BIG.DF.LIST))  )

#FILL in metrics
for(i in 1:length(Means.Here_SKIVE$SKIVEmean_sc_female20)){  #current data from
  print(i)
  #filling in metrics for Means.Here
  Means.Here_SKIVE$SKIVEmean_sc_female20[i] <- mean(SKIVEfemale.20.rand_BIG.DF.LIST[[i]]$chromosomeLength)#looks wacky, but this is right syntax
  Means.Here_SKIVE$SKIVEmean_sc_female19[i] <- mean(SKIVEfemale.19.rand_BIG.DF.LIST[[i]]$chromosomeLength)
  Means.Here_SKIVE$SKIVEmean_sc_male19[i] <- mean(SKIVEmale.19.rand_BIG.DF.LIST[[i]]$chromosomeLength)
}

#MELT, put all 100 means into 1 col and make notation of the dataset it came hence
SKIVEMelted.SC.means <- melt(Means.Here_SKIVE, c( "SKIVEmean_sc_female20","SKIVEmean_sc_female19","SKIVEmean_sc_male19"), 
                           id=c() )

#clean up dm1 (there are a bunch of rows without useful information)
SKIVEMelted.SC.means <- SKIVEMelted.SC.means[(!is.na(SKIVEMelted.SC.means$value)),]
colnames(SKIVEMelted.SC.means) <- c("DataSet", "mean")

#plot the stuff
looky.here <- ggplot(SKIVEMelted.SC.means, aes(x= DataSet, y=mean))+geom_jitter()+geom_boxplot()+ggtitle("SKIVE auto-biv data")

the.tree.means <- c(mean(SKIVEMelted.SC.means$SKIVEmean_sc_female20), mean(SKIVEMelted.SC.means$SKIVEmean_sc_female19), mean(SKIVEMelted.SC.means$SKIVEmean_sc_male19))
t.test(Means.Here$SKIVEmean_sc_female20, Means.Here$SKIVEmean_sc_female19)

