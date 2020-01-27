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



#LEW




#PWD



#MSM


#KAZ


#SKIVE