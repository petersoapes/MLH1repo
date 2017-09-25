```{r, echo=FALSE}
pp <- ggplot(passed_mice_df, aes(x = as.factor(mouse), y = adj_nMLH1.foci)) + geom_boxplot(data = passed_mice_df, aes(fill = factor(strain) ) )+ ggtitle("Passing mice") + theme(axis.title.x=element_blank(),
                                                                                                                                                                                 axis.text.x=element_blank(),
                                                                                                                                                                                 axis.ticks.x=element_blank()) + scale_fill_manual(values=c("#56B4E9", "cadetblue", "lightblue",
                                                                                                                                                                                                                                            "coral1", "#E69F00", "yellowgreen"))
passed_mice_plot <- pp + facet_wrap(~ sex, scales="free")

```
The passing mice plots, are not very different --- it might not be 


```{r, include=FALSE}

idorg <- c(1,2,3,4,5)
x <- c(14,20,21,16,17)
y <- c(31,21,20,50,13)

#dataset <- cbind (idorg,x,y)
#attempt <- list()
#for (i in 1:6)  {
#  attempt[[i]] <- dataset[sample(1:nrow(dataset), 3, replace=FALSE),]#
#
#}
#attempt[6][[1]][,'idorg']# 6th mini df, the column 'idorg'

# apply functions across dataframe for mice, #create a dataframe from the variables made #isolate data set for mice with enough cells (or find a way to code around the errors)

df_sample_size_test <-  ddply(MLH1_data, .(mouse), function(x) {
  #for each mouse in big data set, make samples of different sizes 10 times
  ncell <- length(unique(x$file.name))
  smpl10 <- ifelse(ncell < 5, 'NA',  mean( sample(x$adj_nMLH1.foci, ncell*0.1, replace=FALSE) ) )
  # smpl10x10 <- mean( sample(x$adj_nMLH1.foci, ncell*0.1, replace=FALSE) )
  #attempt = data.frame()
  #make a empty list, fill it with sample of 10 nMLH1 
  dataset <- data.frame(row.names=c("replicate"))
  attempt = list()
  for (i in 1:10)  {
    attempt[[i]] <- sample(x$adj_nMLH1.foci, ncell*0.1, replace=FALSE)
  }
  smple_10x10 = mean(attempt[1:10][1][,'adj_nMLH1.foci'])
  data.frame(ncell=ncell, mean10 = smpl10, s10x10 = smple_10x10)
})


dataset <- data.frame(col.names=c("replicate"))
attempt = list()

dataset <- data.frame(col.names=c("replicate"))
for (i in 1:10)  {
  print(i)
  #to get the list, index must be '[[i]]'                
  dataset[i] <-  sample.int(PWD_m_example$adj_nMLH1.foci, size=10, replace=FALSE)
}

smple_10x10 = mean(attempt[1:10][1][,'adj_nMLH1.foci'])

#attempt[6][[1]][,'idorg']# 6th mini df, the column 'idorg'

PWD_m_example = MLH1_data[MLH1_data$mouse == '10mar15_PWD_m2', ]

#dataset <- cbind (idorg,x,y)
# attempt = c()
#              for (i in 1:3)  {
#                  attempt[[i]] <- sample(PWD_m_example$nMLH1.foci, 10, replace=FALSE)
#              }
#              smple_10x10 = mean(attempt[1:10][[1]][,'nMLH1.foci'])#col isn't name ... don't need the name

#loop below simulates sampling 6 times, from the dataset. a mini df is made out of the samples
#
dataset <- cbind (idorg,x,y)
attempt <- list()
for (i in 1:6)  {
  attempt[[i]] <- dataset[sample(1:nrow(dataset), 3, replace=FALSE),]
  
}
attempt[6][[1]][,'idorg']# 6th mini df, the column 'idorg'

category_pwr_stat <- ddply(MLH1_data, "category", function(x) {
  Nmice <- length( unique(x$mouse) )
  mean.count <- mean(x$adj_nMLH1.foci)
  sd.count <- sd(x$adj_nMLH1.foci)
  cv <- sd.count/mnea.count
  
  ttest.p <- t.test(x$adj_nMLH1.foci, (20), alternative = "two", var.equal=TRUE)$p.value
  ttestCI.lowr <- t.test(x$adj_nMLH1.foci, (20), alternative = "two", var.equal=TRUE)$conf.int[1]
  ttestCI.upr <- t.test(x$adj_nMLH1.foci, (20), alternative = "two", var.equal=TRUE)$conf.int[2]
  ttest.est <- t.test(x$adj_nMLH1.foci, (20), alternative = "two", var.equal=TRUE)$estimate
  
  pwr5mice <- pwr.norm.test(d = (3/sd.count), n = 20, sig.level = 0.05, alternative = "two")$power
  
  data.frame(Nmice=Nmice, cv = cv, ttest=ttest.p, CIlower = ttestCI.lowr, 
             CIupr = ttestCI.upr,
             pwr = pwr5mice, ttest.est=ttest.est)
})



```


```{r}
#for the record, this is a way to make a table with pwr and t.test results
category_pwr_stat <- ddply(MLH1_data, "category", function(x) {
  Nmice <- length( unique(x$mouse) )
  mean.count <- mean(x$adj_nMLH1.foci)
  sd.count <- sd(x$adj_nMLH1.foci)
  cv <- sd.count/mean.count
  
  ttest <- t.test(x$adj_nMLH1.foci, (20), alternative = "two", var.equal=TRUE)$p.value
  ttestCI.lowr <- t.test(x$adj_nMLH1.foci, (20), alternative = "two", var.equal=TRUE)$conf.int[1]
  ttestCI.upr <- t.test(x$adj_nMLH1.foci, (20), alternative = "two", var.equal=TRUE)$conf.int[2]
  ttest.est <- t.test(x$adj_nMLH1.foci, (20), alternative = "two", var.equal=TRUE)$estimate
  
  pwr5mice <- pwr.norm.test(d = (3/sd.count), n = 20, sig.level = 0.05, alternative = "two")$power
  data.frame(Nmice=Nmice, cv.count = cv, ttest=ttest, CIlower = ttestCI.lowr, 
             CIupr = ttestCI.upr,
             pwr = pwr5mice, ttest.est=ttest.est)
})
#category_pwr_stat
```


```{r}
G_f_example = MLH1_data[MLH1_data$mouse == '17mar16_G_f1', ]

datf <- data.frame(
  MLH1_sample = c(sample(G_f_example$nMLH1.foci, 10, replace=FALSE),  sample(G_f_example$nMLH1.foci, 15, replace=FALSE),    sample(G_f_example$nMLH1.foci, 20, replace=FALSE), sample(G_f_example$nMLH1.foci, 30, replace=FALSE), sample(G_f_example$nMLH1.foci, 35, replace=FALSE) ),   
  samplesize = c(rep("10", 10), rep("15", 15), rep("20", 20), rep("30", 30), rep("35", 35)) )

v <- ggplot(data = datf, aes(x=samplesize, y=MLH1_sample)) + geom_boxplot(aes(fill=samplesize) ) 
#v                                                                         
#v + ggtitle("varying sample sizes") 
```
Can the above code be repeated for a larger number of mice (both passing and non-passing) to show the different effects across samples sizes?
power calculations.... from a preivous document --- I showed myself that for 15 (good) cells, would provide a sample big enough to detect a difference of 3 foci. I only did this for a couple mice, I'll try to re-code for every mouse



Diminishing return analysis. Take some of the mice identified above, simulate random sampling of 10,15,20,30. Display the raw mean and sd, and results from 10 simulations.
```{r, comparing sample sizes, echo=FALSE}
PWD_m_example = MLH1_data[MLH1_data$mouse == '10mar15_PWD_m2', ]

smp10dist = sample(PWD_m_example$nMLH1.foci, 10, replace=FALSE)
samplesize = rep("10", 10)
smp10 <- data.frame(smp10dist, samplesize)

smp15dist = sample(PWD_m_example$nMLH1.foci, 15, replace=FALSE)
samplesize = rep("15", 15)
smp15 <- data.frame(smp15dist, samplesize)

smp20dist = sample(PWD_m_example$nMLH1.foci, 20, replace=FALSE)
samplesize = rep("20", 20)
smp20 <- data.frame(smp20dist, samplesize)

smp30dist = sample(PWD_m_example$nMLH1.foci, 30, replace=FALSE)
samplesize = rep("30", 30)
smp10 <- data.frame(smp30dist, samplesize)


#this mouse has 40 cells 
#make a dataframe (so that I can finally make boxplots)
dat <- data.frame(
  MLH1_sample = c(sample(PWD_m_example$adj_nMLH1.foci, 10, replace=FALSE),  sample(PWD_m_example$adj_nMLH1.foci, 15, replace=FALSE),    sample(PWD_m_example$adj_nMLH1.foci, 20, replace=FALSE), sample(PWD_m_example$adj_nMLH1.foci, 30, replace=FALSE), sample(PWD_m_example$adj_nMLH1.foci, 35, replace=FALSE), sample(PWD_m_example$adj_nMLH1.foci, 40, replace=FALSE)),   
  samplesize = c(rep("10", 10), rep("15", 15), rep("20", 20), rep("30", 30), rep("35", 35), rep("40", 40)) )

bb <- ggplot(data = dat, aes(x=samplesize, y=MLH1_sample)) + #geom_boxplot(aes(fill=samplesize))
  
  ```
