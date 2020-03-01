
#All of the Extra Mixed Models..




# Blank1

<!-- unused chunks -->
  
  <Below are examples of plots of SC length distributions across cells. The top figure shows whole cell hand measured data and the bottom shows the Automated bivData from cells with at least 15 bivalents measured. Most plots excluded for space.  

Each point is a bivalent plotted by cell on the x axis. X's are the 4th quartile, big point is the mean and smaller black point is the median. I'm using these to compare the patterns of these statistics in the automated data set which is missing some bivalent data. (the extra stats are not correctly mapped)>
  
  ```{r show.long.SC, echo=FALSE, eval=FALSE}

female_long_biv_plot <- ggplot(real.long.bivData[real.long.bivData$sex == "female",], aes(x=fileName, y=chromosomeLength, color=strain))+geom_point()+facet_wrap(~category, scales="free")+ylim(c(30,200))+ggtitle("Long Bivalent Dataset, \nFemale")+scale_color_manual(values=colors_of_strains)



male_long_biv_plot <- ggplot(real.long.bivData[real.long.bivData$sex == "male",], aes(x=fileName, y=chromosomeLength, color=strain))+geom_point()+facet_wrap(~category, scales="free")+ylim(c(30,200))+ggtitle("Long Bivalent Dataset, \nmale")+scale_color_manual(values=colors_of_strains)


```

<ABOVE plots show the SC lengths for the 'long SC data set'. They are supposed to be the longest 4-5 SC from cells where I could get good measures. These longer bivalents are useful because their patterns shouldn't be affect by chromosome size effect (which effects, CO position). Hopefully this data set will have less noise from chromosome identity, but there was still data missing (they don't come from whole cell measures).>
  
  
  
  <HAND measure METRICS, below chunk goes with the above chunk>
  ```{r long.biv.plots, echo=FALSE, eval=FALSE}

show.the.cells_females_dom <- ggplot(whole.cell.data_manual[(whole.cell.data_manual$sex == "female") & (whole.cell.data_manual$subsp == "Dom"),], aes(x=fileName, y= SC.length, color=strain))+geom_point()+
  
  geom_point(data=manual_summary_stats[(manual_summary_stats$sex == "female") &(manual_summary_stats$subsp == "Dom"),],aes(y=mean_SC, x=fileName, col="red", size=2))+
  geom_point(data=manual_summary_stats[(manual_summary_stats$sex == "female") & (manual_summary_stats$subsp == "Dom"),],aes(y=median_SC, x=fileName, col="black", size=1.5))+
  
  geom_point(data=manual_summary_stats[(manual_summary_stats$sex == "female") & (manual_summary_stats$subsp == "Dom"),],aes(y=Qar.4, x=fileName, col="black", size=2.6), shape=4)+
  
  scale_color_manual(values=colors_of_strains)+
  facet_wrap(~strain, scales="free")+ylim(c(30,200))+ggtitle("Female whole cell manual measures")


show.the.cells_females_dom
#PWD seems to have the biggest variance across cells


almost_cells_DOM_f <- ggplot(almost_whole_cell_BivData[(almost_whole_cell_BivData$sex == "female") & (almost_whole_cell_BivData$subsp == "Dom"),], aes(x=fileName, y= chromosomeLength, color=strain))+
  geom_point()+ 
  geom_point(data=Biv_Data_mean_table[(Biv_Data_mean_table$sex == "female") & (Biv_Data_mean_table$subsp == "Dom"),],aes(y=mean_SC, x=fileName, col="red", size=2))+
  geom_point(data=Biv_Data_mean_table[(Biv_Data_mean_table$sex == "female") & (Biv_Data_mean_table$subsp == "Dom"),],aes(y=median_SC, x=fileName, col="black", size=1.5))+
  geom_point(data=Biv_Data_mean_table[(Biv_Data_mean_table$sex == "female") & (Biv_Data_mean_table$subsp == "Dom"),],aes(y=Qar.4, x=fileName, col="black", size=2.6), shape=4)+
  scale_color_manual(values=colors_of_strains)+
  facet_wrap(~strain, scales="free")+ylim(c(30,200))+ggtitle("Dom female, almost whole cell \nMean, median, 4th Quartille ploted")
almost_cells_DOM_f


#### Musc female

show.the.cells_females_musc <- ggplot(whole.cell.data_manual[(whole.cell.data_manual$sex == "female") & (whole.cell.data_manual$subsp == "Musc"),], aes(x=fileName, y= SC.length, color=strain))+geom_point()+
  
  geom_point(data=manual_summary_stats[(manual_summary_stats$sex == "female") &(manual_summary_stats$subsp == "Musc"),],aes(y=mean_SC, x=fileName, col="red", size=2))+
  geom_point(data=manual_summary_stats[(manual_summary_stats$sex == "female") & (manual_summary_stats$subsp == "Musc"),],aes(y=median_SC, x=fileName, col="black", size=1.5))+
  
  geom_point(data=manual_summary_stats[(manual_summary_stats$sex == "female") & (manual_summary_stats$subsp == "Musc"),],aes(y=Qar.4, x=fileName, col="black", size=2.6), shape=4)+
  
  facet_wrap(~strain, scales="free")+ylim(c(30,200))+ggtitle("Female Musc whole cell manual measures")



almost_cells_Musc_f <- ggplot(almost_whole_cell_BivData[(almost_whole_cell_BivData$sex == "female") & (almost_whole_cell_BivData$subsp == "Musc"),], aes(x=fileName, y= chromosomeLength, color=strain))+
  
  geom_point()+ 
  geom_point(data=Biv_Data_mean_table[(Biv_Data_mean_table$sex == "female") & (Biv_Data_mean_table$subsp == "Musc"),],aes(y=mean_SC, x=fileName, col="red", size=2))+
  geom_point(data=Biv_Data_mean_table[(Biv_Data_mean_table$sex == "female") & (Biv_Data_mean_table$subsp == "Musc"),],aes(y=median_SC, x=fileName, col="black", size=1.5))+
  geom_point(data=Biv_Data_mean_table[(Biv_Data_mean_table$sex == "female") & (Biv_Data_mean_table$subsp == "Musc"),],aes(y=Qar.4, x=fileName, col="black", size=2.6), shape=4)+
  
  facet_wrap(~strain, scales="free")+ylim(c(30,200))+ggtitle("Musc female, almost whole cell \nMean, median, 4th Quartille ploted")



### Dom Male
show.the.cells_male_dom <- ggplot(whole.cell.data_manual[(whole.cell.data_manual$sex == "male") & (whole.cell.data_manual$subsp == "Dom"),], aes(x=fileName, y= SC.length, color=strain))+geom_point()+
  
  geom_point(data=manual_summary_stats[(manual_summary_stats$sex == "male") &(manual_summary_stats$subsp == "Dom"),],aes(y=mean_SC, x=fileName, col="red", size=2))+
  geom_point(data=manual_summary_stats[(manual_summary_stats$sex == "male") & (manual_summary_stats$subsp == "Dom"),],aes(y=median_SC, x=fileName, col="black", size=1.5))+
  
  geom_point(data=manual_summary_stats[(manual_summary_stats$sex == "male") & (manual_summary_stats$subsp == "Dom"),],aes(y=Qar.4, x=fileName, col="black", size=2.6), shape=4)+
  
  facet_wrap(~strain, scales="free")+ylim(c(30,150))+ggtitle("Male Dom whole cell manual measures")



almost_cells_Dom_m <- ggplot(almost_whole_cell_BivData[(almost_whole_cell_BivData$sex == "male") & (almost_whole_cell_BivData$subsp == "Dom"),], aes(x=fileName, y= chromosomeLength, color=strain))+
  
  geom_point()+ 
  geom_point(data=Biv_Data_mean_table[(Biv_Data_mean_table$sex == "male") & (Biv_Data_mean_table$subsp == "Dom"),],aes(y=mean_SC, x=fileName, col="red", size=2))+
  geom_point(data=Biv_Data_mean_table[(Biv_Data_mean_table$sex == "male") & (Biv_Data_mean_table$subsp == "Dom"),],aes(y=median_SC, x=fileName, col="black", size=1.5))+
  geom_point(data=Biv_Data_mean_table[(Biv_Data_mean_table$sex == "male") & (Biv_Data_mean_table$subsp == "Dom"),],aes(y=Qar.4, x=fileName, col="black", size=2.6), shape=4)+
  
  facet_wrap(~strain, scales="free")+ylim(c(30,150))+ggtitle("Dom male, almost whole cell \nMean, median, 4th Quartille ploted")



#MUSC male
show.the.cells_male_musc <- ggplot(whole.cell.data_manual[(whole.cell.data_manual$sex == "male") & (whole.cell.data_manual$subsp == "Musc"),], aes(x=fileName, y= SC.length, color=strain))+geom_point()+
  
  geom_point(data=manual_summary_stats[(manual_summary_stats$sex == "male") &(manual_summary_stats$subsp == "Musc"),],aes(y=mean_SC, x=fileName, col="red", size=2))+
  geom_point(data=manual_summary_stats[(manual_summary_stats$sex == "male") & (manual_summary_stats$subsp == "Musc"),],aes(y=median_SC, x=fileName, col="black", size=1.5))+
  
  geom_point(data=manual_summary_stats[(manual_summary_stats$sex == "male") & (manual_summary_stats$subsp == "Musc"),],aes(y=Qar.4, x=fileName, col="black", size=2.6), shape=4)+
  
  facet_wrap(~strain, scales="free")+ylim(c(30,150))+ggtitle("Musc Dom whole cell manual measures")



almost_cells_Musc_m <- ggplot(almost_whole_cell_BivData[(almost_whole_cell_BivData$sex == "male") & (almost_whole_cell_BivData$subsp == "Musc"),], aes(x=fileName, y= chromosomeLength, color=strain))+
  
  geom_point()+ 
  geom_point(data=Biv_Data_mean_table[(Biv_Data_mean_table$sex == "male") & (Biv_Data_mean_table$subsp == "Musc"),],aes(y=mean_SC, x=fileName, col="red", size=2))+
  geom_point(data=Biv_Data_mean_table[(Biv_Data_mean_table$sex == "male") & (Biv_Data_mean_table$subsp == "Musc"),],aes(y=median_SC, x=fileName, col="black", size=1.5))+
  geom_point(data=Biv_Data_mean_table[(Biv_Data_mean_table$sex == "male") & (Biv_Data_mean_table$subsp == "Musc"),],aes(y=Qar.4, x=fileName, col="black", size=2.6), shape=4)+
  
  facet_wrap(~strain, scales="free")+ylim(c(30,150))+ggtitle("Musc male, almost whole cell \nMean, median, 4th Quartille ploted")

#show.the.cells_male <- ggplot(whole.cell.data_manual[whole.cell.data_manual$sex == "male",], aes(x=fileName, y= SC.length, color=strain))+geom_point()+

#   geom_point(data=manual_summary_stats[manual_summary_stats$sex == "male",],aes(y=mean_SC, x=fileName, col="red", size=2))+

#  geom_point(data=manual_summary_stats[manual_summary_stats$sex == "male",],aes(y=median_SC, x=fileName, col="black", size=1.5))+

#   geom_point(data=manual_summary_stats[manual_summary_stats$sex == "male",],aes(y=Qar.4, x=fileName, col="black", size=2.6), shape=4)+

# facet_wrap(~strain, scales="free")+ylim(c(30,150))+ggtitle("Male whole cell manual measures")
#show.the.cells_male

#almost_whole_cell_BivData
#almost_whole_cell_wsb <- almost_whole_cell_BivData[almost_whole_cell_BivData$strain == "WSB",]
#almost_whole_cell_DOM <- almost_whole_cell_BivData[almost_whole_cell_BivData$subsp == "Dom",]
#almost_whole_cell_MUSC <- almost_whole_cell_BivData[almost_whole_cell_BivData$subsp == "Musc",]


#`r data.frame(table(droplevels(real.long.bivData$category)))`.  
```

<All the plots above show the distributions of manually whole cell measured SC lengths compared to the SC length distributions from the automated bivalent data. It shows the amount of within cell variance across strains. There is a bit of variance across the SC length distributions in the PWD females.
This data set might be noisy, given the amount of variance in the SC length distributions across cells (PWD females, WSB females).
The DF **real.long.bivData** contains `r length(real.long.bivData$Obj.ID)` bivalent measures. The full data set is `r length(Curated_BivData$Obj.ID)`. This is the breakdown of bivalent observations by category for this long dataset are 

**- Try to merge this DF with the whole.cell manual measures.**  
  - Try estimating which 'loose' bivalent observations might be within the long class of bivalents.  
in code chunk above I ran the mouse averages for the longest bivalents. (680 bivalents, from 54 mice.  10202 bivalents from 86 mice.>
                                                                           
                                                                           
                                                                           ```{r SC.distb_manual_Auto, echo=FALSE, eval=FALSE}
                                                                         
                                                                         #calq the mouse means
                                                                         manual_summary_stats <- ddply(whole.cell.data_manual, c("fileName"), summarise,
                                                                                                       
                                                                                                       mean_SC = as.numeric(format(round(  mean(SC.length, na.rm = TRUE), 3 ), nsmall=3) ),
                                                                                                       median_SC = median(SC.length, na.rm = TRUE),
                                                                                                       Qar.4 = quantile(SC.length, na.rm = TRUE)[[4]]
                                                                                                       #                        var_SC = format(round(   as.numeric(var(SC.length)),3), nsmall=3)
                                                                                                       
                                                                         )
                                                                         
                                                                         manual_summary_stats <- add_mouse(manual_summary_stats)
                                                                         manual_summary_stats <- add_strain(manual_summary_stats)
                                                                         manual_summary_stats <- add_sex(manual_summary_stats)
                                                                         manual_summary_stats <- add_subsp(manual_summary_stats)
                                                                         
                                                                         ###
                                                                         #calq the mouse means
                                                                         Biv_Data_mean_table <- ddply(almost_whole_cell_BivData, c("fileName"), summarise,
                                                                                                      
                                                                                                      mean_SC = as.numeric(format(round(  mean(chromosomeLength), 3 ), nsmall=3) ),
                                                                                                      median_SC = median(chromosomeLength),
                                                                                                      Qar.4 = quantile(chromosomeLength)[[4]],
                                                                                                      
                                                                                                      var_SC = format(round(   as.numeric(var(chromosomeLength)),3), nsmall=3)
                                                                                                      
                                                                         )
                                                                         Biv_Data_mean_table <- add_mouse(Biv_Data_mean_table)
                                                                         Biv_Data_mean_table <- add_strain(Biv_Data_mean_table)
                                                                         Biv_Data_mean_table <- add_sex(Biv_Data_mean_table)
                                                                         Biv_Data_mean_table <- add_subsp(Biv_Data_mean_table)
                                                                         
                                                                         #colors_of_strains
                                                                         #scale_color_manual(values=colors_of_strains)
                                                                         
                                                                         #un-order these factors
                                                                         ```
                                                                         
                                                                         
                                                                         <!-- Whole Cell Manual Comparison 
                                                                         
                                                                         ```{r XX.adj_manual.whole.cell, echo=FALSE, eval=FALSE}
                                                                         
                                                                         #load and examine the the whole cell hand measures here
                                                                         
                                                                         female.whole.cell <- whole.cell.data_manual[whole.cell.data_manual$sex == "female",]
                                                                         data.frame(table(droplevels(female.whole.cell$category)))
                                                                         
                                                                         # cell is 22nov16_13nov16_MSM_f2_sp1_19 is missing a biv
                                                                         
                                                                         
                                                                         #cell 5aug17_14jul17_LEW_f1_sp1_11_rev has broken SC - could be used to compare broken SC
                                                                         
                                                                         data.frame(table(droplevels(female.whole.cell$category), female.whole.cell$rank))
                                                                         #missing some strains, most have 3 cells (min). MSM has 5 full cells
                                                                         
                                                                         
                                                                         #remove the incomplete LEW female cell
                                                                         female.whole.cell <- female.whole.cell[female.whole.cell$fileName != "5aug17_14jul17_LEW_f1_sp1_11_rev",]
                                                                         
                                                                         female.whole.cell <- female.whole.cell[female.whole.cell$fileName != "22nov16_13nov16_MSM_f2_sp1_19",]
                                                                         
                                                                         #calq mean SC lengths by rank
                                                                         
                                                                         mean.SC_rank.strain <- ddply(female.whole.cell, c("strain", "rank"), summarise,
                                                                                                      
                                                                                                      mean.sc =   mean(SC.length, na.rm = TRUE),
                                                                                                      nbiv = length(Obj.ID),
                                                                                                      sd.SC_length   = sd(SC.length, na.rm = TRUE),
                                                                                                      se.SC_length   = sd.SC_length / sqrt(nbiv)
                                                                         )
                                                                         #   mean.SC_rank1 = mean(SC.length[which(rank == 20)], na.rm = 
                                                                         
                                                                         top.biv <- mean.SC_rank.strain[mean.SC_rank.strain$rank > 15,]
                                                                         
                                                                         
                                                                         show.SC_by.rank_top <- ggplot(data = top.biv, aes(x=strain, y=mean.sc, color = as.factor(rank)  )) +
                                                                           geom_pointrange(aes(ymin = mean.sc-(se.SC_length*2), ymax = mean.sc+(se.SC_length*2)), 
                                                                                           position=position_dodge(width=0.7), 
                                                                                           linetype='solid') +    theme_bw() + facet_wrap(~strain, scales="free")+
                                                                           ylim(c(71,210) )+ggtitle("mean SC Length by ranks \n2SE as line")
                                                                         
                                                                         show.SC_by.rank_top
                                                                         
                                                                         bottom.biv <- mean.SC_rank.strain[mean.SC_rank.strain$rank < 6,]
                                                                         
                                                                         show.SC_by.rank_bottom <- ggplot(data = bottom.biv, aes(x=strain, y=mean.sc, color = as.factor(rank)  )) +
                                                                           geom_pointrange(aes(ymin = mean.sc-(se.SC_length*2), ymax = mean.sc+(se.SC_length*2)), 
                                                                                           position=position_dodge(width=0.7), 
                                                                                           linetype='solid') +    theme_bw() + facet_wrap(~strain, scales="free")+
                                                                           ylim(c(35,99) )+ggtitle("Bottom Bivalents mean SC Length by ranks \n2SE as line")
                                                                         
                                                                         
                                                                         
                                                                         
                                                                         #CALQ PERCENT TOTAL SC
                                                                         
                                                                         CELL.SC.proportions <- ddply(female.whole.cell, c("fileName"), summarise,
                                                                                                      
                                                                                                      total.sc.cell =   sum(SC.length, na.rm = TRUE),
                                                                                                      nbiv = length(Obj.ID),
                                                                                                      #rank.by.per
                                                                                                      
                                                                                                      rank1_prop = (SC.length[which(rank == 20)] / total.sc.cell),
                                                                                                      rank2_prop = (SC.length[which(rank == 19)] / total.sc.cell),
                                                                                                      rank3_prop = (SC.length[which(rank == 18)] / total.sc.cell),
                                                                                                      rank4_prop = (SC.length[which(rank == 17)] / total.sc.cell),
                                                                                                      rank5_prop = (SC.length[which(rank == 16)] / total.sc.cell)
                                                                                                      #sd.SC_length   = sd(SC.length, na.rm = TRUE),
                                                                                                      #   se.SC_length   = sd.SC_length / sqrt(nbiv)
                                                                         )
                                                                         
                                                                         #MELT ABOVE
                                                                         MELT_Biv.SC_proporion <- melt(CELL.SC.proportions[,c( "fileName","total.sc.cell","nbiv",
                                                                                                                               #leave out the Foci
                                                                                                                               "rank1_prop","rank2_prop","rank3_prop","rank4_prop","rank5_prop")], 
                                                                                                       
                                                                                                       id=c( "fileName","total.sc.cell","nbiv"))
                                                                         
                                                                         MELT_Biv.SC_proporion <- MELT_Biv.SC_proporion[(!is.na(MELT_Biv.SC_proporion$value)),]
                                                                         
                                                                         MELT_Biv.SC_proporion <- add_mouse(MELT_Biv.SC_proporion)
                                                                         MELT_Biv.SC_proporion <- add_strain(MELT_Biv.SC_proporion)
                                                                         
                                                                         
                                                                         show_long.sc.proportions <- ggplot(data = MELT_Biv.SC_proporion, aes(y = value, x = fileName, fill=as.factor(variable)) ) + geom_col()+ggtitle("Breakdown of total SC per cell")+ylim(c(0,1)) +theme(axis.title.x=element_blank(),
                                                                                                                                                                                                                                                                              axis.text.x=element_blank(),
                                                                                                                                                                                                                                                                              axis.ticks.x=element_blank())+facet_wrap(~strain, scales = "free")
                                                                         
                                                                         show_long.sc.proportions
                                                                         
                                                                         
                                                                         #original plot code 
                                                                         #ggplot(data = df1, aes(x, y, color = Group)) +
                                                                         #    geom_pointrange(aes(ymin = lb, ymax = ub), 
                                                                         #                    position=position_jitter(width=0.5), 
                                                                         #                    linetype='dotted') +
                                                                         #    theme_bw()
                                                                         ```
                                                                         
                                                                         The plots above show the mean SC lengths and 2SE error bars for single bivalents which have been given within cell rank.
                                                                         
                                                                         The first plot showing the mean SC lengths by the rank (most all of these cells have 3, MSM has 5 cells (observations)). 
                                                                         
                                                                         The purpose of these plots is to display the variance of single bivalents when they are assigned a within cell rank. For the longest bivalents, XX is predicted to be the 3rd longest (according to physical length Mb).
                                                                         
                                                                         (use the value for the 3rd bivalent to adjust the single bivalent traits for XX -- then compare to males values -- or re-run in the MM).
                                                                         
                                                                         The other figure shows of each single bivalent contributes to the total SC area. Each column is a cell and each color is the percent of total SC area for the longest 5 bivalents in that cell. on average, each of the top longest bivalents make up ~10% of the cell's total SC area. So for cells all 20 bivalents, of it's total SC area, 5-7% is due to a XX, 
                                                                         
                                                                         - Is the difference between cell averages for males and females less that 10%?
                                                                         
                                                                         - also interesting, the pwd and msm don't have longer SC, compared to other strains.
                                                                         
                                                                         <!-- Automated BivData Comparison -->
                                                                         
                                                                         ```{r XX.Automated.biv.stuff, echo=FALSE}
                                                                         
                                                                         #make automated female data to compare to
                                                                         female.Cur.Bivdata <- Curated_BivData[(Curated_BivData$sex == "female"),]
                                                                         
                                                                         female.Bivdata_comp <- female.Cur.Bivdata[ ( (female.Cur.Bivdata$strain == "WSB") | (female.Cur.Bivdata$strain == "G") | (female.Cur.Bivdata$strain == "LEW") | (female.Cur.Bivdata$strain == "PWD")| (female.Cur.Bivdata$strain == "MSM") ),]
                                                                         
                                                                         #table(female.Bivdata_comp$strain)
                                                                         
                                                                         #female.Bivdata_comp
                                                                         
                                                                         
                                                                         #1. calq the rate of chrm segmentation in female categories in automateted data
                                                                         
                                                                         box.rates <- ddply(female.Bivdata_comp, c("fileName"), summarise,
                                                                         
                                                                         Nmice = length(unique(mouse)),
                                                                         mx.box.num = max(boxNumber), # need to run this at the cell level
                                                                         N.biv  = length(Obj.ID),
                                                                         pass.biv = N.biv / mx.box.num
                                                                         )
                                                                         box.rates <- add_mouse(box.rates)
                                                                         box.rates <- add_strain(box.rates)
                                                                         box.rates <- add_category(box.rates)
                                                                         
                                                                         cell.pass.rates <- ggplot(data = box.rates, aes(y=pass.biv, x=mouse, color=strain))+geom_jitter()+ggtitle("Auto biv pass rate per cell")+facet_wrap(~strain, scales="free")+ylim(c(0,1.1))+ theme(axis.title.x=element_blank(),
                                                                         axis.text.x=element_blank(),
                                                                         axis.ticks.x=element_blank())
                                                                         
                                                                         cell.pass.rates
                                                                         
                                                                         
                                                                         category.box.rates <- ddply(box.rates, c("category"), summarise,
                                                                         
                                                                         mean.pass.rate = mean(pass.biv)
                                                                         )
                                                                         #category.box.rates
                                                                         
                                                                         
                                                                         #2. pull the 3rd rank biv's calculate the SC means
                                                                         #calq means by rank (focus on the top 5)
                                                                         
                                                                         ```
                                                                         
                                                                         For the Automated data set, I like to measure the rate of passing bivalent per cell. The mean pass rate will be multiplied to the estimated XX mean_SC. 
                                                                         
                                                                         The table above shows the number of bivalents from the same strains as in the manual whole cell data. The plot shows the bivalent passing rate across all of the individual cells from this female data set. For each strain, I'll calculate the mean bivalent passing rate (maybe I should look at the mouse levels).  
                                                                         
                                                                         (some of the mice have different ranges of per cell passing rate) - given this ranges, i think the xx adjustment factor should be called on the mouse level. (it could even be extended to cell level -- except i don't think the XX SC length estimates wont be good.)

strain.XX.adjustment.factor = per_cell_passing rate \* 1 of 20 random biv will be XX \*  
  
  
  ** It might be simplier to compare the male and female means, and test it they are greater than the whole cell proprotion of the XX in females cells.**  The XX in a whole female cell contributes ~ 7% of total SC, if the female means for a type of total SC measure are from XX. But I am not using 'whole cell' summaries to compare female and males.


**What is the effect of an extra XX-autosome on single bivalent means?**
  
  use a permutation approach: Make a True data set to start with, same(similar) number of cells, mice and bivalents. Make fake data sets which sample 19 bivalents, for 'in silico' cells for males and females. Also Run cntrl-female data set, where 20 bivalents are sampled, but randomly. Run the same bivalent level summaries for each 'permuted data set'; male avSC, 19Female_avSC, and rand.20_Female_avSC.  The difference between the rand.20 and rand.19 female -permuted data sets should indicate the influence of having an extra 'XX-autosome' in the total data set. 


# Blank2
<!-- Note on Heterochiasmy Definition

<!-- add outline for new XX adjust section 
adjust the colors for the plot below (standardize)
-->
  
  I present heterochiasmy as a comparison of oocyte to spermatocyte MLH1 counts, but the sex chromosomes/bivalents complicate this comparison. In females the XX bivalent is indistinguishable from the autosomes. To the meiotic recombination machinery, it is an autosome and has a similar REC landscape. Whereas in spermatocytes the XY bivalent is visually distinct and any MLH1 where not included in the count). (I note if the and Y are paired, which they are at a high rate). The XY pair triggers a response to un-paired chromosomes and only has MLH1 foci within the PAR (the the tips of X and Y). To make a more equivalent comparison I will estimate which bivalent is the XX in oocytes, and subtract that average REC from the category average of each strain.

1. Compile full-cell data from females (all 20 bivalents measured)
2. Look at the SC length -ranked data, extract the 3rd longest estimate average REC for this bivalent, 
3. check how variable the REC is across the 1st,2nd,4th, and 5th are.

According to mouse genome website, the X is the 3rd largest chromosome by total amount of DNA (Mb).

(Put the XX adjustment section here)



There is now MOLF, which has female biased hetC
3 of my Musc strains have male biased patter; SKIVE, PWD and MSM.
1 of the musc strains has female biased heterochiasmy, KAZ. 


```{r mouse.specific.Qplot, echo=FALSE, message=FALSE, results='hide', warning=TRUE, include=FALSE, eval=FALSE}

#REMOVE This BLOCK?

#### Distributions by mouse
#There are code chunks that make a big dfs of CO count by quality score plot. The #code has been commented out, and moved to it's own doc.

#remove this block since it has it's own src/script

### to integrate 'pass'/fail
### this should be in setup data

#mice that don't have female data will break the chain-loop
#MOLF female, HMI, CZECH female, CAROLI female, Tom male

#instead of adding fake data, remove the categories from list

#filler_data_MOLF.f = c("BatchX", "1jan19_fake_MOLF_f1_sp1", "99999", "3","20","20","","","","","","1jan19_fake_MOLF_f1_sp1_rev.tif",
#                       "fake_MOLF_f1", "MOLF female", "MOLF","female","Musc", "20","")
#filler_data_HMI.f = c("BatchX", "1jan19_fake_HMI_f1_sp1", "99999", "3","20","20","","","","","","1jan19_fake_HMI_f1_sp1_rev.tif",
#                       "fake_HMI_f1", "HMI female", "HMI","female","Cast", "20","")
#filler_data_CZECH.f = c("BatchX", "1jan19_fake_CZECH_f1_sp1", "99999", "3","20","20","","","","","","1jan19_fake_CZECH_f1_sp1_rev.tif",
#                       "fake_CZECH_f1", "CZECH female", "CZECH","female","Musc", "20","")
#filler_data_CAROLI.f = c("BatchX", "1jan19_fake_CAROLI_f1_sp1", "99999", "3","20","20","","","","","","1jan19_fake_CAROLI_f1_sp1_rev.tif",
#                       "fake_CAROLI_f1", "CAROLI female", "CAROLI","female","Caroli", "20","")
#MLH1_data_table <- rbind(MLH1_data, filler_data_MOLF.f,filler_data_HMI.f,filler_data_CZECH.f,filler_data_CAROLI.f)#error from dataset thing

pass_mice <- c("28sep18_CZECH_m3","26oct18_MOLF_m1","22jun15_G_m2")


#this loop makes a quant_status col for a list of mice
for(i in 1:length(MLH1_data$mouse)){
  MLH1_data$quant_status[i] <- ifelse( is.element(MLH1_data$mouse[i], pass_mice), "pass", NA)
}


df_list <- split(MLH1_data, (MLH1_data$category))
#remove sections which are in category but have no data
#this is where you remove the non-data data!
df_list <- df_list[ -c(7,12,14,18,20,21,22,24,26,28,30) ]

#7 PERC, 12 MOLF female, 
#

#df list, is a list of df's which were split() from BIG df by category
df_list 
o=0
i=0

MLH1_data$quant_status <- as.factor(MLH1_data$quant_status)

#make a new empty df. create a ddply thing with mean calculations (this is like a list for matching up later)
for(i in 1:(length(df_list))){
  # only include data with quality values, then create a ddply for specific sub-df that calqs mean by each mouse
  df_list[[i]] <- df_list[[i]][!(is.na(df_list[[i]]$quality) | df_list[[i]]$quality==""), ]
  sub_mouse_means <- ddply(df_list[[i]], .(mouse), summarise,
                           mouse_nMLH1 = mean(adj_nMLH1.foci)
  )
  # this loop is going though each row of dataframes within the df.list and assigning the mouse mean
  for(o in 1:length(df_list[[i]]$Original.Name)){
    df_list[[i]]$mouse_mean_MLH1[o] <- sub_mouse_means$mouse_nMLH1[ (df_list[[i]]$mouse[o] == sub_mouse_means$mouse) ]
  }
}


plot_list = list()
for (h in 1:length(df_list)) {
  pf <- ggplot(data = df_list[[h]], aes(colour = factor(Batch)), width = 0.25)+ 
    geom_jitter(aes(y= adj_nMLH1.foci, x= quality)) + 
    geom_rect(data = df_list[[h]], aes(fill=as.factor(quant_status)), alpha=0.5, xmin=-100, xmax=100, ymin=-100, ymax=100)+
    facet_wrap(~ mouse, drop = TRUE) + 
    geom_hline(aes(yintercept = mouse_mean_MLH1), color="red") +scale_fill_manual(values=c(NA, "red"), breaks=NULL) +    geom_hline(yintercept = c( mean(df_list[[h]]$adj_nMLH1.foci) ), color="black" )
  #theme(plot.background = element_rect(fill = "green")) #theme for indiviual mice, 
  #make an empty ggplot? for missing data
  
  plot_list[[h]] =   pf
}

#orig (I think this is the same as above)
#plot_list = list()
#for (h in 1:length(df_list)) {
#  pf <- ggplot(df_list[[h]],(aes(y= adj_nMLH1.foci, x= quality)))
#  pf <- pf +geom_rect(data = df_list[[h]], aes(fill=quant_status), xmin=-100, xmax=100, ymin=-100, ymax=100, alpha=0.005)

#    pf <- pf + geom_jitter(width = 0.25, aes(colour = factor(Batch)) )
#  pf <- pf + geom_hline(yintercept = c( mean(df_list[[h]]$adj_nMLH1.foci) ), color="black" )
#  pf <- pf + facet_wrap(~ mouse) + geom_hline(aes(yintercept = mouse_mean_MLH1), color="red") + 

#   scale_fill_manual(values=alpha(c("red"), .005), breaks=NULL) +

#theme for indiviual mice, 
#theme(plot.background = element_rect(fill = "green"))
#  plot_list[[h]] =   pf
#}
#can't change the alpha/transparency of the red rec

#aes must be same length ---   #geom_point(stat = "identity") +
# the last one is correct....?

#so plot list is some wride dataframe, but indeces 1:12 in plot list are ggplots
```

```{r show.plots, echo=FALSE, message=FALSE,results='hide', warning=FALSE, include=FALSE, eval=FALSE}
geom_hline(aes(yintercept = med, group = gr), colour = 'red')
#no plot is made that goes in the list -- so it can't be shown
for (gg in 1:length(plot_list) ){
  show(plot_list[gg])
  # invisible(lapply(obj, function(x) plot(x,main="some plot")))
}

#this stops at MSM
```

The mouse specific scatter plots aren't show here because there are too bulky. These plots are in a different document.

Making all of these scatter plots, allows us to look at the whole distributions of the data for each mouse. The distance of the red line from the black could be a indicator of slides or mice with slide specific technical noise.

```{r HetC.plot22, echo=FALSE,  fig.width=10, fig.height=5, fig.caption="Sex-Specific MLH1 average", eval=FALSE}


no_female <- c("HMI", "SKIVE", "CZECH", "MOLF", "CAROLI", "TOM","AST")

MixedModel_CO_data <- AP_mouse_table_w.Ages[AP_mouse_table_w.Ages$strain.x != "other",]
CO_data_HetC_w.old <- AP_mouse_table_w.Ages[AP_mouse_table_w.Ages$strain.x != "other",]
CO_data_HetC_w.old <- CO_data_HetC_w.old[ ! CO_data_HetC_w.old$strain.x %in% no_female, ]
CO_data_HetC_w.old <- CO_data_HetC_w.old[!(is.na(CO_data_HetC_w.old$Nmice) | CO_data_HetC_w.old$Nmice==""), ]

MixedModel_CO_data <- MixedModel_CO_data[MixedModel_CO_data$age.weeks < 15,]

MixedModel_CO_data <- MixedModel_CO_data[!(is.na(MixedModel_CO_data$mouse) | MixedModel_CO_data$mouse==""), ]


MixedModel_CO_data <- MixedModel_CO_data[ ! MixedModel_CO_data$strain.x %in% no_female, ]

kuku <- ggplot(MixedModel_CO_data, aes(y=mean_co, x=mouse, color=sex.y))+geom_point()+facet_wrap(~strain.x)

bubu <- ggplot(data=CO_data_HetC_w.old)+geom_point(aes(y=mean_co, x=mouse, color=sex.x))+facet_wrap(~strain.x)

#add titles
#use multiplot
#remove X axis

#multiplot(kuku,bubu, cols=2)
```

```{r F1.by.F2, eval=FALSE}

#try remaking the plot Megan suggested
# for 2CO positions, Foci1, Position  on x and Foci 2 position on y

CurBivData_2CO <- Curated_BivData[Curated_BivData$hand.foci.count == 2,]

CurBivData_2CO <- CurBivData_2CO[!(is.na(CurBivData_2CO$Foci2) | CurBivData_2CO$Foci2==""), ]

#isolate 2COs
#facet by sex and subsp

F1.x.F2 <- ggplot(CurBivData_2CO, aes(x=Foci1,y=Foci2, color=strain) ) + geom_point()+ facet_wrap(~sex)+ggtitle("test plot")


```

<!-- Deleted notes -->

```{r variance.hist, echo=FALSE, eval=FALSE}
#figures of histograms, 

#make series for total dataset?
Var.Hist.total.mice <- ggplot(DF.HetC.MixedModel, aes(x = var, fill=sex)) + 
geom_histogram(binwidth = 1, alpha=.6)  +
ggtitle("Histograms of within mouse variance, all mice")
Var.Hist.total.mice <- Var.Hist.total.mice   + facet_wrap(~ strain, scales="free")
Var.Hist.total.mice

#DF.HetC.MixedModel.HQ
Var.Hist <- ggplot(DF.HetC.MixedModel.HQ, aes(x = var, fill=sex)) + 
geom_histogram(binwidth = 1, alpha=.6)  + 
ggtitle("Histograms of within mouse variance, 10 cell min")
Var.Hist <- Var.Hist   + facet_wrap(~ strain, scales="free")


Var.Hist.15cells <- ggplot(DF.HetC.MixedModel.HQ[DF.HetC.MixedModel.HQ$Ncells >14,], aes(x = var, fill=sex)) + 
geom_histogram(binwidth = 1, alpha=.6)  +   ggtitle("Histograms of within mouse variance, 15 cell min")
Var.Hist.15cells <- Var.Hist.15cells   + facet_wrap(~ strain, scales="free")



Q12.var.hist.plot <- ggplot(Q12_mouse_table_w.Ages, aes(x = var, fill=sex)) + 
geom_histogram(binwidth = 1, alpha=.6)  +   ggtitle("Histograms of within mouse variance, Q12")

Q12.var.hist.plot <- Q12.var.hist.plot   + facet_wrap(~ strain, scales="free")
Q12.var.hist.plot

#some LEW female still have v high within mouse variance (2 mice, )


```


-->



