library(plyr)
library(dplyr)
library(ggplot2)
library(reshape2)
setwd("C:/Users/alpeterson7/Documents/MLH1repo")
load(file="MLH1_data_setup.RData")

## ToDO change SE to Sd ##
# transform()  is the important function to use for permuting a column in a dataframe

# maked a nested linear model of MLH1 counts
# decide what is random and fixed, set the levels

###################
# Setting up Data #
###################

#remove data that might be low quality
#there's a Dom female 4apr15_WSB_f2 that could be skewing results
MLH1_data<- MLH1_data[!grepl("4apr15_WSB_f2", MLH1_data$mouse) , ] #1649
MLH1_data<- MLH1_data[!grepl("12sep16_MSM_f1", MLH1_data$mouse) , ]

MLH1_data<- MLH1_data[!grepl("4jan17_LEW_f1", MLH1_data$mouse) , ]#one duplicate image found
#SPRET females from batch5, are not that good
#leweS FEMALES also seem to have a high variance across means

#1. Create table of mouse level stats for MLH1 foci count
#already in Rdata file
#Mouse_table <- ddply(MLH1_data, c("strain", "sex", "mouse"), summarise,
#                      Nmice = length(unique(mouse)),
#                      Ncells  = length(adj_nMLH1.foci),
#                      mean_co = as.numeric(format(round(  mean(adj_nMLH1.foci), 3 ), nsmall=3) ),
#                      var = format(round(   var(adj_nMLH1.foci),3), nsmall=3),
#                      sd   = round(sd(adj_nMLH1.foci), 3),
#                      se   = round(sd / sqrt(Ncells), 3),
#                      cV = round( (as.numeric(sd) / as.numeric(mean_co) ),3)
#)
#add subsp to this table
#source("src/Func_addSubsp.R")
#Mouse_table <- add_subsp(Mouse_table)

# Divide up mouse table by Dom and Musc
# (use transform to randomize sex!! !)
#

dom_mouse_table <- AP_mouse_table[AP_mouse_table$subsp == "Dom", ]
musc_mouse_table <- AP_mouse_table[AP_mouse_table$subsp == "Musc", ]

#below is a mouse average table with sex randomized
rand_sex_dom_table <- transform(dom_mouse_table, sex = sample(sex) )
#Dom_per <- transform(Dom_Mouse_table, sex = sample(sex))

#permute this process many times.


################
# SIMULATIONS! #
################
# the simulation are a smaller set than the observed set then strain is randomized
# make plot with the pattern of subsampling without randomizing strain - then compare
# that to the strain randomized points.
# 1. Sample mouse means from the whole sex-pool with the same number of obs
# 2. Calq the Poly, (SE of mouse means)
# 3. Calq D, the variance sampled means

Nrep = 10000
##make female dataset to draw samples from

#make seperate dataframes, then merge them together
#Permut_Dommale = data.frame(Poly = as.numeric(c(1) ), Div = as.numeric(c(1)), sex=c("x"), subsp=c("x") )
Permut_Dommale = matrix(ncol=5, nrow=Nrep)
Permut_DomFemale= matrix(ncol=5, nrow=Nrep)
Permut_Mscmale= matrix(ncol=5, nrow=Nrep)
Permut_MscFemale= matrix(ncol=5, nrow=Nrep)
#loop that makes 
for(j in 1:Nrep){
  print(j)
  Tformed_Table <- transform(AP_mouse_table, sex = sample(sex))
  Rand_Dmale_means <- subset(Tformed_Table, subsp == "Dom" & sex == "male")$mean_co
  Rand_Dfemale_means <- subset(Tformed_Table, subsp == "Dom" & sex == "female")$mean_co
  
  Rand_Mmale_means <- subset(Tformed_Table, subsp == "Musc" & sex == "male")$mean_co
  Rand_Mfemale_means <- subset(Tformed_Table, subsp == "Musc" & sex == "female")$mean_co
  # poly, sex, subsp, div  
  Permut_Dommale[j,1] <- as.numeric(sd(Rand_Dmale_means) )#p
  Permut_Dommale[j,2] <-  abs(mean(Rand_Dmale_means) - mean(Rand_Mmale_means))
  Permut_Dommale[j,3] <-  "male" #why is this kept? it shouldn't matter for randomized data
  Permut_Dommale[j,4] <- "Dom"
  Permut_Dommale[j,5] <- "permuted"
#by having     
  Permut_DomFemale[j,1] <- as.numeric(sd(Rand_Dfemale_means))
  Permut_DomFemale[j,2] <-  abs(mean(Rand_Dfemale_means) - mean(Rand_Mfemale_means) )
  Permut_DomFemale[j,3] <-  "female"   
  Permut_DomFemale[j,4] <- "Dom"
  Permut_DomFemale[j,5] <- "permuted"
    
  Permut_Mscmale[j,1] <- as.numeric(sd(Rand_Mmale_means))
  Permut_Mscmale[j,2] <-   abs(mean(Rand_Dmale_means)- mean(Rand_Mmale_means))
  Permut_Mscmale[j,3] <-  "male"   
  Permut_Mscmale[j,4] <- "Musc"
  Permut_Mscmale[j,5] <- "permuted"

  Permut_MscFemale[j,1] <- as.numeric( sd(Rand_Mfemale_means) )
  Permut_MscFemale[j,2]<-   abs(mean(Rand_Dfemale_means)- mean(Rand_Mfemale_means) )
  Permut_MscFemale[j,3] <-  "female"
  Permut_MscFemale[j,4] <- "Musc"  
  Permut_MscFemale[j,5] <- "permuted"
}

colnames(Permut_Dommale) <- c("Poly", "Div", "sex", "subsp", "data")
colnames(Permut_DomFemale) <- c("Poly", "Div", "sex", "subsp", "data")
colnames(Permut_Mscmale) <- c("Poly", "Div", "sex", "subsp", "data")
colnames(Permut_MscFemale) <- c("Poly", "Div", "sex", "subsp", "data")

Perm_data <- rbind(Permut_Dommale,Permut_DomFemale,Permut_Mscmale,Permut_MscFemale)
Perm_data <- as.data.frame(Perm_data)
#
obsDom_m_P <- sd( subset(AP_mouse_table, sex=="male" & subsp == "Dom")$mean_co )

obsDom_f_P <- sd(subset(AP_mouse_table, sex=="female" & subsp == "Dom")$mean_co)

obsMusc_m_P <- sd(subset(AP_mouse_table, sex=="male" & subsp == "Musc")$mean_co)
obsMusc_f_P <- sd(subset(AP_mouse_table, sex=="female" & subsp == "Musc")$mean_co)

#Div is calq as the difference between means
obs_m_D <- abs( mean(subset(AP_mouse_table, sex=="male" & subsp == "Musc")$mean_co) -
                  mean(subset(AP_mouse_table, sex=="male" & subsp == "Dom")$mean_co) )

obs_f_D <- abs( mean(subset(AP_mouse_table, sex=="female" & subsp == "Musc")$mean_co) -
                  mean(subset(AP_mouse_table, sex=="female" & subsp == "Dom")$mean_co) )

obsDom_m <- c(obsDom_m_P, obs_m_D, "male", "Dom", "obs")
obsDom_f <- c(obsDom_f_P, obs_f_D, "female", "Dom", "obs")
obsMusc_m <- c(obsMusc_m_P, obs_m_D, "male", "Musc", "obs")
obsMusc_f <- c(obsMusc_f_P, obs_f_D, "female", "Musc", "obs")

obsData <- as.data.frame( rbind(obsDom_m,obsDom_f,obsMusc_m,obsMusc_f) )
colnames(obsData) <- c("Poly", "Div", "sex", "subsp", "data")

Perm_data <- rbind(Perm_data, obsData)

Perm_data$Poly <- as.character(Perm_data$Poly)
Perm_data$Div <- as.character(Perm_data$Div)

Perm_data$Poly <- as.numeric(Perm_data$Poly)
Perm_data$Div <- as.numeric(Perm_data$Div)

#save this ggplot

#perm table should re-worked .... so that P and D values are in one column
perm_plot <- ggplot(data = Perm_data, aes( y=as.numeric(Poly), x=as.numeric(Div), color=subsp) )+
  labs(x="Divergence\n abs difference between subsp means", y="Polymorphism\n sd(mouse averages)") +
  theme(  axis.title.y = element_text(size=15, face="bold")) +  #not working
  scale_x_continuous(limits=c(0,6) ) +
  scale_y_continuous(limits=c(.5,3.5) ) +
  geom_point( aes(shape=sex, fill=subsp, size=data), alpha = c(0.3) )+ theme_bw() +
  stat_ellipse(linetype = 1, level=0.95) + #default is 95%
  stat_ellipse(type="t", linetype = 1, level=0.9999) +
  scale_size_manual(values=c(1,3)) +
  scale_color_manual(values=c("#56B4E9", "#E69F00"))

perm_plot

dev.off()
#######################
# Save this framework #
#######################
#setwd("C:/Users/alpeterson7/Documents/MLH1repo")
#save.image("PnD_environment.RData")

