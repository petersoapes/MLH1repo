# simulations for Polymorphism and Divergence
# input: RData of MLH1data
# output: plot of PandD values and simulations


library(plyr)
library(dplyr)
library(ggplot2)

setwd("C:/Users/alpeterson7/Documents/MLH1repo")
load(file="MLH1_data_setup.RData")

#bio questions, are the ratios of PandD of male and female measures more different than
#random. (permuted random)

# check assumptions of normality
# check comparisons of variance across hiarchy of levels

#1. calculate the variance measures for the strains
# SE, Var and cV 

#decide on the ddplyr tables to make
# seperate by sex, strain, then mouse?

PnD_female <- MLH1_data[MLH1_data$sex == "female",]
PnD_male <- MLH1_data[MLH1_data$sex == "male",]

################
# Calq Polymorphism #
################
#within subsp  variance

#consider writing a function for these...

Dom_f <- MLH1_data[( MLH1_data$strain == "WSB" | MLH1_data$strain == "G" |MLH1_data$strain == "LEWES"),]

F_poly_table <- ddply(PnD_female, c("strain"), summarise,
                     Nmice = length(unique(mouse)),
                     Ncells  = length(adj_nMLH1.foci),
                     mean_co = format(round(  mean(adj_nMLH1.foci), 3 ), nsmall=3),
                     var = format(round(   var(nMLH1.foci),3), nsmall=3),
                     sd   = round(sd(adj_nMLH1.foci), 3),
                     se   = round(sd / sqrt(Ncells), 3),
                     cV = round( (as.numeric(sd) / as.numeric(mean_co) ),3)
)
DomF_poly_var <- var(F_poly_table$mean_co[1:3])
DomF_poly_sd <- sd(F_poly_table$mean_co[1:3])
DomF_poly_se <- (DomF_poly_sd / sqrt(3) )

MuscF_poly_var <- var(F_poly_table$mean_co[2:5])
MuscF_poly_sd <- sd(F_poly_table$mean_co[2:5])
MuscF_poly_se <- (MuscF_poly_sd / sqrt(2))


M_poly_table <- ddply(PnD_male, c("strain"), summarise,
                      Nmice = length(unique(mouse)),
                      Ncells  = length(adj_nMLH1.foci),
                      mean_co = format(round(  mean(adj_nMLH1.foci), 3 ), nsmall=3),
                      var = format(round(   var(nMLH1.foci),3), nsmall=3),
                      sd   = round(sd(adj_nMLH1.foci), 3),
                      se   = round(sd / sqrt(Ncells), 3),
                      cV = round( (as.numeric(sd) / as.numeric(mean_co) ),3)
)

DomM_poly_var <- var(M_poly_table$mean_co[1:3])
DomM_poly_sd <- sd(M_poly_table$mean_co[1:3])
DomM_poly_se <- (DomM_poly_sd / sqrt(3))

MuscM_poly_var <- var(M_poly_table$mean_co[2:5])
MuscM_poly_sd <- sd(M_poly_table$mean_co[2:5])
MuscM_poly_se <- (MuscM_poly_sd / sqrt(2) )

CastM_poly_var <- var(M_poly_table$mean_co[6:7])
CastM_poly_sd <- sd(M_poly_table$mean_co[6:7])
CastM_poly_se <- (CastM_poly_sd / sqrt(2) )

###################
# Divergence Calq #
###################

#calq subsp wide averages
DomF_D_mean <- mean(as.numeric(F_poly_table$mean_co[1:3]) )
MuscF_D_mean <- mean(as.numeric(F_poly_table$mean_co[2:5]) )

DomM_D_mean <- mean(as.numeric(M_poly_table$mean_co[1:3]) )
MuscM_D_mean <- mean(as.numeric(M_poly_table$mean_co[2:5]) )
CastM_D_mean <- mean(as.numeric(M_poly_table$mean_co[6:7]) )

# divergence can only be calq in pairwise manner
# Musc - Dom, Musc - spretus(spic), Musc - caroli,
F_D_HMdm_se <- ( sd(c(DomF_D_mean, MuscF_D_mean)) / sqrt(2) )

M_D_HMdm_se <- ( sd(c(DomM_D_mean, MuscM_D_mean) ) / sqrt(2) )
M_D_HMdc_se <- ( sd(c(DomM_D_mean, CastM_D_mean) ) / sqrt(2) )
M_D_HMmc_se <- ( sd(c(CastM_D_mean, MuscM_D_mean) ) / sqrt(2) )


##########################
# pair P and D estimates #
##########################

#
plot( c( DomM_poly_se, DomM_poly_se), 
c( F_D_HMdm_se, M_D_HMdm_se) )



#
# Permutations / simulation? #
#

#3 calqs of SE from random samples
n3 <-  replicate(3, (sd(sample(Dom_f$nMLH1.foci, 30) ) / sqrt(30) ) )
#se of the simulated se's       
se_per <- sd( replicate(3, (sd(sample(Dom_f$nMLH1.foci, 30) ) / sqrt(30) ) ) ) / sqrt(3)

#100 simulated se's / P values for fDom
per100se <- replicate(1000,
                      sd( replicate(3, (sd(sample(Dom_f$nMLH1.foci, 30) ) / sqrt(30) ) ) ) / sqrt(3)
)
#this will show a hist of the simulated P values, with R line for the real
hist(per100se, xlim = range(0,.5) )
abline(v=DomM_poly_se,col="red")

