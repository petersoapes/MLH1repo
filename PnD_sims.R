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

#####################
# Calq Polymorphism #
#####################
#within subsp  variance

Dom_m <- PnD_male[( PnD_male$strain == "WSB" | PnD_male$strain == "G" |PnD_male$strain == "LEWES"),]
Musc_m <- PnD_male[( PnD_male$strain == "PWD" | PnD_male$strain == "MSM"),]
Cast_m <- PnD_male[( PnD_male$strain == "CAST" | PnD_male$strain == "HMI"),]

#consider writing a function for these...
Dom_f <- MLH1_data[( MLH1_data$strain == "WSB" | MLH1_data$strain == "G" |MLH1_data$strain == "LEWES"),]
Musc_f <- MLH1_data[( MLH1_data$strain == "PWD" | MLH1_data$strain == "MSM"),]

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

#spret_mean <- mean(PnD_male$nMLH1.foci[(PnD_male$strain == "SPRET")] )
M_D_HMspret_se <- ( sd(c(DomM_D_mean, spret_mean) ) / sqrt(2) )

##############################
# Permutations / simulation? #
##############################

#3 calqs of SE from random samples
#Dom_f_poly <-  replicate(3, (sd(sample(Dom_f$nMLH1.foci, 30) ) / sqrt(30) ) )
#se of the simulated se's       
#se_per <- sd( replicate(3, (sd(sample(Dom_f$nMLH1.foci, 30) ) / sqrt(30) ) ) ) / sqrt(3)

#100 simulated se's / P values for fDom
Dom_f_poly_sim <- replicate(1000,
            sd( replicate(3, (sd(sample(Dom_f$nMLH1.foci, 30) ) / sqrt(30) ) ) ) / sqrt(3)
)
#this will show a hist of the simulated P values, with R line for the real
hist(per100se, xlim = range(0,.5) )
abline(v=DomM_poly_se,col="red")

Dom_m_poly_sim <- replicate(1000,
               sd( replicate(3, (sd(sample(Dom_m$nMLH1.foci, 30) ) / sqrt(30) ) ) ) / sqrt(3)
)
mean(Dom_m_poly_sim)

Dom_m_poly_sim_mn <- replicate(1000,
                mean( replicate(3, (sd(sample(Dom_m$nMLH1.foci, 30) ) / sqrt(30) ) ) )
)
#make a simPoly data set, then mean and sd can be calq from that

#DS from Dom samples 
Dom_m_poly_sim_ds <- replicate(100,
            replicate(3, (sample(Dom_m$nMLH1.foci, 30) ) )
)# 9000, MLH1 values


##################
# Divergence Sim #
##################
#use the previously made Poly
#sim is 1000 long
# var(mean(poly), set_spret)
spret_mean <- mean(PnD_male$nMLH1.foci[(PnD_male$strain == "SPRET")] )
Musc_mean <- mean(Musc_m$nMLH1.foci)

#simulated D
simDom_m_Musc.D <- sd( c(  mean(Dom_m_poly_sim_ds), Musc_mean)) / sqrt(2)
simDom_m_cast.D <-  sd( c(  mean(Dom_m_poly_sim_ds,  )) ) / sqrt(2)
simDom_m_spret.D <- sd( c(  mean(Dom_m_poly_sim_ds, spret_mean)) ) / sqrt(2)

#polymorph
# Dom_m_poly_sim_ds
sim_poly_dom_se <- sd(Dom_m_poly_sim_ds) / sqrt(9000)


#Cast_m --- sd( dom-cast means ) / sqrt(2)
plot(y=c(sim_poly_dom_se, sim_poly_dom_se, sim_poly_dom_se, #static poly for sims

          DomM_poly_se, DomM_poly_se, DomM_poly_se),  #real poly
   
    x=c(simDom_m_Musc.D,  simDom_m_cast.D,  simDom_m_spret.D, # sim D; simDom/real Musc, simDom/real Spret

        M_D_HMdm_se, M_D_HMdc_se, M_D_HMspret_se), #real D; (dom/musc, dom/cast, musc/cast)
      #real spret
     col=c("black","black","black","red","red","red"), #red- real, #black-fake

     xlab="Divergence, SE", ylab="Polymorphism, SE", main="Dom P and D comparisons")

#red is real data, black is sim (simulations for polymorphism, static musc and spret)
#merge the values -- that makes ploting easier
#low polymorphism for simulations

#I think my true comparison of D and P, has 
#real data has a higher level of polymorphism and low divergence (for Dom)?

DomM_poly_se <- (DomM_poly_sd / sqrt(3))
MuscM_poly_se <- (MuscM_poly_sd / sqrt(2) )
CastM_poly_se <- (CastM_poly_sd / sqrt(2) )
#
M_D_HMdm_se <- ( sd(c(DomM_D_mean, MuscM_D_mean) ) / sqrt(2) )
M_D_HMdc_se <- ( sd(c(DomM_D_mean, CastM_D_mean) ) / sqrt(2) )
M_D_HMmc_se <- ( sd(c(CastM_D_mean, MuscM_D_mean) ) / sqrt(2) )


##########################
# pair P and D estimates #
##########################
sample_size = 20 #samples across all mice (maybe should do mouse averages)

#1. make the poly sims for Dom.m and Dom.f
DomF_poly_sims <- replicate(100,sample(Dom_f$nMLH1.foci, sample_size) )
#1000*20 MLH1 measures
#x1 <- sapply(1:reps, function(i){sum(rexp(n=nexps, rate=rate))}))
# pair the DomF_poly_sims[i] with DomF_D_sim --> sd( c(mean(DomF_poly_sims[,i]), mean(cast) ) / sqrt(2)
#  mean(DomF_poly_sims[,i]) - this is mean for each sample
#2. 

simPnD_df = data.frame(Poly=as.numeric(c(1)), Div=as.numeric(c(1)))
for(i in 1:100 ){ #replicate
    simPnD_df[i,1] <- sd(DomF_poly_sims[,i])/sqrt(sample_size)
    simPnD_df[i,2] <- sd(c(mean(DomF_poly_sims[,i]), mean(Musc_f$nMLH1.foci) ) / sqrt(2) )
    print(c(i,  sd(DomF_poly_sims[,i])/sqrt(sample_size),  (sd(c(mean(DomF_poly_sims[,i]), mean(Musc_f$nMLH1.foci) ) / sqrt(2) ) ) ) )
}
# making a simD measure
#make -- the df or matrix with columns, 1) Polysim_SE 2) Divsim_SE

jj <- ggplot(data=simPnD_df, aes(x=Div, y=Poly))+
  geom_point(x=Div, y=Poly)
jj

#NEAT I made a plot that shows simulated and the real data
plot(c(DomF_poly_se,  simPnD_df$Poly), 
     c(F_D_HMdm_se, simPnD_df$Div), col=c("red", rep("black", length(simPnD_df$Poly)))
)


#male sims
DomM_poly_sims <- replicate(100,sample(Dom_m$nMLH1.foci, sample_size) )

#make a male version!
MsimPnD_df = data.frame(Poly=as.numeric(c(1)), Div=as.numeric(c(1)))
for(i in 1:100 ){ #replicate
  MsimPnD_df[i,1] <- sd(DomM_poly_sims[,i])/sqrt(sample_size)
  
  MsimPnD_df[i,2] <- sd(c(mean(DomM_poly_sims[,i]), mean(Musc_m$nMLH1.foci) ) / sqrt(2) )
  print(c(i,  sd(DomM_poly_sims[,i])/sqrt(sample_size),  
          (sd(c(mean(DomM_poly_sims[,i]), mean(Musc_m$nMLH1.foci) ) / sqrt(2) ) ) ) )
}


#male plot. (dom.male_poly,  dom/musc - male)
plot(c(DomM_poly_se,  MsimPnD_df$Poly), 
     c(M_D_HMdm_se, MsimPnD_df$Div), col=c("red", rep("black", length(MsimPnD_df$Poly)))
main="Dom male P and D simulation"
     )
#this plot

#par(1,2)

# For the Dom colm
#  sd( mean(DomF_poly_sims), mean(muscF_MLH1) ) / sqrt(2)
# sd( mean(DomM_poly_sims), mean(castM_MLH1) ) / sqrt(2)

per100se <- replicate(1000,
                      sd( replicate(3, (sd(sample(Musc_f$nMLH1.foci, 30) ) / sqrt(30) ) ) ) / sqrt(3)
)
#this will show a hist of the simulated P values, with R line for the real
hist(per100se, xlim = range(0,.5) )
abline(v=DomM_poly_se,col="red")

hist(male_musc_1000, xlim = range(0,1.7) )
abline(v=MuscM_poly_se,col="red")

par(mfrow=c(3, 1))

hist(male_musc_ss30, xlim = range(0,1.7) )
abline(v=mean(male_musc_ss30),col="blue")
abline(v=MuscM_poly_se,col="red")
hist(male_musc_ss15, xlim = range(0,1.7) )
abline(v=MuscM_poly_se,col="red")
hist(male_musc_ss10, xlim = range(0,1.7) )
abline(v=MuscM_poly_se,col="red")
#the variance of the 3 hist changes, sample 10 is larger.

#this shows the real means agaisnt histograms of the sampled Dom values
b <- hist(male_dom_ss10, xlim = range(10,40) )
b <- abline( v=mean( PnD_male$nMLH1.foci[(PnD_male$strain == "WSB")] ), col="red")
b <- abline( v=mean( PnD_male$nMLH1.foci[(PnD_male$strain == "G")] ), col="red")
b <- abline( v=mean( PnD_male$nMLH1.foci[(PnD_male$strain == "LEWES")] ), col="red")

ggplot()

G <- replicate(100, sample( PnD_male$nMLH1.foci[PnD_male$strain == "G"], 10) )
LEW <- replicate(100, sample( PnD_male$nMLH1.foci[PnD_male$strain == "LEWES"], 10) )
WSB <- replicate(100, sample( PnD_male$nMLH1.foci[PnD_male$strain == "WSB"], 10) )

combined = c(G, LEW, WSB)#3000
plt= ggplot(data.frame(data=c(combined, G, LEW, WSB), labels= rep(c("combined","G","LEW","WSB"), c(6000, 3000, 2000, 1000))),
        aes(x=data)) + stat_bin(aes(fill=labels), position="identity", binwidth=0.25, alpha=0.5) + theme_bw()
plt

G <-   replicate(100, sample( PnD_male$nMLH1.foci[PnD_male$strain == "G"], 20) )#179
LEW <- replicate(100, sample( PnD_male$nMLH1.foci[PnD_male$strain == "LEWES"], 20) )#190
WSB <- replicate(100, sample( PnD_male$nMLH1.foci[PnD_male$strain == "WSB"], 20) )#96
PWD <- replicate(100, sample( PnD_male$nMLH1.foci[PnD_male$strain == "PWD"], 20) )#162

#I guess overlapping the distributions is a way to visually tell how similar the distributions are to each other
#not sure what this shows, 
plt2= ggplot(data.frame(data=c(G, LEW, WSB, PWD), labels= rep(c("G","LEW","WSB","PWD"), c(8000,6000,4000,2000))),
            aes(x=data)) + stat_bin(aes(fill=labels), position="identity", binwidth=0.25, alpha=0.5) + theme_bw()
plt2
#adding PWD kinda shows a shift in the distribution


