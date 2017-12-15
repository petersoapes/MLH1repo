library(plyr)
library(dplyr)
library(ggplot2)
setwd("C:/Users/alpeterson7/Documents/MLH1repo")
load(file="MLH1_data_setup.RData")

## ToDO change SE to Sd ##
# transform()  is the important function to use for permuting a column in a dataframe

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
Mouse_table <- ddply(MLH1_data, c("strain", "sex", "mouse"), summarise,
                      Nmice = length(unique(mouse)),
                      Ncells  = length(adj_nMLH1.foci),
                      mean_co = as.numeric(format(round(  mean(adj_nMLH1.foci), 3 ), nsmall=3) ),
                      var = format(round(   var(adj_nMLH1.foci),3), nsmall=3),
                      sd   = round(sd(adj_nMLH1.foci), 3),
                      se   = round(sd / sqrt(Ncells), 3),
                      cV = round( (as.numeric(sd) / as.numeric(mean_co) ),3)
)
#add subsp to this table
source("src/Func_addSubsp.R")
Mouse_table <- add_subsp(Mouse_table)

# Divide up mouse table by Dom and Musc
# (use transform to randomize sex!! !)
#

Dom_Mouse_table <- subset(Mouse_table, strain == "WSB" | strain == "G" | strain == "LEWES")
Musc_Mouse_table <- subset(Mouse_table, strain == "PWD" | strain == "MSM")
Cast_Mouse_table <- subset(Mouse_table, strain == "CAST" | strain == "HMI")



Q1_Mouse_table <- ddply(MLH1_data[MLH1_data$quality <= 2,], c("strain", "sex", "mouse"), summarise,
                     Nmice = length(unique(mouse)),
                     Ncells  = length(adj_nMLH1.foci),
                     mean_co = as.numeric(format(round(  mean(adj_nMLH1.foci), 3 ), nsmall=3) ),
                     var = format(round(   var(adj_nMLH1.foci),3), nsmall=3),
                     sd   = round(sd(adj_nMLH1.foci), 3),
                     se   = round(sd / sqrt(Ncells), 3),
                     cV = round( (as.numeric(sd) / as.numeric(mean_co) ),3)
)
#remove Na's  

#add subspecies
Q1_Mouse_table <- add_subsp(Q1_Mouse_table)

#(strain table needed for Poly calq)

#calq'd from mouse averages
Strain_table <- ddply(Mouse_table, c("strain", "sex"), summarise,
                            Nmice = length(unique(mouse)),
                            Ncells  = sum(Ncells),
                            strain.mean = format( round(  mean(mean_co), 3 ), nsmall=3),
                            strain.var = round( var(mean_co), 3),
                            strain.sd   = round( sd(mean_co), 3),
#decide if se denominator is n mice or ncells                       
                       strain.se1   = round( strain.sd / sqrt(Nmice), 3),
                       strain.se2   = round( strain.sd / sqrt(Ncells), 3),
                    cV = round( (as.numeric(strain.sd) / as.numeric(strain.mean) ),3)
)

#fewer mice as the denominator results in a large SE estimate...
# DON"T mix levels 


#2. subset by subsp
Dom <- MLH1_data[( MLH1_data$strain == "WSB" | MLH1_data$strain == "G" |MLH1_data$strain == "LEWES"),]
Musc <- MLH1_data[( MLH1_data$strain == "PWD" | MLH1_data$strain == "MSM"),]
#number of mice
Dom_Mouse_table <- subset(Mouse_table, strain == "WSB" | strain == "G" | strain == "LEWES")
Musc_Mouse_table <- subset(Mouse_table, strain == "PWD" | strain == "MSM")
Cast_Mouse_table <- subset(Mouse_table, strain == "CAST" | strain == "HMI")

nDomF_obs <- length(filter(Dom_Mouse_table, sex=="female")$mouse)
nDomM_obs<- length(filter(Dom_Mouse_table, sex=="male")$mouse)
nMuscF_obs<- length(filter(Musc_Mouse_table, sex=="female")$mouse)
nMuscM_obs<- length(filter(Musc_Mouse_table, sex=="male")$mouse)

#3. Calq the observed Polymorphism (SE)
Dom_strain_table <- subset(Strain_table, strain == "WSB" | strain == "G" | strain == "LEWES")
Musc_strain_table <- subset(Strain_table, strain == "PWD" | strain == "MSM")
Cast_strain_table <- subset(Strain_table, strain == "CAST" | strain == "HMI")

nDomM_strains = length(unique(filter(Dom_Mouse_table, sex=="male")$strain) )
nDomF_strains = length(unique(filter(Dom_Mouse_table, sex=="female")$strain) )
nMuscM_strains = length(unique(filter(Musc_Mouse_table, sex=="male")$strain) )
nMuscF_strains = length(unique(filter(Musc_Mouse_table, sex=="female")$strain) )

#########
# FOR P #
#########
#these are from strains
DomF_sd_of_means <- sapply(subset(Dom_strain_table, sex=="female", select= strain.mean), sd)
names(DomF_sd_of_means) <- NULL
DomF_se_of_means <- (DomF_sd_of_means / sqrt(nDomF_strains) )
DomF_var_means <- sapply(subset(Musc_strain_table, sex=="male", select= strain.mean), var)
names(DomF_var_means) <- NULL

DomM_sd_of_means <- sapply(subset(Dom_strain_table, sex=="male", select= strain.mean), sd)
names(DomM_sd_of_means) <- NULL
DomM_se_of_means <- (DomM_sd_of_means / sqrt(nDomM_strains) )
DomM_var_means<- sapply(subset(Dom_strain_table, sex=="male", select= strain.mean), var)
names(DomM_var_means) <- NULL

MuscF_sd_of_means <- sapply(subset(Musc_strain_table, sex=="female", select= strain.mean), sd)
names(MuscF_sd_of_means) <- NULL
MuscF_se_of_means <- (MuscF_sd_of_means / sqrt(nMuscF_strains) )
MuscF_var_means <- sapply(subset(Musc_strain_table, sex=="male", select= strain.mean), var)
names(MuscF_var_means) <- NULL

MuscM_sd_of_means <- sapply(subset(Musc_strain_table, sex=="male", select= strain.mean), sd)
names(MuscM_sd_of_means) <- NULL
MuscM_se_of_means <- (MuscM_sd_of_means / sqrt(nMuscM_strains) )
MuscM_var_means <- sapply(subset(Musc_strain_table, sex=="male", select= strain.mean), var)
names(MuscM_var_means) <- NULL

#make a poly table
#use SD instead of SE
Polymorphism_DF <- data.frame(subsp=c("Dom","Dom","Musc","Musc"),
            n.strains = c(nDomF_strains, nDomM_strains,nMuscF_strains,nMuscM_strains),      
            n.mouse.obs = c(nDomF_obs,nDomM_obs,nMuscF_obs,nMuscM_obs),
              sex= c("female", "male", "female", "male"),
            SD.means = c(DomF_sd_of_means,DomM_sd_of_means, MuscF_sd_of_means, MuscM_sd_of_means),
            SE.means= c(DomF_se_of_means, DomM_se_of_means, MuscF_se_of_means, MuscM_se_of_means))

Polymorphism_DF$SD.means <- as.character(Polymorphism_DF$SD.means)
Polymorphism_DF$SD.means <- as.numeric(Polymorphism_DF$SD.means)

#4. Calq observed Divergence variance levels
##############
# Divergence #
##############
#  HM: Dom/Musc, Musc/Cast, Cast/Dom.
#  Mus: Dom/Spret, Musc/Spret

#calq SE of strain means across subspecies
DomF_sbsp_mean <- mean(as.numeric(subset(Dom_strain_table, sex=="female")$strain.mean) ) #mean( strain means)
DomM_sbsp_mean <- mean(as.numeric(subset(Dom_strain_table, sex=="male")$strain.mean) )

MuscF_sbsp_mean <- mean(as.numeric(subset(Musc_strain_table, sex=="female")$strain.mean) )
MuscM_sbsp_mean <- mean(as.numeric(subset(Musc_strain_table, sex=="male")$strain.mean) )

CastM_sbsp_mean <- mean(as.numeric(subset(Cast_strain_table, sex=="male")$strain.mean) )

HouseMusF_sp_mean <- mean(c(DomF_sbsp_mean,MuscF_sbsp_mean)) #female HM (missing cast)
HouseMusM_sp_mean <- mean(c(DomM_sbsp_mean,MuscM_sbsp_mean,CastM_sbsp_mean))
SpretM_sp_mean <- mean(as.numeric(subset(Strain_table, strain=="SPRET")$strain.mean) ) #not spret female measure yet

# divergence SE, calq in pairwise manner
# Musc - Dom, Musc - spretus(spic), Musc - caroli,
#Div_FeDom.Musc_se <- ( sd(c(DomF_sbsp_mean, MuscF_sbsp_mean)) / sqrt(2) )
Div_FeDom.Musc_sd <- sd(c(DomF_sbsp_mean, MuscF_sbsp_mean))

#some of these should be the same
#Div_MaMusc.Dom_se <- ( sd(c(DomM_sbsp_mean, MuscM_sbsp_mean)) / sqrt(2) )
#Div_MaDom.Musc_se <- ( sd(c(MuscM_sbsp_mean, DomM_sbsp_mean)) / sqrt(2) )
#Div_MaDom.Cast_se <- ( sd(c(DomM_sbsp_mean, CastM_sbsp_mean)) / sqrt(2) )
#Div_MaMusc.Cast_se <- ( sd(c(MuscM_sbsp_mean, CastM_sbsp_mean)) / sqrt(2) )

Div_MaMusc.Dom_sd <- sd(c(DomM_sbsp_mean, MuscM_sbsp_mean))
Div_MaDom.Musc_sd <- sd(c(MuscM_sbsp_mean, DomM_sbsp_mean))
Div_MaDom.Cast_sd <- sd(c(DomM_sbsp_mean, CastM_sbsp_mean))
Div_MaMusc.Cast_sd <- sd(c(MuscM_sbsp_mean, CastM_sbsp_mean))

#species divergence, house mouse mean and spretus mean
Div_MaHM.Spret_se <- sd(c(HouseMusM_sp_mean, SpretM_sp_mean)) / sqrt(2)
Div_MaHM.Spret_sd <- sd(c(HouseMusM_sp_mean, SpretM_sp_mean))
#Div_FeHM.Spret_se <- sd(c(HouseMusF_sp_mean, SpretF_sp_mean)) / sqrt(2)

#make a Divergence table # the full table should be 12 obs long
Divergence_DF <- data.frame(
                  sex= c("female", "male", "female", "male","female", "male","female", "male","female", "male"),
                  Div_pair = c("Dom/Musc","Dom/Musc","Musc/Cast","Musc/Cast","Dom/Cast","Dom/Cast",
                               "HM/SPRET","HM/SPRET","HM/SPIC","HM/SPIC"
                               ),
                  SD= c(Div_FeDom.Musc_sd,Div_MaDom.Musc_sd,
                        "NA",Div_MaMusc.Cast_sd,
                        "NA",Div_MaDom.Cast_sd,
                        "NA", Div_MaHM.Spret_sd,
                        "NA","NA"))

Divergence_DF$SE <- as.character(Divergence_DF$SE)
Divergence_DF$SE <- as.numeric(Divergence_DF$SE)

################
# SIMULATIONS! #
################
# the simulation are a smaller set than the observed set then strain is randomized
# make plot with the pattern of subsampling without randomizing strain - then compare
# that to the strain randomized points.
# 1. Sample mouse means from the whole sex-pool with the same number of obs
# 2. Calq the Poly, (SE of mouse means)
# 3. Calq D, the variance sampled means

# i think sims should
# (draw from the same strains. .... but randomize sex)


# the simulation should be varying just sex (should have strain pooled data)
# <question is, if you pick 50 random mice, from the same subspecies randomizing sex,
# what are the ranges of Polymorphism and Divergence?
#
Nrep = 10000

##make female dataset to draw samples from

#this is how to permute the sex label in the mouse average table!! yayay!
Dom_per <- transform(Dom_Mouse_table, sex = sample(sex))

#dom subsp str
# mean(   mean(mouse_means[1:6]), mean(mouse_means[7:14]), mean(mouse_means[15:16]) )
Rand_Dom = data.frame(smp_mean=as.numeric(c(1)), Poly=as.numeric(c(1)), Div.Dom_Musc=as.numeric(c(1)), Subsp = c(1))
for(i in 1:Nrep ){ #replicate -- choose sample, then put in df
  Dommouse_means <- sample(Dom_Mouse_table$mean_co, 15) #sample mouse
  Muscmouse_means <- sample(Musc_Mouse_table$mean_co, 15) #create random Musc data
  Rand_Dom[i,1] <- mean(as.numeric(Dommouse_means))
  Rand_Dom[i,2] <- sd(  as.numeric(Dommouse_means))
#Div  
  Rand_Dom[i,3] = sd( Muscmouse_means, Dommouse_means)#don't think this calculation is right
  Rand_Dom[i,4] <- "Rand-Dom"
}
#Musc_
#musc data str
rm(Muscmouse_means,Dommouse_means)

Rand_Musc = data.frame(smp_mean=as.numeric(c(1)),Poly=as.numeric(c(1)), Div.Dom_Musc=as.numeric(c(1)), Subsp = c(1))
for(i in 1:Nrep ){ #replicate -- choose sample, then put in df
  Muscmouse_means <- sample(Musc_Mouse_table$mean_co, 15)
  Dommouse_means <- sample(Dom_Mouse_table$mean_co, 15)
  
  Rand_Musc[i,1] <- mean(as.numeric(Muscmouse_means))
  Rand_Musc[i,2] <- sd(as.numeric(Muscmouse_means))
  
  Rand_Musc[i,3] <- sd( Dommouse_means, Muscmouse_means) #switching the order, helped
  Rand_Musc[i,4] <- "Rand-Musc"
}
# divergence has to be changed... right now it's just looking at sd of random Dom to 
# Musc_F. I need to make a random alternate
# I think I need to draw both


#simulations for Male .. when sampling size is the same, the subspecies samples, still diverge.. but less so
#they seem like mirror images


#Make plot
png('PnD_poold_sim_sex_rand.png')

#
Full_sim <- rbind(Rand_Dom, Rand_Musc)#rbind females
#Dom.F
obs_M <-  data.frame(smp_mean=c("NA","NA", "NA", "NA"),
  Poly = c(Polymorphism_DF$SD.means[2],Polymorphism_DF$SD.means[4], #Dom   #musc
           Polymorphism_DF$SD.means[4],Polymorphism_DF$SD.means[2]), #musc #dom
   
   Div.Dom_Musc = c(Divergence_DF$SD[2],Divergence_DF$SD[2],  #dom-musc
                     Divergence_DF$SD[4], #musc- cast
                     Divergence_DF$SD[6] #dom-cast - remove musc point
                     ),
    Subsp = c("obsDomM", "obsMuscM", "obsMuscM", "obsDomM")) #Poly, Div.Dom_Musc, Subsp

obs_F <-  data.frame(smp_mean = c("NA", "NA"), Poly = c(Polymorphism_DF$SD.means[1],Polymorphism_DF$SD.means[3] ),
                     Div.Dom_Musc = c(Divergence_DF$SD[1],Divergence_DF$SD[1]),
                     Subsp = c("obsDomF", "obsMuscF")) #Poly, Div.Dom_Musc, Subsp

Full_sim <- rbind(Full_sim, obs_M, obs_F)

#make axis titles bigger
mumu <- ggplot(data = Full_sim, aes(x=Div.Dom_Musc, y=Poly, fill=Subsp, color=Subsp))+
  labs(x="Divergence", y= "Polymorphism") +
  theme(
    axis.title.y = element_text(size=15, face="bold")) +  #not working
  ylim(0,5)+xlim(0,8)+geom_point(aes(shape=Subsp, size=Subsp) )+theme_bw()  + 
  scale_colour_manual(values = c("#56B4E9", "#56B4E9", "#E69F00", "#E69F00",
                   "lightblue", "red"))  +
  scale_size_manual(values=c(4,4,4,4,2,2))+
  scale_shape_manual(values=c(16,17,16,17,1,1))
  
mumu

dev.off()


#######################
# Save this framework #
#######################
setwd("C:/Users/alpeterson7/Documents/MLH1repo")
save.image("PnD_environment.RData")

