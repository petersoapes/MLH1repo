library(plyr)
library(dplyr)
library(ggplot2)
setwd("C:/Users/alpeterson7/Documents/MLH1repo")
load(file="MLH1_data_setup.RData")

###################
# Setting up Data #
###################
#this WSB female mouse is likely skweing the data -- few cells
#there's a Dom female 4apr15_WSB_f2 that could be skewing results
MLH1_data<- MLH1_data[!grepl("4apr15_WSB_f2", MLH1_data$mouse) , ] #1649
MLH1_data<- MLH1_data[!grepl("12sep16_MSM_f1", MLH1_data$mouse) , ]

MLH1_data<- MLH1_data[!grepl("4jan17_LEW_f1", MLH1_data$mouse) , ]#one duplicate image found
#SPRET females from batch5, are not that good
#
#leweS FEMALES also seem to have a high variance across means

#1. subset data by sex
Mouse_table <- ddply(MLH1_data, c("strain", "sex", "mouse"), summarise,
                      Nmice = length(unique(mouse)),
                      Ncells  = length(adj_nMLH1.foci),
                      mean_co = as.numeric(format(round(  mean(adj_nMLH1.foci), 3 ), nsmall=3) ),
                      var = format(round(   var(adj_nMLH1.foci),3), nsmall=3),
                      sd   = round(sd(adj_nMLH1.foci), 3),
                      se   = round(sd / sqrt(Ncells), 3),
                      cV = round( (as.numeric(sd) / as.numeric(mean_co) ),3)
)
#add subsp to this df

source("src/Func_addSubsp.R")
Mouse_table <- add_subsp(Mouse_table)

#<change from cal from full MLh1-data
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

#why is the full data set refered?
#i don't think these are used
#PnD_female <- MLH1_data[MLH1_data$sex == "female",]
#PnD_male <- MLH1_data[MLH1_data$sex == "male",]
#nFemale_obs <- length(PnD_female$Batch)
#nMale_obs <- length(PnD_male$Batch)

#2. subset by subsp

#don't 
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
Polymorphism_DF <- data.frame(subsp=c("Dom","Dom","Musc","Musc"),
            n.strains = c(nDomF_strains, nDomM_strains,nMuscF_strains,nMuscM_strains),      
            n.mouse.obs = c(nDomF_obs,nDomM_obs,nMuscF_obs,nMuscM_obs),
              sex= c("female", "male", "female", "male"),
            SE.means= c(DomF_se_of_means, DomM_se_of_means, MuscF_se_of_means, MuscM_se_of_means))

Polymorphism_DF$SE.means <- as.character(Polymorphism_DF$SE.means)
Polymorphism_DF$SE.means <- as.numeric(Polymorphism_DF$SE.means)


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
Div_FeDom.Musc_se <- ( sd(c(DomF_sbsp_mean, MuscF_sbsp_mean)) / sqrt(2) )

#some of these should be the same
Div_MaMusc.Dom_se <- ( sd(c(DomM_sbsp_mean, MuscM_sbsp_mean)) / sqrt(2) )
Div_MaDom.Musc_se <- ( sd(c(MuscM_sbsp_mean, DomM_sbsp_mean)) / sqrt(2) )
Div_MaDom.Cast_se <- ( sd(c(DomM_sbsp_mean, CastM_sbsp_mean)) / sqrt(2) )
Div_MaMusc.Cast_se <- ( sd(c(MuscM_sbsp_mean, CastM_sbsp_mean)) / sqrt(2) )

#species divergence, house mouse mean and spretus mean
Div_MaHM.Spret_se <- sd(c(HouseMusM_sp_mean, SpretM_sp_mean)) / sqrt(2)
#Div_FeHM.Spret_se <- sd(c(HouseMusF_sp_mean, SpretF_sp_mean)) / sqrt(2)

#make a Divergence table # the full table should be 12 obs long
Divergence_DF <- data.frame(
                  sex= c("female", "male", "female", "male","female", "male","female", "male","female", "male"),
                  Div_pair = c("Dom/Musc","Dom/Musc","Musc/Cast","Musc/Cast","Dom/Cast","Dom/Cast",
                               "HM/SPRET","HM/SPRET","HM/SPIC","HM/SPIC"
                               ),
                  SE= c(Div_FeDom.Musc_se,Div_MaDom.Musc_se,
                        "NA",Div_MaMusc.Cast_se,
                        "NA",Div_MaDom.Cast_se,
                        "NA", Div_MaHM.Spret_se,
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
Nrep = 5000

##make female dataset to draw samples from
PnD_female <- Mouse_table[Mouse_table$sex == "female",] #sample from within 
PnD_female<- PnD_female[!grepl("SPRET", PnD_female$strain) , ]

PnD_male <- Mouse_table[Mouse_table$sex == "male",]

#remove non-HM from males
PnD_male<- PnD_male[!grepl("SPRET", PnD_male$strain) , ]
#but cast, mice means can still be drawn


#Dom_F
#dom subsp str
# mean(   mean(mouse_means[1:6]), mean(mouse_means[7:14]), mean(mouse_means[15:16]) )

Rand_Dom_F = data.frame(smp_mean=as.numeric(c(1)), Poly=as.numeric(c(1)), Div.Dom_Musc=as.numeric(c(1)), Subsp = c(1))
for(i in 1:Nrep ){ #replicate -- choose sample, then put in df
  mouse_means <- sample(PnD_female$mean_co, nDomF_obs) #sample mouse
  Rand_Dom_F[i,1] <- mean(as.numeric(mouse_means))
    Rand_Dom_F[i,2] <- sd(  as.numeric(mouse_means))/sqrt(nDomF_obs)
#Div  
  Rand_Dom_F[i,3] = sd( c(  mean(   mean(mouse_means[1:6]), mean(mouse_means[7:13]), mean(mouse_means[14:16]) ),
                            MuscF_sbsp_mean ) ) / sqrt(2)
  Rand_Dom_F[i,4] <- "Rand-Dom-Female"
}

#Musc_F
#musc data str
Rand_Musc_F = data.frame(smp_mean=as.numeric(c(1)),Poly=as.numeric(c(1)), Div.Dom_Musc=as.numeric(c(1)), Subsp = c(1))
for(i in 1:Nrep ){ #replicate -- choose sample, then put in df
  mouse_means <- sample(PnD_female$mean_co, nMuscF_obs)
  Rand_Musc_F[i,1] <- mean(as.numeric(mouse_means))
  Rand_Musc_F[i,2] <- sd(as.numeric(mouse_means))/sqrt(nMuscF_obs)
  
  Rand_Musc_F[i,3] <- sd( c(mean(  mean(mouse_means[1:13]), mean(mouse_means[14:17] ) ), 
                            DomF_sbsp_mean ) ) / sqrt(2)
  
  Rand_Musc_F[i,4] <- "Rand-Musc-Female"
}

#Make plot
png('PnD_Female_sim.png')

Full_sim_F <- rbind(Rand_Dom_F, Rand_Musc_F)
#Dom.F Musc.F
obs_F <-  data.frame(smp_mean = c("NA", "NA"), Poly = c(Polymorphism_DF$SE.means[1],Polymorphism_DF$SE.means[3] ),
                     Div.Dom_Musc = c(Divergence_DF$SE[1],Divergence_DF$SE[1]),
                     Subsp = c("obsDomF", "obsMuscF")) #Poly, Div.Dom_Musc, Subsp
Full_sim_F <- rbind(Full_sim_F, obs_F)
juju <- ggplot(data = Full_sim_F, aes(x=Div.Dom_Musc, y=Poly, fill=Subsp, color=Subsp)) + 
  ylim(0,1.7)+geom_point()
juju
dev.off()

#simulations for Male .. when sampling size is the same, the subspecies samples, still diverge.. but less so
#they seem like mirror images
#Dom_M
Rand_Dom_M = data.frame(smp_mean=as.numeric(c(1)),Poly=as.numeric(c(1)), Div.Dom_Musc=as.numeric(c(1)), Subsp = c(1))
for(i in 1:Nrep ){ #replicate -- choose sample, then put in df
  mouse_means <- sample(PnD_male$mean_co, nDomM_obs)
  Rand_Dom_M[i,1] = mean(as.numeric(mouse_means))
  Rand_Dom_M[i,2] = sd(as.numeric(mouse_means))/sqrt(nDomM_obs)#nDomM_obs
  
#replicate the data structure for strain mean calculations
# mean(  mean( smp[1:5]), mean(smpl[6:13]), mean(smpl[14:20]) )
  Rand_Dom_M[i,3] = sd( c(  mean(  mean(mouse_means[1:5]), mean(mouse_means[6:13]), mean(mouse_means[14:19]) ), 
                            MuscM_sbsp_mean ) ) / sqrt(2)
  Rand_Dom_M[i,4] <- "Rand-Dom-male"
  # print(c(i,  sd(sampld_DomF[,i])/sqrt(sample_size),  (sd( c( mean(  as.numeric(sampld_DomF[,i]) ), DomM_sbsp_mean ) / sqrt(2) ) ) ) )
}
#Musc_M
#strain mean str
# mean( mean(smpl[1:7]), mean(smpl[8:14])  )

Rand_Musc_M = data.frame(smp_mean=as.numeric(c(1)),Poly=as.numeric(c(1)), Div.Dom_Musc=as.numeric(c(1)), Subsp = c(1))
for(i in 1:Nrep ){ #replicate -- choose sample, then put in df
  mouse_means <- sample(PnD_male$mean_co, nMuscM_obs) #if the number of observations was changd
  Rand_Musc_M[i,1] = mean(as.numeric(mouse_means))
  Rand_Musc_M[i,2] = sd(as.numeric(mouse_means))/sqrt(nMuscM_obs)
  
  Rand_Musc_M[i,3] = sd( c(  mean(  mean(mouse_means[1:7] ), mean(mouse_means[8:13] ) ),
                           DomM_sbsp_mean ) ) / sqrt(2) #
  Rand_Musc_M[i,4] <- "Rand-Musc-male"
  # print(c(i,  sd(sampld_DomF[,i])/sqrt(sample_size),  (sd( c( mean(  as.numeric(sampld_DomF[,i]) ), DomM_sbsp_mean ) / sqrt(2) ) ) ) )
}

#Make plot
png('PnD_Male_sim.png')


#
Full_sim_M <- rbind(Rand_Dom_M, Rand_Musc_M)
#Dom.F
obs_M <-  data.frame(smp_mean=c("NA","NA", "NA", "NA"),
  Poly = c(Polymorphism_DF$SE.means[2],Polymorphism_DF$SE.means[4], #Dom   #musc
           Polymorphism_DF$SE.means[4],Polymorphism_DF$SE.means[2]), #musc #dom
   
   Div.Dom_Musc = c(Divergence_DF$SE[2],Divergence_DF$SE[2],  #dom-musc
                     Divergence_DF$SE[4], #musc- cast
                     Divergence_DF$SE[6] #dom-cast - remove musc point
                     ),
    Subsp = c("obsDomM", "obsMuscM", "obsMuscM", "obsDomM")) #Poly, Div.Dom_Musc, Subsp

Full_sim_M <- rbind(Full_sim_M, obs_M)

mumu <- ggplot(data = Full_sim_M, aes(x=Div.Dom_Musc, y=Poly, fill=Subsp, color=Subsp))+
  ylim(0,1.7)+geom_point()
mumu

dev.off()
