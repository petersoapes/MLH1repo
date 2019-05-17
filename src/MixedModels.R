# Mixed models for Heterochiasmy aim
# input:
# output: MM's, effect sizes, and p values for LRTs of fixed and Rand effects

#which package is the best choice?
#glm


#load data
setwd("C:/Users/alpeterson7/Documents/MLH1repo/")

load(file="data/MLH1_data_setup.RData")
source("src/CommonFunc_MLH1repo.R")

#clean data, all strains ahve bothe sexes, make sure variables set as factors and are in a consistant order

MixedModel_CO_data <- AP_mouse_table[AP_mouse_table$strain != "other",]
MixedModel_CO_data <- MixedModel_CO_data[MixedModel_CO_data$strain != "HMI",]

#add species
MixedModel_CO_data <- add_species(MixedModel_CO_data)
MixedModelMus <- MixedModel_CO_data[MixedModel_CO_data$species == "M.musculus",]


MixedModelMus$subsp <-  factor(MixedModelMus$subsp, ordered=TRUE, levels =c( "Dom","Cast", "Musc"))

MixedModelMus$strain <-  factor(MixedModelMus$strain, ordered = TRUE, levels =c( "WSB", "G", "LEWES", "PERC",
                                                 "PWD", "MSM","KAZ","CZECH",
                                                 "CAST", "HMI",
                                                 "SPRET", "SPIC", "CAROLI", "other"))

MixedModelMus$species <- factor(MixedModelMus$species)



#make Mixed models for CO counts nlme package
library(nlme)

#build model
MM <- lme(mean_co ~ subsp * sex, data= MixedModelMus, random=list(strain=pdDiag(~sex) ) )

#report coefficicents
summary(MM)


#estimate random effects
#ranef(MM)



#Liklihood ratio tests
