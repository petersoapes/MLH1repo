# Mixed models for Heterochiasmy aim
# input:
# output: MM's, effect sizes, and p values for LRTs of fixed and Rand effects

#which package is the best choice?
#glm


#load data
setwd("C:/Users/alpeterson7/Documents/MLH1repo/")

load(file="data/MLH1/MLH1_data_setup_8.29.19.RData")
source("src/CommonFunc_MLH1repo.R")

#clean data, all strains ahve bothe sexes, make sure variables set as factors and are in a consistant order


MixedModel_CO_data <- AP_mouse_table_w.Ages[AP_mouse_table_w.Ages$strain.x != "other",]
CO_data_HetC_w.old <- AP_mouse_table_w.Ages[AP_mouse_table_w.Ages$strain.x != "other",]
CO_data_HetC_w.old <- CO_data_HetC_w.old[ ! CO_data_HetC_w.old$strain.x %in% no_female, ]
CO_data_HetC_w.old <- CO_data_HetC_w.old[!(is.na(CO_data_HetC_w.old$Nmice) | CO_data_HetC_w.old$Nmice==""), ]


MixedModel_CO_data <- MixedModel_CO_data[MixedModel_CO_data$age.weeks < 15,]

MixedModel_CO_data <- MixedModel_CO_data[!(is.na(MixedModel_CO_data$mouse) | MixedModel_CO_data$mouse==""), ]

no_female <- c("HMI", "SKIVE", "CZECH", "MOLF", "CAROLI", "TOM","AST")
MixedModel_CO_data <- MixedModel_CO_data[ ! MixedModel_CO_data$strain.x %in% no_female, ]

MM <- lme(mean_co ~ subsp.x * sex.x, data= MixedModel_CO_data, random=list(strain.x=pdDiag(~sex.x) ) )
anova(MM, verbose = TRUE)
anova(MM, verbose = TRUE, test = TRUE, type = "marginal")

kuku <- ggplot(MixedModel_CO_data, aes(y=mean_co, x=mouse, color=sex.y))+geom_point()+facet_wrap(~strain.x)

bubu <- ggplot(data=CO_data_HetC_w.old)+geom_point(aes(y=mean_co, x=mouse, color=sex.x))+facet_wrap(~strain.x)

#This plot shows the overall pattern that most strains have low to limited hetC (but in female biased direction)
#PWD and MSM are the only strains with male biased HetC
#SPRET follows the pattern, SPIC might aswell, but there might be a fair amount of noise



#there are factors L and Q, what do they stand for? 




#add species
MixedModel_CO_data <- add_species(MixedModel_CO_data)

#MixedModelMus <- MixedModel_CO_data[MixedModel_CO_data$species == "M.musculus",]


MixedModelMus$subsp <-  factor(MixedModelMus$subsp, ordered=TRUE, levels =c( "Dom","Cast", "Musc"))#add molf?

MixedModelMus$sex <-  factor(MixedModelMus$sex, ordered=TRUE, levels =c( "female", "male"))

MixedModelMus <- add_strain(MixedModelMus)
MixedModelMus$strain <-  factor(MixedModelMus$strain, ordered = TRUE, levels =c( "WSB", "G", "LEW", "PERC",
                                                 "PWD", "MSM","KAZ","CZECH","TOM","AST",
                                                 "CAST", "HMI",
                                                 "SPRET", "SPIC", "CAROLI", "other"))

#should strain really be ordered?
MixedModelMus$species <- factor(MixedModelMus$species)

#remove all  without female
table(MixedModelMus$category)


#clean up data, 




#make Mixed models for CO counts nlme package
library(nlme)

#build model
MM <- lme(mean_co ~ subsp * sex, data= MixedModelMus, random=list(strain=pdDiag(~sex) ) )

#there are factors L and Q, what do they stand for? 

ranef(lme.SC_length.MM)
#this will estimate the rand effects, but not test


#report coefficicents
summary(MM)

#run anova, for fixed effects
anova(MM, verbose = TRUE)
anova(MM, verbose = TRUE, test = TRUE, type = "marginal")

#Seems like there's not sig effect for subsp

#estimate random effects
ranef(MM)



#Liklihood ratio tests




#MixedModel_CO_data <- MixedModel_CO_data[MixedModel_CO_data$strain != c("HMI", "SKIVE", "CZECH", "MOLF"),]
#blah <- MixedModel_CO_data[ !grepl( c("HMI", "SKIVE", "CZECH", "MOLF"), MixedModel_CO_data$strain) , ]
