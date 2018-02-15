# Nested ANOVA
# input: RData frame
# output: linear model with nested structure

#to do , meeting with cecile
# nested mixed model
# make male the first factor (default) this accounts for missing data
# 


library(lme4)
library(knitr)
library(ggplot2)
library(pwr)
library(plyr)
library(lattice)
library(dplyr)
library(nlme)
setwd("C:/Users/alpeterson7/Documents/MLH1repo")
load(file="nested_environment.RData")

#purpose -- make a 5 level nested ANOVA
# clone MLH1_data and start adding cols, with  mini$strain_sex <- factor(mini$strain:mini$sex)
# interactions between the levels

mini <- MLH1_data

#only use HM data
mini <- mini[ !grepl("Spretus", mini$subsp) , ]
mini <- mini[ !grepl("Spic", mini$subsp) , ]

#new model from Cecile
#prepare AP_mouse_table (only HM, make sure the right things are factors)
AP_mouse_table <- AP_mouse_table[ !grepl("outgroup", AP_mouse_table$subsp) , ]

#factors
AP_mouse_table$sex <- as.factor(AP_mouse_table$sex)
AP_mouse_table$sex <- relevel(AP_mouse_table$sex, "male")

AP_mouse_table$subsp <- as.factor(AP_mouse_table$subsp)

#C suggestion
cc <- lme(mean_co ~ sex * subsp , data=AP_mouse_table, random = list(strain = pdDiag(~sex)) )

#simpler ones
cc <- lme(mean_co ~ sex * subsp, data=AP_mouse_table, 
         random = list(~1|strain, ~0+sex|strain)  )

gg <- lme(mean_co ~ sex * subsp, data=AP_mouse_table, 
          random = ~sex|strain )

#error, invalid formula for groups
library(lme4)
attach(AP_mouse_table)
bb <- lmer(mean_co ~ sex*subsp
           +(1|strain)
           +(0+sex | strain), data=AP_mouse_table)


#what I think is nested ANOVA
# the SS stay the same for each level

#do these slowly one at a time
fm1a <- aov(formula= nMLH1.foci ~ subsp+ strain:subsp, data=mini)#very quick
fm1b <- aov(formula= nMLH1.foci ~ subsp+ strain:subsp+sex:strain, data=mini)#quick
# most useful model
fm1c <- aov(formula= nMLH1.foci ~ subsp+ strain:subsp+sex:strain+mouse:sex, data=mini)#quick
# below takes too long and breaks R
#fm1d <- aov(formula= nMLH1.foci ~ subsp+ strain:subsp+sex:strain+mouse:sex+Original.Name:mouse, data=mini)

SSt <- 31923
MSt <- 2966.1

female_mini <- subset(mini, sex=="female")
male_mini <- subset(mini, sex=="male")

ffm1a <- aov(formula= nMLH1.foci ~ subsp+ strain:subsp, data=female_mini)#very quick
ffm1b <- aov(formula= nMLH1.foci ~ subsp+ strain:subsp+mouse:strain, data=female_mini)#quick

mm1a <- aov(formula= nMLH1.foci ~ subsp+ strain:subsp, data=male_mini)#very quick
mm1b <- aov(formula= nMLH1.foci ~ subsp+ strain:subsp+mouse:strain, data=male_mini)#quick


Dom_mini <- subset(mini, subsp=="Dom")
Musc_mini <- subset(mini, subsp=="Musc")

Domb <- aov(formula= nMLH1.foci ~ strain + sex:strain + sex:mouse + Original.Name:mouse, data=Dom_mini)#quick
Domb <- aov(formula= nMLH1.foci ~ strain + sex:strain + sex:mouse, data=Dom_mini)
Muscb <- aov(formula= nMLH1.foci ~ strain + sex:strain + sex:mouse, data=Musc_mini)#quick



gm1a <- aov(formula= adj_nMLH1.foci ~ subsp+ strain:subsp, data=mini)#very quick
gm1b <- aov(formula= adj_nMLH1.foci ~ subsp+ strain:s- ubsp+sex:strain, data=mini)#quick
# most useful model
gm1c <- aov(formula= adj_nMLH1.foci ~ subsp+ strain:subsp+sex:strain+mouse:sex, data=mini)#quick
#even if I reverse the 'nestd factors
gm1z <- aov(formula= adj_nMLH1.foci ~ subsp + subsp:strain +strain:sex + sex:mouse, data=mini)#quick

#library(vegan)
adonis2(adj_nMLH1.foci ~ subsp+ strain:subsp+sex:strain+mouse:sex, data=mini)

save.image("nested_environment.RData")


#seperate by sex

MLH1_data$subsp <- as.factor(MLH1_data$subsp)
MLH1_data$mouse <- as.factor(MLH1_data$mouse)

#how do I treat the quality
good_Q <- MLH1_data[MLH1_data$quality <= 4,]

#build nested nova model
MLf <- good_Q[good_Q$sex == "female",]
MLfvv <- MLH1_data[MLH1_data$sex == "female",]

gdMLm <- good_Q[good_Q$sex == "male",]
MLm <- MLH1_data[MLH1_data$sex == "male",]

#aov(Y ~ A/B, data=d)
#aov(MLH1.foci ~ subsp + strain %in% subsp, + mouse %in% strain, data= MLH1)

#potential Nesting coding
ffq <- aov(nMLH1.foci ~  subsp + strain %in% subsp + mouse %in% strain, 
          data= MLf)

op <- par(mfrow = c(2, 2))
plot(ff)
par(op)

simp <- aov(nMLH1.foci ~  subsp * strain * mouse, 
            data= MLfvv)
ff <- aov(nMLH1.foci ~  subsp + strain %in% subsp + mouse %in% strain, 
           data= MLfvv)
# this is the same SS results as by nested notation... %in% doesn't change
# 
ffgg <- aov(nMLH1.foci ~ subsp/strain/mouse, 
          data= MLfvv)
#MLf
ll <- anova(lm(nMLH1.foci ~ subsp/strain/mouse, 
               data= MLf))

hh <- anova(lm(nMLH1.foci ~ subsp/strain/mouse, 
          data= MLfvv))
# all of the above models give the same summary results
# #i don't think I should expect the models to change that much...?

mm <- aov(nMLH1.foci ~  subsp + strain %in% subsp + mouse %in% strain, 
          data= MLm)

qqmm <- aov(nMLH1.foci ~  subsp + strain %in% subsp + mouse %in% strain, 
          data= gdMLm)

op <- par(mfrow = c(2, 2))
plot(qqmm)
par(op)

aov(Y ~ A + B %in% A, data=d)
aov(Y ~ A + A:B, data=d)
