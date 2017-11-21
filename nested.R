# Nested ANOVA
# input: RData frame
# output: linear model with nested structure

library(lme4)
library(knitr)
library(ggplot2)
library(pwr)
library(plyr)
library(lattice)
library(dplyr)
setwd("C:/Users/alpeterson7/Documents/MLH1repo")
load(file="MLH1_data_setup.RData")

#seperate by sex

MLH1_data$subsp <- as.factor(MLH1_data$subsp)
MLH1_data$mouse <- as.factor(MLH1_data$mouse)

good_Q <- MLH1_data[MLH1_data$quality <= 4,]

#build nested nova model
MLf <- good_Q[good_Q$sex == "female",]
MLfvv <- MLH1_data[MLH1_data$sex == "female",]

gdMLm <- good_Q[good_Q$sex == "male",]
MLm <- MLH1_data[MLH1_data$sex == "male",]


#aov(Y ~ A/B, data=d)
#aov(MLH1.foci ~ subsp + strain %in% subsp, + mouse %in% strain, data= MLH1)
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
