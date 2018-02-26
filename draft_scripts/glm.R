# GLM
# 
#

library(plyr)
library(dplyr)
library(ggplot2)
library(lme4)
setwd("C:/Users/alpeterson7/Documents/MLH1repo")
load(file="MLH1_data_setup.RData")


#nested



#models of MLH1 counts

nosubsplmm <- lmer(nMLH1.foci ~ sex + (1 | strain), data = MLH1_data, REML = FALSE)
nostrainlmm <- lmer(nMLH1.foci ~ sex + (1 | subsp), data = MLH1_data, REML = FALSE)
nosexlmm <- lmer(nMLH1.foci ~ strain + (1 | subsp), data = MLH1_data, REML = FALSE)
nofixedlmm <- lmer(nMLH1.foci ~ 1 + (1 | subsp), data = MLH1_data, REML = FALSE)
anova(nostrainlmm, nosexlmm, nofixedlmm, nosubsplmm)
#compare lowest AIC, no sex

#while nested random effects take the form (1 | r1 / r2).

#sex fixed, other's random
model.cell.level = lme(nMLH1.foci ~ sex, random=list(~1|subsp, ~1|strain, ~1|mouse),
            data=MLH1_data, 
            method="REML")

model_fxd = lm(nMLH1.foci ~ sex+subsp+strain, random = ~1|mouse,
               data=MLH1_data)

anova(model_fxd)
summary(model_fxd)
varcomp(model_fxd)#only works for lme


# models of mouse averages

model.mouse.level = lm(mean_co ~ sex, random=list(~1|subsp, ~1|strain),
                       data=AP_mouse_table, 
                       method="REML")

model.mouse.level = lm(mean_co ~ sex, random=list(~1|subsp, ~1|strain),
                       data=AP_mouse_table)

model.mouse.nested = lmer(mean_co ~ subsp + sex + strain +(subsp|strain),
                       data=AP_mouse_table)
