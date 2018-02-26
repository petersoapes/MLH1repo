# protocol for making a Linear model from MLH1 count data
#
#

library(plyr)
library(dplyr)
library(ggplot2)
library(lme4)
setwd("C:/Users/alpeterson7/Documents/MLH1repo")

#step 1. load data
load(file="MLH1_data_setup.RData")

#remove non-house-mouse for this
MLH1_data <- MLH1_data[ !grepl("SPRET", MLH1_data$strain) , ]
MLH1_data <- MLH1_data[ !grepl("SPIC", MLH1_data$strain) , ]

# potentially removed order of factors in dataframe
MLH1_data$strain <- factor(MLH1_data$strain)
MLH1_data$subsp <- factor(MLH1_data$subsp)

# model the relationship of MLH1 count with sex, subsp, strain and mouse
# 

#make boxplots of all variables, how variable are the means
(subsp <- ggplot(MLH1_data, aes(subsp, nMLH1.foci)) +
    geom_boxplot(fill = "#CD3333", alpha = 0.8, colour = "#8B2323") +
    theme.clean() +  
    theme(axis.text.x = element_text(size = 12, angle = 0)) +
    labs(x = "x", y = "MLH1.counts"))

(sex <- ggplot(MLH1_data, aes(sex, nMLH1.foci)) +
    geom_boxplot(fill = "#CD3333", alpha = 0.8, colour = "#8B2323") +
    theme.clean() +  
    theme(axis.text.x = element_text(size = 12, angle = 0)) +
    labs(x = "x", y = "MLH1.counts"))

(strain <- ggplot(MLH1_data, aes(strain, nMLH1.foci)) +
    geom_boxplot(fill = "#CD3333", alpha = 0.8, colour = "#8B2323") +
    theme.clean() +  
    theme(axis.text.x = element_text(size = 12, angle = 0)) +
    labs(x = "x", y = "y"))

(mouse <- ggplot(MLH1_data, aes(mouse, nMLH1.foci)) +
    geom_boxplot(fill = "#CD3333", alpha = 0.8, colour = "#8B2323") +
    theme.clean() +  
    theme(axis.text.x = element_text(size = 1, angle = 0)) +
    labs(x = "x", y = "y"))

#create single factor models
a.sex <- lm(nMLH1.foci ~ sex, data=MLH1_data)#
a.subsp <- lm(nMLH1.foci ~ subsp, data=MLH1_data)#
a.strain <- lm(nMLH1.foci ~ strain, data=MLH1_data)#
a.mouse <- lm(nMLH1.foci ~ mouse, data=MLH1_data)#

#check if residuals are normal distb
a.sex.resid <- resid(a.sex)
a.subsp.resid <- resid(a.subsp)
a.strain.resid <- resid(a.strain)
a.mouse.resid <- resid(a.mouse)

shapiro.test(a.sex.resid)#reject normal distrb
shapiro.test(a.subsp.resid)#reject normal distrb
shapiro.test(a.strain.resid)#reject normal distrb
shapiro.test(a.mouse.resid)#reject normal distrb
#assumption of normality viloated for all the above models

bartlett.test(nMLH1.foci ~ sex, data=MLH1_data)#null supportd
bartlett.test(nMLH1.foci ~ subsp, data=MLH1_data)#null rejected
bartlett.test(nMLH1.foci ~ strain, data=MLH1_data)#null rejctd
bartlett.test(nMLH1.foci ~ mouse, data=MLH1_data)#null rejected

#male and females have eual variances, only two categories

# these violations (of test models) may indcate that switching to a different distrbution
# and using the package glm() allows you to choose a different distribution
# .. but it doesn't really seem to improve anything (restoring normality)

#try a full
full <- lm(nMLH1.foci ~ sex + subsp + strain + mouse, data=MLH1_data)#
#the co-effecicent labels for strain are strange...

full.resid <- resid(full)
plot(full.resid)
shapiro.test(full.resid)#not normal

#try nested models (not all of the coefficients return estimates)

aov(Y ~ A/B, data=d)
aov(Y ~ A + B %in% A, data=d)
aov(Y ~ A + A:B, data=d)

nest.sb.strain <- lm(nMLH1.foci ~ strain %in% subsp, data=MLH1_data)
nest.strain.m <- lm(nMLH1.foci ~ mouse %in% strain, data=MLH1_data)

gnest.sb.strain <- glm(nMLH1.foci ~ strain %in% subsp, data=MLH1_data)
gnest.strain.m <- glm(nMLH1.foci ~ mouse %in% strain, data=MLH1_data)

aov(nest.strain.m)

mm <- resid(nest.sb.strain)
plot(mm)
shapiro.test(mm)#not normal


# create the simplist fixed effect LM
a1 <- lm(nMLH1.foci ~ 1 + sex + subsp + strain + mouse, data=MLH1_data)#
a2 <- lm(nMLH1.foci ~ sex + subsp + strain, data=MLH1_data)
#this model has extra Coefficients

aov(nMLH1.foci ~ sex, random=list(~1|subsp, ~1|strain, ~1|mouse),
    data=MLH1_data)

gnest.full <- glm(nMLH1.foci ~ strain %in% subsp + sex %in% strain + mouse %in% sex, data=MLH1_data)

gnest.full2 <- glm(nMLH1.foci ~ subsp/strain/sex/mouse, data=MLH1_data)

aavv <- aov(gnest.full2)

anova(gnest.full)


# It isn't clear to me how sex fits into a nexted mixed model, with the goal of 
#partioning variance for subsp and strains. I think a straight
# forward solution is to seperate the data by sex


MLH1_data_f <- subset(MLH1_data, sex="female")
MLH1_data_m <- subset(MLH1_data, sex="male")

aov(nMLH1.foci ~ subsp/strain/mouse,data=MLH1_data_f)

aov(nMLH1.foci ~ subsp/strain/mouse,data=MLH1_data_m)


#aa <- lm(nMLH1.foci ~ subsp/strain/mouse, data=MLH1_data_m)# not right
#bb<- glm(nMLH1.foci ~ subsp/strain/mouse, data=MLH1_data_m)#do not think this is right, strainPWD -- WSB male

cc <- lm(nMLH1.foci ~ subsp/strain + strain/mouse, data=MLH1_data_m)#not right
dd <- lm(nMLH1.foci ~ sex + subsp + subsp/strain + strain/mouse, data=MLH1_data)#







model = lme(nMLH1.foci ~ sex, random=list(~1|subsp, ~1|strain, ~1|mouse),
            data=MLH1_data, 
            method="REML")

model_fxd = lm(nMLH1.foci ~ sex+subsp+strain, random = ~1|mouse,
            data=MLH1_data)

anova(model_fxd)
summary(model_fxd)
varcomp(model_fxd)#only works for lme
#using male data and having subsp fixed, returns estimate

m <- anova(model_m)
summary(m)



leastsquare = lsmeans(model_m, 
                      pairwise ~ subsp, 
                      adjust="tukey")


anova.lme(model_m, 
          type="sequential", 
          adjustSigma = FALSE)

# Mixed model for MLH1 so that I can estimate random variables
# Strain, mouse and maybe sex should be Random effects
# rand variables seems better for nesting and variance partitioning
# 

#I CAN NEST random variables within a lme object
#random=list(~1|subsp, ~1|strain, ~1|mouse) and then calq varcomp for them
#i'm not sure how to get sex variance -- maybe I need to seperate the data


#https://ourcodingclub.github.io/2017/03/15/mixed-models.html
#following example that compares two continous variables...(my data doesn't have many continous variables)
#could add aver_SC to MLH1 data

#scale normalized MLH1 number
MLH1_data$nrm_adjust_nMLH1 <- scale(MLH1_data$adj_nMLH1.foci)

hist(MLH1_data$nrm_adjust_nMLH1)


basic.lm.g <- glm(adj_nMLH1.foci ~ sex, family = gaussian, data = MLH1_data)
basic.lm.p <- glm(adj_nMLH1.foci ~ sex, family = poisson, data = MLH1_data)
summary(basic.lm.g)
summary(basic.lm.p)


#Geom_smooth fits a line
ggplot(MLH1_data, aes(x = quality, y = adj_nMLH1.foci)) +
  geom_point() +
  geom_smooth(method = "lm") 

plot(basic.lm, which = 1)


boxplot(adj_nMLH1.foci ~ strain, data = MLH1_data)


ggplot(aes(sex, adj_nMLH1.foci), data = MLH1_data) + 
  geom_point() + geom_jitter() +
  facet_wrap(~ strain) + 
  xlab("strain") + 
  ylab("adj_MLH1")

#in example, fix mtRange, as random effect. whats the relationship after controlling for Mt
mixed.lmer <- lmer(testScore ~ bodyLength2 + (1|mountainRange), data = dragons)
mixed.lmer <- lmer(adj_nMLH1.foci ~ sex + (1|strain) + (1|mouse), data = MLH1_data)

mixed.lmer <- lme(adj_nMLH1.foci ~ sex + (1|strain) + (1|mouse), data = MLH1_data)

#subspecies should also be added!
mixed <- lme(adj_nMLH1.foci ~ sex+subsp+strain, random=list(~1|sex, ~1|subsp, ~1|strain, ~1|mouse), data=MLH1_data)
plot(mixed)
varcomp(mixed)# 

subsp    strain     mouse    Within 
2.058765  1.547842  3.555290 12.717996 

#mouse needs to be explicitly nested ()

mixed.WRONG <- lmer(testScore ~ bodyLength2 + (1|mountainRange) + (1|site), data = dragons)  
# treats the two random effects as if they are crossed

#tring to create an explicitly nested factor for mouse, but each mouse
#is unique ... so I don't think this is an issue
#below code doesn't work
MLH1_data <- within(MLH1_data, mouse2 <- factor(strain:mouse))

#Our question gets adjusted slightly again: Is there an association between body length and intelligence in dragons after controlling for variation in mountain ranges and sites within mountain ranges?
mixed.lmer2 <- lmer(testScore ~ bodyLength2 + (1|mountainRange) + (1|sample), data = dragons)  # the syntax stays the same, but now the nesting is taken into account
summary(mixed.lmer2)

mixed.lmer2 <- lmer(adj_nMLH1.foci ~ sex + (1|strain) + (1|mouse), data = MLH1_data)  # the syntax stays the same, but now the nesting is taken into account
summary(mixed.lmer2)


ggplot(MLH1_data, aes(x = sex, y = adj_nMLH1.foci, colour = mouse)) +
  facet_wrap(~strain, nrow=3) +
   geom_jitter()+
  theme_classic() +
  geom_line(data = cbind(MLH1_data, pred = predict(mixed.lmer2)), aes(y = pred)) +
  theme(legend.position = "none")


#makes a nice table of LM
stargazer(mixed.lmer2, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")


#


#https://ase.tufts.edu/gsc/gradresources/guidetomixedmodelsinr/mixed%20model%20guide.html

#desity plot of data
ggplot(MLH1_data, aes(x = adj_nMLH1.foci)) + geom_density() + facet_wrap(strain ~
                                                                   sex)
#transforming data didn't change the shape much
ggplot(MLH1_data, aes(x = adj_nMLH1.foci)) + geom_histogram() + facet_wrap(strain ~
                                                                           sex)
#create a model, 
lmm <- lmer(adj_nMLH1.foci ~ sex + (1 | strain) + (1 | mouse), data = MLH1_data,
            REML = FALSE)
summary(lmm)
plot(fitted(lmm), residuals(lmm), xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, lty = 2)
lines(smooth.spline(fitted(lmm), residuals(lmm)))
#plot the residuals


# find the best prob distribution use qqp() plot
qqp(MLH1_data$adj_nMLH1.foci, "norm")#bad
qqp(MLH1_data$adj_nMLH1.foci, "lnorm")

poisson <- fitdistr(MLH1_data$adj_nMLH1.foci, "Poisson")
qqp(MLH1_data$adj_nMLH1.foci, "pois", poisson$estimate) #best out of the ones tested

poisson.m <- fitdistr(as.numeric(AP_mouse_table$mean_co), "Poisson")
qqp(MLH1_data$adj_nMLH1.foci, "pois", poisson.m$estimate) #best out of the ones tested


gamma <- fitdistr(MLH1_data$adj_nMLH1.foci, "gamma")
qqp(MLH1_data$adj_nMLH1.foci, "gamma", shape = gamma$estimate[[1]], rate = gamma$estimate[[2]])

gamma.m <- fitdistr(as.numeric(AP_mouse_table$mean_co), "gamma")
qqp(AP_mouse_table$mean_co, "gamma", shape = gamma.m$estimate[[1]], rate = gamma.m$estimate[[2]])


