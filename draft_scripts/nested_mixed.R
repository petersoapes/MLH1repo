# Create a mixed model with nested factors using lme or lmer
# input: mouse average table
# output: aov of mixed model 

#load data
setwd("C:/Users/alpeterson7/Documents/MLH1repo")
load(file="nested_environment.RData")

#adjust data

#remove non house mouse 

AP_mouse_table <- AP_mouse_table[ !grepl("outgroup", AP_mouse_table$subsp) , ]
#remove cast (no female data)
AP_mouse_table <- AP_mouse_table[ !grepl("Cast", AP_mouse_table$subsp) , ]
#this seemed to work...

#factors
AP_mouse_table$sex <- as.factor(AP_mouse_table$sex)
AP_mouse_table$sex <- relevel(AP_mouse_table$sex, "male")

AP_mouse_table$subsp <- as.factor(AP_mouse_table$subsp)
AP_mouse_table$mouse <- as.factor(AP_mouse_table$mouse)
AP_mouse_table$strain <- as.factor(AP_mouse_table$strain)

attach(AP_mouse_table)
#lme
library(nlme)

# this needs the random parameter
aa <- lme(mean_co ~ sex * subsp, data=AP_mouse_table)
#Error in getGroups.data.frame(dataMix, groups) : 
#invalid formula for groups

#once 'Cast' observations removed, lme models worked

bb <- lme(mean_co ~ sex * subsp, data=AP_mouse_table, 
          random = ~sex|strain )

cc <- lme(mean_co ~ sex * subsp, data=AP_mouse_table, 
          random = list( ~1|strain, ~0+sex|strain ) )

dd <- lme(mean_co ~ sex * subsp, data=AP_mouse_table, 
          random = list( strain = pdDiag(~sex) )
          )

#gg <- lme(cV ~ sex * subsp, data=AP_mouse_table, 
#          random = list( strain = pdDiag(~sex) )
#)

zz <- lme(mean_co ~ sex*subsp, data=AP_mouse_table,
          random = ~1|strain) #removes strain specific sex effect


#subset data
Fmice_data <- subset(AP_mouse_table, sex=="female")
Mmice_data <- subset(AP_mouse_table, sex == "male")

#female models
fcc <- lme(mean_co ~ subsp, data=Fmice_data, 
           random = list( ~1|strain) )
# i don't think these will work without a random effect

#male models
mcc <- lme(mean_co ~ subsp, data=Mmice_data, 
           random = list( ~1|strain) )

 

#sd of CO #
sdcc <- lme(sd ~ sex * subsp, data=AP_mouse_table, 
          random = list( ~1|strain, ~0+sex|strain ) )

sddd <- lme(sd ~ sex * subsp, data=AP_mouse_table, 
          random = list( strain = pdDiag(~sex) )
)

sdzz <- lme(sd ~ sex*subsp, data=AP_mouse_table,
          random = ~1|strain) #removes strain specific sex effect

#cV
cVdd <- lme(cV ~ sex * subsp, data=AP_mouse_table, 
            random = list( strain = pdDiag(~sex) )
)

cVzz <- lme(cV ~ sex*subsp, data=AP_mouse_table,
            random = ~1|strain) #  




pp <- ggplot(AP_mouse_table, aes(x = as.factor(mouse), y = sd, color= strain ))+
  geom_point()+
  ggtitle("Boxplots of sd(MLH1) distributions by mouse") + 
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(),  axis.ticks.x=element_blank())
scale_color_manual(values=c("#56B4E9", "cadetblue", "lightblue",  "coral1", "#E69F00", "yellowgreen", "green", "grey", "black" ))
pp <- pp   + facet_wrap(~ sex, scales="free")
pp



#aov() for bb, cc and dd are the same
#anova values are slightly different, dd is most different

#aov(dd)
#                     sex    subsp sex:subsp Residuals
#Sum of Squares    2.2443 134.5151  126.1939  210.5428
#Deg. of Freedom        1        2         1        65

# subsp and the sex by subsp have an about equal amount of variance (effect)
# sex when 
#now sex factor will be pointless, but ... 

#subset data
Fmice_data <- subset(AP_mouse_table, sex=="female")
Mmice_data <- subset(AP_mouse_table, sex == "male")

#female models
fbb <- lme(mean_co ~ sex * subsp, data=Fmice_data, 
          random = ~sex|strain )
fcc <- lme(mean_co ~ sex * subsp, data=Fmice_data, 
          random = list( ~1|strain, ~ 0+sex|strain ) )
fdd <- lme(mean_co ~ sex * subsp, data=Fmice_data, 
          random = list( strain = pdDiag(~sex) ) )

#male models
mbb <- lme(mean_co ~ sex * subsp, data=AP_mouse_table, 
           random = ~sex|strain )
mcc <- lme(mean_co ~ sex * subsp, data=AP_mouse_table, 
           random = list( ~1|strain, ~ 0+sex|strain ) )
mdd <- lme(mean_co ~ sex * subsp, data=AP_mouse_table, 
           random = list( strain = pdDiag(~sex) ) )

#attempt lme4
library(lme4)
attach(AP_mouse_table)

zz <- lmer(mean_co ~ sex*subsp
           +(1|strain)
           +(0+sex | strain), data=AP_mouse_table)
#fixed-effect model matrix is rank deficient so dropping 1 column / coefficient
#Warning messages:
# 1: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#  unable to evaluate scaled gradient
# 2: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
# Model failed to converge: degenerate  Hessian with 1 negative eigenvalues

