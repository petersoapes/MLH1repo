#
# junk text -- dumping the junk text







<!--
  For later analysis.  Not sure what these are good for.

```{r Mixed.models.pooled.cells, echo=FALSE, eval=FALSE, include=FALSE}

# check if the main mixed model results hold up when all cells are pooled

#pooled.cells
DF.HetC.MixedModel.pooled <- MLH1_data[MLH1_data$species == "M.musculus",]
DF.HetC.MixedModel.pooled <- DF.HetC.MixedModel.pooled[DF.HetC.MixedModel.pooled$subsp != "Cast",]
#remove musc strains
DF.HetC.MixedModel.pooled <- DF.HetC.MixedModel.pooled[DF.HetC.MixedModel.pooled$strain != "AST",]
DF.HetC.MixedModel.pooled <- DF.HetC.MixedModel.pooled[DF.HetC.MixedModel.pooled$strain != "TOM",]

#remove PERC, MOLF, CZECH,
DF.HetC.MixedModel.pooled <- DF.HetC.MixedModel.pooled[DF.HetC.MixedModel.pooled$strain != "CZECH",]
DF.HetC.MixedModel.pooled <- DF.HetC.MixedModel.pooled[DF.HetC.MixedModel.pooled$strain != "MOLF",]
DF.HetC.MixedModel.pooled <- DF.HetC.MixedModel.pooled[DF.HetC.MixedModel.pooled$strain != "PERC",]


#sex only has 2 levels and this breaks...
model.pooled <- lme(nMLH1.foci ~ subsp * sex, data=DF.HetC.MixedModel.pooled, random=list(strain=pdDiag(~sex) ) )

#pull out values / other things
summary(model.pooled)# indeces for this object: [1]-structure
ranef(model.pooled)

model.pooled$coefficients

#pooled cells,  DF.HetC.MixedModel.pooled
DF.HetC.MixedModel.pooled <- droplevels(DF.HetC.MixedModel.pooled)
DF.HetC.MixedModel.pooled <- DF.HetC.MixedModel.pooled[!(is.na(DF.HetC.MixedModel.pooled$mouse) | DF.HetC.MixedModel.pooled$mouse==""), ]

#consider changing to ordered = FALSE, for the DFs used for MM
DF.HetC.MixedModel.pooled$sex <- factor(DF.HetC.MixedModel.pooled$sex, ordered = TRUE, levels =c("male", "female") )

DF.HetC.MixedModel.pooled$strain<- factor(DF.HetC.MixedModel.pooled$strain, ordered = TRUE, levels =c( "WSB", "G", "LEW", "PWD", "MSM", "SKIVE", "KAZ") )

DF.HetC.MixedModel.pooled <- droplevels(DF.HetC.MixedModel.pooled)


```


- The MSM strain effect is 0, but the sex*strain for molossinus is 7 (this seems very high). This means there is 0 variance added to mean_CO to mean due to being MSM. Does this mean the model is very good at predicting MSM mean_CO. The fixed strain - sex effects are for musc are large. 
This will likely change with MOLF (male molf are low, so smaller fixed effect and )

- I think adding Mol, moves the highest strain into it's own category, so the coefficient is large.
In the first model MSM has largest random effect, since it's so far from WSB female.

-adding MOLF also doesn't have significant factors


changed all to unordered factors.
all the variables are coded as ordered factors.
When the variables are kept factors, and unordered the names appear! yay!

Character assigned variables.  The values are now quite different. Male, sex coefficient is negative. The fixed sex coefficient is the sex effect independent of subspecies or strain.
-->


<!--

#from chrm proportion black

#oopp2 <- lm( mean.MLH1.foci2 ~ per0CO+ per1CO +per2CO +per3CO, data = proportion_stats_category)
#summary(oopp2)
#none of the classes are significant

#mean is the new calculated-- mean 0CO is significant and intercept
#oopp <-  lm( mean.MLH1.foci ~ per0CO+ per1CO +per2CO +per3CO, data = #proportion_stats_category)
#summary(oopp)
#not sure if these are inter-correlated
#I don't know why 0COs and the intercept are significant..
#0CO and 1CO are significant when only chrm classes are included (and intercept)
#no longer significant when used
#lm regression indicates that the proportions aren't significant contributers to meanMLH1 -- but this could be due to error
#after added more MSM data, tot_biv is not significant
#0CO is now more significant, and 2CO is sightly significant
#2CO proportion is the most significant, 1CO is second most significat, 3CO doesn't have enough power - total bivalents is also significant, there might be a bias
#MSM needs more bialent observations
#use melt to put all propotions 
#display / visualize / compare proportions
#the table needs to be melted, because it's split by category

```{r Q2.IFD.log.reg, echo=FALSE, eval=FALSE}

# LOGISTIC REGRESSION FOR IFD

log.reg_IFD_ABS_mouse.av <- glm(Rec.group ~ mean_IFD.2CO_ABS,
                                data=mouse.avs_4MM[(mouse.avs_4MM$sex == "male") & (mouse.avs_4MM$subsp == "Musc"),], family=binomial(link="logit"))

summary(log.reg_IFD_ABS_mouse.av)# 0.0366 *

log.reg_IFD_PER_mouse.av <- glm(Rec.group ~ mean_IFD.2CO_PER, 
                                data=mouse.avs_4MM[(mouse.avs_4MM$sex == "male") & (mouse.avs_4MM$subsp == "Musc"),], family=binomial(link="logit"))

summary(log.reg_IFD_PER_mouse.av)#NS


### Single Bivalent levels

log.reg_IFD_ABS_single.biv <- glm(Rec.group ~ IFD1_ABS, 
                                  data=Curated_BivData[(Curated_BivData$sex == "male") & (Curated_BivData$subsp == "Musc"),], family=binomial(link="logit"))
summary(log.reg_IFD_ABS_single.biv)

log.reg_IFD_PER_single.biv <- glm(Rec.group ~ IFD1_PER, 
                                  data=Curated_BivData[(Curated_BivData$sex == "male") & (Curated_BivData$subsp == "Musc"),], family=binomial(link="logit"))
summary(log.reg_IFD_PER_single.biv)
```


```{r trackcurBivData, echo=FALSE, eval=FALSE}
#REMOVE THIS BLOCK??

#list / tally of current curated Bivdata

Cur_biv = read.csv("~./MLH1repo/data/CurrentCuratedDataset_13.1.20.csv", header = TRUE)

colnames(Cur_biv) <- c('mouse', 'blank','0','1')


AP_mouse_table_w.Ages_HQ <- AP_mouse_table_w.Ages[ -c(9:12, 14, 18, 22:24) ]

AP_mouse_table_w.Ages_HQ <- add_sex(AP_mouse_table_w.Ages_HQ)
AP_mouse_table_w.Ages_HQ <- add_strain(AP_mouse_table_w.Ages_HQ)
AP_mouse_table_w.Ages_HQ <- add_category(AP_mouse_table_w.Ages_HQ)
AP_mouse_table_w.Ages_HQ <- add_subsp(AP_mouse_table_w.Ages_HQ)
AP_mouse_table_w.Ages_HQ <- add_species(AP_mouse_table_w.Ages_HQ)

#only M.musculus
AP_mouse_table_w.Ages_HQ <- AP_mouse_table_w.Ages_HQ[AP_mouse_table_w.Ages_HQ$species == "M.musculus",]
#REMOVE EXTRA STRAINS
AP_mouse_table_w.Ages_HQ <- AP_mouse_table_w.Ages_HQ[AP_mouse_table_w.Ages_HQ$subsp != "Cast",]
#remove musc strains
AP_mouse_table_w.Ages_HQ <- AP_mouse_table_w.Ages_HQ[AP_mouse_table_w.Ages_HQ$strain != "AST",]
AP_mouse_table_w.Ages_HQ <- AP_mouse_table_w.Ages_HQ[AP_mouse_table_w.Ages_HQ$strain != "TOM",]
#remove PERC, MOLF, CZECH, (no females)
AP_mouse_table_w.Ages_HQ <- AP_mouse_table_w.Ages_HQ[AP_mouse_table_w.Ages_HQ$strain != "CZECH",]
AP_mouse_table_w.Ages_HQ <- AP_mouse_table_w.Ages_HQ[AP_mouse_table_w.Ages_HQ$strain != "PERC",]
#156 mice (from 193)

#some mice are missing 13oct14

#match the MM dataset
AP_mouse_table_w.Ages_HQ <- AP_mouse_table_w.Ages_HQ[AP_mouse_table_w.Ages_HQ$Ncells > 9,]
AP_mouse_table_w.Ages_HQ <- AP_mouse_table_w.Ages_HQ[AP_mouse_table_w.Ages_HQ$age.weeks < 26,]
#126 mice

#AP_mouse_table_w.Ages_HQ <- AP_mouse_table_w.Ages_HQ[!(is.na(AP_mouse_table_w.Ages_HQ$mouse) | DF.HetC.MixedModel.HQ$mouse==""), ]

#mice missing from curated list
mice.within.BivData <- AP_mouse_table_w.Ages_HQ[ AP_mouse_table_w.Ages_HQ$mouse %in% Cur_biv$mouse, ]#54 mice in (14 mice have enteries but 0 curated biv)

#(14 mice need at least some curation

cur.data.mice <- Cur_biv[ Cur_biv$mouse %in% AP_mouse_table_w.Ages_HQ$mouse, ]

mice.missing.bivData <- AP_mouse_table_w.Ages_HQ[ ! AP_mouse_table_w.Ages_HQ$mouse %in% Cur_biv$mouse, ]#72 (includes mice not in MM)
#72 mice that I need to upload/run the algorithm on their images

#mice missing SC pass == '1'
colnames(Cur_biv) <- c("mouse","blank","SC.fail", "SC.pass")
NeedsCur <- Cur_biv[Cur_biv$SC.fail == 0,]

NeedsCur <- add_strain(NeedsCur)
write.table(NeedsCur, "~./MLH1repo/data/needs_cur.csv", sep=",",
            row.names = TRUE)


#44% of mice are missing curation

#write this table
#write.table(OG.cURATE.table.mouse, "~./MLH1repo/data/CurrentCuratedDataset_13.1.20.csv", sep=",",
#            row.names = TRUE)

mice.missing.bivData <- mice.missing.bivData[!(is.na(mice.missing.bivData$mouse) | mice.missing.bivData$mouse==""), ]
write.table(mice.missing.bivData, "~./MLH1repo/data/MissingMice_13.1.20.csv", sep=",",
            row.names = TRUE)


```


```{r chrm.pro.Curated.BivData, echo=FALSE, eval=FALSE}


#this chunk migth need to be deleted // I think everything is already done within the BivData
#this is a second mode of 

#bivalent data might be too big..
#load(file="~./MLH1repo/data/CleanBivData.RData")
#Curates_BivData -- hand.foci.count!

#For each mouse I need to count up
#the number of bivalent classes are not counted up

chrm.propotion.category <- ddply(.data=Curated_BivData, 
                                 .(category),
                                 summarize, 
                                 ncells = length(unique(fileName)),
                                 nmice = length(unique(mouse)),
                                 tot_biv  = length(Obj.ID),
                                 
                                 n0CO = length(which(hand.foci.count == 0)),
                                 n1CO = length(which(hand.foci.count == 1)),
                                 n2CO = length(which(hand.foci.count == 2)),
                                 n3CO = length(which(hand.foci.count == 3)),
                                 #n4CO = sum(n4CO),
                                 
                                 per0CO = sum(n0CO) / tot_biv,                 
                                 per1CO= sum(n1CO)/ tot_biv,
                                 per2CO= sum(n2CO)/ tot_biv,
                                 per3CO= sum(n3CO)/ tot_biv )
# per4CO= sum(n4CO)/ tot_biv  )



Chrm.proportions_Biv.Data <- melt(chrm.propotion.category, c(
  "per0CO", "per1CO", "per2CO", "per3CO"), 
  id=c("category", "tot_biv") )
colnames(Chrm.proportions_Biv.Data) <- c("category","total_biv","Chrm.Type","value") 

# I think I need to make 2 melt tables -- for count and proportions
Chrm.numbers_Biv.Data <- melt(chrm.propotion.category, c("n0CO", "n1CO", "n2CO", "n3CO" ), 
                              id=c("category", "tot_biv") )
colnames(Chrm.numbers_Biv.Data) <- c("category","total_biv", "Chrm.Type", "biv_count") 

Chrm.prop_Biv.Data <- cbind(Chrm.proportions_Biv.Data, Chrm.numbers_Biv.Data[,4])
colnames(Chrm.prop_Biv.Data) <- c("category","total_biv", "Chrm.Type","value", "biv_count") 

Chrm.prop_Biv.Data <-Chrm.prop_Biv.Data[(!is.na(Chrm.prop_Biv.Data$value)),]

Chrm.prop_Biv.Data$sex <- ifelse(grepl("female", Chrm.prop_Biv.Data$category), "female", ifelse(grepl("male", Chrm.prop_Biv.Data$category), "male", "" ))

Chrm.prop_Biv.Data$strain <- ifelse(grepl("WSB", Chrm.prop_Biv.Data$category), "WSB", ifelse(grepl("G", Chrm.prop_Biv.Data$category), "G",
                                                                                             ifelse(grepl("LEW", Chrm.prop_Biv.Data$category), "LEW",                                                                                                           
                                                                                                    ifelse(grepl("KAZ", Chrm.prop_Biv.Data$category), "KAZ",           
                                                                                                           ifelse(grepl("CZECH", Chrm.prop_Biv.Data$category), "CZECH", 
                                                                                                                  ifelse(grepl("SKIVE", Chrm.prop_Biv.Data$category), "SKIVE",
                                                                                                                         ifelse(grepl("PWD", Chrm.prop_Biv.Data$category), "PWD",  
                                                                                                                                ifelse(grepl("MSM", Chrm.prop_Biv.Data$category), "MSM", "" ))))))))

Chrm.prop_Biv.Data$strain<- factor(Chrm.prop_Biv.Data$strain,levels =c("WSB","G","LEW",
                                                                       "KAZ","CZECH", "SKIVE","PWD","MSM"), order=T )
#no mol yet

Chrm.prop_Biv.Data <- with(Chrm.prop_Biv.Data, Chrm.prop_Biv.Data[order(strain),])

#need to make list of positions

chrm_prop.BivData_plot <- ggplot(data = Chrm.prop_Biv.Data, aes(y = value, x = strain, fill=as.factor(Chrm.Type)) ) + geom_col(position = "dodge")+ggtitle("Chrm Proportions from BivData")+ facet_wrap(~sex, scales = "free", ncol = 1 )+ylim(c(0,1)) + labs(x="", y='Proportions') +   geom_text(aes(label = biv_count), position = position_dodge(0.99))

chrm_prop.BivData_plot

```


```{r WSB.IFD, echo=FALSE, warning=FALSE}
#make the dataset
#WSB.Bivalents.DF

#table(WSB.Bivalents.DF$sex, WSB.Bivalents.DF$hand.foci.count)#the 2CO's will be used
WSB.Bivalents.DF <- Curated_BivData[Curated_BivData$strain == "WSB",]


#plot the differences
wsb.ploty_IFD_raw <- ggplot(WSB.Bivalents.DF[WSB.Bivalents.DF$hand.foci.count == 2,],  aes(y=IFD1_ABS, x = sex))+
  geom_jitter()+ggtitle("Raw IFD, WSB")+geom_boxplot(aes(x=sex))
wsb.ploty_IFD_raw


wsb.ploty_IFD_PER <- ggplot(WSB.Bivalents.DF[WSB.Bivalents.DF$hand.foci.count == 2,],  aes(y=IFD1_PER, x = sex))+
  geom_jitter()+ggtitle("Percent IFD, WSB")+geom_boxplot(aes(x=sex))
wsb.ploty_IFD_PER


WSB.IFDnorm_scatter.plot <- ggplot(WSB.Bivalents.DF[WSB.Bivalents.DF$hand.foci.count == 2,],  aes(y=IFD1_PER, x = chromosomeLength, color = sex))+        geom_jitter()+ggtitle("WSB \n Normalized IFD and SC Length")+ylim(c(0,1.1))

WSB.IFDnorm_scatter.plot
#this plot is much messier than the MSM (PWD?)


#try plotting the other IFD

#make a table of the means and n ect
wsb.table_IFD <- ddply(WSB.Bivalents.DF, c("sex"), summarise,
                       
                       total.biv = length(Obj.ID),
                       
                       mean.IFD_2CO = mean(IFD1_ABS[which(hand.foci.count == 2)], na.rm = TRUE),
                       mean.IFD.PER = mean(IFD1_PER[which(hand.foci.count == 2)], na.rm = TRUE),
                       
                       n_2CO =  sum(hand.foci.count == 2, na.rm = TRUE) #this is the right syntax
)


#run a t.test
wsb.ttest  <- t.test(WSB.Bivalents.DF$chromosomeLength[WSB.Bivalents.DF$sex == "female"], WSB.Bivalents.DF$chromosomeLength[WSB.Bivalents.DF$sex == "male"])

```

The range of normalized IFDs overlap closer in males and females in the WSB data. 

```{r LEW.IFD, echo=FALSE, warning=FALSE}
#make the dataset
LEW.Bivalents.DF <- Curated_BivData[Curated_BivData$strain == "LEW",]

LEW.Bivalents.DF_IFD <- LEW.Bivalents.DF[(!is.na(LEW.Bivalents.DF$sex)) & (LEW.Bivalents.DF$hand.foci.count == 2),]

LEW.Bivalents.DF_IFD$sex <- droplevels(LEW.Bivalents.DF_IFD$sex)

#table(LEW.Bivalents.DF$sex, LEW.Bivalents.DF$hand.foci.count)

#plot the differences
#LEW.ploty <- ggplot(LEW.Bivalents.DF,  aes(y=chromosomeLength, x = sex, color= #as.factor(hand.foci.count)))+geom_jitter()+ggtitle("")


lew.ploty_IFD_raw <- ggplot( LEW.Bivalents.DF_IFD,  aes(y=IFD1_ABS, x = sex, na.rm = TRUE))+                     geom_jitter( na.rm = TRUE)+ggtitle("Raw IFD, LEW")+geom_boxplot(aes(x=sex, na.rm = TRUE))

#arg, little annoyed that the x NA won't go away


lew.ploty_IFD_PER <- ggplot(LEW.Bivalents.DF[LEW.Bivalents.DF$hand.foci.count == 2,],  aes(y=IFD1_PER, x = sex))+
  geom_jitter()+ggtitle("Percent IFD, LEW")+geom_boxplot(aes(x=sex))



LEW.IFDnorm_scatter.plot <- ggplot(LEW.Bivalents.DF[LEW.Bivalents.DF$hand.foci.count == 2,],  aes(y=IFD1_PER, x = chromosomeLength, color = sex))+  
  scale_color_manual(values=c('#56B4E9','#E69F00'))+
  geom_jitter()+ggtitle("LEW \n Normalized IFD and SC Length")+ylim(c(0,1.1))



#make a table of the means and n ect
lew.table_IFD <- ddply(LEW.Bivalents.DF, c("sex"), summarise,
                       
                       #     mean_SC = mean(chromosomeLength),
                       n_SC = length(Obj.ID),
                       
                       mean.IFD_2CO = mean(IFD1_ABS[which(hand.foci.count == 2)], na.rm = TRUE),
                       mean.IFD.PER = mean(IFD1_PER[which(hand.foci.count == 2)], na.rm = TRUE),
                       
                       n_2CO =  sum(hand.foci.count == 2, na.rm = TRUE) #this is the right syntax
)

#can I plot these stats
#run a t.test seperate out by the chrm classes
LEW.ttest  <- t.test(LEW.Bivalents.DF$chromosomeLength[LEW.Bivalents.DF$sex == "female"], LEW.Bivalents.DF$chromosomeLength[LEW.Bivalents.DF$sex == "male"])

```

The Lew pattern doesn't have a clean cut off of nrm.IFD. the range of male and females overlap, but there are more female observations below. 

```{r G.IFD, echo=FALSE, warning=FALSE}
#make the dataset
G.Bivalents.DF <- Curated_BivData[Curated_BivData$strain == "G",]
#table(G.Bivalents.DF$sex, G.Bivalents.DF$hand.foci.count)

G.Bivalents.DF <- G.Bivalents.DF[!(is.na(G.Bivalents.DF$fileName) | G.Bivalents.DF$fileName == ""), ]

#plot the metric
#remove the NAs

G.ploty_IFD_raw <- ggplot(G.Bivalents.DF[G.Bivalents.DF$hand.foci.count == 2,],  aes(y=IFD1_ABS, x = sex))+
geom_jitter()+ggtitle("Raw IFD, G")+geom_boxplot(aes(x=sex))

#
G.ploty_IFD_PER <- ggplot(G.Bivalents.DF[G.Bivalents.DF$hand.foci.count == 2,],  aes(y=IFD1_PER, x = sex))+
geom_jitter()+ggtitle("Percent IFD, G")+geom_boxplot(aes(x=sex))


G.IFDnorm_scatter.plot <- ggplot(G.Bivalents.DF[G.Bivalents.DF$hand.foci.count == 2,],  aes(y=IFD1_PER, x = chromosomeLength, color = sex))+ geom_jitter()+ggtitle("G \n Normalized IFD and SC Length")+ylim(c(0,1.1))


G.IFDnorm_scatter.plot <- ggplot(G.Bivalents.DF[G.Bivalents.DF$hand.foci.count == 2,],  aes(y=IFD1_PER, x = chromosomeLength, color = sex))+  geom_jitter()+ggtitle("G \n Normalized IFD and SC Length")+ylim(c(0,1.1))


#make a table of the means and n ect
G.IFD.table <- ddply(G.Bivalents.DF, c("sex"), summarise,

total.biv = length(Obj.ID),
mean.IFD_2CO = mean(IFD1_ABS[which(hand.foci.count == 2)], na.rm = TRUE),
mean.IFD.PER = mean(IFD1_PER[which(hand.foci.count == 2)], na.rm = TRUE),
n_2CO =  sum(hand.foci.count == 2, na.rm = TRUE) #this is the right syntax
)

#run a t.test seperate out by the chrm classes
G.ttest  <- t.test(G.Bivalents.DF$chromosomeLength[G.Bivalents.DF$sex == "female"], G.Bivalents.DF$chromosomeLength[G.Bivalents.DF$sex == "male"])

#calQ percent < 30%

oo <- G.Bivalents.DF[ (G.Bivalents.DF$sex == "male") & (G.Bivalents.DF$IFD1_PER <= .3 ), ]
oo <- oo[!(is.na(oo$fileName) | oo$fileName == ""), ]

#the na's are being counted (ughhhhhh)
#use the file thing instead

G.Bivalents.DF <- G.Bivalents.DF[!(is.na(G.Bivalents.DF$fileName) | G.Bivalents.DF$fileName == ""), ]


LT30_PER_male <-  length(G.Bivalents.DF$Obj.ID[ (  (G.Bivalents.DF$sex == "male") & (G.Bivalents.DF$IFD1_PER >= .3 ) ) ]) / 
  length(G.Bivalents.DF$Obj.ID[ (G.Bivalents.DF$sex == "male") ]) #0.8302619

LT30_PER_female <-  length(G.Bivalents.DF$Obj.ID[(G.Bivalents.DF$sex == "female") & (G.Bivalents.DF$IFD1_PER <= .3 ) ]) / length(G.Bivalents.DF$Obj.ID[ (G.Bivalents.DF$sex == "female") ])
#0.7062663

#would Chi-sq be a good test to compare the ratios ect?
#the sum of these doesn't add 
#G.Bivalents.DF
```


```{r PWD.IFD, echo=FALSE, warning=FALSE}
#make the dataset
PWD.Bivalents.DF <- Curated_BivData[Curated_BivData$strain == "PWD",]
#table(PWD.Bivalents.DF$sex, PWD.Bivalents.DF$hand.foci.count)

#plot the differences
#remove the NAs

PWD.ploty_IFD_raw <- ggplot(PWD.Bivalents.DF[PWD.Bivalents.DF$hand.foci.count == 2,],  aes(y=IFD1_ABS, x = sex))+                          geom_jitter()+ggtitle("Raw IFD, PWD")+geom_boxplot(aes(x=sex))




PWD.ploty_IFD_PER <- ggplot(PWD.Bivalents.DF[PWD.Bivalents.DF$hand.foci.count == 2,],  aes(y=IFD1_PER, x = sex))+
  geom_jitter()+ggtitle("Percent IFD, PWD")+geom_boxplot(aes(x=sex))

PWD.IFDnorm_scatter.plot <- ggplot(PWD.Bivalents.DF[PWD.Bivalents.DF$hand.foci.count == 2,],  aes(y=IFD1_PER, x = chromosomeLength, color = sex))+ geom_jitter()+ggtitle("PWD \n Normalized IFD and SC Length")+ylim(c(0,1.1))+scale_color_manual(values=c('#56B4E9','#E69F00'))

#run a t.test seperate out by the chrm classes
PWD.ttest  <- t.test(PWD.Bivalents.DF$chromosomeLength[PWD.Bivalents.DF$sex == "female"], PWD.Bivalents.DF$chromosomeLength[PWD.Bivalents.DF$sex == "male"])
```

For PWD, there are a few observations of the short IFDs for males, but there seems to be a cut-off / threshold at .3

```{r KAZ.IFD, echo=FALSE, warning=FALSE}
#make the dataset
#KAZ.Bivalents.DF <- Curated_BivData[Curated_BivData$strain == "KAZ",]

#table(KAZ.Bivalents.DF$sex, KAZ.Bivalents.DF$hand.foci.count)

#plot the differences

#remove the NAs
KAZ.ploty_IFD_raw <- ggplot(KAZ.Bivalents.DF[KAZ.Bivalents.DF$hand.foci.count == 2,],  aes(y=IFD1_ABS, x = sex))+
  geom_jitter()+ggtitle("Raw IFD, KAZ")+geom_boxplot(aes(x=sex))

KAZ.ploty_IFD_PER <- ggplot(KAZ.Bivalents.DF[KAZ.Bivalents.DF$hand.foci.count == 2,],  aes(y=IFD1_PER, x = sex))+
  geom_jitter()+ggtitle("Percent IFD, KAZ")+geom_boxplot(aes(x=sex))


KAZ.IFDnorm_scatter.plot <- ggplot(KAZ.Bivalents.DF[KAZ.Bivalents.DF$hand.foci.count == 2,],  aes(y=IFD1_PER, x = chromosomeLength, color = sex))+ geom_jitter()+ggtitle("KAZ \n Normalized IFD and SC Length")+ylim(c(0,1.1))+scale_color_manual(values=c('#56B4E9','#E69F00'))


#run a t.test seperate out by the chrm classes
KAZ.ttest  <- t.test(KAZ.Bivalents.DF$chromosomeLength[KAZ.Bivalents.DF$sex == "female"], KAZ.Bivalents.DF$chromosomeLength[KAZ.Bivalents.DF$sex == "male"])
```

For the KAZ, pattern the distinction between the male and female pattern is less distinct. There are fewer instances of females with v close IFD distances.


```{r SKIVE.IFD, echo=FALSE, warning=FALSE}
#make the dataset
SKIVE.Bivalents.DF <- Curated_BivData[Curated_BivData$strain == "SKIVE",]
#table(SKIVE.Bivalents.DF$sex, SKIVE.Bivalents.DF$hand.foci.count)

#plot the differences

#remove the NAs
SKIVE.ploty_IFD_raw <- ggplot(SKIVE.Bivalents.DF[SKIVE.Bivalents.DF$hand.foci.count == 2,],  aes(y=IFD1_ABS, x = sex))+
  geom_jitter()+ggtitle("Raw IFD, SKIVE")+geom_boxplot(aes(x=sex))

SKIVE.ploty_IFD_PER <- ggplot(SKIVE.Bivalents.DF[SKIVE.Bivalents.DF$hand.foci.count == 2,],  aes(y=IFD1_PER, x = sex))+
  geom_jitter()+ggtitle("Percent IFD, SKIVE")+geom_boxplot(aes(x=sex))

SKIVE.IFDnorm_scatter.plot <- ggplot(SKIVE.Bivalents.DF[SKIVE.Bivalents.DF$hand.foci.count == 2,],  aes(y=IFD1_PER, x = chromosomeLength, color = sex))+ geom_jitter()+ggtitle("SKIVE \n Normalized IFD and SC Length")+ylim(c(0,1.1))

SKIVE.table <-  ddply(SKIVE.Bivalents.DF, c("sex"), summarise,
                      
                      total.biv = length(Obj.ID),
                      
                      mean.IFD_2CO = mean(IFD1_ABS[which(hand.foci.count == 2)], na.rm = TRUE),
                      mean.IFD.PER = mean(IFD1_PER[which(hand.foci.count == 2)], na.rm = TRUE),
                      
                      n_2CO =  sum(hand.foci.count == 2, na.rm = TRUE) #this is the right syntax
)

#run a t.test seperate out by the chrm classes
SKIVE.ttest  <- t.test(SKIVE.Bivalents.DF$chromosomeLength[SKIVE.Bivalents.DF$sex == "female"], SKIVE.Bivalents.DF$chromosomeLength[SKIVE.Bivalents.DF$sex == "male"])
```

In the Skive data, it could be the case that the v. short IFD measures in females are rare / another class of observations.


```{r MSM.IFD, echo=FALSE, warning=FALSE}
#make the dataset
MSM.Bivalents.DF <- Curated_BivData[Curated_BivData$strain == "MSM",]
#table(MSM.Bivalents.DF$sex, MSM.Bivalents.DF$hand.foci.count)

#plot the differences
MSM.ploty <- ggplot(MSM.Bivalents.DF,  aes(y=chromosomeLength, x = sex, color= as.factor(hand.foci.count)))+geom_jitter()+ggtitle("")

MSM.ploty_IFD_raw <- ggplot(MSM.Bivalents.DF[MSM.Bivalents.DF$hand.foci.count == 2,],  aes(y=IFD1_ABS, x = sex))+
  geom_jitter()+ggtitle("Raw IFD, MSM")+geom_boxplot(aes(x=sex))


MSM.ploty_IFD_PER <- ggplot(MSM.Bivalents.DF[MSM.Bivalents.DF$hand.foci.count == 2,],  aes(y=IFD1_PER, x = sex))+
  geom_jitter()+ggtitle("Percent IFD, MSM")+geom_boxplot(aes(x=sex))


#another plot
MSM.IFD_another.plot <- ggplot(MSM.Bivalents.DF[MSM.Bivalents.DF$hand.foci.count == 2,],  aes(y=IFD1_ABS, x = chromosomeLength, color = sex))+                          geom_jitter()+ggtitle("MSM")

MSM.IFDnorm_another.plot <- ggplot(MSM.Bivalents.DF[MSM.Bivalents.DF$hand.foci.count == 2,],  aes(y=IFD1_PER, x = chromosomeLength, color = sex))+                          geom_jitter()+ggtitle("MSM")+ylim(c(0,1.1))

#this plot helps illustrate sex-specific differences for normalized IFDs -- both sexes have a large class / proportion of bivalents with similar IFD_PER values -- but the mean in females is lower due to them having MORE low IFD_PER values


#make a table of the means and n ect
MSM.table  <-  ddply(MSM.Bivalents.DF, c("sex"), summarise,
                     
                     total.biv = length(Obj.ID),
                     
                     mean.IFD_2CO = mean(IFD1_ABS[which(hand.foci.count == 2)], na.rm = TRUE),
                     mean.IFD.PER = mean(IFD1_PER[which(hand.foci.count == 2)], na.rm = TRUE),
                     
                     n_2CO =  sum(hand.foci.count == 2, na.rm = TRUE) #this is the right syntax
)

#can I plot these stats


#run a t.test seperate out by the chrm classes
MSM.ttest  <- t.test(MSM.Bivalents.DF$chromosomeLength[MSM.Bivalents.DF$sex == "female"], MSM.Bivalents.DF$chromosomeLength[MSM.Bivalents.DF$sex == "male"])

```

The MSM pattern has a short range and longer range of nrm.IFD in males and females respectively.  

```{r MOLF.IFD, echo=FALSE}

#SHUT DOWN THIS BLOCK UNTIL MOLF FEMALE DATA

#make the dataset
#MOLF.Bivalents.DF <- Curated_BivData[Curated_BivData$strain == "MOLF",]
MOLF.Bivalents.DF <- Curated_BivData %>%  filter(strain == "MOLF")
table(MOLF.Bivalents.DF$sex, MOLF.Bivalents.DF$hand.foci.count)

#plot the differences
#MOLF.ploty <- ggplot(MOLF.Bivalents.DF,  aes(y=chromosomeLength, x = sex, color= as.factor(hand.foci.count)))+geom_jitter()+ggtitle("")

#remove the NAs
#molf.ploty_IFD_raw <- ggplot(MOLF.Bivalents.DF[MOLF.Bivalents.DF$hand.foci.count == 2,],  aes(y=IFD1_ABS, x = sex))+geom_jitter()+ggtitle("Raw IFD, MOLF")+geom_boxplot(aes(x=sex))

#molf.ploty_IFD_PER <- ggplot(MOLF.Bivalents.DF[MOLF.Bivalents.DF$hand.foci.count == 2,],  aes(y=IFD1_PER, x = sex))+  geom_jitter()+ggtitle("Percent IFD, MOLF")+geom_boxplot(aes(x=sex))


#make a table of the means and n ect
MOLF.table  <-  ddply(MOLF.Bivalents.DF, c("sex"), summarise,
                      
                      total.biv = length(Obj.ID),
                      
                      mean.IFD_2CO = mean(IFD1_ABS[which(hand.foci.count == 2)], na.rm = TRUE),
                      mean.IFD.PER = mean(IFD1_PER[which(hand.foci.count == 2)], na.rm = TRUE),
                      
                      n_2CO =  sum(hand.foci.count == 2, na.rm = TRUE) #this is the right syntax
)


#run a t.test seperate out by the chrm classes
#MOLF.ttest  <- t.test(MOLF.Bivalents.DF$chromosomeLength[MOLF.Bivalents.DF$sex == "female"], #MOLF.Bivalents.DF$chromosomeLength[MOLF.Bivalents.DF$sex == "male"])

```

The strains which show a clean "30% threshold" for normalized IFD in males are: PWD, SKIVE, and MSM (which are the 2 high Rec and a intermediate strain). The other strains which have more overlap between males and females are the Dom strains and KAZ.


<!--
  #### IFD 3CO bivalents
  
  Run comparisons for 3CO bivalents.  

```{r CO3.comparisons, echo=FALSE}

total.IFDs <- Curated_BivData %>% filter(hand.foci.count > 1)

#take a look at the 3CO things

```
-->
  
  <!--
  Heterochiasmy Prediction

> Do the IFD measures lack significant sex effect? (as predicted)

There are sex effects for the lme() models for both the ABS and PER IFD measures. There is slight evidence for significant strain effect.


> Are there signiticant strain effects?

The random strain effect was tested for raw and normalized IFD, neither are significant.
-->
  
  
  ```{r Q1.strain.specific.junk, include=FALSE}

#Dom.all
#Q1glm_SCLength_M2.Dom <- glm(mean_SC ~ strain * sex, data=Q1.lmer_totSC.mouse.av.table[Q1.lmer_totSC.mouse.av.table$subsp == "Dom",])
#summary(Q1glm_SCLength_M2.Dom)

#Dom.1CO
#Q1glm_SCLength_M2.1CO.Dom <- glm(mean.SC_1CO ~ strain * sex, data=Q1.lmer_totSC.mouse.av.table[Q1.lmer_totSC.mouse.av.table$subsp == "Dom",])
#summary(Q1glm_SCLength_M2.1CO.Dom)

#Dom.2CO
#Q1glm_SCLength_M2.2CO.Dom <- glm(mean.SC_2CO ~ strain * sex, data=Mouse.Table_BivData_4MM[Mouse.Table_BivData_4MM$subsp == "Dom",])
#summary(Q1glm_SCLength_M2.2CO.Dom)


#Musc.all
#Q1glm_SCLength_M2.Musc <- glm(mean_SC ~ strain * sex, data=Mouse.Table_BivData_4MM[Mouse.Table_BivData_4MM$subsp == "Musc",])
#summary(Q1glm_SCLength_M2.Musc)

#Musc.1CO
#Q1glm_SCLength_M2.1CO.Musc <- glm(mean.SC_1CO ~ strain * sex, data=Mouse.Table_BivData_4MM[Mouse.Table_BivData_4MM$subsp == "Musc",])
#summary(Q1glm_SCLength_M2.1CO.Musc)

#Musc.2CO
#Q1glm_SCLength_M2.2CO.Musc <- glm(mean.SC_2CO ~ strain * sex, data=Mouse.Table_BivData_4MM[Mouse.Table_BivData_4MM$subsp == "Musc",])
#summary(Q1glm_SCLength_M2.2CO.Musc)

#Mol.all
#Q1glm_SCLength_M2.Mol <- glm(mean_SC ~ strain * sex, data=Mouse.Table_BivData_4MM[Mouse.Table_BivData_4MM$subsp == "Mol",])
#summary(Q1glm_SCLength_M2.Mol)

#Mol.1CO
#Q1glm_SCLength_M2.1CO.Mol <- glm(mean.SC_1CO ~ strain * sex, data=Mouse.Table_BivData_4MM[Mouse.Table_BivData_4MM$subsp == "Mol",])
#summary(Q1glm_SCLength_M2.1CO.Mol)

#Mol.2CO
#Q1glm_SCLength_M2.2CO.Mol <- glm(mean.SC_2CO ~ strain * sex, data=Mouse.Table_BivData_4MM[Mouse.Table_BivData_4MM$subsp == "Mol",])
#summary(Q1glm_SCLength_M2.2CO.Mol)
```


```{r cv.plots, echo=FALSE, eval=FALSE}

cV.Hist.total.mice <- ggplot(DF.HetC.MixedModel, aes(x =cV , fill=sex)) + 
  geom_histogram(binwidth = 1, alpha=.6)  +
  ggtitle("cV of within mouse variance, all mice")
cV.Hist.total.mice <- cV.Hist.total.mice   + facet_wrap(~ strain, scales="free")
cV.Hist.total.mice

#DF.HetC.MixedModel.HQ
cV.Hist <- ggplot(DF.HetC.MixedModel.HQ, aes(x = cV, fill=sex)) + 
  geom_histogram(binwidth = 1, alpha=.6)  + 
  ggtitle("cV of within mouse variance, 10 cell min")
cV.Hist <- cV.Hist   + facet_wrap(~ strain, scales="free")
cV.Hist
```

