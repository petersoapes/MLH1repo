# set up data file for algorithm data output
# input: Currated BivDataFile
# output: cleanBivData.csv (write out the )

library(plyr)
library(lattice)
library(dplyr)
library(ggplot2)


#ToDo
#need to remake the large full DF -- since the makefile was leaving out rows

#1. check for dups and make a list for human reference
#2. pull all the bivalents that have good SC segmentation
#3. make a master csv file for copying measurements into

#make OG.curatedBivData (see SC pass =0 | 1)

setwd("~./MLH1repo/")

source("src/CommonFunc_MLH1repo.R")
#load main data file ... for other files
#load(file="data/MLH1/MLH1_data_setup_11.12.19.RData")

#master_curated_BivData.org = read.csv("data/BivData/FULL_Merged_Curated_BivData.csv")#6859
#merge / find what's missing in old a new curate versions
#'curate.csv' is the grep string!

#BIVDATA
new.Curate.FULL <- read.csv("data/BivData/curation/FULL_Curated_BivData.csv")
#80919
#61084
#60241
#60830
#57548
#this file is made with a make pipeline. cat all .csv files
#Currently / best way is to compile this by hand -- currently have make file for compiling 

#str(new.Curate.FULL) #start 27,764

new.Curate.FULL <- new.Curate.FULL[!(is.na(new.Curate.FULL$fileName) | new.Curate.FULL$fileName==""), ]
new.Curate.FULL$Obj.ID <- paste(new.Curate.FULL$fileName, new.Curate.FULL$boxNumber, sep = "_")
new.Curate.FULL <- add_mouse(new.Curate.FULL)


#make OG copy
new.Curate.FULL.org <- new.Curate.FULL

new.Curate.FULL.org <- add_category(new.Curate.FULL.org)

OG.cURATE.table.mouse <- table(new.Curate.FULL.org$mouse, new.Curate.FULL.org$SC.pass)
OG.cURATE.table.category <- table(new.Curate.FULL.org$category, new.Curate.FULL.org$SC.pass)


OG.cURATE.table.mouse <- add_strain(OG.cURATE.table.mouse)

#write the curated file
write.table(OG.cURATE.table.mouse, "~./MLH1repo/data/CurrentCuratedDataset_17.1.20.csv", sep=",",
            row.names = TRUE)


write.table(OG.cURATE.table.category, "~./MLH1repo/data/CurrentCuratedDataset_cat_17.1.20.csv", sep=",",
            row.names = TRUE)

#bc there are a bunch of dumb chrs, all the useful cols will be factors
anyDuplicated(new.Curate.FULL$Obj.ID)#360 duplicated
#16242
#31186
#13981 -- 320 rows

dupies <- new.Curate.FULL[duplicated(new.Curate.FULL$Obj.ID),]#800 rows duplicated

#large number of dupies for blank Obj.ID
#23938

#remove rows and lines with NA
dupies <- add_mouse(dupies)
table(dupies$mouse)
#2 MSM mice! 13nov16_MSM_m2, 13nov16_MSM_m1, KAZ mouse




#most of the mice with duplicates: # 21may18_KAZ_m2, 13nov16_MSM_m1, 13nov16_MSM_m2
# remove the duplicate KAZ
# others are pretty random, just a few

#remove 1 copy of duplicates
new.Curate.FULL <- new.Curate.FULL %>% distinct()


#remove rows I haven't finalized
new.Curate.FULL <- new.Curate.FULL[ !grepl("[?]", new.Curate.FULL$SC.pass) , ]
new.Curate.FULL <- new.Curate.FULL[ !grepl("[?]", new.Curate.FULL$hand.foci.count) , ]

new.Curate.FULL <- new.Curate.FULL.org[(new.Curate.FULL.org$SC.pass == 1 | new.Curate.FULL.org$SC.pass == 0),]
new.Curate.FULL <- droplevels(new.Curate.FULL)

new.Curate.FULL <- add_mouse(new.Curate.FULL)
new.Curate.FULL <- add_category(new.Curate.FULL)

pass.fail.table <- table(new.Curate.FULL$SC.pass, new.Curate.FULL$category)
#this outsput shows the relative ratios of pass to fail biv segmentation

#DF to use, sc pass
Curated_BivData <- new.Curate.FULL[new.Curate.FULL$SC.pass == 1,] #26,847 to 3,602
Curated_BivData <- droplevels(Curated_BivData)
#5680

#write the curated file
write.table(Curated_BivData, "~./MLH1repo/data/clean_BivData_10.1.20.csv", sep=",", row.names = FALSE)

Curated_BivData <- add_mouse(Curated_BivData)
Curated_BivData <- add_category(Curated_BivData)
Curated_BivData <- add_strain(Curated_BivData)
Curated_BivData <- add_sex(Curated_BivData)
Curated_BivData <- add_subsp(Curated_BivData)

#current remake, SKIVE male, MSM female, (3MSM male), PWD female
#made this new one by hand 
#merging old curated lists with the new ones might be more inclusive

#change everything to Curated_BivData (adjust all the factor-numbers)

##change factor cols to numbers (factor -> chr -> number)
Curated_BivData$centromere_ABS_Position <- as.character(Curated_BivData$centromere_ABS_Position)
Curated_BivData$centromere_PER_Position <- as.character(Curated_BivData$centromere_PER_Position)
Curated_BivData$chromosomeLength <- as.character(Curated_BivData$chromosomeLength)

Curated_BivData$hand.foci.count <- as.character(Curated_BivData$hand.foci.count)

Curated_BivData$Foci1 <- as.character(Curated_BivData$Foci1)
Curated_BivData$Foci2 <- as.character(Curated_BivData$Foci2)
Curated_BivData$Foci3 <- as.character(Curated_BivData$Foci3)

##
Curated_BivData$centromere_ABS_Position <- as.numeric(Curated_BivData$centromere_ABS_Position)
Curated_BivData$centromere_PER_Position <- as.numeric(Curated_BivData$centromere_PER_Position)
Curated_BivData$chromosomeLength <- as.numeric(Curated_BivData$chromosomeLength)

Curated_BivData$hand.foci.count <- as.numeric(Curated_BivData$hand.foci.count)

Curated_BivData$Foci1 <- as.numeric(Curated_BivData$Foci1)
Curated_BivData$Foci2 <- as.numeric(Curated_BivData$Foci2)
Curated_BivData$Foci3 <- as.numeric(Curated_BivData$Foci3)
#I think the number of sig figs and rounding changes slightly from the blocks above

#adding more feature calculations (IFD, ect)
Curated_BivData <- add_IFD(Curated_BivData)
Curated_BivData$IFD1_PER = Curated_BivData$IFD1_ABS / Curated_BivData$chromosomeLength
#IFD.2 does mulitple IFDs (just absolute)
Curated_BivData <- add_IFD.2(Curated_BivData)

Curated_BivData$PER_Foci_1 <- Curated_BivData$Foci1 / Curated_BivData$chromosomeLength
Curated_BivData$PER_Foci_2 <- Curated_BivData$Foci2 / Curated_BivData$chromosomeLength
Curated_BivData$PER_Foci_3 <- Curated_BivData$Foci3 / Curated_BivData$chromosomeLength

Curated_BivData$IFD1 <- as.numeric(Curated_BivData$IFD1)
Curated_BivData$IFD2 <- as.numeric(Curated_BivData$IFD2)
Curated_BivData$IFD3 <- as.numeric(Curated_BivData$IFD3)

#rbar (this function isn't working) (doesn't code for 4 foci, but removing this didn't fix it)
#master_curated_BivData <- master_curated_BivData[!master_curated_BivData$hand.foci.count == 4,]
#master_curated_BivData <- calq.intra.rbar(master_curated_BivData)

#siscoten 
#Curated_BivData <- add_SisCoTen(Curated_BivData)

#distance to cent  foci1 - centromere_ABS_Position
Curated_BivData$dis.cent <- Curated_BivData$Foci1 - Curated_BivData$centromere_ABS_Position
Curated_BivData$dis.cent.PER  <- Curated_BivData$dis.cent / Curated_BivData$chromosomeLength

#this metric is harder, since I need to define the max foci number
#Curated_BivData$dis.telo <- 

#remove the few odd rows with incorrect hand.foci
#for some reason this makes a bunch of NAs
#Curated_BivData <- Curated_BivData[Curated_BivData$hand.foci.count < 5, ]

#some of the obj.ID are missing
Curated_BivData$Obj.ID <- paste(Curated_BivData$fileName, Curated_BivData$boxNumber, sep = "_")


cells.BivData_table <- ddply(.data=Curated_BivData, 
                            .(fileName),
                            summarize, 
                            ncells = length(unique(fileName)),
                            nbivs = length(unique(Obj.ID))
)

cells.BivData_table <- add_mouse(cells.BivData_table)
cells.BivData_table <- add_category(cells.BivData_table)

#hist(cells.BivData_table$nbivs)#shows the number of distribution of bivalents for all cells


#save Rdata
save.image("data/CleanBivData.RData")
#save.image("data/BivData_4_PCA.RData")



#CLEAN UP the STUFF below



#a couple graphs
#hist(master_curated_BivData$dis.cent)

#g.dist.to.cent <- ggplot(master_curated_BivData, aes(dis.cent)) + geom_histogram() +facet_wrap(~ category)
#LEW male and KAZ male have much broader distribution for distance from centr
#PWD, -- and most females have left biased distribution

#g.dist.to.cent.poin <- ggplot(master_curated_BivData, aes(x=dis.cent, y=chromosomeLength, color= as.factor(hand.foci.count)  ))+
#  geom_point(aes(alpha=.5)) +facet_wrap(~ category)




full.cells <- cell.BivData_table[cell.BivData_table$nbivs > 15,]
#I would need to go and varify these cells...
master_curated_BivData$IFD1 <- as.numeric(master_curated_BivData$IFD1)

master_curated_BivData <- add_mouse(master_curated_BivData)
master_curated_BivData <- add_strain(master_curated_BivData)
master_curated_BivData <- add_category(master_curated_BivData)

master_curated_BivData <- add_category(master_curated_BivData)

master_curated_BivData <- master_curated_BivData[!(is.na(master_curated_BivData$hand.foci.count) | master_curated_BivData$hand.foci.count==0), ]






model <- glm(dis.cent ~ mouse+hand.foci.count+chromosomeLength+centromere_ABS_Position+centromere_PER_Position+strain, data=bivs.male)
#for simple model, CO number, chrm length and cent position are good predictors

model99 <- glm(hand.foci.count ~ mouse+dis.cent+chromosomeLength+centromere_ABS_Position+centromere_PER_Position+strain, data=bivs.male)


mean.cent.dis.KAZ <- ddply(.data=bivs.KAZ.male, 
                                  .(hand.foci.count),
                                  summarize, 
                           Nbiv = length(Obj.ID), 
                           mean.dist_cent = mean(dis.cent, na.rm=TRUE),
                           var.dist_cent = var(dis.cent, na.rm=TRUE),
                           sd.dist_cent   = round(sd(dis.cent, na.rm=TRUE), 3),
                           se.dist_cent   = round(sd.dist_cent / sqrt(Nbiv), 3)
)
#mean.cent.dis.PWD




#think of summarizing the image level first -- nboxes per image
#I need to seperate out the 0,12, and 4CO

cell.level.BivData_table <- ddply(.data=master_curated_BivData, 
                       .(fileName),
                       summarize, 
                       boxs.IDd = max(boxNumber),
                       nboxes_passed = length(boxNumber),
                       
                       Nbiv = length(Obj.ID), #if there are na's below -- it breaks
                       CO0 =  sum(hand.foci.count == 0, na.rm=TRUE), 
                       CO1 =  sum(hand.foci.count == 1, na.rm=TRUE), 
                       CO2 = sum(hand.foci.count == 2, na.rm=TRUE),
                       CO3 = sum(hand.foci.count == 3, na.rm=TRUE),
                       
                       ncells = length(unique(fileName)),
                       nbivs = length(unique(Obj.ID)),
                       
                       mean.SC_length = mean(chromosomeLength),
                       var.SC_length = var(chromosomeLength),
                       sd.SC_length   = round(sd(chromosomeLength), 3),
                       se.SC_length   = round(sd.SC_length / sqrt(nbivs), 3),
                       
                       #add the centromere and ifd here?
                       mean.dist_cent = mean(dis.cent, na.rm=TRUE),
                       var.dist_cent = var(dis.cent, na.rm=TRUE),
                       sd.dist_cent   = round(sd(dis.cent, na.rm=TRUE), 3),
                       se.dist_cent   = round(sd.dist_cent / sqrt(nbivs), 3),
                       
                       mean.siscoten = mean(SisCoTen, na.rm=TRUE),
                       var.siscoten = var(SisCoTen, na.rm=TRUE),
                       sd.siscoten   = round(sd(SisCoTen, na.rm=TRUE), 3),
                       se.siscoten   = round(sd.siscoten / sqrt(nbivs), 3),
                       
                       mean.IFD = mean(IFD1, na.rm=TRUE),#IFD1 is chr
                       var.IFD = var(IFD1, na.rm=TRUE),
                       sd.IFD   = round(sd(IFD1, na.rm=TRUE), 3),
                       se.IFD   = round(sd.IFD / sqrt(nbivs), 3),
                       
                       mean.IFDper = mean(IFD1_PER, na.rm=TRUE),#IFD1 is chr
                       var.IFDper = var(IFD1_PER, na.rm=TRUE),
                       sd.IFDper   = round(sd(IFD1_PER, na.rm=TRUE), 3),
                       se.IFDper   = round(sd.IFDper / sqrt(nbivs), 3)
                       
)
cell.level.BivData_table <- add_mouse(cell.level.BivData_table)
cell.level.BivData_table <- add_strain(cell.level.BivData_table)
cell.level.BivData_table <- add_category(cell.level.BivData_table)

lw.male <- cell.level.BivData_table[cell.level.BivData_table$category == "LEW male",]

#--what should I draw from the cell.level. table
#Isolated specific category for these plots
SC.length.points.cells <- ggplot(lw.male, aes(x=mean.dist_cent, y=mean.SC_length, color= as.factor(mouse) ))+
  geom_jitter()+ 
  geom_errorbar(aes(ymin=mean.SC_length-se.SC_length, ymax=mean.SC_length+se.SC_length), width=.2,position=position_dodge(0.05))+
  facet_wrap(~ mouse) + ggtitle("")


SC.length.points.cells <- ggplot(lw.male, aes(x=mean.dist_cent, y=mean.SC_length, color= as.factor(mouse) ))+
  geom_jitter()+ 
  geom_errorbar(aes(ymin=mean.SC_length-se.SC_length, ymax=mean.SC_length+se.SC_length), width=.2,position=position_dodge(0.05))+
  facet_wrap(~ mouse) + ggtitle("")


#within mouse variance, 
BivData_table <- ddply(.data=master_curated_BivData, 
                          .(mouse),
                          summarize, 
                       ncells = length(unique(fileName)),
                       nbivs = length(unique(Obj.ID)),
                          
                       mean.SC_length = mean(chromosomeLength),
                       var.Sc_length = var(chromosomeLength),
                       sd.SC_length   = round(sd(chromosomeLength), 3),
                       se.SC_length   = round(sd.SC_length / sqrt(nbivs), 3),
                          
                       mean.dist_cent = mean(dis.cent, na.rm=TRUE),
                       var.dist_cent = var(dis.cent, na.rm=TRUE),
                       sd.dist_cent   = round(sd(dis.cent, na.rm=TRUE), 3),
                       se.dist_cent   = round(sd.dist_cent / sqrt(nbivs), 3),
                       
                       mean.siscoten = mean(SisCoTen, na.rm=TRUE),
                       var.siscoten = var(SisCoTen, na.rm=TRUE),
                       sd.siscoten   = round(sd(SisCoTen, na.rm=TRUE), 3),
                       se.siscoten   = round(sd.siscoten / sqrt(nbivs), 3),
                       
                       mean.IFD = mean(IFD1, na.rm=TRUE),#IFD1 is chr
                       var.IFD = var(IFD1, na.rm=TRUE),
                       sd.IFD   = round(sd(IFD1, na.rm=TRUE), 3),
                       se.IFD   = round(sd.IFD / sqrt(nbivs), 3),
                       
                       mean.IFDper = mean(IFD1_PER, na.rm=TRUE),#IFD1 is chr
                       var.IFDper = var(IFD1_PER, na.rm=TRUE),
                       sd.IFDper   = round(sd(IFD1_PER, na.rm=TRUE), 3),
                       se.IFDper   = round(sd.IFDper / sqrt(nbivs), 3)
                       
                       
)
#use this to remove mice with too few cells, remove all mice below 9 cells
#var and mean SC length problematic
#I need the low bin! function
#males PWD, MSM have very low dist to cent, except kaz males

#I need a way to get lowes 25
BivData_table.clean = BivData_table[BivData_table$ncells > 9,]
BivData_table.clean <- add_strain(BivData_table.clean)
BivData_table.clean <- add_category(BivData_table.clean)
BivData_table.clean <- add_sex(BivData_table.clean)

SC.length.points <- ggplot(BivData_table.clean, aes(x=nbivs, y=mean.SC_length, color= as.factor(strain) ))+
  geom_jitter()+ 
  geom_errorbar(aes(ymin=mean.SC_length-se.SC_length, ymax=mean.SC_length+se.SC_length), width=.2,position=position_dodge(0.05))+
  facet_wrap(~ sex) + ggtitle("")

Dist_cent.points <- ggplot(BivData_table.clean, aes(x=sex, y=mean.dist_cent, color= as.factor(strain) ))+
  geom_jitter()+ 
  geom_errorbar(aes(ymin=mean.dist_cent-se.dist_cent, ymax=mean.dist_cent+se.dist_cent), width=.2,position=position_dodge(0.05))+
  facet_wrap(~ strain)+ ggtitle("Mean distance to centromere by Mouse, +/- se")


IFD.points <- ggplot(BivData_table.clean, aes(x=sex, y=mean.IFD, color= as.factor(strain) ))+
  geom_jitter()+ 
  geom_errorbar(aes(ymin=mean.IFD-se.IFD, ymax=mean.IFD+se.IFD), width=.2,position=position_dodge(0.05))+
  facet_wrap(~ strain)+ ggtitle("Mean Sis-co-ten by Mouse, +/- se")


IFDper.points <- ggplot(BivData_table.clean, aes(x=sex, y=mean.IFDper, color= as.factor(strain) ))+
  geom_jitter()+ 
  geom_errorbar(aes(ymin=mean.IFDper-se.IFDper, ymax=mean.IFDper+se.IFDper), width=.2,position=position_dodge(0.05))+
  facet_wrap(~ strain)+ ggtitle("Mean norm IFD by Mouse, +/- se")



siscoten.points <- ggplot(BivData_table.clean, aes(x=sex, y=mean.siscoten, color= as.factor(strain) ))+
  geom_jitter()+ 
  geom_errorbar(aes(ymin=mean.siscoten-se.siscoten, ymax=mean.siscoten+se.siscoten), width=.2,position=position_dodge(0.05))+
  facet_wrap(~ strain)+ ggtitle("Mean Sis-co-ten by Mouse, +/- se")


#geom_errorbar(aes(ymin=len-sd, ymax=len+sd), width=.2,
#position=position_dodge(0.05))


blah.blah <- ggplot(BivData_table.clean, aes(x=mean.dist_cent, y=nbivs, color= as.factor(strain) )) +geom_point()+facet_wrap(~ sex)
#LEW male and KAZ male have much broader distribution for distance from centr
#PWD, -- and most females have left biased distribution

#KAZ have foci further from centromere (on average), but doesn't seem as far as LEW (what about SKIVE)

blah.blah22 <- ggplot(BivData_table.clean, aes(x=mean.dist_cent, y=mean.siscoten, color= as.factor(strain) )) +
  geom_point()+facet_wrap(~ sex)
#plot the variance / se


blah.blah22var <- ggplot(BivData_table.clean, aes(x=var.Sc_length, y=nbivs, color= as.factor(strain) )) +geom_point()+facet_wrap(~ sex)
#male variance in SC length has pretty small range



blah.blah99 <- ggplot(BivData_table.clean, aes(x=mean.siscoten, y=mean.SC_length, color= as.factor(strain) )) +geom_point()+facet_wrap(~ sex)
#females have greater sis-co-ten
#sis - co - ten is correlated with SC length
#does the correlation change by hand foci number?


g.dist.to.cent.poin <- ggplot(master_curated_BivData, aes(x=dis.cent, y=chromosomeLength, color= as.factor(hand.foci.count)  ))+
  geom_point(aes(alpha=.5)) +facet_wrap(~ category)

boxplots <- ggplot(BivData_table.clean, aes())




cell_biv_table <- ddply(mstr_manual_data, c("strain", "sex", "image.title"), summarise,
                        Nbiv = length(manual.blobject.name),
                        CO0 =  sum(blobjectClass == "0CO"),
                        CO1 =  sum(blobjectClass == "1CO"), 
                        CO2 = sum(blobjectClass == "2CO"),
                        CO3 = sum(blobjectClass == "3CO"),
                        totalCO = (CO1+(CO2*2)+(CO3*3)),
                        IFD.mean = mean(as.numeric(IFD), na.rm=TRUE),
                        AutoSC = sum( SC.length[which(blobjectClass != "XY")] ),
                        AutoSC.mean = mean(SC.length[which(blobjectClass != "XY")]),
                        AutoSC.sd = sd(SC.length[which(blobjectClass != "XY")]),
                        AutoSC.se = AutoSC.sd / Nbiv,
                        XY = mean(sum( SC.length[which(blobjectClass == "XY")] ) ),
                        SC.CO = AutoSC / totalCO ,
                        Xguess = SC.length[(rank(SC.length)[3])],
                        XovrAuto = (Xguess / AutoSC)*100
                        
)
cell_biv_table

#check dissection list agaisnt the BivData list
Dissection_list = read.csv("data/mouseDissections.csv", sep = ",", header = TRUE)

#check for complete data (which mice are missing)
#add IFD -- add age?

#####################
# REMOVE EXTRA DATA #
#####################
#remove data which has paint marks
P_fileNames <- fullBivData$fileName[(grep('p_rev', fullBivData$fileName))]
length(P_fileNames)#187

prev_num = length(P_fileNames)
fullBivData <- fullBivData[!(fullBivData$fileName %in% P_fileNames), ]

#remove anything with 'DUPLICATE'
DUP_list <- fullBivData$fileName[(grep('DUP', fullBivData$fileName))] 
DUP_num = length(DUP_list)#0
fullBivData <- fullBivData[!(fullBivData$fileName %in% DUP_list), ]

#remove images that would be doubles ... 12, 12.1, 12.2
#this isn't completely effective
dup_list= c()
for(i in fullBivData$fileName){  #change the file name header,  
  # print(i)
  ifelse(grepl("\\.[0-9]_rev", i),
         (dup_list = c(dup_list, i)), NA)
}
MultiCells = c()
for(j in dup_list){
  stringLIST = strsplit(j, split="\\.")[[1]]
  new_string = paste(stringLIST[[1]][1], "rev", sep="_")
  MultiCells = c(MultiCells, new_string)
}
MultiCells = unique(MultiCells)
multi_cell_num = length(MultiCells)
#test this part of the function
fullBivData <- fullBivData[!(fullBivData$fileName %in% MultiCells), ]
#save these number measures to report


#mice with good stains
good_stain_mice = c("20dec16_LEW_m1", "20dec16_LEW_m2", "20dec16_LEW_m3", "8may17_LEW_m1","8may17_LEW_m2",
         "13nov16_LEWES_m1","13jan17_LEW_m1","13nov16_MSM_f1","13nov16_MSM_f2","14jul17_SPRET_f3",
         "13nov16_MSM_m1","13nov16_MSM_m2","30may17_MSM_m1","30sep16_MSM_f2",  "30may17_SPRET_m1",  "22jun15_G_m2",
         "30dec14_WSB_m2","17mar16_G_f4","17mar16_G_f5","4jan17_LEW_f1","4jan17_LEW_f3","4jan17_LEW_f6",
         "4jan17_LEW_f7","4jan17_LEW_m1", "3jan16_G_m2")

bad_stain_mice = c("13jan17_LEW_f3","12jun15_WSB_m1","12sep16_MSM_f3","14jul17_SPRET_f2","13jan17_LEW_f2",
        "30may17_SPRET_f1","30dec14_WSB_m4","31jul17_HMI_m1","30dec14_WSB_m5","16jun15_WSB_f4","30dec14_WSB_m1",
        "30dec16_MSM_m1","14jul17_LEW_f1","1mar17_CAST_f1")


bad_stain_mice_len <- length(BivData[BivData$mouse %in% bad_stain_mice, ])
#remove bad mice
#BivData <- BivData[BivData$mouse %in% good_stain_mice, ]
#remove data without mouse
fullBivData <- fullBivData[!(is.na(fullBivData$mouse) ), ]

#remove chrms from images with too many boxes ID'd(>40)
#ID images with >50 boxNumbers 

rawBivData = BivData
rawBivData$fileName[max(rawBivData$boxNumber)]

RawDataTable <- as.data.frame(colSums(!is.na(BivData)) )
#print .txt file
#write.table(RawDataTable, "results/RawDataTable.txt", sep="\t")




##############
# CLEAN DATA #
##############

#remove data with foci >7
num_7foci = length(BivData[(BivData$numberCrossOvers < 7),])
BivData <- BivData[(BivData$numberCrossOvers < 7), ]#2170 removed

#remove extra columns (make this automated..)
BivData <- BivData[ -c(26:127) ]#this should many extra columns
#remove chrms with centromere_PER over 10% OR >20 pixels away
BivData <- BivData[(BivData$centromere_ABS_Position < 20), ]
BivData <- BivData[(BivData$centromere_PER_Position < .2), ] #removes ~1000 when both applied

#(remove 'other' sex) #don't call this when strain and sex assignment isn't working
#BivData <- BivData[!(BivData$strain == "other"), ]
#BivData <- BivData[!(BivData$sex == "other"), ]

source("src/Func_CheckFociOrder.R")
BackwardsChrms_BivData <- Check.Foci.Order(BivData)
nBackwardsChrms_BivData <- length(BackwardsChrms_BivData$wrong.order.image)

#this shouldn't need to be fixed... since I ran this on rawBivData
FixedFoci_objBivD <- FixFociOrder(BivData)
BivData <- FixedFoci_objBivD$DF

checkFO <- length(Check.Foci.Order(BivData)$wrong.order.image)

#centromere - associated foci
#how do I change the data? (delete the cells)


#produce cell and chrm level stats


#make image/cell level table from cleaned DF

#add total COs
BivData_cells <- ddply(fullBivData, c("fileName"), summarise,
                            
                    boxs.IDd = max(boxNumber),
                    nboxes_passed = length(boxNumber),
                    pass_rate = nboxes_passed/boxs.IDd,
                  
                    
                    totalCO = sum(numberCrossOvers),     
                    avCO.per.CHRM = mean(numberCrossOvers),
                    varCO.per.CHRM = var(numberCrossOvers),
                            
                    avSC.intensity = mean(avergeRED, na.rm = TRUE),
                    varSC.intensity = var(avergeRED, na.rm = TRUE),
                            
                    avSC.length = mean(chromosomeLength),
                    varSC.length = var(chromosomeLength),
                            
                    av.COdensity = mean(chromosomeLength /numberCrossOvers),
                    varCOdensity = var(chromosomeLength /numberCrossOvers),
                            #av CO density
                    #change the mean list for column numbers 
                    avFoci.intensity = mean(c(foci_intensity_1,foci_intensity_2,foci_intensity_3,foci_intensity_4,foci_intensity_5,foci_intensity_6),
                                      na.rm = TRUE),  
                    varFoci.intensity = var(c(foci_intensity_1,foci_intensity_2,foci_intensity_3,foci_intensity_4,foci_intensity_5,foci_intensity_6),
                                        na.rm = TRUE) 
)

BivData_cells <- add_mouse(BivData_cells)
BivData_cells <- add_category(BivData_cells)
BivData_cells <- add_strain(BivData_cells)
BivData_cells <- add_sex(BivData_cells)


#Histograms
numb.box <- ggplot(data =BivData_cells, aes(x=boxs.IDd) )+ geom_histogram()+ggtitle("Box Number Histogram")

pass.num <- ggplot(data =BivData_cells, aes(x=nboxes_passed) )+ geom_histogram()+
  ggtitle("Pass Number Histogram")

pass.rate <- ggplot(data =BivData_cells, aes(x=pass_rate) )+ geom_histogram()+
  ggtitle("Pass Rate Histogram")



#push the count numbers into a df?
save.image("data/MLH1_BivData.RData")



#list of images with >30 boxes id'd, remove those
tooBigImage_list <- BivData_cells$fileName[(BivData_cells$boxs.IDd >= 30)]
betterData <- BivData[ ! BivData$fileName %in% tooBigImage_list, ]


#list of images with av-chrm.nCOs >5 (these are likely false chrms )
#chrm level doesn't need to be run on full dataset
#play_BivData_Chrms <- ddply(play_BivData, c("fileName", "boxNumber"), summarise,

#png(filename="results/cleanData_plots.png", width =1200, height =500)#good dem for 1,3
#par(mfrow=c(1,3))

cent_ABS_cplot <- plot(fullBivData$centromere_ABS_Position, fullBivData$chromosomeLength, 
                  main ="ABS cent position", xlab = "ABS cent pos", ylab = "SC Length" )

cent_PER_cplot <- plot(fullBivData$centromere_PER_Position, fullBivData$chromosomeLength, 
                      main ="PER cent position", xlab = "PER cent pos", ylab = "SC Length" )

FociCount_chist <-hist(fullBivData$numberCrossOvers, main= "Foci Count Hist",breaks = 5)
dev.off()


#histograms
png(filename="results/cleanData_hist.png", width =1200, height =500)#good dem for 1,3
par(mfrow=c(1,2))

raw_FC_hist <- hist(rawBivData$numberCrossOvers,
               main ="raw Foci Count Distrb", xlab = "FociCount", ylab = "", sub="\n \n median blue, mean red")
abline(v=median(rawBivData$numberCrossOvers, na.rm = TRUE), col="blue")
abline(v=mean(rawBivData$numberCrossOvers, na.rm = TRUE), col="red")

clean_FC_hist <- hist(BivData$numberCrossOvers, breaks = 7,
                    main ="cleaned Foci Count Distrb", xlab = "FociCount", ylab = "", sub="\n \n median blue, mean red")

abline(v=median(BivData$numberCrossOvers, na.rm = TRUE), col="blue")
abline(v=mean(BivData$numberCrossOvers, na.rm = TRUE), col="red")
dev.off()


master_fullBivData = read.csv("data/BivData/MUS_FULL_BIVDATA_10.11.19.csv")
#33286 obs
#+PWD and KAZ females 
#+PWD (male)
#+KAZ (male)
#+LEW (female-male)

#add obj.ID
master_fullBivData$Obj.ID <- paste(master_fullBivData$fileName, master_fullBivData$boxNumber, sep = "_")

#some of the images have bad fileNames, -- so those should be removed, or Idk, checked
#removed, PWD_f1_sp1  and 8oct14_PWDf2_sp1
#not sure where these files/images are

anyDuplicated(master_fullBivData$Obj.ID) #I think these are lines, no duplicated -- just the header line
master.full.dup <- master_fullBivData[duplicated(master_fullBivData$Obj.ID),]

anyDuplicated(master_curated_BivDatalist$Obj.ID)#2546 is line number duplicated in master curated
dup.curated <- master_curated_BivDatalist[duplicated(master_curated_BivDatalist$Obj.ID),]

orig_DF <- original_DF[duplicated(original_DF$fileName),]
#re-write the excel sheet for unique observations
#mark which have been curated and which havent

#making full bivData --> then requires a curate list (filtered from) Bivdata
#just use the curate dataset (if I get into predicting bivalent passing -- then merge the DFs)


hand.measure.cells <- c("12jan17_4jan17_LEW_f1_sp1_22_rev",  "13jul15_12jun15_WSB_f1_sp1_13_rev",  "14oct16_30jun16_CAST_m3_sp1_3.2",  "15sep15_16jun15_WSB_f3_sp2_1_rev",
  "18mar15_10mar15_WSB_m1_sp1_13_rev",  "1mar16_3jan16_G_m2_sp1_19.3_rev",  "1oct16_30jun16_CAST_m3_sp1_m3_5.1",
  "20jul15_18may15_PWD_m1_sp1_2.2_rev",  "20jul15_18may15_PWD_m1_sp1_6.1_rev",  "2aug15_8jun15_G_f4_sp1_18_rev",  "2aug15_8jun15_G_f4_sp1_7_rev",
  "30mar15_13oct14_G_f1_sp1_1.1_rev",  "27jul15_22jun15_G_m2_sp1_8_rev",  "28jul15_22jun15_G_m2_sp1_19_rev",
  "28jul15_22jun15_G_m2_sp1_3_rev","13jul15_12jun15_WSB_f1_sp1_13_rev","14oct16_30jun16_CAST_m3_sp1_3.2","15sep15_16jun15_WSB_f3_sp2_1_rev",
"18mar15_10mar15_WSB_m1_sp1_13_rev","1mar16_3jan16_G_m2_sp1_19.3_rev","1oct16_30jun16_CAST_m3_sp1_m3_5.1",
"2aug15_8jun15_G_f4_sp1_18_rev","2aug15_8jun15_G_f4_sp1_7_rev","30mar15_13oct14_G_f1_sp1_1.1_rev","27jul15_22jun15_G_m2_sp1_8_rev","28jul15_22jun15_G_m2_sp1_19_rev",
"28jul15_22jun15_G_m2_sp1_3_rev","12jan17_4jan17_LEW_f1_sp1_22_rev","3aug17_8may17_LEW_m2_sp1_3_rev","3aug17_8may17_LEW_m2_sp1_5_rev",
"7feb17_20dec16_LEW_m2_sp1_20_rev","7feb17_20dec16_LEW_m2_sp1_25_rev",
"13jul15_12jun15_WSB_f1_sp1_13_rev","15sep15_16jun15_WSB_f3_sp2_1_rev","2apr15_14sep14_WSB_f1_sp1_15_rev",
"18mar15_10mar15_WSB_m1_sp1_13_rev","30jul15_12jun15_WSB_m2_sp1_3_rev","30jul15_12jun15_WSB_m2_sp1_7.2_rev",
"4dec14_8oct14_PWD_f1_sp1_12_rev","4dec14_8oct14_PWD_f1_sp1_16_rev","8jun15_23apr15_PWD_f2_sp1_20_rev","20jul15_18may15_PWD_m1_sp1_2.2_rev",
"20jul15_18may15_PWD_m1_sp1_6.1_rev","22mar15_10mar15_PWD_m2_sp1_15_rev","22nov16_13nov16_MSM_f2_sp1_18","22nov16_13nov16_MSM_f2_sp1_19",
"22nov16_30sep16_MSM_f2_sp1_13.1_rev","22nov16_30sep16_MSM_f2_sp1_22_rev","29nov16_13nov16_MSM_f1_sp1_24.1_rev",
"29nov16_13nov16_MSM_f1_sp1_5","27oct16_31aug16_MSM_m1_sp1_11","27oct16_31aug16_MSM_m1_sp1_18_rev","27oct16_31aug16_MSM_m1_sp1_26",
"27oct16_31aug16_MSM_m1_sp1_9_rev","29nov16_13nov16_MSM_m2_sp1_13")

#these won't perfectly match the MLH1 data ... (rev.tif)

blahy <- data.frame(fileName = hand.measure.cells)

#add the rev .tif 
blahy <- add_fullFileName(blahy)
#missing stain info 

#write out the subseted MLH1 data for the hand measured ones
fullBivData <- duplicated[!(duplicated$fileName %in% P_fileNames), ]



####################
# SAVE R DATA FILE #
####################
#rm values that I shouldn;t need
rm(DUP_list)

#push the count numbers into a df?
save.image("data/BivData_date.RData")

#save.image("data/returnPaper_NEW_BivData.RData")
