# make curate list from raw BivData
# input: BBIIGG type bivdata file (raw bivdata from DNAcrossover)
# output: new file for manual curation

setwd("~./MLH1repo/")

#in.progress.Lew_BivData = read.csv("results/LEW_forcuration.csv", header=TRUE )
#~500 scored in ~5 hrs

#FULL_BivData = read.csv("data/BivData/MUS_FULL_BIVDATA_11.14.19.csv", header=TRUE ) #10.25.19, 41499
#keep track of which mice have bivalent data and what still needs to be run
#focus on getting female bivdata (PWD and KAZ)


#FULL.BIVDATA FILES are too big -- must do with smaller batches
#DEALING With the new BivData files
#FULL_BivData_REDO2 = read.csv("data/BivData/FULL_MERGED_BIVDATA.csv", header=TRUE )
#FULL_BivData_REDO = read.csv("data/BivData/FULL_MERGED_BIVDATA.csv", header=TRUE )


#11.16.19 6pm - full merged bivData, 1,048,576 
#7pm 1,048,576
#THIS IS a very loong set to up dated this
#check all of the repeats so that it speeds up in the future
WSB_BivData = read.csv("data/BivData/algorithm\ output/WSB_output111419_BBIIGG_11.16.19.csv", header=TRUE )
WSB_BivData$Obj.ID <- paste(WSB_BivData$fileName, WSB_BivData$boxNumber, sep = "_")

G_BivData = read.csv("data/BivData/algorithm\ output/G_output111419_BBIIGG_11.16.19.csv", header=TRUE )
G_BivData$Obj.ID <- paste(G_BivData$fileName, G_BivData$boxNumber, sep = "_")


#CZECH
CZECH_BivData = read.csv("data/BivData/algorithm\ output/SKIVE_CZECH_BBIIGG_11.15.19.csv", header=TRUE )
CZECH_BivData$Obj.ID <- paste(CZECH_BivData$fileName, CZECH_BivData$boxNumber, sep = "_")

#10.1.19
BBIIGG_phytomorphor_4.9.19_moreFULL

moreFULL_BivData = read.csv("data/BivData/curation/BBIIGG_phytomorphor_4.9.19_moreFULL.csv", header=TRUE )
moreFULL_BivData$Obj.ID <- paste(moreFULL_BivData$fileName, moreFULL_BivData$boxNumber, sep = "_")

#check for duplicate lines
anyDuplicated(moreFULL_BivData$Obj.ID)
the.dups <- moreFULL_BivData[duplicated(moreFULL_BivData$Obj.ID),]
#I think just the headers usually are duplicated

#don't need to add these cols if taken from single batches
source("~./MLH1repo/src/CommonFunc_MLH1repo.R")
moreFULL_BivData <- add_mouse(moreFULL_BivData)
moreFULL_BivData <- add_strain(moreFULL_BivData)
moreFULL_BivData <- add_subsp(moreFULL_BivData)
moreFULL_BivData <- add_sex(moreFULL_BivData)
moreFULL_BivData <- add_category(moreFULL_BivData)

table(moreFULL_BivData$strain)#only 1 strain
#more.FULL. WSB, LEW, PWD, MSM, KAZ, SPRET

#SKIVE_bivData <- FULL_BivData_REDO[FULL_BivData_REDO$strain == "SKIVE",]
moreFULL_BivData.WSB <- moreFULL_BivData[moreFULL_BivData$strain == "WSB",]
#keep MSM male / since female seems to be complete
#MSM_male_BivData <- FULL_BivData_REDO[FULL_BivData_REDO$category == "MSM male",]

#filter by centromere
moreFULL_BivData.WSB$centromere_PER_Position <- as.character(moreFULL_BivData.WSB$centromere_PER_Position)
moreFULL_BivData.WSB$centromere_PER_Position <- as.numeric(moreFULL_BivData.WSB$centromere_PER_Position)
moreFULL_BivData.WSB <- moreFULL_BivData.WSB[moreFULL_BivData.WSB$centromere_PER_Position < 0.2,]

#remove extra cols  (conservative  9:80)
moreFULL_BivData.WSB <- moreFULL_BivData.WSB[ -c(9:70) ]

#test is 14dec17_18nov17_WSB_f3_sp1_10_rev_11, old xcel file has 16, 
#there are 17 .csv files in the main output folder
#newest BBIIGG also has 16 after running through this script
#i am likely forgetting the removal of bad bivs --- I might have been worried for nothing


#integrate WSB female into the Master Curation

#reminder -- makeing the masterCuration file is doen through makefile
#compile a bunch of csv files grep 'curated'

CZECH_BivData$SC.pass <- ""
CZECH_BivData$foci.pass<- ""
CZECH_BivData$curated.1pass.0fail<- ""
CZECH_BivData$curation.notes<- ""
CZECH_BivData$hand.foci.count	<- ""
CZECH_BivData$Foci1	<- ""
CZECH_BivData$Foci2<- ""
CZECH_BivData$Foci3<- ""
CZECH_BivData$notes<- ""

#CZECH
write.table(CZECH_BivData, "~./MLH1repo/data/BivData/curation/redo_CZECH_11.19.19.csv", sep=",", row.names = FALSE)


#WSB
write.table(G_BivData, "~./MLH1repo/data/BivData/curation/G_for_curation_11.18.19.csv", sep=",", row.names = FALSE)

#write to file for making manual notes
write.table(CZECH_bivData, "~./MLH1repo/results/CZECH_for_curation_10.25.19.txt", sep="\t", row.names = FALSE)
write.table(SKIVE_bivData, "~./MLH1repo/results/SKIVE_for_curation_11.15.19.txt", sep="\t", row.names = FALSE)

#MSM sheet
write.table(MSM_male_BivData, "~./MLH1repo/results/newMSM_for_curation_11.16.19.txt", sep="\t", row.names = FALSE)


#read in the curated file -- and merge
#NEEDS to fill in the fileName
new.curated.SKIVE = read.csv("data/BivData/SKIVE_new_curation_11.15.19.csv", header=TRUE )
#new file has curated things ... but is missing some of the lines

new.curated.SKIVE$Obj.ID <- paste(new.curated.SKIVE$fileName, new.curated.SKIVE$boxNumber, sep = "_")

newSKIVE_bivData <- FULL_BivData_REDO[FULL_BivData_REDO$strain == "SKIVE",]

newSKIVE_bivData$Obj.ID <- paste(newSKIVE_bivData$fileName, newSKIVE_bivData$boxNumber, sep = "_")

SKIVE.merge <- merge(new.curated.SKIVE, newSKIVE_bivData, by="Obj.ID", all.y = TRUE)

write.table(SKIVE.merge, "~./MLH1repo/results/REDO_SKIVE_curate.csv", sep=",", row.names = FALSE)




#remove mice 13nov16_MSM_m1 and 13nov16_MSM_m2 -- so I don't double curate, I think I avoided running these
#MSM.male_bivData.dup.remove <- MSM.male_bivData[ ! MSM.male_bivData$Obj.ID %in% MSM_bivData$Obj.ID, ]

#write to file for making manual notes
write.table(MSM.male_bivData, "~./MLH1repo/results/MSM_male_for_curation_10.17.19.txt", sep="\t", row.names = FALSE)

write.table(MSM_bivData, "~./MLH1repo/results/MSM_for_curation_10.16.19.txt", sep="\t", row.names = FALSE)
#think about adding ~6 more columns for the curation process


#str(LEW_bivData)

#load/read in fullBivdata file
new_PWD.KAZ_BivData_8.12.19.org = 
  read.csv("~./MLH1repo/data/BivData/PWD_KAZ_female_BivData_8.12.19.csv", header = TRUE)#740

#(PWD-KAZ females), 8.12.19

#extra filters?  extra chrms?  too long chrms?
#20 lower end?
#mus female: consider cutoff at 200
#mus male:

#consider altering the cols -- to make curation easier
#remove all foci position columns


#remove this mouse
#8oct14_PWDf2_sp1
#PWD_f1_sp1


source("~./MLH1repo/src/CommonFunc_MLH1repo.R")
#meta.data <- add_euth_date(meta.data)
#meta.data <- add_age(meta.data)



new_PWD.KAZ_BivData_8.12.19 <- add_mouse(new_PWD.KAZ_BivData_8.12.19)
new_PWD.KAZ_BivData_8.12.19 <- add_strain(new_PWD.KAZ_BivData_8.12.19)
new_PWD.KAZ_BivData_8.12.19 <- add_subsp(new_PWD.KAZ_BivData_8.12.19)
new_PWD.KAZ_BivData_8.12.19 <- add_sex(new_PWD.KAZ_BivData_8.12.19)
new_PWD.KAZ_BivData_8.12.19 <- add_category(new_PWD.KAZ_BivData_8.12.19)


table(new_PWD.KAZ_BivData_8.12.19$mouse)
table(new_PWD.KAZ_BivData_8.12.19$strain)


#good mice
#11jan18_KAZ_f1,f2,f3

hist(new_PWD.KAZ_BivData_8.12.19$chromosomeLength)


#analyze deal with the curate file

PWD.KAZ_BivData_8.14.19_curated = 
  read.csv("~./MLH1repo/results/KAZ.PWD.female_curated.csv", header = TRUE)#740

source("~./MLH1repo/src/CommonFunc_MLH1repo.R")
PWD.KAZ_BivData_8.14.19_curated <- add_mouse(PWD.KAZ_BivData_8.14.19_curated)
PWD.KAZ_BivData_8.14.19_curated <- add_strain(PWD.KAZ_BivData_8.14.19_curated)
PWD.KAZ_BivData_8.14.19_curated <- add_subsp(PWD.KAZ_BivData_8.14.19_curated)
PWD.KAZ_BivData_8.14.19_curated <- add_sex(PWD.KAZ_BivData_8.14.19_curated)
PWD.KAZ_BivData_8.14.19_curated <- add_category(PWD.KAZ_BivData_8.14.19_curated)

PWD.KAZ_BivData_8.14.19_curated_clean <- PWD.KAZ_BivData_8.14.19_curated[!(is.na(PWD.KAZ_BivData_8.14.19_curated$SC.pass) | PWD.KAZ_BivData_8.14.19_curated$SC.pass == ""), ]

table(PWD.KAZ_BivData_8.14.19_curated_clean$strain)
table(PWD.KAZ_BivData_8.14.19_curated_clean$mouse)
#two mice have >100 -- maybe get them so there about equal ~3 more 1feb18_KAZ_f3 cells?
#i'm not sure how many of the total these are

SC.passed <- PWD.KAZ_BivData_8.14.19_curated_clean[PWD.KAZ_BivData_8.14.19_curated_clean$SC.pass == 1,]

table(SC.passed$strain)
table(SC.passed$mouse)
#1feb_KAZ_f1 has way fewer passing SC measures