# make curate list from raw BivData
# input: BBIIGG type bivdata file (raw bivdata from DNAcrossover)
# output: new file for manual curation


#load/read in fullBivdata file
new_PWD.KAZ_BivData_8.12.19.org = 
  read.csv("~./MLH1repo/data/BivData/PWD_KAZ_female_BivData_8.12.19.csv", header = TRUE)#740

#(PWD-KAZ females), 8.12.19

#delete 'p_rev' files


#filter by centromere
new_PWD.KAZ_BivData_8.12.19 <- new_PWD.KAZ_BivData_8.12.19.org[new_PWD.KAZ_BivData_8.12.19.org$centromere_PER_Position < 0.2,] #665
#14000 bivalents

#add Obj.ID
new_PWD.KAZ_BivData_8.12.19$Obj.ID <- paste(new_PWD.KAZ_BivData_8.12.19$fileName, new_PWD.KAZ_BivData_8.12.19$boxNumber, sep = "_")

#KAZ-PWD female
#remove bad mice


#extra filters?  extra chrms?  too long chrms?
#20 lower end?
#mus female: consider cutoff at 200
#mus male:

#consider altering the cols -- to make curation easier
#remove all foci position columns

new_PWD.KAZ_BivData_8.12.19$SC.pass <- ""
new_PWD.KAZ_BivData_8.12.19$foci.pass<- ""
new_PWD.KAZ_BivData_8.12.19$curated.1pass.0fail<- ""
new_PWD.KAZ_BivData_8.12.19$curation.notes<- ""
new_PWD.KAZ_BivData_8.12.19$hand.foci.count	<- ""
new_PWD.KAZ_BivData_8.12.19$Foci1	<- ""
new_PWD.KAZ_BivData_8.12.19$Foci2<- ""
new_PWD.KAZ_BivData_8.12.19$Foci3<- ""


#remove this mouse
#8oct14_PWDf2_sp1
#PWD_f1_sp1


#write to file for making manual notes
write.table(new_PWD.KAZ_BivData_8.12.19, "~./MLH1repo/results/forCuration_PWD.KAZfemale_8.12.19.txt", sep="\t", row.names = FALSE)
#think about adding ~6 more columns for the curation process


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