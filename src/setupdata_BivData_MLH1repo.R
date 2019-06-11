# set up data file for ML data output
# input: BivData 5-29-19
# output: RData file, Table of counts

library(plyr)
library(lattice)
library(dplyr)
library(ggplot2)

#ToDo
#add IFD
#add the melted foci here

setwd("~./MLH1repo/")
fullBivData = read.csv("~./MLH1repo/data/BivData/CLEAN_MUS_FULL_BIVDATA_5.28.19.csv")
#5402

#check dissection list
Dissection_list = read.csv("data/mouseDissections.csv")

#add obj.ID
fullBivData$Obj.ID <- paste(fullBivData$fileName, fullBivData$boxNumber, sep = "_")


source("~./MLH1repo/src/CommonFunc_MLH1repo.R")
fullBivData <- add_mouse(fullBivData)
fullBivData <- add_category(fullBivData)
fullBivData <- add_strain(fullBivData)
fullBivData <- add_sex(fullBivData)
fullBivData <- add_subsp(fullBivData)
#this 5.28.19 dataset is missing so many categories


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


#print out list of mice that have Image folder, but are missing from the BivData




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


####################
# SAVE R DATA FILE #
####################
#rm values that I shouldn;t need
rm(DUP_list)

#push the count numbers into a df?
save.image("data/BivData.RData")

#save.image("data/returnPaper_NEW_BivData.RData")
