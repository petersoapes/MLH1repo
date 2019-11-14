
# make a list of all images and those 

setwd("~./MLH1repo/")

#full list of bivdata
full_Biv.Data = read.csv("data/BivData/CLEAN_MUS_FULL_BIVDATA_5.28.19.csv")
#curated list
full_curated_BivData = read.csv("data/BivData/FULL_Merged_curated_BivData.csv")


full_Biv.Data$Obj.ID <- paste(full_Biv.Data$fileName, full_Biv.Data$boxNumber, sep = "_")


obj.ID.missing <- full_Biv.Data[!(full_Biv.Data$Obj.ID %in% full_curated_BivData$Obj.ID),]

source("src/CommonFunc_MLH1repo.R")

#obj.ID.missing$fileName <- (obj.ID.missing$Original.Name)
obj.ID.missing <- add_mouse(obj.ID.missing)


table(obj.ID.missing$mouse)



