#setup.MetaData File
# input: newest / most recent Dissection csv file
# output: .csv file for reading/loading in other scripts

setwd("C:/Users/alpeterson7/Documents/MLH1repo/")

#load Dissection file

#use updated metadata -- 
#8.30.19
meta.data.file = read.csv("~./MLH1repo/data/DandM_8.30.19.csv", header = TRUE)
#these are mostly clean ... some wrong things in the Maternal_DOB thing
#all none age values removed from the csv file
#had to reformat the dates, since there were inconsistancies in year digit length
#had to clean up some cells

meta.data.file <- meta.data.file[!(is.na(meta.data.file$mouse)|meta.data.file$mouse==""),]
#445? -- probably extras at bottom not good

#how many mice have maternal DOB
meta.data.file.w.mat.DOB <- meta.data.file[!(is.na(meta.data.file$maternal_DOB)|meta.data.file$maternal_DOB==""),]
#151

#format this meta.data.file
source("~./MLH1repo/src/CommonFunc_MLH1repo.R")

meta.data.file.w.mat.DOB <- add_mouse(meta.data.file.w.mat.DOB)
meta.data.file.w.mat.DOB <- add_category(meta.data.file.w.mat.DOB)
meta.data.file.w.mat.DOB <- add_strain(meta.data.file.w.mat.DOB)
meta.data.file.w.mat.DOB <- add_sex(meta.data.file.w.mat.DOB)
meta.data.file.w.mat.DOB <- add_subsp(meta.data.file.w.mat.DOB)

#check if date format's been applied before tryung the age functions
str(meta.data.file.w.mat.DOB)

#DOB and Mat.DOB need to be date formated, make sure that the dates are consistant in the original file
#the dates in my file are not all standardized... year is somethimes 2 - sometimes 4
meta.data.file.w.mat.DOB$DOB <- as.Date(meta.data.file.w.mat.DOB$DOB, format= "%m/%d/%Y")#%Y 2000, %y '18,
meta.data.file.w.mat.DOB$maternal_DOB <-as.Date(meta.data.file.w.mat.DOB$maternal_DOB, format= "%m/%d/%Y")

str(meta.data.file.w.mat.DOB)

meta.data.file.w.mat.DOB <- add_euth_date(meta.data.file.w.mat.DOB)
meta.data.file.w.mat.DOB <- add_age(meta.data.file.w.mat.DOB)
meta.data.file.w.mat.DOB <- add_mat_age(meta.data.file.w.mat.DOB)

#save this data frame... or put into permanate file


#examine data





n_occur <- data.frame(table(meta.data.file$fileName))

#n_occur[n_occur$Freq > 1,] displays the dups
original_DF_duplicates <- meta.data.file[meta.data.file$fileName %in% n_occur$Var1[n_occur$Freq > 1],]

#meta.data.file = read.csv("~./MLH1repo/data/Mouse_MetaData_6.12.19.csv", header = TRUE)#455

#loading previously cleaned data 
#clean.meta.data <- read.csv("~./MLH1repo/data/clean.Meta.Data.txt",header = TRUE, sep='\t')
#don't load this yet
#integrate batch information

#read or load MLH1 data  #load most recent RData file
#load(file="~./MLH1repo/data/MLH1/MLH1_data_setup_8.29.19.RData")#8.29.19 has batch15
#MLH1.data = read.csv("~/MLH1repo/data/MLH1/MLH1_data_setup_8.15.19.RData", header = TRUE) 

#take note when I switch from 2 digit year (mouse, euth date) and 4-digit year 'DOB'!
#remember to switch '/%y' to upper case 'Y'!!

#clean up meta.data.file
meta.data.file
meta.data.file <- meta.data.file[!(is.na(meta.data.file$mouse)|meta.data.file$mouse==""),] #375
meta.data.file$DOB <- as.Date(meta.data.file$DOB, format= "%m/%d/%Y")#%Y 2000, %y '18,


meta.data <- meta.data[!(is.na(meta.data$mouse)|meta.data$mouse==""),] #375
meta.data$DOB <- as.Date(meta.data$DOB, format= "%m/%d/%Y")#%Y 2000, %y '18,

#calculate age ect, add other labels..
#work with meta.data.file

source("~./MLH1repo/src/CommonFunc_MLH1repo.R")
meta.data <- add_euth_date(meta.data)
meta.data <- add_age(meta.data)



meta.data <- add_strain(meta.data)
meta.data <- add_subsp(meta.data)
meta.data <- add_sex(meta.data)
meta.data <- add_category(meta.data)


#subset org_DF for mice in meta_data list 
#original_DF$Batch

original_DF$fileName <- original_DF$Original.Name
original_DF <- add_mouse(original_DF)

org.DF.mice <- original_DF[(original_DF$mouse %in% meta.data$mouse),]
#this produces the whole dataset -- might just need unique mouse and batch combos

oo <- table(original_DF$mouse,original_DF$Batch)
#colnames -- batches
#rownames -- mice
#converting to DF might make things easier

oo.DF <- as.data.frame(oo)#this has a dif str, 3 cols -- like it was melted
oo.DF <- oo.DF[!(is.na(oo.DF$Freq)|oo.DF$Freq==0),] #remove all 0 counts for batches

#duplicated(oo.DF$Var1)

mulitple.batch.mice <- oo.DF$Var1[duplicated(oo.DF$Var1)]
xu2 <- oo.DF[!unique(oo.DF$Var1),]


#push colname of matching mouse into cell of metadata
table(oo.DF$Var1)# this is how you can fin the double counts

#maybe change colnames of oo.DF

#can I collapse the mulitple batches mice?

#to match -- I may need to merge
oo.merge <- merge(oo.DF, meta.data, by.y = "mouse", by.x = "Var1", all.y = TRUE)
mouse_table_w.Ages <- oo.merge


#batch14 is missing .. it hasn't been integrated into the set up data i think

#but this is missing the non-batch
#the merge is basically what I want expect there are duplicate Mouse for those quant in mulitple batches...

#add column if there's a folder?


#write csv file (/data)
write.table(meta.data, "~./MLH1repo/data/clean.Meta.Data_8.28.19.txt", 
            sep="\t", row.names = FALSE)

write.table(oo.merge, "~./MLH1repo/data/Meta.Data.Merge_batch_quant_8.29.19.txt", 
            sep="\t", row.names = FALSE)

#save Rdata file
save.image("data/MetaData.RData")