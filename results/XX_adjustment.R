# XX adjustment
# input: main data file
# output: estimate for an adjustment for XX in females


#Issue with XX not making female and male gwRR comparable

#females have higher rates of achiasmy, so more 0CO bivs 
#but XX is larger autosome, so 


#how many females have 1 achiasmate?
#do they differ in the MLH1 count


#load MLH1 data
setwd("C:/Users/alpeterson7/Documents/MLH1repo/")
load(file="data/MLH1/MLH1_data_setup_11.12.19.RData")

load(file="C:/Users/alpeterson7/Documents/MLH1repo/data/CleanBivData.RData")

#whole cell data set
whole.cell.data = read.csv("~./MLH1repo/whole_cell_measures.csv", header = TRUE)

# "return5"         "fileName"        "boxNumber"       "centromere"      "SC.length"       "rank"           
#[7] "hand.foci.count" "F1"              "F2"              "F3"              "notes"           "X"              
#[13] "X.1"    


#add in extra cols to whole.cell.data
whole.cell.data <- whole.cell.data[!(is.na(whole.cell.data$fileName) | whole.cell.data$fileName==""), ]
#this doesn't remove all of the data that are missing values

whole.cell.data$Obj.ID <- paste(whole.cell.data$fileName, whole.cell.data$boxNumber, sep = "_")

whole.cell.data.XY <- whole.cell.data
whole.cell.data <- whole.cell.data[(whole.cell.data$hand.foci.count!="XY"), ]

#whole.cell.data$hand.foci.count <- as.numeric(as.character(whole.cell.data$hand.foci.count))
#whole.cell.data$SC.length <- as.numeric(as.character(whole.cell.data$SC.length))

#Estimate XX in female whole cell biv measures (3rd)
#how different are these across strains?
#how different are the top 4 from each other (SC length, CO count)

#image level table
#add total COs
whole_cell_summary <- ddply(whole.cell.data[whole.cell.data$chrm.class != "XY",], c("fileName"), summarise,
                       
                       totalCO = sum(hand.foci.count),     
                       avCO.per.CHRM = mean(hand.foci.count, na.rm = TRUE),
                       varCO.per.CHRM = var(hand.foci.count, na.rm = TRUE),

                      CO0 =  sum(hand.foci.count == 0, na.rm = TRUE),
                      CO1 =  sum(hand.foci.count == 1, na.rm = TRUE),
                      CO2 =  sum(hand.foci.count == 2, na.rm = TRUE),
                      CO3 =  sum(hand.foci.count == 3, na.rm = TRUE),
                      
                       avSC.length = mean(SC.length, na.rm = TRUE),
                       varSC.length = var(SC.length, na.rm = TRUE),
                       
                       av.COdensity = mean(SC.length /hand.foci.count),
                       varCOdensity = var(SC.length /hand.foci.count)
                          
                       )
#add mouse ect.

whole_cell_summary <- add_mouse(whole_cell_summary)
whole_cell_summary <- add_strain(whole_cell_summary)
whole_cell_summary <- add_sex(whole_cell_summary)
whole_cell_summary <- add_category(whole_cell_summary)


#I always foret the lines for adding the rank to each... bivalent, what's wrong with me?

#isolate longest 5, longest 3
#compare the number COs between each of these

#ADDING RANK to chrm obs
#ADD THE RANK -- this stoped working for a bit, 
#fixed it with rank

for(musy in as.character(unique(whole.cell.data$fileName))){
  #print(musy)
  #order the chrm lengths -- this is order
  chrm.order = rank(whole.cell.data$SC.length[whole.cell.data$fileName == musy])
  whole.cell.data$rank[whole.cell.data$fileName == musy] <- chrm.order   #this is just a string of numbers--
  
}
#this function greaks whe NA's run thru it

whole.cell.data <- add_mouse(whole.cell.data)
whole.cell.data <- add_strain(whole.cell.data)
whole.cell.data <- add_sex(whole.cell.data)
whole.cell.data <- add_category(whole.cell.data)

#exclude cells with the wrong number / double check the expectations for the cell images.

#subset / isolate female cell images (because I want to check for XX)

female.whole.cell <- whole.cell.data[whole.cell.data$sex == "female", ]#384

### isolate rank 1:5 chrms

#plot cos / per chrm


#THE ranks are backwards
pp <- ggplot(female.whole.cell[female.whole.cell$rank > 15,], aes(x=rank, y=SC.length, color=as.factor(hand.foci.count) ))+
  geom_jitter()+  facet_wrap(~strain)

theme(legend.position = "none"  )+ggtitle("my first plot")

# need more data on the ratios of 0:1:2:3 COs per biv across these ranks
# 5 cells per mouse?
#>30 cells per strain


#calculate a XX adjust factor for each of the female strains (>30 cells per strain)
#The XX adjust factor would be


#from BivData (female), find the number of whole cells!!


curated_cell_summary <- ddply(Curated_BivData[Curated_BivData$sex == "female",], c("fileName"), summarise,
                            
                            boxs.IDd = max(boxNumber),
                            nboxes_passed = length(boxNumber),
                            pass_rate = nboxes_passed/boxs.IDd,
                            
                            totalCO = sum(hand.foci.count, na.rm = TRUE),     
                            avCO.per.CHRM = mean(hand.foci.count, na.rm = TRUE),
                            varCO.per.CHRM = var(hand.foci.count, na.rm = TRUE),
                            
                            CO0 =  sum(hand.foci.count == 0, na.rm = TRUE),
                            CO1 =  sum(hand.foci.count == 1, na.rm = TRUE),
                            CO2 =  sum(hand.foci.count == 2, na.rm = TRUE),
                            CO3 =  sum(hand.foci.count == 3, na.rm = TRUE),
                            
                            avSC.length = mean(chromosomeLength, na.rm = TRUE),
                            varSC.length = var(chromosomeLength, na.rm = TRUE),
                            
                            av.COdensity = mean(chromosomeLength / hand.foci.count),
                            varCOdensity = var(chromosomeLength / hand.foci.count)
                            
)
#there are some images with >20 Bivalents, they should be edited
#3 female cell images with 20 bivalents, 11 cells with 19


#table summarize by rank
#compare_means


male.whole.cell <- whole.cell.data[whole.cell.data$sex == "male", ]#359




biv.count_table <-  ddply(.data=whole.cell.data,
                          .(fileName),
                          summarize,
                          nbiv = length(SC.length)
)#all have 20 or 19 (after removing XY)

#real data
male.biv.count_table <-  ddply(.data=male.whole.cell,
                               .(fileName),
                               summarize,
                               nbiv = length(SC.length),
                               nCO0 =  sum(hand.foci.count == 0, na.rm=TRUE), 
                               nCO1 =  sum(hand.foci.count == 1, na.rm=TRUE), 
                               nCO2 = sum(hand.foci.count == 2, na.rm=TRUE),
                               nCO3 = sum(hand.foci.count == 3, na.rm=TRUE),
                               
                               totCO = sum(hand.foci.count),
                               total.SC = sum(SC.length),
                               mean_co = mean(hand.foci.count, na.rm = TRUE),
                               mean_sc = mean(SC.length),
                               var_co = var(hand.foci.count),
                               var_sc = var(SC.length),
                               cv_co.count = cv(hand.foci.count),
                               cv_SC.length = cv(SC.length)
)
#exclude last one
male.biv.count_table <- male.biv.count_table[!(is.na(male.biv.count_table$fileName) | male.biv.count_table$fileName==""), ]