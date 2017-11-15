

#add mouse column to MLH1 df
add_mouse <- function(dat){
  dframe <- dat
  count =1
  for(i in dframe$Original.Name){
    #print(i)
    templist= strsplit(i, split="_")[[1]]
    c = paste(templist[2], templist[3],templist[4], sep = "_")
    dframe$mouse[count] <- c
    count= count +1
  }
  return(dframe)
}

