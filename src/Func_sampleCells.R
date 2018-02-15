# Sample effect function
#
# purpose, for MLH1 df create Big data frame of pvales/ for different sample size distributions

# input: df, sample size
# return: df

#source("src/Func_sampleCells.R")


# sample size
#sample_cells_pvals
sample_cells_pvals <- function(list, DF, ncells, sim_num){
  outlist = list()
  for(mouse in 1:length(unique(list$mouse))){
    #print(list$mouse[mouse])
    mus_data <- DF[DF$mouse == list$mouse[mouse],]
    outlist[mouse] <- list(replicate(sim_num, t.test(sample(mus_data$adj_nMLH1.foci, ncells, replace = FALSE),
                                        sample(mus_data$adj_nMLH1.foci, ncells, replace = FALSE) )$p.value ) )
    #names(outlist[mouse]) <- c(list$mouse[mouse])
  }
  names(outlist) <- c(list$mouse)
  return(outlist)
}