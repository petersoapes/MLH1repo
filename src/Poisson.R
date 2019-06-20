# playing with poisson distributions
# input:
# output:


# load data
setwd("C:/Users/alpeterson7/Documents/MLH1repo/")
load(file="data/MLH1_data_setup.RData")
#(load mouse level data)

library(ggplot2)
#do Kaz female to start

KAZf_DF <- MLH1_data[MLH1_data$category == "KAZ female",]

# plot the distributions of indiviual mouse CO counts (as histograms)
# when thinking about the CO counts on bivalents, (CO counts are not poisson distributed)
# when there is interference

oopop <- ggplot(data = KAZf_DF, aes(x=nMLH1.foci, fill = mouse))+ geom_histogram()

oopop2 <- ggplot(data = KAZf_DF, aes(x=nMLH1.foci, fill = mouse))+ geom_density(alpha=.3)

#these distributions are cut off at 20

oopop3 <- ggplot(data = KAZf_DF, aes(y=nMLH1.foci, x=mouse, fill = mouse))+ geom_jitter()+
  geom_boxplot(alpha=.3)


#so fitting a poisson means -- variance and mean are the same