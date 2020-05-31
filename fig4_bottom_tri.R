#load data
remove_strains.1sex <- c("CZECH", "TOM","AST","CZECH","CAST","HMI","SPRET","CAROLI", "F1", "PERC")

#Curated_BivData.FM <- Curated_BivData[ ! Curated_BivData$strain %in% remove_strains.1sex, ]

Curated_BivData

my.data = (Curated_BivData %>% filter(hand.foci.count == 2) )
#remove_strains

#reorder dataframe
my.data$strain <- factor(my.data$strain, ordered = TRUE, 
              levels =c( "WSB", "G", "LEW", 
              "PWD","SKIVE", "KAZ",
              "MSM", "MOLF") )


top.triangle <- function(my.data) {
  #current.strain = 
  for(i in 1:(length( unique(my.data$strain) ))){
    print(i)  
    current.strain <- as.character( unique(my.data$strain)[i] )

    plot( (my.data %>% filter(strain == current.strain) %>% filter(sex == "female") )$PER_Foci_2 ~
     (my.data %>% filter(strain == current.strain) %>% filter(sex == "female") )$PER_Foci_1, 
         data= ( my.data %>% filter(sex == "female") %>% filter(strain == current.strain) ), 
       axes=FALSE, ylab="", xlab="", main='', xlim=c(0, 1), ylim=c(0, 1), 
       xaxt="n", yaxt="n", asp=1, col =  colors_of_strains[current.strain] )
    
    #mtext(current.strain, 3, 5, font=1, cex=1, adj=.95)
  
     # mtext("Female Position.2", 2, .75)
  #  mtext("Female Position.1", 3, 2)
    axis(side=2, las=1, pos=0)
    axis(side=3, las=1, pos=1)
    lines(0:1, 0:1)
      }
  }

bottom.triangle.only <- function(my.data) {
  x.dist <- .1
  #my.data.2 <- transform(my.data, my.y.top=my.y.bottom + x.dist)

  my.data.2 <- transform( ( my.data %>% filter(sex == "male")  ) , 
                            PER_Foci_2=PER_Foci_2 + x.dist)
  
  for(i in 1:(length( unique(my.data.2$strain) ) ) ){
    print(i)
    current.strain <- as.character( unique(my.data.2$strain)[i] )
  
    #(my.data %>% filter(strain == current.strain) %>% filter(sex == "male") )
    #( my.data %>% filter(strain == current.strain) %>% filter(sex == "male") )
  
    #changing to just bottom triangle is harder than I thought
    plot()
      points( (my.data.2 %>% filter(sex == "male") %>% filter(strain == current.strain) )$PER_Foci_1 ~ (my.data.2 %>% filter(sex == "male") %>% filter(strain == current.strain) )$PER_Foci_2,
         data= ( my.data.2 %>% filter(sex == "male") %>% filter(strain == current.strain) ), 
         col =  colors_of_strains[current.strain], xpd=TRUE)
 
 #    mtext("Position.2", 1, 1.5, at=mean(par()$usr[1:2]) + x.dist)
#    mtext("Position.1", 4, 3, padj=par()$usr[1] + 3)

    x.at <- axisTicks(par()$usr[1:2], 0) + x.dist
    axis(side=1, las=1, pos=0, at=x.at, 
       labels=FALSE, xpd=TRUE)
    mtext(seq(0, 1, 0.2), 1, 0, at=x.at, cex=0.7)
    axis(4, las=1, pos=1 + x.dist)
    lines(0:1 + x.dist, 0:1, xpd=TRUE)
    }
  }




dev.off()
par(mar=c(2.5, 4, 2, 3), oma=c(1, 1, 1, 1), mfrow=c(1,1))

by(my.data.wsb, my.data.wsb$strain, function(sub){
  bottom.triangle.only(sub)
#need the top triangle to call plot
  })


