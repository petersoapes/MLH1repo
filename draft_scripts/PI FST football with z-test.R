setwd("/Users/Doug/Desktop")
bp=read.table("jerez.txt")

bp$Pi=bp$pi


bpPi=read.table("Jerez2_filtered.genes.pi")

bpFST=read.table("BJPL2_filt_genewise.fst")

bp=merge(bpPi,bpFST,by="V1")
library(plyr)
bp=rename(bp, c("V1"="gene_name","V4.x"="Pi", "V7"="FST","V13"="FST.3","V15"="FST.4"))


#Pesticide 

bpIns <- subset(bp, 
					gene_name=="TCOGS2:TC016365-RA" | 
					gene_name=="TCOGS2:TC015102-RA" |
					gene_name=="TCOGS2:TC014402-RA" | 
					gene_name=="TCOGS2:TC013633-RA" | 
					gene_name=="TCOGS2:TC012852-RA" |
					gene_name=="TCOGS2:TC012627-RA" | 
					gene_name=="TCOGS2:TC012503-RA" |
					gene_name=="TCOGS2:TC012112-RA"	|
					gene_name=="TCOGS2:TC009170-RA" | 
					gene_name=="TCOGS2:TC008198-RA" | 
					gene_name=="TCOGS2:TC007155-RA" |
					gene_name=="TCOGS2:TC007061-RA" | 
					gene_name=="TCOGS2:TC006445-RA" |
					gene_name=="TCOGS2:TC005352-RA"	|
					gene_name=="TCOGS2:TC004838-RA" | 
					gene_name=="TCOGS2:TC004836-RA" | 
					gene_name=="TCOGS2:TC004749-RA" |
					gene_name=="TCOGS2:TC004383-RA" | 
					gene_name=="TCOGS2:TC004126-RA" |
					gene_name=="TCOGS2:TC003369-RA" |
					gene_name=="TCOGS2:TC003239-RA"	|
					gene_name=="TCOGS2:TC001018-RA" | 
					gene_name=="TCOGS2:TC000981-RA" | 
					gene_name=="TCOGS2:TC000394-RA" 
					, select = c(gene_name,Pi,FST,FST.3,FST.4))       #CHAGE FST


#Sperm

bpIns <- subset(bp, 
					gene_name=="TCOGS2:TC013049-RA" | 
					gene_name=="TCOGS2:TC012560-RA" |
					gene_name=="TCOGS2:TC011481-RA" | 
					gene_name=="TCOGS2:TC010065-RA" | 
					gene_name=="TCOGS2:TC010064-RA" |
					gene_name=="TCOGS2:TC009459-RA" | 
					gene_name=="TCOGS2:TC005744-RA" |
					gene_name=="TCOGS2:TC003552-RA"	|
					gene_name=="TCOGS2:TC015849-RA" | 
					gene_name=="TCOGS2:TC015056-RA" | 
					gene_name=="TCOGS2:TC010066-RA" |
					gene_name=="TCOGS2:TC008976-RA" | 
					gene_name=="TCOGS2:TC006088-RA" 
					, select = c(gene_name,Pi,FST,FST.3,FST.4))       #CHAGE FST
					
					
#Female

bpIns <- subset(bp, 
					gene_name=="TCOGS2:TC008595-RA" | 
					gene_name=="TCOGS2:TC007131-RA" |
					gene_name=="TCOGS2:TC007081-RA" | 
					gene_name=="TCOGS2:TC016271-RA" | 
					gene_name=="TCOGS2:TC000031-RA" |
					gene_name=="TCOGS2:TC004457-RA" | 
					gene_name=="TCOGS2:TC011099-RA" |
					gene_name=="TCOGS2:TC000504-RA" |
					gene_name=="TCOGS2:TC008099-RA" | 
					gene_name=="TCOGS2:TC013394-RA" | 
					gene_name=="TCOGS2:TC000618-RA" |
					gene_name=="TCOGS2:TC013372-RA" | 
					gene_name=="TCOGS2:TC011798-RA" |		
					gene_name=="TCOGS2:TC014055-RA" | 
					gene_name=="TCOGS2:TC010546-RA"  					
					, select = c(gene_name,Pi,FST,FST.3,FST.4))      #CHAGE FST
					
					
					
					
					


bpInsFST=((bpIns$FST+bpIns$FST.3+bpIns$FST.4)/3)				      #CHAGE FST

bpFST=((bp$FST+bp$FST.3+bp$FST.4)/3)                                  #CHAGE FST


bpmean=mean(bp$Pi,na.rm = TRUE)
bpmeanFST=mean(bpFST)


nreps <- 10000
PI <- numeric(nreps) 
FST <- numeric(nreps)  

PI[1] <- bpmean
FST[1] <- bpmeanFST


for (i in 2:nreps) {
  bpR=bp[sample(nrow(bp), length(bpIns[,1])), ]										
  bpRmean=mean(bpR$Pi, na.rm = TRUE)
  bpRmeanFST=mean(((bpR$FST+bpR$FST.3+bpR$FST.4)/3))      			 #CHAGE FST              
  PI[i] <- bpRmean
  FST[i] <- bpRmeanFST
  }


library(car)
dataEllipse(FST,PI,ylim=c(0,0.02),xlim=c(0,0.4),levels = c(0.95))
title("Jerez Rep2")
points(mean(bpInsFST),mean(bpIns$Pi),col="Orange",pch=8,cex=2,lwd=2)


prob_Pi<- length(PI[PI >= mean(bpIns$Pi)])/nreps
prob_FST <- length(FST[FST >= mean(bpInsFST)])/nreps


##P-values
1-prob_Pi
prob_Pi

1-prob_FST
prob_FST


peepeePi=c(1-prob_Pi,prob_Pi)
feefeeFST=c(1-prob_FST,prob_FST)
###########################################

# set p to the smaller of the 
p=c((min(peepeePi))*2,(min(feefeeFST))*2)


Stouffer.test <- function(p, w) { # p is a vector of p-values
  if (missing(w)) {
    w <- rep(1, length(p))/length(p)
  } else {
    if (length(w) != length(p))
      stop("Length of p and w must equal!")
  }
  Zi <- qnorm(1-p) 
  Z  <- sum(w*Zi)/sqrt(sum(w^2))
  p.val <- 1-pnorm(Z)
  return(c(Z = Z, p.value = p.val))
}


Stouffer.test(p=p)

