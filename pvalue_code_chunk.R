
#below will plot pvalues
bb<- replicate(1000, t.test(sample(Dom_f$nMLH1.foci, 5), sample(Dom_f$nMLH1.foci, 30) )$p.value )

ggplot(data.frame(bb), aes(y=bb, x=seq(1:1000))) + 
  #geom_histogram(aes(y=..density..)) +
  geom_point()
#still thinking of the right set-up for comparisons


#pieces from Rmd Report

```{r}
colnms = c("mouse", "rep")
ply_data <- df.pasd.mice[df.pasd.mice$mouse == "8oct14_PWD_f8",]
#make this the function that I apply over the mice in the data

#simplify the t.test
t.test(sample(ply_data$nMLH1.foci, 5), sample(ply_data$nMLH1.foci, 5))$p.value
t.test(sample(ply_data$nMLH1.foci, 13), sample(ply_data$nMLH1.foci, 13))$p.value

#repeat the sampling 100 times
bb <-ddply(jj, .(mouse), function(x) t.test(x[1,2:5],x[2,2:5])$p.value )#t.test for 2 

#try with do
blah <- df.pasd.mice %>% group_by(mouse) %>% do(head(.))

plist=data.frame(pv=as.numeric(), mean1=as.numeric(), mean2=as.numeric())

hundred <- function(x){
  plist=data.frame(pv=as.numeric(), mean1=as.numeric(), mean2=as.numeric())
  for(eye in 1:100){
    samp1 = sample(x, 5)
    samp2 = sample(x, 5)
    plist$pv[eye] <- t.test(samp1,samp2)$p.value
    plist$mean1[eye] <- mean(samp1)
    plist$mean2[eye] <- mean(samp2)
  }
  return(plist)
} 
mm <- hundred(ply_data$nMLH1.foci)


#makes dfs, with the samples. // sampling from MLH1
permut.mouse.numb <- function(x, rep_size, samp_size){
  new.df = matrix(nrow=rep_size, ncol=samp_size)
  for(i in 1:rep_size) {
    new.df[i,] = sample(x, samp_size)
  }
  return(as.data.frame(new.df, colnames(colnms) ) )
  #return sampled
}
#can I integrate the sampling and t.test here?

#function for 

#this works. sampling x permutations, y sample size
vv <- permut.mouse.numb(df.pasd.mice$nMLH1.foci,5,4)

## I think this does it! this makes a list of 23 dfs (for each mouse in the large df)
##this makes dfs for doing the t.tests
kk<-dlply(df.pasd.mice, .(mouse), function(x) permut.mouse.numb(x$nMLH1.foci,2,5) )
#applies permut function over mouse -- then run a t.test and return pvalue
jj<-ddply(df.pasd.mice, .(mouse), function(x) permut.mouse.numb(x$nMLH1.foci,2,20) )

#
pval <- function()
  
  #t.test and p values
  #maybe I should just choose 2 samples, do a t.test, do that 100 times..?
  #below runs t.tests on
  #this is making a new tab;e, 
  bb <- ddply(jj, .(mouse), function(x) t.test(x[1,2:5],x[2,2:5])$p.value )#t.test for 2 samples of size 20

# figure out a way to mush dd into a matrix

repeats = 10
new.p.matrix = matrix(ncol=length(unique(jj$mouse)), nrow = repeats)
for(e in 1:repeats){
  new.p.matrix[e]<- ddply(jj, .(mouse), function(x) t.test(x[1,2:5],x[2,2:5])$p.value )
  
}

powrange <- seq(0.4, .9, .1)
n <- sapply(powrange, function(i) power.prop.test(p1=0.4,p2=0.24,power=i,sig.level=0.05)$n)
plot(powrange, n)


bb$mouse[(bb$V1 < 0.05)]
#mice which had significantly difference in samples of size 20
#16jan16_G_f2  20feb16_G_f3  3nov15_WSB_f1
#17mar16_G_f1
#16jan16_G_f2   20dec16_LEW_m3
#30sep16_MSM_f2
#22jun15_G_m2

#figure out how to permute these tests. repeat the above... then return distributions of p values

#10
#20feb16_G_f3   13nov16_MSM_m1

#figure out a way to plot/visualize these pvalues
#2 samples are good for t.tests, but I need to repeat 100 times
#I think I need to ... add the replicates that can be xxply through

## for 1:100, cal permt(10,2) then call 
ll=c()
ty <- for(i in 1:10){
  print(i)
  jj <-ddply(df.pasd.mice, .(mouse), function(x) permut.mouse.numb(x$nMLH1.foci,2,10) )
  
  bb <-ddply(jj, .(mouse), function(x) t.test(x[1,2:5],x[2,2:5])$p.value )
  #the t.test$p.values are not exactly what they should be
  #put p values into a list? (100 pvalues )
  ll[i] = bb$V1[i]#this returns a list of p values for different mice? one mouse?
}

#for 

gg<-t.test(kk$`10mar15_PWD_m2`[1,2:5], kk$`10mar15_PWD_m2`$V3)  
#jj$`10mar15_PWD_m2`
#jj[1,1:5]
#$`10mar15_PWD_m2`
#[1] 0.6071011

##then do t.test to test the distributions 
## data is the data frames, by mouse, function is t.test


# use q_cut_off table, to find the mice to use
listOmice <- q_cutoff_table$mouse[ (q_cutoff_table$total >= 25) ]
#mke new df with from 
df.pasd.mice <- MLH1_data[MLH1_data$mouse %in% listOmice, ]

#write function --- for taking 100 samples of 10
permut.df <- function(df){
  #semperate by mouse
  mouse.data    <- ddply(df, c("mouse"), summarise,
                         df <- data.frame(
                           samp = sample(nMLH1.foci,5)
                         ) 
  )
  

    return(mouse.data)
}

mouse.data <- ddply(df.pasd.mice, c("mouse"), summarise,
                    
                    m.df <- data.frame(sample(nMLH1.foci, 5, replace = FALSE) )
)



# this creates dataframe, which is a subset of larger dataframe ... not sure se_alone, but samples 40 of 
this =ddply (df.pasd.mice, .(mouse), subset, sample(seq_along(nMLH1.foci)<=5))
#ss <- sample(df.pasd.mice$nMLH1.foci)

Table_BD_mouse <- ddply(BDMLH1_data, c("ANIMAL_ID", "Cross"), summarise,
                        N  = length(nMLH1_foci),
                        mean_co = format(round( mean(nMLH1_foci), 3), nsmall = 3),
                        var = format(round( var(nMLH1_foci),3), nsmall = 3),
                        sd   = round(sd(nMLH1_foci),3),
                        se   = round(sd / sqrt(N),3)
                        #text=paste(Cross, collapse=""))
)

```