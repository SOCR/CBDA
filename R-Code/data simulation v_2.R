## STEP 1 - DATA CLEANING
## This is just an example with the MRI dataset "NeuroIm1.txt"
# Set the working directory for the R Code development
setwd("~/Desktop/R")

## Retrieve the dataset (edit the directory where the dataset is located)
#NeuroIm1 = read.table("NeuroIm1.txt", header = TRUE)
NeuroIm1 = read.table("NeuroIm1.txt", header = TRUE)
#NeuroIm1 = read.delim("/home/simeonem/Documents/NIH-grant/SOCR/GITHUB/DATA/NeuroIm1.txt", header = TRUE)
dim(NeuroIm1)


# DATA SIMULATION

# we could use histogram to preliminary decide the distributions type of 
# different variables. For instance, In this subdata, only Sex belongs to
# Categorical distribution, other variables belong to continuous distribution.
hist(NeuroIm1$Value, main="value Histogram")
hist(NeuroIm1$MMSE, main="value1 Histogram")
hist(NeuroIm1$Value, main="value Histogram")
unique(NeuroIm1$Group)
unique(NeuroIm1$ID)
#generate with the Normal Distribution
# all figures are generated from the first row of the data
NeuroIm_random<-NeuroIm1
for(i in 1:104){
  w=which(NeuroIm1$Subject_ID==i)
  NeuroIm_random[w,1]<-i
  NeuroIm_random[w,2]<-sample(unique(NeuroIm1$Group),1,replace = TRUE)
  NeuroIm_random[w,5]<-sample(unique(NeuroIm1$Sex),1,replace = TRUE)
}
NeuroIm_random$Measure<-sample(unique(NeuroIm1$Measure),23296,replace = TRUE)
for(i in c(3:4,6:11,13)){
  NeuroIm_random[,i]<-rnorm(n=23296, mean=mean(NeuroIm1[1,i]), sd=0.25*mean(NeuroIm1[1,i]))
}
for(i in c(3,6:11) ){
  NeuroIm_random[,i]<-as.integer(NeuroIm_random[,i])
}
for(i in 4){
  NeuroIm_random[,i]<-round(NeuroIm_random[,i],digits = 1)
}
for(i in 13){
  NeuroIm_random[,i]<-round(NeuroIm_random[,i],digits = 2)
}
write.csv(NeuroIm_random, file="normal distribution")

#generate with the Uniform Distribution
NeuroIm_random2<-NeuroIm1
for(i in 1:104){
  w=which(NeuroIm1$Subject_ID==i)
  NeuroIm_random2[w,1]<-i
  NeuroIm_random2[w,2]<-sample(unique(NeuroIm1$Group),1,replace = TRUE)
  NeuroIm_random2[w,5]<-sample(unique(NeuroIm1$Sex),1,replace = TRUE)
}
NeuroIm_random2$Measure<-sample(unique(NeuroIm1$Measure),23296,replace = TRUE)

for(i in  c(3:4,6:11,13)){
  NeuroIm_random2[,i]<-runif(n=23296, min=0.75*NeuroIm1[1,i], max=1.25*NeuroIm1[1,i])
}
for(i in c(3,6:11) ){
  NeuroIm_random2[,i]<-as.integer(NeuroIm_random[,i])
}
for(i in 4){
  NeuroIm_random2[,i]<-round(NeuroIm_random[,i],digits = 1)
}
for(i in 13){
  NeuroIm_random2[,i]<-round(NeuroIm_random[,i],digits = 2)
}
write.csv(NeuroIm_random2, file="uniform distribution")