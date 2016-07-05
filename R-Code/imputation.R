
setwd("~/Desktop/R")

## STEP 1 - DATA CLEANING
## This is just an example with the MRI dataset "NeuroIm1.txt"
NeuroIm1 = read.table("NeuroIm1.txt", header=TRUE,fill=TRUE)

# Delete the last 3 columns from the big matrix NeurIm1 ["ROI","Measure","Value"]
# and store the rest in a temp matrix, compressing unique values by patients
NeuroIm1_Fix <- unique(NeuroIm1[,-1*(11:13)])
#length(unique(NeurIm1_Fix))


# Define Variables/Columns: Patients, type of Measures and ROI [Region of Interest]
Patients <- NeuroIm1_Fix$Subject_ID
Measures <- c("SA","SI","CV","FD")
ROI <- unique(NeuroIm1$ROI)
# Initialize a new data matrix that has the correct # of columns
NeuroIm1_NEW = array(0, c(length(Patients), length(ROI)*length(Measures)))

## We assign names to the columns in the form of Value_Measure_ROI


names = NULL
for (j in 1:length(Measures)) {
  for (i in 1:length(ROI))
    names = c(names, paste("Value",Measures[j], ROI[i],"END", sep="_"))
}
#length(names)
#dim(NeuroIm1_NEW)
names(NeuroIm1_NEW) <- names


# This loops extract a record from the big dataset, matching patient id, type of measure and ROI.
# Then It looks at the columns of the expanded matrix (# columns = Measures x ROI), and selects
# the column that matches the label resulting by combining Measures and ROI values in the record.
# Then it retries the value in the Value field of the big matrix and place it in the expanded matrix
# at the selected column

for (i in 1:length(Patients)) {
  for (j in 1:length(Measures)) {
    for (s in 1:length(ROI)) {
      NeuroIm1_temp = NeuroIm1[NeuroIm1$Subject_ID==i & NeuroIm1$Measure==Measures[j] & NeuroIm1$ROI==ROI[s],]
      a = paste(c("Value_",Measures[j],"_",ROI[s],"_END"),collapse="")
      b = which(names(NeuroIm1_NEW)==a)
      NeuroIm1_NEW[i,b] <- NeuroIm1_temp$Value
    }
  }
}
print(NeuroIm1_NEW[1,56])
# Appends the matrix that is fixed from the big matrix to the expanded one.
# The final dimension of this matrix is rows=# patients
# This is the matrix to use for the analysis with SuperLearner, after few more
# data cleaning and recasting.
# List of libraries/packages needed below
library("colorspace")
library("grid")
library("data.table")
library("VIM")
library(MASS)
library(Matrix)
library(lme4)
library(arm)
library(foreach)
library(glmnet)
library(class)
library(nnet)

NeuroIm1_Final <- cbind(NeuroIm1_Fix, NeuroIm1_NEW)
# Set the names/labes of the columns
names(NeuroIm1_Final) <- c(names(NeuroIm1_Fix),names)

# Normalization of the aggregated matrix without Group and Sex
# This step will need to be moved within the SuperLearner loop,
# at the same time when IMPUTATION is performed on each subset of the 
# aggregated matrix
a1 = which(names(NeuroIm1_Final) == "Group")
a2 = which(names(NeuroIm1_Final) == "Sex")

cont = 1:length(NeuroIm1_Final)
cont <- cont[-1*c(a1,a2)]

NeuroIm1_Final[,cont] <- data.frame(apply(NeuroIm1_Final[,cont], 2, function(x)
{x <- rescale(x, "full")}));
rm(cont)

# Recast the binary variable Sex
NeuroIm1_Final$Sex <- ifelse(NeuroIm1_Final$Sex=="F",1,0)

## Generating binary outcome matrices and relabeling categorical variables
## SINCE WE HAVE 3 GROUPS: AD-aLZHEIMER, MCI=MINOR COGNITIVE IMPAIRMENT, NC=NORMAL
NeuroIm1_Final_AD = NeuroIm1_Final[NeuroIm1_Final$Group == "AD",]
NeuroIm1_Final_NC = NeuroIm1_Final[NeuroIm1_Final$Group == "NC",]
NeuroIm1_Final_MCI = NeuroIm1_Final[NeuroIm1_Final$Group == "MCI",]

# Merge the datasets for training. I am defining 3 datsets here to be used for training
# since the SUperLearner function only works with binomial outcomes (for now).
# We will test SL comparing AD vs NC
NeuroIm1_Final_AD_vs_NC_training = rbind(NeuroIm1_Final_AD,NeuroIm1_Final_NC) # This is our aggregated dataset !!
NeuroIm1_Final_AD_vs_MCI_training = rbind(NeuroIm1_Final_AD,NeuroIm1_Final_MCI)
NeuroIm1_Final_NC_vs_MCI_training = rbind(NeuroIm1_Final_NC,NeuroIm1_Final_MCI)

# Labels the columns of the new matrices
names(NeuroIm1_Final_AD_vs_NC_training) <- c(names(NeuroIm1_Fix),names)
names(NeuroIm1_Final_AD_vs_MCI_training) <- c(names(NeuroIm1_Fix),names)
names(NeuroIm1_Final_NC_vs_MCI_training) <- c(names(NeuroIm1_Fix),names)

# Defining and recasting the binary variable Group for each dataset
NeuroIm1_Final_AD_vs_NC_training$Group <- ifelse(NeuroIm1_Final_AD_vs_NC_training$Group=="AD",1,0)
NeuroIm1_Final_AD_vs_MCI_training$Group <- ifelse(NeuroIm1_Final_AD_vs_MCI_training$Group=="AD",1,0)
NeuroIm1_Final_NC_vs_MCI_training$Group <- ifelse(NeuroIm1_Final_NC_vs_MCI_training$Group=="MCI",1,0)

# Define the temporary output [Ytemp] and input [Xtemp] matrices for the SuperLearner call
Xtemp = NeuroIm1_Final_AD_vs_NC_training; # temporary X-->Xtemp to modify and pass to SuperLearner
#Xtemp = NeuroIm1_Final_AD_vs_MCI_training; 
#Xtemp = NeuroIm1_Final_MCI_vs_NC_training; 
#Xnew = NeuroIm1_Final_AD_vs_NC_test; # temporary X-->Xtemp to modify and pass to SuperLearner

# assign the Group column to the output Y
Ytemp = NeuroIm1_Final_AD_vs_NC_training$Group; # Output Matrix Y for SuperLearner
#Y = NeuroIm1_Final_AD_vs_MCI_training$Group; # Output Matrix Y for SuperLearner
#Y = NeuroIm1_Final_MCI_vs_NC_training$Group; # Output Matrix Y for SuperLearner

# Select the columns Patient ID [1], MMSE [3]  (Mini-Mental State Exam score, a cognitive assessment measure),
# and CDR [4] (Clinical Dementia Rating scale from the test dataset X)[Group]
# and eliminate them from the training dataset because almost perfectly correlated to Y
w = which(names(NeuroIm1_Final_AD_vs_NC_training) == "Subject_ID" | names(NeuroIm1_Final_AD_vs_NC_training) == "Group" |
            names(NeuroIm1_Final_AD_vs_NC_training) == "MMSE" | names(NeuroIm1_Final_AD_vs_NC_training) == "CDR")
names(Xtemp)
Xtemp <- Xtemp[,-1*w] # Eliminate the output column (Group) from the training dataset X 
names(Xtemp)
dim(Xtemp)

## SAMPLE THE PREDICTION DATASET
# Fraction (SET TO 15% BELOW) of data/patients to use for prediction, IN A BALANCED WAY
alpha=0.15
a1=round(length(which(Ytemp==1))*alpha);
a2=round(length(which(Ytemp==0))*alpha);
print(length(which(Ytemp==1)))
print(length(which(Ytemp==0)))

# selects randomly patients for prediction
q1 = sample(which(Ytemp==1),a1)
q2 = sample(which(Ytemp==0),a2)
q <- c(q1 , q2)
Xnew <- as.data.frame(Xtemp[q,]) # define the patients to predict
print(dim(Xnew))
print(q)


Xtemp <- Xtemp[-1*q,] # eliminate q patients for prediction [not used in the training]
print(dim(Xtemp))

Ypred <- Ytemp[q] # assign the output for the prediction set [not used in the training]
Ytemp <- Ytemp[-1*q] # eliminate q patients for prediction [not used in the training]
print(q)
print(q1)
print(q2)
# SET THE SAME NAMES/LABELS FOR THE NEW MATRIX Xnew
names(Xnew) <- names(Xtemp)



#STEP: IMPUTATION
#GENERATING MISSSING VALUE
#imputation:generate missing values at random in each subject, probability of
# missing is 20%.
beta=0.2
b1=round(53*beta);
Xtemp_miss<-Xtemp
names(Xtemp_miss)<-names(Xtemp)


for(j in 1:230){
  mj=sample(53,b1)
  Xtemp_miss[mj,j]=NA
  }
print(b1)

#IMPUTATION PART
#MICE
library(mice)
#output pattern
md.pattern(Xtemp_miss[,1:50])
# draw a plot to indicate the missing value situation
library(VIM)
mice_plot<-aggr(Xtemp_miss[1:53,1:50],col=c('navyblue','yellow'),numners=TRUE,
                sortvars=TRUE,labels=names(Xtemp_miss),cex.axis=.005,gap=3,
                ylabs=c("missingdata","pattern"))


# because the #of the data may influence the processing time, 
# I try to imputed first 50 columns of the matrix at first. 

# According to the research, set the # of iterations equal to 10 is 
# sufficient. 
imputed_mice<-mice(Xtemp_miss[,1:50],m=5,maxit=10)
summary(imputed_mice)

#calculate the time spent in imputating
system.time(mice(Xtemp_miss[,1:50]))

# since we have 5 imputation datasets, we could fit the dataset with each 
# one of them.
completedata.1<-complete(imputed_mice,1)
completedata.2<-complete(imputed_mice,2)
completedata.3<-complete(imputed_mice,3)
completedata.4<-complete(imputed_mice,4)
completedata.5<-complete(imputed_mice,5)

#calculate the MSE for different imputation sets.
for(j in 1:5){
t.j<-array()
completedata.j<-complete(imputed_mice,j)
print(j)
for(k in 1:50)
    {
          mse.k<-(sum(completedata.j[,k]-Xtemp[,k])^2/b1)
          t.j[k]<-mse.k
     }
print(t.j)
t.j<-NA
}



#amelia
# for some reason, every time when I tried to run this function, the R studio
# will close automatically. I am trying to figure out the reason. 
library(Amelia)
amelia_fit<-amelia(Xtemp_miss[,1:20],m=5,p2s=1)

#missoforest
library(missForest)
#impute missing value with this method and calculate the MSE.
missforest_fit<-missForest(Xtemp_miss[,1:50],maxiter=10,variablewise=TRUE)
missforest_fit$OOBerror

#calculate the processing time for impute all missing values
system.time(missForest(Xtemp_miss[,1:230]))


#hmisc
library(Hmisc)
#impute  missing value with this method,get summary and calculate the time needed.
impute_arg<-aregImpute(~Sex+Age+TBV+GMV+WMV+CSFV+Value_SA_1_END+Value_SA_2_END+Value_SA_3_END+
                       Value_SA_4_END+Value_SA_5_END+Value_SA_6_END+
                       Value_SA_7_END+Value_SA_8_END+Value_SA_9_END+
                       Value_SA_10_END+Value_SA_11_END+Value_SA_12_END+
                       Value_SA_13_END+Value_SA_14_END+Value_SA_15_END+
                         Value_SA_16_END+Value_SA_17_END+Value_SA_18_END+
                         Value_SA_19_END+Value_SA_20_END+Value_SA_21_END+
                         Value_SA_22_END+Value_SA_23_END+Value_SA_24_END+
                         Value_SA_25_END+Value_SA_26_END+Value_SA_27_END+
                         Value_SA_28_END+Value_SA_29_END+Value_SA_30_END+
                         Value_SA_31_END+Value_SA_32_END+Value_SA_33_END+
                         Value_SA_34_END+Value_SA_35_END+Value_SA_36_END+
                         Value_SA_37_END+Value_SA_38_END+Value_SA_39_END+
                         Value_SA_40_END+Value_SA_41_END+Value_SA_42_END+
                         Value_SA_43_END+Value_SA_44_END,data=Xtemp_miss[,1:50],n.impute=5)
impute_arg

system.time(aregImpute(~Sex+Age+TBV+GMV+WMV+CSFV+Value_SA_1_END+Value_SA_2_END+Value_SA_3_END+
                         Value_SA_4_END+Value_SA_5_END+Value_SA_6_END+
                         Value_SA_7_END+Value_SA_8_END+Value_SA_9_END+
                         Value_SA_10_END+Value_SA_11_END+Value_SA_12_END+
                         Value_SA_13_END+Value_SA_14_END+Value_SA_15_END+
                         Value_SA_16_END+Value_SA_17_END+Value_SA_18_END+
                         Value_SA_19_END+Value_SA_20_END+Value_SA_21_END+
                         Value_SA_22_END+Value_SA_23_END+Value_SA_24_END+
                         Value_SA_25_END+Value_SA_26_END+Value_SA_27_END+
                         Value_SA_28_END+Value_SA_29_END+Value_SA_30_END+
                         Value_SA_31_END+Value_SA_32_END+Value_SA_33_END+
                         Value_SA_34_END+Value_SA_35_END+Value_SA_36_END+
                         Value_SA_37_END+Value_SA_38_END+Value_SA_39_END+
                         Value_SA_40_END+Value_SA_41_END+Value_SA_42_END+
                         Value_SA_43_END+Value_SA_44_END,data=Xtemp_miss[,1:50],n.impute=5))

#mi
library(mi)
# check the type and imputation method for different subjects' missing values. 
mdf<-missing_data.frame(Xtemp_miss[,1:20])
summary(mdf)

#step 1
# use ppd method impute missing value.
mi_data<-mi(Xtemp_miss[,1:20],n.iter=10,n.chain=4)
summary(mi_data)
system.time(mi(Xtemp_miss[,1:20],n.iter=10,n.chain=4))


#step 2
#since all other method use pmm to impute missing value. In order to get 
# parallel comparison, changethe imputation method from pdd to pmm.
t<-list()
t<-names(Xtemp_miss[1:20])
print(t)
mdf<-change(mdf,y=t,what="imputation_method",to="pmm")
show(mdf)

#impute missing values with this new method and achieve the results.
mi_data<-mi(mdf,n.iter=10,n.chain=4)
summary(mi_data)





