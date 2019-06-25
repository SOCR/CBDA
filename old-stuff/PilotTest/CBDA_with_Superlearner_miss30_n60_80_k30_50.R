# Load a session
#load("~/Documents/NIH-grant/SOCR/CBDA_v0.RData")
#load("~/Documents/NIH-grant/SOCR/CBDA_v2_temp.RData")

## STEP 1 - DATA CLEANING
## This is just an example with the MRI dataset "NeuroIm1.txt"
# Set the working directory for the R Code development
# setwd("/Documents/NIH-grant/SOCR/GITHUB/")

## Retrieve the dataset
#NeuroIm1 = read.table("NeuroIm1.txt", header = TRUE)
NeuroIm1 = read.table("/users/simeonem/CBDA-SL/NeuroIm1.txt", header = TRUE)
#NeuroIm1 = read.delim("/home/simeonem/Documents/NIH-grant/SOCR/GITHUB/DATA/NeuroIm1.txt", header = TRUE)

# Set the list of packages/libraries to install/include (done through the ipak.R function) 
packages <- c("ggplot2", "plyr","dplyr", "colorspace","grid","data.table","VIM","MASS","Matrix",
              "lme4","arm","foreach","glmnet","class","nnet","mice","missForest",
              "calibrate","nnls","SuperLearner","plotrix","TeachingDemos","plotmo",
              "earth","parallel","splines","gam", "Amelia", "Hmisc", "mi")
#source('~/Documents/NIH-grant/SOCR/GITHUB/ipak.R')
source('ipak.R')
ipak(packages)

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



# STEP 1
names = NULL
for (j in 1:length(Measures)) {
  for (i in 1:length(ROI))
    names = c(names, paste("Value",Measures[j], ROI[i],"END", sep="_"))
}
#length(names)
#dim(NeuroIm1_NEW)
names(NeuroIm1_NEW) <- names


# STEP 2 - DATA HARMONIZATION and STEP 3 DATA AGGREGATION
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

# Appends the matrix that is fixed from the big matrix to the expanded one.
# The final dimension of this matrix is rows=# patients
# This is the matrix to use for the analysis with SuperLearner, after few more
# data cleaning and recasting.
# List of libraries/packages needed below

## Use ctrl+shift+C to comment/uncomment multiple lines
NeuroIm1_Final <- cbind(NeuroIm1_Fix, NeuroIm1_NEW)
# Set the names/labes of the columns
names(NeuroIm1_Final) <- c(names(NeuroIm1_Fix),names)


# DATA relabeling
# Recast the binary variable Sex
NeuroIm1_Final$Sex <- ifelse(NeuroIm1_Final$Sex=="F",1,0)

## Generating binary outcome matrices and relabeling categorical variables
## SINCE WE HAVE 3 GROUPS: AD-aLZHEIMER, MCI=MINOR COGNITIVE IMPAIRMENT, NC=NORMAL
NeuroIm1_Final_AD = NeuroIm1_Final[NeuroIm1_Final$Group == "AD",]
NeuroIm1_Final_NC = NeuroIm1_Final[NeuroIm1_Final$Group == "NC",]
NeuroIm1_Final_MCI = NeuroIm1_Final[NeuroIm1_Final$Group == "MCI",]

# Merge the datasets for training. I am defining 3 datsets here to be used for training
# since the SuperLearner function only works with binomial outcomes (for now).
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

# Assign the Group column to the output Y
Ytemp = NeuroIm1_Final_AD_vs_NC_training$Group; # Output Matrix Y for SuperLearner
#Y = NeuroIm1_Final_AD_vs_MCI_training$Group; # Output Matrix Y for SuperLearner
#Y = NeuroIm1_Final_MCI_vs_NC_training$Group; # Output Matrix Y for SuperLearner

# Select the columns Patient ID [1], MMSE [3]  (Mini-Mental State Exam score, a cognitive assessment measure),
# and CDR [4] (Clinical Dementia Rating scale from the test dataset X) and [Group]
# and eliminate them from the training dataset because almost perfectly correlated to Y
w = which(names(NeuroIm1_Final_AD_vs_NC_training) == "Subject_ID" | names(NeuroIm1_Final_AD_vs_NC_training) == "Group" |
            names(NeuroIm1_Final_AD_vs_NC_training) == "MMSE" | names(NeuroIm1_Final_AD_vs_NC_training) == "CDR")
names(Xtemp)
Xtemp <- Xtemp[,-1*w] # Eliminate the output column (Group) from the training dataset X 
names(Xtemp)

## IMPUTATION AND NORMALIZATION STEP (OFFLINE ON THE WHOLE DATASET)
## DATA IMPUTATION
# Xtemp is the dataset to be imputed before the SL-LOOP
# Here I first replace % (i.e., misValperc) of the data with missing data (i.e., NA)
# Xtemp -> Xtemp.mis
misValperc = 0.3;
Xtemp_mis <- prodNA(Xtemp, noNA = misValperc)
#eval(parse(text=paste0("Xtemp_mis <- prodNA(Xtemp, noNA = " ,misValperc,")")))

# Here I impute the missing data in Xtemp.mis with the function missForest
# Xtemp.mis -> Xtemp.imp
Xtemp_imp <- missForest(Xtemp_mis, maxiter = 5)
#eval(parse(text=paste0("X_imp <- missForest(X_mis, maxiter = 5)")))

# #check imputed values, imputation error
# Xtemp_imp$ximp
# Xtemp_imp$OOBerror
# #comparing actual data accuracy
# Xtemp.err <- mixError(Xtemp_imp$ximp, Xtemp_mis, Xtemp)
# Xtemp.err

## MORE OPTIONS FOR IMPUTATION METHODS BELOW [EMPTY NOW]


# DATA NORMALIZATION of the sampled matrix without Group and Sex
a1 = which(names(Xtemp_imp$ximp) == "Group")
a2 = which(names(Xtemp_imp$ximp) == "Sex")
cont = 1:length(Xtemp_imp$ximp)
cont <- cont[-1*c(a1,a2)]
# DATA NORMALIZATION if IMPUTATION IS PERFORMED
Xtemp_imp$ximp[,cont] <- scale(Xtemp_imp$ximp[,cont])
# DATA NORMALIZATION if IMPUTATION IS NOT PERFORMED
#X[,cont] <- scale(X[,cont])
#rm(cont)



# SAMPLING OF THE PREDICTION DATASET
## SAMPLE THE PREDICTION DATASET
# Fraction (SET TO 15% BELOW) of data/patients to use for prediction, IN A BALANCED WAY
alpha=0.15
a1=round(length(which(Ytemp==1))*alpha);
a2=round(length(which(Ytemp==0))*alpha);
# selects randomly patients for prediction
q1 = sample(which(Ytemp==1),a1)
q2 = sample(which(Ytemp==0),a2)
q <- c(q1 , q2)
Xnew <- as.data.frame(Xtemp_imp$ximp[q,]) # define the patients to predict
Xnorm <- Xtemp_imp$ximp[-1*q,] # eliminate q patients for prediction [not used in the training]
Ypred <- Ytemp[q] # assign the output for the prediction set [not used in the training]
Ytemp <- Ytemp[-1*q] # eliminate q patients for prediction [not used in the training]

# SET THE SAME NAMES/LABELS FOR THE NEW MATRIX Xnew
names(Xnew) <- names(Xtemp_imp$ximp)
names(Xnorm) <- names(Xtemp_imp$ximp)

# STEPS 5 and 6 ADD LIBRARIES
## STEP 5 - SUPERLEARNER FUNCTION LOOP (TRAINING/VALIDATION): M,K and N
 
# Specify new SL prediction algorithm wrappers 
SL.glmnet.0 <- function(..., alpha = 0){
  SL.glmnet(..., alpha = alpha)
} # ridge penalty
SL.library <- c("SL.glm","SL.gam","SL.glmnet","SL.glmnet.0")

M=10000; # This is the number of random subsets of the big dataset [from 1e2 to 1e5] to perform SuperLearner on
coordSL=dim(Xnorm)
N=coordSL[1]
K=coordSL[2]

range_n <- c("60_80")
range_k <- c("30_50")

## SUPERLEARNER LOOP
for(j in 1:M) {
  Kcol <- round(K*runif(1,0.30,0.50)) # sample a value from a uniform distribution within 0.15 and 0.3 [number of columns/covariates between 15-30% of the big dataset]
  Nrow <- round(N*runif(1,0.6,0.8)) # sample a value from a uniform distribution within 0.6 and 0.8 [number of rows/subjects between 60-80% of the big dataset]
  #Nrow <- N # this option will NOT sample subjects/rows, it will include them all
  k <- sample(1:K,Kcol) # this is where I generate the sample of columns
  n <- sample(1:N,Nrow) # this is where I generate the sample of rows
  # Automated labeling of sub-matrices, assigned to X
  #eval(parse(text=paste0("X",j," <- as.data.frame(Xtemp[,k])")))
  #eval(parse(text=paste0("X",j," <- as.data.frame(dplyr::slice(X",j,",n))")))
  eval(parse(text=paste0("X",j," <- Xnorm[,k]")))
  #eval(parse(text=paste0("X",j," <- dplyr::slice(X",j,",n)")))
  eval(parse(text=paste0("X",j," <- X",j,"[n,]")))
  eval(parse(text=paste0("X <- X",j)))
  eval(parse(text=paste0("Y",j," <- Ytemp[n]")))
  eval(parse(text=paste0("Y <- Y",j)))

  eval(parse(text=paste0("k",j," <- k")))
  eval(parse(text=paste0("n",j," <- n")))
  
  print("ITERATION")
  print(j)
  print("% completed jobs")
  print(100*j/M)

# SUPERLEARNER-SL FUNCTION CALL that generates SL objects
  SL <- SuperLearner(Y,X,
                           family=binomial(),
                           SL.library=SL.library,
                           method="method.NNLS",
                           verbose = FALSE,
                           control = list(saveFitLibrary = TRUE),
                           cvControl = list(V=10));
    eval(parse(text=paste0("SL_",j," <- SL")));
    # Generate the prediction object from the SL object
    eval(parse(text=paste0("SL_Pred_",j," <- predict(SL_",j,", Xnew[,k",j,"])")))
    # remove the large SL object
    eval(parse(text=paste0("rm(SL_",j,")")))
}

# STEP 7 - GENERATING PREDICTIONS ON THE PREDICTION DATASET
    # Generates SL_Pred objects using the predict function on the prediction 
    # dataset with the SL object as the predictive model.
    # SL_Pred returns both the SuperLearner predictions ("pred") and 
    # predictions for each algorithm in the library (SL.library above)
#for(j in 1:M) {
#  eval(parse(text=paste0("SL_Pred_",j," <- predict(SL_",j,", Xnew[,k",j,"])")))
#} 

# Test if all the k are generated throughout the SuperLearner loop
for (j in 1:M){
  j
  eval(parse(text=paste0("k",j)))
  }


# STEPS 7 AND 8
## STEP 7 - GENERATING MSE
## MSE obtained by looping through the predictions made on the external dataset of q patients
## using the SuperLearner prediction algorithm output [SL_Pred_j], with the Xnew 
## matrix of the appropriate subset of covariates kj
Ynew = NeuroIm1_Final_AD_vs_NC_training$Group[q];
sum=0
for (j in 1:M) {
  for(i in 1:length(Ynew)) {
    eval(parse(text=paste0("sum <- sum(Ynew[",i,"] - SL_Pred_",j,"$pred[",i,"])^2")))
    } 
  eval(parse(text=paste0("MSE_",j," <- sum/length(Ynew)")))
  sum = 0;
}
## STEP 7 - RANKING MSE
## MSE obtained by looping through the predictions made on the external dataset of q patients
## using the SuperLearner prediction algorithm output [SL_Pred_j], with the Xnew 
## matrix of the appropriate subset of covariates kj

#  GENERATING THE ARRAY OF MSE FOR ALL THE M SL OBJECTS
MSE=0;
for (j in 1:M) {
  eval(parse(text=paste0("MSE[j] <- MSE_",j)))
}

# HISTOGRAMS
for (s in seq(10,M,M/10)){
  MSE_temp <- NULL
  MSE_sorted_temp <- NULL
  #MSE_temp <- data.table(mse=MSE[1:s],rank=1:s)
  MSE_temp <- data.frame(mse=MSE[1:s],rank=1:s)
  #MSE_sorted_temp <- setorder(MSE_temp, mse,-rank)
  #MSE_sorted_temp <- sort(MSE_temp, mse,-rank)
  MSE_sorted_temp <- MSE_temp[order(MSE_temp$mse),]
  ## DEFINE HERE THE TOP # OF COVARIATES TO LIST in the MODEL MINING STEP
  top = 10;
  eval(parse(text=paste0("k_top_",top,"_temp <- NULL")))
  for (i in 1:top){
      eval(parse(text=paste0("k_top_",top,"_temp <- c(k_top_",top,"_temp, k",MSE_sorted_temp$rank[i],")")))
    }
  ## STEP 8 - MODEL MINING (HISTOGRAMS OF TOP 20 or 50 COVARIATES)
  # GENERATE HISTOGRAMS OF THE TOP # OF COVARIATES
  eval(parse(text=paste0("x = k_top_",top,"_temp")))
  #h = hist(x)
  #eval(parse(text=paste0("h = hist(k_top_",top,"_temp,breaks=seq(min(k_top_",top,"_temp)-0.5,max(k_top_",top,"_temp)+0.5, by=1))")))
  #eval(parse(text=paste0("hist(k_top_",top,"_temp, breaks=seq(min(k_top_",top,"_temp)-0.5, max(k_top_",top,"_temp)+0.5, by=1))")))
  #h$density = h$counts/sum(h$counts)*100
  #plot(h,freq=FALSE,ylab='Density (%)',xlab='Covariate #')
  #:readline("Press <return to continue")
}


# RETRIEVE THE LABEL OF THE MORE FREQUENTLY SELECTED COVARIATES, echo=TRUE, message=FALSE, warning=FALSE}
# WITHIN THE TOP # OF COVARIATES IN THE PREDICTIONS
eval(parse(text=paste0("aqw <- data.frame(table(k_top_",top,"_temp))")))
aqw_ranked <- aqw[order(aqw$Freq),]

t1 = tail(aqw_ranked,10)
#t1=as.integer(t1[[1]])
ind_labels <- NULL
for (i in 1:top){
  ind_labels = c(ind_labels,as.integer(t1$k_top_10_temp[i]))
}
names(Xtemp)[ind_labels]

# Delete all the SL objects, nj arrays, Yj array and Xj matrices generated throughout the SuperLearner loop
for (j in 1:M){
  j
  #eval(parse(text=paste0("rm(SL_",j,")")))
  eval(parse(text=paste0("rm(n",j,")")))
  eval(parse(text=paste0("rm(Y",j,")")))
  eval(parse(text=paste0("rm(X",j,")")))
}

#range_n <- c("60_80")
#range_k <- c("15_30")
#misValperc
#M
#save.image("/users/simeonem/CBDA-SL/CBDA_SL_test.RData")
#save.image("/users/simeonem/CBDA-SL/CBDA_SL_M2_miss30_n60_80_k15_30.RData")
eval(parse(text=paste0("save.image(\"/users/simeonem/CBDA-SL/CBDA_SL_M",M,"_miss",misValperc*100,"_n",range_n,"_k",range_k,".RData\")")))

# Labeling the top covariates in the histogram}
#install.packages('calibrate')
#install.packages('MASS')
# library(calibrate)
# library(MASS)
# eval(parse(text=paste0("h = hist(k_top_",top,"_temp,breaks=seq(min(k_top_",top,"_temp)-0.5,max(k_top_",top,"_temp)+0.5, by=1))")))
#   #eval(parse(text=paste0("hist(k_top_",top,"_temp, breaks=seq(min(k_top_",top,"_temp)-0.5, max(k_top_",top,"_temp)+0.5, by=1))")))
# labels_temp <- NULL
# for (i in 1:length(Xtemp)){
# labels_temp <- c(labels_temp," ")
# }
# #labels_temp[t1$k_top_10_temp] <- names(Xtemp)[t1$k_top_10_temp]
# labels_temp[ind_labels] <- ind_labels
#   h$density = h$counts/sum(h$counts)*100;
#   plot(h,freq=FALSE,ylab='Density (%)',xlab='Covariate #',labels = labels_temp)
#   #textxy(t1, tail(aqw_ranked$N,10), names(Xtemp)[t1])
# #textxy(enrollmentData$YEAR, enrollmentData$UNEM, enrollmentData$ROLL)
# 
# ## Generates a list of empty labels first, then populates only the top ten labels at the correspondent positions
# 
# #load("~/Documents/NIH-grant/SOCR/CBDA_v2_temp.RData")
# 
