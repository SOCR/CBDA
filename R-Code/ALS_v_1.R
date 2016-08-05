# Load a session

## STEP 1 - DATA CLEANING
setwd("~/Desktop/R")

ALS1 = read.csv("ALS_TrainingData_2223.csv", header = TRUE)
# Set the list of packages/libraries to install/include (done through the ipak.R function) 
packages <- c("ggplot2", "plyr","dplyr", "colorspace","grid","data.table","VIM","MASS","Matrix",
              "lme4","arm","foreach","glmnet","class","nnet","mice","missForest",
              "calibrate","nnls","SuperLearner","plotrix","TeachingDemos","plotmo",
              "earth","parallel","splines","gam", "Amelia", "Hmisc", "mi",
              "BayesTree","e1071","randomForest","bartMachine")
source('~/Desktop/R/ipak.R')
#source('ipak.R')
ipak(packages)

#remove the column ALSFRS_slope, ID and SubjectID
Xals<-ALS1
w = which(names(ALS1) == "ALSFRS_slope"|names(ALS1)=="ID"|names(ALS1)=="SubjectID")
Xals <- Xals[,-1*w]
names(Xals)

# define the response variable is ALSFRS_slope, since the objective of this experiment
# is predicting the change of the ALSFRS slope change using the holistic patient-specific
# data.
w=which(names(ALS1)=="ALSFRS_slope")
Yals<-ALS1[,w]

# DATA NORMALIZATION
Xalsnorm <- scale(Xals)


## IMPUTATION AND NORMALIZATION STEP (OFFLINE ON THE WHOLE DATASET)
## DATA IMPUTATION
# Xals is the dataset to be imputed before the SL-LOOP
# Here I first replace % (i.e., misValperc) of the data with missing data (i.e., NA)
# Xals -> Xals.mis
misValperc = 0;
Xals_mis <- prodNA(Xalsnorm, noNA = misValperc)
#eval(parse(text=paste0("Xals_mis <- prodNA(Xals, noNA = " ,misValperc,")")))

# Here I impute the missing data in Xals.mis with the function missForest
# Xals.mis -> Xals.imp
Xals_imp <- missForest(Xals_mis, maxiter = 5)
#eval(parse(text=paste0("X_imp <- missForest(X_mis, maxiter = 5)")))
#Xals_imp$ximp
# #check imputed values, imputation error
# Xals_imp$ximp
# Xals_imp$OOBerror
# #comparing actual data accuracy
# Xals.err <- mixError(Xals_imp$ximp, Xals_mis, Xals)
# Xals.err



# SAMPLING OF THE PREDICTION DATASET
## SAMPLE THE PREDICTION DATASET
# Fraction (SET TO 15% BELOW) of data to use for prediction, IN A BALANCED WAY
alpha=0.15
dim(Xalsnorm )[1]
a1=round(dim(Xalsnorm )[1]*alpha);


# selects randomly patients for prediction
q=sample(nrow(Xalsnorm),a1)
mode(q)
Xals_new <- as.data.frame(Xals_imp$ximp[q,]) # define the patients to predict
Xals_train <-as.data.frame(Xals_imp$ximp[-1*q, ])# eliminate q patients for prediction [not used in the training]

Yals_new <- Yals[q ] # assign the output for the prediction set [not used in the training]
Yals_train <- Yals[-1*q]  # eliminate q patients for prediction [not used in the training]




## MORE OPTIONS FOR IMPUTATION METHODS BELOW [EMPTY NOW]
# STEPS 5 and 6 ADD LIBRARIES
## STEP 5 - SUPERLEARNER FUNCTION LOOP (TRAINING/VALIDATION): M,K and N
# Specify new SL prediction algorithm wrappers #
# I CAN EXPLAIN THIS STEP (wrappers) IN MORE DETAIL 
SL.glmnet.0 <- function(..., alpha = 0){
  SL.glmnet(..., alpha = alpha)
} # ridge penalty


SL.library <- c("SL.glm","SL.gam",
               "SL.glmnet","SL.glmnet.0")


M=10; # This is the number of random subsets of the big dataset [from 1e2 to 1e5] to perform SuperLearner on
coordSL=dim(Xals_train)
N=coordSL[1]
K=coordSL[2]


## SUPERLEARNER LOOP
for(j in seq(1:M)) {
  Kcol <- round(K*runif(1,0.15,0.3)) # sample a value from a uniform distribution within 0.15 and 0.3 [number of columns/covariates between 15-30% of the big dataset]
  Nrow <- round(N*runif(1,0.6,0.8)) # sample a value from a uniform distribution within 0.6 and 0.8 [number of rows/subjects between 60-80% of the big dataset]
  #Nrow <- N # this option will NOT sample subjects/rows, it will include them all
  k <- sample(1:K,Kcol) # this is where I generate the sample of columns
  n <- sample(1:N,Nrow) # this is where I generate the sample of rows
  
  # Automated labeling of sub-matrices, assigned to X
  #eval(parse(text=paste0("X",j," <- as.data.frame(Xals[,k])")))
  #eval(parse(text=paste0("X",j," <- as.data.frame(dplyr::slice(X",j,",n))")))
  eval(parse(text=paste0("X",j," <- Xals_train[,k]")))
  #eval(parse(text=paste0("X",j," <- dplyr::slice(X",j,",n)")))
  eval(parse(text=paste0("X",j," <- X",j,"[n,]")))
  eval(parse(text=paste0("X <- X",j)))
  
  eval(parse(text=paste0("Y",j," <- Yals_train[n]")))
  eval(parse(text=paste0("Y <- Y",j)))
  
  eval(parse(text=paste0("k",j," <- k")))
  eval(parse(text=paste0("n",j," <- n")))
  

  
  #readline("Press <return to continue")
  # SUPERLEARNER-SL FUNCTION CALL that generates SL objects
  SL <- SuperLearner(Y=Y,X=X,
                     family=gaussian(),
                     SL.library=SL.library,
                     method="method.NNLS",
                     verbose = TRUE,
                     control = list(saveFitLibrary = TRUE),
                     cvControl = list(V=10));
  eval(parse(text=paste0("SL_",j," <- SL")));
  eval(parse(text=paste0("SL_Pred_",j," <- predict(SL_",j,", Xals_new[,k",j,"])")))
  eval(parse(text=paste0("rm(SL_",j,")")))
  print("ITERATION")
  print(j)
  print("% completed jobs")
  print(100*j/M)
}

# STEP 7 - GENERATING PREDICTIONS ON THE PREDICTION DATASET
# Generates SL_Pred objects using the predict function on the prediction 
# dataset with the SL object as the predictive model.
# SL_Pred returns both the SuperLearner predictions ("pred") and 
# predictions for each algorithm in the library (SL.library above)
#for(j in seq(1:M)) {
#  eval(parse(text=paste0("SL_Pred_",j," <- predict(SL_",j,", Xnew[,k",j,"])")))
#} 

# Test if all the k are generated throughout the SuperLearner loop
for (j in seq(1:M)){
  j
  eval(parse(text=paste0("k",j)))
}


# STEPS 7 AND 8
## STEP 7 - GENERATING MSE
## MSE obtained by looping through the predictions made on the external dataset of q patients
## using the SuperLearner prediction algorithm output [SL_Pred_j], with the Xnew 
## matrix of the appropriate subset of covariates kj
sum=0
for (j in 1:M) {
  for(i in 1:length(Yals_new)) {
    eval(parse(text=paste0("sum <- sum(Yals_new[",i,"] - SL_Pred_",j,"$pred[",i,"])^2")))
  } 
  eval(parse(text=paste0("MSE_",j," <- sum/length(Yals_new)")))
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
for (s in seq(10,M,10)){
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
  #readline("Press <return to continue")
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
names(Xals)[ind_labels]

# Delete all the SL objects, nj arrays, Yj array and Xj matrices generated throughout the SuperLearner loop
for (j in seq(1:M)){
  j
  eval(parse(text=paste0("rm(n",j,")")))
  eval(parse(text=paste0("rm(Y",j,")")))
  eval(parse(text=paste0("rm(X",j,")")))
}

# LABELS TO IDENTIFY THE WORKSPACE THAT WILL BE SAVED ON THE NEXT COMMAND
range_n=c("60_80")
range_k=c("15_30")
#misValperc
#M

# EDIT THE DIRECTORY WHERE YOU WANT THE WORKSPACE TO BE SAVED
#eval(parse(text=paste0("save.image(\"/users/simeonem/CBDA-SL/CBDA_SL_M",M,"_miss",misValperc*100,"_n",range_n,"_k",range_k,".RData\")")))
eval(parse(text=paste0("save.image(\"/Desktop/R/ALS",M,"_miss",misValperc*0,"_n",range_n,"_k",range_k,".RData\")")))
#eval(parse(text=paste0("save.image(\"/users/simeonem/CBDA-SL/CBDA_SL_M",M,"_miss",misValperc*100,"_n",range_n,"_k",range_k,".RData\")")))

# Labeling the top covariates in the histogram}
#install.packages('calibrate')
#install.packages('MASS')
# library(calibrate)
# library(MASS)
eval(parse(text=paste0("h = hist(k_top_",top,"_temp,breaks=seq(min(k_top_",top,"_temp)-0.5,max(k_top_",top,"_temp)+0.5, by=1))")))
#   #eval(parse(text=paste0("hist(k_top_",top,"_temp, breaks=seq(min(k_top_",top,"_temp)-0.5, max(k_top_",top,"_temp)+0.5, by=1))")))
labels_temp <- NULL
for (i in 1:length(Xals)){
  labels_temp <- c(labels_temp," ")
}
# #labels_temp[t1$k_top_10_temp] <- names(Xals)[t1$k_top_10_temp]
labels_temp[ind_labels] <- ind_labels
h$density = h$counts/sum(h$counts)*100;
plot(h,freq=FALSE,ylab='Density (%)',xlab='Covariate #',labels = labels_temp)
#   #textxy(t1, tail(aqw_ranked$N,10), names(Xals)[t1])
# #textxy(enrollmentData$YEAR, enrollmentData$UNEM, enrollmentData$ROLL)
# 
# ## Generates a list of empty labels first, then populates only the top ten labels at the correspondent positions
# 
# #load("~/Documents/NIH-grant/SOCR/CBDA_v2_temp.RData")
# 
