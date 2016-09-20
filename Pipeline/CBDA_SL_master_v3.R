#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

arg_file = as.array(args[1])
i=as.numeric(args[2])
dataset_file = as.array(args[3])
workspace_directory=as.array(args[4])
# /ifs/loni/ccb/temp/CBDA_SL

eval(parse(text=paste0("arguments = read.table(arg_file, header = TRUE)")))

M <-arguments[i,1]
misValperc <- arguments[i,2]
Kcol_min <- arguments[i,3]
Kcol_max <- arguments[i,4]
Nrow_min <- arguments[i,5]
Nrow_max <- arguments[i,6]
#Complete <- as.array(arguments[i,7])
#Light <- as.array(arguments[i,8])
## Labels for the workspaces
  #Complete <- as.array(arguments[i,7])
  #Light <- as.array(arguments[i,8])
  ## Labels for the workspaces
  
range_n <- eval(parse(text=paste0("c(\"",Nrow_min,"_",Nrow_max,"\")")))
range_k <- eval(parse(text=paste0("c(\"",Kcol_min,"_",Kcol_max,"\")")))

#print(workspace_directory)
#eval(parse(text=paste0("save.image(\"~/CBDA-SL/CBDA_SL_M",M,"_miss",misValperc,"_n",range_n,"_k",range_k,"_test.RData\")")))

#Set the list of packages/libraries to install/include (done through the ipak.R function)
packages <- c("ggplot2", "plyr","dplyr", "colorspace","grid","data.table","VIM","MASS","Matrix",
              "lme4","arm","foreach","glmnet","class","nnet","mice","missForest",
              "calibrate","nnls","SuperLearner","plotrix","TeachingDemos","plotmo",
              "earth","parallel","splines","gam", "Hmisc","Amelia",  "mi",
              "BayesTree","e1071","randomForest","bartMachine")

## ipak function below: install (if missing) and load (if installed) multiple R packages
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE, repos='http://cran.rstudio.com/')
  sapply(pkg, require, character.only = TRUE)
}

install.packages('package_name', dependencies=TRUE, repos='http://cran.rstudio.com/')
ipak(packages)

# library("ggplot2")
# library("plyr")
# #library("dplyr")
# library("colorspace")
# library("grid")
# library("lattice")
# #library("data.table")
# #library("VIM")
# library("MASS")
# library("Matrix")
# #library("lme4")
# #library("arm")
# library("foreach")
# #library("glmnet")
# library("class")
# library("nnet")
# #library("mice")
# library("missForest")
# library("calibrate")
# library("nnls")
# library("SuperLearner")
# library("plotrix")
# library("TeachingDemos")
# library("plotmo")
# library("earth")
# library("parallel")
# library("splines")
# library("gam") 
# library("Hmisc")
# library("Amelia")
# library("mi")
# library("BayesTree")
# library("e1071")
# library("randomForest")
#library("bartMachine")

## STEP 1 - DATA CLEANING/HARMONIZATION for the MRI dataset "NeuroIm1.txt"

## This step is DATA-DEPENDENT and it can only be generalized for the IMPUTATION/NORMALIZATION STEP
## Retrieve the dataset
eval(parse(text=paste0("NeuroIm1 = read.table(dataset_file, header = TRUE)")))
# Delete the last 3 columns from the big matrix NeurIm1 ["ROI","Measure","Value"]
# and store the rest in a temp matrix, compressing unique values by patients
NeuroIm1_Fix <- unique(NeuroIm1[,-1*(11:13)])

# Define Variables/Columns: Patients, type of Measures and ROI [Region of Interest]
Patients <- NeuroIm1_Fix$Subject_ID
Measures <- c("SA","SI","CV","FD")
ROI <- unique(NeuroIm1$ROI)

# Initialize a new data matrix that has the correct # of columns
NeuroIm1_NEW = array(0, c(length(Patients), length(ROI)*length(Measures)))

## Assign names to the columns in the form of Value_Measure_ROI
names = NULL
for (j in 1:length(Measures)) {
  for (i in 1:length(ROI))
    names = c(names, paste("Value",Measures[j], ROI[i],"END", sep="_"))
}
names(NeuroIm1_NEW) <- names


# DATA HARMONIZATION
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

# Assign the Group column to the output Y
Ytemp = NeuroIm1_Final_AD_vs_NC_training$Group; # Output Matrix Y for SuperLearner

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
Xtemp_mis <- prodNA(Xtemp, noNA = misValperc/100)
#eval(parse(text=paste0("Xtemp_mis <- prodNA(Xtemp, noNA = " ,misValperc,")")))

# Here I impute the missing data in Xtemp.mis with the function missForest
# Xtemp.mis -> Xtemp.imp
Xtemp_imp <- missForest(Xtemp_mis, maxiter = 10)
#eval(parse(text=paste0("X_imp <- missForest(X_mis, maxiter = 5)")))

# #check imputed values, imputation error
# Xtemp_imp$ximp
# Xtemp_imp$OOBerror
# #comparing actual data accuracy
# Xtemp.err <- mixError(Xtemp_imp$ximp, Xtemp_mis, Xtemp)
# Xtemp.err

## MORE OPTIONS FOR IMPUTATION METHODS BELOW [EMPTY NOW]


## DATA NORMALIZATION of the sampled matrix without Group and Sex
## This step can be generalized if the data is formatted RAW, with categorical variables as binary or strings
a1 = which(names(Xtemp_imp$ximp) == "Group")
a2 = which(names(Xtemp_imp$ximp) == "Sex")
cont = 1:length(Xtemp_imp$ximp)
cont <- cont[-1*c(a1,a2)]
# DATA NORMALIZATION if IMPUTATION IS PERFORMED
Xtemp_imp$ximp[,cont] <- scale(Xtemp_imp$ximp[,cont])

## SAMPLE THE PREDICTION DATASET -- THIS STEP IS DATA INDEPENDENT
## The fraction alpha of data/patients to use for prediction (SET TO 15% BELOW)can be passed as an input argument as well
## Below the sampling is balanced
alpha=0.15
a1=round(length(which(Ytemp==1))*alpha);
a2=round(length(which(Ytemp==0))*alpha);
# Randomly select patients for prediction
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
# Specify new SL prediction algorithm wrappers 
SL.glmnet.0 <- function(..., alpha = 0){
  SL.glmnet(..., alpha = alpha)
} # ridge penalty

SL.glmnet.0.25 <- function(..., alpha = 0.25){
  SL.glmnet(..., alpha = alpha)
}

SL.glmnet.0.50 <- function(..., alpha = 0.50){
  SL.glmnet(..., alpha = alpha)
}

SL.glmnet.0.75 <- function(..., alpha = 0.75){
  SL.glmnet(..., alpha = alpha)
}

SL.gam.1<-function(...,control=gam.control(deg.gam=1)){
  SL.gam(...,control=control)
}
SL.gam.3<-function(...,control=gam.control(deg.gam=3)){
  SL.gam(...,control=control)
}
SL.gam.4<-function(...,control=gam.control(deg.gam=4)){
  SL.gam(...,control=control)
}
SL.gam.5<-function(...,control=gam.control(deg.gam=5)){
  SL.gam(...,control=control)
}


create.SL.glmnet.alpha<-function(...,alpha=c(0.25,0.5,0.75))
{
  SL.glmnet(..., alpha=alpha)
}
SL.library <- c("SL.glm","SL.gam","SL.gam.1","SL.gam.3","SL.gam.4","SL.gam.5",
                "SL.glmnet","SL.glmnet.0","SL.glmnet.0.25","SL.glmnet.0.50","SL.glmnet.0.75",
                "SL.svm",
                "SL.randomForest")
#                "SL.bartMachine")

## Asess the dimensions of the normalized data matrix
coordSL=dim(Xnorm)
N=coordSL[1]
K=coordSL[2]


## SUPERLEARNER LOOP
system.time({
  for(j in 1:M)
  {
    Kcol <- round(K*runif(1,Kcol_min/100,Kcol_max/100)) # sample a value from a uniform distribution within 0.15 and 0.3 [number of columns/covariates between 15-30% of the big dataset]
    Nrow <- round(N*runif(1,Nrow_min/100,Nrow_max/100)) # sample a value from a uniform distribution within 0.6 and 0.8 [number of rows/subjects between 60-80% of the big dataset]
    #Kcol <- round(K*runif(1,0.05,0.15)) # sample a value from a uniform distribution within 0.15 and 0.3 [number of columns/covariates between 15-30% of the big dataset]
    #Nrow <- round(N*runif(1,0.6,0.8)) # sample a value from a uniform distribution within 0.6 and 0.8 [number of rows/subjects between 60-80% of the big dataset]
    #Nrow <- N # this option will NOT sample subjects/rows, it will include them all
    k <- sample(1:K,Kcol) # this is where I generate the sample of columns
    #n <- sample(1:N,Nrow) # this is where I generate the sample of rows
    n <- 1:N # this is where I generate the sample of rows
    # Automated labeling of sub-matrices, assigned to X
    eval(parse(text=paste0("X",j," <- Xnorm[,k]")))
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
    
    SL <- try(SuperLearner(Y,X,
                           family=binomial(),
                           SL.library=SL.library,
                           method="method.NNLS",
                           verbose = TRUE,
                           control = list(saveFitLibrary = TRUE),
                           cvControl = list(V=10)));
    eval(parse(text=paste0("SL_",j," <- SL")));
    # Generate the prediction object from the SL object
    # STEP 7 - GENERATING PREDICTIONS ON THE PREDICTION DATASET
    # Generates SL_Pred objects using the predict function on the prediction 
    # dataset with the SL object as the predictive model.
    # SL_Pred returns both the SuperLearner predictions ("pred") and 
    # predictions for each algorithm in the library (SL.library above)
    eval(parse(text=paste0("try(SL_Pred_",j," <- predict(SL_",j,", Xnew[,k",j,"]))")))
    # remove the large SL object
    print(SL_Pred_1)
    #eval(parse(text=paste0("rm(SL_",j,")")))
#    print("ITERATION")
#    print(j)
#    print("% completed jobs")
#    print(100*j/M)
  }
})[[3]]

Complete = "Complete"
Light = "Light"
# SAVE THE COMPLETE DATASET WITH ALL THE SL OBJECTS
eval(parse(text=paste0("save.image(\"",workspace_directory,"CBDA_SL_M",M,"_miss",misValperc,"_n",range_n,"_k",range_k,".RData\")")))
#eval(parse(text=paste0("save.image(\"~/CBDA-SL/CBDA_SL_M",M,"_miss",misValperc,"_n",range_n,"_k",range_k,"_Complete.RData\")")))
#eval(parse(text=paste0("save.image(\"Desktop/R/CBDA_SL_M",M,"_miss",misValperc*100,"_n",range_n,"_k",range_k,".RData\")")))

# GENERATE THE LIGHT DATASET BY DELETING THE SL OBJECTS
for (j in 1:M){
  eval(parse(text=paste0("rm(SL_",j,")")))
}
#eval(parse(text=paste0("save.image(\"~/CBDA-SL/CBDA_SL_M",M,"_miss",misValperc,"_n",range_n,"_k",range_k,"_Light.RData\")")))
eval(parse(text=paste0("save.image(\"",workspace_directory,"CBDA_SL_M",M,"_miss",misValperc,"_n",range_n,"_k",range_k,"_Light.RData\")")))

print(workspace_directory)