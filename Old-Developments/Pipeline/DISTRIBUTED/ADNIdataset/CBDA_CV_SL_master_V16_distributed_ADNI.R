#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
print(args)
arg_file = as.array(args[1])
print(arg_file)
j_global=as.numeric(args[2])
dataset_file = as.array(args[3])
workspace_directory=as.array(args[4])
i_exp=as.numeric(args[5])

# label to append to the RData workspaces as soon as they are created
label=c("ADNI_dataset_MN")
# /ifshome/pipelnvt/ # home directory as aguest on LONI Pipeline on "Cranium"

#Set the list of packages/libraries to install/include (done through the ipak.R function)
packages <- c("ggplot2", "plyr", "colorspace","grid","data.table","VIM","MASS","Matrix",
              "lme4","arm","foreach","glmnet","class","nnet","mice","missForest",
              "calibrate","nnls","SuperLearner","plotrix","TeachingDemos","plotmo",
              "earth","parallel","splines","gam","mi",
              "BayesTree","e1071","randomForest", "Hmisc","dplyr","Amelia","bartMachine","knockoff")

## ipak function below: install (if missing) and load (if installed) multiple R packages
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE, repos='http://cran.rstudio.com/')
  sapply(pkg, require, character.only = TRUE)
}

#install.packages('package_name', dependencies=TRUE, repos='http://cran.rstudio.com/')
ipak(packages)

## Reads the dataset to be processed as passed from the input argument
#dataset_file = c("ADNI_dataset.txt")
#setwd("C:/Users/simeonem/Documents/CBDA-SL/ExperimentsNov2016/ADNI/")
eval(parse(text=paste0("ADNI = read.csv(dataset_file, header = TRUE)")))

## Change the name of the outcome variable
names(ADNI)[8]<- "subjectinfo"

## columns to eliminate --> c(1:6,9:11,17,20)
ADNI <- ADNI[,-c(1:6,9:11,17,20)]

# Make the variable SubjectSex binomial
#ADNI$subjectSex <- ifelse(ADNI$subjectSex == 'F',0,1)
ADNI$subjectSex <- as.numeric(ADNI$subjectSex == "M")

# Assign the Group column to the output Y
Ytemp = ADNI$subjectinfo; # Output Matrix Y for SuperLearner

# Define the temporary output [Ytemp] and input [Xtemp] matrices for the SuperLearner call
Xtemp = ADNI[,-2]; # temporary X-->Xtemp to modify and pass to SuperLearner


## Since ADNI dataset is multinomial, i.e. the outcome has 4 categories --> c("Normal","AD","MCI","LMCI"),
## in order to use the SuperLearner with family = Binomial, I will run 4 different Superlearner testing/predicting
## each category vs everything else. Ultimately  I will pick the max probability predicted in each SuperLearner run.
Ytemp_Normal = as.numeric(Ytemp == "Normal")
Ytemp_AD = as.numeric(Ytemp == "AD")
Ytemp_MCI = as.numeric(Ytemp == "MCI")
Ytemp_LMCI = as.numeric(Ytemp == "LMCI")

# SET THE SAME NAMES/LABELS FOR THE X dataset
original_names <- names(Xtemp)
names(Xtemp) <- 1:dim(Xtemp)[2]

## IMPUTATION AND NORMALIZATION STEP (OFFLINE ON THE WHOLE DATASET)
## DATA IMPUTATION
# Xtemp is the dataset to be imputed before the SL-LOOP
# Here I first replace % (i.e., misValperc) of the data with missing data (i.e., NA)
#arg_file = c("arg_ADNI.txt")
eval(parse(text=paste0("arguments = read.table(arg_file, header = TRUE)")))
misValperc <- arguments[i_exp,2]
# For real datasets, there's no need to pass the missValperc, because 
# we will impute whatever the missing values are

#Xtemp_mis <- prodNA(Xtemp, noNA = misValperc/100)
Xtemp_mis <- Xtemp
# Here I impute the missing data in Xtemp.mis with the function missForest
Xtemp_imp <- missForest(Xtemp_mis, maxiter = 5)

## DATA NORMALIZATION of the sampled matrix without Group and Sex
## This step can be generalized if the data is formatted RAW,
# with categorical variables as binary or strings (see commented out example below)
# a1 = which(names(Xtemp_imp$ximp) == "Group")
# a2 = which(names(Xtemp_imp$ximp) == "Sex")
# cont = 1:length(Xtemp_imp$ximp)
# cont <- cont[-1*c(a1,a2)]
# # DATA NORMALIZATION if IMPUTATION IS PERFORMED
Xnorm_ALL <- as.data.frame(scale(Xtemp_imp$ximp))

## SAMPLE THE PREDICTION DATASET -- THIS STEP IS DATA INDEPENDENT AND FIXED (THE SEED IS ALWAYS THE SAME)
## The fraction alpha of data/patients to use for prediction could be passed as an input argument as well
## Below the sampling is balanced
## Eliminating q subjects for prediction, in a BALANCED WAY
alpha = 0.30; # % of the initial subjects to set aside for prediction
## How many categories are in the multinomial outcome
#num_cat = 4;
## Subjects to sample in a balanced way from the multinomial outcome
#a0 = round(alpha*dim(Xtemp)[1]/num_cat)
# Randomly select patients for prediction
#set.seed(843)
q1_Normal = sample(which(Ytemp == "Normal"),round(alpha*length(which(Ytemp == "Normal"))))
#set.seed(843)
q2_AD = sample(which(Ytemp == "AD"),round(alpha*length(which(Ytemp == "AD"))))
#set.seed(843)
q3_MCI = sample(which(Ytemp == "MCI"),round(alpha*length(which(Ytemp == "MCI"))))
#set.seed(843)
q4_LMCI = sample(which(Ytemp == "LMCI"),round(alpha*length(which(Ytemp == "LMCI"))))
q <- c(q1_Normal , q2_AD , q3_MCI , q4_LMCI)
q <- sort(q)

#set.seed(j_global)
# Selecting the q patients from the training dataset [not used in the training]  (renamed as Xpred)
Xpred <- Xnorm_ALL[q,]
# Eliminating the q patients from the "training/learning" matrix of features (renamed as Xnorm_sub) 
Xnorm_sub <- Xnorm_ALL[-1*q,]  
Ypred <- Ytemp[q] # define the output for prediction (renamed Ypred) [not used in the training/learning]
Ytemp_sub <- Ytemp[-1*q] # define the output for learning by eliminating the q subjects that are not used in the training/learning

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

## The bartMachine wrapper won't be necessary with the latest release of the SL.bartMachine.
## It's not properly installed yet.

#' Wrapper for bartMachine learner
#'
#' Support bayesian additive regression trees via the bartMachine package.
#'
#' @param Y Outcome variable
#' @param X Covariate dataframe
#' @param newX Optional dataframe to predict the outcome
#' @param obsWeights Optional observation-level weights (supported but not tested)
#' @param id Optional id to group observations from the same unit (not used
#'   currently).
#' @param family "gaussian" for regression, "binomial" for binary
#'   classification
#' @param num_trees The number of trees to be grown in the sum-of-trees model.
#' @param num_burn_in Number of MCMC samples to be discarded as "burn-in".
#' @param num_iterations_after_burn_in Number of MCMC samples to draw from the
#'   posterior distribution of f(x).
#' @param alpha Base hyperparameter in tree prior for whether a node is
#'   nonterminal or not.
#' @param beta Power hyperparameter in tree prior for whether a node is
#'   nonterminal or not.
#' @param k For regression, k determines the prior probability that E(Y|X) is
#'   contained in the interval (y_{min}, y_{max}), based on a normal
#'   distribution. For example, when k=2, the prior probability is 95\%. For
#'   classification, k determines the prior probability that E(Y|X) is between
#'   (-3,3). Note that a larger value of k results in more shrinkage and a more
#'   conservative fit.
#' @param q Quantile of the prior on the error variance at which the data-based
#'   estimate is placed. Note that the larger the value of q, the more
#'   aggressive the fit as you are placing more prior weight on values lower
#'   than the data-based estimate. Not used for classification.
#' @param nu Degrees of freedom for the inverse chi^2 prior. Not used for
#'   classification.
#' @param verbose Prints information about progress of the algorithm to the
#'   screen.
#' @param ... Additional arguments (not used)
#'
#' @encoding utf-8
#' @export
SL.bartMachine <- function(Y, X, newX, family, obsWeights, id,
                           num_trees = 50, num_burn_in = 250, verbose = F,
                           alpha = 0.95, beta = 2, k = 2, q = 0.9, nu = 3,
                           num_iterations_after_burn_in = 1000,
                           ...) {
  #.SL.require("bartMachine")
  model = bartMachine::bartMachine(X, Y, num_trees = num_trees,
                                   num_burn_in = num_burn_in, verbose = verbose,
                                   alpha = alpha, beta = beta, k = k, q = q, nu = nu,
                                   num_iterations_after_burn_in = num_iterations_after_burn_in)
  # pred returns predicted responses (on the scale of the outcome)
  pred <- predict(model, newX)
  # fit returns all objects needed for predict.SL.template
  fit <- list(object = model)
  #fit <- vector("list", length=0)
  class(fit) <- c("SL.bartMachine")
  out <- list(pred = pred, fit = fit)
  return(out)
}

#' bartMachine prediction
#' @param object SuperLearner object
#' @param newdata Dataframe to predict the outcome
#' @param family "gaussian" for regression, "binomial" for binary
#'   classification. (Not used)
#' @param Y Outcome variable (not used)
#' @param X Covariate dataframe (not used)
#' @param ... Additional arguments (not used)
#'
#' @export
predict.SL.bartMachine <- function(object, newdata, family, X = NULL, Y = NULL,...) {
  pred <- predict(object$object, newdata)
  return(pred)
}
# SL.library <- c("SL.glm","SL.gam","SL.gam.1","SL.gam.3","SL.gam.4","SL.gam.5",
#                 "SL.glmnet","SL.glmnet.0","SL.glmnet.0.25","SL.glmnet.0.50","SL.glmnet.0.75",
#                 "SL.svm",
#                 "SL.randomForest","SL.bartMachine")
SL.library <- c("SL.glm",
                "SL.glmnet","SL.glmnet.0","SL.glmnet.0.25","SL.glmnet.0.50","SL.glmnet.0.75",
                "SL.svm","SL.randomForest","SL.bartMachine")

## Assess the dimensions of the normalized data matrix
coordSL=dim(Xnorm_sub)
N=coordSL[1]
K=coordSL[2]


## INITIALIZATION BEFORE THE SUPERLEARNER LOOP
M <-arguments[i_exp,1]
print(misValperc)
Kcol_min <- arguments[i_exp,3]
Kcol_max <- arguments[i_exp,4]
Nrow_min <- arguments[i_exp,5]
Nrow_max <- arguments[i_exp,6]
range_n <- eval(parse(text=paste0("c(\"",Nrow_min,"_",Nrow_max,"\")")))
range_k <- eval(parse(text=paste0("c(\"",Kcol_min,"_",Kcol_max,"\")")))

# sample a value from a uniform distribution for the SSR-Subject Sampling Range [based on the arguments passed, Nrow_min-Nrow_max]
Nrow <- round(dim(Xnorm_sub)[1]*(runif(1,Nrow_min/100,Nrow_max/100))) 
eval(parse(text=paste0("n <- sample(1:dim(Xnorm_sub)[1],Nrow)")))

# sample a value from a uniform distribution for the FSR-Feature Sampling Range [based on the arguments passed, Kcol_min-Kcol_max]
Kcol <- round(dim(Xnorm_sub)[2]*(runif(1,Kcol_min/100,Kcol_max/100))) 
eval(parse(text=paste0("k <- sample(1:dim(Xnorm_sub)[2],Kcol)")))

print(c(j_global,i_exp))
n <- sort(sample(1:N,Nrow)) # this is where I generate the sample of rows
k <- sort(sample(1:K,Kcol)) # this is where I generate the sample of columns

# Automated labeling of sub-matrices, assigned to X
# eval(parse(text=paste0("X",j_global," <- Xnorm_sub[n,k]")))
# eval(parse(text=paste0("X <- as.data.frame(X",j_global,")")))

X <- Xnorm_sub[n,k];
## Eliminate the prediction set from the outcome variable for training
Y_Normal <- Ytemp_Normal[-q]
Y_AD <- Ytemp_AD[-q]
Y_MCI <- Ytemp_MCI[-q]
Y_LMCI <- Ytemp_LMCI[-q]
## Outcome variable sampled for training
Y_Normal <- Y_Normal[n]
Y_AD <- Y_AD[n]
Y_MCI <- Y_MCI[n]
Y_LMCI <- Y_LMCI[n]
# eval(parse(text=paste0("Y",j_global," <- Ytemp_sub[n]")))
# eval(parse(text=paste0("Y <- Y",j_global)))
eval(parse(text=paste0("k",j_global," <- k")))
eval(parse(text=paste0("n",j_global," <- n")))
# c("Normal","AD","MCI","LMCI")

## KNOCKOFF FILTER IMPLEMENTATION  
## IMPORTANT  --> subjects # >> features # !!!
## It creates KO_result_j objects with all the stats, results, FDR proportion,...
# knockoff.filter(X, Y, fdr = 0.2, statistic = NULL,
# threshold = c("knockoff", "knockoff+"), knockoffs = c("equicorrelated","sdp"),
#               normalize = TRUE, randomize = FALSE)

#eval(parse(text=paste0("KO_result_",j_global," = knockoff.filter(Xnorm_sub[n",j_global,",k",j_global,"], Y[n",j_global,"],fdr = 0.05)")))
eval(parse(text=paste0("KO_result_Normal_",j_global," = knockoff.filter(X,Y_Normal,fdr = 0.05)")))
eval(parse(text=paste0("KO_result_AD_",j_global," = knockoff.filter(X,Y_AD,fdr = 0.05)")))
eval(parse(text=paste0("KO_result_MCI_",j_global," = knockoff.filter(X,Y_MCI,fdr = 0.05)")))
eval(parse(text=paste0("KO_result_LMCI_",j_global," = knockoff.filter(X,Y_LMCI,fdr = 0.05)")))

eval(parse(text=paste0("KO_selected_Normal_",j_global," <- as.numeric(sub(\"V\",\"\",names(KO_result_Normal_",j_global,"$selected)))")))
eval(parse(text=paste0("KO_selected_AD_",j_global," <- as.numeric(sub(\"V\",\"\",names(KO_result_AD_",j_global,"$selected)))")))
eval(parse(text=paste0("KO_selected_MCI_",j_global," <- as.numeric(sub(\"V\",\"\",names(KO_result_MCI_",j_global,"$selected)))")))
eval(parse(text=paste0("KO_selected_LMCI_",j_global," <- as.numeric(sub(\"V\",\"\",names(KO_result_LMCI_",j_global,"$selected)))")))

# eval(parse(text=paste0("print(KO_selected_Normal_",j_global,")")))
# eval(parse(text=paste0("print(KO_selected_AD_",j_global,")")))
# eval(parse(text=paste0("print(KO_selected_MCI_",j_global,")")))
# eval(parse(text=paste0("print(KO_selected_LMCI_",j_global,")")))

eval(parse(text=paste0("KO_selected_",j_global," <- unique(c(KO_selected_Normal_",j_global,",KO_selected_AD_",
                       j_global,",KO_selected_MCI_",j_global,",KO_selected_LMCI_",j_global,"))")))
## SUPERLEARNER LOOP
# SUPERLEARNER-SL FUNCTION CALL that generates SL objects
## Superlearner Function ##
SL_Normal <- try(SuperLearner(Y_Normal , X , newX = Xpred[,k],
                              family=binomial(),
                              SL.library=SL.library,
                              method="method.NNLS",
                              verbose = FALSE,
                              control = list(saveFitLibrary = TRUE),
                              cvControl = list(V=10)));
SL_AD <- try(SuperLearner(Y_AD , X , newX = Xpred[,k],
                              family=binomial(),
                              SL.library=SL.library,
                              method="method.NNLS",
                              verbose = FALSE,
                              control = list(saveFitLibrary = TRUE),
                              cvControl = list(V=10)));
SL_MCI <- try(SuperLearner(Y_MCI , X , newX = Xpred[,k],
                              family=binomial(),
                              SL.library=SL.library,
                              method="method.NNLS",
                              verbose = FALSE,
                              control = list(saveFitLibrary = TRUE),
                              cvControl = list(V=10)));
SL_LMCI <- try(SuperLearner(Y_LMCI , X , newX = Xpred[,k],
                              family=binomial(),
                              SL.library=SL.library,
                              method="method.NNLS",
                              verbose = FALSE,
                              control = list(saveFitLibrary = TRUE),
                              cvControl = list(V=10)));
# eval(parse(text=paste0("SL_",j_global," <- SL")));
# eval(parse(text=paste0("SL_",j_global)))

#SL_pred <- data.frame(pred_A = fit_A$SL.predict[, 1], pred_B = fit_B$SL.predict[, 1], pred_C = fit_C$SL.predict[, 1])
SL_pred <- data.frame(pred_Normal = SL_Normal$SL.predict[, 1], pred_AD = SL_AD$SL.predict[, 1], 
                      pred_MCI = SL_MCI$SL.predict[, 1], pred_LMCI = SL_LMCI$SL.predict[, 1])
Classify <- apply(SL_pred, 1, function(xx) c("Normal","AD", "MCI", "LMCI")[unname(which.max(xx))])
eval(parse(text=paste0("SL_pred",j_global," <- Classify")))

# table(Classify, Ypred)
# library("caret")
# confusionMatrix(Classify, Ypred)
#table(Classify, Y_test)

# STEP 7 - GENERATING PREDICTIONS ON THE PREDICTION DATASET
# Generates SL_Pred object using the predict function on the prediction 
# dataset with the SL object as the predictive model.
# SL_Pred returns both the SuperLearner predictions ("pred") and 
# predictions for each algorithm in the library (SL.library above)
#eval(parse(text=paste0("try(SL_Pred_",j_global," <- predict(SL_",j_global,", Xpred[,k",j_global,"]))")))

# This checks if the SL_Pred object was successfully generated (i.e., if it exists)
# If it does not exist, it is set to a double equal to 100
eval(parse(text=paste0("ifelse(exists(\"SL_Pred_",j_global,"\"),'OK',
                   SL_Pred_",j_global," <- 100)")))

# GENERATE THE LIGHT DATASET BY DELETING THE SL OBJECT
rm(SL_Normal)
rm(SL_AD)
rm(SL_MCI)
rm(SL_LMCI)
  
nonzero=0;
# SAVE THE RDATA WORKSPACE WITH THE ALL DATA
eval(parse(text=paste0("save(Xpred,label, Xnorm_ALL, Xnorm_sub, q, Ypred, M, Ytemp, Ytemp_sub, SL_Pred_",j_global,
                       ",nonzero,n",j_global,",k",j_global,",KO_selected_",j_global,",
                       file= \"",workspace_directory,"/CBDA_SL_M",M,"_miss",misValperc,"_n",range_n,"_k"
                       ,range_k,"_Light_",j_global,"_",label,".RData\")")))
#eval(parse(text=paste0("save(arguments,label,workspace_directory,i_exp,file= \"~/temp_data_info_",label,".RData\")")))
eval(parse(text=paste0("save(arguments,label,workspace_directory,i_exp,file= \"~/temp_data_info.RData\")")))

#CV Superlearner function application [NOT TESTED YET]
# CV_SL <- try(CV.SuperLearner(Y,
#                              X,
#                              V=10, family=gaussian(),
#                              SL.library=SL.library,
#                              method="method.NNLS",
#                              verbose = TRUE,
#                              control = list(saveFitLibrary = TRUE),
#                              cvControl = list(V=10), saveAll = TRUE));#,
#                              #parallel = 'multicore'));
# 
# eval(parse(text=paste0("CV_SL_",j_global,"_KO <- CV_SL")));
# 
# eval(parse(text=paste0("ifelse(exists(\"CV_SL_",j_global,"_KO\"),'OK',
#                        CV_SL_",j_global,"_KO <- 1)")))
# 
# eval(parse(text=paste0("save(Xnew,Ynew,CV_SL_",j_global,"_KO,k",j_global,",n",j_global,",file= \"",
#                        workspace_directory,"CBDA_CV_SL_M",M,"_miss",misValperc,"_n",range_n,"_k",range_k,"_",j_global,"_KO.RData\")")))
