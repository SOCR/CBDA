#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
print(args) 
arg_file = as.array(args[1])
print(arg_file)
j_global=as.numeric(args[2])
alpha=as.numeric(args[3])
dataset_file = as.array(args[4])
workspace_directory=as.array(args[5])
label=as.array(args[6])
i_exp=as.numeric(args[7])

print(args)
print(label)
print(arg_file)
print(workspace_directory)
print(j_global)
print(alpha)
print(dataset_file)
print(label)

#Set the list of packages/libraries to install/include (done through the ipak.R function)
packages <- c("ggplot2", "plyr", "colorspace","grid","data.table","VIM","MASS","Matrix",
              "lme4","arm","foreach","glmnet","class","nnet","mice","missForest",
              "calibrate","nnls","SuperLearner","plotrix","TeachingDemos","plotmo",
              "earth","parallel","splines","gam","mi",
              "BayesTree","e1071","randomForest", "Hmisc","dplyr","Amelia","bartMachine",
              "knockoff","caret","smotefamily","FNN")

## ipak function below: install (if missing) and load (if installed) multiple R packages
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE, repos='http://cran.rstudio.com/')
  sapply(pkg, require, character.only = TRUE)
}

#install.packages('package_name', dependencies=TRUE, repos='http://cran.rstudio.com/')
ipak(packages)

# Reads the dataset to be processed as passed from the input argument
Data = read.csv(dataset_file, header = TRUE)
arguments = read.table(arg_file, header = TRUE)

Ytemp <- Data[,1] # col 9
original_names_Data <- names(Data)
cols_to_eliminate=1
Xtemp <- Data[-cols_to_eliminate]
original_names_Xtemp <- names(Xtemp)

# SET THE SAME NAMES/LABELS FOR THE X dataset
names(Xtemp) <- 1:dim(Xtemp)[2]

## SAMPLE THE PREDICTION DATASET -- THIS STEP IS DATA INDEPENDENT
## This ensures reproducibility of the analysis
set.seed(12345)
## The fraction alpha of data/patients to use for prediction could be passed as an input argument as well
## Below the sampling is balanced
## Eliminating q subjects for prediction, in a BALANCED WAY
## Subjects to sample in a balanced way from 0-1 outcomes
a0 = round(alpha*dim(Xtemp)[1]/2)
# Randomly select patients for prediction
q1 = sample(which(Ytemp==1),a0)
q2 = sample(which(Ytemp==0),a0)
q <- c(q1 , q2)

Xpred <- Xtemp[q,] # define the feature set for prediction (renamed Xpred) [not used in the training/learning]
Ypred <- Ytemp[q] # define the output for prediction (renamed Ypred) [not used in the training/learning]

# Re-set the seed to run a different CBDA selection on the Data
set.seed(round(as.numeric(Sys.time()) %% 1e3 * j_global * (1+log(j_global))))

# Set the specs for the unique CBDA-SL & KO job
M <-arguments[i_exp,1]
Kcol_min <- arguments[i_exp,3]
Kcol_max <- arguments[i_exp,4]
Nrow_min <- arguments[i_exp,5]
Nrow_max <- arguments[i_exp,6]
range_n <- eval(parse(text=paste0("c(\"",Nrow_min,"_",Nrow_max,"\")")))
range_k <- eval(parse(text=paste0("c(\"",Kcol_min,"_",Kcol_max,"\")")))

# This step defines and samples the subsets of features for training based on the Kcol sample
Kcol <- round(dim(Xtemp)[2]*(runif(1,Kcol_min/100,Kcol_max/100))) # sample a value from a uniform distribution within Kcol_min and Kcol_max [number of features/columns of the big dataset]
k <- sample(1:dim(Xtemp)[2],Kcol)

# This step defines and samples the correct subsets of subjects for training
Nrow <- round(dim(Xtemp[-q,])[1]*(runif(1,Nrow_min/100,Nrow_max/100))) # sample a value from a uniform distribution Nrox_min and Nrow_max [number of rows/subjects of the big dataset]
n_all <- 1:length(Ytemp)
n_sub <- n_all[-q]
a0 = round(Nrow/2) # balanced # of subjects
# Randomly select patients for prediction in a balanced way based on the Nrow sample and a0
n1 = sample(which(Ytemp[n_sub]==1),a0)
n2 = sample(which(Ytemp[n_sub]==0),a0)
n <- c(n1 , n2)


# Pass the missing value %
# For real datasets, there's no need to pass the missValperc, because 
# we will impute whatever the missing values are
misValperc <- arguments[i_exp,2]
print(dim(Xtemp))
print(dim(Ytemp))
#Xtemp_mis <- prodNA(Xtemp, noNA = misValperc/100)
# Here we pass ONLY the subset of rows/columns [n,k] for imputation and scaling of Xtemp [training/learning]
# If M subsets have been already generated offline, then a RData workspace will be loaded
# with the correspondent imputed/normalized X and Xpred
Xtemp_mis <- Xtemp[n,k]
# Here we pass ONLY the subset of columns [,k] for imputation and scaling of Xpred [validation/preditcion]
Xpred_mis <- Xpred[,k]
# Here I impute the missing data with the function missForest
Xtemp_imp <- missForest(Xtemp_mis, maxiter = 5)
Xpred_imp <- missForest(Xpred_mis, maxiter = 5)

## DATA NORMALIZATION of the sampled matrix without Group and Sex
## This step can be generalized if the data is formatted RAW,
# with categorical variables as binary or strings (see commented out example below)
# a1 = which(names(Xtemp_imp$ximp) == "Group")
# a2 = which(names(Xtemp_imp$ximp) == "Sex")
# cont = 1:length(Xtemp_imp$ximp)
# cont <- cont[-1*c(a1,a2)]
# # DATA NORMALIZATION if IMPUTATION IS PERFORMED
#Xnorm_ALL <- as.data.frame(scale(Xtemp_imp$ximp))
Xtemp_norm <- as.data.frame(scale(Xtemp_imp$ximp))
Xpred_norm <- as.data.frame(scale(Xpred_imp$ximp))

# STEPS 5 and 6 ADD LIBRARIES
# Specify new SL prediction algorithm wrappers 
SL.glmnet.0 <- function(..., alpha = 0,family="binomial"){
  SL.glmnet(..., alpha = alpha , family = family)
} # ridge penalty
SL.glmnet.1 <- function(..., alpha = 1,family="binomial"){
  SL.glmnet(..., alpha = alpha , family = family)
} # ridge penalty

SL.glmnet.0.25 <- function(..., alpha = 0.25,family="binomial"){
  SL.glmnet(..., alpha = alpha, family = family)
}

SL.glmnet.0.50 <- function(..., alpha = 0.50,family="binomial"){
  SL.glmnet(..., alpha = alpha, family = family)
}

SL.glmnet.0.75 <- function(..., alpha = 0.75,family="binomial"){
  SL.glmnet(..., alpha = alpha, family = family)
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

create.SL.glmnet.alpha<-function(...,alpha=c(0,0.25,0.5,0.75,1))
{
  SL.glmnet(..., alpha=alpha)
}

#change the kernel to linear, polynomial, sigmoid
SL.svm.linear <-function(...,kernel = "linear"){
    SL.svm(...,kernel = "linear")
}

SL.svm.sigmoid <-function(...,kernel = "sigmoid"){
    SL.svm(...,kernel = "sigmoid")
}

SL.svm.polynomial <-function(...,kernel="polynomial", cost=1, degree = 2){
    SL.svm(...,kernel="polynomial", cost=1, degree = 2)
}
#change the cost of radial to 2 and 3
SL.svm.radial.2 <-function(..., cost=2){
    SL.svm(..., cost=2)
}

SL.svm.radial.3 <-function(..., cost=3){
    SL.svm(..., cost=3)
}


# change the ntree numbers to 2000, 3000, 4000, 5000
SL.randomforest.2000<-function(...,ntree = 2000){
    SL.randomForest(...,ntree=2000)
}

SL.randomforest.3000<-function(...,ntree = 3000){
    SL.randomForest(...,ntree=3000)
}

SL.randomforest.4000<-function(...,ntree = 4000){
    SL.randomForest(...,ntree=4000)
}

SL.randomforest.5000<-function(...,ntree = 5000){
    SL.randomForest(...,ntree=5000)
}
# tunning the mtry numbers by multiplying 0.5, 0.75, 2
SL.randomforest.0.5<-function(...,family,mtry = ifelse(family$family == "gaussian", max(floor(0.5*ncol(X)/3), 1), floor(0.5*sqrt(ncol(X))))){
    SL.randomForest(...,family,mtry = ifelse(family$family == "gaussian", max(floor(0.5*ncol(X)/3), 1), floor(0.5*sqrt(ncol(X)))))
}

SL.randomforest.0.75<-function(...,family,mtry = ifelse(family$family == "gaussian", max(floor(0.75*ncol(X)/3), 1), floor(0.75*sqrt(ncol(X))))){
    SL.randomForest(...,family,mtry = ifelse(family$family == "gaussian", max(floor(0.75*ncol(X)/3), 1), floor(0.75*sqrt(ncol(X)))))
}

SL.randomforest.2<-function(...,family,mtry = ifelse(family$family == "gaussian", max(floor(2*ncol(X)/3), 1), floor(0.75*sqrt(ncol(X))))){
    SL.randomForest(...,family,mtry = ifelse(family$family == "gaussian", max(floor(2*ncol(X)/3), 1), floor(2*sqrt(ncol(X)))))
}

SL.xgboost.2000 <- function(...,ntree = 2000){
    SL.xgboost(...,ntree=2000)
}

SL.xgboost.3000 <- function(...,ntree = 3000){
    SL.xgboost(...,ntree=3000)
}

SL.xgboost.4000 <- function(...,ntree = 3000){
    SL.xgboost(...,ntree=3000)
}

SL.xgboost.15 <- function(...,minobspernode = 15){
    SL.xgboost(...,minobspernode = 15)
}

SL.xgboost.20 <- function(...,minobspernode = 20){
    SL.xgboost(...,minobspernode = 20)
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
#                 "SL.glmnet","SL.glmnet.0","SL.glmnet.0.25","SL.glmnet.0.50","SL.glmnet.0.75","SL.glmnet.1",
#                 "SL.svm",
#                 "SL.randomForest","SL.bartMachine")
SL.library <- c("SL.glm",
                "SL.glmnet","SL.glmnet.0","SL.glmnet.0.25","SL.glmnet.0.50","SL.glmnet.0.75","SL.glmnet.1",
                "SL.svm","SL.randomForest","SL.bartMachine","SL.randomforest.2000","SL.randomforest.3000",
                "SL.randomforest.4000","SL.randomforest.5000","SL.randomforest.0.5","SL.randomforest.0.75","SL.randomforest.2",
                "SL.svm.linear", "SL.svm.sigmoid","SL.svm.polynomial","SL.svm.radial.2","SL.svm.radial.3","SL.xgboost",
                "SL.xgboost.2000","SL.xgboost.3000","SL.xgboost.4000","SL.xgboost.15","SL.xgboost.20")

# Automated labeling of sub-matrices, assigned to X
X <- as.data.frame(Xtemp_norm)
Y <- Ytemp[n]
eval(parse(text=paste0("k",j_global," <- k")))
eval(parse(text=paste0("n",j_global," <- n")))

## KNOCKOFF FILTER IMPLEMENTATION  
## IMPORTANT  --> subjects # >> features # !!!
## It creates KO_result_j objects with all the stats, results, FDR proportion,...
# knockoff.filter(X, Y, fdr = 0.2, statistic = NULL,
# threshold = c("knockoff", "knockoff+"), knockoffs = c("equicorrelated","sdp"),
#               normalize = TRUE, randomize = FALSE)
if (dim(X)[2]<dim(X)[1])
{
  eval(parse(text=paste0("KO_result_",j_global," = knockoff.filter(X,Y,fdr = 0.05)")))
  eval(parse(text=paste0("KO_selected_",j_global," <- as.numeric(sub(\"V\",\"\",names(KO_result_",j_global,"$selected)))")))
  eval(parse(text=paste0("print(KO_selected_",j_global,")")))
  
} else {
  eval(parse(text=paste0("print(KO_selected_",j_global,"<-NULL)")))
  
}

## SUPERLEARNER LOOP
# SUPERLEARNER-SL FUNCTION CALL that generates SL objects
## Superlearner Function ##
SL <- try(SuperLearner(Y , X , newX = Xpred_norm,
                       family=binomial(),
                       SL.library=SL.library,
                       method="method.NNLS",
                       verbose = FALSE,
                       control = list(saveFitLibrary = TRUE),
                       cvControl = list(V=10)));

SL_Pred <- data.frame(prediction = SL$SL.predict[, 1])
Classify <-  NULL;
Classify_MSE <- NULL
for (i in 1:dim(SL_Pred)[1])
{
  ifelse((SL_Pred[i,] > 0.5),Classify[i] <- 1,Classify[i] <- 0)
}
                      
Classify_MSE <- apply(SL_Pred, 1, function(xx) xx[unname(which.max(xx))])

eval(parse(text=paste0("SL_Pred_",j_global," <- Classify")))
eval(parse(text=paste0("SL_Pred_MSE_",j_global," <- Classify_MSE")))



# This checks if the SL_Pred object was successfully generated (i.e., if it exists)
# If it does not exist, it is set to a double equal to 100
eval(parse(text=paste0("ifelse(exists(\"SL_Pred_",j_global,"\"),'OK',
                   SL_Pred_",j_global," <- 100)")))

truth=Ypred; # set the true outcome to be predicted
pred=Classify
# pred_temp <- pred;pred_temp[which(pred_temp == "MCI/LMCI")] <- c("MCI")
# truth_temp <- truth;truth_temp[which(truth_temp == "LMCI")] <- c("MCI")

cmatrix <- confusionMatrix(pred,truth)
# This sets the "MSE_jglobal" to the ACCURACY of the prediction (i.e., cmatrix$overall[1])
# The final ranking will then be done on the accuracy rather than 
# on a distance-based metrics. A real MSE calculation might be OK for continuous outcomes
eval(parse(text=paste0("Accuracy_",j_global,"<- unname(cmatrix$overall[1])")))

## MSE GENERATION
sum=0
for(s in 1:length(Ypred)) {
  ## This checks first if the TYPE of the prediction object SL_Pred is NOT a double (which means that
  ## the prediction step worked in the previous module. If it is NOT a double, the MSE is calculated
  ## as the mean of the sum of the square of the difference between the prediction and the data to predict.
  # If the SL_Pred is a double, SL_Pred_j is assigned to its MSE (very high value).
  # eval(parse(text=paste0("ifelse(typeof(SL_Pred_MSE_",j_global,") != \"double\",
  #                        sum <- sum+sum(Ypred[",s,"] - SL_Pred_MSE_",j_global,"$pred[",s,"])^2,sum <- SL_Pred_MSE_",j_global,")")))
  
  #ifelse(typeof(Classify_MSE) != "double",sum <- sum+sum(Ypred[s] - Classify_MSE[s])^2,sum <- Classify_MSE)
  #ifelse(exists("Classify_MSE") ,sum <- sum+sum(Ypred[s] - Classify_MSE[s])^2,sum <- Classify_MSE)
  ifelse(exists("Classify_MSE") ,sum <- sum+sum(Ypred[s] - Classify_MSE[s])^2,sum <- 100)
  }
# eval(parse(text=paste0("ifelse(exists(\"SL_Pred_",j_global,"\"),'OK',
#                  SL_Pred_",j_global," <- 100)")))
 eval(parse(text=paste0("ifelse(exists(\"SL_Pred_MSE_",j_global,"\"),sum <- sum+sum(Ypred[s] - SL_Pred_MSE_",j_global,"[s])^2,
                          sum <- 10000)")))

  ## This step makes the final calculation of the MSE and it labels it with a j --> MSE_j
  eval(parse(text=paste0("MSE_",j_global," <- sum/length(Ypred)")))
  ## This resets the sum to 0 for the next MSE calculation

# GENERATE THE LIGHT DATASET BY DELETING THE SL OBJECT
rm(SL)
#nonzero=c(10,20,30,40,50,60,70,80,90,100)
 nonzero=c(1,30,60,100,130,160,200,230,260,300)
# nonzero=c(1,100,200,300,400,500,600,700,800,900)
# nonzero=c(1,100,200,400,600,800,1000,1200,1400,1500)

# SAVE THE RDATA WORKSPACE WITH THE ALL DATA
eval(parse(text=paste0("save(Xtemp,Xpred, q, Ypred, M, i_exp, Ytemp, SL_Pred_MSE_",j_global,",SL_Pred_",j_global,
                       ",nonzero,k",j_global,",MSE_",j_global,",Accuracy_",j_global,",KO_selected_",j_global,",
                       file= \"",workspace_directory,"/CBDA_SL_M",M,"_miss",misValperc,"_n",range_n,"_k"
                       ,range_k,"_Light_",j_global,"_",label,".RData\")")))
eval(parse(text=paste0("save(arguments,label,workspace_directory,i_exp,arg_file,
                       file= \"~/",label,"_info_for_consolidation.RData\")")))

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
