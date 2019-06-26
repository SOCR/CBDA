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

version
print ("Attaching CBDA, xgboost,Superlearner, FNN and SMOTE package....")
library(CBDA)
library(FNN)
library(smotefamily)
library(SuperLearner)
library(bartMachine)
library(bartMachineJARs)
library(xgboost)
print ("Status OK ... Exiting!") 

# pkg =c("CBDA","missForest" , "SuperLearner" , "knockoff","smotefamily" , "glmnet","bartMachine")
# CBDA_initialization <- function(pkg =c("missForest" , "SuperLearner" , "knockoff" ,
#                                        "smotefamily" , "glmnet","bartMachine")) {
#   new.pkg <- pkg[!(pkg %in% utils::installed.packages()[, "Package"])]
#   if (length(new.pkg))
#     utils::install.packages(new.pkg, dependencies = TRUE, repos =  'https://cran.r-project.org/')
# 
#   a_temp <- sapply(pkg, require, character.only = TRUE)
# 
#   return(a_temp)
# }
# 
# CBDA_initialization(pkg = pkg)

# Reads the dataset to be processed as passed from the input argument
Data = read.csv(dataset_file, header = TRUE)
arguments = read.table(arg_file, header = TRUE)  # NO NEED FOR THIS

Ytemp <- Data[,1] # col 9
original_names_Data <- names(Data)
cols_to_eliminate=1
Xtemp <- Data[-cols_to_eliminate]
original_names_Xtemp <- names(Xtemp)

# SET THE SAME NAMES/LABELS FOR THE X dataset
names(Xtemp) <- 1:dim(Xtemp)[2]


# SL.bartMachine <- function(Y, X, newX, family, obsWeights, id,
#                            num_trees = 50, num_burn_in = 250, verbose = F,
#                            alpha = 0.95, beta = 2, k = 2, q = 0.9, nu = 3,
#                            num_iterations_after_burn_in = 1000,
#                            ...) {
#   #.SL.require("bartMachine")
#   model = bartMachine::bartMachine(X, Y, num_trees = num_trees,
#                                    num_burn_in = num_burn_in, verbose = verbose,
#                                    alpha = alpha, beta = beta, k = k, q = q, nu = nu,
#                                    num_iterations_after_burn_in = num_iterations_after_burn_in)
#   # pred returns predicted responses (on the scale of the outcome)
#   pred <- predict(model, newX)
#   # fit returns all objects needed for predict.SL.template
#   fit <- list(object = model)
#   #fit <- vector("list", length=0)
#   class(fit) <- c("SL.bartMachine")
#   out <- list(pred = pred, fit = fit)
#   return(out)
# }
# predict.SL.bartMachine <- function(object, newdata, family, X = NULL, Y = NULL,...) {
#   .SL.require("bartMachine")
#   pred <- predict(object$object, newdata)
#   return(pred)
# }
# 

# Specify new SL prediction algorithm wrappers
SL.glmnet.0 <- function(..., alpha = 0,family="binomial"){
  SL.glmnet(..., alpha = alpha , family = family)
} # ridge penalty - DEFAULT

SL.glmnet.0.75 <- function(..., alpha = 0.75,family="binomial"){
  SL.glmnet(..., alpha = alpha , family = family)
} # ridge penalty

SL.glmnet.0.25 <- function(..., alpha = 0.25,family="binomial"){
  SL.glmnet(..., alpha = alpha, family = family)
}
SL.glmnet.0.50 <- function(..., alpha = 0.50,family="binomial"){
  SL.glmnet(..., alpha = alpha, family = family)
}

SL.svm.radial.10 <- function(..., family="binomial", kernel = "radial", gamma = 10){
  SL.svm(..., family = family, kernel = kernel, gamma = gamma)
}

SL.svm.radial.0.1 <- function(...,family="binomial", kernel = "radial", gamma = 0.1){
  SL.svm(...,family = family, kernel = kernel, gamma = gamma)
}

SL.svm.radial.default <- function(...,family="binomial", kernel = "radial"){
  SL.svm(...,family = family, kernel = kernel)
}
# default: gamma = if (is.vector(x)) 1 else 1 / ncol(x)

SL.svm.poly.2.0 <- function(...,family="binomial", kernel = "polynomial", gamma = 1, coef0 = 0, degree = 2){
  SL.svm(...,family = family, kernel = kernel, gamma = gamma, coef0 = coef0, degree = degree )
}

SL.svm.poly.3.0 <- function(...,family="binomial", kernel = "polynomial", gamma = 1, coef0 = 0, degree = 3){
  SL.svm(...,family = family, kernel = kernel, gamma = gamma, coef0 = coef0, degree = degree )
}

SL.svm.poly.3.10 <- function(...,family="binomial", kernel = "polynomial", gamma = 1, coef0 = 10, degree = 3){
  SL.svm(...,family = family, kernel = kernel, gamma = gamma, coef0 = coef0, degree = degree )
}

SL.svm.poly.3.n10 <- function(...,family="binomial", kernel = "polynomial", gamma = 1, coef0 = -10, degree = 3){
  SL.svm(...,family = family, kernel = kernel, gamma = gamma, coef0 = coef0, degree = degree )
}

SL.svm.poly.6.0 <- function(...,family="binomial", kernel = "polynomial", gamma = 1, coef0 = 0, degree = 6){
  SL.svm(...,family = family, kernel = kernel, gamma = gamma, coef0 = coef0, degree = degree )
}

SL.svm.poly.6.10 <- function(...,family="binomial", kernel = "polynomial", gamma = 1, coef0 = 10, degree = 6){
  SL.svm(...,family = family, kernel = kernel, gamma = gamma, coef0 = coef0, degree = degree )
}

SL.svm.poly.6.n10 <- function(...,family="binomial", kernel = "polynomial", gamma = 1, coef0 = -10, degree = 6){
  SL.svm(...,family = family, kernel = kernel, gamma = gamma, coef0 = coef0, degree = degree )
}

SL.svm.linear <- function(...,family="binomial", kernel = "linear"){
  SL.svm(...,family = family, kernel = kernel)
}

SL.svm.sigmoid <- function(...,family="binomial", kernel = "sigmoid", gamma = 1, coef0 = 0){
  SL.svm(...,family = family, kernel = kernel, gamma = gamma, coef0 = coef0)
}

SL.randomForest.1000 <- function(..., ntree=1000,family = "binomial"){
  SL.randomForest(..., ntree = ntree,family = family)
}

SL.randomForest.500 <- function(..., ntree=500,family = "binomial"){
  SL.randomForest(..., ntree = ntree,family = family)
}

SL.randomForest.300 <- function(..., ntree=300,family = "binomial"){
  SL.randomForest(..., ntree = ntree,family = family)
}

SL.randomForest.100 <- function(..., ntree=100,family = "binomial"){
  SL.randomForest(..., ntree = ntree,family = family)
}

SL.randomForest.50 <- function(..., ntree=50,family = "binomial"){
  SL.randomForest(..., ntree = ntree,family = family)
}

SL.randomForest.20 <- function(..., ntree=20,family = "binomial"){
  SL.randomForest(..., ntree = ntree,family = family)
}

SL.xgboost.500 <- function(..., ntrees=500,family = "binomial"){
  SL.xgboost(..., ntrees=500,family = family)
}

SL.xgboost.300 <- function(..., ntrees=300,family = "binomial"){
  SL.xgboost(..., ntrees=300,family = family)
}

SL.xgboost.2000 <- function(..., ntrees=2000,family = "binomial"){
  SL.xgboost(..., ntrees=2000,family = family)
}

SL.xgboost.100 <- function(..., ntrees=100,family = "binomial"){
  SL.xgboost(..., ntrees=100,family = family)
}

SL.xgboost.1500 <- function(..., ntrees=1500,family = "binomial"){
  SL.xgboost(..., ntrees=1500,family = family)
}

SL.xgboost.d3 <- function(..., max_depth=3, family = "binomial"){
  SL.xgboost(..., max_depth=3, family = family)
}

SL.xgboost.d5 <- function(..., max_depth=5, family = "binomial"){
  SL.xgboost(..., max_depth=3, family = family)
}

SL.xgboost.d6 <- function(..., max_depth=6, family = "binomial"){
  SL.xgboost(..., max_depth=3, family = family)
}

SL.xgboost.gau <- function(..., family = "gaussian"){
  SL.xgboost(..., family=family)
}

SL.xgboost.shrink.15 <- function(..., shrinkage=0.15, family="binomial"){
  SL.xgboost(..., shrinkage=0.15, family=family)
}

SL.xgboost.shrink.2 <- function(..., shrinkage=0.2, family="binomial"){
  SL.xgboost(..., shrinkage=0.2, family=family)
}
SL.xgboost.shrink.05 <- function(..., shrinkage=0.05, family="binomial"){
  SL.xgboost(..., shrinkage=0.05, family=family)
}
SL.xgboost.shrink.25 <- function(..., shrinkage=0.25, family="binomial"){
  SL.xgboost(..., shrinkage=0.25, family=family)
}

#bartMachine()    
SL.bartMachine.20 <- function(..., family = "binomial", ntrees = 20){
  SL.bartMachine(..., family = family, ntrees = ntrees)
}
SL.bartMachine.100 <- function(..., family = "binomial", ntrees = 100){
  SL.bartMachine(..., family = family, ntrees = ntrees)
}
SL.bartMachine.500 <- function(..., family = "binomial", ntrees = 500){
  SL.bartMachine(..., family = family, ntrees = ntrees)
}

#knn
SL.knn.5 <- function(..., k = 5,family = "binomial"){
  SL.knn(...,  k = k,family = family)
}
SL.knn.25 <- function(..., k = 25,family = "binomial"){
  SL.knn(...,  k = k,family = family)
}
SL.knn.50 <- function(..., k = 50,family = "binomial"){
  SL.knn(...,  k = k,family = family)
}
SL.knn.100 <- function(..., k = 100,family = "binomial"){
  SL.knn(...,  k = k,family = family)
}
test_example <- c("SL.glm","SL.bayesglm","SL.earth","SL.glm.interaction","SL.ipredbagg",
                  "SL.mean","SL.nnet","SL.nnls",
              "SL.glmnet","SL.glmnet.0","SL.glmnet.0.25","SL.glmnet.0.50","SL.glmnet.0.75",
              "SL.knn","SL.knn.100","SL.knn.50","SL.knn.25","SL.knn.5",
              "SL.svm","SL.svm.radial.10", "SL.svm.radial.0.1", "SL.svm.radial.default",
              "SL.svm.poly.2.0", "SL.svm.poly.3.0", "SL.svm.poly.3.10", "SL.svm.poly.3.n10",
              "SL.svm.poly.6.0", "SL.svm.poly.6.10", "SL.svm.poly.6.n10", "SL.svm.linear","SL.svm.sigmoid",
              "SL.randomForest","SL.randomForest.1000","SL.randomForest.500","SL.randomForest.300",
              "SL.randomForest.100","SL.randomForest.50","SL.randomForest.20",
              "SL.xgboost.500","SL.xgboost.300","SL.xgboost.2000",
              "SL.xgboost.1500","SL.xgboost.d3","SL.xgboost.d5", "SL.xgboost.d6",
              "SL.xgboost.gau","SL.xgboost.shrink.15","SL.xgboost.shrink.2", 
              "SL.xgboost.shrink.05","SL.xgboost.shrink.25",
              "SL.bartMachine","SL.bartMachine.500","SL.bartMachine.100","SL.bartMachine.20")

#test_example <- c("SL.xgboost","SL.bartMachine","SL.glmnet")
algorithm_list_not_working = c("SL.biglasso","SL.extraTrees","SL.kernelKnn","SL.ksvm",
                               "SL.lda","SL.lm","SL.qda","SL.speedglm","SL.speedlm")

## THIS IS THE DEFINITION OF THE CBDA.pipeline() function used below
CBDA.pipeline(job_id = j_global, Ytemp , Xtemp ,
              M = 300 , Nrow_min = 50, Nrow_max = 60, Kcol_min = 10, Kcol_max = 15 ,
              top = 100, max_covs = 50 , min_covs = 3,
              label = label , algorithm_list = test_example ,
              workspace_directory = workspace_directory)
# CBDA.pipeline(job_id = j_global, Ytemp , Xtemp ,
#               M = 9000 , Nrow_min = 1, Nrow_max = 10, Kcol_min = 1, Kcol_max = 10 ,
#               top = 1000, max_covs = 50 , min_covs = 3,
#               label = label , algorithm_list = test_example ,
#               workspace_directory = workspace_directory)
