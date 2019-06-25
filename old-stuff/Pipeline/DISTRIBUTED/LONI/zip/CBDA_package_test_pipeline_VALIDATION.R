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

print(label)
print(workspace_directory)
print(j_global)
print(alpha)

#Set the list of packages/libraries to install/include (done through the ipak.R function)

pkg =c("CBDA","missForest" , "SuperLearner" , "knockoff","smotefamily" , "glmnet","bartMachine")
CBDA_initialization <- function(pkg =c("missForest" , "SuperLearner" , "knockoff" ,
                                       "smotefamily" , "glmnet","bartMachine")) {
  new.pkg <- pkg[!(pkg %in% utils::installed.packages()[, "Package"])]
  if (length(new.pkg))
    utils::install.packages(new.pkg, dependencies = TRUE, repos =  'https://cran.r-project.org/')

  a_temp <- sapply(pkg, require, character.only = TRUE)

  return(a_temp)
}

CBDA_initialization(pkg = pkg)

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
predict.SL.bartMachine <- function(object, newdata, family, X = NULL, Y = NULL,...) {
  .SL.require("bartMachine")
  pred <- predict(object$object, newdata)
  return(pred)
}

# label = "SINGLE_CORE"
# workspace_directory = "/ifs/loni/ccb/collabs/2016/CBDA_SL_2016/2018"
cat("BEFORE")
print(label)

filename_specs <- file.path(workspace_directory,paste0(label,"_validation_info.RData"))
load(filename_specs)

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


test_example <- c("SL.glm","SL.glmnet","SL.glmnet.0.25","SL.glmnet.0.50","SL.glmnet.0.75",
                  "SL.svm","SL.randomForest","SL.bartMachine","SL.glmnet.0",
                  "SL.svm.radial.10", "SL.svm.radial.0.1", "SL.svm.radial.default",
                  "SL.svm.poly.2.0", "SL.svm.poly.3.0", "SL.svm.poly.3.10", "SL.svm.poly.3.n10",
                  "SL.svm.poly.6.0", "SL.svm.poly.6.10", "SL.svm.poly.6.n10", "SL.svm.linear",
                  "SL.svm.sigmoid",
                  "SL.bayesglm","SL.earth","SL.glm.interaction","SL.ipredbagg","SL.knn","SL.mean","SL.nnet","SL.nnls")

algorithm_list_not_working = c("SL.biglasso","SL.extraTrees","SL.kernelKnn","SL.ksvm",
                               "SL.lda","SL.lm","SL.qda","SL.speedglm","SL.xgboost","SL.speedlm")

# algorithm_list <- c("SL.glmnet","SL.glmnet.0.25","SL.glmnet.0.50","SL.bartMachine",
#                   "SL.glmnet.0.75","SL.svm","SL.randomForest", "SL.knn","SL.glmnet.0")
#test_example <- c("SL.glm","SL.randomForest","SL.glmnet","SL.svm")

CBDA_Validation.pipeline(job_id_val = j_global, Ytemp , Xtemp ,  max_covs = max_covs , min_covs = min_covs,
                         alpha = alpha, Kcol_min = Kcol_min , Kcol_max = Kcol_max,
                         Nrow_min = Nrow_min , Nrow_max = Nrow_max ,
                         misValperc = misValperc, M = M , N_cores = 1 ,top = top,
                         label = label , workspace_directory = workspace_directory)


