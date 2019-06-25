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

#library(CBDA)
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

#' XGBoost SuperLearner wrapper
SL.xgboost = function(Y, X, newX, family, obsWeights, id, ntrees = 1000,
                      max_depth = 4, shrinkage = 0.1, minobspernode = 10,
                      params = list(),
                      nthread = 1,
                      verbose = 0,
                      save_period = NULL,
                      ...) {
  .SL.require("xgboost")
  if(packageVersion("xgboost") < 0.6) stop("SL.xgboost requires xgboost version >= 0.6, try help(\'SL.xgboost\') for details")
  # X needs to be converted to a matrix first, then an xgb.DMatrix.
  if (!is.matrix(X)) {
    X = model.matrix(~ . - 1, X)
  }
  
  # Convert to an xgboost compatible data matrix, using the sample weights.
  xgmat = xgboost::xgb.DMatrix(data = X, label = Y, weight = obsWeights)
  
  # TODO: support early stopping, which requires a "watchlist". See ?xgb.train
  
  if (family$family == "gaussian") {
    model = xgboost::xgboost(data = xgmat, objective="reg:linear", nrounds = ntrees,
                             max_depth = max_depth, min_child_weight = minobspernode, eta = shrinkage,
                             verbose = verbose, nthread = nthread, params = params,
                             save_period = save_period)
  }
  if (family$family == "binomial") {
    model = xgboost::xgboost(data = xgmat, objective="binary:logistic", nrounds = ntrees,
                             max_depth = max_depth, min_child_weight = minobspernode, eta = shrinkage,
                             verbose = verbose, nthread = nthread, params = params,
                             save_period = save_period)
  }
  if (family$family == "multinomial") {
    # TODO: test this.
    model = xgboost::xgboost(data = xgmat, objective="multi:softmax", nrounds = ntrees,
                             max_depth = max_depth, min_child_weight = minobspernode, eta = shrinkage,
                             verbose = verbose, num_class = length(unique(Y)), nthread = nthread,
                             params = params,
                             save_period = save_period)
  }
  
  # Newdata needs to be converted to a matrix first, then an xgb.DMatrix.
  if (!is.matrix(newX)) {
    newX = model.matrix(~ . - 1, newX)
  }
  
  pred = predict(model, newdata = newX)
  
  fit = list(object = model)
  class(fit) = c("SL.xgboost")
  out = list(pred = pred, fit = fit)
  return(out)
}

predict.SL.xgboost <- function(object, newdata, family, ...) {
  .SL.require("xgboost")
  if(packageVersion("xgboost") < 0.6) stop("SL.xgboost requires xgboost version >= 0.6, try help(\'SL.xgboost\') for details")
  # newdata needs to be converted to a matrix first
  if (!is.matrix(newdata)) {
    newdata = model.matrix(~ . - 1, newdata)
  }
  pred = predict(object$object, newdata = newdata)
  return(pred)
}

create.SL.xgboost = function(tune = list(ntrees = c(1000), max_depth = c(4), shrinkage = c(0.1),
                                         minobspernode = c(10)), detailed_names = F, env = .GlobalEnv,
                             name_prefix = "SL.xgb") {
  # Create all combinations of hyperparameters, for grid-like search.
  tuneGrid = expand.grid(tune, stringsAsFactors=F)
  
  names = rep("", nrow(tuneGrid))
  
  for (i in seq(nrow(tuneGrid))) {
    g = tuneGrid[i,]
    if (detailed_names) {
      name = paste(name_prefix, g$ntrees, g$max_depth, g$shrinkage, g$minobspernode, sep=".")
    } else {
      name = paste(name_prefix, i, sep=".")
    }
    names[i] = name
    eval(parse(text = paste0(name, "= function(..., ntrees = ", g$ntrees, ", max_depth = ", g$max_depth, ", shrinkage=", g$shrinkage, ", minobspernode=", g$minobspernode, ") SL.xgboost(..., ntrees = ntrees, max_depth = max_depth, shrinkage=shrinkage, minobspernode=minobspernode)")), envir = env)
  }
  results = list(grid = tuneGrid, names = names)
  invisible(results)
}

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


test_example <- c("SL.glm","SL.bayesglm","SL.earth","SL.glm.interaction","SL.ipredbagg",
                  "SL.knn","SL.mean","SL.nnet","SL.nnls",
              "SL.randomForest","SL.bartMachine",
              "SL.svm","SL.glmnet",
              "SL.svm.radial.10", "SL.svm.radial.0.1", "SL.svm.radial.default",
              "SL.svm.poly.2.0", "SL.svm.poly.3.0", "SL.svm.poly.3.10", "SL.svm.poly.3.n10",
              "SL.svm.poly.6.0", "SL.svm.poly.6.10", "SL.svm.poly.6.n10", "SL.svm.linear","SL.svm.sigmoid",
              "SL.glmnet.0","SL.glmnet.0.25","SL.glmnet.0.50","SL.glmnet.0.75","SL.xgboost")

algorithm_list_not_working = c("SL.biglasso","SL.extraTrees","SL.kernelKnn","SL.ksvm",
                               "SL.lda","SL.lm","SL.qda","SL.speedglm","SL.xgboost","SL.speedlm")
# test_example <- c("SL.glm","SL.glmnet",
#                   "SL.svm","SL.randomForest","SL.bartMachine")

## THIS IS THE DEFINITION OF THE CBDA.pipeline() function used below
CBDA.pipeline(job_id = j_global, Ytemp , Xtemp ,
              M = 9000 , Nrow_min = 40, Nrow_max = 60,
              top = 1000, max_covs = 50 , min_covs = 3,
              label = label , algorithm_list = test_example ,
              workspace_directory = workspace_directory)
