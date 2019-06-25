#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
print(args)
arg_file = as.array(args[1])
print(arg_file)
j_global=as.numeric(args[2])
dataset_file = as.array(args[3])
workspace_directory=as.array(args[4])
i_exp=as.numeric(args[5])

label=c("Gaussian")
# /ifs/loni/ccb/temp/CBDA_SL
#print(c(j_global,i_exp))

#Set the list of packages/libraries to install/include (done through the ipak.R function)
#library(rJava,lib.loc = "/ifshome/shobel/R-3.3.1/library")
packages <- c("ggplot2", "plyr", "colorspace","grid","data.table","VIM","MASS","Matrix",
              "lme4","arm","foreach","glmnet","class","nnet","mice","missForest",
              "calibrate","nnls","SuperLearner","plotrix","TeachingDemos","plotmo",
              "earth","parallel","splines","gam","mi",
              "BayesTree","e1071","randomForest", "Hmisc","dplyr","Amelia","bartMachine","knockoff")#,"rJava")

## ipak function below: install (if missing) and load (if installed) multiple R packages
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE, repos='http://cran.rstudio.com/')
  sapply(pkg, require, character.only = TRUE)
}

#install.packages('package_name', dependencies=TRUE, repos='http://cran.rstudio.com/')
ipak(packages)

eval(parse(text=paste0("Data = read.csv(dataset_file, header = TRUE)")))

Ytemp <- Data[,1]
Xtemp <- Data[,-1]

names(Xtemp) <- 1:dim(Xtemp)[2]
p = 100;   # number of variables
nonzero=c(1,seq(10,p,10)); # variables with nonzero coefficients
#knockoff.filter(Xtemp,Ytemp)

# The false discovery proportion is
# fdp <- function(selected) sum(beta[selected] == 0) / max(1, length(selected))
# fdp(result$selected)

## IMPUTATION AND NORMALIZATION STEP (OFFLINE ON THE WHOLE DATASET)
## DATA IMPUTATION
# Xtemp is the dataset to be imputed before the SL-LOOP
# Here I first replace % (i.e., misValperc) of the data with missing data (i.e., NA)
eval(parse(text=paste0("arguments = read.table(arg_file, header = TRUE)")))
misValperc <- arguments[i_exp,2]
Xtemp_mis <- prodNA(Xtemp, noNA = misValperc/100)
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


## DATA NORMALIZATION of the sampled matrix without Group and Sex
## This step can be generalized if the data is formatted RAW, with categorical variables as binary or strings
# a1 = which(names(Xtemp_imp$ximp) == "Group")
# a2 = which(names(Xtemp_imp$ximp) == "Sex")
# cont = 1:length(Xtemp_imp$ximp)
# cont <- cont[-1*c(a1,a2)]
# # DATA NORMALIZATION if IMPUTATION IS PERFORMED
# Xtemp_imp$ximp[,cont] <- scale(Xtemp_imp$ximp[,cont])
Xnorm_ALL <- as.data.frame(scale(Xtemp_imp$ximp))

## SAMPLE THE PREDICTION DATASET -- THIS STEP IS DATA INDEPENDENT
## The fraction alpha of data/patients to use for prediction (SET TO 15% BELOW)can be passed as an input argument as well
## Below the sampling is balanced
## Eliminating q subjects for prediction
alpha = 0.30; # % of the initial subjects to set aside for prediction
q <- round(length(Ytemp)*alpha); 
q <- sample(length(Ytemp),q) # sampling the q patients from the dataset
Xpred <- Xnorm_ALL[q,]
Xnorm_sub <- Xnorm_ALL[-1*q,]  # eliminate the q patients from the "training/learning" matrix of features (renamed as Xnorm) [not used in the training]
Ypred <- Ytemp[q] # define the output for prediction (renamed Ypred) [not used in the training/learning]
Ytemp_sub <- Ytemp[-1*q] # define the output for learning by eliminating the q subjects that are not used in the training/learning

# SET THE SAME NAMES/LABELS FOR THE NEW MATRIX Xnew
# names(Xnew) <- names(Xtemp_imp$ximp)
# names(Xnorm) <- names(Xtemp_imp$ximp)

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
#source("SL.bartMachine")
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


## SUPERLEARNER LOOP

M <-arguments[i_exp,1]
print(misValperc)
Kcol_min <- arguments[i_exp,3]
Kcol_max <- arguments[i_exp,4]
Nrow_min <- arguments[i_exp,5]
Nrow_max <- arguments[i_exp,6]
range_n <- eval(parse(text=paste0("c(\"",Nrow_min,"_",Nrow_max,"\")")))
range_k <- eval(parse(text=paste0("c(\"",Kcol_min,"_",Kcol_max,"\")")))
# Kcol <- round(K*(runif(1,Kcol_min/100,Kcol_max/100))) # sample a value from a uniform distribution within 0.15 and 0.3 [number of columns/covariates between 15-30% of the big dataset]
# Nrow <- round(N*(runif(1,Nrow_min/100,Nrow_max/100))) # sample a value from a uniform distribution within 0.6 and 0.8 [number of rows/subjects between 60-80% of the big dataset]

Kcol <- round(dim(Xnorm_sub)[2]*(runif(1,Kcol_min/100,Kcol_max/100))) # sample a value from a uniform distribution within 0.6 and 0.8 [number of rows/subjects between 60-80% of the big dataset]
eval(parse(text=paste0("k <- sample(1:length(Xnorm_sub),Kcol)")))
Nrow <- round(dim(Xnorm_sub)[1]*(runif(1,Nrow_min/100,Nrow_max/100))) # sample a value from a uniform distribution within 0.6 and 0.8 [number of rows/subjects between 60-80% of the big dataset]
eval(parse(text=paste0("n <- sample(1:length(Ytemp_sub),Nrow)")))



print(c(j_global,i_exp))
      k <- sample(1:K,Kcol) # this is where I generate the sample of columns
      n <- sample(1:N,Nrow) # this is where I generate the sample of rows
      #n <- 1:N # this is where I generate the sample of rows
      # Automated labeling of sub-matrices, assigned to X
      eval(parse(text=paste0("X",j_global," <- Xnorm_sub[n,k]")))
      eval(parse(text=paste0("X <- as.data.frame(X",j_global,")")))
      eval(parse(text=paste0("Y",j_global," <- Ytemp_sub[n]")))
      eval(parse(text=paste0("Y <- Y",j_global)))
      eval(parse(text=paste0("k",j_global," <- k")))
      eval(parse(text=paste0("n",j_global," <- n")))
      # SUPERLEARNER-SL FUNCTION CALL that generates SL objects

      ## KNOCKOFF FILTER IMPLEMENTATION  
      ## IMPORTANT  --> subjects # >> features # !!!
      ## It creates KO_result_j objects with all the stats, results, FDR proportion,...
      # knockoff.filter(X, Y, fdr = 0.2, statistic = NULL,
      # threshold = c("knockoff", "knockoff+"), knockoffs = c("equicorrelated","sdp"),
      #               normalize = TRUE, randomize = FALSE)
      
      # eval(parse(text=paste0("KO_result_",j_global," = knockoff.filter(X, Y, normalize = FALSE)")))
      # eval(parse(text=paste0("print(KO_result_",j_global,")")))
      
      eval(parse(text=paste0("KO_result_",j_global," = knockoff.filter(Xnorm_sub[n",j_global,",k",j_global,"], Ytemp_sub[n",j_global,"],fdr = 0.05)")))

      eval(parse(text=paste0("KO_selected_",j_global," <- as.numeric(sub(\"V\",\"\",names(KO_result_",j_global,"$selected)))")))
      #eval(parse(text=paste0("KO_sub <- c(KO_sub,KO_selected_sub_",j_global,")")))
      
      
    ## Superlearner Function ##
    SL <- try(SuperLearner(Y,X,
                           family=binomial(),
                           SL.library=SL.library,
                           method="method.NNLS",
                           verbose = FALSE,
                           control = list(saveFitLibrary = TRUE),
                           cvControl = list(V=10)));
    eval(parse(text=paste0("SL_",j_global," <- SL")));
    eval(parse(text=paste0("SL_",j_global)))
    #eval(parse(text=paste0("typeof(SL_",j_global,")")));
    # Generate the prediction object from the SL object
    # STEP 7 - GENERATING PREDICTIONS ON THE PREDICTION DATASET
    # Generates SL_Pred objects using the predict function on the prediction 
    # dataset with the SL object as the predictive model.
    # SL_Pred returns both the SuperLearner predictions ("pred") and 
    # predictions for each algorithm in the library (SL.library above)
    eval(parse(text=paste0("try(SL_Pred_",j_global," <- predict(SL_",j_global,", Xpred[,k",j_global,"]))")))

    #eval(parse(text=paste0("ifelse(exists(\"SL_Pred_",j_global,"\"),'OK',
    #                         ifelse(!exists(\"SL_Pred_",j_global,"\"),SL_Pred_",j_global," <- NULL))")))
    eval(parse(text=paste0("ifelse(exists(\"SL_Pred_",j_global,"\"),'OK',
                       SL_Pred_",j_global," <- 100)")))
    #eval(parse(text=paste0("typeof(SL_Pred_",j_global,")")));
    # remove the large SL object
    #print(SL_Pred_1)
    #eval(parse(text=paste0("rm(SL_",j_global,")")))
    # print("ITERATION")
    # print(j_global)
    # print("% completed jobs")
    # print(100*j_global/M)
#  }
#})[[3]]

#Ynew = NeuroIm1_Final_AD_vs_NC_training$Group[q];
#Complete = "Complete"
#Light = "Light"
# SAVE THE COMPLETE DATASET WITH ALL THE SL OBJECTS
# eval(parse(text=paste0("con_Complete <- '~/CBDA_SL_M",M,"_miss",misValperc,"_n",range_n,"_k",range_k,"_Complete.RData'")))
# save.image(file=con_Complete) 
# close(con_Complete)

## Save the complete workspaces with SL objects
#eval(parse(text=paste0("save.image(\"",workspace_directory,"/CBDA_SL_M",M,"_miss",misValperc,"_n",range_n,"_k",range_k,"_Complete.RData\")")))

#eval(parse(text=paste0("save.image(\"CBDA_SL_M",M,"_miss",misValperc,"_n",range_n,"_k",range_k,".RData\")")))
#eval(parse(text=paste0("save.image(\"~/CBDA-SL/CBDA_SL_M",M,"_miss",misValperc,"_n",range_n,"_k",range_k,"_Complete.RData\")")))
#eval(parse(text=paste0("save.image(\"Desktop/R/CBDA_SL_M",M,"_miss",misValperc*100,"_n",range_n,"_k",range_k,".RData\")")))

# GENERATE THE LIGHT DATASET BY DELETING THE SL OBJECTS
#for (j in 1:M){
  eval(parse(text=paste0("rm(SL_",j_global,")")))
#}
## Save the complete workspaces without the SL objects
#w1<-Sys.Date()
## Unix set of commands for creating a directory and moving a subset of files into it
#eval(parse(text=paste0("dir.create(\"",workspace_directory,"\\",w1,"\")"))) # Unix
#eval(parse(text=paste0("save.image(\"~/CBDA-SL/CBDA_SL_M",M,"_miss",misValperc,"_n",range_n,"_k",range_k,"_Light.RData\")")))
#eval(parse(text=paste0("save.image(\"",workspace_directory,"/CBDA_SL_M",M,"_miss",misValperc,"_n",range_n,"_k",range_k,"_Light.RData\")")))
#eval(parse(text=paste0("save(Ynew,SL_Pred_",j_global,",k",j_global,",file= ",workspace_directory,"CBDA_SL_M",M,"_miss",misValperc,"_n",range_n,"_k",range_k,"_Light.RData\")")))

eval(parse(text=paste0("save(Xpred,label, Xnorm_ALL, Xnorm_sub, q, Ypred, M, Ytemp, Ytemp_sub, SL_Pred_",j_global,
                       ",nonzero,n",j_global,",k",j_global,",KO_selected_",j_global,",
                       file= \"",workspace_directory,"/CBDA_SL_M",M,"_miss",misValperc,"_n",range_n,"_k"
                       ,range_k,"_Light_",j_global,"_",label,"_Dec5.RData\")")))

#eval(parse(text=paste0("KO_result_",j_global," = knockoff.filter(X, Y, normalize = FALSE)")))

#eval(parse(text=paste0("save(arguments,workspace_directory,file= \"~/MSE_M",M,"_miss",misValperc,"_n",range_n,"_k",range_k,"_Light_",j_global,"_KO.RData\")")))
# eval(parse(text=paste0("save(arguments,label,workspace_directory,file= \"",workspace_directory,"/MSE_temp_",label,"_Dec5.RData\")")))
# eval(parse(text=paste0("save(arguments,label,workspace_directory,file= \"",workspace_directory,"/MSE_temp_",label,"_Dec5.RData\")")))

eval(parse(text=paste0("save(arguments,label,workspace_directory,file= \"~/temp_data_info.RData\")")))

# #CV Superlearner function application
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
