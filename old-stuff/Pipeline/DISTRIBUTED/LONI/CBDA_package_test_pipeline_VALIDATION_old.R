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

CBDA_initialization <- function(pkg =c("missForest" , "SuperLearner" , "knockoff" ,
                                       "smotefamily" , "glmnet","bartMachine")) {
  new.pkg <- pkg[!(pkg %in% utils::installed.packages()[, "Package"])]
  if (length(new.pkg))
    utils::install.packages(new.pkg, dependencies = TRUE, repos='http://cran.rstudio.com/')

  a_temp <- sapply(pkg, require, character.only = TRUE)

  return(a_temp)
}

CBDA_initialization()

## THIS IS THE DEFINITION OF THE CBDA_Validation.pipeline() function used below
CBDA_Validation.pipeline <- function(job_id_val, Ytemp , Xtemp , label = "CBDA_package_test" , alpha = 0.2,
                                     Kcol_min = 5 , Kcol_max = 15, Nrow_min = 30 , Nrow_max = 50 ,
                                     misValperc = 0, M = 3000 , N_cores = 1 ,top = 1000,
                                     workspace_directory = tempdir(), max_covs = 100 , min_covs = 5,
                                     algorithm_list = c("SL.glm","SL.glmnet","SL.svm","SL.randomForest")) {

  # Check if min_covs is > 2
  if (min_covs < 3){
    cat("The parameter min_covs must be at least 3 !!\n\n")
    cat("It's been set to 3 by default.\n\n")
    min_covs <- 3
  }

  range_n <- range_k <- qa_ALL <- algorithm_list <- NULL

  cat("VALIDATION STEP for TOP nested predictive models has started.\n\n")
  #  eval(parse(text=paste0("load(\"",workspace_directory,"/",label,"_info.RData\")")))
  filename_specs <- file.path(workspace_directory,paste0(label,"_validation_info.RData"))
  cat("Test1.\n\n")
  cat(filename_specs)
  cat("\n\n")
  load(filename_specs)

  Xpred <- Xtemp[q,] # define the feature set for prediction (renamed Xpred) [not used in the training/learning]
  Ypred <- Ytemp[q] # define the output for prediction (renamed Ypred) [not used in the training/learning]

  print(dim(Xpred))
  print(length(Ypred))

  # This step defines and samples the subsets of features for training based on the Kcol sample
  #max_k <- max_covs#min(max_covs,dim(qa_ALL)[1])
  k_Acc <- as.numeric(paste(qa_ALL$Accuracy[1:(max_covs)]))
  k_MSE <- as.numeric(paste(qa_ALL$MSE[1:(max_covs)]))

  # Here we chose the ranking determined by the MSE performance as the best
  k_ALL <- k_MSE


  cmatrix_ALL_validation <- NULL

  counter = 1

  cat("Subsampling size = ", M,"\n\n")
  cat("Case Sampling Range - CSR (%) = ", range_n,"%\n\n")

  print(job_id_val)

  j_global <- job_id_val

  print(j_global)

  # Re-set the seed to run a different CBDA selection on the Data
  set.seed(round(as.numeric(Sys.time()) %% 1e3 * j_global * (1+log(j_global))))

  # This step defines and samples the subsets of features for training based on the Kcol sample
  k <- k_ALL[1:j_global]
  print(k)

  # This step defines and samples the correct subsets of subjects for training
  Nrow <- round(dim(Xtemp[-q,])[1]*(stats::runif(1,Nrow_min/100,Nrow_max/100))) # sample a value from a uniform distribution Nrox_min and Nrow_max [number of rows/subjects of the big dataset]
  n_all <- 1:length(Ytemp)
  n_sub <- n_all[-q]
  a0 = round(Nrow/2) # balanced # of subjects
  # Randomly select patients for prediction in a balanced way based on the Nrow sample and a0
  n1 = sample(which(Ytemp[n_sub]==1),a0)
  n2 = sample(which(Ytemp[n_sub]==0),a0)
  n <- c(n1 , n2)

  ## DATA IMPUTATION
  Xtemp_mis <- Xtemp[n,k]
  # Here we pass ONLY the subset of columns [,k] for imputation and scaling of Xpred [validation/preditcion]
  Xpred_mis <- Xpred[,k]

  # Here I impute the missing data with the function missForest
  if (isTRUE(length(which(is.na(Xtemp_mis) == "TRUE")) == 0)){
    ## DATA NORMALIZATION (NO IMPUTATION)
    Xtemp_norm <- as.data.frame(scale(Xtemp_mis))
    Xpred_norm <- as.data.frame(scale(Xpred_mis))
  } else {
    ## DATA IMPUTATION
    Xtemp_imp <-missForest::missForest(Xtemp_mis, maxiter = 5)
    Xpred_imp <-missForest::missForest(Xpred_mis, maxiter = 5)

    ## DATA NORMALIZATION
    Xtemp_norm <- as.data.frame(scale(Xtemp_imp$ximp))
    Xpred_norm <- as.data.frame(scale(Xpred_imp$ximp))
  }

  # Automated labeling of sub-matrices, assigned to X
  X <- as.data.frame(Xtemp_norm)
  Y <- Ytemp[n]
  eval(parse(text=paste0("k",j_global," <- k")))
  eval(parse(text=paste0("n",j_global," <- n")))

  ## KNOCKOFF FILTER
  ## IMPORTANT  --> subjects # >> features # !!!
  ## It creates KO_result_j objects with all the stats, results, FDR proportion,...
  # knockoff.filter(X, Y, fdr = 0.2, statistic = NULL,
  # threshold = c("knockoff", "knockoff+"), knockoffs = c("equicorrelated","sdp"),
  #               normalize = TRUE, randomize = FALSE)
  if (dim(X)[2]<dim(X)[1])
  {
    eval(parse(text=paste0("KO_result_",j_global," = knockoff::knockoff.filter(X,Y,fdr = 0.05)")))
    eval(parse(text=paste0("KO_selected_",j_global," <- as.numeric(sub(\"V\",\"\",names(KO_result_",j_global,"$selected)))")))
  } else {
    eval(parse(text=paste0("KO_selected_",j_global,"<-NULL")))

  }
  print(algorithm_list)
  # SUPERLEARNER-SL FUNCTION CALL that generates SL objects
  SL <- try(SuperLearner::SuperLearner(Y , X , newX = Xpred_norm,
                                       family=stats::binomial(),
                                       SL.library=algorithm_list,
                                       method="method.NNLS",
                                       verbose = FALSE,
                                       control = list(saveFitLibrary = TRUE),
                                       cvControl = list(V=10)));

  ## Here I store the SL cofficients for each subsample
  eval(parse(text=paste0("m_validation_",j_global,"<-stats::coef(SL)")))
  #m_validation[j_global,] <- stats::coef(SL)

  SL_Pred <- data.frame(prediction = SL$SL.predict[, 1])
  cat("SL_Pred")
  print(SL_Pred)
  Classify <-  NULL;
  Classify_MSE <- NULL
  for (i in 1:dim(SL_Pred)[1])
  {
    ifelse(is.na(SL_Pred[i,]),Classify[i] <- stats::runif(1,0,1),Classify[i] <- SL_Pred[i,])
  }
  for (i in 1:dim(SL_Pred)[1]) {
    ifelse((Classify[i] > 0.5),Classify[i] <- 1,Classify[i] <- 0)
  }

  Classify_MSE <- apply(SL_Pred, 1, function(xx) xx[unname(which.max(xx))])

  print("Classify_MSE")
  print(Classify_MSE)
  eval(parse(text=paste0("SL_Pred_",j_global," <- Classify")))
  eval(parse(text=paste0("SL_Pred_MSE_",j_global," <- Classify_MSE")))

  # This checks if the SL_Pred object was successfully generated (i.e., if it exists)
  # If it does not exist, it is set to a double equal to 100
  eval(parse(text=paste0("ifelse(exists(\"SL_Pred_",j_global,"\"),'OK',
                         SL_Pred_",j_global," <- 100)")))

  truth=Ypred; # set the true outcome to be predicted
  pred=Classify

  cmatrix <- caret::confusionMatrix(pred,truth)
  eval(parse(text=paste0("cmatrix_",j_global,"<- cmatrix")))
  eval(parse(text=paste0("Accuracy_",j_global,"<- unname(cmatrix$overall[1])")))

  ## MSE GENERATION
  # This sets the "MSE_jglobal" to the ACCURACY of the prediction (i.e., cmatrix$overall[1])
  # The final ranking will then be done on the accuracy rather than
  # on a distance-based metrics. A real MSE calculation might be OK for continuous outcomes
  sum=0
  for(s in 1:length(Ypred)) {
    ## This checks first if the TYPE of the prediction object SL_Pred is NOT a double (which means that
    ## the prediction step worked in the previous module. If it is NOT a double, the MSE is calculated
    ## as the mean of the sum of the square of the difference between the prediction and the data to predict.
    # If the SL_Pred is a double, SL_Pred_j is assigned to its MSE (very high value).
    ifelse(exists("Classify_MSE") ,sum <- sum+sum(Ypred[s] - Classify_MSE[s])^2,sum <- 100)
  }
  ## This step makes the final calculation of the MSE and it labels it with a j --> MSE_j
  eval(parse(text=paste0("MSE_",j_global," <- sum/length(Ypred)")))
  print("MSE_jglobal")
  eval(parse(text=paste0("print(MSE_",j_global,")")))

  # GENERATE THE LIGHT DATASET BY DELETING THE SL OBJECT
  rm(SL)
  rm(Xtemp_norm)
  rm(Xpred_norm)
  rm(X)
  rm(Y)

  # Here I remove the imputed dataset, if imputation was performed
  if (isTRUE(length(which(is.na(Xtemp_mis) == "TRUE")) > 0)){
    rm(Xtemp_imp)
    rm(Xpred_imp)
  }

  # SAVE THE RDATA WORKSPACE WITH THE ALL DATA
  filename <- file.path(workspace_directory,
                        paste0("CBDA_SL_M",M,"_miss",misValperc,"_n",range_n,
                               "_k",range_k,"_Light_",j_global,"_",label,"_VALIDATION.RData"))
  save(list = ls(all.names = TRUE), file = filename)

  #eval(parse(text=paste0("save(list = ls(all.names = TRUE),
  #                           file= \"",workspace_directory,"/CBDA_SL_M",M,"_miss",misValperc,"_n",range_n,"_k"
  #                       ,range_k,"_Light_",j_global,"_",label,"_VALIDATION.RData\")")))
  message("Completion %")
  message((counter/(max_covs-min_covs+1))*100, digits = 2)
  counter = counter + 1;

  ## Consolidation of the Validation Workspaces
  # for (i in min_covs:max_covs){
  #   filename <- file.path(workspace_directory,
  #                         paste0("CBDA_SL_M",M,"_miss",misValperc,"_n",range_n,
  #                                "_k",range_k,"_Light_",i,"_",label,"_VALIDATION.RData"))
  #   load(filename)
  # }

  eval(parse(text=paste0("print(MSE_",j_global,")")))
  print(sum/length(Ypred))
  return("CIAO !!")
}

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
filename_specs <- file.path(workspace_directory,paste0(label,"_validation_info.RData"))
#filename_specs <- file.path(workspace_directory,paste0(label,"_info.RData"))
load(filename_specs)

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

# algorithm_list <- c("SL.glmnet","SL.glmnet.0.25","SL.glmnet.0.50","SL.bartMachine",
#                   "SL.glmnet.0.75","SL.svm","SL.randomForest", "SL.knn","SL.glmnet.0")
#test_example <- c("SL.glm","SL.randomForest","SL.glmnet","SL.svm")
CBDA_Validation.pipeline(job_id_val = j_global, Ytemp , Xtemp ,  max_covs = max_covs , min_covs = min_covs,
                         alpha = alpha, Kcol_min = Kcol_min , Kcol_max = Kcol_max,
                         Nrow_min = Nrow_min , Nrow_max = Nrow_max ,
                         misValperc = misValperc, M = M , N_cores = N_cores ,top = top,
                         label = label , workspace_directory = workspace_directory)


