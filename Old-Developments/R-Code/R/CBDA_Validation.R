#' @title
#' CBDA Validation function for Compressive Big Data Analytics
#'
#' @description
#'  This CBDA function generates *max_covs - min_covs* nested models based on the ranking
#'  returned by the *Consolidation* function. It also consolidates all the *max_covs - min_covs*
#'  workspaces into a single one.

# @param Ytemp This is the output variable (vector) in the original Big Data
#
# @param Xtemp This is the input variable (matrix) in the original Big Data

#' @param label This is the label appended to RData workspaces generated within the CBDA calls

#' @param alpha Percentage of the Big Data to hold off for Validation

#' @param Kcol_min Lower bound for the percentage of features-columns sampling (used for the Feature Sampling Range - FSR)

#' @param Kcol_max Upper bound for the percentage of features-columns sampling (used for the Feature Sampling Range - FSR)

#' @param Nrow_min 	Lower bound for the percentage of cases-rows sampling (used for the Case Sampling Range - CSR)

#' @param Nrow_max Upper bound for the percentage of cases-rows sampling (used for the Case Sampling Range - CSR)

#' @param misValperc Percentage of missing values to introduce in BigData (used just for testing, to mimic real cases).

#' @param M Number of the BigData subsets on which perform Knockoff Filtering and SuperLearner feature mining

#' @param N_cores Number of Cores to use in the parallel implementation

#' @param top Top predictions to select out of the M

#' @param workspace_directory Directory where the results and workspaces are saved

#' @param max_covs Top features to display and include in the Validation Step where nested models are tested

#' @param min_covs Minimum number of top features to include in the initial model for the Validation Step

#' @return value

#' @export
#'
#' @import foreach
#'
#' @importFrom SuperLearner "All"


# CBDA_Validation <- function(Ytemp , Xtemp , label = "CBDA_package_test" , alpha = 0.2,
#                             Kcol_min = 5 , Kcol_max = 15, Nrow_min = 30 , Nrow_max = 50 ,
#                             misValperc = 0, M = 3000 , N_cores = 1 ,top = 1000,
#                             workspace_directory = tempdir(), max_covs = 100 , min_covs = 5) {

CBDA_Validation <- function(label = "CBDA_package_test" , alpha = 0.2,
                            Kcol_min = 5 , Kcol_max = 15, Nrow_min = 30 , Nrow_max = 50 ,
                            misValperc = 0, M = 3000 , N_cores = 1 ,top = 1000,
                            workspace_directory = tempdir(), max_covs = 100 , min_covs = 5) {

  m <- NULL
  # Check if min_covs is > 2
  if (min_covs < 3){
    cat("The parameter min_covs must be at least 3 !!\n\n")
    cat("It's been set to 3 by default.\n\n")
    min_covs <- 3
  }

  range_n <- range_k <- qa_ALL <- algorithm_list  <- cmatrix_ALL_validation <- Xtemp <- Ytemp <- NULL

  cat("VALIDATION STEP for TOP nested predictive models has started.\n\n")
  filename_specs <- file.path(workspace_directory,paste0(label,"_info.RData"))
  #eval(parse(text=paste0("load(\"",workspace_directory,"/",label,"_info.RData\")")))
  load(filename_specs)

  filename <- file.path(workspace_directory,
                        paste0("CBDA_SL_M",M,"_miss",misValperc,"_n",range_n,
                               "_k",range_k,"_Light_",label,".RData"))
  load(filename)

  # This step defines and samples the correct subsets of subjects for training
  Nrow <- round(dim(Xtemp[-q,])[1]*(stats::runif(1,Nrow_min/100,Nrow_max/100))) # sample a value from a uniform distribution Nrox_min and Nrow_max [number of rows/subjects of the big dataset]
  n_all <- 1:length(Ytemp)
  n_sub <- n_all[-q]
  a0 = round(Nrow/2) # balanced # of subjects
  # Randomly select patients for prediction in a balanced way based on the Nrow sample and a0
  n1 = sample(which(Ytemp[n_sub]==1),a0)
  n2 = sample(which(Ytemp[n_sub]==0),a0)
  n <- c(n1 , n2)
  n <- n_sub

  Xpred <- Xtemp[q,] # define the feature set for prediction (renamed Xpred) [not used in the training/learning]
  Ypred <- Ytemp[q] # define the output for prediction (renamed Ypred) [not used in the training/learning]

  # This step defines and samples the subsets of features for training based on the Kcol sample
  #max_k <- max_covs#min(max_covs,dim(qa_ALL)[1])
  k_Acc <- as.numeric(paste(qa_ALL$Accuracy[1:(max_covs)]))
  k_MSE <- as.numeric(paste(qa_ALL$MSE[1:(max_covs)]))

  # Here we chose the ranking determined by the MSE performance as the best
  k_ALL <- k_MSE

  #algorithm_list <- c("SL.glm","SL.xgboost",
  #                     "SL.glmnet","SL.svm","SL.randomForest","SL.bartMachine")

  counter = 1

  cat("Subsampling size = ", M,"\n\n")
  cat("Case Sampling Range - CSR (%) = ", range_n,"%\n\n")
  cat("Feature Sampling Range - FSR (%) = ", range_k,"%\n\n")

  if (N_cores != 1) {
    message("MULTICORE IS ENABLED")
    cl <- parallel::makeCluster(N_cores,type = "SOCK")# linux type="FORK"
    doParallel::registerDoParallel(cl)
    foreach::foreach(j_global=min_covs:max_covs,.packages =c("SuperLearner","foreach")) %dopar%
    {
    # Re-set the seed to run a different CBDA selection on the Data
    set.seed(round(as.numeric(Sys.time()) %% 1e3 * j_global * (1+log(j_global))))

    # This step defines and samples the subsets of features for training based on the Kcol sample
    k <- k_ALL[1:j_global]

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

    ## BALANCING BEFORE THE SUPERLEARNER LOOP
    ## This steps balances the dataset wrt the different categories,
    ## using SMOTE (Synthetic Minority Oversampling TEchnique)
    X_unbalanced <- as.data.frame(Xtemp_norm)
    Y_unbalanced <- Ytemp[n]
    Data_unbalanced <- cbind(X_unbalanced,Y_unbalanced)
    out_temp <- dim(Data_unbalanced)[2]
    Data_balanced <- suppressWarnings(smotefamily::SMOTE(Data_unbalanced[,-out_temp],Data_unbalanced[,out_temp]))
    X <- Data_balanced$data[,-out_temp]
    Y <- as.numeric(Data_balanced$data[,out_temp])

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

    SL_Pred <- data.frame(prediction = SL$SL.predict[, 1])
    Classify <-  NULL;

    # This checks if the SL_Pred object was successfully generated (i.e., if it exists)
    # If it does not exist, it is set to a double equal to 100
    eval(parse(text=paste0("ifelse(exists(\"SL_Pred_",j_global,"\"),'OK',
                           SL_Pred_",j_global," <- 100)")))

    for (i in 1:dim(SL_Pred)[1])
    {
      ifelse(is.na(SL_Pred[i,]),Classify[i] <- stats::runif(1,0,1),Classify[i] <- SL_Pred[i,])
    }
    for (i in 1:dim(SL_Pred)[1]) {
      ifelse((Classify[i] > 0.5),Classify[i] <- 1,Classify[i] <- 0)
    }

    eval(parse(text=paste0("SL_Pred_",j_global," <- Classify")))

    truth=factor(Ypred); # set the true outcome to be predicted
    pred=factor(Classify)

    cmatrix <- caret::confusionMatrix(pred,truth, positive = "1")
    eval(parse(text=paste0("Accuracy_",j_global,"<- unname(cmatrix$overall[1])")))
    eval(parse(text=paste0("cmatrix_",j_global,"<- cmatrix")))

    ## MSE GENERATION
    # This sets the "MSE_jglobal" to the ACCURACY of the prediction (i.e., cmatrix$overall[1])
    # The final ranking will then be done on the accuracy rather than
    # on a distance-based metrics. A real MSE calculation might be OK for continuous outcomes
    Classify_MSE <- NULL
    Classify_MSE <- apply(SL_Pred, 1, function(xx) xx[unname(which.max(xx))])
    eval(parse(text=paste0("SL_Pred_MSE_",j_global," <- Classify_MSE")))
    sum=0
    for(s in 1:length(Ypred)) {
      ## This checks first if the TYPE of the prediction object SL_Pred is NOT a double (which means that
      ## the prediction step worked in the previous module. If it is NOT a double, the MSE is calculated
      ## as the mean of the sum of the square of the difference between the prediction and the data to predict.
      # If the SL_Pred is a double, SL_Pred_j is assigned to its MSE (very high value).
      ifelse(exists("Classify_MSE") ,sum <- sum+sum(Ypred[s] - Classify_MSE[s])^2,sum <- 100)
    }
    eval(parse(text=paste0("ifelse(exists(\"SL_Pred_MSE_",j_global,"\"),sum <- sum+sum(Ypred[s] - SL_Pred_MSE_",j_global,"[s])^2,
                           sum <- 10000)")))

    ## This step makes the final calculation of the MSE and it labels it with a j --> MSE_j
    eval(parse(text=paste0("MSE_",j_global," <- sum/length(Ypred)")))

    # GENERATE THE LIGHT DATASET BY DELETING THE SL OBJECT
    rm(SL)
    rm(Xtemp_norm)
    rm(Xpred_norm)
    rm(X)
    rm(Y)

    # Here I remove the imupated dataset, if imputation was performed
    if (isTRUE(length(which(is.na(Xtemp_mis) == "TRUE")) > 0)){
      rm(Xtemp_imp)
      rm(Xpred_imp)
    }

    # SAVE THE RDATA WORKSPACE WITH THE ALL DATA
    filename <- file.path(workspace_directory,
                          paste0("CBDA_SL_M",M,"_miss",misValperc,"_n",range_n,
                                 "_k",range_k,"_Light_",j_global,"_",label,"_VALIDATION.RData"))
    save(list = ls(all.names = TRUE), file=filename)

    #eval(parse(text=paste0("save(list = ls(all.names = TRUE),
    #                         file= \"",workspace_directory,"/CBDA_SL_M",M,"_miss",misValperc,"_n",range_n,"_k"
    #                       ,range_k,"_Light_",j_global,"_",label,"_VALIDATION.RData\")")))
    cat("Completion %\n")
    message(round((counter/(max_covs-min_covs+1))*100, digits = 2))
    counter = counter + 1;
          }

    filename_specs <- file.path(workspace_directory,paste0(label,"_info.RData"))
    save(label,workspace_directory,M,range_k,range_n,misValperc,
         Nrow_min,Nrow_max,N_cores,Kcol_min,Kcol_max,min_covs,max_covs,
         top,algorithm_list,alpha, file = filename_specs)

    #eval(parse(text=paste0("save(label,workspace_directory,M,range_k,range_n,misValperc,
    #          Nrow_min,Nrow_max,N_cores,Kcol_min,Kcol_max,min_covs,max_covs,top,algorithm_list,
    #                       file= \"",workspace_directory,"/",label,"_info.RData\")")))
    parallel::stopCluster(cl)
} else {

  ## Here I define an empty matrix where I store the SL cofficients for each subsample
  m_validation = matrix(0,max_covs,length(algorithm_list))

    for (j_global in min_covs:max_covs)
  {
  # Re-set the seed to run a different CBDA selection on the Data
  set.seed(round(as.numeric(Sys.time()) %% 1e3 * j_global * (1+log(j_global))))

  # This step defines and samples the subsets of features for training based on the Kcol sample
  k <- k_ALL[1:j_global]

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

  ## BALANCING BEFORE THE SUPERLEARNER LOOP
  ## This steps balances the dataset wrt the different categories,
  ## using SMOTE (Synthetic Minority Oversampling TEchnique)
  X_unbalanced <- as.data.frame(Xtemp_norm)
  Y_unbalanced <- Ytemp[n]
  Data_unbalanced <- cbind(X_unbalanced,Y_unbalanced)
  out_temp <- dim(Data_unbalanced)[2]
  Data_balanced <- suppressWarnings(smotefamily::SMOTE(Data_unbalanced[,-out_temp],Data_unbalanced[,out_temp]))
  X <- Data_balanced$data[,-out_temp]
  Y <- as.numeric(Data_balanced$data[,out_temp])

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

  # SUPERLEARNER-SL FUNCTION CALL that generates SL objects
  SL <- try(SuperLearner::SuperLearner(Y , X , newX = Xpred_norm,
                         family=stats::binomial(),
                         SL.library=algorithm_list,
                         method="method.NNLS",
                         verbose = FALSE,
                         control = list(saveFitLibrary = TRUE),
                         cvControl = list(V=10)));

  ## Here I store the SL cofficients for each subsample
  m_validation[j_global,] <- stats::coef(SL)

  SL_Pred <- data.frame(prediction = SL$SL.predict[, 1])
  Classify <-  NULL;

  # This checks if the SL_Pred object was successfully generated (i.e., if it exists)
  # If it does not exist, it is set to a double equal to 100
  eval(parse(text=paste0("ifelse(exists(\"SL_Pred_",j_global,"\"),'OK',
                         SL_Pred_",j_global," <- 100)")))

  for (i in 1:dim(SL_Pred)[1])
  {
    ifelse(is.na(SL_Pred[i,]),Classify[i] <- stats::runif(1,0,1),Classify[i] <- SL_Pred[i,])
  }
  for (i in 1:dim(SL_Pred)[1]) {
    ifelse((Classify[i] > 0.5),Classify[i] <- 1,Classify[i] <- 0)
  }

  eval(parse(text=paste0("SL_Pred_",j_global," <- Classify")))

  truth=factor(Ypred); # set the true outcome to be predicted
  pred=factor(Classify)

  cmatrix <- caret::confusionMatrix(pred,truth, positive = "1")
  eval(parse(text=paste0("Accuracy_",j_global,"<- unname(cmatrix$overall[1])")))
  eval(parse(text=paste0("cmatrix_",j_global,"<- cmatrix")))

  ## MSE GENERATION
  # This sets the "MSE_jglobal" to the ACCURACY of the prediction (i.e., cmatrix$overall[1])
  # The final ranking will then be done on the accuracy rather than
  # on a distance-based metrics. A real MSE calculation might be OK for continuous outcomes
  Classify_MSE <- NULL
  Classify_MSE <- apply(SL_Pred, 1, function(xx) xx[unname(which.max(xx))])
  eval(parse(text=paste0("SL_Pred_MSE_",j_global," <- Classify_MSE")))
  sum=0
  for(s in 1:length(Ypred)) {
    ## This checks first if the TYPE of the prediction object SL_Pred is NOT a double (which means that
    ## the prediction step worked in the previous module. If it is NOT a double, the MSE is calculated
    ## as the mean of the sum of the square of the difference between the prediction and the data to predict.
    # If the SL_Pred is a double, SL_Pred_j is assigned to its MSE (very high value).
    ifelse(exists("Classify_MSE") ,sum <- sum+sum(Ypred[s] - Classify_MSE[s])^2,sum <- 100)
  }
  eval(parse(text=paste0("ifelse(exists(\"SL_Pred_MSE_",j_global,"\"),sum <- sum+sum(Ypred[s] - SL_Pred_MSE_",j_global,"[s])^2,
                         sum <- 10000)")))

  ## This step makes the final calculation of the MSE and it labels it with a j --> MSE_j
  eval(parse(text=paste0("MSE_",j_global," <- sum/length(Ypred)")))

  # GENERATE THE LIGHT DATASET BY DELETING THE SL OBJECT
  rm(SL)
  rm(Xtemp_norm)
  rm(Xpred_norm)
  rm(X)
  rm(Y)

  # Here I remove the imupated dataset, if imputation was performed
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
  message((counter/(max_covs-min_covs+1))*100)
  counter = counter + 1;
    }
}
  ## Consolidation of the Validation Workspaces
  for (i in min_covs:max_covs){
    filename <- file.path(workspace_directory,
                          paste0("CBDA_SL_M",M,"_miss",misValperc,"_n",range_n,
                                 "_k",range_k,"_Light_",i,"_",label,"_VALIDATION.RData"))
    load(filename)

    #eval(parse(text=paste0("load(\"",workspace_directory,"/CBDA_SL_M",M,"_miss",misValperc,"_n",
    #                       range_n,"_k",range_k,"_Light_",i,"_",label,"_VALIDATION.RData\")")))
}

  if (N_cores>1)
  {
    m_validation = matrix(0,max_covs,length(algorithm_list))
    for (i in min_covs:max_covs){
      eval(parse(text=paste0("m_validation[",i,",] <- m_validation_",i)))
    }
  }

  for (i in min_covs:max_covs){
    eval(parse(text=paste0("cmatrix_ALL_validation[[",i,"]] <- cmatrix_",i)))
  }


  CBDA_object <- NULL

  CBDA_object[[1]] <- qa_ALL
  CBDA_object[[2]] <- cmatrix_ALL_validation
  CBDA_object[[3]] <- algorithm_list
  CBDA_object[[4]] <- m_validation
  CBDA_object[[6]] <- m


  filename <- file.path(workspace_directory,
                        paste0("CBDA_SL_M",M,"_miss",misValperc,"_n",range_n,
                               "_k",range_k,"_Light_",label,"_VALIDATION.RData"))
  save(list = ls(all.names = TRUE), file = filename)

  #eval(parse(text=paste0("save(list = ls(all.names = TRUE),
  #                           file= \"",workspace_directory,"/CBDA_SL_M",M,"_miss",misValperc,"_n",range_n,"_k"
  #                         ,range_k,"_Light_",label,"_VALIDATION.RData\")")))

  cat("Validation workspace successfully created.\n\n")
  cat("VALIDATION STEP COMPLETED SUCCESSFULLY !!\n\n")

  return()
}
