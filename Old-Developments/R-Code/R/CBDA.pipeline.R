#' @title
#'  Training/Leaning Step for Compressive Big Data Analytics - LONI PIPELINE
#'
#' @description
#'  The CBDA.pipeline() function comprises all the input specifications to run a set M of subsamples
#'  from the Big Data [Xtemp, Ytemp]. We assume that the Big Data is already clean and harmonized.
#'  This version 1.0.0 is fully tested ONLY on continuous features Xtemp and binary outcome Ytemp.
#'
#' @param job_id This is the ID for the job generator in the LONI pipeline interface
#'
#' @param Ytemp This is the output variable (vector) in the original Big Data

#' @param Xtemp This is the input variable (matrix) in the original Big Data

#' @param label This is the label appended to RData workspaces generated within the CBDA calls

#' @param alpha Percentage of the Big Data to hold off for Validation

#' @param Kcol_min Lower bound for the percentage of features-columns sampling (used for the Feature Sampling Range - FSR)

#' @param Kcol_max Upper bound for the percentage of features-columns sampling (used for the Feature Sampling Range - FSR)

#' @param Nrow_min 	Lower bound for the percentage of cases-rows sampling (used for the Case Sampling Range - CSR)

#' @param Nrow_max Upper bound for the percentage of cases-rows sampling (used for the Case Sampling Range - CSR)

#' @param misValperc Percentage of missing values to introduce in BigData (used just for testing, to mimic real cases).

#' @param M Number of the BigData subsets on which perform Knockoff Filtering and SuperLearner feature mining

#' @param N_cores Number of Cores to use in the parallel implementation (default is set to 1 core)

#' @param top Top predictions to select out of the M (must be < M, optimal ~0.1*M)

#' @param workspace_directory Directory where the results and workspaces are saved (set by default to tempdir())

#' @param max_covs Top features to display and include in the Validation Step where nested models are tested

#' @param min_covs Minimum number of top features to include in the initial model
#'                 for the Validation Step (it must be greater than 2)

#' @param algorithm_list List of algorithms/wrappers used by the SuperLearner.
#'                   By default is set to the following list
#'                   algorithm_list <- c("SL.glm","SL.xgboost",
#'                   "SL.glmnet","SL.svm","SL.randomForest","SL.bartMachine")
#'
#' @return CBDA object with validation results and 3 RData workspaces

#' @export
#'
#' @import foreach
#' @importFrom SuperLearner "All"
#'
CBDA.pipeline <- function(job_id , Ytemp , Xtemp , label = "CBDA_package_test" , alpha = 0.2,
                          Kcol_min = 5 , Kcol_max = 15, Nrow_min = 30 , Nrow_max = 50 ,
                          misValperc = 0, M = 3000 , N_cores = 1 , top = 1000,
                          workspace_directory = setwd(tempdir()), max_covs = 100 , min_covs = 5 ,
                          algorithm_list = c("SL.glm","SL.xgboost","SL.glmnet","SL.svm","SL.randomForest","SL.bartMachine")) {
  #print(algorithm_list)

  # Check if min_covs is > 2
  if (min_covs < 3){
    cat("The parameter min_covs must be at least 3 !!\n\n")
    cat("It's been set to 3 by default.\n\n")
    min_covs <- 3
  }


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

  # Set the specs for the unique CBDA-SL & KO job
  range_n <- eval(parse(text=paste0("c(\"",Nrow_min,"_",Nrow_max,"\")")))
  range_k <- eval(parse(text=paste0("c(\"",Kcol_min,"_",Kcol_max,"\")")))

  cat("Subsampling size = ", M,"\n\n")
  cat("Case Sampling Range - CSR (%) = ", range_n,"%\n\n")
  cat("Feature Sampling Range - FSR (%) = ", range_k,"%\n\n")

  print("Learning/Training steps initiated successfully !!")

  ## Here I define an empty matrix where I store the SL cofficients for each subsample
  #m = matrix(0,M,length(algorithm_list))

  j_global <- job_id

  # Re-set the seed to run a different CBDA selection on the Data
  set.seed(round(as.numeric(Sys.time()) %% 1e3 * j_global * (1+log(j_global))))

  # This step defines and samples the subsets of features for training based on the Kcol sample
  Kcol <- round(dim(Xtemp)[2]*(stats::runif(1,Kcol_min/100,Kcol_max/100))) # sample a value from a uniform distribution within Kcol_min and Kcol_max [number of features/columns of the big dataset]
  k <- sample(1:dim(Xtemp)[2],Kcol)

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
    Xtemp_imp <- missForest::missForest(Xtemp_mis, maxiter = 5)
    Xpred_imp <- missForest::missForest(Xpred_mis, maxiter = 5)

    ## DATA NORMALIZATION
    Xtemp_norm <- as.data.frame(scale(Xtemp_imp$ximp))
    Xpred_norm <- as.data.frame(scale(Xpred_imp$ximp))
  }

  # Automated labeling of sub-matrices, assigned to X
  # X <- as.data.frame(Xtemp_norm)
  # Y <- Ytemp[n]
  eval(parse(text=paste0("k",j_global," <- k")))
  eval(parse(text=paste0("n",j_global," <- n")))


  ## BALANCING BEFORE THE SUPERLEARNER LOOP
  ## This steps balances the dataset wrt the different categories,
  ## using SMOTE (Synthetic Minority Oversampling TEchnique)
  X_unbalanced <- as.data.frame(Xtemp_norm)
  Y_unbalanced <- Ytemp[n]
  Data_unbalanced <- cbind(X_unbalanced,Y_unbalanced)
  Data_balanced <- smotefamily::SMOTE(Data_unbalanced[,-dim(Data_unbalanced)[2]],Data_unbalanced[,dim(Data_unbalanced)[2]])
  X <- Data_balanced$data[,-dim(Data_unbalanced)[2]]
  Y <- as.numeric(Data_balanced$data[,dim(Data_unbalanced)[2]])

  ## KNOCKOFF FILTER
  ## IMPORTANT  --> subjects # >> features # !!!
  ## It creates KO_result_j objects with all the stats, results, FDR proportion,...
  if (dim(X)[2]<dim(X)[1])
  {
    eval(parse(text=paste0("KO_result_",j_global," = knockoff::knockoff.filter(X,Y,fdr = 0.05)")))
    eval(parse(text=paste0("KO_selected_",j_global," <- as.numeric(sub(\"V\",\"\",names(KO_result_",j_global,"$selected)))")))
  } else {
    eval(parse(text=paste0("KO_selected_",j_global,"<-NULL")))

  }

  # SUPERLEARNER-SL FUNCTION CALL that generates SL objects
  # SL <- SuperLearner::SuperLearner(Y , X , newX = Xpred_norm,
  #                                      family=stats::binomial(),
  #                                      SL.library=algorithm_list,
  #                                      method="method.NNLS",
  #                                      verbose = FALSE,
  #                                      control = list(saveFitLibrary = TRUE),
  #                                      cvControl = list(V=10));
  SL <- try(SuperLearner::SuperLearner(Y , X , newX = Xpred_norm,
                                       family=stats::binomial(),
                                       SL.library=algorithm_list,
                                       method="method.NNLS",
                                       verbose = FALSE,
                                       control = list(saveFitLibrary = TRUE),
                                       cvControl = list(V=10)));

  ## Here I store the SL cofficients for each subsample
  #m[j_global,] <- stats::coef(SL)
  eval(parse(text=paste0("m_",j_global,"<-stats::coef(SL)")))

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

  cmatrix <- caret::confusionMatrix(Classify,Ypred)
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
  # I might need to add the "/" before "CBDA ("/CBDA_...)
  filename <- file.path(workspace_directory,
                        paste0("CBDA_SL_M",M,"_miss",misValperc,"_n",range_n,
                               "_k",range_k,"_Light_",j_global,"_",label,".RData"))
  save(list = ls(all.names = TRUE), file=filename)

  # I might need to add the "/" before label (paste0("/",label,"_info.RData"))
  filename_specs <- file.path(workspace_directory,paste0(label,"_info.RData"))
  save(label,workspace_directory,M,range_k,range_n,misValperc,
       Nrow_min,Nrow_max,N_cores,Kcol_min,Kcol_max,min_covs,max_covs,
       top,alpha, file = filename_specs)

  return()
}
