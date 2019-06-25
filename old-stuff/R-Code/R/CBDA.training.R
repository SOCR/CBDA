#' @title
#'  Training/Learning Compressive Big Data Analytics - CBDA.training function
#'
#' @description
#'  This CBDA function comprises all the input specifications to run a set M of subsamples
#'  from the Big Data [Xtemp, Ytemp]. We assume that the Big Data is already clean and harmonized.
#'  This version 1.0.0 is fully tested ONLY on continuous features Xtemp and binary outcome Ytemp.
#'  It only performs the Training/Learning step of the CBDA protocol.
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
#' @details This function comprises all the input specifications to run a set M of subsamples
#'  from the Big Data [Xtemp, Ytemp]. We assume that the Big Data is already clean and harmonized.
#'  After the necessary data wrangling (i.e., imputation, normalization and rebalancing),
#'  an ensemble predictor (i.e., SuperLearner) is applied to each subsample for training/learning.
#'  The list of algorithms used by the SuperLearner is supplied by an external file to be placed
#'  in the working directory (e.g.: CBDA_SL_library.m in our release). The file can contain any SuperLearner
#'  wrapper and any wrappers properly defined by the user.
#'  The ensemble predictive model is then validated on a fraction alpha of the Big Data.
#'  Each subsample generates a predictive model that is ranked based on performance metrics
#'  (e.g., Mean Square Error-MSE and Accuracy) during the first validation step.
#'  IMPORTANT - Memory limits to run CBDA:
#'  see \url{https://stat.ethz.ch/R-manual/R-devel/library/base/html/Memory-limits.html}
#'  for various limitations on memory needs while running R under different OS.
#'  As far as CBDA is concerned, a CBDA object can be up to 200-300 Mb.
#'  The space needed to save all the workspaces however may need to be as large as 1-5 Gb,
#'  depending on the number of subsamples.
#'  We are working on an new CBDA implementation that reduces the storage constraints.
#'
#'
#' @return CBDA object with validation results and 3 RData workspaces
#'
#' @references See \url{https://github.com/SOCR/CBDA/releases} for details on the CBDA protocol
#' and the manuscript "Controlled Feature Selection and Compressive Big Data Analytics:
#' Applications to Big Biomedical and Health Studies” [under review] authored by Simeone Marino,
#' Jiachen Xu, Yi Zhao, Nina Zhou, Yiwang Zhou, Ivo D. Dinov from the University of Michigan
#'
#' @examples
#' # Installation
#' # Please upload the Windows binary and/or source CBDA_1.0.0 files from
#' # the CBDA Github repository https://github.com/SOCR/CBDA/releases

#' \dontrun{
#' # Installation from the Windows binary (recommended for Windows systems)
#' install.packages("/filepath/CBDA_1.0.0_binary_Windows.zip", repos = NULL, type = "win.binary")
#'
#' # Installation from the source (recommended for Macs and Linux systems)
#' install.packages("/filepath/CBDA_1.0.0_source_.tar.gz", repos = NULL, type = "source")
#'
#' # Initialization
#' # This function call installs (if needed) and attaches all the necessary packages to run
#' # the CBDA package v1.0.0. It should be run before any production run or test.
#' # The output shows a table where for each package a TRUE or FALSE is displayed.
#' # Thus the necessary steps can be pursued in case some package has a FALSE.
#' CBDA_initialization()
#'
#' # Set the specs for the synthetic dataset to be tested
#' n = 300          # number of observations
#' p = 100          # number of variables
#'
#' # Generate a nxp matrix of IID variables (e.g., ~N(0,1))
#' X1 = matrix(rnorm(n*p), nrow=n, ncol=p)
#'
#' # Setting the nonzero variables - signal variables
#' nonzero=c(1,100,200,300,400,500,600,700,800,900)
#'
#' # Set the signal amplitude (for noise level = 1)
#' amplitude = 10
#'
#' # Allocate the nonzero coefficients in the correct places
#' beta = amplitude * (1:p %in% nonzero)
#'
#' # Generate a linear model with a bias (e.g., white  noise ~N(0,1))
#' ztemp <- function() X1 %*% beta + rnorm(n)
#' z = ztemp()
#'
#' # Pass it through an inv-logit function to
#' # generate the Bernoulli response variable Ytemp
#' pr = 1/(1+exp(-z))
#' Ytemp = rbinom(n,1,pr)
#' X2 <- cbind(Ytemp,X1)
#'
#' dataset_file ="Binomial_dataset_3.txt"
#'
#' # Save the synthetic dataset
#' a <- tempdir()
#' write.table(X2, file = paste0(file.path(a),'/',dataset_file), sep=",")
#'
#' # The file is now stored in the directory a
#' a
#' list.files(a)
#'
#' # Load the Synthetic dataset
#' Data = read.csv(paste0(file.path(a),'/',dataset_file),header = TRUE)
#' Ytemp <- Data[,1] # set the outcome
#' original_names_Data <- names(Data)
#' cols_to_eliminate=1
#' Xtemp <- Data[-cols_to_eliminate] # set the matrix X of features/covariates
#' original_names_Xtemp <- names(Xtemp)
#'
#' # Add more wrappers/algorithms to the SuperLearner ensemble predictor
#' # It can be commented out if only the default set of algorithms are used,
#' # e.g., algorithm_list = c("SL.glm","SL.xgboost","SL.glmnet","SL.svm",
#' #                          "SL.randomForest","SL.bartMachine")
#' # This defines a "new" wrapper, based on the default SL.glmnet
#'  SL.glmnet.0.75 <- function(..., alpha = 0.75,family="binomial"){
#'                  SL.glmnet(..., alpha = alpha, family = family)}
#'
#'  test_example <- c("SL.glmnet","SL.glmnet.0.75")
#'
#' # Call the CBDA function
#' # Multicore functionality NOT enabled
#' CBDA_object <- CBDA.training(Ytemp , Xtemp , M = 12 , Nrow_min = 50, Nrow_max = 70,
#'               top = 10, max_covs = 8 , min_covs = 3,algorithm_list = test_example ,
#'               workspace_directory = a)
#'
#' # Multicore functionality enabled
#' test_example <- c("SL.xgboost","SL.svm")
#' CBDA_test <- CBDA.training(Ytemp , Xtemp , M = 40 , Nrow_min = 50, Nrow_max = 70,
#'                N_cores = 2 , top = 30, max_covs = 20 ,
#'                 min_covs = 5 , algorithm_list = test_example ,
#'               workspace_directory = a)
#'                 }
#'
#' @export
#'
#' @import foreach
#' @importFrom SuperLearner "All"
#'
CBDA.training <- function(Ytemp , Xtemp , label = "CBDA_package_test" , alpha = 0.2,
                 Kcol_min = 5 , Kcol_max = 15, Nrow_min = 30 , Nrow_max = 50 ,
                 misValperc = 0, M = 3000 , N_cores = 1 , top = 1000,
                 workspace_directory = tempdir(), max_covs = 100 , min_covs = 5 ,
                 algorithm_list = c("SL.glm","SL.xgboost","SL.glmnet","SL.svm","SL.randomForest","SL.bartMachine")) {


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
  range_n <- paste0(Nrow_min,"_",Nrow_max)
  range_k <- paste0(Kcol_min,"_",Kcol_max)
  #range_n <- eval(parse(text=paste0("c(\"",Nrow_min,"_",Nrow_max,"\")")))
  #range_k <- eval(parse(text=paste0("c(\"",Kcol_min,"_",Kcol_max,"\")")))

  cat("Subsampling size = ", M,"\n\n")
  cat("Case Sampling Range - CSR (%) = ", range_n,"%\n\n")
  cat("Feature Sampling Range - FSR (%) = ", range_k,"%\n\n")

  message("Learning/Training steps initiated successfully !!")

  if (N_cores != 1) {
    message("MULTICORE IS ENABLED")
    suppressWarnings(cl <- parallel::makeCluster(N_cores,type = "SOCK", outfile=""))# linux type="FORK"
    suppressWarnings(doParallel::registerDoParallel(cl))
    suppressWarnings(foreach::foreach(j_global=1:M,.packages =c("SuperLearner","foreach"),
          .export=ls(envir=globalenv()))) %dopar%
      suppressWarnings({
     # Suppress the doMC warning message for parallelization statistics
     #suppressMessages(message("Warning: doMC is not installed. Without parallelization, the statistics will be slower to compute"))
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
    if (dim(X)[2]<dim(X)[1])
    {
      eval(parse(text=paste0("KO_result_",j_global," = knockoff::knockoff.filter(X,Y,fdr = 0.05)")))
      eval(parse(text=paste0("KO_selected_",j_global," <- as.numeric(sub(\"V\",\"\",names(KO_result_",j_global,"$selected)))")))

    } else {
      eval(parse(text=paste0("KO_selected_",j_global,"<-NULL")))
    }

    ## SUPERLEARNER and KNOCKOFF LOOP
    # SUPERLEARNER-SL FUNCTION CALL that generates SL objects
    SL <- try(SuperLearner::SuperLearner(Y , X , newX = Xpred_norm,
                           family=stats::binomial(),
                           SL.library=algorithm_list,
                           method="method.NNLS",
                           verbose = FALSE,
                           control = list(saveFitLibrary = TRUE),
                           cvControl = list(V=10)));

    ## Here I store the SL cofficients for each subsample
    eval(parse(text=paste0("m_",j_global,"<-stats::coef(SL)")))

    SL_Pred <- data.frame(prediction = SL$SL.predict[, 1])
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

    eval(parse(text=paste0("SL_Pred_",j_global," <- Classify")))
    eval(parse(text=paste0("SL_Pred_MSE_",j_global," <- Classify_MSE")))

    # This checks if the SL_Pred object was successfully generated (i.e., if it exists)
    # If it does not exist, it is set to a double equal to 100
    eval(parse(text=paste0("ifelse(exists(\"SL_Pred_",j_global,"\"),'OK',
                       SL_Pred_",j_global," <- 100)")))

    truth=factor(Ypred); # set the true outcome to be predicted
    pred=factor(Classify)

    cmatrix <- suppressWarnings(caret::confusionMatrix(pred,truth, positive = "1"))
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
                                   "_k",range_k,"_Light_",j_global,"_",label,".RData"))
      save(list = ls(all.names = TRUE), file=filename)

      #eval(parse(text=paste0("save(list = ls(all.names = TRUE),
      #                       file= \"",workspace_directory,"/CBDA_SL_M",M,"_miss",misValperc,"_n",range_n,"_k"
      #                       ,range_k,"_Light_",j_global,"_",label,".RData\")")))

   })
      filename_specs <- file.path(workspace_directory,paste0(label,"_info.RData"))
      save(label,workspace_directory,M,range_k,range_n,misValperc,
           Nrow_min,Nrow_max,N_cores,Kcol_min,Kcol_max,min_covs,max_covs,
           top,algorithm_list,alpha, file = filename_specs)

    #eval(parse(text=paste0("save(label,workspace_directory,M,range_k,range_n,misValperc,label,Nrow_min,Nrow_max,N_cores,
    #                            Kcol_min,Kcol_max,min_covs,max_covs,top,algorithm_list,
    #                         file= \"",workspace_directory,"/",label,"_info.RData\")")))
      suppressWarnings(parallel::stopCluster(cl))
} else suppressWarnings({

  # Suppress the doMC warning message for parallelization statistics
  #suppressMessages(message("Warning: doMC is not installed. Without parallelization, the statistics will be slower to compute"))
  # Here I define an empty matrix where I store the SL cofficients for each subsample
  m = matrix(0,M,length(algorithm_list))

  for (j_global in 1:M)
  {
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
    out_temp <- dim(Data_unbalanced)[2]
    Data_balanced <- suppressWarnings(smotefamily::SMOTE(Data_unbalanced[,-out_temp],Data_unbalanced[,out_temp]))
    X <- Data_balanced$data[,-out_temp]
    Y <- as.numeric(Data_balanced$data[,out_temp])

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
    SL <- try(SuperLearner::SuperLearner(Y , X , newX = Xpred_norm,
                           family=stats::binomial(),
                           SL.library=algorithm_list,
                           method="method.NNLS",
                           verbose = FALSE,
                           control = list(saveFitLibrary = TRUE),
                           cvControl = list(V=10)));

    ## Here I store the SL cofficients for each subsample
    m[j_global,] <- stats::coef(SL)

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

    cmatrix <- suppressWarnings(caret::confusionMatrix(pred,truth, positive = "1"))
    eval(parse(text=paste0("Accuracy_",j_global,"<- unname(cmatrix$overall[1])")))

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
    filename_specs <- file.path(workspace_directory,paste0(label,"_info.RData"))
    save(label,workspace_directory,M,range_k,range_n,misValperc,
         Nrow_min,Nrow_max,N_cores,Kcol_min,Kcol_max,min_covs,max_covs,
         top,algorithm_list,alpha, file = filename_specs)

    filename <- file.path(workspace_directory,
                          paste0("CBDA_SL_M",M,"_miss",misValperc,"_n",range_n,
                                 "_k",range_k,"_Light_",j_global,"_",label,".RData"))
    save(list = ls(all.names = TRUE), file=filename)


    # eval(parse(text=paste0("save(label,workspace_directory,M,range_k,range_n,misValperc,
    #                               Nrow_min,Nrow_max,N_cores,Kcol_min,Kcol_max,min_covs,max_covs,
    #                               top,algorithm_list,alpha,
    #                        file= \"",workspace_directory,"/",label,"_info.RData\")")))
    message("Completion %")
    message((j_global/M)*100)
  }
})
  cat("Learning/Training steps completed successfully !!\n\n")

  return()

}
