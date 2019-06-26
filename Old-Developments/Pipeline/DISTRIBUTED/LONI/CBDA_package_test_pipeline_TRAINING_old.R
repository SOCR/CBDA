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
CBDA_initialization <- function(pkg =c("missForest" , "SuperLearner" , "knockoff" ,
                                       "smotefamily" , "glmnet","bartMachine")) {
  new.pkg <- pkg[!(pkg %in% utils::installed.packages()[, "Package"])]
  if (length(new.pkg))
    utils::install.packages(new.pkg, dependencies = TRUE, repos='http://cran.rstudio.com/')

  a_temp <- sapply(pkg, require, character.only = TRUE)

  return(a_temp)
}
print(tempdir())

CBDA_initialization()

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

# SL.Bartmachine Wrappers
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

SL.randomForest.100 <- function(..., ntree=100,family = "binomial"){
  SL.randomForest(..., ntree = ntree,family = family)
}
SL.randomForest.200 <- function(..., ntree=200,family = "binomial"){
  SL.randomForest(..., ntree = ntree,family = family)
}
SL.randomForest.300 <- function(..., ntree=300,family = "binomial"){
  SL.randomForest(..., ntree = ntree,family = family)
}
SL.randomForest.400 <- function(..., ntree=400,family = "binomial"){
  SL.randomForest(..., ntree = ntree,family = family)
}
SL.randomForest.600 <- function(..., ntree=600,family = "binomial"){
  SL.randomForest(..., ntree = ntree,family = family)
}
SL.randomForest.700 <- function(..., ntree=700,family = "binomial"){
  SL.randomForest(..., ntree = ntree,family = family)
}
SL.randomForest.800 <- function(..., ntree=800,family = "binomial"){
  SL.randomForest(..., ntree = ntree,family = family)
}
SL.randomForest.900 <- function(..., ntree=900,family = "binomial"){
  SL.randomForest(..., ntree = ntree,family = family)
}
SL.randomForest.1000 <- function(..., ntree=1000,family = "binomial"){
  SL.randomForest(..., ntree = ntree,family = family)
}

test_example <- c("SL.glm","SL.bayesglm","SL.earth","SL.glm.interaction","SL.ipredbagg",
                  "SL.knn","SL.mean","SL.nnet","SL.nnls",
                  "SL.randomForest","SL.bartMachine",
                  "SL.svm","SL.glmnet",
                  "SL.svm.radial.10", "SL.svm.radial.0.1", "SL.svm.radial.default",
                  "SL.svm.poly.2.0", "SL.svm.poly.3.0", "SL.svm.poly.3.10", "SL.svm.poly.3.n10",
                  "SL.svm.poly.6.0", "SL.svm.poly.6.10", "SL.svm.poly.6.n10", "SL.svm.linear","SL.svm.sigmoid",
                  "SL.glmnet.0","SL.glmnet.0.25","SL.glmnet.0.50","SL.glmnet.0.75",
                  "SL.bartMachine.20","SL.bartMachine.100","SL.bartMachine.500",
                  "SL.knn.5","SL.knn.25","SL.knn.50","SL.knn.100",
                  "SL.randomForest.100","SL.randomForest.200","SL.randomForest.300","SL.randomForest.400",
                  "SL.randomForest.600","SL.randomForest.700","SL.randomForest.800","SL.randomForest.900",
                  "SL.randomForest.1000")

algorithm_list_not_working = c("SL.biglasso","SL.extraTrees","SL.kernelKnn","SL.ksvm",
                               "SL.lda","SL.lm","SL.qda","SL.speedglm","SL.xgboost","SL.speedlm")

## THIS IS THE DEFINITION OF THE CBDA.pipeline() function used below
CBDA.pipeline <- function(job_id , Ytemp , Xtemp , label = "CBDA_package_test" , alpha = 0.2,
                          Kcol_min = 5 , Kcol_max = 15, Nrow_min = 30 , Nrow_max = 50 ,
                          misValperc = 0, M = 3000 , N_cores = 1 , top = 1000,
                          workspace_directory = setwd(tempdir()), max_covs = 100 , min_covs = 5 ,
                          algorithm_list = c("SL.glm","SL.glmnet","SL.svm","SL.randomForest","SL.bartMachine")) {
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

CBDA.pipeline(job_id = j_global, Ytemp , Xtemp ,
              M = 9000 , Nrow_min = 40, Nrow_max = 60,
              top = 1000, max_covs = 50 , min_covs = 3,
              label = "Binomial" , algorithm_list = test_example ,
              workspace_directory = workspace_directory)
