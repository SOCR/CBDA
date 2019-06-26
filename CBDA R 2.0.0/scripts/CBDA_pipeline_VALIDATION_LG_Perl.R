#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
job_id=as.numeric(args[1])
label=as.array(args[2])
workspace_directory=as.array(args[3])
#M= as.array(args[4])
#top= as.array(args[5])
#max_covs= as.array(args[6])
#dataset_file = as.array(args[2])
#alpha=as.numeric(args[3])
# i_exp=as.numeric(args[5])

print(job_id)
#print(dataset_file)
#print(alpha)
print(workspace_directory)
print(label)

version
print ("Attaching CBDA, xgboost,Superlearner, FNN and SMOTE package....")
library(CBDA)
library(xgboost)
library(bartMachine)
library(bartMachineJARs)
library(FNN)
library(smotefamily)
library(SuperLearner)
print ("Status OK ... Exiting!")

library(CBDA)
library(xgboost)
library(bartMachine)
library(bartMachineJARs)
library(FNN)
library(smotefamily)
library(SuperLearner)
print ("Status OK ... Exiting!")


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
                  "SL.bartMachine","SL.bartMachine.500","SL.bartMachine.100","SL.bartMachine.20",
                  "SL.svm","SL.svm.radial.10", "SL.svm.radial.0.1", "SL.svm.radial.default",
                  "SL.svm.poly.2.0", "SL.svm.poly.3.0", "SL.svm.poly.3.10", "SL.svm.poly.3.n10",
                  "SL.svm.poly.6.0", "SL.svm.poly.6.10", "SL.svm.poly.6.n10", "SL.svm.linear","SL.svm.sigmoid",
                  "SL.randomForest","SL.randomForest.1000","SL.randomForest.500","SL.randomForest.300",
                  "SL.randomForest.100","SL.randomForest.50","SL.randomForest.20",
                  "SL.xgboost","SL.xgboost.500","SL.xgboost.300","SL.xgboost.2000",
                  "SL.xgboost.1500","SL.xgboost.d3","SL.xgboost.d5", "SL.xgboost.d6",
                  "SL.xgboost.gau","SL.xgboost.shrink.15","SL.xgboost.shrink.2",
                  "SL.xgboost.shrink.05","SL.xgboost.shrink.25",
                  "SL.knn","SL.knn.100","SL.knn.50","SL.knn.25","SL.knn.5")
algorithm_list_not_working = c("SL.biglasso","SL.extraTrees","SL.kernelKnn","SL.ksvm",
                               "SL.lda","SL.lm","SL.qda","SL.speedglm","SL.speedlm")
# Removed because of the following error message:
#Error in get.knnx(data, query, k, algorithm) : Data include NAs
#Calls: CBDA.pipeline.Perl.LG -> <Anonymous> -> knearest -> <Anonymous> -> get.knnx
#"SL.knn","SL.knn.100","SL.knn.50","SL.knn.25","SL.knn.5",




## THIS IS THE DEFINITION OF THE CBDA.pipeline() function used below
CBDA.pipeline.Validation.Perl.LG <- function(job_id , label = "CBDA_package_test" , misValperc = 0 ,
                                  M = 9000 ,  top = 1000, max_covs = 100 , min_covs = 3 ,
                                  algorithm_list = c("SL.glm","SL.xgboost","SL.glmnet","SL.svm","SL.randomForest","SL.bartMachine"),
                                  workspace_directory = setwd(tempdir())) {
  
  # Check if min_covs is > 2
  if (min_covs < 3){
    cat("The parameter min_covs must be at least 3 !!\n\n")
    cat("It's been set to 3 by default.\n\n")
    min_covs <- 3
  }
  
  # I might need to add the "/" before label (paste0("/",label,"_info.RData"))
  # filename_specs_1 <- file.path(workspace_directory,paste0(label,"_info.RData"))
  # M=as.numeric(M)
  # print(filename_specs_1)
  # save(label,workspace_directory,M,misValperc,top,min_covs,max_covs,
  #      file = filename_specs_1)
  eval(parse(text=paste0("trainingSet <- c('X",job_id,"_final.txt')")))
  eval(parse(text=paste0("validationSet <- c('X",job_id,"_validation_final.txt')")))
  eval(parse(text=paste0("featureSet <- c('newk",job_id,".txt')")))
  trainingSet_specs <- file.path(workspace_directory,trainingSet)
  validationSet_specs <- file.path(workspace_directory,validationSet)
  featureSet_specs <- file.path(workspace_directory,featureSet)
  
  print(trainingSet_specs)
  print(validationSet_specs)
  print(featureSet)
  print(featureSet_specs)
  
X=read.table(trainingSet_specs,sep=",",header = FALSE)
rownames(X)<-colnames(X)<-NULL
Xv=read.table(validationSet_specs,sep=",",header = FALSE)
rownames(Xv)<-colnames(Xv)<-NULL
print(dim(X))
print(dim(Xv))

print(X[1:5,])
print(Xv[1:5,])
pilot_data=X
a=ifelse((is.na(pilot_data)),1,0);
print(100*sum(a)/(dim(pilot_data)[1]*dim(pilot_data)[2]))
pilot_data=Xv
a=ifelse((is.na(pilot_data)),1,0);
print(100*sum(a)/(dim(pilot_data)[1]*dim(pilot_data)[2]))

# k=as.numeric(unlist(read.table(featureSet_specs,sep=",",header = FALSE,col.names = FALSE)))
# rownames(k)<-colnames(k)<-NULL
# eval(parse(text=paste0("k",job_id,"<-k")))

cases_to_delete=NULL
cases_with_924_missing_data = as.numeric(c(102, 112, 230, 244, 249, 337, 418))
# Cases that overlap between training and validation sets
b=intersect(Xv[,1],X[,1])
# Cases with too many missing data
b=sort(unique(c(b)))
if (length(b)==0){
  print("No cases overlap between training and validation sets\n\n")
} else {
  for (i in 1:length(b))
  {
    a=which(Xv[,1]==b[i])
    print(a)
    is.null(a)
    cases_to_delete[i]=as.numeric(a)}
}

# Here I delete the cases in the validation set that overlap with the training  set
Xv=Xv[-cases_to_delete,]
dim(X)
dim(Xv)

## TRAINING SETS

# Here I save into n1 the cases left selected for training
eval(paste0("n",job_id,"<-X[,1]"))
# Here I save into Ytemp the output for training
Ytemp <- X[,2]
print(Ytemp)

# Here I save into k1 the features selected for training
# eval(parse(text=paste0("featureSet <- c('newk",job_id,".txt')")))
# featureSet_specs <- file.path(workspace_directory,featureSet)
# columnNames <- read.table(featureSet_specs,sep=",",header = FALSE)
# rownames(columnNames)<-colnames(columnNames)<-NULL
# eval(paste0("k",job_id,"<-columnNames"))

k=as.numeric(unlist(read.table(featureSet_specs,sep=",",header = FALSE,col.names = FALSE)))
rownames(k)<-colnames(k)<-NULL
eval(parse(text=paste0("k",job_id,"<-k")))
columnNames <- k
print(columnNames)


# here I define and delete the first 2 columns from both the X training
# and validation sets (cases IDs and Output columns, respectively
cols_to_delete = c(1,2)
Xtemp <- X[-cols_to_delete]
names(Xtemp) <- columnNames

print(dim(Xtemp))
print(Xtemp[1:5,])
## VALIDATION SETS
# Here I save into n1 the cases left selected for validation
casesIDsValidation <- Xv[,1]
# Here I save into Ytemp the output for validation
Ypred <- Xv[,2]
# here I define and delete the first 2 columns from the X validation set (cases IDs and Output columns, respectively
Xpred <- Xv[-cols_to_delete]
names(Xpred) <- columnNames

print(length(Ypred))
print(Ypred[1:5])


## DATA IMPUTATION
Xtemp_mis <- Xtemp
# Here we pass ONLY the subset of columns [,k] for imputation and scaling of Xpred [validation/preditcion]
Xpred_mis <- Xpred
# Here I impute the missing data with the function missForest
if (isTRUE(length(which(is.na(Xtemp_mis) == "TRUE")) == 0)){
  ## DATA NORMALIZATION (NO IMPUTATION)
  Xtemp_norm <- as.data.frame(scale(Xtemp_mis))
  Xpred_norm <- as.data.frame(scale(Xpred_mis))
} else {
  ## DATA IMPUTATION
  print("Data Imputation is being performed !!")
  Xtemp_imp <- missForest::missForest(Xtemp_mis, maxiter = 5)
  Xpred_imp <- missForest::missForest(Xpred_mis, maxiter = 5)
  
  ## DATA NORMALIZATION
  print("Data Normalization is being performed !!")
  Xtemp_norm <- as.data.frame(scale(Xtemp_imp$ximp))
  Xpred_norm <- as.data.frame(scale(Xpred_imp$ximp))
}

# print(dim(Xtemp_norm))
# print(Xtemp_norm[1:5,])
# 
# print(dim(Xpred_norm))
# print(Xpred_norm[1:5,])
# 

# Automated labeling of sub-matrices, assigned to X
# X <- as.data.frame(Xtemp_norm)
# Y <- Ytemp[n]
#eval(parse(text=paste0("k",job_id," <- columnNames")))
#eval(parse(text=paste0("n",job_id," <- casesIDs")))


## BALANCING BEFORE THE SUPERLEARNER LOOP
## This steps balances the dataset wrt the different categories,
## using SMOTE (Synthetic Minority Oversampling TEchnique)
X_unbalanced <- as.data.frame(Xtemp_norm)
Y_unbalanced <- Ytemp
Data_unbalanced <- cbind(X_unbalanced,Y_unbalanced)
Data_balanced <- smotefamily::SMOTE(Data_unbalanced[,-dim(Data_unbalanced)[2]],Data_unbalanced[,dim(Data_unbalanced)[2]])
X <- Data_balanced$data[,-dim(Data_unbalanced)[2]]
Y <- as.numeric(Data_balanced$data[,dim(Data_unbalanced)[2]])

data_temp=X
a=ifelse((is.na(data_temp)),1,0);
print("Check if there are missing data")
print(100*sum(a)/(dim(data_temp)[1]*dim(data_temp)[2]))

data_temp=Xpred_norm
a=ifelse((is.na(data_temp)),1,0);
print("Check if there are missing data")
print(100*sum(a)/(dim(data_temp)[1]*dim(data_temp)[2]))

## KNOCKOFF FILTER
## IMPORTANT  --> subjects # >> features # !!!
## It creates KO_result_j objects with all the stats, results, FDR proportion,...
if (dim(X)[2]<dim(X)[1])
{
  eval(parse(text=paste0("KO_result_",job_id," = knockoff::knockoff.filter(X,Y,fdr = 0.05)")))
  eval(parse(text=paste0("KO_selected_",job_id," <- as.numeric(sub(\"V\",\"\",names(KO_result_",job_id,"$selected)))")))
} else {
  eval(parse(text=paste0("KO_selected_",job_id,"<-NULL")))
  
}
algorithm_list=test_example
print(dim(X))
print(length(Y))
print(dim(Xpred_norm))
print(X[1:5,])
print(Y[1:5])
print(Xpred_norm[1:5,])

SL <- try(SuperLearner::SuperLearner(Y , X , newX = Xpred_norm,
                                     family=stats::binomial(),
                                     SL.library=algorithm_list,
                                     method="method.NNLS",
                                     verbose = FALSE,
                                     control = list(saveFitLibrary = TRUE),
                                     cvControl = list(V=10)));

## Here I store the SL cofficients for each subsample
#m[job_id,] <- stats::coef(SL)
eval(parse(text=paste0("m_validation_",job_id,"<-stats::coef(SL)")))

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
eval(parse(text=paste0("SL_Pred_",job_id," <- Classify")))
eval(parse(text=paste0("SL_Pred_MSE_",job_id," <- Classify_MSE")))

# This checks if the SL_Pred object was successfully generated (i.e., if it exists)
# If it does not exist, it is set to a double equal to 100
eval(parse(text=paste0("ifelse(exists(\"SL_Pred_",job_id,"\"),'OK',
                       SL_Pred_",job_id," <- 100)")))

cat('Ypred')
print(Ypred)
cat('Classify')
print(Classify)
truth=factor(Ypred) # set the true outcome to be predicted
pred=factor(Classify)
cat('levels of truth-Ypred \n')
print(levels(truth))
cat('levels of pred-Classify \n')
print(levels(pred))
cat('confusion matrix call \n')
cmatrix <- caret::confusionMatrix(pred,truth)
eval(parse(text=paste0("cmatrix",job_id,"<- cmatrix")))
eval(parse(text=paste0("Accuracy_",job_id,"<- unname(cmatrix$overall[1])")))

## MSE GENERATION
# This sets the "MSE_jobID" to the ACCURACY of the prediction (i.e., cmatrix$overall[1])
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
eval(parse(text=paste0("MSE_",job_id," <- sum/length(Ypred)")))
print("MSE_job_id")
eval(parse(text=paste0("print(MSE_",job_id,")")))

rm(SL)

# SAVE THE RDATA WORKSPACE WITH THE ALL DATA
# I might need to add the "/" before "CBDA ("/CBDA_...)
filename <- file.path(workspace_directory,
                      paste0("CBDA_M",M,"_miss",misValperc,"_",job_id,"_",label,"_VALIDATION.RData"))
save(list = ls(all.names = TRUE), file=filename)

cat("VALIDATION SUCCESSFULL !!!\n\n")

return()

}

filename_specs <- file.path(workspace_directory,paste0(label,"_info.RData"))
print(workspace_directory)
print(filename_specs)
load(filename_specs)
top=as.numeric(top)
max_covs=as.numeric(max_covs)

## Before I call the VALIDATION function, I want to clean up the workspace directory from
## unnecessary files, namely rows_only.txt, rows_only_validation.txt, k*.txt and X*.txt
cat("Removing unnecessary files (k.txt, rows_only.txt and rows_only_validation.txt)\n\n")
w0=list.files(path=workspace_directory,pattern="only")
w1=list.files(path=workspace_directory,pattern="^X")
w2=list.files(path=workspace_directory,pattern="^k")
file.remove(path=workspace_directory,w0)
#file.remove(path=workspace_directory,w1)
file.remove(path=workspace_directory,w2)
cat("Unnecessary files removed\n\n")

CBDA.pipeline.Validation.Perl.LG(job_id = job_id, label = label , M = M , 
                      min_covs = as.numeric(min_covs) , max_covs = as.numeric(max_covs) , 
                      top = as.numeric(top), algorithm_list = test_example , 
                      workspace_directory = workspace_directory)


