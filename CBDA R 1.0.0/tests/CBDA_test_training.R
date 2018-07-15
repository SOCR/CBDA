# Attach the CBDA library
suppressPackageStartupMessages(library(CBDA))
suppressPackageStartupMessages(library(randomForest ))
suppressPackageStartupMessages(library(glmnet ))
suppressPackageStartupMessages(library(SuperLearner))
suppressPackageStartupMessages(library(bartMachine))

library(CBDA, quietly = TRUE, verbose = FALSE)
CBDA_initialization()

# Set the specs for the synthetic dataset to be tested
n = 30000          # number of observations
p = 3000          # number of variables

# Generate a nxp matrix of IID variables (e.g., ~N(0,1))
X1 = matrix(rnorm(n*p), nrow=n, ncol=p)

# Setting the nonzero variables - signal variables
nonzero=c(1,100,200,300,400,500,600,700,800,900)
#nonzero=c(10,20,30,40,50,60,70,80,90,100)

# Set the signal amplitude (for noise level = 1)
amplitude = 10

# Allocate the nonzero coefficients in the correct places
beta = amplitude * (1:p %in% nonzero)

# Generate a linear model with a bias (e.g., white  noise ~N(0,1))
ztemp <- function() X1 %*% beta + rnorm(n)
z = ztemp()

# Pass it through an inv-logit function to
# generate the Bernoulli response variable Ytemp
pr = 1/(1+exp(-z))
Ytemp = rbinom(n,1,pr)
X2 <- cbind(Ytemp,X1)

#dataset_file ="Binomial_dataset.txt"
dataset_file ="Binomial_dataset_30k_1k.txt"
# Save the synthetic dataset
a <- tempdir()
write.table(X2, file = file.path(a, dataset_file), sep=",")

# Load the Synthetic dataset
#Data = read.csv(paste0(file.path(a),'/',dataset_file),header = TRUE)
Data = read.csv(file.path(a, dataset_file),header = TRUE)
Ytemp <- Data[,1] # set the outcome
original_names_Data <- names(Data)
cols_to_eliminate=1
Xtemp <- Data[-cols_to_eliminate] # set the matrix X of features/covariates
original_names_Xtemp <- names(Xtemp)

# Add more wrappers/algorithms to the SuperLearner ensemble predictor
# It can be commented out if only the default set of algorithms are used,
# e.g., algorithm_list = c("SL.glm","SL.xgboost","SL.glmnet","SL.svm",
#                          "SL.randomForest","SL.bartMachine")
# This defines a "new" wrapper, based on the default SL.glmnet
 SL.glmnet.0.75 <- function(..., alpha = 0.75,family="binomial"){
                  SL.glmnet(..., alpha = alpha, family = family)}

# Using Support Vector Machine and the new Glmnet wrapper above
 #test_example <- c("SL.glmnet","SL.glmnet.0.75")
 test_example <- c("SL.glmnet","SL.svm","SL.randomForest","SL.bartMachine")

 test_example <- c("SL.bartMachine","SL.bayesglm","SL.biglasso",
                   "SL.earth","SL.extraTrees","SL.glm.interaction","SL.glmnet",
                   "SL.ipredbagg","SL.kernelKnn","SL.knn","SL.ksvm","SL.lda",
                   "SL.lm","SL.glmnet.0.75","SL.mean","SL.nnet","SL.nnls",
                   "SL.qda","SL.randomForest",
                   "SL.speedglm","SL.speedlm","SL.svm","SL.xgboost")
 ## SINGLE CORE EXAMPLE - TRAINING/LEARNING ONLY
 CBDA_singlecore_training <- CBDA.training(Ytemp , Xtemp , M = 8 ,
                                           Nrow_min = 10, Nrow_max = 15,
                                           top = 6, max_covs = 6 , min_covs = 3,
                                           algorithm_list = test_example , label = "SINGLE_CORE" ,
                                           workspace_directory = a)
 CBDA_singlecore <- CBDA(Ytemp , Xtemp , M = 8 ,Nrow_min = 5, Nrow_max = 10,
                         max_covs = 6 , min_covs = 3,N_cores = 1,top = 6,
                         algorithm_list = test_example , label = "SingleCore-30K_3K_Wrappers" ,
                         workspace_directory = a, Kcol_min = 1, Kcol_max = 5)
 CBDA_multicore_wrappers <- CBDA(Ytemp , Xtemp , M = 8 ,Nrow_min = 5, Nrow_max = 10,
                                 max_covs = 6 , min_covs = 3,N_cores = 1,top = 6,
                                 algorithm_list = test_example , label = "MultiCore-30K_3K_Wrappers" ,
                                 workspace_directory = a, Kcol_min = 1, Kcol_max = 5)
