## ----Installation of the CBDA package, eval = FALSE----------------------
#  # Installation from the Windows binary (recommended for Windows systems)
#  install.packages("/filepath/CBDA_1.0.0.zip", repos = NULL, type = "win.binary")
#  # Installation from the source (recommended for Macs and Linux systems)
#  install.packages("/filepath/CBDA_1.0.0.tar.gz", repos = NULL, type = "source")

## ----Installation of the CBDA package from CRAN, eval = FALSE------------
#  install.packages("CBDA")

## ----Memory allocation, eval = FALSE-------------------------------------
#  memory.limit(50000)  # to allocate 50Gb of memory

## ----setup, eval = FALSE-------------------------------------------------
#  # Set the specs for the synthetic dataset to be tested
#  n = 300          # number of observations
#  p = 900          # number of variables
#  
#  # Generate a nxp matrix of IID variables (e.g., ~N(0,1))
#  X1 = matrix(rnorm(n*p), nrow=n, ncol=p)
#  
#  # Setting the nonzero variables - signal variables
#  nonzero=c(1,100,200,300,400,500,600,700,800,900)
#  
#  # Set the signal amplitude (for noise level = 1)
#  amplitude = 10
#  
#  # Allocate the nonzero coefficients in the correct places
#  beta = amplitude * (1:p %in% nonzero)
#  
#  # Generate a linear model with a bias (e.g., white  noise ~N(0,1))
#  ztemp <- function() X1 %*% beta + rnorm(n)
#  z = ztemp()
#  
#  # Pass it through an inv-logit function to
#  # generate the Bernoulli response variable Ytemp
#  pr = 1/(1+exp(-z))
#  Ytemp = rbinom(n,1,pr)
#  X2 <- cbind(Ytemp,X1)
#  
#  dataset_file ="Binomial_dataset_3.txt"
#  
#  # Save the synthetic dataset
#  write.table(X2,dataset_file,sep=",")
#  
#  # Load the Synthetic dataset
#  Data = read.csv(dataset_file,header = TRUE)
#  Ytemp <- Data[,1] # set the outcome
#  original_names_Data <- names(Data)
#  cols_to_eliminate=1
#  Xtemp <- Data[-cols_to_eliminate] # set the matrix X of features/covariates
#  original_names_Xtemp <- names(Xtemp)
#  
#  workspace_directory <- getwd()
#  
#  SL.glmnet.0.75 <- function(..., alpha = 0.75,family="binomial"){
#    SL.glmnet(..., alpha = alpha, family = family)}
#  
#  simeone <- c("SL.glm","SL.glmnet",
#               "SL.svm","SL.randomForest","SL.bartMachine","SL.glmnet.0.75")
#  
#  # Call the Main CBDA function
#  # Multicore functionality NOT enabled
#  CBDA_object <- CBDA(Ytemp , Xtemp , M = 16 , Nrow_min = 50, Nrow_max = 70,
#                      top = 15, max_covs = 10 , min_covs = 3)
#  
#  # Multicore functionality enabled
#  CBDA_object <- CBDA(Ytemp , Xtemp , M = 24 , Nrow_min = 60, Nrow_max = 80,
#                      N_cores = 4 , top = 20, max_covs = 15 ,  min_covs = 3,
#                      algorithm_list = simeone , label = "CBDA_package_test_multicore")

