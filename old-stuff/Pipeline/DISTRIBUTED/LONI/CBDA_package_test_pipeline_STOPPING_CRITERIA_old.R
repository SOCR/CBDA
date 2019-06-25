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

# cat("lambda has been changed again")
# label = "Binomial"
# workspace_directory = "~/"
#workspace_directory = "/ifs/loni/ccb/collabs/2016/CBDA_SL_2016/2018"




filename_specs <- file.path(workspace_directory,paste0(label,"_validation_info.RData"))
print(filename_specs)
load(filename_specs)

# Consolidation of the Validation Workspaces
for (i in min_covs:max_covs){
  filename <- file.path(workspace_directory,
                        paste0("CBDA_SL_M",M,"_miss",misValperc,"_n",range_n,
                               "_k",range_k,"_Light_",i,"_",label,"_VALIDATION.RData"))
  load(filename)
}

filename <- file.path(workspace_directory,
                      paste0("CBDA_SL_M",M,"_miss",misValperc,"_n",range_n,
                             "_k",range_k,"_Light_",label,"_VALIDATION.RData"))
save(list = ls(all.names = TRUE), file = filename)

CBDA_Stopping_Criteria.pipeline <- function(label = "CBDA_package_test" , Kcol_min = 5 , Kcol_max = 15,
                                   Nrow_min = 30 , Nrow_max = 50 , misValperc = 0, M = 3000 ,
                                   workspace_directory = tempdir(), max_covs = 100 , min_covs = 5,
                                   lambda = 1.005) {

  range_n <- range_k <- qa_ALL <- NULL

  message("STOPPING CRITERIA GENERATION STEP HAS STARTED !!")
  filename_specs <- file.path(workspace_directory,paste0(label,"_validation_info.RData"))
  #eval(parse(text=paste0("load(\"",workspace_directory,"/",label,"_info.RData\")")))
  load(filename_specs)

  filename <- file.path(workspace_directory,
                        paste0("CBDA_SL_M",M,"_miss",misValperc,"_n",range_n,
                               "_k",range_k,"_Light_",label,"_VALIDATION.RData"))
  load(filename)
  #eval(parse(text=paste0("load(\"",workspace_directory,"/CBDA_SL_M",M,"_miss",
  #                       misValperc,"_n",range_n,"_k",range_k,"_Light_",label,"_VALIDATION.RData\")")))

  qa_ALL_Validation <- NULL
  qa_ALL_Validation <- matrix(0,max_covs-min_covs+1,5)

  counter <- 1
  for(j_global in min_covs:max_covs)
  {
    eval(parse(text=paste0("qa_ALL_Validation[",counter,",1] <- ",j_global,"")))
    eval(parse(text=paste0("qa_ALL_Validation[",counter,",2] <- Accuracy_",j_global,"")))
    eval(parse(text=paste0("qa_ALL_Validation[",counter,",3] <- MSE_",j_global,"")))
    counter <- counter + 1
  }

  m_validation = matrix(0,max_covs,length(algorithm_list))
    for (i in min_covs:max_covs){
      eval(parse(text=paste0("m_validation[",i,",] <- m_validation_",i)))
      eval(parse(text=paste0("cmatrix_ALL_validation[[",i,"]] <- cmatrix_",i)))
      }
print(qa_ALL_Validation)
  ## Stopping Criteria for Accuracy and MSE Performance Metrics
  ## Two more columns added with 0 (continue) and 1 (stop)
  StopAcc <- NULL
  StopMSE <- NULL
  for(i in 1:dim(qa_ALL_Validation)[1]-1)
  {
    # Simple improvement (1%,5%, 0.05% in Accuracy)
    ifelse((qa_ALL_Validation[i+1,2]/qa_ALL_Validation[i,2]) > lambda,
           StopAcc[i] <- "Keep Going", StopAcc[i] <- "Stop")
    # F of Fisher test
    ifelse((qa_ALL_Validation[i,3]/qa_ALL_Validation[i,3])/(qa_ALL_Validation[i+1,3]/qa_ALL_Validation[i+1,3])
           > stats::qf(.95, df1=qa_ALL_Validation[i,3], df2=qa_ALL_Validation[i+1,3]),
           StopMSE[i] <- "Keep Going", StopMSE[i] <- "Stop")
  }

  Stopping_Criteria <- data.frame(NumberOfTopFeatures=qa_ALL_Validation[,1],Inference_Acc=qa_ALL_Validation[,2],
                                  Inference_MSE = qa_ALL_Validation[,3] ,
                                  StopAcc=c(StopAcc,"NA"), StopMSE=c(StopMSE,"NA"))

  if(is.na(StopMSE[1]))
  {StopMSE <- StopAcc}
  CBDA_object <- NULL
  CBDA_object[[1]] <- qa_ALL
  CBDA_object[[2]] <- cmatrix_ALL_validation
  CBDA_object[[3]] <- algorithm_list
  CBDA_object[[4]] <- m_validation
  CBDA_object[[5]] <- Stopping_Criteria

  names(CBDA_object)[1] <- c("LearningTable")
  names(CBDA_object)[2] <- c("ConfusionMatrices")
  names(CBDA_object)[3] <- c("SuperLearnerLibrary")
  names(CBDA_object)[4] <- c("SuperLearnerCoefficients")
  names(CBDA_object)[5] <- c("ValidationTable")
  CBDA_object[[6]] <- as.numeric(as.character(CBDA_object$LearningTable[,1]))
  names(CBDA_object)[6] <- c("TopFeatures")

  for (j in min_covs:max_covs)
  {
    eval(parse(text=paste0("rm(cmatrix_",j,")")))
    eval(parse(text=paste0("rm(KO_result_",j,")")))
    eval(parse(text=paste0("rm(Accuracy_",j,")")))
    eval(parse(text=paste0("rm(Classify)")))
    eval(parse(text=paste0("rm(Classify_MSE)")))
    eval(parse(text=paste0("rm(counter)")))
    eval(parse(text=paste0("rm(j)")))
    eval(parse(text=paste0("rm(i)")))
    eval(parse(text=paste0("rm(j_global)")))
    eval(parse(text=paste0("rm(k)")))
    eval(parse(text=paste0("rm(k_Acc)")))
    eval(parse(text=paste0("rm(k_ALL)")))
  }

  filename <- file.path(workspace_directory,
                        paste0("CBDA_SL_M",M,"_miss",misValperc,"_n",range_n,
                               "_k",range_k,"_Light_",label,"_VALIDATION.RData"))
  save(list = ls(all.names = TRUE), file = filename)

  # This only saves the matrix of SL coefficients for the training step
  filename_1 <- file.path(workspace_directory,
                          paste0(label,"_m_validation.RData"))
  save(m_validation, file = filename_1)
  
  cat("Performance metrics for the nested Predictive models.\n")
  cat("VALIDATION TABLE\n")
  print(CBDA_object$ValidationTable)
  cat("\n\nStopping Criteria completed successfully !!\n\n")

  return(CBDA_object)
}


CBDA_object <- CBDA_Stopping_Criteria.pipeline(label = label , Kcol_min = Kcol_min , Kcol_max = Kcol_max,
                                   Nrow_min = Nrow_min , Nrow_max = Nrow_max ,
                                   misValperc = misValperc, workspace_directory = workspace_directory,
                                   M = M , max_covs = max_covs , min_covs = min_covs)

# This loop cleans up all the top ranked validation predictive models
for (j in min_covs:max_covs) {
  filename <- file.path(workspace_directory,
                        paste0("CBDA_SL_M",M,"_miss",misValperc,"_n",range_n,
                               "_k",range_k,"_Light_",j,"_",label,"_VALIDATION.RData"))
  file.remove(filename)

}
cat("Clean up completed successfully !!")
