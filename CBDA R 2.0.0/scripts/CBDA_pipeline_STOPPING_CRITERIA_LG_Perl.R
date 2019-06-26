#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
print(args)
label=as.array(args[1])
workspace_directory=as.array(args[2])
# arg_file = as.array(args[1])
# print(arg_file)
# j_global=as.numeric(args[2])
# alpha=as.numeric(args[3])
# dataset_file = as.array(args[4])
# i_exp=as.numeric(args[7])

print(label)
print(workspace_directory)
# print(j_global)
# print(alpha)

# workspace_directory = "~/"
#workspace_directory = "/ifs/loni/ccb/collabs/2016/CBDA_SL_2016/2018"

filename_specs <- file.path(workspace_directory,paste0(label,"_validation_info.RData"))
#filename_specs <- file.path(workspace_directory,paste0("_validation_info.RData"))

print(filename_specs)
load(filename_specs)

print(max_covs)
print(min_covs)

## Here I check if all the validation jobs completed successfully (like for the training)
## Here I check if any job failed, before launchng the consolidation step
filename1 <- paste0("CBDA_M",M,"_miss",misValperc,"_")
filename2 <- paste0("_",label,"_VALIDATION.RData")
print(filename1)
print(filename2)
pattern_temp = paste0("*",filename2) 
w0=list.files(path = workspace_directory , pattern = pattern_temp)
print(w0)

jobID_temp_validation <- NULL
for (i in 1:length(w0))
{
  # print(w0[i])
  test1=sub(x=w0[i],filename1,"")
  # print(test1)
  # print(filename2)
  test2=sub(x=test1,filename2,"")
  jobID_temp_validation=c(jobID_temp_validation,test2)
}

failedJobs_validation <- NULL
print(jobID_temp_validation)
validation_seq=seq(min_covs,max_covs,1)
failedJobs_locations <- which(validation_seq %in% as.numeric(jobID_temp_validation)==FALSE)
failedJobs_validation <- validation_seq[failedJobs_locations]
print(failedJobs_validation)
filename_failedJobs <- file.path(workspace_directory,'/Results/failedJobs_VALIDATION.txt')
#print(filename_failedJobs)
## This checks if it is the 2nd time (at least) that the training has been restarted
## with an existing failedJobs_VALIDATION.txt file in the directory. If that is the case,
## the existing file failedJobs.txt is deleted and recreated, if there's any failed job
if (length(list.files(path=workspace_directory,pattern="/Results/failedJobs_VALIDATION.txt")>0))
{
  cat("Removing the existing file: failedJobs_VALIDATION.txt \n\n")
  file.remove(filename_failedJobs) 
}
## This should be empty
list.files(path=workspace_directory,pattern="/Results/failedJobs_VALIDATION.txt")

if (length(failedJobs_validation)>0)
{ 
  cat("Some jobs failed !! Recreate the  file: failedJobs_VALIDATION.txt \n\n")
  write.table(as.matrix(failedJobs_validation),file=filename_failedJobs,row.names = FALSE,
              col.names = FALSE,sep = ",", eol = "\r\n" ,
              fileEncoding = "UTF-8", quote = FALSE)
  # If I want to save it as a row
  #write.table(as.matrix(t(failedJobs_validation)),file="failedJobs.txt",row.names = FALSE,
  #            col.names = FALSE,sep = ",", eol = "\r\n" ,
  #            fileEncoding = "UTF-8", quote = FALSE)
  stop(paste0("IMPORTANT: ", length(failedJobs_validation), " VALIDATION jobs failed !!\n\n
              See the file failedJobs_VALIDATION.txt for details."))
}

print("All VALIDATION jobs completed successfully !! \n
      Consolidation can start !!")
cat("CONSOLIDATION STEP HAS STARTED !!\n\n")

# Consolidation of the Validation Workspaces
for (i in min_covs:max_covs){
  filename <- file.path(workspace_directory,
                        paste0("CBDA_M",M,"_miss",misValperc,"_",i,"_",label,"_VALIDATION.RData"))
  load(filename)
}

filename <- file.path(workspace_directory,
                      paste0("CBDA_M",M,"_miss",misValperc,"_",label,"_VALIDATION.RData"))
save(list = ls(all.names = TRUE), file = filename)

library(CBDA)
# CBDA_object <- CBDA_Stopping_Criteria.pipeline(label = label , Kcol_min = Kcol_min , Kcol_max = Kcol_max,
#                                    Nrow_min = Nrow_min , Nrow_max = Nrow_max ,
#                                    misValperc = misValperc, workspace_directory = workspace_directory,
#                                    M = M , max_covs = max_covs , min_covs = min_covs)

CBDA_Stopping_Criteria.pipeline.LG <- function(label = "CBDA_package_test" , misValperc = 0, M = 3000 ,
                                               workspace_directory = tempdir(), max_covs = 100 , min_covs = 3,
                                               lambda = 1.005) {
  
  range_n <- range_k <- qa_ALL <- algorithm_list <- cmatrix_ALL_validation <- NULL
  
  message("STOPPING CRITERIA GENERATION STEP HAS STARTED !!")
  # filename_specs <- file.path(workspace_directory,paste0(label,"_validation_info.RData"))
  # #eval(parse(text=paste0("load(\"",workspace_directory,"/",label,"_info.RData\")")))
  # load(filename_specs)
  
  filename <- file.path(workspace_directory,
                        paste0("CBDA_M",M,"_miss",misValperc,"_",label,"_VALIDATION.RData"))
  load(filename)

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
  print(length(algorithm_list))
  print(max_covs)
  m_validation = matrix(0,max_covs,length(algorithm_list))
  for (i in min_covs:max_covs){
    eval(parse(text=paste0("m_validation[",i,",] <- m_validation_",i)))
    eval(parse(text=paste0("cmatrix_ALL_validation[[",i,"]] <- cmatrix",i)))
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
  
  #load("qa_ALL.RData")
  #if(is.na(StopMSE[1]))
  #{StopMSE <- StopAcc}
  CBDA_object_Validation <- NULL
  #CBDA_object_Validation[[1]] <- qa_ALL_Validation
  CBDA_object_Validation[[1]] <- Stopping_Criteria
  CBDA_object_Validation[[2]] <- cmatrix_ALL_validation
  CBDA_object_Validation[[3]] <- algorithm_list
  CBDA_object_Validation[[4]] <- m_validation
  CBDA_object_Validation[[5]] <-   as.numeric(as.character(qa_ALL$MSE[1:dim(qa_ALL)[1]]))-1
  CBDA_object_Validation[[6]] <-   as.numeric(as.character(qa_ALL$Accuracy[1:dim(qa_ALL)[1]]))-1
  
  #names(CBDA_object_Validation)[1] <- c("LearningTable")
  names(CBDA_object_Validation)[1] <- c("ValidationTable")
  names(CBDA_object_Validation)[2] <- c("ConfusionMatrices")
  names(CBDA_object_Validation)[3] <- c("SuperLearnerLibrary")
  names(CBDA_object_Validation)[4] <- c("SuperLearnerCoefficients")
  names(CBDA_object_Validation)[5] <- c("TopFeaturesMSE")
  names(CBDA_object_Validation)[6] <- c("TopFeaturesAccuracy")
  
  for (j in min_covs:max_covs)
  {
    eval(parse(text=paste0("rm(cmatrix",j,")")))
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
                        paste0("CBDA_M",M,"_miss",misValperc,"_",label,"_VALIDATION.RData"))
  save(list = ls(all.names = TRUE), file = filename)
  
  cat("Performance metrics for the nested Predictive models.\n\n")
  cat("VALIDATION TABLE\n\n")
  print(CBDA_object_Validation$ValidationTable)
  cat("\n\nStopping Criteria completed successfully !!\n\n")
  
  
  cat("Removing *jobID_validation.RData workspaces\n\n")
  for (j in min_covs:max_covs) {
    filename <- file.path(workspace_directory,
                          paste0("CBDA_M",M,"_miss",misValperc,
                                 "_",j,"_",label,"_VALIDATION.RData"))
    file.remove(filename)
  }
  
  return(CBDA_object_Validation)
}

print(max_covs)
print(min_covs)
print(label)

CBDA_object_Validation <- CBDA_Stopping_Criteria.pipeline.LG(label = label ,
                                    workspace_directory = workspace_directory,
                                    M = M , max_covs = max_covs , min_covs = min_covs)


