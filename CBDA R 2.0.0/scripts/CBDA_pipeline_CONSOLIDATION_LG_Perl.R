#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
label=as.array(args[1])
workspace_directory=as.array(args[2])

print(label)
print(workspace_directory)

library(CBDA)
#CBDA_Consolidation.pipeline(top , max_covs , M , misValperc ,  range_k , range_n ,
#                            label = label, workspace_directory = workspace_directory)

CBDA_Consolidation.pipeline_LG <- function(M = 9000, top = 1000, max_covs = 50 , misValperc = 0,
                                        min_covs = 3 , label = 'test', workspace_directory = tempdir()) {
  
  N_cores <- algorithm_list <- x_hist <- Top_features_MSE <- NULL
  Kcol_max <- Kcol_min <- Nrow_max <- Nrow_min <- Xtemp <- Ytemp <- alpha <-  min_covs <- NULL
  
  ## DATA CONSOLIDATION - load M RData workspaces [j_global] per experiment [i_exp]
  ##                      and consolidate them into 1 RData workspace
   for (j in 1:M){
    print(sprintf("Loading workspace: %d", j))
    utils::flush.console()
#    filename <- file.path(workspace_directory,
#                          paste0("CBDA_SL_M",M,"_miss",misValperc,"_n",range_n,
#                                 "_k",range_k,"_Light_",j,"_",label,".RData"))
    filename <- file.path(workspace_directory,
                          paste0("CBDA_M",M,"_miss",misValperc,"_",j,"_",label,".RData"))
    eval(parse(text=paste0("featureSet <- c('k",j,".txt')")))
    featureSet_specs <- file.path(workspace_directory,featureSet)
    k=as.numeric(unlist(read.table(featureSet_specs,sep=",",header = FALSE,col.names = FALSE)))
    rownames(k)<-colnames(k)<-NULL
    eval(parse(text=paste0("k",j,"<-k")))
    
    load(filename)
  }
  print("All kj.txt loaded and k relabeled")
  #  save(list = ls(all.names = TRUE), file=filename)
  #print(M)
  #print(algorithm_list)
  M=as.numeric(M)
  #eval(parse(text=paste0("m_",j_global,"<-stats::coef(SL)")))
  ## Here I define an empty matrix where I store the SL cofficients for each subsample
  m_training = matrix(0,M,length(algorithm_list))
  for (j in 1:M){
    eval(parse(text=paste0("m_training[",j,",] <- m_",j)))
  }
  #  GENERATING THE ARRAY OF MSE AND ACCURACY METRICS FOR ALL THE M SL OBJECTS
  MSE=0;
  Accuracy=0;
  
  for (j in 1:M) {
    eval(parse(text=paste0("MSE[j] <- MSE_",j)))
    eval(parse(text=paste0("Accuracy[j] <- Accuracy_",j)))
  }
  #  REMOVE THE ARRAYS OF MSE and ACCURACY FOR ALL THE
  #  M OBJECTS to avoid stack overflow errors
  for (j in 1:M) {
    eval(parse(text=paste0("rm(MSE_",j,")")))
    eval(parse(text=paste0("rm(Accuracy_",j,")")))
    eval(parse(text=paste0("rm(SL_Pred_",j,")")))
    eval(parse(text=paste0("rm(SL_Pred_MSE_",j,")")))
    
  }

  ## THIS SAVES THE CONSOLIDATED WORKSPACE FOR EACH EXPERIMENT
  # MSE RANKING
  s=M;
  MSE_temp <- NULL
  MSE_sorted_temp <- NULL
  MSE_temp <- data.frame(mse=MSE[1:s],k_set=1:s)
  MSE_sorted_temp <- MSE_temp[order(MSE_temp$mse),]
  
  filename <- file.path(workspace_directory,
                        paste0("CBDA_M",M,"_miss",misValperc,"_",label,".RData"))
  save(list = ls(all.names = TRUE), file = filename)
  
  print("MSE_sorted_temp")
  print(MSE_sorted_temp)
    ## DEFINE HERE THE TOP # OF COVARIATES TO LIST in the MODEL MINING STEP
  # "top" is defined at the beginning (line 8) and represents the top MSEs to consider for
  # feature mining (ks). Each one will have a set of best features with their relative highest frequencies
  top=100
  eval(parse(text=paste0("k_top_",top,"_temp <- NULL")))
  for (r in 1:top){
    #eval(parse(text=paste0("k_top_",top,"_temp <- c(k_top_",top,"_temp, k",MSE_sorted_temp$k_set[r],")")))
    print(parse(text=paste0("k_top_",top,"_temp <- c(k_top_",top,"_temp,as.numeric(k",MSE_sorted_temp$k_set[r],"))")))
    eval(parse(text=paste0("k_top_",top,"_temp <- c(k_top_",top,"_temp,as.numeric(k",MSE_sorted_temp$k_set[r],"))")))
  }
  
  #eval(parse(text=paste0("save(list = ls(all.names = TRUE),
  #    file= \"",workspace_directory,"/CBDA_SL_M",M,"_miss",misValperc,"_n",range_n,
  #                       "_k",range_k,"_Light_",label,".RData\")")))
  
  print("MSE generated correctly")
  #  GENERATING THE ARRAY OF ACCURACY FOR ALL THE M SL OBJECTS
  # ACCURACY RANKING
  Accuracy_temp <- NULL
  Accuracy_sorted_temp <- NULL
  Accuracy_temp <- data.frame(Accuracy=Accuracy[1:s],k_set=1:s)
  Accuracy_sorted_temp <- Accuracy_temp[order(-Accuracy_temp$Accuracy),]
  
  eval(parse(text=paste0("k_top_",top,"_temp_Accuracy <- NULL")))
  for (r in 1:top){
    #eval(parse(text=paste0("k_top_",top,"_temp_Accuracy <- c(k_top_",top,"_temp_Accuracy,k",Accuracy_sorted_temp$k_set[r],")")))
    eval(parse(text=paste0("k_top_",top,"_temp_Accuracy <- c(k_top_",top,"_temp_Accuracy,as.numeric(k",Accuracy_sorted_temp$k_set[r],"))")))
    }
  print("Accuracy generated correctly")
  # Cumulative KNOCKOFF results
  KO_sub <- NULL
  for (j in 1:s) {
    eval(parse(text=paste0("KO_sub <- c(KO_sub,KO_selected_",j,")")))
  }
  # for (j in 1:s) {
  #   eval(parse(text=paste0("rm(KO_result_",j,")")))
  #   eval(parse(text=paste0("rm(KO_selected_",j,")")))
  # }
  
  # # GENERATE HISTOGRAM OF THE CUMULATIVE KNOCKOFF RESULTS FOR SINGLE EXPERIMENT
  x = KO_sub;
  #print("start KO_sub")
  if (identical(x, numeric(0))) {
    "KO empty"
  }   else {
    h_KO_sub=graphics::hist(x, plot = FALSE )
    h_KO_sub$density = h_KO_sub$counts/sum(h_KO_sub$counts)*100
    title_temp <- c("KNOCKOFF FILTER RESULTS")
    #plot(h_KO_sub,breaks=seq(min(x)-0.5, max(x)+0.5, by=1),freq=FALSE,ylab='Density (%)',xlab='Feature #',
    #     main = title_temp,ylim=c(0,max(h_KO_sub$density)))
  }
  print("KO generated correctly")
  # GENERATE DATA FOR HISTOGRAM OF THE TOP # OF COVARIATES - MSE
  #print("start h_MSE")
  eval(parse(text=paste0("x_hist = k_top_",top,"_temp")))
  h_MSE=graphics::hist(x_hist, plot = FALSE ,breaks=seq(min(x_hist)-0.5, max(x_hist)+0.5, by=1))
  h_MSE$density = h_MSE$counts/sum(h_MSE$counts)*100

  # GENERATE DATA FOR HISTOGRAM OF THE TOP # OF COVARIATES - ACCURACY
  #print("start h_accuracy")
  eval(parse(text=paste0("x_hist = k_top_",top,"_temp_Accuracy")))
  h_Accuracy=graphics::hist(x_hist, plot = FALSE ,breaks=seq(min(x_hist)-0.5, max(x_hist)+0.5, by=1))
  h_Accuracy$density = h_Accuracy$counts/sum(h_Accuracy$counts)*100

    
  # RETRIEVE AND SAVE THE LABELS OF THE TOP [max_covs] FEATURES
  Top_features <- NULL
  eval(parse(text=paste0("Top_features=sort(table(k_top_",top,"_temp_Accuracy), decreasing = TRUE)")))
  eval(parse(text=paste0("Top_features_MSE=sort(table(k_top_",top,"_temp), decreasing = TRUE)")))
  
  qa <-as.data.frame(Top_features[1:max_covs])
  qa_MSE <-as.data.frame(Top_features_MSE[1:max_covs])
  
  names(qa) <- c("Accuracy","Acc_Frequency")
  names(qa_MSE) <- c("MSE","MSE_Frequency")
  
  qa$Density <- 100*(qa$Acc_Frequency/sum(Top_features))
  qa_MSE$Density <- 100*(qa_MSE$MSE_Frequency/sum(Top_features_MSE))
  
  qa_ALL <- cbind(qa,qa_MSE)
  print(dim(qa_ALL))
  
filename_qa_ALL <- file.path(workspace_directory,paste0(label,"_qa_ALL.RData"))
  save(label,workspace_directory,M,misValperc,min_covs,max_covs,
       top,qa_ALL,algorithm_list,qa_ALL, file = filename_qa_ALL)
  
  
  message("Learning/Training Table with Top features")
  if (identical(KO_sub, numeric(0))) {
    names(qa_ALL) <- c("Accuracy","Count","Density","MSE","Count","Density")
    print(qa_ALL[1:max_covs,], right = FALSE, row.names = FALSE)
  }   else {
    Top_Knockoff_features=sort(table(KO_sub), decreasing = TRUE)
    Top_Knockoff_features_labels <- as.numeric(names(Top_Knockoff_features)[1:max_covs])
    qa_ALL$Knockoff <- Top_Knockoff_features_labels
    qa_ALL$KO_Count <- Top_Knockoff_features[1:max_covs]
    qa_ALL$KO_Density <- 100*(Top_Knockoff_features[1:max_covs]/sum(Top_Knockoff_features))
    names(qa_ALL) <- c("Accuracy","Count","Density","MSE","Count","Density","Knockoff","Count","Density")
    print(qa_ALL[1:max_covs,], right = FALSE, row.names = FALSE)
  }
  CBDA_object_Training <- NULL
#  qa_ALL$MSE <- as.numeric(as.character(qa_ALL$MSE[1:dim(qa_ALL)[1]]))-1
#  qa_ALL$Accuracy <- as.numeric(as.character(qa_ALL$Accuracy[1:dim(qa_ALL)[1]]))-1
  CBDA_object_Training[[2]] <- m_training
  CBDA_object_Training[[3]] <- algorithm_list

  names(CBDA_object_Training)[1] <- c("LearningTable")
  names(CBDA_object_Training)[2] <- c("SuperLearnerCoefficients")
  names(CBDA_object_Training)[3] <- c("SuperLearnerLibrary")

  filename_specs <- file.path(workspace_directory,paste0(label,"_validation_info.RData"))
#  save(label,workspace_directory,M,range_k,range_n,misValperc,
#       Nrow_min,Nrow_max,N_cores,Kcol_min,Kcol_max,min_covs,max_covs,
#       top,alpha,q,Xtemp,Ytemp,qa_ALL,algorithm_list, file = filename_specs)
  save(label,workspace_directory,M,misValperc,min_covs,max_covs,
       top,qa_ALL,algorithm_list, file = filename_specs)
  
  
#  filename <- file.path(workspace_directory,
#                        paste0("CBDA_SL_M",M,"_miss",misValperc,"_n",range_n,
#                               "_k",range_k,"_Light_",label,".RData"))
  filename <- file.path(workspace_directory,
                        paste0("CBDA_M",M,"_miss",misValperc,"_",label,".RData"))
  save(list = ls(all.names = TRUE), file = filename)
  
  #eval(parse(text=paste0("save(list = ls(all.names = TRUE),
  #    file= \"",workspace_directory,"/CBDA_SL_M",M,"_miss",misValperc,"_n",range_n,
  #                       "_k",range_k,"_Light_",label,".RData\")")))
  
  # This loop cleans up all the first M learning/training subsamples
  for (s in 1:M) {
    #filename <- file.path(workspace_directory,
    #                      paste0("CBDA_SL_M",M,"_miss",misValperc,"_n",range_n,
    #                             "_k",range_k,"_Light_",s,"_",label,".RData"))
    filename <- file.path(workspace_directory,
                          paste0("CBDA_M",M,"_miss",misValperc,"_",s,"_",label,".RData"))
    file.remove(filename)
  }
  # I might need to add the "/" before label (paste0("/",label,"_validation_info.RData"))
  
  ## The next chunk generates the text files to be used by the
  ## postProcessing validation chunks
  for (i in min_covs:max_covs)
  {
    k_temp <- qa_ALL$MSE[1:i]
    #write.csv(k_temp)
    filename <- file.path(workspace_directory,paste0("newk",i,".txt"))
    k_temp1 <- paste0(as.numeric(levels(k_temp)[k_temp]), collapse=",")
    write.table(k_temp1,file=filename,row.names = FALSE,col.names = FALSE,sep = ",", eol = "\r\n" ,
                fileEncoding = "UTF-8", quote = FALSE)
  }
  
  cat("Consolidated workspace successfully created.\n\n")
  cat("Subsample workspaces successfully deleted.\n\n")
  cat("Consolidation completed successfully !!\n\n")
  return()
}

filename_specs <- file.path(workspace_directory,paste0(label,"_info.RData"))
print(filename_specs)
load(filename_specs)
#workspace_directory <- "/ifs/loni/ccb/collabs/2016/CBDA_SL_2016/2018/Simeone/Binomial_10k_1k/"

## Here I check if any job failed, before launchng the consolidation step
#filename1 <- paste0("CBDA_M",M,"_miss",misValperc,"_")
#filename1 <- file.path(workspace_directory,paste0("CBDA_M",M,"_miss",misValperc,"_"))
filename1 <- paste0("CBDA_M",M,"_miss",misValperc,"_")
filename2 <- paste0("_",label,".RData")
print(filename1)
print(filename2)
pattern_temp = paste0("*",filename2) 
#w0=list.files(path = workspace_directory , pattern = "*Binomial_10k_1k_300_NEW.RData")
w0=list.files(path = workspace_directory , pattern = pattern_temp)
print(w0)

jobID_temp <- NULL
for (i in 1:length(w0))
{
  # print(w0[i])
  test1=sub(x=w0[i],filename1,"")
  # print(test1)
  # print(filename2)
  test2=sub(x=test1,filename2,"")
  jobID_temp=c(jobID_temp,test2)
}

#M=30
failedJobs <- NULL
print(jobID_temp)
print(M)
failedJobs <- which(seq(1,as.numeric(M),1) %in% as.numeric(jobID_temp)==FALSE)
#print(failedJobs)
filename_failedJobs <- file.path(workspace_directory,'/Results/failedJobs.txt')
#print(filename_failedJobs)
## This checks if it is the 2nd time (at least) that the training has been restarted
## with an existing failedJobs.txt file in the directory. If that is the case,
## the existing file failedJobs.txt is deleted and recreated, if there's any failed job
if (length(list.files(path=workspace_directory,pattern="/Results/failedJobs.txt")>0))
    {
     cat("Removing the existing file: failedJobs.txt \n\n")
     file.remove(filename_failedJobs) 
}
## This should be empty
list.files(path=workspace_directory,pattern="/Results/failedJobs.txt")

if (length(failedJobs)>0)
    { 
    cat("Some jobs failed !! Recreate the  file: failedJobs.txt \n\n")
    write.table(as.matrix(failedJobs),file=filename_failedJobs,row.names = FALSE,col.names = FALSE,sep = ",", eol = "\r\n" ,
              fileEncoding = "UTF-8", quote = FALSE)
  # If I want to save it as a row
  #write.table(as.matrix(t(failedJobs)),file="failedJobs.txt",row.names = FALSE,col.names = FALSE,sep = ",", eol = "\r\n" ,
  #            fileEncoding = "UTF-8", quote = FALSE)
    stop(paste0("IMPORTANT: ", length(failedJobs), " jobs failed !!\n See failedJobs.txt for details."))
    }

## I need to find a way to pass this list of failed jobs to LONI as a generator on new
## training jobs

print("All jobs completed successfully !!  Consolidation can start !!")
cat("CONSOLIDATION STEP HAS STARTED !!\n\n")
CBDA_Consolidation.pipeline_LG(M=M,top=top,max_covs=max_covs,min_covs = min_covs , 
                               label = label, workspace_directory = workspace_directory)
