#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
print(args)
arg_file = as.array(args[1])
print(arg_file)
i_exp=as.numeric(args[2])
dataset_file = as.array(args[3])
workspace_directory=as.array(args[4])
#i_exp=as.numeric(args[5])
eval(parse(text=paste0("arguments = read.table(arg_file, header = TRUE)")))
label=c("ADNI_dataset_MN3")

eval(parse(text=paste0("save(arguments,label,workspace_directory,i_exp,file= \"~/temp_data_info_module2.RData\")")))
## LOADS BASIC INFO TO RUN THE CONSOLIDATION
eval(parse(text=paste0("load(\"~/temp_data_info_module2.RData\")")))

## OPENS A PDF FILE FOR RESULTS [OPTIONAL]
#eval(parse(text=paste0("pdf(\"",workspace_directory,"/CBDA-SL-TEST-output.pdf\")"))) # Windows

## This "top" parameter identifies the sets of features associated with the top MSEs
top=50;

## THIS STEP GENERATES EMPTY VARIABLES/WORKSPACE TO STORE THE
## "CUMULATIVE" RESULTS FOR ALL THE EXPERIMENTS (not enforced right now)
eval(parse(text=paste0("k_top_",top,"_ALL <- NULL")))
eval(parse(text=paste0("save(k_top_",top,"_ALL,file= \"",workspace_directory,"/k_top_",top,"_ALL_Light.RData\")"))) # Windows
#"\\" means backslash  ,"\""

## DATA CONSOLIDATION - load M RData workspaces [j_global] per experiment [i_exp]
##                      and consolidate them into 1 RData workspace
for (i in i_exp) {
  print(i)
  M <-arguments[i,1]
  misValperc <- arguments[i,2]
  Kcol_min <- arguments[i,3]
  Kcol_max <- arguments[i,4]
  Nrow_min <- arguments[i,5]
  Nrow_max <- arguments[i,6]
  range_n <- eval(parse(text=paste0("c(\"",Nrow_min,"_",Nrow_max,"\")")))
  range_k <- eval(parse(text=paste0("c(\"",Kcol_min,"_",Kcol_max,"\")")))
  print(c(M,misValperc,Kcol_min,Kcol_max,Nrow_min,Nrow_max))
  for (s in 1:M){
    eval(parse(text=paste0("load(\"",workspace_directory,"CBDA_SL_M",M,"_miss",misValperc,"_n",range_n,"_k",range_k,"_Light_",s,"_",label,".RData\")")))
  }
  
  ## THIS SAVES THE CONSOLIDATED WORKSPACE FOR EACH EXPERIMENT
  eval(parse(text=paste0("save.image(\"",workspace_directory,"CBDA_SL_M",M,"_miss",misValperc,"_n",range_n,"_k",range_k,"_Light_",label,".RData\")")))
  ## THIS SAVES A TEMP FILE AGAIN TO BE USED AFTER THE CURRENT WORKSPACE IS EMPTIED TO SPEED UP PROCESSING
  eval(parse(text=paste0("save(arguments,label,workspace_directory,i_exp,file= \"~/temp_data_info_module2.RData\")")))
  ## CLEARS THE WORKSPACE FOR FASTER PROCESSING
  rm(list = ls())
  # LOADS THE BASIC INFO TO RUN THE CONSOLIDATION ON THE NEXT EXPERIMENT
  eval(parse(text=paste0("load(\"~/temp_data_info_module2.RData\")")))
}

## STEP 7 - GENERATION AND RANKING OF MSE for each experiment
## Generate all the MSE for each prediction obJect returned by the CBDA-SL algorithm
## MSE obtained by looping through the predictions made on the external dataset of q patients
## using the SuperLearner prediction algorithm output [SL_Pred_j]

# LOADS THE BASIC INFO TO RUN THE CONSOLIDATION ON THE NEXT EXPERIMENT
eval(parse(text=paste0("load(\"~/temp_data_info_module2.RData\")")))
for (i in i_exp) {
  print(i)
  print(workspace_directory)
  print(arguments)
  M <-arguments[i,1]
  misValperc <- arguments[i,2]
  Kcol_min <- arguments[i,3]
  Kcol_max <- arguments[i,4]
  Nrow_min <- arguments[i,5]
  Nrow_max <- arguments[i,6]
  range_n <- eval(parse(text=paste0("c(\"",Nrow_min,"_",Nrow_max,"\")")))
  range_k <- eval(parse(text=paste0("c(\"",Kcol_min,"_",Kcol_max,"\")")))

  eval(parse(text=paste0("load(\"",workspace_directory,"CBDA_SL_M",M,"_miss",misValperc,"_n",range_n,"_k",range_k,"_Light_",label,".RData\")")))

  ## MSE GENERATION
  sum=0
  for (j in 1:M) {
    for(s in 1:length(Ypred)) {
      ## This checks first if the TYPE of the prediction object SL_Pred is NOT a double (which means that
      ## the prediction step worked in the previous module. If it is NOT a double, the MSE is calculated
      ## as the mean of the sum of the square of the difference between the prediction and the data to predict.
      # If the SL_Pred is a double, SL_Pred_j is assigned to its MSE (very high value).
      eval(parse(text=paste0("ifelse(typeof(SL_Pred_",j,") != \"double\",
                             sum <- sum+sum(Ypred[",s,"] - SL_Pred_",j,"$pred[",s,"])^2,sum <- SL_Pred_",j,")")))
      ## This step makes the final calculation of the MSE and it labels it with a j --> MSE_j
      eval(parse(text=paste0("MSE_",j," <- sum/length(Ypred)")))
      ## This resets the sum to 0 for the next MSE calculation
      sum = 0;
      }
    }

  #  GENERATING THE ARRAY OF MSE FOR ALL THE M SL OBJECTS
  MSE=0;
  for (j in 1:M) {
    eval(parse(text=paste0("MSE[j] <- MSE_",j)))
  }

  # MSE RANKING
  #for (s in seq(10,M,M/10)){
  s=M;
  MSE_temp <- NULL
  MSE_sorted_temp <- NULL
  MSE_temp <- data.frame(mse=MSE[1:s],k_set=1:s)
  MSE_sorted_temp <- MSE_temp[order(MSE_temp$mse),]

  ## DEFINE HERE THE TOP # OF COVARIATES TO LIST in the MODEL MINING STEP
  # "top" is defined at the beginning (line 8) and represents the top MSEs to consider for 
  # feature mining (ks). Each one will have a set of best features with their relative highest frequencies
  eval(parse(text=paste0("k_top_",top,"_temp <- NULL")))
  for (r in 1:top){
    eval(parse(text=paste0("k_top_",top,"_temp <- c(k_top_",top,"_temp, k",MSE_sorted_temp$k_set[r],")")))
  }

  # Cumulative KNOCKOFF results
  KO_sub <- NULL
  for (j in 1:M) {
    eval(parse(text=paste0("KO_sub <- c(KO_sub,KO_selected_",j,")")))
  }
  # GENERATE HISTOGRAM OF THE CUMULATIVE KNOCKOFF RESULTS FOR SINGLE EXPERIMENT
  x = KO_sub;
  eval(parse(text=paste0("h=hist(x, plot = FALSE ,breaks=seq(min(x)-0.5, max(x)+0.5, by=1))")))
  h$density = h$counts/sum(h$counts)*100
  title_temp <- c("Gaussian Test - Knockoff Filter",range_n,range_k)
  print(title_temp)
  plot(h,freq=FALSE,ylab='Density (%)',xlab='Feature #',main = title_temp,ylim=c(0,15))
  
  # GENERATE HISTOGRAM OF THE TOP # OF COVARIATES FOR SINGLE EXPERIMENT
  eval(parse(text=paste0("x = k_top_",top,"_temp")))
  eval(parse(text=paste0("h=hist(k_top_",top,"_temp, plot = FALSE ,breaks=seq(min(k_top_",top,"_temp)-0.5, max(k_top_",top,"_temp)+0.5, by=1))")))
  h$density = h$counts/sum(h$counts)*100
  title_temp <- c("Gaussian Test - CBDA-SL",range_n,range_k)
  print(title_temp)
  plot(h,freq=FALSE,ylab='Density (%)',xlab='Feature #',main = title_temp,ylim=c(0,2.5))

  # RETRIEVE AND SAVE THE LABELS OF THE TOP [BEST] FEATURES
  BEST=20;
  eval(parse(text=paste0("Top_features=sort(table(k_top_",top,"_temp), decreasing = TRUE)")))
  Top_features_labels <- names(Xnorm_sub)[as.numeric(names(Top_features))]
  eval(parse(text=paste0("Top_",BEST,"_features_labels <- names(Xnorm_sub)[as.numeric(names(Top_features)[1:",BEST,"])]")))
  eval(parse(text=paste0("print(Top_",BEST,"_features_labels)")))
  eval(parse(text=paste0("print(Top_features[1:",BEST,"])")))

  ## This saves the updated RData workspaces with all the results for the analysis and R Markdown reports
  eval(parse(text=paste0("save.image(\"",workspace_directory,"CBDA_SL_M",M,"_miss",misValperc,"_n",range_n,"_k",range_k,"_Light_",label,".RData\")")))
  ## This save the info needed for the next experiments (only used if multiple experiments are processed 
  ## in the Consolidation & Results module.
  #eval(parse(text=paste0("save(arguments,label,workspace_directory,i_exp,file= \"~/temp_data_info_module2.RData\")")))
  ## Cleans up the workspace
  rm(list = ls())
  ## This loads the info needed for the next experiment (only used if multiple experiments are processed 
  ## in the Consolidation & Results module.
  #eval(parse(text=paste0("load(\"~/temp_data_info_module2.RData\")")))
  #readline("Press <return to continue")
}

# This checks how many files are in the workspace directory, before clean up is performed.
w00<-list.files(pattern = "\\.RData$", full.names = TRUE)
print(length(w00))

## This next loop deletes all the "_Light_JOBID_label.RData" workspaces,
## and leave the consolidated one "_Light_label.RData". First loads the info.
eval(parse(text=paste0("load(\"~/temp_data_info_module2.RData\")")))
for (i in i_exp) {
  print(i)
  print(workspace_directory)
  print(arguments)
  M <-arguments[i,1]
  misValperc <- arguments[i,2]
  Kcol_min <- arguments[i,3]
  Kcol_max <- arguments[i,4]
  Nrow_min <- arguments[i,5]
  Nrow_max <- arguments[i,6]
  range_n <- eval(parse(text=paste0("c(\"",Nrow_min,"_",Nrow_max,"\")")))
  range_k <- eval(parse(text=paste0("c(\"",Kcol_min,"_",Kcol_max,"\")")))
  
  for (s in 1:M) {
    eval(parse(text=paste0("file.remove(\"",workspace_directory,"CBDA_SL_M",M,"_miss",misValperc,"_n",range_n,"_k",range_k,"_Light_",s,"_",label,".RData\")")))
  }
}
# This checks how many files are in the workspace directory, AFTER clean up is performed. It's an indirect check if the Consolidation
# module worked
w00<-list.files(pattern = "\\.RData$", full.names = TRUE)
print(length(w00))

# This is enforced in case a pdf channel has been opened at the beginning (line 5) 
# to save results in a pdf
#dev.off()
