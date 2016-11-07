# #!/usr/bin/env Rscript
# args = commandArgs(trailingOnly=TRUE)
# #print(args)
# arg_file = as.array(args[1])
# print(arg_file)
# # dataset_file = as.array(args[2])
# # #print(dataset_file)
#  workspace_directory=as.array(args[3])
# print(workspace_directory)
#i_exp=as.numeric(args[5])
#print(i_exp)



#workspace_directory<-setwd("U:/SOCR/CBDA-SL/Laptop/Dump/TestOct25/")
#arg_file = as.array('U:/SOCR/CBDA-SL/Cranium/arg_parallelNEW.txt')

#dev.off()
#pdf("CBDA-SL-Cranium_TEST_Parallelized_Results_v1.pdf")
#eval(parse(text=paste0("pdf(\"",workspace_directory,"CBDA-SL-Cranium_TEST_Parallelized_Results_TODAY.pdf\")"))) # Windows
#arg_file = as.array('C:/Users/simeonem/CBDA-SL/Cranium/arg_test1.txt')

#arg_file = as.array('C:/Users/simeonem/CBDA-SL/Laptop/arg_test1_NEW.txt')
#i=as.numeric(args[2]) # to be replaced by a for loop
#dataset_file = as.array(args[3])
#dataset_file = as.array('C:/Users/simeonem/CBDA-SL/Cranium/NeuroIm.txt')
#workspace_directory<-setwd('C:/Users/simeonem/CBDA-SL/Cranium')
#workspace_directory<-setwd('C:/Users/simeonem/CBDA-SL/Laptop')

## Read the input arguments and the dataset from a file
#eval(parse(text=paste0("arguments <- read.table(arg_file, header = TRUE)")))
#print(arguments)

eval(parse(text=paste0("load(\"~/MSE_temp.RData\")")))
#eval(parse(text=paste0("save(arguments,workspace_directory,i,file= \"~/MSE_temp.RData\")")))

top=3;
#w1<-Sys.Date()
eval(parse(text=paste0("k_top_",top,"_ALL <- NULL")))
eval(parse(text=paste0("save(k_top_",top,"_ALL,file= \"",workspace_directory,"/k_top_",top,"_ALL_Light.RData\")"))) # Windows
#eval(parse(text=paste0("save(k_top_",top,"_ALL,file= \"~/k_top_",top,"_ALL_Light.RData\")"))) # Windows
#eval(parse(text=paste0("save(k_top_",top,"_ALL,file= \"",workspace_directory,"\\",w1,"/k_top_",top,"_ALL_Light.RData\")"))) # Windows
#eval(parse(text=paste0("load(\"",workspace_directory,"\\",w1,"/k_top_",top,"_ALL_Light.RData\")")))  # Unix
#"\\" means backslash  ,"\""

#save (myobject1,myobject2,etc.,file="mypath/myworkspace")
for (i in 1:dim(arguments)[1]) {
  print(i)
  # arg_file = as.array('C:/Users/simeonem/CBDA-SL/Laptop/arg_test1_NEW.txt')
  # dataset_file = as.array('C:/Users/simeonem/CBDA-SL/Cranium/NeuroIm.txt')
  # workspace_directory<-setwd('C:/Users/simeonem/CBDA-SL/Laptop')
  
  ## new set of arguments
  #arg_file = as.array(args[1])
  #dataset_file = as.array(args[2])
  #workspace_directory=as.array(args[3])
  #load('C:/Users/simeonem/CBDA-SL/Laptop/Dump/Parallel/pipelnvt/CBDA_SL_M8_miss10_n100_100_k5_15_Light.RData')
  ## Read the input arguments and the dataset from a file
  #eval(parse(text=paste0("arguments <- read.table(arg_file, header = TRUE)")))
  #arguments <- read.table("C:/Users/simeonem/CBDA-SL/Cranium/arg_parallel.txt", header = TRUE)
  #eval(parse(text=paste0("NeuroIm1 = read.table(dataset_file, header = TRUE)")))
  M <-arguments[i,1]
  misValperc <- arguments[i,2]
  Kcol_min <- arguments[i,3]
  Kcol_max <- arguments[i,4]
  Nrow_min <- arguments[i,5]
  Nrow_max <- arguments[i,6]
  range_n <- eval(parse(text=paste0("c(\"",Nrow_min,"_",Nrow_max,"\")")))
  range_k <- eval(parse(text=paste0("c(\"",Kcol_min,"_",Kcol_max,"\")")))

  #w0<-list.files(pattern = "M5000")
  #print(w0)
  #w1<-Sys.Date()
  #eval(parse(text=paste0("load(\"",workspace_directory,"\\",w1,"/CBDA_SL_M",M,"_miss",misValperc,"_n",range_n,"_k",range_k,"_Light.RData\")")))  # Unix
  for (s in 1:M){
    eval(parse(text=paste0("load(\"",workspace_directory,"CBDA_SL_M",M,"_miss",misValperc,"_n",range_n,"_k",range_k,"_Light_",s,".RData\")")))
    #eval(parse(text=paste0("load(\"CBDA_SL_M",M,"_miss",misValperc,"_n",range_n,"_k",range_k,"_Light_",s,".RData\")")))
    #eval(parse(text=paste0("load(\"",workspace_directory,"CBDA_SL_M10_miss0_n60_80_k15_30_Light_",s,".RData\")")))
  }
     

  #eval(parse(text=paste0("load(\"",workspace_directory,"/CBDA_SL_M",M,"_miss",misValperc,"_n",range_n,"_k",range_k,"_Light.RData\")")))
  #eval(parse(text=paste0("load(\"",workspace_directory,"/CBDA_SL_M",M,"_miss",misValperc,"_n",range_n,"_k",range_k,"_Light.RData\")")))
  #workspace_directory<-setwd('C:/Users/simeonem/CBDA-SL/Laptop')
 #eval(parse(text=paste0("save(Xnew,M,top,args,file= \"",workspace_directory,"/temp.RData\")")))
 eval(parse(text=paste0("save(Xnew,top,M,arguments,workspace_directory,i,file= \"~/temp.RData\")")))
  
  #Ynew = NeuroIm1_Final_AD_vs_NC_training$Group[q];
  
  
  ## STEP 7 - GENERATION AND RANKING OF MSE
  ## Generate all the MSE for each prediction obiect returned by the CBDA-SL algorithm
  ## MSE obtained by looping through the predictions made on the external dataset of q patients
  ## using the SuperLearner prediction algorithm output [SL_Pred_j], with the Xnew 
  ## matrix of the appropriate subset of covariates kj
 
  ## MSE GENERATION
  sum=0
  for (j in 1:M) {
    for(s in 1:length(Ynew)) {
      ## this checks first if the prediction object SL_Pred exists. If it does, the MSE is calculated
      ## If the SL_Pred does NOT exist, a dummy value is assigned to its MSE (very high)
      #eval(parse(text=paste0("ifelse(exists(\"SL_Pred_",j,"\"),sum <- sum(Ynew[",s,"] - SL_Pred_",j,"$pred[",s,"])^2,
      #                       ifelse(!exists(\"SL_Pred_",j,"\"),sum <- 100))")))}
      #eval(parse(text=paste0("ifelse(SL_Pred_",j," == 100,
      #                 sum <- 100,sum <- sum(Ynew[",s,"] - SL_Pred_",j,"$pred[",s,"])^2)")))}
    #print(SL_Pred_1)
    eval(parse(text=paste0("ifelse(typeof(SL_Pred_",j,") != \"double\",
      sum <- sum(Ynew[",s,"] - SL_Pred_",j,"$pred[",s,"])^2,sum <- SL_Pred_",j,")")))}

    ## this step makes the final calculation of the MSE and it labels it with a j --> MSE_j
    eval(parse(text=paste0("MSE_",j," <- sum/length(Ynew)")))
    sum = 0;
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
  #MSE_sorted_temp <- setorder(MSE_temp, mse,-k_set)
  #MSE_sorted_temp <- sort(MSE_temp, mse,-k_set)
  MSE_sorted_temp <- MSE_temp[order(MSE_temp$mse),]
  
  ## DEFINE HERE THE TOP # OF COVARIATES TO LIST in the MODEL MINING STEP
  # ""top" is defined at the beginning (line 14) and represents the top MSEs to consider for feature mining (ks).
  # Each one will have a set of best features with their relative highest frequencies
  eval(parse(text=paste0("k_top_",top,"_temp <- NULL")))
  for (r in 1:top){
    eval(parse(text=paste0("k_top_",top,"_temp <- c(k_top_",top,"_temp, k",MSE_sorted_temp$k_set[r],")")))
    }

  # GENERATE HISTOGRAM OF THE TOP # OF COVARIATES FOR SINGLE EXPERIMENT
  eval(parse(text=paste0("x = k_top_",top,"_temp")))
  #h = hist(x)
  eval(parse(text=paste0("h = hist(x)")))#,breaks=seq(min(k_top_",top,"_temp)-0.5,max(k_top_",top,"_temp)+0.5, by=1))")))
  #eval(parse(text=paste0("hist(k_top_",top,"_temp, breaks=seq(min(k_top_",top,"_temp)-0.5, max(k_top_",top,"_temp)+0.5, by=1))")))
  eval(parse(text=paste0("hist(x, breaks=seq(min(x)-0.5, max(x)+0.5, by=1))")))
  h$density = h$counts/sum(h$counts)*100
  title_temp <- c(M,misValperc,range_n,range_k)
  print(title_temp)
  plot(h,freq=FALSE,ylab='Density (%)',xlab='Feature #',main = title_temp)
  #readline("Press <return to continue")
  
  ## This is where I generate the list of the top features across different CBDA-SL experiments
  ## Load the k_top_ALL workspace
  #w1<-Sys.Date()
  #eval(parse(text=paste0("load(\"",workspace_directory,"\\",w1,"/k_top_",top,"_ALL_Light.RData\")")))  # Unix
  eval(parse(text=paste0("load(\"",workspace_directory,"/k_top_",top,"_ALL_Light.RData\")")))  # Unix
  #eval(parse(text=paste0("load(\"~/k_top_",top,"_ALL_Light.RData\")")))  # Unix
  #eval(parse(text=paste0("load(\"",workspace_directory,"/k_top_",top,"_ALL_Light.RData\")")))
  ## Append the k_top_temp from the current workspace to the k_top_ALL
  eval(parse(text=paste0("k_top_",top,"_ALL <- c(k_top_",top,"_ALL,k_top_",top,"_temp)")))
  ## Save the k_top_ALL workspace
  #eval(parse(text=paste0("save(k_top_",top,"_ALL,file= \"",workspace_directory,"\\",w1,"/k_top_",top,"_ALL_Light.RData\")")))  # Unix
  eval(parse(text=paste0("save(k_top_",top,"_ALL,file= \"",workspace_directory,"/k_top_",top,"_ALL_Light.RData\")"))) # Windows
  #eval(parse(text=paste0("save(k_top_",top,"_ALL,file= \"~/k_top_",top,"_ALL_Light.RData\")"))) # Windows
  #eval(parse(text=paste0("save(k_top_",top,"_ALL,file= \"",workspace_directory,"/k_top_",top,"_ALL_Light.RData\")")))
  
  # RETRIEVE AND SAVE THE LABELS OF THE TOP [BEST] FEATURES 
  BEST=6;
  eval(parse(text=paste0("Top_features=sort(table(k_top_",top,"_temp), decreasing = TRUE)")))
  Top_features_labels <- names(Xnew)[as.numeric(names(Top_features))]
  eval(parse(text=paste0("Top_",BEST,"_features_labels <- names(Xnew)[as.numeric(names(Top_features)[1:",BEST,"])]")))
  
  eval(parse(text=paste0("print(\"CBDA_SL_M",M,"_miss",misValperc,"_n",range_n,"_k",range_k,"_Light.RData\")")))
  eval(parse(text=paste0("print(Top_",BEST,"_features_labels)")))
  eval(parse(text=paste0("print(Top_features[1:",BEST,"])")))
  
  ## Save the workspace previously loaded with all the experiments + the feature mining variables
  #w1<-Sys.Date()
  eval(parse(text=paste0("save.image(\"",workspace_directory,"/Top_",BEST,"_features_labels_M",M,"_miss",misValperc,"_n",range_n,"_k",range_k,"_Light.RData\")")))  # Unix
  #eval(parse(text=paste0("save.image(\"~/Top_",BEST,"_features_labels_M",M,"_miss",misValperc,"_n",range_n,"_k",range_k,"_Light.RData\")")))  # Unix
  #eval(parse(text=paste0("save.image(\"",workspace_directory,"/Top_",BEST,"_features_labels_M",M,"_miss",misValperc,"_n",range_n,"_k",range_k,"_Light.RData\")")))
  
  ## Clear the workspace to free memory and allow faster computations
  rm(list = ls())
  ## Read the input arguments again and the dataset from a file
  eval(parse(text=paste0("load(\"~/temp.RData\")")))
}

## HISTOGRAM OF THE TOP FEATURES ACROSS ALL THE CBDA-SL EXPERIMENTS
## Load the k_top_ALL workspace
#workspace_directory<-setwd('C:/Users/simeonem/CBDA-SL/Laptop')
eval(parse(text=paste0("load(\"~/temp.RData\")")))
#workspace_directory=as.array(args[4])
#eval(parse(text=paste0("load(\"",workspace_directory,"/temp.RData\")")))

#eval(parse(text=paste0("load(\"",workspace_directory,"/k_top_",top,"_ALL_Light.RData\")")))
#w1<-Sys.Date()
#eval(parse(text=paste0("load(\"",workspace_directory,"\\",w1,"/k_top_",top,"_ALL_Light.RData\")")))  # Unix
eval(parse(text=paste0("load(\"",workspace_directory,"/k_top_",top,"_ALL_Light.RData\")")))  # Unix
#eval(parse(text=paste0("load(\"~/k_top_",top,"_ALL_Light.RData\")")))  # Unix

# Histogram of k_top_ALL
eval(parse(text=paste0("x = k_top_",top,"_ALL")))
h = hist(x)
#eval(parse(text=paste0("h = hist(k_top_",top,"_ALL,breaks=seq(min(k_top_",top,"_temp)-0.5,max(k_top_",top,"_temp)+0.5, by=1))")))
eval(parse(text=paste0("h = hist(x,breaks=seq(min(x)-0.5,max(x)+0.5, by=1))")))
#eval(parse(text=paste0("h = hist(x)")))#,breaks=seq(min(k_top_",top,"_ALL)-0.5,max(k_top_",top,"_ALL)+0.5, by=1))")))
h$density = h$counts/sum(h$counts)*100
title_temp_ALL <- c(M,"ALL")
print(title_temp_ALL)
plot(h,freq=FALSE,ylab='Density (%)',xlab='Feature #',main = title_temp_ALL)

# RETRIEVE AND SAVE THE LABELS OF THE TOP [BEST] FEATURES ACROSS ALL THE CBDA-SL EXPERIMENTS
BEST=6;
eval(parse(text=paste0("Top_features_ALL=sort(table(k_top_",top,"_ALL), decreasing = TRUE)")))
Top_features_labels_ALL <- names(Xnew)[as.numeric(names(Top_features_ALL))]
eval(parse(text=paste0("Top_",BEST,"_features_labels_ALL <- names(Xnew)[as.numeric(names(Top_features_ALL)[1:",BEST,"])]")))

eval(parse(text=paste0("print(Top_",BEST,"_features_labels_ALL)")))
eval(parse(text=paste0("print(Top_features_ALL[1:",BEST,"])")))

## Save the k_top_ALL workspace with all the Top Features labeled
#eval(parse(text=paste0("save(k_top_",top,"_ALL,file= \"",workspace_directory,"/k_top_",top,"_ALL_Light.RData\")")))
#eval(parse(text=paste0("save(k_top_",top,"_ALL,file= \"",workspace_directory,"\\",w1,"/k_top_",top,"_ALL_Light.RData\")")))  # Unix
eval(parse(text=paste0("save(k_top_",top,"_ALL,file= \"",workspace_directory,"/k_top_",top,"_ALL_Light.RData\")")))  # Unix
#eval(parse(text=paste0("save(k_top_",top,"_ALL,file= \"~/k_top_",top,"_ALL_Light.RData\")")))  # Unix



# ## The section of the R script below uses the CV.Superlearner objects to generate 
# ## V-fold Cross Validation Risk Estimate confidence intervals for each algorithm 
# ## in the SLlibrary, including the Superlearner. It loads each CV_SL objects, plots 
# ## a summary of the V-fold CV Risk Estimates, lists and saves the best algorithm 
# ## for each validation folder and then re-saves the workspaces. This section can be 
# ## added as a separatepost-optimization step.
# rm(list = ls())
# eval(parse(text=paste0("load(\"~/temp.RData\")")))
# # line 79 eval(parse(text=paste0("save(Xnew,top,M,arguments,workspace_directory,i,file= \"~/temp.RData\")")))
# 
# ## First loop is through the experiments
# for (s in 1:dim(arguments)[1]){
#   for (j in 1:M){
#     eval(parse(text=paste0("load(\"~/temp.RData\")")))
#     misValperc <- arguments[s,2]
#     Kcol_min <- arguments[s,3]
#     Kcol_max <- arguments[s,4]
#     Nrow_min <- arguments[s,5]
#     Nrow_max <- arguments[s,6]
#     range_n <- eval(parse(text=paste0("c(\"",Nrow_min,"_",Nrow_max,"\")")))
#     range_k <- eval(parse(text=paste0("c(\"",Kcol_min,"_",Kcol_max,"\")")))
#     eval(parse(text=paste0("load(\"",workspace_directory,"/CBDA_CV_SL_M",M,"_miss",misValperc,"_n",range_n,"_k",range_k,"_",j,".RData\")")))
#     # Evaluate SuperLearner prediction model
#     eval(parse(text=paste0("pdf(\"",workspace_directory,"/CBDA-SL-Cranium_TEST_Parallelized_Results_v2.pdf\")"))) # Windows
#     eval(parse(text=paste0("summary(CV_SL_",j,")")))
#     eval(parse(text=paste0("plot(CV_SL_",j,", package = \"ggplot2\")")))
#     #summary(pm1)
#     #plot(pm1, packag ="ggplot2")
#     
#     #Best algorithm for each V-fold
#     eval(parse(text=paste0("CV_SL_",j,"$whichDiscreteSL")))
#     eval(parse(text=paste0("CV_SL_",j,"_best_algorithms <- unlist(CV_SL_",j,"$whichDiscreteSL)")))
#     eval(parse(text=paste0("CV_SL_",j,"_best_algorithms <- table(CV_SL_",j,"_best_algorithms)")))
#     #eval(parse(text=paste0("save.image(\"",workspace_directory,"/CBDA_CV_SL_M",M,"_miss",misValperc,"_n",range_n,"_k",range_k,"_",s,".RData\")")))
#     
#     eval(parse(text=paste0("save(CV_SL_",j,"_best_algorithms,file= \"",workspace_directory,"/CBDA_CV_SL_M",M,"_miss",misValperc,"_n",range_n,"_k",range_k,"_",j,"_BEST_ALGORITHMS.RData\")")))  # Unix
#     #a1<-unlist(CV_SL_2$whichDiscreteSL)
#     #pm1$whichDiscreteSL
#     eval(parse(text=paste0("save(Xnew,top,M,arguments,workspace_directory,s,file= \"~/temp.RData\")")))
#     
#     ## Clear the workspace to free memory and allow faster computations
#     rm(list = ls())
#   }
#   eval(parse(text=paste0("load(\"~/temp.RData\")")))
#   s <- s+1
#   eval(parse(text=paste0("save(Xnew,top,M,arguments,workspace_directory,s,file= \"~/temp.RData\")")))
# }
# 
# ## I want to add here a loop that loads all the "best_algorithms" across all the iterations
# ## first (j_global) and then across experiments (i), thus to inform the estimation module
# ## for a more detailed and specialized analysis (i.e., the ESTIMATION MODULE)
# 
# #dev.off()
