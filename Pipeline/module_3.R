#!/usr/bin/env Rscript



#print(arg_file)
#eval(parse(text=paste0("arguments = read.table(arg_file, header = TRUE)")))
#eval(parse(text=paste0("NeuroIm1 = read.table(dataset_file, header = TRUE)")))

for(i in 1:2){
  args = commandArgs(trailingOnly=TRUE)
  arg_file = as.array(args[1])
  dataset_file = as.array(args[2])
  workspace_directory=as.array(args[3])
  eval(parse(text=paste0("arguments = read.table(arg_file, header = TRUE)")))
  eval(parse(text=paste0("NeuroIm1 = read.table(dataset_file, header = TRUE)")))
  M <-arguments[i,1]
  misValperc <- arguments[i,2]
  Kcol_min <- arguments[i,3]
  Kcol_max <- arguments[i,4]
  Nrow_min <- arguments[i,5]
  Nrow_max <- arguments[i,6]
  range_n <- eval(parse(text=paste0("c(\"",Nrow_min,"_",Nrow_max,"\")")))
  range_k <- eval(parse(text=paste0("c(\"",Kcol_min,"_",Kcol_max,"\")")))
  #print(arg_file)
  #print(workspace_directory)
  #eval(parse(text=paste0("load(\"~/CBDA-SL/CBDA_SL_M",M,"_miss",misValperc,"_n",range_n,"_k",range_k,"_Complete.RData\")")))
  eval(parse(text=paste0("load(\"",workspace_directory,"CBDA_SL_M",M,"_miss",misValperc,"_n",range_n,"_k",range_k,"_Complete.RData\")")))
  #str1 <-paste0("load(\"~/CBDA-SL/CBDA_SL_M2_miss30_n60_80_k15_30_Complete.RData\")")
  #print(parse(text=str1))
  #eval(str1)
  Ynew = NeuroIm1_Final_AD_vs_NC_training$Group[q];
  sum=0
  for (j in 1:M) {
    for(i in 1:length(Ynew)) {
      try(eval(parse(text=paste0("sum <- sum(Ynew[",i,"] - SL_Pred_",j,"$pred[",i,"])^2"))))
    } 
    try(eval(parse(text=paste0("MSE_",j," <- sum/length(Ynew)"))))
    sum = 0;
  }
  ## STEP 7 - RANKING MSE
  ## MSE obtained by looping through the predictions made on the external dataset of q patients
  ## using the SuperLearner prediction algorithm output [SL_Pred_j], with the Xnew 
  ## matrix of the appropriate subset of covariates kj
  #  GENERATING THE ARRAY OF MSE FOR ALL THE M SL OBJECTS
  MSE=0;
  for (j in 1:M) {
    eval(parse(text=paste0("MSE[j] <- MSE_",j)))
  }
  
  # HISTOGRAMS
  #for (s in seq(10,M,M/10)){
  s=M;
  MSE_temp <- NULL
  MSE_sorted_temp <- NULL
  #MSE_temp <- data.table(mse=MSE[1:s],rank=1:s)
  MSE_temp <- data.frame(mse=MSE[1:s],rank=1:s)
  #MSE_sorted_temp <- setorder(MSE_temp, mse,-rank)
  #MSE_sorted_temp <- sort(MSE_temp, mse,-rank)
  MSE_sorted_temp <- MSE_temp[order(MSE_temp$mse),]
  ## DEFINE HERE THE TOP # OF COVARIATES TO LIST in the MODEL MINING STEP
  top = 2;
  eval(parse(text=paste0("k_top_",top,"_temp <- NULL")))
  for (i in 1:top){
    eval(parse(text=paste0("k_top_",top,"_temp <- c(k_top_",top,"_temp, k",MSE_sorted_temp$rank[i],")")))
    #}
    ## STEP 8 - MODEL MINING (HISTOGRAMS OF TOP 20 or 50 COVARIATES)
    # GENERATE HISTOGRAMS OF THE TOP # OF COVARIATES
    eval(parse(text=paste0("x = k_top_",top,"_temp")))
    #h = hist(x)
    eval(parse(text=paste0("h = hist(k_top_",top,"_temp,breaks=seq(min(k_top_",top,"_temp)-0.5,max(k_top_",top,"_temp)+0.5, by=1))")))
    #eval(parse(text=paste0("hist(k_top_",top,"_temp, breaks=seq(min(k_top_",top,"_temp)-0.5, max(k_top_",top,"_temp)+0.5, by=1))")))
    h$density = h$counts/sum(h$counts)*100
    plot(h,freq=FALSE,ylab='Density (%)',xlab='Covariate #')
    #:readline("Press <return to continue")
  }
  
  # RETRIEVE THE LABEL OF THE MORE FREQUENTLY SELECTED COVARIATES, echo=TRUE, message=FALSE, warning=FALSE}
  # WITHIN THE TOP # OF COVARIATES IN THE PREDICTIONS
  eval(parse(text=paste0("aqw <- data.frame(table(k_top_",top,"_temp))")))
  aqw_ranked <- aqw[order(aqw$Freq),]
  
  t1 = tail(aqw_ranked,top)
  #t1=as.integer(t1[[1]])
  ind_labels <- NULL
  for (i in 1:top){
    ind_labels = c(ind_labels,as.integer(t1$k_top_10_temp[i]))
  }
  names(Xtemp)[ind_labels]
  
  # This directory needsto be changed to reflect an existing directory OR just use the following for LONI pipeline
  
  # RETRIEVE AND SAVE THE LABELS OF THE TOP [BEST] FEATURES 
  TOP=top;
  BEST=5;
  eval(parse(text=paste0("Top_features <- data.frame(table(k_top_",TOP,"_temp))")))
  colnames(Top_features) <- c("Position","Freq")
  Top_features_ranking <- Top_features[order(Top_features$Freq, decreasing = TRUE),]
  a1=Top_features_ranking$Position[1:BEST]
  names(Xnew[a1]) 
  head(Top_features_ranking)
  eval(parse(text=paste0("TOP_",BEST,"_features_",M,"_miss",misValperc*100,"_n",range_n,"_k",range_k,"<-names(Xnew[a1])")))
  eval(parse(text=paste0("save.image(\"",workspace_directory,"CBDA_SL_M",M,"_miss",misValperc,"_n",range_n,"_k",range_k,"_Light.RData\")")))
  
  eval(parse(text=paste0("print(\"CBDA_SL_M",M,"_miss",misValperc,"_n",range_n,"_k",range_k,"_Complete.RData\")")))
  print(names(Xnew[a1]))
  print(head(Top_features_ranking))
  rm(list=ls())
  }
  