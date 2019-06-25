# #!/usr/bin/env Rscript
# workspace_directory<-setwd("C:/Users/simeonem/Documents/CBDA-SL/ExperimentsNov2016/MRI/M1000/")
# arg_file = as.array('C:/Users/simeonem/Documents/CBDA-SL/Cranium/arg_Nov18_test.txt')
#
# ## Read the input arguments and the dataset from a file
# eval(parse(text=paste0("arguments <- read.table(arg_file, header = TRUE)")))
# eval(parse(text=paste0("save(arguments,workspace_directory,file= \"~/MSE_temp.RData\")")))

#pdf("U:/SOCR/CBDA-SL/ExperimentsNov2016/CBDA-SL-1000_MRIDataset_Results_v1.pdf")
#pdf("CBDA-SL-Cranium_TEST_Parallelized_Results_v1.pdf")
#arg_file = as.array('C:/Users/simeonem/CBDA-SL/Cranium/arg_test1.txt')
#workspace_directory <- ("/ifshome/pipelnvt/")
#eval(parse(text=paste0("save(arguments,workspace_directory,file= \"~/temp_KO.RData\")")))

## LINE 18 is for the Cranium pipeline implementation [PREVIOUS LINES can be disregarded]
## The file MSE_temp.RData has all the necessary info to proceed with data consolidation and analysis
## [arguments,workspace_directory]
#eval(parse(text=paste0("load(\"~/MSE_temp_Gaussian_Dec5.RData\")")))
#eval(parse(text=paste0("save(arguments,label,workspace_directory,file= \"~/temp_data_info.RData\")")))
eval(parse(text=paste0("load(\"~/temp_data_info.RData\")")))
#print(workspace_directory)
#print(arguments)

## OPENS A PDF FILE FOR RESULTS
#eval(parse(text=paste0("pdf(\"",workspace_directory,"/CBDA-SL-TEST-output.pdf\")"))) # Windows

## This "top" parameter identifies the sets of features associated with the top MSEs
top=50;
#w1<-Sys.Date()

## THIS STEP GENERATES EMPTY VARIABLES/WORKSPACE TO STORE THE
## "CUMULATIVE" RESULTS FOR ALL THE EXPERIMENTS (not enforced right now)
eval(parse(text=paste0("k_top_",top,"_ALL <- NULL")))
eval(parse(text=paste0("save(k_top_",top,"_ALL,file= \"",workspace_directory,"/k_top_",top,"_ALL_Light.RData\")"))) # Windows
#"\\" means backslash  ,"\""

## DATA CONSOLIDATION - load M RData workspaces [j_global] per experiment [i_exp]
##                      and consolidate them into 1 RData workspace
list_exps=1:dim(arguments)[1];
for (i in list_exps) {
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
    print(s)
    eval(parse(text=paste0("load(\"",workspace_directory,"CBDA_SL_M",M,"_miss",misValperc,"_n",range_n,"_k",range_k,"_Light_",s,"_Gaussian_Dec5.RData\")")))
  }

  ## THIS SAVES THE CONSOLIDATED WORKSPACE FOR EACH EXPERIMENT
  eval(parse(text=paste0("save.image(\"",workspace_directory,"CBDA_SL_M",M,"_miss",misValperc,"_n",range_n,"_k",range_k,"_Light_Gaussian_Dec5.RData\")")))
  #eval(parse(text=paste0("save.image(\"C:/Users/simeonem/Documents/CBDA-SL/ExperimentsNov2016/MRI/CBDA_SL_M",M,"_miss",misValperc,"_n",range_n,"_k",range_k,"_Light.RData\")")))
  ## THIS SAVES A TEMP FILE TO BE USED AFTER THE CURRENT WORKSPACE IS EMPTIED TO SPEED UP PROCESSING
  #eval(parse(text=paste0("save(Xnew,top,M,arguments,workspace_directory,i,file= \"~/temp.RData\")")))
  list_exps=1:dim(arguments)[1];
  #list_exps=list_exps[-1*c(1:18,20:30)]
  #list_exps=20;
  #  list_exps=list_exps[-1*c(6:30)]
  #eval(parse(text=paste0("save(arguments,workspace_directory,list_exps,file= \"~/temp_Gaussian_Dec5.RData\")")))
  eval(parse(text=paste0("save(arguments,label,workspace_directory,file= \"~/temp_data_info.RData\")")))
  rm(list = ls())
  #print(s)
  # eval(parse(text=paste0("load(\"~/temp_Gaussian_Dec5.RData\")")))
  eval(parse(text=paste0("load(\"~/temp_data_info.RData\")")))
  # print(workspace_directory)
  # print(arguments)
}

  #Ynew = NeuroIm1_Final_AD_vs_NC_training$Group[q];

  ## STEP 7 - GENERATION AND RANKING OF MSE for each experiment
  ## Generate all the MSE for each prediction obJect returned by the CBDA-SL algorithm
  ## MSE obtained by looping through the predictions made on the external dataset of q patients
  ## using the SuperLearner prediction algorithm output [SL_Pred_j], with the Xnew
  ## matrix of the appropriate subset of covariates kj
#print(s)
# eval(parse(text=paste0("load(\"~/temp_Gaussian_Dec5.RData\")")))
eval(parse(text=paste0("load(\"~/temp_data_info.RData\")")))
#eval(parse(text=paste0("load(\"~/MSE_temp_Gaussian_Dec5.RData\")")))
list_exps=1:dim(arguments)[1];
#list_exps=list_exps[-1*c(1:18,20:30)]
#list_exps=20;
#top=10;
for (i in list_exps) {
  print(i)
  print(workspace_directory)
  print(arguments)
  top=10
  M <-arguments[i,1]
  misValperc <- arguments[i,2]
  Kcol_min <- arguments[i,3]
  Kcol_max <- arguments[i,4]
  Nrow_min <- arguments[i,5]
  Nrow_max <- arguments[i,6]
  range_n <- eval(parse(text=paste0("c(\"",Nrow_min,"_",Nrow_max,"\")")))
  range_k <- eval(parse(text=paste0("c(\"",Kcol_min,"_",Kcol_max,"\")")))

  eval(parse(text=paste0("load(\"",workspace_directory,"CBDA_SL_M",M,"_miss",misValperc,"_n",range_n,"_k",range_k,"_Light_Gaussian_Dec5.RData\")")))
#top=10;
  ## MSE GENERATION
  sum=0
  for (j in 1:M) {
    for(s in 1:length(Ypred)) {
      ## this checks first if the prediction object SL_Pred exists. If it does, the MSE is calculated
      ## If the SL_Pred does NOT exist, a dummy value is assigned to its MSE (very high)
      #eval(parse(text=paste0("ifelse(exists(\"SL_Pred_",j,"\"),sum <- sum(Ynew[",s,"] - SL_Pred_",j,"$pred[",s,"])^2,
      #                       ifelse(!exists(\"SL_Pred_",j,"\"),sum <- 100))")))}
      #eval(parse(text=paste0("ifelse(SL_Pred_",j," == 100,
      #                 sum <- 100,sum <- sum(Ynew[",s,"] - SL_Pred_",j,"$pred[",s,"])^2)")))}
      #print(SL_Pred_1)
      eval(parse(text=paste0("ifelse(typeof(SL_Pred_",j,") != \"double\",
                             sum <- sum+sum(Ypred[",s,"] - SL_Pred_",j,"$pred[",s,"])^2,sum <- SL_Pred_",j,")")))
      #(Ypred - t(SL_Pred_1$pred)) %*% t(Ypred - t(SL_Pred_1$pred))
     # print(c(Ypred[s],SL_Pred_1$pred[s]))
     # print(c(sum,s))
     #  readline("Press <return to continue")
     #  }
    
    ## this step makes the final calculation of the MSE and it labels it with a j --> MSE_j
    eval(parse(text=paste0("MSE_",j," <- sum/length(Ypred)")))
    sum = 0;
    #print(j)
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
  #MSE_sorted_temp <- setorder(MSE_temp, mse,-k_set)
  #MSE_sorted_temp <- sort(MSE_temp, mse,-k_set)
  MSE_sorted_temp <- MSE_temp[order(MSE_temp$mse),]

  ## DEFINE HERE THE TOP # OF COVARIATES TO LIST in the MODEL MINING STEP
  # ""top" is defined at the beginning (line 14) and represents the top MSEs to consider for feature mining (ks).
  # Each one will have a set of best features with their relative highest frequencies
  top=50;
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
  eval(parse(text=paste0("h=hist(x, breaks=seq(min(x)-0.5, max(x)+0.5, by=1))")))
  h$density = h$counts/sum(h$counts)*100
  title_temp <- c("Gaussian Test - Knockoff Filter",range_n,range_k)
  print(title_temp)
  plot(h,freq=FALSE,ylab='Density (%)',xlab='Feature #',main = title_temp,ylim=c(0,15))
  
  # GENERATE HISTOGRAM OF THE TOP # OF COVARIATES FOR SINGLE EXPERIMENT
  eval(parse(text=paste0("x = k_top_",top,"_temp")))
  #h = hist(x)
  #eval(parse(text=paste0("h = hist(x)")))#,breaks=seq(min(k_top_",top,"_temp)-0.5,max(k_top_",top,"_temp)+0.5, by=1))")))
  eval(parse(text=paste0("h=hist(k_top_",top,"_temp, breaks=seq(min(k_top_",top,"_temp)-0.5, max(k_top_",top,"_temp)+0.5, by=1))")))
  #eval(parse(text=paste0("hist(x, breaks=seq(min(x)-0.5, max(x)+0.5, by=1))")))
  h$density = h$counts/sum(h$counts)*100
  title_temp <- c("Gaussian Test - CBDA-SL",range_n,range_k)
  print(title_temp)
  plot(h,freq=FALSE,ylab='Density (%)',xlab='Feature #',main = title_temp,ylim=c(0,2.5))
  #readline("Press <return to continue")

  # RETRIEVE AND SAVE THE LABELS OF THE TOP [BEST] FEATURES
  BEST=20;
  eval(parse(text=paste0("Top_features=sort(table(k_top_",top,"_temp), decreasing = TRUE)")))
  Top_features_labels <- names(Xnorm_sub)[as.numeric(names(Top_features))]
  eval(parse(text=paste0("Top_",BEST,"_features_labels <- names(Xnorm_sub)[as.numeric(names(Top_features)[1:",BEST,"])]")))

  #eval(parse(text=paste0("print(\"CBDA_SL_M",M,"_miss",misValperc,"_n",range_n,"_k",range_k,"_Light.RData\")")))
  eval(parse(text=paste0("print(Top_",BEST,"_features_labels)")))
  eval(parse(text=paste0("print(Top_features[1:",BEST,"])")))


  #eval(parse(text=paste0("save.image(\"C:/Users/simeonem/Documents/CBDA-SL/ExperimentsNov2016/MRI/CBDA_SL_M",M,"_miss",misValperc,"_n",range_n,"_k",range_k,"_Light_Results_",i,".RData\")")))
  eval(parse(text=paste0("save.image(\"",workspace_directory,"CBDA_SL_M",M,"_miss",misValperc,"_n",range_n,"_k",range_k,"_Light_Gaussian_Dec5.RData\")")))
  list_exps=1:dim(arguments)[1];
  #list_exps=list_exps[-1*c(1:18,20:30)]
  #list_exps=1:dim(arguments)[1];
  #list_exps=list_exps[-1*c(6:30)]
  eval(parse(text=paste0("save(arguments,label,workspace_directory,file= \"~/temp_data_info.RData\")")))
  rm(list = ls())
  #print(s)
  # eval(parse(text=paste0("load(\"~/temp_Gaussian_Dec5.RData\")")))
  eval(parse(text=paste0("load(\"~/temp_data_info.RData\")")))
  # eval(parse(text=paste0("save(arguments,workspace_directory,i,list_exps,file= \"~/temp_Gaussian_Dec5.RData\")")))
  # rm(list = ls())
  # #print(s)
  # eval(parse(text=paste0("load(\"~/temp_Gaussian_Dec5.RData\")")))
  # #readline("Press <return to continue")
}
w00<-list.files(pattern = "\\.RData$", full.names = TRUE)
print(length(w00))

## Delete all the "_Light_jglobal_KO.RData" workspaces, and leave the consolidated one "_Light_KO.RData"
eval(parse(text=paste0("load(\"~/temp_data_info.RData\")")))
list_exps=1:dim(arguments)[1];
#list_exps=list_exps[-1*c(1:18,20:30)]
#list_exps=20;
for (i in list_exps) {
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
  
  for (j in 1:M) {
    eval(parse(text=paste0("file.remove(\"",workspace_directory,"CBDA_SL_M",M,"_miss",misValperc,"_n",range_n,"_k",range_k,"_Light_",j,"_Gaussian_Dec5.RData\")")))
  }
}
w00<-list.files(pattern = "\\.RData$", full.names = TRUE)
print(length(w00))

#dev.off()
