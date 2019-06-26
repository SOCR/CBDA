# This loads the experiments specs
# eval(parse(text=paste0("save(Xtemp,Xpred,label, q, Ypred, M, i_exp, Ytemp, SL_Pred_MSE",j_global,"SL_Pred_",j_global,
#                        ",nonzero,k",j_global,",MSE_",j_global,",Accuracy_",j_global,",KO_selected_",j_global,",
#                        file= \"",workspace_directory,"CBDA_SL_M",M,"_miss",misValperc,"_n",range_n,"_k"
#                        ,range_k,"_Light_",j_global,"_",label,".RData\")")))
# eval(parse(text=paste0("save(arguments,label,workspace_directory,i_exp,
#                        file= \"~/",label,"_info_for_consolidation.RData\")")))
label = c("Binomial_dataset_3_final_INFERENCE") #Binomial_dataset_3_final_rep1
eval(parse(text=paste0("load(\"~/",label,"_info_for_consolidation.RData\")")))
label = c("Binomial_dataset_3_final_INFERENCE")
#load("/ifshome/smarino/Binomial_dataset_new_sd_info_for_consolidation.RData")
# label = c("Binomial_dataset_1_new_sd")
# workspace_directory = c("/ifs/loni/ccb/collabs/2016/CBDA_SL_2016/Simeone/")
# i_exp=1
# arguments <- NULL
# #arguments <- rbind(arguments,c(9000,	0,	5,	15,	60,	80))
# arguments <- rbind(arguments,c(9000,	0,	15,	30,	60,	80))
# arguments <- rbind(arguments,c(9000,	20,	5,	15,	60,	80))
# arguments <- rbind(arguments,c(9000,	20,	15,	30,	60,	80))

save.image("~/Info_for_consolidation.RData")
load("~/Info_for_consolidation.RData")

print(arguments)
print(label)
print(i_exp)
## OPENS A PDF FILE FOR RESULTS [OPTIONAL]
#eval(parse(text=paste0("pdf(\"",workspace_directory,"/CBDA-SL-TEST-output.pdf\")"))) # Windows
eval(parse(text=paste0("pdf(\"",workspace_directory,"Plot_Histograms_",label,".pdf\")")))

## This "top" parameter identifies the sets of features associated with the top MSEs
top=1000;

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
  G=1200
  for (s in 1:G){
    print(s)
    eval(parse(text=paste0("load(\"",workspace_directory,"CBDA_SL_M",M,"_miss",misValperc,"_n",range_n,"_k",range_k,"_Light_",s,"_",label,".RData\")")))
  }
  #  GENERATING THE ARRAY OF MSE AND ACCURACY METRICS FOR ALL THE M SL OBJECTS
  MSE=0;
  Accuracy=0;
  for (j in 1:G) {
    eval(parse(text=paste0("MSE[j] <- MSE_",j)))
    eval(parse(text=paste0("Accuracy[j] <- Accuracy_",j)))
  }
    #  REMOVE THE ARRAYS OF MSE and ACCURACY FOR ALL THE
    #  M OBJECTS to avoid stack overflow errors
  for (j in 1:G) {
    eval(parse(text=paste0("rm(MSE_",j,")")))
    eval(parse(text=paste0("rm(Accuracy_",j,")")))
    eval(parse(text=paste0("rm(SL_Pred_",j,")")))
    eval(parse(text=paste0("rm(SL_Pred_MSE_",j,")")))

  }
  ## THIS SAVES THE CONSOLIDATED WORKSPACE FOR EACH EXPERIMENT
  eval(parse(text=paste0("save.image(\"",workspace_directory,"CBDA_SL_M",M,"_miss",misValperc,"_n",range_n,"_k",range_k,"_Light_",label,".RData\")")))
  ## THIS SAVES A TEMP FILE AGAIN TO BE USED AFTER THE CURRENT WORKSPACE IS EMPTIED TO SPEED UP PROCESSING
  save(arguments,label,workspace_directory,i_exp,file= "~/Info_for_consolidation.RData")
  ## CLEARS THE WORKSPACE FOR FASTER PROCESSING
  rm(list = ls())
  # LOADS THE BASIC INFO TO RUN THE CONSOLIDATION ON THE NEXT EXPERIMENT
  load("~/Info_for_consolidation.RData")
}
# 
# 
# ## This next loop deletes all the "_Light_JOBID_label.RData" workspaces,
# ## and leave the consolidated one "_Light_label.RData". First loads the info.
load("~/Info_for_consolidation.RData")
#print(i_exp)
i=6
for (i in i_exp) {
#   print(i)
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

  G=1200
  for (s in 1:G) {
    eval(parse(text=paste0("file.remove(\"",workspace_directory,"CBDA_SL_M",M,"_miss",misValperc,"_n",range_n,"_k",range_k,"_Light_",s,"_",label,".RData\")")))
  }
}
# 

load("~/Info_for_consolidation.RData")
eval(parse(text=paste0("load(\"",workspace_directory,"CBDA_SL_M",M,"_miss",misValperc,"_n",range_n,"_k",range_k,"_Light_",label,".RData\")")))

# MSE RANKING
s=1200;
MSE_temp <- NULL
MSE_sorted_temp <- NULL
MSE_temp <- data.frame(mse=MSE[1:s],k_set=1:s)
MSE_sorted_temp <- MSE_temp[order(MSE_temp$mse),]
## DEFINE HERE THE TOP # OF COVARIATES TO LIST in the MODEL MINING STEP
# "top" is defined at the beginning (line 8) and represents the top MSEs to consider for
# feature mining (ks). Each one will have a set of best features with their relative highest frequencies
top=500
eval(parse(text=paste0("k_top_",top,"_temp <- NULL")))
for (r in 1:top){
  eval(parse(text=paste0("k_top_",top,"_temp <- c(k_top_",top,"_temp, k",MSE_sorted_temp$k_set[r],")")))
}
save(s,top,file = "exp_specs.RData")
#  GENERATING THE ARRAY OF ACCURACY FOR ALL THE M SL OBJECTS
# ACCURACY RANKING
#for (s in seq(10,M,M/10)){
Accuracy_temp <- NULL
Accuracy_sorted_temp <- NULL
Accuracy_temp <- data.frame(Accuracy=Accuracy[1:s],k_set=1:s)
Accuracy_sorted_temp <- Accuracy_temp[order(-Accuracy_temp$Accuracy),]

eval(parse(text=paste0("k_top_",top,"_temp_Accuracy <- NULL")))
for (r in 1:top){
  eval(parse(text=paste0("k_top_",top,"_temp_Accuracy <- c(k_top_",top,"_temp_Accuracy,k",Accuracy_sorted_temp$k_set[r],")")))
}

# Cumulative KNOCKOFF results
KO_sub <- NULL
for (j in 1:s) {
  eval(parse(text=paste0("KO_sub <- c(KO_sub,KO_selected_",j,")")))
}

# GENERATE HISTOGRAM OF THE CUMULATIVE KNOCKOFF RESULTS FOR SINGLE EXPERIMENT 
x = KO_sub;
h=hist(x, plot = FALSE ,breaks=seq(min(x)-0.5, max(x)+0.5, by=1))
h$density = h$counts/sum(h$counts)*100
title_temp <- c("KNOCKOFF FILTER RESULTS")
plot(h,freq=FALSE,ylab='Density (%)',xlab='Feature #',main = title_temp,ylim=c(0,max(h$density)))

# GENERATE HISTOGRAM OF THE TOP # OF COVARIATES FOR SINGLE EXPERIMENT - MSE
eval(parse(text=paste0("x = k_top_",top,"_temp")))
h=hist(x, plot = FALSE ,breaks=seq(min(x)-0.5, max(x)+0.5, by=1))
h$density = h$counts/sum(h$counts)*100
title_temp <- c("CBDA-SL RESULTS - MSE metric")
plot(h,freq=FALSE,ylab='Density (%)',xlab='Feature #',main = title_temp,ylim=c(0,max(h$density)))
#readline("Press <return to continue")

# GENERATE HISTOGRAM OF THE TOP # OF COVARIATES FOR SINGLE EXPERIMENT - ACCURACY
eval(parse(text=paste0("x = k_top_",top,"_temp_Accuracy")))
h=hist(x, plot = FALSE ,breaks=seq(min(x)-0.5, max(x)+0.5, by=1))
h$density = h$counts/sum(h$counts)*100
title_temp <- c("CBDA-SL RESULTS - Accuracy metric")
plot(h,freq=FALSE,ylab='Density (%)',xlab='Feature #',main = title_temp,ylim=c(0,max(h$density)))

# RETRIEVE AND SAVE THE LABELS OF THE TOP [BEST] FEATURES
#Binomial_dataset_3_new = read.csv("Binomial_dataset_3_final.txt",header = TRUE)
BEST=100; # how many top rows to show
#cols_to_eliminate=c(1)
Top_features <- NULL
eval(parse(text=paste0("Top_features=sort(table(k_top_",top,"_temp_Accuracy), decreasing = TRUE)")))
eval(parse(text=paste0("Top_features_MSE=sort(table(k_top_",top,"_temp), decreasing = TRUE)")))
# 
# Top_features_labels <- names(Binomial_dataset_3_new[-cols_to_eliminate])[as.numeric(names(Top_features))]
# Top_features_labels_MSE <- names(Binomial_dataset_3_new[-cols_to_eliminate])[as.numeric(names(Top_features_MSE))]
# 
# eval(parse(text=paste0("Top_",BEST,"_features_labels <- names(Binomial_dataset_3_new[-cols_to_eliminate])[as.numeric(names(Top_features)[1:",BEST,"])]")))
# eval(parse(text=paste0("Top_",BEST,"_features_labels_MSE <- names(Binomial_dataset_3_new[-cols_to_eliminate])[as.numeric(names(Top_features_MSE)[1:",BEST,"])]")))
# 
qa <-as.data.frame(Top_features[1:BEST])
qa_MSE <-as.data.frame(Top_features_MSE[1:BEST])

names(qa) <- c("CBDA","Frequency")
names(qa_MSE) <- c("CBDA","Frequency")

qa$Density <- 100*(qa$Frequency/sum(Top_features))
qa_MSE$Density <- 100*(qa_MSE$Frequency/sum(Top_features_MSE))

qa_ALL <- cbind(qa,qa_MSE)

# print("TABLE with CBDA-SL & KNOCKOFF FILTER RESULTS")
# print(c("REPLICATION",i))
# print(c("TRUE FEATURES"))
# print(nonzero)
Top_Knockoff_features=sort(table(KO_sub), decreasing = TRUE)
Top_Knockoff_features_labels <- as.numeric(names(Top_Knockoff_features)[1:BEST])
qa_ALL$Knockoff <- Top_Knockoff_features_labels
qa_ALL$KO_Count <- Top_Knockoff_features[1:BEST]
qa_ALL$KO_Density <- 100*(Top_Knockoff_features[1:BEST]/sum(Top_Knockoff_features))

names(qa_ALL) <- c("Accuracy","Count","Density","MSE","Count","Density","Knockoff","Count","Density")
#names(qa) <- c("CBDA","Frequency","Density","Knockoff","Density")
print(qa_ALL[1:BEST,], right = FALSE, row.names = FALSE)

## THIS SAVES THE CONSOLIDATED WORKSPACE FOR EACH EXPERIMENT
eval(parse(text=paste0("save.image(\"",workspace_directory,"CBDA_SL_M",M,"_miss",misValperc,"_n",range_n,"_k",range_k,"_Light_",label,".RData\")")))

# This is enforced in case a pdf channel has been opened at the beginning (line 5) 
# to save results in a pdf
dev.off()
