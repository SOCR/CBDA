load("~/Info_for_consolidation.RData")
label = c("ADNI_MN2_balanced_INFERENCE")
for(j_global in 1:100)
{
  eval(parse(text=paste0("load(file= \"",workspace_directory,"CBDA_Inference_",j_global,"_",label,".RData\")")))
}
eval(parse(text=paste0("save.image(\"",workspace_directory,"CBDA_Inference_",label,".RData\")")))
eval(parse(text=paste0("save.image(\"",workspace_directory,"CBDA_Inference_",label,"_n",range_n,"_k",range_k,".RData\")")))

#eval(parse(text=paste0("pdf(\"",workspace_directory,"Plot_Inference_",label,".pdf\")")))

for(j_global in 1:100)
{
  eval(parse(text=paste0("qa_ALL$NumberOfFeatures[",j_global,"] <- ",2+j_global,"")))
  eval(parse(text=paste0("qa_ALL$Inference_Acc[",j_global,"] <- Accuracy_",j_global,"")))
  eval(parse(text=paste0("qa_ALL$Inference_MSE[",j_global,"] <- MSE_",j_global,"")))
}
print(head(qa_ALL))

## Stopping Criteria for Accuracy and MSE Performance Metrics
## Two more columns added with 0 (continue) and 1 (stop)
for(i in 1:(100-1))
{
  # Simple improvement (1%,5%, 0.05% in Accuracy)
  ifelse((qa_ALL$Inference_Acc[i+1]/qa_ALL$Inference_Acc[i])>1.0005,
         qa_ALL$StopAcc[i] <- "Keep Going", qa_ALL$StopAcc[i] <- "Stop")
# F of Fisher test
  ifelse((qa_ALL$Inference_MSE[i]/qa_ALL$NumberOfFeatures[i])/(qa_ALL$Inference_MSE[i+1]/qa_ALL$NumberOfFeatures[i+1]) 
         > qf(.95, df1=qa_ALL$NumberOfFeatures[i], df2=qa_ALL$NumberOfFeatures[i+1]),
         qa_ALL$StopMSE[i] <- "Keep Going", qa_ALL$StopMSE[i] <- "Stop")
}

StopAcc <- which(qa_ALL$StopAcc == 1)[1]
StopMSE <- which(qa_ALL$StopMSE == 1)[1]
if(is.na(StopMSE))
  {StopMSE <- StopAcc}
print(StopAcc)
print(StopMSE)
# plot(1:length(qa_ALL$Inference_Acc),qa_ALL$Inference_Acc,xlab = "# top features", ylab = " Accuracy")
# plot(1:length(qa_ALL$Inference_MSE),qa_ALL$Inference_MSE,xlab = "# top features", ylab = " MSE")


eval(parse(text=paste0("save.image(\"",workspace_directory,"CBDA_Inference_",label,".RData\")")))
eval(parse(text=paste0("save(qa_ALL,file= \"",workspace_directory,"CBDA_Inference_",label,"qa_ALL.RData\")")))

#dev.off()