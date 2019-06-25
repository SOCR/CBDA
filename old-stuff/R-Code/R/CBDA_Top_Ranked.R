#' @title
#' CBDA Top-Ranked selection function for Compressive Big Data Analytics
#'
#' @description
#'  This CBDA function has all the features of the *Consolidation()* function but allows to choose a
#'  different **top** value (i.e., different from the one specified in the main *CBDA()* function

#' @param top_new The new value for the Top predictions to select out of the M

#' @param label This is the label appended to RData workspaces generated within the CBDA calls

#' @param Kcol_min Lower bound for the percentage of features-columns sampling (used for the Feature Sampling Range - FSR)

#' @param Kcol_max Upper bound for the percentage of features-columns sampling (used for the Feature Sampling Range - FSR)

#' @param Nrow_min 	Lower bound for the percentage of cases-rows sampling (used for the Case Sampling Range - CSR)

#' @param Nrow_max Upper bound for the percentage of cases-rows sampling (used for the Case Sampling Range - CSR)

#' @param misValperc Percentage of missing values to introduce in BigData (used just for testing, to mimic real cases).

#' @param M Number of the BigData subsets on which perform Knockoff Filtering and SuperLearner feature mining

#' @param workspace_directory Directory where the results and workspaces are saved

#' @return value

#' @export

CBDA_Top_Ranked <- function(top_new = 500 , label = "CBDA_package_test" , Kcol_min = 5 , Kcol_max = 15,
                                   Nrow_min = 30 , Nrow_max = 50 , misValperc = 0, M = 3000 ,
                                   workspace_directory = getwd()) {

  range_n <- range_k <- MSE_sorted_temp <- Accuracy <- KO_sub <- NULL

  eval(parse(text=paste0("load(\"",workspace_directory,"/",label,"_info_for_consolidation.RData\")")))
  print("Previous top predictive models")
  print(top)

    eval(parse(text=paste0("load(\"",workspace_directory,"/CBDA_SL_M",M,"_miss",
                         misValperc,"_n",range_n,"_k",range_k,"_Light_",label,".RData\")")))
  eval(parse(text=paste0("save(list = ls(all.names = TRUE),
      file= \"",workspace_directory,"/CBDA_SL_M",M,"_miss",misValperc,"_n",range_n,
                         "_k",range_k,"_Light_",label,"_top_",top,".RData\")")))
  print("New top predictive models")
  top <- top_new
  print(top)
## DEFINE HERE THE TOP # OF COVARIATES TO LIST in the MODEL MINING STEP
# "top" is defined at the beginning (line 8) and represents the top MSEs to consider for
# feature mining (ks). Each one will have a set of best features with their relative highest frequencies
eval(parse(text=paste0("k_top_",top,"_temp <- NULL")))
for (r in 1:top){
  eval(parse(text=paste0("k_top_",top,"_temp <- c(k_top_",top,"_temp, k",MSE_sorted_temp$k_set[r],")")))
}
eval(parse(text=paste0("print(k_top_",top,"_temp)")))

#  GENERATING THE ARRAY OF ACCURACY FOR ALL THE M SL OBJECTS
# ACCURACY RANKING
Accuracy_temp <- NULL
Accuracy_sorted_temp <- NULL
Accuracy_temp <- data.frame(Accuracy=Accuracy,k_set=1:M)
Accuracy_sorted_temp <- Accuracy_temp[order(-Accuracy_temp$Accuracy),]

eval(parse(text=paste0("k_top_",top,"_temp_Accuracy <- NULL")))
for (r in 1:top){
  eval(parse(text=paste0("k_top_",top,"_temp_Accuracy <- c(k_top_",top,"_temp_Accuracy,k",Accuracy_sorted_temp$k_set[r],")")))
}

eval(parse(text=paste0("save(list = ls(all.names = TRUE),
      file= \"",workspace_directory,"/CBDA_SL_M",M,"_miss",misValperc,"_n",range_n,
                       "_k",range_k,"_Light_",label,".RData\")")))
# # GENERATE HISTOGRAM OF THE CUMULATIVE KNOCKOFF RESULTS FOR SINGLE EXPERIMENT
x = KO_sub;
print(x)
if (identical(x, numeric(0))) {
  "KO empty"
}   else {
  h_KO_sub=graphics::hist(x, plot = FALSE )
  h_KO_sub$density = h_KO_sub$counts/sum(h_KO_sub$counts)*100
  title_temp <- c("KNOCKOFF FILTER RESULTS")
}

# GENERATE HISTOGRAM OF THE TOP # OF COVARIATES FOR SINGLE EXPERIMENT - MSE
eval(parse(text=paste0("x = k_top_",top,"_temp")))
h_MSE=graphics::hist(x,breaks=seq(min(x)-0.5, max(x)+0.5, by=1), plot = FALSE )
h_MSE$density = h_MSE$counts/sum(h_MSE$counts)*100
title_temp <- c("CBDA-SL RESULTS - MSE metric")

# GENERATE HISTOGRAM OF THE TOP # OF COVARIATES FOR SINGLE EXPERIMENT - ACCURACY
eval(parse(text=paste0("x = k_top_",top,"_temp_Accuracy")))
h_Accuracy=graphics::hist(x, plot = FALSE )
h_Accuracy$density = h_Accuracy$counts/sum(h_Accuracy$counts)*100
title_temp <- c("CBDA-SL RESULTS - Accuracy metric")

return()

}
