#' @title
#' CBDA Consolidation function for Compressive Big Data Analytics - LONI pipeline
#'
#' @description
#'  This CBDA function consolidates all the M workspaces generated in the
#'  Learning/Training step into a single workspace. It also ranks all the predictive models and selects
#'  the **top** ones to be sifted for top predictive features
#'  to be passed to the next step (i.e., **the Validation Step**).
#'
#' @param top Top predictions to select out of the M

#' @param max_covs Top features to display and include in the Validation Step where nested models are tested

#' @param M Number of the BigData subsets on which perform Knockoff Filtering and SuperLearner feature mining

#' @param misValperc Percentage of missing values to introduce in BigData (used just for testing, to mimic real cases).

#' @param range_k Features Sampling Range - FSR

#' @param range_n Cases Sampling Range - CSR

#' @param label This is the label appended to RData workspaces generated within the CBDA calls

#' @param workspace_directory Directory where the results and workspaces are saved

#' @return value

#' @export

CBDA_Consolidation.pipeline <- function(top , max_covs , M , misValperc ,
                               range_k , range_n , label , workspace_directory = tempdir()) {

  N_cores <- algorithm_list <- x_hist <- Top_features_MSE <- NULL

  ## This "top" parameter identifies the sets of features associated with the top MSEs
  cat("CONSOLIDATION STEP HAS STARTED !!\n\n")
  filename_specs <- file.path(workspace_directory,paste0(label,"_info.RData"))
  load(filename_specs)

  ## DATA CONSOLIDATION - load M RData workspaces [j_global] per experiment [i_exp]
  ##                      and consolidate them into 1 RData workspace
  for (j in 1:M){
    print(sprintf("Loading workspace: %d", j))
    utils::flush.console()
    filename <- file.path(workspace_directory,
                          paste0("CBDA_SL_M",M,"_miss",misValperc,"_n",range_n,
                                 "_k",range_k,"_Light_",j,"_",label,".RData"))
    load(filename)
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

  ## DEFINE HERE THE TOP # OF COVARIATES TO LIST in the MODEL MINING STEP
  # "top" is defined at the beginning (line 8) and represents the top MSEs to consider for
  # feature mining (ks). Each one will have a set of best features with their relative highest frequencies
  eval(parse(text=paste0("k_top_",top,"_temp <- NULL")))
  for (r in 1:top){
    eval(parse(text=paste0("k_top_",top,"_temp <- c(k_top_",top,"_temp, k",MSE_sorted_temp$k_set[r],")")))
  }
  #  GENERATING THE ARRAY OF ACCURACY FOR ALL THE M SL OBJECTS
  # ACCURACY RANKING
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
  for (j in 1:s) {
    eval(parse(text=paste0("rm(KO_result_",j,")")))
    eval(parse(text=paste0("rm(KO_selected_",j,")")))
  }

  # # GENERATE HISTOGRAM OF THE CUMULATIVE KNOCKOFF RESULTS FOR SINGLE EXPERIMENT
  x = KO_sub;
  if (identical(x, numeric(0))) {
    "KO empty"
    }   else {
    h_KO_sub=graphics::hist(x, plot = FALSE )
    h_KO_sub$density = h_KO_sub$counts/sum(h_KO_sub$counts)*100
    title_temp <- c("KNOCKOFF FILTER RESULTS")
    #plot(h_KO_sub,breaks=seq(min(x)-0.5, max(x)+0.5, by=1),freq=FALSE,ylab='Density (%)',xlab='Feature #',
    #     main = title_temp,ylim=c(0,max(h_KO_sub$density)))
    }

  # GENERATE DATA FOR HISTOGRAM OF THE TOP # OF COVARIATES - MSE
  eval(parse(text=paste0("x_hist = k_top_",top,"_temp")))
  h_MSE=graphics::hist(x_hist, plot = FALSE ,breaks=seq(min(x_hist)-0.5, max(x_hist)+0.5, by=1))
  h_MSE$density = h_MSE$counts/sum(h_MSE$counts)*100

  # GENERATE DATA FOR HISTOGRAM OF THE TOP # OF COVARIATES - ACCURACY
  eval(parse(text=paste0("x_hist = k_top_",top,"_temp_Accuracy")))
  h_Accuracy=graphics::hist(x_hist, plot = TRUE ,breaks=seq(min(x_hist)-0.5, max(x_hist)+0.5, by=1))
  h_Accuracy$density = h_Accuracy$counts/sum(h_Accuracy$counts)*100

  # RETRIEVE AND SAVE THE LABELS OF THE TOP [max_covs] FEATURES
  Top_features <- NULL
  eval(parse(text=paste0("Top_features=sort(table(k_top_",top,"_temp_Accuracy), decreasing = TRUE)")))
  eval(parse(text=paste0("Top_features_MSE=sort(table(k_top_",top,"_temp), decreasing = TRUE)")))

  qa <-as.data.frame(Top_features[1:max_covs])
  qa_MSE <-as.data.frame(Top_features_MSE[1:max_covs])

  names(qa) <- c("CBDA","Frequency")
  names(qa_MSE) <- c("CBDA","Frequency")

  qa$Density <- 100*(qa$Frequency/sum(Top_features))
  qa_MSE$Density <- 100*(qa_MSE$Frequency/sum(Top_features_MSE))

  qa_ALL <- cbind(qa,qa_MSE)

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

  filename <- file.path(workspace_directory,
                        paste0("CBDA_SL_M",M,"_miss",misValperc,"_n",range_n,
                               "_k",range_k,"_Light_",label,".RData"))
  save(list = ls(all.names = TRUE), file = filename)

  #eval(parse(text=paste0("save(list = ls(all.names = TRUE),
  #    file= \"",workspace_directory,"/CBDA_SL_M",M,"_miss",misValperc,"_n",range_n,
  #                       "_k",range_k,"_Light_",label,".RData\")")))

  # This loop cleans up all the first M learning/training subsamples
  for (s in 1:M) {
    filename <- file.path(workspace_directory,
                          paste0("CBDA_SL_M",M,"_miss",misValperc,"_n",range_n,
                                 "_k",range_k,"_Light_",s,"_",label,".RData"))
    file.remove(filename)

    #eval(parse(text=paste0("file.remove(\"",workspace_directory,"/CBDA_SL_M",M,"_miss",misValperc,
    #                       "_n",range_n,"_k",range_k,"_Light_",s,"_",label,".RData\")")))
  }
  cat("Consolidated workspace successfully created.\n\n")
  cat("Subsample workspaces successfully deleted.\n\n")
  cat("Consolidation completed successfully !!\n\n")
  return()
}
