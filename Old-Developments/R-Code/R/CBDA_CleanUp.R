#' @title
#' CBDA Clean up function for Compressive Big Data Analytics
#'
#' @description
#'  This CBDA cleans the current directory where all the intermediate workspaces have been created.

#' @param label This is the label appended to RData workspaces generated within the CBDA calls

#' @param workspace_directory Directory where the results and workspaces are saved

#' @return value

#' @export

CBDA_CleanUp <- function(label = "CBDA_package_test" , workspace_directory = tempdir()) {

  M <- misValperc <- range_n <- range_k <- min_covs <- max_covs <- NULL
  cat("Clean up started !!\n\n")
  filename_specs <- file.path(workspace_directory,paste0(label,"_info.RData"))
  #eval(parse(text=paste0("load(\"",workspace_directory,"/",label,"_info.RData\")")))
  load(filename_specs)

  filename <- file.path(workspace_directory,
                        paste0("CBDA_SL_M",M,"_miss",misValperc,"_n",range_n,
                               "_k",range_k,"_Light_",label,"_VALIDATION.RData"))
  load(filename)

  # This loop cleans up all the top ranked validation predictive models
  for (j in min_covs:max_covs) {
    filename <- file.path(workspace_directory,
                          paste0("CBDA_SL_M",M,"_miss",misValperc,"_n",range_n,
                                 "_k",range_k,"_Light_",j,"_",label,"_VALIDATION.RData"))
    file.remove(filename)

    #eval(parse(text=paste0("file.remove(\"",workspace_directory,"/CBDA_SL_M",M,"_miss",misValperc,
    #                         "_n",range_n,"_k",range_k,"_Light_",j,"_",label,"_VALIDATION.RData\")")))
  }
  cat("Clean up completed successfully !!")
  return()
}
