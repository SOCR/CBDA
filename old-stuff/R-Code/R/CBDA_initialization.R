#' @title
#' CBDA Initialization function for Compressive Big Data Analytics
#'
#' @description
#'  This CBDA function installs and attaches all the packages needed to run the CBDA.
#'  A user-defined list of packages can be passed as argument. Itis recommended to first execute
#'  the function without any arguments.
#'
#' @param pkg List of packages to install and attach for running the CBDA algorithm.
#'            A default list is already defined.
#' @param install Option to setup installation and attachment of the listed package.
#'                Set to FALSE by default
#' @return value
#'
#' @export
#'

CBDA_initialization <- function(pkg =c("missForest" , "stats" , "utils" , "prettydoc" ,
                                       "foreach" , "SuperLearner" , "knockoff" , "caret" ,
                                       "smotefamily" , "parallel" , "doParallel" , "glmnet"),
                                install = FALSE) {

  for (i in 1:length(pkg))
  {
    if(identical(find.package(pkg[i],quiet = TRUE),character(0))==TRUE)
    {
      if(install == TRUE)
      {
        utils::install.packages(pkg[i], dependencies = TRUE)
      } else {
      warning(paste("There is no package called '",pkg[i],"'. It needs to be installed"))
      }
    } else {
      cat(paste("package",pkg[i],"already installed\n"))
    }
  }
  a_temp <- sapply(pkg, require, character.only = TRUE)

  return(a_temp)
}
