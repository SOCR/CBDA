#' @title
#' CBDA Spectrum plot function for Compressive Big Data Analytics
#'
#' @description
#'  This CBDA function generates histograms of the feature counts/densities as returned
#'  by the Accuracy and MSE metrics after the Learning/Training step.
#'
#' @param top Top ranked predictive models from the Learning/Training step
#'
#' @return value
#'
#' @export
#'

CBDA_spectrum_plots <- function(top) {
  # GENERATE HISTOGRAM OF THE TOP # OF COVARIATES FOR SINGLE EXPERIMENT - MSE
  x_hist <- NULL
  eval(parse(text=paste0("x_hist = k_top_",top,"_temp")))
  h_MSE=graphics::hist(x_hist, plot = TRUE,ylab='Density (Count)',xlab='Feature #',
             main = c("CBDA RESULTS - MSE metric (counts)") ,
             breaks=seq(min(x_hist)-0.5, max(x_hist)+0.5, by=1))

  h_MSE$density = h_MSE$counts/sum(h_MSE$counts)*100
  title_temp <- c("CBDA RESULTS - MSE metric (%)")
  graphics::plot(h_MSE,freq=FALSE,ylab='Density (%)',xlab='Feature #',main = title_temp,ylim=c(0,max(h_MSE$density)))

  # GENERATE HISTOGRAM OF THE TOP # OF COVARIATES FOR SINGLE EXPERIMENT - ACCURACY
  eval(parse(text=paste0("x_hist = k_top_",top,"_temp_Accuracy")))
  h_Accuracy=graphics::hist(x_hist, plot = TRUE,ylab='Density (Count)',xlab='Feature #',
                  main = c("CBDA RESULTS - Accuracy metric (counts)") ,
                  breaks=seq(min(x_hist)-0.5, max(x_hist)+0.5, by=1))

  h_Accuracy$density = h_Accuracy$counts/sum(h_Accuracy$counts)*100
  title_temp <- c("CBDA RESULTS - Accuracy metric (%)")
  graphics::plot(h_Accuracy,freq=FALSE,ylab='Density (%)',xlab='Feature #'
       ,main = title_temp,ylim=c(0,max(h_Accuracy$density)))
  return("Histograms of both MSE and Accuracy metrics generated successfully !!")
}
