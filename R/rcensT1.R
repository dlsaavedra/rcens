#' @title  Generate Censoring Sample, Type I
#'
#' @description
#'  Generator of censored samples type I with right or left censoring,
#'  given a generator of samples of the
#' distribution X (rdistrX) with parameters appended by the list param_X.
#'  In which, you can control the censorship time or the desired censorship percentage.
#'
#' @param rdistrX  sample generator of distribution X. \cr First argument number of samples, next arguments in param_X.
#' @param param_X  list with parameters of rdistrX function.
#' @param qdistrX  quantile function of X.
#' @param n        number of sample to create.
#' @param t_censored time level censored.
#' @param theta Desired censoring percentage.
#' @param check if TRUE print a censoring percentage of new created database.
#' @param right if TRUE create right-censored data, else create left-censored
#'
#'
#' @return A list with sample data information: \tabular{ll}{
#'    \code{sample_censored} \tab vector of censored sample \cr
#'    \tab \cr
#'    \code{sample_uncensored} \tab vector of uncensored sample (original) \cr
#'    \tab \cr
#'    \code{censored_indicator} \tab vector of 1 and 0 indicating whether the i-th sample is censored  \cr
#'    \code{censored_time} \tab vector of censorship time \cr
#'    \code{n_censored} \tab number of censored samples  \cr
#' }
#'
#' @seealso \code{\link{rcensT2}} for generate censorship sample type II.\cr
#' \code{\link{rcensT3}} for generate censorship sample type III\cr
#' \code{\link{rcensI}} for generate interval censoring sample
#'  with random length interval\cr
#' \code{\link{rcensIfix}} for generate interval censoring sample
#'  with fix length interval
#'
#' @example man/examples/rcensT1.R
##@example examples_plot/Example_rcensT1_plot.R
#' @author Daniel Saavedra Morales
#' @export
#'
rcensT1 <- function(rdistrX, param_X, qdistrX = NULL,
                    n = 1e04, t_censored = -1,
                    theta = .5, check = TRUE, right = TRUE)
{

   if (theta > 1 || theta < 0){
    warning("theta is not between 0 and 1 ")
    return()
  }
  if (t_censored <= 0 & (theta > 1 || theta < 0) ){
    warning("Warning: t_censored <= 0, without censored data")
  }
  if (t_censored < 0 ){
    if (is.null(qdistrX)){
      warning("Warning: If theta is defined should be accompanied with quantile function of X (qdistrX)")
      return()
    }
    if(right){
    t_censored = do.call('qdistrX', c(1 - theta, param_X))
    }
    else{
      t_censored = do.call('qdistrX', c(theta, param_X))
    }
  }
  #Generate Sample of X
  x = do.call('rdistrX', c(n, param_X))

  delta <- rep (1, n)
  t = x
  if (right){
  delta[x >= t_censored] = 0
  t[x >= t_censored] = t_censored
  }
  else{
    delta[x <= t_censored] = 0
    t[x <= t_censored] = t_censored
  }
  if (check){cat("Censorship percentage:",1 - (sum (delta) / n ), "\n")}

  return(list("sample_censored" = t,
              "sample_uncensored" = x,
              "censored_indicator" = delta,
              "censored_time" = t_censored,
              "n_censored" =  sum (delta == 0) ))
}


