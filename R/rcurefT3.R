#source("R/rcuref.R")

#' Title Generate Sample with Cure Fraction, Random Censoring
#'
#'@description
#' Generator Sample with Cure Fraction, Random Censoring.
#'  Given a generator of samples of the distribution X (rdistrX) with
#'  parameters appended by the list param_X. Also accumulate function of distribution and generator sample
#'  of distribution C (censoring) with parameters appended by the list param_C
#'  In which, you can control the desired censorship percentage.
#'  \cr
#'  Note: cure fraction (p) must be less than desired censorship percentage.
#'
#'
#' @param rdistrX sample generator of distribution X. \cr
#' First argument number of samples, next arguments in param_X.
#' @param pdistrC function distribution of C. First argument probabilities, next arguments in param_C.
#' @param rdistrC sample generator of distribution C. \cr First argument number of samples, next arguments in param_C.
#' @param param_X list with parameters of rdistrX function.
#' @param param_C list with parameters of rdistrC function, one of these parameters \cr should be "lambda",
#'  this wil be the searched parameter.
#' @param n number of sample to create.
#' @param theta Desired censoring percentage
#' @param n_mc number of sample use in Monte Carlo integration, greater n_mc more accuracy.
#' @param tol_low lowest value where live the search parameter lambda.
#' @param tol_upper  uppest value where live the search parameter lambda.
#' @param check if TRUE print a censoring percentage of new created database.
#' @param right if TRUE create right-censored data, else create left-censored
#' @param p cure fraction
#'
#' @return A list with sample data information: \tabular{ll}{
#'    \code{lambda}\tab searched censoring distribution parameter.\cr
#'    \code{sample_censored} \tab vector of censored sample. \cr
#'    \tab \cr
#'    \code{sample_uncensored} \tab vector of uncensored sample (original). \cr
#'    \tab \cr
#'    \code{censored_indicator} \tab vector of 1 and 0 indicating whether the i-th sample is censored.  \cr
#'      \tab 1:= no censored, 0:= censored \cr
#'    \code{censored_time} \tab vector of censorship time. \cr
#'    \code{n_censored} \tab number of censored samples.  \cr
#'     \code{cure_list} \tab vector of 1 and 0 indicating whether the i-th sample is cured. \cr
#'     \tab 1:= cure , 0:= no cure \cr
#'    \code{cure_fraction} \tab cure fraction used to create de sample. \cr
#'    \tab \cr

#' }
#'
#' @seealso \code{\link{rcuref}} Generate Sample with Cure Fraction.\cr
#'
#' @example man/examples/rcurefT3.R
## @example examples_plot/Example_rcurefT3_plot.R
#' @author Daniel Saavedra Morales
#' @export


rcurefT3 <- function(rdistrX, pdistrC, rdistrC ,param_X, param_C,
                    p = 0.1 , n = 1e04, theta = .5, n_mc = 1e04,
                    tol_low = 1e-06, tol_upper = 1e04,
                    check = TRUE, right= TRUE)
{
  if (p > 1 || p < 0){
    warning("p is not between 0 and 1 ")
    return()
  }
  if (theta > 1 || theta < 0){
    warning("theta is not between 0 and 1 ")
    return()
  }
  if (p > theta ){
    warning("p must be less than theta")
    return()
  }

  #Calculate the P(C < X) with Monte Carlo
  sample_cf_mc = rcuref(rdistrX = rdistrX, param_X = param_X,
                     n = n_mc, p = p)

  sample_x_mc = sample_cf_mc$data_cf

  f<- function(lambda){
    param_C[param_C == "lambda"] = lambda
    return(sapply(sample_x_mc,
                  function(x){do.call("pdistrC",
                                      c(x, param_C))}))
  }

  if (!right){theta = 1 - theta}
  #Resolved the P(C < X) - theta = 0 with uniroot (searched lambda)

  f_eq <- function(lambda){mean(f(lambda)) - theta}
  lambda_mc = uniroot(f_eq, interval = c(tol_low, tol_upper))$root[1]

  #Create the new censored data

  sample_cf = rcuref(rdistrX = rdistrX, param_X = param_X,
                     n = n, p = p)
  x = sample_cf$data_cf

  param_C[param_C == "lambda"] = lambda_mc
  c = do.call("rdistrC",
              c(n, param_C))

  delta <- rep (0, n)
  t = x
  if (right){
    delta[x<=c] = 1
    t[x>c] = c[x>c]
  }
  else{
    delta[x>=c] = 1
    t[x<c] = c[x<c]
  }

  if (check){cat("Censorship percentage:",1 - (sum (delta) / n ))}

  return(list("lambda" = lambda_mc,
              "sample_censored" = t,
              "sample_uncensored" = x,
              "censored_indicator" = delta,
              "censored_time" = c,
              "n_censored" = sum (delta == 0),
              "cure_list" = sample_cf$cure_list,
              "cure_fraction" = p))
}
