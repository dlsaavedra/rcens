#' @title Generate Censoring Sample,Type III (Random)
#'
#' @description
#' Generator of censored samples type III with right or left censoring,
#'  given a generator of samples of the distribution X (rdistrX) with
#'  parameters appended by the list param_X. Also accumulate function of distribution and generator sample
#'  of distribution C (censoring) with parameters appended by the list param_C
#'  In which, you can control the desired censorship percentage.
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
#'
#' @return A list with sample data information: \tabular{ll}{
#'    \code{lambda}\tab searched censoring distribution parameter.\cr
#'    \code{sample_censored} \tab vector of censored sample. \cr
#'    \tab \cr
#'    \code{sample_uncensored} \tab vector of uncensored sample (original). \cr
#'    \tab \cr
#'    \code{censored_indicator} \tab vector of 1 and 0 indicating whether the i-th sample is censored.  \cr
#'    \code{censored_time} \tab vector of censorship time. \cr
#'    \code{n_censored} \tab number of censored samples.  \cr
#' }
#'
#' @seealso \code{\link{rcensT1}} for generate censorship sample type I.\cr
#' \code{\link{rcensT2}} for generate censorship sample type II \cr
#' \code{\link{rcensI}} for generate interval censoring sample
#'  with random length interval\cr
#' \code{\link{rcensIfix}} for generate interval censoring sample
#'  with fix length interval
#'
#' @example man/examples/rcensT3.R
## @example examples_plot/Example_rcensT3_plot.R
#' @author Daniel Saavedra Morales
#' @export


rcensT3 <- function(rdistrX, pdistrC, rdistrC ,param_X, param_C,
                  n = 1e04, theta = .5, n_mc = 1e04,
                  tol_low = 1e-06, tol_upper = 1e04,
                  check = TRUE, right= TRUE)
{

  if (theta > 1 || theta < 0){
    warning("theta is not between 0 and 1 ")
    return()
  }
  #Calculate the P(C < X) with Monte Carlo
  sample_x_mc = do.call('rdistrX', c(n_mc, param_X))

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

  x = do.call('rdistrX', c(n, param_X))
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
              "n_censored" = sum (delta == 0) ))
}
