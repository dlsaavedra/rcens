#' Title Generate Sample with Cure Fraction
#'
#'@description
#' Generator Sample with Cure Fraction,
#'  given a generator of samples of the distribution X (rdistrX) with
#'  parameters appended by the list param_X. Also the proportion of cure desired p.
#'
#' @param rdistrX  sample generator of distribution X. \cr First argument number of samples, next arguments in param_X.
#' @param param_X  list with parameters of rdistrX function.
#' @param n number of sample to create.
#' @param p cure fraction
#'
#' @return A list with sample data information: \tabular{ll}{
#'    \code{data_cf}\tab vector of cure fraction sample.\cr
#'    \code{cure_list} \tab vector of 1 and 0 indicating whether the i-th sample is cured. \cr
#'     \tab 1:= cure , 0:= no cure \cr
#'    \code{cure_fraction} \tab cure fraction used to create de sample. \cr
#'    \tab \cr
#' }
#'
#'@example man/examples/rcuref.R
## @example examples_plot/Example_rcensT3_plot.R
#' @author Daniel Saavedra Morales
#' @export


rcuref <- function(rdistrX, param_X, n = 1e04, p = 0.5){

  # Generate Bernoulli
  b = rbinom(n = n,  size = 1, prob =  p)
  #Generate Sample of X
  x = do.call('rdistrX', c(n, param_X))
  x[b==1] = Inf

  return(list("data_cf" = x,
              "cure_list" = b,
              "cure_fraction" = p))
}



