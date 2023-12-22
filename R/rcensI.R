source("R/tools.R")
#' @title Generate interval censoring sample
#'
#' @description
#' Generator of interval censored samples where the length of interval is random,
#'  given a generator of samples of the distribution X (rdistrX) with
#'  parameters appended by the list param_X. Generator sample
#'  of distribution C (censoring) with parameters appended by the list param_C
#'  In which, you can control the desired censorship percentage.
#'
#' @param rdistrX sample generator of distribution X. \cr
#' First argument number of samples, next arguments in param_X.
#' @param rdistrC sample generator of distribution C. \cr First argument number of samples, next arguments in param_C.
#' @param param_X list with parameters of rdistrX function.
#' @param param_C list with parameters of rdistrC function, one of these parameters \cr should be "lambda",
#'  this wil be the searched parameter.
#' @param n number of sample to create.
#' @param theta Desired censoring percentage
#' @param n_mc number of sample use to estimate the moments of C, greater n_mc more accuracy.
#' @param check if TRUE print a censoring percentage of new created database.
#' @param epsilon Parameter to estimate the number of visit (in [0,1]), shrink it only if the algorithm takes too long
#'
#' @return A list with sample data information: \tabular{ll}{
#'    \code{sample_censored} \tab vector of censored sample \cr
#'    \tab \cr
#'    \code{sample_uncensored} \tab vector of uncensored sample (original) \cr
#'    \tab \cr
#'    \code{censored_indicator} \tab vector of 1 and 0 indicating whether the i-th sample is censored  \cr
#'    \tab 1:= no censored, 0:= censored \cr
#'    \code{n_censored} \tab number of censored samples  \cr
#' }
#'
#' @seealso \code{\link{rcensT1}} for generate censorship sample type I.\cr
#' \code{\link{rcensT2}} for generate censorship sample type II.\cr
#' \code{\link{rcensT3}} for generate censorship sample type III\cr
#' \code{\link{rcensIfix}} for generate interval censoring sample
#'  with fix length interval
#'
#' @example man/examples/rcensI.R
## @example examples_plot/Example_rcensI_plot.R

#' @author Daniel Saavedra Morales
#' @export

rcensI <- function(rdistrX, rdistrC ,param_X, param_C,
                   n = 1e04, epsilon = .9, n_mc = 1e05,
                   theta = 1, check = TRUE){

  n1 = ceiling(n*theta) # Data con censura
  n2 = round((1-theta)*n) # Data no censurada

  sample_C_mc = do.call('rdistrC', c(n_mc, param_C))
  e_u = mean(sample_C_mc)
  var_u = var(sample_C_mc)

  x = do.call('rdistrX', c(n, param_X))

  censored =  matrix(0, nrow = n, ncol = 2,
                     dimnames = list(c(), c("Lower", "Upper")))
  censored[1:n2, 1] = x[1:n2]
  censored[1:n2, 2] = x[1:n2]
  delta = c(rep(1,n2), rep(0,n1))

  for (i in (n2 + 1):n){
    n_interval = ceiling(root_x2_p((1-epsilon)*e_u^2, - epsilon*var_u, - x[i]^2))
    u = c()
    while(sum(u) < x[i]){
      u = c(u, do.call('rdistrC', c(n_interval, param_C)))
    }

    acum = cumsum(u)
    index = which.max(acum > x[i])
    if (index == 1){ censored[i, 1] = 0}
    else{ censored[i, 1] = acum[index - 1]}
    censored[i, 2] = acum[index ]

  }
  if (check){cat("Censorship percentage:",1 - (sum (delta) / n ))}

  return(list( "sample_censored" = censored,
              "sample_uncensored" = x,
              "censored_indicator" = delta,
              "n_censored" = sum (delta == 0) ))
}


