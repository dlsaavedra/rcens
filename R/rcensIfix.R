#' @title Generate interval censoring sample (Fix)
#'
#' @description
#' Generator of interval censored samples where the length of interval is fixed,
#'  given a generator of samples of the distribution X (rdistrX) with
#'  parameters appended by the list param_X.
#'  In which, you can control the desired censorship percentage.
#'
#' @param rdistrX sample generator of distribution X. \cr
#' First argument number of samples, next arguments in param_X.
#' @param param_X list with parameters of rdistrX function.
#' @param interval_length length of interval
#' @param n number of sample to create.
#' @param theta Desired censoring percentage
#' @param check if TRUE print a censoring percentage of new created database.
#'
#' @return A list with sample data information: \tabular{ll}{
#'    \code{sample_censored} \tab vector of censored sample \cr
#'    \tab \cr
#'    \code{sample_uncensored} \tab vector of uncensored sample (original) \cr
#'    \tab \cr
#'    \code{censored_indicator} \tab vector of 1 and 0 indicating whether the i-th sample is censored  \cr
#'    \code{n_censored} \tab number of censored samples  \cr
#' }
#'
#' @seealso \code{\link{rcensT1}} for generate censorship sample type I.\cr
#' \code{\link{rcensT2}} for generate censorship sample type II.\cr
#' \code{\link{rcensT3}} for generate censorship sample type III\cr
#' \code{\link{rcensI}} for generate interval censoring sample
#'  with random length interval\cr
#'
#' @example man/examples/rcensIfix.R
#'
## @example examples_plot/Example_rcensIfix_plot.R

#' @author Daniel Saavedra Morales
#' @export
rcensIfix <- function(rdistrX ,param_X, interval_length,
                   n = 1e04, theta = 1, check = TRUE){


  n1 = ceiling(n*theta) # Data con censura
  n2 = round((1-theta)*n) # Data no censurada

  x = do.call('rdistrX', c(n, param_X))

  censored =  matrix(0, nrow = n, ncol = 2, dimnames = list(c(), c("Lower", "Upper")))
  censored[1:n2, 1] = x[1:n2]
  censored[1:n2, 2] = x[1:n2]
  delta = c(rep(1,n2), rep(0,n1))

  for (i in (n2 + 1):n){
    n_interval = ceiling(x[i]/interval_length)
    u = c()
    while(sum(u) < x[i]){
      u = c(u, rep(interval_length, n))
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

