#' @title Generate Censoring Sample, Type II
#'
#' @description
#' Generator of censored samples type II with right or left censoring,
#'  given a generator of samples of the
#' distribution X (rdistrX) with parameters appended by the list param_X.
#'  In which, you can control the number of censored sample or the desired censorship percentage.
#'
#' @param rdistrX  sample generator of distribution X. \\ First argument number of samples, next arguments in param_X.
#' @param param_X  list with parameters of rdistrX function.
#' @param n        number of sample to create.
#' @param m_censored number of sample censored. m_censored < n. \\
#' If m_censored <= n, m_censored is estimate with the desired censoring percentage.
#'
#' @param theta Desired censoring percentage.
#' @param check if TRUE print a censoring percentage of new created database.
#' @param right if TRUE create right-censored data, else create left-censored
#'
#' @return A list with sample data information: \tabular{ll}{
#'    \code{sample_censored} \tab vector of censored sample \cr
#'    \tab \cr
#'    \code{sample_uncensored} \tab vector of uncensored sample (original) \cr
#'    \tab \cr
#'    \code{censored_indicator} \tab vector of 1 and 0 indicating whether the i-th sample is censored  \cr
#'    \tab 1:= no censored, 0:= censored \cr
#'    \code{censored_time} \tab vector of censorship time \cr
#'    \code{n_censored} \tab number of censored samples  \cr
#' }
#'
#' @seealso \code{\link{rcensT1}} for generate censorship sample type I.\cr
#' \code{\link{rcensT3}} for generate censorship sample type III\cr
#' \code{\link{rcensI}} for generate interval censoring sample
#'  with random length interval\cr
#' \code{\link{rcensIfix}} for generate interval censoring sample
#'  with fix length interval
#'
#' @example man/examples/rcensT2.R
## @example examples_plot/Example_rcensT2_plot.R
#' @author Daniel Saavedra Morales
#' @export

rcensT2 <- function(rdistrX ,param_X,
                    n = 1e04, m_censored = -1,
                    theta = .5, check = TRUE, right = TRUE)
{
  if (theta > 1 || theta < 0){
    warning("theta is not between 0 and 1 ")
    return()
  }
  if (m_censored >= n){
    m_censored = -1
    warning("Warning: m_censored >= n, use theta 0.5 for estimate m_censored")
  }
  if (m_censored < 0){
    m_censored = round(n*theta)
    cat("number of sample censored:", m_censored, "\n")
  }

  #Generate Sample of X
  x = do.call('rdistrX', c(n, param_X))

  delta <- rep (1, n)
  delta[(n - m_censored + 1): n] = 0

  if (right){
    t <- sort(x)
  }
  else{
    t <- sort(x, decreasing = TRUE)
  }

  t[(n - m_censored + 1): n] = t[n - m_censored]

  if (check){cat("Censorship percentage:",1 - (sum (delta) / n ), "\n")}

  return(list("sample_censored" = t,
              "sample_uncensored" = sort(x),
              "censored_indicator" = delta,
              "censored_time" = t[n - m_censored],
              "n_censored" =  m_censored))
}

