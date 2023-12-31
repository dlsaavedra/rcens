##Example Exponential

## Number of sample censored
Data_T2 = rcensT2(rdistrX = rexp, param_X = list("rate" = 2), n = 1e02, m_censored = 9)

## Number of censored sample estimate with desired censoring percentage.
Data_T2 = rcensT2(rdistrX = rexp, param_X = list("rate" = 2), n = 1e02, theta = .8)


## Example with plot in examples_plot/Example_rcensT2_plot.R
