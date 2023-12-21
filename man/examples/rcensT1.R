## Example Exponential
## time censored

Data_T1 = rcensT1(rdistrX = rexp, param_X = list("rate" = 2),
                 n = 1e02, t_censored = 1)

## time censored estimate with desired censoring percentage.

Data_T1 = rcensT1(rdistrX = rexp, param_X = list("rate" = 2),
                 qdistrX = qexp, n = 1e02, theta = .8)

## Example with plot in examples_plot/Example_rcensT1_plot.R
