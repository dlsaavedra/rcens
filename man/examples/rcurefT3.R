
#Example Exponential - Uniform


Data_T3 = rcurefT3(rdistrX = rexp, pdistrC = punif, rdistrC = runif,
                   param_X = list("rate" = 2),
                   param_C = list("min" = 0, "max" = "lambda"),
                   n = 1e02, theta = .9, p = 0.2)




## Example with plot in examples_plot/Example_rcurefT3_plot.R
