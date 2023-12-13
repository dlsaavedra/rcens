#Example Exponential - Uniform

Salida = rcensI(rdistrX = rexp, rdistrC = runif,
               param_X = list("rate" = 2),
              param_C = list("min" = 0, "max" = 1),
             n = 1e02, theta = .9)

## Example with plot in examples_plot/Example_rcensI_plot.R
