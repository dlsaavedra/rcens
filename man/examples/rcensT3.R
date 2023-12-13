
#Example Exponential - Uniform


Salida = rcensT3(rdistrX = rexp, pdistrC = punif, rdistrC = runif,
              param_X = list("rate" = 2),
             param_C = list("min" = 0, "max" = "lambda"),
            n = 1e02, theta = .9, right = TRUE)


Salida = rcensT3(rdistrX = rexp, pdistrC = punif, rdistrC = runif,
                 param_X = list("rate" = 2),
                 param_C = list("min" = 0, "max" = "lambda"),
                 n = 1e02, theta = .1, right = FALSE)


## Example with plot in examples_plot/Example_rcensT3_plot.R

