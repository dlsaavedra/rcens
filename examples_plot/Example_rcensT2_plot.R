library(survival)
#source("R/rcensT2.R")
#Example Weibull - Uniform----
alpha = 2
beta = 1
scale = beta ** (-1/alpha )
theta = .5

## Right-Censored Plot KM
Salida = rcensT2(rdistrX = rweibull,param_X = list("shape" = alpha, "scale" = scale ),
                 n = 1e02, theta = theta, right = TRUE)

S = Surv(Salida$sample_censored,Salida$censored_indicator, type = "right")
s1 = survfit(S ~ 1)

CDF_censored = ecdf(Salida$sample_censored)
Survival_CDF = Vectorize(function(x){ 1 - CDF_censored(x)})
CDF_original= ecdf(Salida$sample_uncensored)
Survival_CDF_original = Vectorize(function(x){ 1 - CDF_original(x)})

plot(Survival_CDF, col = "blue", xlim = c(0,2))
title("Survival Curve")
plot(Survival_CDF_original, col = "red", add= TRUE, xlim = c(0,2))
lines(s1$time, s1$surv, col = "green", xlim = c(0,2))
legend("topright",c("original","censured", "Survival_KM"),
       col = c("red", "blue", "green"), lty = 1)


## Left-Censored Plot KM
Salida = rcensT2(rdistrX = rweibull,param_X = list("shape" = alpha, "scale" = scale ),
                 n = 1e02, theta = theta, right = FALSE)

S = Surv(Salida$sample_censored,Salida$censored_indicator, type = "left")
s1 = survfit(S ~ 1)

CDF_censored = ecdf(Salida$sample_censored)
Survival_CDF = Vectorize(function(x){ 1 - CDF_censored(x)})
CDF_original= ecdf(Salida$sample_uncensored)
Survival_CDF_original = Vectorize(function(x){ 1 - CDF_original(x)})

plot(Survival_CDF, col = "blue", xlim = c(0,2))
title("Survival Curve")
plot(Survival_CDF_original, col = "red", add= TRUE, xlim = c(0,2))
lines(s1$time, s1$surv, col = "green", xlim = c(0,2))
legend("topright",c("original","censured", "Survival_KM"),
       col = c("red", "blue", "green"), lty = 1)
