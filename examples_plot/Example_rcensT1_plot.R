library(survival)
#source("R/rcensT1.R")
#Example Weibull - Uniform----
alpha = 2
beta = 1
scale = beta ** (-1/alpha )
theta = .2

## Right-Censored Plot KM
#Data = rcensT1(rdistrX = rweibull,param_X = list("shape" = alpha, "scale" = scale ),
 #                n = 1e02, t_censored = 1,right = TRUE)
Data = rcensT1(rdistrX = rweibull,param_X = list("shape" = alpha, "scale" = scale ),
                 n = 1e02, theta = theta, qdistrX = qweibull, right = TRUE)

S = Surv(Data$sample_censored,Data$censored_indicator, type = "right")
s1 = survfit(S ~ 1)

CDF_censored = ecdf(Data$sample_censored)
Survival_CDF = Vectorize(function(x){ 1 - CDF_censored(x)})
CDF_original= ecdf(Data$sample_uncensored)
Survival_CDF_original = Vectorize(function(x){ 1 - CDF_original(x)})

plot(Survival_CDF, col = "blue", xlim = c(0,2))
title("Survival Curve")
plot(Survival_CDF_original, col = "red", add= TRUE, xlim = c(0,2))
lines(s1$time, s1$surv, col = "green", xlim = c(0,2))
legend("topright",c("original","censured", "Survival_KM"),
       col = c("red", "blue", "green"), lty = 1)



## Left-Censored Plot KM
#Data = rcensT1(rdistrX = rweibull,param_X = list("shape" = alpha, "scale" = scale ),
 #                n = 1e02, t_censored = 1,right = FALSE)
Data = rcensT1(rdistrX = rweibull,param_X = list("shape" = alpha, "scale" = scale ),
                 n = 1e02, theta = theta, qdistrX = qweibull, right = FALSE)

S = Surv(Data$sample_censored,Data$censored_indicator, type = "left")
s1 = survfit(S ~ 1)

CDF_censored = ecdf(Data$sample_censored)
Survival_CDF = Vectorize(function(x){ 1 - CDF_censored(x)})
CDF_original= ecdf(Data$sample_uncensored)
Survival_CDF_original = Vectorize(function(x){ 1 - CDF_original(x)})

plot(Survival_CDF, col = "blue", xlim = c(0,2))
title("Survival Curve")
plot(Survival_CDF_original, col = "red", add= TRUE, xlim = c(0,2))
lines(s1$time, s1$surv, col = "green", xlim = c(0,2))
legend("topright",c("original","censured", "Survival_KM"),
       col = c("red", "blue", "green"), lty = 1)

