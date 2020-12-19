library(comprehenr)
library(ggplot2)
library(grid)
library(gridExtra)
library(rmutil)
library(stabledist)
library(SymTS)


set.seed(1234)
g1 <- function(x) x ^ 2
g2 <- function(x) (x + x ^ 6) / 2
g3 <- function(x)(x + x ^ (6 / 5)) * exp(1 - x) / 2
pi1 <- function(dist_func, g) {
  return(integrate(
    function(t) g(1 - dist_func(t)),
    lower = 0,
    upper = +Inf,
    stop.on.error = FALSE
  )$value)
}
pi2 <- function(dist_func, g) {
  return(integrate(
    function(t) g(1 - dist_func(t)) - 1,
    lower = -Inf,
    upper = 0,
    stop.on.error = FALSE
  )$value + integrate(
    function(t) g(1 - dist_func(t)),
    lower = 0,
    upper = +Inf,
    stop.on.error = FALSE
  )$value)
}
pi_mera <- function(dist_func) {
  return(data.frame(
    pi1.g1 = pi1(dist_func, g1),
    pi1.g2 = pi1(dist_func, g2),
    pi1.g3 = pi1(dist_func, g3),
    pi2.g1 = pi2(dist_func, g1),
    pi2.g2 = pi2(dist_func, g2),
    pi2.g3 = pi2(dist_func, g3)
  ))
}

mean1 <- -3
mean2 <- 0
mean3 <- 3
sd1 <- 1
sd2 <- 0.7
sd3 <- 0.5

alpha1 <- 0.5
alpha2 <- 0.9
alpha3 <- 2
beta1 <- 0.5
beta2 <- 0
beta3 <- 1
gamma1 <- 2
gamma2 <- 1
gamma3 <- 3
delta1 <- -1
delta2 <- 0
delta3 <- 1

entire_mera_df <- data.frame()
entire_mera_df <- rbind(entire_mera_df,
  Norm_m3_1 = pi_mera(function(t) pnorm(t, mean = mean1, sd = sd1)),
  Norm_0_1 = pi_mera(function(t) pnorm(t, mean = mean2, sd = sd1)),
  Norm_3_1 = pi_mera(function(t) pnorm(t, mean = mean3, sd = sd1)),
  Norm_0_1 = pi_mera(function(t) pnorm(t, mean = mean2, sd = sd1)),
  Norm_0_0.7 = pi_mera(function(t) pnorm(t, mean = mean2, sd = sd2)),
  Norm_0_0.5 = pi_mera(function(t) pnorm(t, mean = mean2, sd = sd3)),
  Stable_0.5_0_1_0 = pi_mera(function(t) pstable(t, alpha=alpha1, beta=beta2, gamma=gamma2, delta = delta2)),
  Stable_0.9_0_1_0 = pi_mera(function(t) pstable(t, alpha=alpha2, beta=beta2, gamma=gamma2, delta = delta2)),
  Stable_2_0_1_0 = pi_mera(function(t) pstable(t, alpha=alpha3, beta=beta2, gamma=gamma2, delta = delta2)),
  Stable_0.9_0.5_1_0 = pi_mera(function(t) pstable(t, alpha=alpha2, beta=beta1, gamma=gamma2, delta = delta2)),
  Stable_0.9_0_1_0 = pi_mera(function(t) pstable(t, alpha=alpha2, beta=beta2, gamma=gamma2, delta = delta2)),
  Stable_0.9_1_1_0 = pi_mera(function(t) pstable(t, alpha=alpha2, beta=beta3, gamma=gamma2, delta = delta2)),
  Stable_0.9_0_2_0 = pi_mera(function(t) pstable(t, alpha=alpha2, beta=beta2, gamma=gamma1, delta = delta2)),
  Stable_0.9_0_1_0 = pi_mera(function(t) pstable(t, alpha=alpha2, beta=beta2, gamma=gamma2, delta = delta2)),
  Stable_0.9_0_3_0 = pi_mera(function(t) pstable(t, alpha=alpha2, beta=beta2, gamma=gamma3, delta = delta2)),
  Stable_0.9_0_1_m1 = pi_mera(function(t) pstable(t, alpha=alpha2, beta=beta2, gamma=gamma2, delta = delta1)),
  Stable_0.9_0_1_0 = pi_mera(function(t) pstable(t, alpha=alpha2, beta=beta2, gamma=gamma2, delta = delta2)),
  Stable_0.9_0_1_1 = pi_mera(function(t) pstable(t, alpha=alpha2, beta=beta2, gamma=gamma2, delta = delta3))
)
entire_mera_df
