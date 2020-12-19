library(cvar)
library(CTE)

# norm

get_norm_var_es_tvar <- function(mean, sd){
  num_of_samples <- 1000000
  var <- VaR(qnorm, mean=mean, sd=sd)
  es <- ES(qnorm, mean=mean, sd=sd)
  distr <- rnorm(num_of_samples, mean=mean, sd=sd)
  tvar <- ifelse(min(distr) < 0, mean(distr[distr < var]), mean(distr[distr > var]))
  return(c(var, es, tvar))
}

mean1 <- -3
mean2 <- 6
sd1 <- 5
sd2 <- 3

x <- seq(-20, 20, 0.1)
params1 <- get_norm_var_es_tvar(mean1, sd1)
params2 <- get_norm_var_es_tvar(mean2, sd2)

distr1 <- data.frame(rating = dnorm(x, mean=mean1, sd=sd1), x=x)
distr2 <- data.frame(rating = dnorm(x, mean=mean2, sd=sd2), x=x)

                            
ggplot() +
  geom_line(data=distr1, aes(x=x, y=rating), color = "red") + 
  geom_vline(xintercept = params1[1], color="red", alpha=0.5) + 
  geom_text(aes(x=params1[1], label="\nVaR", y=0.03, angle=90), colour="red") +
  geom_vline(xintercept = params1[2], color="red", alpha=0.5) + 
  geom_text(aes(x=params1[2], label="\nES", y=0.03, angle=90), colour="red") +
  geom_vline(xintercept = params1[3], color="red", alpha=0.5) + 
  geom_text(aes(x=params1[3], label="\nTVaR", y=0.03, angle=90), colour="red") +
  
  geom_line(data=distr2, aes(x=x, y=rating), color = "blue") + 
  geom_vline(xintercept = params2[1], color="blue", alpha=0.5) + 
  geom_text(aes(x=params2[1], label="\nVaR", y=0.03, angle=90), colour="blue") +
  geom_vline(xintercept = params2[2], color="blue", alpha=0.5) + 
  geom_text(aes(x=params2[2], label="\nES", y=0.03, angle=90), colour="blue") +
  geom_vline(xintercept = params2[3], color="blue", alpha=0.5) + 
  geom_text(aes(x=params2[3], label="\nTVaR", y=0.03, angle=90), colour="blue")
  


# stable

get_stable_var_es_tvar <- function(alpha, beta, gamma, delta){
  num_of_samples <- 1000000
  distr <- rstable(num_of_samples, alpha=alpha, beta=beta, gamma=gamma, delta=delta)
  var <- var <- quantile(distr, 0.05)
  es <- ifelse(min(x) < 0, mean(x[x < 0]), mean(distr))
  tvar <- ifelse(min(distr) < 0, mean(distr[distr < var]), mean(distr[distr > var]))
  return(c(var, es, tvar))
}

alpha1 <- 0.5
alpha2 <- 0.9
beta1 <- 0.5
beta2 <- 0
gamma1 <- 2
gamma2 <- 1
delta1 <- -1
delta2 <- 0

x <- seq(-30, 30, 0.1)
params1 <- get_stable_var_es_tvar(alpha=alpha1, beta=beta1, gamma=gamma1, delta=delta1)
params2 <- get_stable_var_es_tvar(alpha=alpha2, beta=beta2, gamma=gamma2, delta=delta2)

distr1 <- data.frame(rating = dstable(x, alpha=alpha1, beta=beta1, gamma=gamma1, delta=delta1), x=x)
distr2 <- data.frame(rating = dstable(x, alpha=alpha2, beta=beta2, gamma=gamma2, delta=delta2), x=x)


ggplot() +
  geom_line(data=distr1, aes(x=x, y=rating), color = "red") + 
  geom_vline(xintercept = params1[1], color="red", alpha=0.5) + 
  geom_text(aes(x=params1[1], label="\nVaR", y=0, angle=90), colour="red") +
  geom_vline(xintercept = params1[2], color="red", alpha=0.5) + 
  geom_text(aes(x=params1[2], label="\nES", y=0, angle=90), colour="red") +
  geom_vline(xintercept = params1[3], color="red", alpha=0.5) + 
  geom_text(aes(x=params1[3], label="\nTVaR", y=0, angle=90), colour="red") +
  
  geom_line(data=distr2, aes(x=x, y=rating), color = "blue") + 
  geom_vline(xintercept = params2[1], color="blue", alpha=0.5) + 
  geom_text(aes(x=params2[1], label="\nVaR", y=0, angle=90), colour="blue") +
  geom_vline(xintercept = params2[2], color="blue", alpha=0.5) + 
  geom_text(aes(x=params2[2], label="\nES", y=0, angle=90), colour="blue") + 
  geom_vline(xintercept = params2[3], color="blue", alpha=0.5) + 
  geom_text(aes(x=params2[3], label="\nTVaR", y=0, angle=90), colour="blue") +
  xlim(-30, 30)


# CTS

get_cts_var_es_tvar <- function(cts_alpha, c, lambda, mu){
  num_of_samples <- 10000
  distr <- rCTS(num_of_samples, alpha=cts_alpha, c=c, ell=lambda, mu = mu)
  var <- quantile(distr, 0.05)
  es <- ifelse(min(distr) < 0, mean(distr[distr < 0]), mean(distr))
  tvar <- ifelse(min(distr) < 0, mean(distr[distr < var]), mean(distr[distr > var]))
  return(c(var, es, tvar))
}

x <- seq(-30, 30, 0.1)
params1 <- get_cts_var_es_tvar(cts_alpha1, c1, lambda1, mu1)
params2 <- get_cts_var_es_tvar(cts_alpha2, c2, lambda2, mu2)

distr1 <- data.frame(rating = dCTS(x, alpha=cts_alpha1, c=c1, ell=lambda1, mu = mu1), x=x)
distr2 <- data.frame(rating = dstable(x, alpha=cts_alpha2, c=c2, ell=lambda2, mu = mu2), x=x)


ggplot() +
  geom_line(data=distr1, aes(x=x, y=rating), color = "red") + 
  geom_vline(xintercept = params1[1], color="red", alpha=0.5) + 
  geom_text(aes(x=params1[1], label="\nVaR", y=0, angle=90), colour="red") +
  geom_vline(xintercept = params1[2], color="red", alpha=0.5) + 
  geom_text(aes(x=params1[2], label="\nES", y=0, angle=90), colour="red") +
  geom_vline(xintercept = params1[3], color="red", alpha=0.5) + 
  geom_text(aes(x=params1[3], label="\nTVaR", y=0, angle=90), colour="red") +
  
  geom_line(data=distr2, aes(x=x, y=rating), color = "blue") + 
  geom_vline(xintercept = params2[1], color="blue", alpha=0.5) + 
  geom_text(aes(x=params2[1], label="\nVaR", y=0, angle=90), colour="blue") +
  geom_vline(xintercept = params2[2], color="blue", alpha=0.5) + 
  geom_text(aes(x=params2[2], label="\nES", y=0, angle=90), colour="blue") + 
  geom_vline(xintercept = params2[3], color="blue", alpha=0.5) + 
  geom_text(aes(x=params2[3], label="\nTVaR", y=0, angle=90), colour="blue") +
  xlim(-30, 30)

