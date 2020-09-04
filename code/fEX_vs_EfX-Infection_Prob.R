if("lamW" %in% rownames(installed.packages())==FALSE){install.packages("lamW"); require(lamW)}else{require(lamW)}
library(ggplot2)
# Generated data
lambda <- 3.10E-06
mu <- log(100)
sigsq <- log10(16)^2

rLISest <- function(n, mu = mu, sigsq = sigsq, lambda = lambda) {
  #IS Estimator of e^{-lambda X}
  lambdahat <- lambda * 10^(mu)
  Y <- rnorm(n,mean = 0, sd = sqrt(sigsq))
  Wu <- lambertW0(sigsq*lambdahat)
  Nu <- exp(-(Wu/sigsq)*(exp(Y)-1-Y))
  LIS <- exp(-((Wu^2) + 2*Wu)/(2*sigsq)) * mean(Nu)
  return(1 - LIS)
}

LISapprox <- function(mu = mu, sigsq = sigsq, lambda = lambda) {
  #IS Estimator of e^{-lambda X}
  lambdahat <- lambda * 10^(mu)
  Wu <- lambertW0(sigsq*lambdahat)
  LIS <- (1/sqrt(1+Wu))*exp(-((Wu^2) + 2*Wu)/(2*sigsq))
  return(1 - LIS)
}

lambda <- 3.10E-06
N <- 20
AppPInfect <- c()
ExPInfect <- c()
IS <- c()

Mu_Or_LogDose <- seq(0,5.25, length.out = N)
for (i in 1:N) {
  AppPInfect[i] = LISapprox(Mu_Or_LogDose[i], sigsq, lambda)
  ExPInfect[i] = 1-exp(-(3/10)*lambda * 10^(Mu_Or_LogDose[i] + sigsq/2))
  IS[i] <- rLISest(10000, Mu_Or_LogDose[i], sigsq, lambda)
}

ggplot() +
  geom_line(aes(x = Mu_Or_LogDose, y = AppPInfect, color = 'Eq. 6 (approx. exp. of funct.)')) + geom_point(aes(x = Mu_Or_LogDose, y = AppPInfect)) +
  geom_line(aes(x = Mu_Or_LogDose, y = ExPInfect, color = 'Eq. 5 (function of exp.)')) + geom_point(aes(x = Mu_Or_LogDose, y = ExPInfect)) +
  #geom_line(aes(x = Mu_Or_LogDose, y = IS, color = 'Importance Samp')) + geom_point(aes(x = Mu_Or_LogDose, y = IS)) +
  labs(x = 'Mean Log10(Dose)', y = 'Infection Probability', color = 'Method')+
  xlim(1,5) +
  ylim(0, ExPInfect[20])+
  theme_bw() +
  scale_x_continuous(name=expression(" Mean Log"[10]*"(Dose)")) +
  theme(legend.position = c(0.25,0.85),text = element_text(size=20))


