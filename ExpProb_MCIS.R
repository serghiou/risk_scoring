if("lamW" %in% rownames(installed.packages())==FALSE){install.packages("lamW"); require(lamW)}else{require(lamW)}

# Generated data
rLISest <- funtion(n, mu, sigsq, lambda) {
  #IS Estimator of e^{-lambda X}
  Y <- rnorm(n,mean = mu, sd = sigsq)
  Wu <- lambertW0(sigsq*lambda)
  Nu <- exp(-(Wu^2)*(exp(Y)-1-Y))
  LIS <- 1 - exp(-lambda * mu)*exp(-((Wu^2) * 2*Wu)/(2*sigsq))*Nu
  return(LIS)
}

# Given data
LISest <- funtion(dtdose) {
  #IS Estimator of e^{-lambda X}
  mu <- mean(log(dtdose))
  sigsq <- sd(log(dtDose))
  Y <- (log(dtDose)-mu)
  lambda <- 10/100
  Wu <- lambertW0(sigsq*lambda)
  Nu <- exp(-(Wu^2)*(exp(Y)-1-Y))
  LIS <- 1 - exp(-lambda * mu)*exp(-((Wu^2) * 2*Wu)/(2*sigsq))*Nu
  return(LIS)
}