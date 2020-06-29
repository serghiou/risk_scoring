FracAsympt <- 0.15

GenSurvP <- function(x) {
  F <- function(x) {
    plnorm(x,meanlog = log(5.8), sdlog = log(1.80))
  }
  return(1 - F(x))
}


GenDayOnsetP <- function(x) {
  return(GenSurvP(x-1) - GenSurvP(x))
}

GenCondDayOnsetP <- function(x) {
  return((GenSurvP(x-1) - GenSurvP(x))/GenSurvP(x-1))
}

AsymptSurvP <- function(x) {
  F <- function(x) {
    plnorm(x,meanlog = log(7.7), sdlog = log(1.37))
  }
  return(1 - F(x))
}

AsymptDayOnsetP <- function(x) {
  return(AsymptSurvP(x-1) - AsymptSurvP(x))
}

AsymptCondDayOnsetP <- function(x) {
  return((AsymptSurvP(x-1) - AsymptSurvP(x))/AsymptSurvP(x-1))
}


shedOverP <- c(1,0.986486486,0.986486486,0.986486486,0.986486486,0.986486486,0.986486486,0.986486486,0.986486486,0.986486486,0.972972973,0.959459459,0.959459459,0.959459459,0.945945946,0.905405405,0.891891892,0.864864865,0.851351351,0.824324324,0.783783784,0.756756757,0.702702703,0.689189189,0.662162162,0.662162162,0.621621622,0.554054054,0.540540541,0.486486486,0.432432432,0.378378378,0.310810811,0.256756757,0.216216216,0.189189189,0.148648649,0.121621622,0.094594595,0.054054054,0,0,0,0,0)
shedOverP <- sort(shedOverP)
M <- length(shedOverP) - 1

AsymptshedOver <- c()
for (i in 0:M) {
  AsymptshedOver[i+1] <- AsymptDayOnsetP(0:(i)) %*% shedOverP[1:(i+1)]
}

SurvShed <- c()
for (i in 0:M) {
  SurvShed[i+1] <- 1 - (1 - FracAsympt) * (1 - GenSurvP(i)) - FracAsympt * AsymptshedOver[i+1]
}



df.shedding <- data.frame(SurvGeneral <- GenSurvP(0:M), GenDayOnset <- GenDayOnsetP(0:M), GenCondDayOnset <- GenCondDayOnsetP(0:M), GenShedOver <- shedOverP, SurvAsympt <- AsymptSurvP(0:M), AsymptDayOnset <- AsymptDayOnsetP(0:M), AsymptCondDayOnset <- AsymptCondDayOnsetP(0:M), AsymptShedOver <- AsymptshedOver, SurvShedTot <- SurvShed)
names(df.shedding) <- c('GenSurvP', 'GenDayOnsetP', 'GenCondDayOnsetP', 'GenShedFinishedP', 'AsymptSurvP', 'AsymptDayOnsetP', 'AsymptCondDayOnsetP', 'AsymptShedFinishedP','SurvShedTotP')
df.shedding

write.csv(df.shedding, file = 'MetaSheddingProb.csv')
