proj_dir <- '/Users/andy/Dropbox/TSCreator/TSCreator_development/Developers/Andy/Projects/Phanerozoic_Data_Mining'
# datapack directory
dp_dir <- '/Users/andy/Dropbox/TSCreator/TSCreator_development/Developers/Andy/Projects/Phanerozoic_Data_Mining/Datapack/'
setwd(dp_dir)
dp_fname <- 'MarineGenera_13Jan13.xls'

dff_backup <- dff
AGE_SLIDE <- 0.5 # 500Ka
AGE_DIFF = NA
if (floor(AGE_SLIDE) == 0) {
  AGE_DIFF = paste(AGE_SLIDE * 1000, "K", sep="")
} else {
  AGE_DIFF = paste(AGE_SLIDE, "M", sep="")
}
pfname <- paste(getwd(),"/marine_genera_event_extraction", "/marine_genera_speciation_extinction_age_slide_", AGE_DIFF, ".csv", sep="")
pfname

# Read the marine genera bin wise evolution frequency/turnover data
dff <- read.csv(file=pfname)

head(dff)
datatable(dff)
colnames(dff)

turnProb = data.frame(age=dff$age, turnProb=dff$HMM.turnover.probability)
spec = mtm(turnProb, output=1)
spec$Period = 1/spec$Frequency
spec <- spec[order(spec$Harmonic_CL, decreasing = T),]
head(spec, 50)
turnPwr=eha(turnProb, win=30.001, fmin=0, fmax=1.5, #step=0.2,
              pad=4000, 
              genplot=4, palette=5, pl=2, ydir=-1, 
              tbw=2, output=2)

# Extract only Cenozoic
CENOZOIC_BASE <- 67
df_C <- df[df$age > 0 & df$age <= CENOZOIC_BASE, ]
tail(df_C)
library(astrochron)
turnProb_C = data.frame(age=df_C$age, turnProb=df_C$HMM.turnover.probability)
spec_C = mtm(turnProb_C, output=1)
spec_C$Period = 1/spec_C$Frequency
spec_C <- spec_C[order(spec_C$Harmonic_CL, decreasing = T),]
head(spec_C, 50)
turnPwr_C=eha(turnProb_C, win=20, fmin=0, fmax=1, #step=0.2,
            pad=4000, 
            genplot=4, palette=5, pl=2, ydir=-1, 
            tbw=2, output=2)

MESOZOIC_BASE <- 252
df_M <- df[df$age > CENOZOIC_BASE & df$age <= MESOZOIC_BASE, ]
head(df_M)
tail(df_M)
turnProb_M = data.frame(age=df_C$age, turnProb=df_C$HMM.turnover.probability)
spec_M = mtm(turnProb_M, output=1)
spec_M$Period = 1/spec_M$Frequency
spec_M <- spec_M[order(spec_M$Harmonic_CL, decreasing = T),]
head(spec_M, 60)

turnPwr_M=eha(turnProb_M, win=20.001, fmin=0, fmax=1, #step=0.2,
            pad=4000, 
            genplot=4, palette=5, pl=2, ydir=-1, 
            tbw=2, output=2)

PALEOZOIC_BASE <- 541
df_P <- df[df$age > MESOZOIC_BASE & df$age <= PALEOZOIC_BASE, ]
head(df_P)
tail(df_P)
turnProb_P = data.frame(age=df_P$age, turnProb=df_P$HMM.turnover.probability)
spec_P = mtm(turnProb_P, output=1)
spec_P$Period = 1/spec_P$Frequency
spec_P <- spec_P[order(spec_P$Harmonic_CL, decreasing = T),]
head(spec_P, 50)

turnPwr_P=eha(turnProb_P, win=20.001, fmin=0, fmax=1, #step=0.2,
              pad=4000, 
              genplot=4, palette=5, pl=2, ydir=-1, 
              tbw=2, output=2)

# Change the dff here depending on the time period being focused on
dff <- df_C

#png('turnover.png')
par(mfrow=c(9,1))
par(mar=c(1,4,1,1))
plot(-dff$age, dff$N.speciations, t='l', col='green', 
     xlab='Ma', ylab='N speciation')
plot(-dff$age, dff$raw.speciation.probability, t='l', col='green',
     xlab='Ma', ylab='Raw spec. prob')
plot(-dff$age, dff$HMM.speciation.state.probability, t='l', col='green',
     xlab='Ma', ylab='HMM spec. prob')
plot(-dff$age, dff$N.extinctions, t='l', col='red', 
     xlab='Ma', ylab='N extinction')
plot(-dff$age, dff$raw.extinction.probability, t='l', col='red',
     xlab='Ma', ylab='Raw exti. prob')
plot(-dff$age, dff$HMM.extinction.state.probability, t='l', col='red',
     xlab='Ma', ylab='HMM exti. prob')
plot(-dff$age, dff$N.turnover, t='l', col='black',
     xlab='Ma', ylab='N turnover')
plot(-dff$age, dff$raw.turnover.probability, t='l', col='black',
     xlab='Ma', ylab='Raw turn. prob')
plot(-dff$age, dff$HMM.turnover.probability, t='l', col='black',
     xlab='Ma', ylab='HMM turn. prob')

# Calculate HMM turnover probability
# Find the best value for hidden state m
# m that minimizes the AIC

# R code to compute basic hidden Markov model from user-supplied vectors of:
#   count of speciations or extinctions at each pseudolevel ("counts"), and
#   count of species extant at each pseudolevel ("pn")

# Function to initialize Pi matrix required by "dthmm"
# Initialized using random numbers from normal distribution
# Assume that values on diagonal ~0.4
# Rows must sum to 1
setPi <- function(m) {  
  Pi <- matrix(data=NA, nrow=m, ncol=m, byrow=T)  
  for(p in 1:m){
    Pi[p,p] <- rnorm(1, mean=0.4, sd=0.1) 
    Pi[p,-p] <- (1-Pi[p,p])/(m-1)
  }
  return(Pi)
}

# Function to initialize delta and pm vectors required by "dthmm"
# Initialized using random numbers from normal distribution
# Delta must sum to 1
setDPm <- function(m) {                 
  DPm <- vector(mode="numeric", length=m)
  DPm[1] <- rnorm(1, mean=0.4, sd=0.1)
  DPm[-1] <- (1-DPm[1])/(m-1)
  return(DPm)
}
# Fit hidden Markov model
# "m" is the number of discrete states in the model – this must be set by the user
# "counts" is the user-supplied vector of speciations or extinctions at each pseudolevel
# "pn" is the user-supplied vector of species extant at each pseudolevel
# Note: by default, the output states are not ordered by magnitude
library(HiddenMarkov)


# be careful with whether loading from file or generation
#counts <- as.numeric(dff$N.speciations)
#pn <- as.numeric(dff$N.species.speciation.)
counts <- as.numeric(dff$N.speciations)
head(counts)
pn <- as.numeric(dff$N.species.speciation)
head(pn)

# Find the best state for speciation
mlist <- seq(2, 10)
AICv <- c()
for (m in mlist) {
  x <- dthmm(counts, Pi=setPi(m), delta= setDPm(m), pm= list(prob=setDPm(m)), 
           pn=list(size=pn), distn="binom", discrete=TRUE)
  y <- BaumWelch(x)               # estimated parameters of the HMM
  n.param <- (m*(m-1))+2*m-1      # number of parameters in the model
  log.lik.y <- logLik(y)          # log likelihood of model y
  AIC.y <- 2*n.param-2*log.lik.y  # Akaike’s Information Criterion for model y
  AICv <- c(AICv, AIC.y)
}

par(mar=c(4,4,4,2))
par(mfrow=c(1,1))
plot(mlist, AICv, t='l', 
     xlab='Hidden state, m',
     ylab='AIC',
     main='Model comparison for speciation')
m.speciation <- which.min(AICv) + 1

# Use the best hidden state to get HMM series
m <- m.speciation                          # specify number of states in the model
m <- 5 # 5 for 100K interval
counts <- dff$N.speciations
head(counts)
pn <- dff$N.species.speciation
head(pn)
x <- dthmm(counts, Pi=setPi(m), delta= setDPm(m), pm= list(prob=setDPm(m)), 
           pn=list(size=pn), distn="binom", discrete=TRUE)
y <- BaumWelch(x)               # estimated parameters of the HMM

# Predict most likely sequence of states given observations and model y
v <- Viterbi(y)                 # predicted sequence of HMM states at each pseudolevel

dff$`HMM.speciation.state` <- v
dff$`HMM.speciation.state.probability` <- y$pm$prob[v]

counts <- dff$N.extinctions
head(counts)
pn <- dff$N.species.extinction
head(pn)

# Find the best state for extinction
mlist <- seq(2, 10)
AICv <- c()
for (m in mlist) {
  #m <- 4                          # specify number of states in the model
  
  x <- dthmm(counts, Pi=setPi(m), delta= setDPm(m), pm= list(prob=setDPm(m)), 
             pn=list(size=pn), distn="binom", discrete=TRUE)
  y <- BaumWelch(x)               # estimated parameters of the HMM
  n.param <- (m*(m-1))+2*m-1      # number of parameters in the model
  log.lik.y <- logLik(y)          # log likelihood of model y
  AIC.y <- 2*n.param-2*log.lik.y  # Akaike’s Information Criterion for model y
  AICv <- c(AICv, AIC.y)
}

par(mar=c(4,4,4,2))
par(mfrow=c(1,1))
plot(mlist, AICv, t='l', 
     xlab='Hidden state, m',
     ylab='AIC',
     main='Model comparison for extinction')
m.extinction <- which.min(AICv) + 1
m.extinction


# 3 state extinction
# 5 state extinction for 100K interval of marine genera events
m <- m.extinction                          # specify number of states in the model

counts <- dff$N.extinctions
pn <- dff$N.species.extinction
x <- dthmm(counts, Pi=setPi(m), delta= setDPm(m), pm= list(prob=setDPm(m)), 
           pn=list(size=pn), distn="binom", discrete=TRUE)
y <- BaumWelch(x)               # estimated parameters of the HMM

# Predict most likely sequence of states given observations and model y
v <- Viterbi(y)                 # predicted sequence of HMM states at each pseudolevel

dff$`HMM.extinction.state` <- v
dff$`HMM.extinction.state.probability` <- y$pm$prob[v]

dff$`HMM.turnover.probability` <- dff$`HMM.speciation.state.probability` + dff$`HMM.extinction.state.probability`

library(DT)
datatable(dff)

AGE_DIFF = 0.1
if (floor(AGE_SLIDE) == 0) {
  AGE_DIFF = paste(AGE_SLIDE * 1000, "K", sep="")
} else {
  AGE_DIFF = paste(AGE_SLIDE, "M", sep="")
}

pfname <- paste(getwd(),"/marine_genera_event_extraction", "/cenozoic_marine_genera_speciation_extinction_age_slide_", AGE_DIFF, ".csv", sep="")
pfname
write.csv(dff, file=pfname, col.names = TRUE)


# Read the marine genera bin wise evolution frequency/turnover data
dff <- read.csv(file=pfname)
head(dff)
colnames(dff)

# R code to generate surrogate simulations in order to assess suitability of 
#   spectral analysis procedures provided in Astrochron

# Load the library Astrochron
library(astrochron)

library(astsa)
ar1_noise_model <- sarima(dff$HMM.turnover.probability, 1, 0, 0)
ar1_ttable = ar1_noise_model$ttable
rho = ar1_ttable[1,1] 
rho
npts = dim(dff)[1]
dt = 0.1

par(mar=c(4,4,4,4))
# Conduct simulations using AR1 surrogates, and the Periodogram-AR1 approach
testBackground(npts=npts, dt=dt, noiseType="ar1", coeff= 0.3082017, 
               method="periodogramAR1", detrend=T,iter=2000)

# Conduct simulations using AR1 surrogates and the MTM-AR1 approach
testBackground(npts=npts, dt=dt, noiseType="ar1", coeff=rho, 
               method="mtmAR1", detrend=T, tbw=2, iter=2000)

# Conduct simulations using AR1 surrogates and the MTM-ML96 approach
testBackground(npts=npts, dt=dt, noiseType="ar1", coeff=rho, 
               method="mtmML96",detrend=T, tbw=2, iter=2000)

# Conduct simulations using AR1 surrogates and the LOWSPEC approach
testBackground(npts=npts, dt=dt, noiseType="ar1", coeff=rho, 
               method="lowspec",detrend=T, tbw=2, iter=2000)

mtmPL(data.frame(dff$age, dff$HMM.turnover.probability))
beta = -0.2311157

# Conduct simulations using power law surrogates, and the MTM-PL approach
testBackground(npts=npts, dt=dt, noiseType="pwrLaw", coeff=beta, 
               method="mtmPL",detrend=T, tbw=2, iter=2000)

# Conduct simulations using power law surrogates, and the Periodogram-PL approach
testBackground(npts=npts, dt=dt, noiseType="pwrLaw", coeff= beta, 
               method="periodogramPL", detrend=T,iter=2000)

# Conduct simulations using power law surrogates and the LOWSPEC approach
testBackground(npts=npts, dt=dt, noiseType="pwrLaw", coeff=beta, 
               method="lowspec",detrend=T, tbw=2, iter=2000)



# R code to generate surrogate simulations in order to assess suitability of 
#   spectral analysis procedures provided in Astrochron: testing the 
#   use of AR1-based models to analyze power law surrogates, and vice versa

# Load the library Astrochron
library(astrochron)

# Conduct simulations using power law surrogates and MTM-AR1 approach
testBackground(npts=npts, dt=dt, noiseType="pwrLaw", coeff=beta, 
               method="mtmAR1", detrend=T,tbw=2,iter=2000, genplot = T)

# Conduct simulations using power law surrogates and the MTM-ML96 approach
testBackground(npts=npts, dt=dt, noiseType="pwrLaw", coeff=beta, 
               method="mtmML96", detrend=T,tbw=2, iter=2000)

# Conduct simulations using power law surrogates and the LOWSPEC approach
testBackground(npts=npts, dt=dt, noiseType="pwrLaw", coeff=beta, 
               method="lowspec", detrend=T,tbw=2, iter=2000)

# Conduct simulations using AR1 surrogates, and the MTM-PL approach
testBackground(npts=npts, dt=dt, noiseType="ar1", coeff= rho, 
               method="mtmPL", detrend=T, tbw=2, iter=2000)

# Conduct simulations using AR1 surrogates, and the Periodogram-PL approach
testBackground(npts=npts, dt=dt, noiseType="ar1", coeff= rho, 
               method="periodogramPL", detrend=T, tbw=2, iter=2000)

# Conduct simulations using power law surrogates, and the Periodogram-AR1 approach
testBackground(npts=npts, dt=dt, noiseType="pwrLaw", coeff=rho, 
               method="periodogramAR1", detrend=T, iter=2000)


# HMM turnover probabilities should be placed in the data frame "turnProb"
# Download these data from the Astrochron server
turnProb=getData("graptolite")

# Load the library Astrochron
library(astrochron)

turnProb = data.frame(age=dff$age, turnProb=dff$HMM.turnover.probability)

# Conduct LOWSPEC analysis
specLow=lowspec(turnProb, detrend=T, tbw=2, padfac=1, pl=2, sigID=F, output=1)

# Conduct MTM-PL analysis
specPL1
  
mtmAR1(turnProb, detrend=T, tbw=2, padfac=1, pl=2)#, sigID=F, output=1)

  
mtmPL(turnProb, detrend=T, tbw=2, padfac=1, pl=2, sigID=F, output=1)

# Conduct Periodogram-AR1 analysis
specAR1=
  periodogram(cosTaper(turnProb, demean=T, detrend=T), demean=F, 
                    background=1, fNyq=F, padfac=1, output=1)
# Conduct Periodogram-PL analysis
specPL2=periodogram(cosTaper(turnProb, demean=T, detrend=T), demean=F, 
                    background=2, fNyq=F, padfac=1, output=1)

# R code for Bonferroni multiple test correction of spectral analyses

# Define the frequency resolution bands for LOWSPEC associated with predicted 
#   ~1.2 Myr and ~2.4 Myr grand cycles;  
#   these are slightly larger than for the other methods, due to prewhitening
flow=c((1/2.4)-0.0322841,(1/1.2)-0.0322841)
fhigh=c((1/2.4)+0.0322841,(1/1.2)+0.0322841)

# Apply the Bonferroni correction to the LOWSPEC confidence levels 
confAdjust(specLow, npts=npts, dt=dt, tbw=2, ntap=3, pl=2, 
           flow=flow, fhigh=fhigh, 
           output=F) 

# Define the frequency resolution bands for MTM-PL, associated with predicted 
#   ~1.2 Myr and ~2.4 Myr grand cycles
flow=c((1/2.4)-0.03225806,(1/1.2)-0.03225806)
fhigh=c((1/2.4)+0.03225806,(1/1.2)+0.03225806)

# Apply the Bonferroni correction to the MTM-PL confidence levels 
confAdjust(specPL1, npts=npts, dt=dt, tbw=2, ntap=3, pl=2, flow=flow, fhigh=fhigh) 

# Apply the Bonferroni correction to the Periodogram-AR1 confidence levels 
confAdjust(specAR1, npts=npts, dt=dt, tbw=1, ntap=1, pl=2, flow=flow, fhigh=fhigh) 

# Apply the Bonferroni correction to the Periodogram-PL confidence levels 
confAdjust(specPL2, npts=npts, dt=dt, tbw=1, ntap=1, pl=2, flow=flow, fhigh=fhigh) 


# R code for multiple test corrections of spectral analyses
# Uses objects created by Dataset S5

# Define the frequency resolution bands for LOWSPEC associated with predicted 
#   ~1.2 Myr and ~2.4 Myr grand cycles
flow=c((1/2.4)-0.0322841,(1/1.2)-0.0322841)
fhigh=c((1/2.4)+0.0322841,(1/1.2)+0.0322841)

# Multiple test corrections using LOWSPEC
multiTest(specLow, flow=flow, fhigh=fhigh)

# Define the frequency resolution bands for MTM-PL associated with predicted 
#   ~1.2 Myr and ~2.4 Myr grand cycles
flow=c((1/2.4)-0.03225806,(1/1.2)-0.03225806)
fhigh=c((1/2.4)+0.03225806,(1/1.2)+0.03225806)

# Multiple test corrections using MTM PL
multiTest(specPL1, flow=flow, fhigh=fhigh)

# Multiple test corrections using periodogram-AR1
multiTest(specAR1, flow=flow, fhigh=fhigh)
# Multiple test corrections using periodogram-PL
multiTest(specPL2, flow=flow, fhigh=fhigh)

# HMM turnover probabilities should be placed in the data frame "turnProb"
# Conduct MTM spectral analysis with LOWSPEC background, and a finer frequency grid
specLow2=lowspec(turnProb, detrend=T, tbw=2, padfac=10, pl=2, xmax=2, sigID=F, output=1)

# R code to compute EHA and EPSA results for HMM turnover probability time series

# HMM turnover probabilities should be placed in the data frame "turnProb"
#d = data.frame(age=dff$age, tp=dff$N.turnover)
turnPwr=eha(turnProb, win=20.001, fmin=0, fmax=1.5, #step=0.2,
            pad=4000, 
            genplot=4, palette=5, pl=2, ydir=-1, 
            tbw=2, output=2)

# R code to calculate variance associated with cycles and apply bandpass filters
# Uses object created by Dataset S9

# Integrate power spectra 
pwrRes1.3=integratePower(turnPwr, npts=npts, pad=4000, ydir=-1, xmax=2, unity=F, ln=T, 
                         flow=0.53, fhigh=0.90, genplot=F) 
pwrRes2.6=integratePower(turnPwr,npts=npts,pad=4000,ydir=-1, xmax=2, unity=F, ln=T, 
                         flow=0.21, fhigh=0.52, genplot=F) 
pwrResBoth=cb(pwrRes1.3[1], pwrRes1.3[4]) 
pwrResBoth[2]=pwrRes1.3[4]+pwrRes2.6[4] 

# Bandpass filter 
ecc=taner(turnProb, xmax=2, padfac=5, detrend=T, flow=1/3.1, fhigh=1/2.1, roll=10^6, 
          genplot=F) 
obl=taner(turnProb, xmax=2, padfac=5, detrend=T, flow=1/1.18, fhigh=1/1.47, roll=10^6, 
          genplot=F)

# A summary plot
pl(r=1,c=4) 
plot(pwrRes1.3[,4], pwrRes1.3[,1], type="l", lwd=1.5, col="red", 
     ylim= c(470.925,429.025), ylab="Millions of years", xlab= "1.3 Myr variance", 
     cex.axis=1.2, cex.lab=1.3) 
plot(pwrRes2.6[,4], pwrRes2.6[,1], type="l", lwd=1.5, col="blue", 
     ylim=c(470.925,429.025), ylab="Millions of years", xlab= "2.6 Myr variance", 
     cex.axis=1.2, cex.lab=1.3)   
plot(pwrResBoth[,2], pwrResBoth[,1], type="l", lwd=1.5, ylim=c(470.925,429.025), 
     ylab="Millions of years", xlab="Combined variance", cex.axis=1.2, cex.lab=1.3) 
plot(ecc[,2], ecc[,1], type="l", lwd=1.5, col="blue", ylim=c(470.925,429.025), 
     ylab="Millions of years", xlab="Filter outputs", cex.axis=1.2, cex.lab=1.3)
lines(obl[,2], obl[,1], lwd=1.5, col="red", cex.axis=1.2, cex.lab=1.3) 
