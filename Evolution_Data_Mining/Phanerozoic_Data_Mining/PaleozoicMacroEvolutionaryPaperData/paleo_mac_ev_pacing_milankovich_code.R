# Pacing of Paleozoic macroevolutionary rates by Milankovitch grand cycles
# Graptoloid evolution analysis

pal_mac_data_dir <- "~/Dropbox/TSCreator/TSCreator development/Developers/Andy/Projects/Phanerozoic_Data_Mining/PaleozoicMacroEvolutionaryPaperData"
setwd(pal_mac_data_dir)

# Read the excel file
library(readxl)
dfxl <- read_excel("pnas.1714342115.sd01.xlsx")
c <- colnames(dfxl)

# raw data
cn1 = c()
cil1 = c()
for(ci in 1:7) {
  cname = dfxl[[c[ci]]][2]
  if (!is.null(cname) && !is.na(cname)) {
    cil1 = c(cil1, ci)
    cn1 = c(cn1, cname)
  }
}

cn1
cil1
age = as.numeric(as.character(dfxl[[c[1]]][3:1242]))
df1 <- data.frame(Age=age) 
for(cil in cil1[-1]) {
  cv <- as.numeric(as.character(dfxl[[c[cil]]][3:1242]))
  df1 <- cbind(df1, cv)
}
colnames(df1) = c("Age", cn1[-1])
df1$`raw turnover probability` <- df1$`Raw speciation probability` + df1$`Raw extinction probability`

head(df1)
tail(df1)

# Four-state (speciation) and three-state (extinction) HMMs used to generate the default turnover time series
cn2 = c()
cil2 = c()
for(ci in 8:11) {
  cname = dfxl[[c[ci]]][2]
  if (!is.null(cname) && !is.na(cname)) {
    cil2 = c(cil2, ci)
    cn2 = c(cn2, cname)
  }
}

cn2
cil2

age = as.numeric(as.character(dfxl[[c[1]]][3:1242]))
df2 <- data.frame(Age=age) 
for(cil in cil2) {
  cv <- as.numeric(as.character(dfxl[[c[cil]]][3:1242]))
  df2 <- cbind(df2, cv)
}
colnames(df2) = c("Age", cn2)

head(df2)
tail(df2)
df2$`HMM turnover probability` <- df2$`HMM speciation state probability` + df2$`HMM extinction state probability`

# Model averaged, three-state HMMs for data given in columns F-G
cn3 = c()
cil3 = c()
for(ci in 13:16) {
  cname = dfxl[[c[ci]]][2]
  if (!is.null(cname) && !is.na(cname)) {
    cil3 = c(cil3, ci)
    cn3 = c(cn3, cname)
  }
}

cn3
cil3

age = as.numeric(as.character(dfxl[[c[1]]][3:1242]))
df3 <- data.frame(Age=age) 
for(cil in cil3) {
  cv <- as.numeric(as.character(dfxl[[c[cil]]][3:1242]))
  df3 <- cbind(df3, cv)
}
colnames(df3) = c("Age", cn3)

head(df3)
tail(df3)


# Data for which the effects of speculative, widespread, gradual phyletic evolution have been removed; three-state HMMs
cn4 = c()
cil4 = c()
for(ci in 18:27) {
  cname = dfxl[[c[ci]]][2]
  if (!is.null(cname) && !is.na(cname)) {
    cil4 = c(cil4, ci)
    cn4 = c(cn4, cname)
  }
}

cn4
cil4

age = as.numeric(as.character(dfxl[[c[1]]][3:1242]))
df4 <- data.frame(Age=age) 
for(cil in cil4) {
  cv <- as.numeric(as.character(dfxl[[c[cil]]][3:1242]))
  df4 <- cbind(df4, cv)
}
colnames(df4) = c("Age", cn4)

head(df4)
tail(df4)

# combine all the data frames in a single one
combined_df = cbind(df1, df2[,2:dim(df2)[2]], df3[,2:dim(df3)[2]], df4[,2:dim(df4)[2]])
head(combined_df)

# Count the extant species at each pseudolevel (year window)
head(df1)
tail(df1)


# Dataset S2
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
# 4 state speciation
ms <- 2:10
AICs <- c()
ys <- c()
for (m in ms)
{                         # specify number of states in the model
counts <- df1$`N speciations`
pn <- df1$`N species (speciation)`
x <- dthmm(counts, Pi=setPi(m), delta= setDPm(m), pm= list(prob=setDPm(m)), 
           pn=list(size=pn), distn="binom", discrete=TRUE)
y <- BaumWelch(x)               # estimated parameters of the HMM
ys <- c(ys, y)
n.param <- (m*(m-1))+2*m-1      # number of parameters in the model
log.lik.y <- logLik(y)          # log likelihood of model y
AIC.y <- 2*n.param-2*log.lik.y  # Akaike’s Information Criterion for model y
AICs <- c(AICs, AIC.y)
}

plot(ms, AICs, t='l')
# m   AIC
#10   3947.76
# 9   3927.369
# 8   3891.741
# 7   3874.213
# 6   3851.973  
# 5   3847.937
# 4   3841.5
# 3   3846.564
# 2   4028.095

# Predict most likely sequence of states given observations and model y
v <- Viterbi(y)                 # predicted sequence of HMM states at each pseudolevel

df1$`HMM speciation state` <- v
df1$`HMM speciation state probability` <- y$pm$prob[v]

# 3 state extinction
ms2 <- 2:8                          # specify number of states in the model
AICs2 <- c()
ys2 <- c()
for(m in ms2) 
{
counts <- df1$`N extinctions`
pn <- df1$`N species (extinction)`
x <- dthmm(counts, Pi=setPi(m), delta= setDPm(m), pm= list(prob=setDPm(m)), 
           pn=list(size=pn), distn="binom", discrete=TRUE)
y <- BaumWelch(x)               # estimated parameters of the HMM
ys2 <- c(ys2, y)
n.param <- (m*(m-1))+2*m-1      # number of parameters in the model
log.lik.y <- logLik(y)          # log likelihood of model y
AIC.y <- 2*n.param-2*log.lik.y  # Akaike’s Information Criterion for model y
AICs2 <- c(AICs2, AIC.y)
}

# Predict most likely sequence of states given observations and model y
v <- Viterbi(y)                 # predicted sequence of HMM states at each pseudolevel

df1$`HMM extinction state` <- v
df1$`HMM extinction state probability` <- y$pm$prob[v]

df1$`Raw turnover probability` <- df1$`Raw speciation probability` + df1$`Raw extinction probability`
df1$`HMM turnover probability` <- df1$`HMM speciation state probability` + df1$`HMM extinction state probability`

head(df1)


library(zoo)
par(mfrow=c(3,1))
m <- 1 #0.25/0.05
x <- rollmean(df1$Age, m)
xlim <- c(-450, -440)
y1 <- rollmean(df1$`Raw speciation probability`, m)
plot(-x, y1, t='l', col='green', xlim=xlim)
y2 <- rollmean(df1$`Raw extinction probability`, m)
plot(-x, y2, t='l', col='red', xlim=xlim)
y3 <- rollmean(df1$`Raw turnover probability`, m)
plot(-x, y3, t='l', xlim=xlim)

par(mfrow=c(3,1))
x <- rollmean(df1$Age, m)
y1 <- rollmean(df1$`HMM speciation state probability`, m)
plot(-x, y1, t='l', col='green', xlim=xlim)
y2 <- rollmean(df1$`HMM extinction state probability`, m)
plot(-x, y2, t='l', col='red', xlim=xlim)
y3 <- rollmean(df1$`HMM turnover probability`, m)
plot(-x, y3, t='l', xlim=xlim)

#
dat =getData("graptolite")

# a) mtm - conventional mtm-AR1 approach
mtm(dat, detrend = T)
# b) lowspec ar1 - lowspec analysis of ar1 surrogate needed
lowspec(dat, detrend=T) #, tbw=2, padfac=1, pl=2, sigID=F, output=1)

# c) periodogram - conventional ar1 with 25% cosine tapered periodogram, 
# background : 1=ar1 , 2=pwrlaw
periodogram(cosTaper(dat, demean=T, detrend=T), demean=F, 
            background=1, fNyq=F, padfac=1, output=1)

# d) mtmML96 - robust red noise mtm
mtmML96(dat)

# e) periodogram power law 25% cosine tapered periodogram, background 2
periodogram(cosTaper(dat, demean=T, detrend=T), demean=F, 
            background=2, fNyq=F, padfac=1, output=1)


# f) mtmPL - power law fit to mtm spectrum
mtmPL(dat)

# Dataset S3
# R code to generate surrogate simulations in order to assess suitability of 
#   spectral analysis procedures provided in Astrochron

# Load the library Astrochron
library(astrochron)

library(astsa)
ar1_noise_model <- sarima(df2$`HMM turnover probability`, 1, 0, 0)
ar1_noise_model$ttable

# Conduct simulations using AR1 surrogates and the MTM-AR1 approach
testBackground(npts=1240, dt=0.05, noiseType="ar1", coeff=0.3082017, 
               method="mtmAR1", detrend=T, tbw=2, iter=2000)

# Conduct simulations using AR1 surrogates and the MTM-ML96 approach
testBackground(npts=1240, dt=0.05, noiseType="ar1", coeff=0.3082017, 
               method="mtmML96",detrend=T, tbw=2, iter=2000)

# Conduct simulations using AR1 surrogates and the LOWSPEC approach
testBackground(npts=1240, dt=0.05, noiseType="ar1", coeff=0.3082017, 
               method="lowspec",detrend=T, tbw=2, iter=2000)

library(poweRlaw)
m_pl = conpl$new(df2$`HMM turnover probability`)
pwrlw_noise_model <- estimate_xmin(m_pl)
pwrlw_noise_model

m_pl2 <- displ$new(df2$`HMM extinction state`)
estimate_xmin(m_pl2)

# Conduct simulations using power law surrogates, and the MTM-PL approach
testBackground(npts=1240, dt=0.05, noiseType="pwrLaw", coeff= 0.3938128, 
               method="mtmPL",detrend=T, tbw=2, iter=2000)

# Conduct simulations using power law surrogates, and the Periodogram-PL approach
testBackground(npts=1240, dt=0.05, noiseType="pwrLaw", coeff= 0.3938128, 
               method="periodogramPL", detrend=T,iter=2000)

# Conduct simulations using AR1 surrogates, and the Periodogram-AR1 approach
testBackground(npts=1240, dt=0.05, noiseType="ar1", coeff= 0.3082017, 
               method="periodogramAR1", detrend=T,iter=2000)


# Dataset S4
# R code to generate surrogate simulations in order to assess suitability of 
#   spectral analysis procedures provided in Astrochron: testing the 
#   use of AR1-based models to analyze power law surrogates, and vice versa

# Load the library Astrochron
library(astrochron)

# Conduct simulations using power law surrogates and MTM-AR1 approach
testBackground(npts=1240, dt=0.05, noiseType="pwrLaw", coeff=0.3938128, 
               method="mtmAR1", detrend=T,tbw=2,iter=2000, genplot = T)

# Conduct simulations using power law surrogates and the MTM-ML96 approach
testBackground(npts=1240, dt=0.05, noiseType="pwrLaw", coeff=0.3938128, 
               method="mtmML96", detrend=T,tbw=2, iter=2000)

# Conduct simulations using power law surrogates and the LOWSPEC approach
testBackground(npts=1240, dt=0.05, noiseType="pwrLaw", coeff=0.3938128, 
               method="lowspec", detrend=T,tbw=2, iter=2000)

# Conduct simulations using AR1 surrogates, and the MTM-PL approach
testBackground(npts=1240, dt=0.05, noiseType="ar1", coeff= 0.3082017, 
               method="mtmPL", detrend=T, tbw=2, iter=2000)

# Conduct simulations using AR1 surrogates, and the Periodogram-PL approach
testBackground(npts=1240, dt=0.05, noiseType="ar1", coeff= 0.3082017, 
               method="periodogramPL", detrend=T, tbw=2, iter=2000)

# Conduct simulations using power law surrogates, and the Periodogram-AR1 approach
testBackground(npts=1240, dt=0.05, noiseType="pwrLaw", coeff=0.3938128, 
               method="periodogramAR1", detrend=T, iter=2000)

# Dataset S5
# R code for spectral analysis of the HMM turnover probability time series

# HMM turnover probabilities should be placed in the data frame "turnProb"
# Download these data from the Astrochron server
turnProb=getData("graptolite")

# Load the library Astrochron
library(astrochron)

# Conduct LOWSPEC analysis
specLow=lowspec(turnProb, detrend=T, tbw=2, padfac=1, pl=2, sigID=F, output=1)

# Conduct MTM-PL analysis
specPL1=mtmPL(turnProb, detrend=T, tbw=2, padfac=1, pl=2, sigID=F, output=1)

# Conduct Periodogram-AR1 analysis
specAR1=periodogram(cosTaper(turnProb, demean=T, detrend=T), demean=F, 
                    background=1, fNyq=F, padfac=1, output=1)

# Conduct Periodogram-PL analysis
specPL2=periodogram(cosTaper(turnProb, demean=T, detrend=T), demean=F, 
                    background=2, fNyq=F, padfac=1, output=1)

# Dataset S6
# R code for Bonferroni multiple test correction of spectral analyses
# Uses objects created by Dataset S5

# Define the frequency resolution bands for LOWSPEC associated with predicted 
#   ~1.2 Myr and ~2.4 Myr grand cycles;  
#   these are slightly larger than for the other methods, due to prewhitening
flow=c((1/2.4)-0.0322841,(1/1.2)-0.0322841)
fhigh=c((1/2.4)+0.0322841,(1/1.2)+0.0322841)

# Apply the Bonferroni correction to the LOWSPEC confidence levels 
confAdjust(specLow, npts=1239, dt=0.05, tbw=2, ntap=3, pl=2, flow=flow, fhigh=fhigh, 
           output=F) 

# Define the frequency resolution bands for MTM-PL, associated with predicted 
#   ~1.2 Myr and ~2.4 Myr grand cycles
flow=c((1/2.4)-0.03225806,(1/1.2)-0.03225806)
fhigh=c((1/2.4)+0.03225806,(1/1.2)+0.03225806)

# Apply the Bonferroni correction to the MTM-PL confidence levels 
confAdjust(specPL1, npts=1240, dt=0.05, tbw=2, ntap=3, pl=2, flow=flow, fhigh=fhigh) 

# Apply the Bonferroni correction to the Periodogram-AR1 confidence levels 
confAdjust(specAR1, npts=1240, dt=0.05, tbw=1, ntap=1, pl=2, flow=flow, fhigh=fhigh) 

# Apply the Bonferroni correction to the Periodogram-PL confidence levels 
confAdjust(specPL2, npts=1240, dt=0.05, tbw=1, ntap=1, pl=2, flow=flow, fhigh=fhigh) 

# Dataset S7
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

# Dataset S8
# R code to more accurately estimate cycle period of HMM turnover probability time series

# HMM turnover probabilities should be placed in the data frame "turnProb"
# Download these data from the Astrochron server
turnProb=getData("graptolite")

# Load the library Astrochron
library(astrochron)

# Conduct MTM spectral analysis with LOWSPEC background, and a finer frequency grid
specLow2=lowspec(turnProb, detrend=T, tbw=2, padfac=10, pl=2, xmax=2, sigID=F, output=1)

# Dataset S9
# R code to compute EHA and EPSA results for HMM turnover probability time series

# HMM turnover probabilities should be placed in the data frame "turnProb"
# Download these data from the Astrochron server
turnProb=getData("graptolite")

# Load the library Astrochron
library(astrochron)

turnPwr=eha(turnProb, win=20.001, step=.1, pad=4000, genplot=4, palette=5, pl=2, ydir=-1, 
            tbw=2, output=2)

# Dataset S10
# R code to compute spectral analyses of HMM turnover probability time series
#   for the interval between 466 and 460 Ma

# HMM turnover probabilities should be placed in the data frame "turnProb"
# Download these data from the Astrochron server
turnProb=getData("graptolite")

# Load the library Astrochron
library(astrochron)

# Isolate the turnover probability series between 460 and 466 Ma
turnProbLower=iso(turnProb, xmin=460, xmax=466)

# Conduct MTM spectral analysis with LOWSPEC background, apply Bonferroni correction, 
#   and evaluate a range of multiple testing procedures
specLow=lowspec(turnProbLower, detrend=T, tbw=2, padfac=1, pl=2, sigID=F, output=1)
flow=c((1/2.4)- 0.3361345,(1/1.2)- 0.3361345)
fhigh=c((1/2.4)+ 0.3361345,(1/1.2)+ 0.3361345) 
confAdjust(specLow, npts=119, dt=0.05, tbw=2, ntap=3, pl=2, flow=flow, fhigh=fhigh)
multiTest(specLow, flow=flow, fhigh=fhigh)

# Conduct MTM-PL analysis, apply Bonferroni correction, and evaluate a range of 
#   multiple testing procedures
specPL1=mtmPL(turnProbLower, detrend=T, tbw=2, padfac=1, pl=2, sigID=F, output=1)
flow=c((1/2.4)- 0.3333333,(1/1.2)- 0.3333333)
fhigh=c((1/2.4)+ 0.3333333,(1/1.2)+ 0.3333333) 
confAdjust(specPL1, npts=120, dt=0.05, tbw=2, ntap=3, pl=2, flow=flow, fhigh=fhigh)
multiTest(specPL1, flow=flow, fhigh=fhigh)

# Conduct Periodogram-AR1 analysis, apply Bonferroni correction, and evaluate a range 
#   of multiple testing procedures
specAR1=periodogram(cosTaper(turnProbLower, demean=T, detrend=T), demean=F, 
                    background=1, fNyq=F, padfac=1, output=1)
confAdjust(specAR1, npts=120, dt=0.05, tbw=2, ntap=3, pl=2, flow=flow, fhigh=fhigh)
multiTest(specAR1, flow=flow, fhigh=fhigh)

# Conduct Periodogram-PL analysis, apply Bonferroni correction, and evaluate a range 
#   of multiple testing procedures
specPL2=periodogram(cosTaper(turnProbLower, demean=T, detrend=T), demean=F, 
                    background=2, fNyq=F, padfac=1, output=1)
confAdjust(specPL2, npts=120, dt=0.05, tbw=2, ntap=1, pl=2, flow=flow, fhigh=fhigh)
multiTest(specPL2, flow=flow, fhigh=fhigh)

# Dataset S11
# R code to calculate variance associated with cycles and apply bandpass filters
# Uses object created by Dataset S9

# Integrate power spectra 
pwrRes1.3=integratePower(turnPwr, npts=401, pad=4000, ydir=-1, xmax=2, unity=F, ln=T, 
                         flow=0.53, fhigh=0.90, genplot=F) 
pwrRes2.6=integratePower(turnPwr,npts=401,pad=4000,ydir=-1, xmax=2, unity=F, ln=T, 
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

# Dataset S12
# R code to compute a hidden Markov model that is constrained to honor user-supplied 
#   state probabilities. User supplies the following vectors:
#   count of speciations or extinctions at each pseudolevel ("counts"), 
#   count of species extant at each pseudolevel ("pn"), and
#   median (or mean) hidden Markov model state probabilities ("pm.med")

# Function to initialize Pi matrix required by "dthmm"
# Initialized using random numbers from normal distribution
# Assume that values on diagonal ~0.4
# Rows must sum to 1
setPi <- function(m) {  
  Pi <- matrix(data=NA, nrow=m, ncol=m, byrow=T)  
  for(p in 1:m){
    Pi[p,p] <- rnorm(1,mean=0.4,sd=0.1)
    Pi[p,-p] <- (1-Pi[p,p])/(m-1)
  }
  return(Pi)
}

# Function to initialize delta and pm vectors required by "dthmm"
# Initialized using random numbers from normal distribution
# Delta must sum to 1
setDPm <- function(m) {                 
  DPm <- vector(mode="numeric", length=m)
  DPm[1] <- rnorm(1,mean=0.4,sd=0.1)
  DPm[-1] <- (1-DPm[1])/(m-1)
  return(DPm)
}

# Function to define new distribution; ensures that pm not overwritten
# Adapted from function "Mstep.binom"
Mstep.xyz <- function (x, cond, pm, pn) 
{
  if (is.null(pn)) 
    stop("Variable \"size\" must be specified in pn")
  prob <- pm$prob
  return(list(prob = prob))
}

# Define probability functions for distribution "xyz" 
# Based on binomial distribution
rxyz <- rbinom
dxyz <- dbinom
pxyz <- pbinom
qxyz <- qbinom

# Fit constrained hidden Markov model
# "m" is the number of discrete states in the model – this must be set by the user
# "counts" is the user-supplied vector of speciations or extinctions at each pseudolevel
# "pn" is the user-supplied vector of species extant at each pseudolevel
# "pm.med" is user-supplied vector of pm
library(HiddenMarkov)
m <- 3                          # specify number of states in the model
x <- dthmm(counts, Pi=setPi(n.states), delta=setDPm(n.states), dist="xyz", 
           pm=list(prob=pm.med), pn=list(size=pn), discrete=TRUE)
y <- BaumWelch(x)               # estimated parameters of the constrained HMM
n.param <- (m*(m-1))+2*m-1      # number of parameters in the model
log.lik.y <- logLik(y)          # log likelihood of model y
AIC.y <- 2*n.param-2*log.lik.y  # Akaike’s Information Criterion for model y

# Predict most likely sequence of states given observations and model y
v <- Viterbi(y)                 # predicted sequence of HMM states


# Dataset S13
# R code to simulate a Markov time series based on supplied parameters (input delta, pm and 
#   Pi) of a hidden Markov model:
#   “in.delta” and “in.pm” formatted as vectors
#   “in.Pi” formatted as an array
# “pn” is the user-supplied vector of observed species extant at each pseudolevel
# “seed” is user-supplied seed for the random number generator - 
#   using a particular value of seed will allow exact reproduction of a given simulation

xs <- dthmm(NULL, Pi=in.Pi, delta=in.delta, distn="binom", pm=list(prob=in.pm), 
            pn=list(size=pn), discrete=TRUE)  
zs <- simulate(xs, nsim=nrow(hmmdat), seed=seed)  # simulated time series is vector zs$x

