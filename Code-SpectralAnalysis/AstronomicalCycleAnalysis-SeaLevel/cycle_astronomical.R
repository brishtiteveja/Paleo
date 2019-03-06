# Ice Ages and Astronomical Causes: Page 131 - Sudden terminations and sawtooth shape
# Deuterium Record from Vostok ICE


setwd("~/Dropbox/TSCreator/TSCreator development/Developers/Andy/Datapacks")
dp_fname <- "Phan_GTS2016_for_7.1_HaqJur_ForamMikrotax_28July2017.xls"
library(readxl)
dfxl <- read_excel(dp_fname)

#Long-Term Phanerozoic (SEPM98-Haq'08) data
#Long-Term Phanerozoic (SEPM98-Haq'08) data
# ages
lta <- as.numeric(dfxl$`1.3`[25253:26029])
# sea level
ltsl <- as.numeric(dfxl$X__1[25253:26029])

ltsl_df <- data.frame(Age=lta, SL=ltsl)

plot(ltsl_df, t='l')
points(ltsl_df, cex=0.25)

# Mean Sea Level (intermediate term; SEPM-Haq'08 synthetic)
# age
msla <- as.numeric(dfxl$`1.3`[24873:25246])
# sea level
msl <- as.numeric(dfxl$X__1[24873:25246])

msl_df <- data.frame(Age=msla, SL=msl)

plot(msl_df, t='l')
points(msl_df, cex=0.25)

# Short-Term Phanerozoic
# ages
sta <- as.numeric(dfxl$`1.3`[24220:24867])
# sea level
stsl <- as.numeric(dfxl$X__1[24220:24867])

rm(stsl_df)
stsl_df <- data.frame(Age=sta, SL=stsl)

plot(stsl_df$Age, stsl_df$SL, t='l')
points(stsl_df$Age, stsl_df$SL, cex=0.15)

# Rolling average
s <- stsl_df$SL
n <- length(s)
roll_avg <- c(1, 0.5, 2.5, 5, 10, 25, 50)
roll_n <- length(roll_avg)
for (k in roll_size) {
  N <- k
  x <- na.omit(stats::filter(s, sides=2, rep(1,N)/N))
  if (k != 1)
    lines(x, col=k, lty=2)
  else
    plot(x, col=k, lty=1, lwd=2)
}
legend('topleft', legend = paste(roll_size, 'Myr'), col=roll_size, lty=c(1, rep(2, roll_n-1)) , cex=0.5)

# loess fit
model.loess <- loess(SL~Age, data = stsl_df) #, family='symmetric')
fit.loess <- predict(model.loess, stsl_df$Age)
plot(stsl_df$Age, stsl_df$SL, t='l')
lines(fit.loess, col='red')

# lowess fit
plot(stsl_df$Age, stsl_df$SL, t='l', xlab='Age (Myr)', ylab='Sea Level(meters above present day)')
# 25% weighted averaging # locally weighted averagin
SL.25Avg <- fit.lowess <- lowess(stsl_df$Age, stsl_df$SL, f = 0.25)
#x2 <- predict(fit.loess, stsl_df$Age)
lines(SL.25Avg, col='red')

# detrended SL ( residuals of 25% weighted average)
stsl_df$SL.dtrnd25WAvg <- stsl_df$SL - SL.25Avg$y
plot(stsl_df$Age, stsl_df$SL.dtrnd25WAvg, t='l')

# Gaussian bandpass filter output
# 36 Myr
df <- data.frame(Age=stsl_df$Age, SL.25Avg=stsl_df$SL.dtrnd25WAvg)
dtmin <- min(abs(diff(df$Age)))
library(astrochron)
slInterp = linterp(df)

freq <- 0.028 # 1/36Myr
df <- 0.012
fl <- freq - df
fh <- freq + df
res_36 <- bandpass(slInterp, 
         flow = fl, 
         fhigh = fh,
         win = 1, # Gaussian bandpass filter
         output=1
         )
par(mfrow=c(1,1))
plot(stsl_df$Age, stsl_df$SL.dtrnd25WAvg, t='l')
par(new=T)
plot(res_36, t = 'l', col='red', axes=F, xlab=NA, ylab=NA)
locv = floor(seq(min(floor(res_36$SL.25Avg)), max(floor(res_36$SL.25Avg)), length.out = 5))
axis(side=4, at=locv, 
     tick = T)

freq <- 1/91 # 1/36Myr
df <- 0.012
fl <- freq - df
fh <- freq + df
res_91 <- bandpass(slInterp, 
         flow = fl, 
         fhigh = fh,
         win = 1, # Gaussian bandpass filter
         output=1
         )
par(mfrow=c(1,1))
plot(stsl_df$Age, stsl_df$SL.dtrnd25WAvg, t='l')
par(new=T)
plot(res_36, t = 'l', col='red', axes=F, xlab=NA, ylab=NA)
locv = floor(seq(min(floor(res_36$SL.25Avg)), max(floor(res_36$SL.25Avg)), length.out = 5))
axis(side=4, at=locv, 
     tick = T)
par(new=T)
plot(res_91, t = 'l', col='green', axes=F, xlab=NA, ylab=NA)

         
freq <- 0.028
df <- 0.012
fl <- freq - df
fh <- freq + df
res_36 <- bandpass(slInterp, 
         flow = fl, 
         fhigh = fh,
         win = 1 # Gaussian bandpass filter
         )

# polynomial fit
fit.poly <- lm(SL~poly(Age, 6), data=stsl_df)
lines(stsl_df$Age, fit.poly$fitted.values, col='green')

legend('topright', 
       legend=c('25% weighted average', '6 degree polynomial fit'), 
       col=c('red','green'),lty = 1)


#demeaned
stsl_df$SL.demeaned <- stsl_df$SL - mean(stsl_df$SL)
lm.fit <- lm(SL.demeaned~Age, data=stsl_df)
#detrended SL
stsl_df$SL.detrended <- lm.fit$residuals
par(mfrow=c(1,1))
plot(stsl_df$Age, stsl_df$SL.detrended, t='l')

#detrending using Astrochron
library(astrochron)
stsl_dfM <- demean(stsl_df, genplot = F)
stsl_dfD <- detrend(stsl_dfM, genplot = F)
plot(stsl_dfD$Age, stsl_dfD$SL, t='l')


library(plotly)
library(dplyr)
plot_ly(stsl_df, x=~Age, y=~SL, 
        type='scatter',
        mode='lines', #+markers',
        line=list(color='rgb(0,0,0)')  
        #marker=list(color='rgb(0,0,0)', size=2) 
        ) %>%
    add_lines(
        x=fit.lowess$x, 
        y=fit.lowess$y,
        mode='lines',
        name='25% weighted average',
        line=list(color='rgb(255,0,0)')) %>%
    add_lines(
        x=stsl_df$Age, 
        y=fit.poly$fitted.values,
        name='6-degree poly fit',
        mode='lines',
        line=list(color='rgb(0,255,0)')) 


# Using my own fft
plot.spectrum(stsl_df$SL.dtrnd25WAvg, xlimits = c(0,100))

# Using periodogram
spec.pgram(stsl_df$SL.dtrnd25WAvg, log='no', type='bl', xlim=c(0.001,0.12))
spec.pgram(stsl_df$SL.dtrnd25WAvg, log='no', demean=TRUE, xlim=c(0,0.12))
pgs <- spec.pgram(stsl_df$SL.dtrnd25WAvg, log='no', demean=TRUE, detrend=TRUE, xlim=c(0,0.12))
plot(pgs$freq, pgs$spec, xlim = c(0,0.12), t='l')
abline(v=1/91, lty=2)
abline(v=1/40, lty=2)
abline(v=1/36, lty=2)


library(psd)
tapinit=1
Pspec <- psdcore(stsl_df$SL.dtrnd25WAvg, ntaper = tapinit) 
Pspec <- data.frame(Pspec)
# plot confidence(interval)
#plot(Pspec$freq, Pspec$spec, t='bl', log='y', xlim=c(0.001,0.12), cex=0.10)
par(mfrow=c(1,1))
plot(Pspec$freq, Pspec$spec, 
     type='bl', 
     xlab='frequency (1/Myr)', ylab='spectrum',
     xlim=c(0.001,0.12))
abline(v=1/91, lty=2)
mtext("91",
      side=2, # from left y axis
      line=-4, # distance from y axis
      #at=par("usr")[1]+0.05*diff(par("usr")[1:2]),
      cex=1)

abline(v=1/36, lty=2)
mtext("36",
      side=2, # from left y axis
      line=-8, # distance from y axis
      #at=par("usr")[1]+0.05*diff(par("usr")[1:2]),
      cex=1)

abline(v=1/40, lty=2)
mtext("40",
      side=2, # from left y axis
      line=-7.5, # distance from y axis
      #at=par("usr")[1]+0.05*diff(par("usr")[1:2]),
      cex=1)


Pfspec <- Pspec[Pspec$freq > 0 & Pspec$freq <= 0.12,]
Pfspec$period <- 1/Pfspec$freq
Pfspec <- Pfspec[order(Pfspec$spec, decreasing = T),]
Pfspec



library(astrochron)
a <- stsl_df$Age
sl <- stsl_df$SL.dtrnd25WAvg
t <- seq(min(a), max(a), length.out = length(a))
fit.st <- loess(stsl_df$SL.dtrnd25WAvg ~ stsl_df$Age, span = 0.005, family='symmetric')
slp <- predict(fit.st, t)
plot(a, sl, t='l')
lines(t, slp, t='l', col='red')
sldf <- data.frame(Age=t, SL=slp)
Mspec <- mtm(sldf, #detrend = T, 
             tbw = 2,
             #ar1 = TR,
             CLpwr = TRUE,
             pl  = 2,
             xmin=0, xmax=0.12 
            )


# Use the linear interpolation of astrochron
stsl_dff <- stsl_df[,c('Age', 'SL.dtrnd25WAvg')]
par(mfrow=c(1,1))
plot(stsl_dff, t='l')
dtmin <- min(abs(diff(stsl_dff$Age)))
modelSLInterp = linterp(stsl_dff)
modelSLInterpMin = linterp(stsl_dff, dt=dtmin)
par(mfrow=c(1,1))
plot(stsl_dff$Age, stsl_df$SL.dtrnd25WAvg, t='l')
lines(modelSLInterpMin, col='green')
lines(modelSLInterp, col='red')

write.table(stsl_dff, file = '~/Desktop/DetrendedSL.csv', row.names = F, col.names = F, sep = "\t")

# Use the linear interpolation of astrochron
Mtmspec<- mtm(modelSLInterp,
    #demean = T,
    #detrend = T,
    tbw = 2,
    output = 1,
    xmin = 0,
    xmax = 0.12, 
    ar1 = T,
    CLpwr = T,
    pl=2)

Mtmspec <- Mtmspec[Mtmspec$Frequency <= 0.12,]
Pwr <- sort(Mtmspec$Harmonic_CL, decreasing = TRUE, index.return=TRUE)
cl <- Mtmspec$Harmonic_CL[Pwr$ix][1:100]
topF <- Mtmspec$Frequency[Pwr$ix][1:100]
topP <- 1/Mtmspec$Frequency[Pwr$ix][1:100]
data.frame(Period = topP, Frequency = topF, CL = cl)

par(mfrow=c(1,1))
plot(Mtmspec$Frequency, Mtmspec$Power, 
     t='l', log='y', 
     xlim = c(0, 0.12), 
     lwd=2,
     xlab='Frequency (cycle/Myr)')
abline(v=1/91)
abline(v=1/36)

lines(Mtmspec$Frequency, Mtmspec$Harmonic_CL, 
     t='l', log='y', 
     xlim = c(0, 0.12), 
     col='red')

lines(Mtmspec$Frequency, Mtmspec$AR1_CL, 
     t='l', log='y', 
     xlim = c(0, 0.12), 
     col='green')
     
lines(Mtmspec$Frequency, Mtmspec$AR1_90_power, 
     t='l', log='y', 
     xlim = c(0, 0.12), 
     col=3)

lines(Mtmspec$Frequency, Mtmspec$AR1_95_power, 
     t='l', log='y', 
     xlim = c(0, 0.12), 
     col=4)

lines(Mtmspec$Frequency, Mtmspec$AR1_99_power, 
     t='l', log='y', 
     xlim = c(0, 0.12), 
     col=4)


ix <- which(Mtmspec$Harmonic_CL >= 88 && Mtmspec$Harmonic_CL >= 92)
harmonic_90_power <- data.frame(Frequency=Mtmspec$Frequency[ix],
                                CL = Mtmspec$Harmonic_CL[ix])

plot(harmonic_90_power, t='l')

fCL=eha(modelSLInterpMin
        #, pad = 100,
        , fmin=0
        , fmax=0.15
        , win=50
        , step=1
        , pl=2)


# Using RobPer package
# https://cran.r-project.org/web/packages/RobPer/vignettes/RobPer_vignette.pdf
library(RobPer)
n <- length(stsl_dff$SL.dtrnd25WAvg)
var <- var(stsl_df$SL.dtrnd25WAvg)*n/2

# Generalized Lomb-Scargle Zechmeister and Kurster 2009
PP_new <- RobPer(ts=stsl_dff, weighting=FALSE, periods=1/PP_konv$freq,
    regression="L2", 
    model="sine")
plot(PP_konv$freq, PP_new*var, 
     type="l", col='black', xlim=c(0,0.15), lwd=2,
     xlab='frequency(cycles/Myr)',
     ylab='spectrum',
     main='Sea Level spectral analysis')
abline(v=1/91, lty=2)
abline(v=1/36, lty=2)


PP_new <- RobPer(ts=stsl_dff, weighting=FALSE, periods=1/PP_konv$freq,
    regression="huber", 
    model="sine")
plot(PP_konv$freq, PP_new*var, 
     type="l", col='black', xlim=c(0,0.15), lwd=2,
     xlab='frequency(cycles/Myr)',
     ylab='spectrum',
     main='Sea Level spectral analysis')
abline(v=1/91, lty=2)
abline(v=1/36, lty=2)

# BlackMan Tuckey
BTSpec <- spec.BT(stsl_df$SL.dtrnd25WAvg, x_limit = c(0,0.15))
#BTSpec <- spec.BT(modelSLInterp$SL.dtrnd25WAvg, x_limit = c(0,0.15))
abline(v=1/91, lty=2)
abline(v=1/36, lty=2)

spec.pgram(stsl_df$SL.dtrnd25WAvg, log='no', xlim=c(0,0.12))
abline(v=1/91)
abline(v=1/36)

spec.pgram(ldeaths)
spec.ar(ldeaths)
ARSpec <- spec.ar(modelSLInterp$SL.dtrnd25WAvg, method='yule-walker', xlim=c(0,0.12))
plot(ARSpec$freq, ARSpec$spec)


# Vostok data
setwd("~/Dropbox/TSCreator/TSCreator development/Developers/Andy/Projects/ML-Data Mining/SpectralAnalysis/AstronomicalCycleAnalysis-SeaLevel/Data")
library(readxl)
fxl <- read_excel('vostok_dD.xls')
c1 <- fxl$`Site Name: Vostok` 
c2 <- fxl$X__1
c3 <- fxl$X__2
c4 <- fxl$X__3

n <- length(c1)
depth <- as.numeric(c1[10:n])
age <- as.numeric(c2[10:n])
delta_Dt <- as.numeric(c3[10:n])
delta_T <- as.numeric(c4[10:n])

# age vs D
plot(age, delta_Dt, xlab = 'age (kyr)', ylab = 'D(%)', t='l', main = 'Deuterium record from Vostok ice.')

# filtered delta_Dt
nF <- 80
delta_Dt.filt = stats::filter(delta_Dt, rep(1/nF,nF))
idx <- !(is.na(delta_Dt.filt))
delta_Dt.filt = delta_Dt.filt[!is.na(delta_Dt.filt)]
plot(age[idx], delta_Dt.filt, xlab = 'age (kyr)', ylab = 'D(%)', t='l', main = 'Deuterium record from Vostok ice.')

fit.loess <- loess(delta_Dt ~ age)
t <- seq(0, 370, 10)
dDt <- predict(fit.loess, t)

plot(t, dDt, xlab = 'age (kyr)', ylab = 'D(%)', 
     xlim = c(0,370),
     t='l', main = 'Deuterium record from Vostok ice.')


Pspec <- spec.pgram(delta_Dt.filt, taper=0.15, demean = TRUE)
plot(Pspec$freq, Pspec$spec, t='l', xlim=c(0, 0.02))

# using multitaper library
library(multitaper)
Mspec <- spec.mtm(delta_Dt, demean = TRUE, detrend = TRUE, xlim=c(0, 0.06))
plot(Mspec$freq, Mspec$spec, t='l', xlim=c(0,0.02))

library(psd)
tapinit=1
Pspec <- psdcore(delta_Dt, ntaper = tapinit) 
plot(Pspec, xlim=c(0, 0.02), log='no')
#plot(Pspec$freq, Pspec$spec, t='l', xlim=c(0, 0.02))
spp <- spectral_properties(Pspec[["taper"]], db.ci = TRUE)
#psppu <- with(Pspec, create_poly(freq, dB(spec), spp$stderr.chi.upper))
psppu <- with(Pspec, create_poly(freq, spec, spp$stderr.chi.upper))
psppl <- with(Pspec, create_poly(freq, spec, spp$stderr.chi.lower))
psppa <- with(Pspec, create_poly(freq, spec, spp$stderr.chi.approx))
par(new=T)
plot(psppu$x.x, psppu$y.y, lty=2, type='l', 
     axes=FALSE, xlab='', ylab='', col='green', 
     xlim=c(0,0.02))
par(new=T)
plot(psppl$x.x, psppl$y.y, lty=2, type='l', 
     axes=FALSE, xlab='', ylab='', col='red', 
     xlim=c(0,0.02))
par(new=T)
plot(psppa$x.x, psppa$y.y, lty=2, type='l', 
     axes=FALSE, xlab='', ylab='', col='pink', 
     xlim=c(0,0.02))


# Blackman tukey
?auspec
library(timsac)
auspec(delta_Dt.filt, lag=1/3)
