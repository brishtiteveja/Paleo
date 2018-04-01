## Spectrum Analysis
#Demean and Detrend data
#```{r}


# Number of events every 50 year
data_dir <- ('/Users/andy/Dropbox/TSCreator/TSCreator development/Developers/Andy/Projects/ML-Data Mining/programming/')
fn_50 <- paste(data_dir, 'event_frequency_per_50_yr_dat.txt', sep='')
ev_f_50 <- read.csv(fn_50, sep=' ')

time_interval <- 5
ev_f_50 <- subset(ev_f_50, ev_f_50$ages_50 <= time_interval)

eva <- ev_f_50$ages_50
ev <- ev_f_50$events_50
# demean event data
ev.dm <- ev - mean(ev)

# plot
plot(eva, ev.dm, xlab='Age(Ka)', 
     ylab='Number of Events', type='p', cex=0.25, col='blue', lwd=2,
     main='Demeaned Event Number Data', ylim=c(-10,40)
)
lines(ev_f_50$ages_50, ev.dm, col='blue')
abline(h=0, lty=2, col='blue')

hanning_ma <- function(df, n) {
  x <- 0.5 * df[n] + 0.25*(df[n-1] + df[n+1])
  return(x)
}

# filter event data
ev.filt <- c()
i <- 1
nr <- length(ev.dm)
for (n in 2:(nr-1)) {
  h <- hanning_ma(ev.dm, n)
  ev.filt <- c(ev.filt, h)
}

nr <- length(ev_f_50$ages_50)
plot(eva[2:(nr-1)], ev.filt, t='l', col='black', lwd=2)
abline(h=0, lty=2, col='black')

# detrend
#pev <- prewhiten(ev.filt)
#ev.dtrend <- pev[['prew_lm']]
lmfit <- lm(ev.filt ~ eva[2:(nr-1)])
ev.dtrend <- lmfit$residuals
plot(eva[2:(nr-1)], ev.dtrend, t='l', col='black', lwd=2, main='Demeaned + Detrended')
lines(eva[2:(nr-1)], ev.filt, t='l', col='blue', lty=2)
abline(h=0, lty=2, col='black')

# taper event data
ev.tap <- ev.dtrend
i <- 1
nr <- length(as.numeric(eva))
n <- length(ev.filt)

tap_perc_n <- n * 10/100 # 10% tapering to reduce the effect of the terminations of data at the ends of the window

for (j in 1:tap_perc_n) {
  w = j * pi / tap_perc_n
  ev.tap[j] = ev.tap[j] * 0.5 * (1-cos(w))
}
for (j in (n-tap_perc_n):n) {
  w = (512-j) * pi / tap_perc_n
  ev.tap[j] = ev.filt[j] * 0.5 * (1-cos(w))
}
# plot
plot(as.numeric(eva)[2:(nr-1)], ev.tap, col='black', 
     lty=1, t='l', lwd=2,
     main='Demeaned + Detrended + Tapered')
lines(eva[2:(nr-1)], ev.filt, t='l', col='blue', lty=2)
abline(h=0, lty=2)

# Spectral Analysis
# FFT
ev.fft <- fft(ev.tap)



# Calculate Amplitude
Amp <- Mod(ev.fft)

# Calculate Power Spectrum
PowerSpec <- Amp^2

# Take half of it , coz mirrored
PowerSpec <- PowerSpec[1:(length(Amp)/2)]

PowerSpec.han <- c()
i <- 1
nr <- length(PowerSpec)
for (n in 2:(nr-1)) {
  h <- hanning_ma(PowerSpec, n)
  PowerSpec.han <- c(PowerSpec.han, h)
}

t <- as.numeric(ev_f_50$ages_50)
window_width <- max(t) - min(t)
freq <- 1:(length(PowerSpec)-2)
period <- window_width/freq 

TotPowerSpec <- sum(PowerSpec)
RelPowerSpec <- PowerSpec.han/TotPowerSpec

# Charts of Wavenumber vs. Smoothed Power 
plot(freq, PowerSpec.han, t='l', 
     xlab='Frequency (Cycles / 5Ka)', 
     ylab='Power Spectra [Amplitude]^2',
     lwd=2
)

plot(period*1000, PowerSpec.han, t='l', 
     xlab='Time (Yr)', 
     ylab='Power Spectra [Amplitude]^2',
     xlim=c(0,1000),
     lwd=2
)
abline(v=250,lty=2)
abline(v=490, lty=2)
abline(v=205, lty=3)
abline(v=170, lty=3)

# )Wavelength vs. Relative Power Spectra
plot(RelPowerSpec, t='l', 
     xlab='Year (Ka)', 
     ylab='Relative Power Spectra [Amplitude]^2',
     main='Spectral analysis using single split cosine taper method',
     xaxt='n'
     )
idx <- seq(1,max(freq),by=12)
axis(side=1, at=idx, labels=FALSE)
mtext(side=1, padj = 1, text=round(period[idx],2), at=freq[idx])


evt <- ts(ev, start=0.04, frequency=20, deltat=5)
s <- spec.mtm(ts(ev), demean=FALSE, detrend=FALSE, nw = 1, k=1)
s$
  , 
              sineSmoothFact = 0.02,
              xlab='Year', 
              ylab='Relative Power Spectra [Amplitude]^2',
              main='Spectral analysis using multitaper method',
              Ftest = TRUE)
idx <- seq(0,4,by=1)
axis(side=1, at=idx, labels=FALSE)
p_idx <- seq(1, length(period), by=5)
mtext(side=1, padj = 1, text=round(period[p_idx],2), at=idx)



### With spec.pgram --- ambiguous


#CC <- ts(ev, start=0, end=12, frequency=10)  # 1000/100 = 10
CC <- ts(ev, start=0, end=1, frequency=12)  # 12000/100 = 120
par(mfrow=c(1,2))
P1 <- spec.pgram(CC,log='no',taper=0,pad=0,fast=FALSE,demean=TRUE,detrend=FALSE, xlab="Frequency(1/Ky)") 

# Periods calculated from frequency
Periods = time_interval/P1$freq
Periods

abline(v=24, lty=2)
abline(v=37, lty=2)
abline(v=48, lty=2)
# logarithmic plot
P2 <- spec.pgram(CC,log='yes',taper=0,pad=0,fast=FALSE,demean=TRUE,detrend=FALSE, xlab="Frequency(1/Ky)") 

abline(v=24, lty=2)
abline(v=37, lty=2)
abline(v=48, lty=2)

par(mfrow=c(2,1))
plot.frequency.spectrum(ev.fft, xlab='Cycles / 5Ka')


amp <- Mod(ev.fft)
N <- length(amp)
amp.h <- amp[0:(N/2)]
x <- 0:(N/2-1)
d <- data.frame(x, amp.h)
d.sorted <- d[order(amp.h, decreasing = TRUE),]
head(d.sorted, 20)

hm <- c(10, 20, 19, 3, 13)
plot.show(ev.tap, start_time = 0, time = 5, harmonics = hm,
          xlab='Age (Ka)', ylab='', scale=0.5)

hm_l <- paste(hm, ' cycles')
hm_l <- c(hm_l, 'combined cycle')
legend('topleft', legend=hm_l, col = c(1, 2, 3, 4, 5, 6,'darkblue'), 
       lty = rep(1,7), cex=0.40)

Xspec<- spec.pgram(ev.tap, log='no', lwd=2)

library(multitaper)
Mspec <- spec.mtm(ev.tap, deltat=1/100,
                  nw = 4, k = 7,
                  jackknife = TRUE, 
                  log='no', lwd=2)

a <- unlist(Mspec$mtm$jk$upperCI) #Mspec$spec)
N <- length(a)
a.h <- a[0:(N/2)]
x <- 0L:(N/2-1)
d <- data.frame(x, a.h)
d.sorted <- d[order(a.h, decreasing = TRUE),]
head(Mspec$freq[d.sorted$x], 20)

hm <- c(13, 16, 6, 21, 6, 8)
plot.show(ev.tap, start_time = min(eva), time = max(eva) - min(eva), harmonics = hm,
          xlab='Age (Ka)', ylab='', scale=0.5)
hm_l <- paste(hm, ' cycles')
hm_l <- c(hm_l, 'combined cycle')
legend('topright', legend=hm_l, col = c('blue','green','red','darkblue'),
       lty = rep(1,4), cex=0.40)

